use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU32, Ordering};

use serde_json::{json, Value as J};
use xlcalc_om::{flags, Application, Call, FileSystem, RealFs};
use xlcalc_variant::{ObjectId, Variant};

use super::omsteps::OmSession;
use super::{Outcome, Tally};

pub struct OutFs {
    root: PathBuf,
}

static NEXT_DIR: AtomicU32 = AtomicU32::new(0);

pub fn new_out_dir() -> PathBuf {
    std::env::temp_dir()
        .join("xlcalc-conform-out")
        .join(format!(
            "{}-{}",
            std::process::id(),
            NEXT_DIR.fetch_add(1, Ordering::Relaxed)
        ))
}

impl OutFs {
    pub fn new(root: PathBuf) -> OutFs {
        OutFs { root }
    }

    fn rest(path: &str) -> Option<String> {
        let p = path.replace('/', "\\");
        let lower = p.to_ascii_lowercase();
        if lower == "@out" {
            return Some(String::new());
        }
        lower.starts_with("@out\\").then(|| p[5..].to_string())
    }

    pub fn real(&self, path: &str) -> Option<PathBuf> {
        OutFs::rest(path).map(|r| self.root.join(r))
    }
}

impl FileSystem for OutFs {
    fn read(&self, path: &str) -> io::Result<Vec<u8>> {
        match self.real(path) {
            Some(p) => std::fs::read(p),
            None => RealFs.read(path),
        }
    }

    fn write(&self, path: &str, bytes: &[u8]) -> io::Result<()> {
        match self.real(path) {
            Some(p) => {
                std::fs::create_dir_all(&self.root)?;
                RealFs.write(&p.to_string_lossy(), bytes)
            }
            None => RealFs.write(path, bytes),
        }
    }

    fn exists(&self, path: &str) -> bool {
        match self.real(path) {
            Some(p) => p.is_file(),
            None => RealFs.exists(path),
        }
    }

    fn full_path(&self, path: &str) -> Option<String> {
        let Some(rest) = OutFs::rest(path) else {
            return RealFs.full_path(path);
        };
        let mut parts: Vec<&str> = Vec::new();
        for seg in rest.split('\\').filter(|s| !s.is_empty() && *s != ".") {
            if seg == ".." {
                parts.pop()?;
            } else {
                parts.push(seg);
            }
        }
        parts.last()?;

        let dir = &parts[..parts.len() - 1];
        if !dir.is_empty() && !self.root.join(dir.join("\\")).is_dir() {
            return None;
        }
        Some(format!("@out\\{}", parts.join("\\")))
    }
}

struct Reader<'a> {
    app: &'a mut Application<'static>,
    temps: Vec<ObjectId>,
}

const GET: Call = Call {
    flags: flags::METHOD | flags::GET,
    lcid: 0,
};

impl Reader<'_> {
    fn get(&mut self, obj: ObjectId, name: &str, args: &[Variant]) -> Option<Variant> {
        let v = self.app.invoke(obj, name, GET, args, &[]).ok()?;
        if let Variant::Dispatch(Some(id)) = v {
            self.temps.push(id);
        }
        Some(v)
    }

    fn obj(&mut self, obj: ObjectId, name: &str, args: &[Variant]) -> Option<ObjectId> {
        match self.get(obj, name, args)? {
            Variant::Dispatch(Some(id)) => Some(id),
            _ => None,
        }
    }

    fn safe(&mut self, obj: Option<ObjectId>, name: &str, args: &[Variant]) -> J {
        obj.and_then(|o| self.get(o, name, args))
            .map_or(J::Null, |v| to_json(&v))
    }

    fn string(&mut self, obj: ObjectId, name: &str, args: &[Variant]) -> Option<String> {
        match self.get(obj, name, args)? {
            Variant::Bstr(s) => Some(s),
            v => Some(
                to_json(&v)
                    .as_str()
                    .map_or_else(String::new, str::to_string),
            ),
        }
    }

    fn int(&mut self, obj: ObjectId, name: &str) -> Option<i64> {
        match self.get(obj, name, &[])? {
            Variant::I2(n) => Some(n.into()),
            Variant::I4(n) => Some(n.into()),
            Variant::I8(n) => Some(n),
            Variant::R8(x) => Some(x.round() as i64),
            Variant::Bool(b) => Some(if b { -1 } else { 0 }),
            _ => None,
        }
    }

    fn bool(&mut self, obj: ObjectId, name: &str) -> Option<bool> {
        match self.get(obj, name, &[])? {
            Variant::Bool(b) => Some(b),
            Variant::I4(n) => Some(n != 0),
            Variant::I2(n) => Some(n != 0),
            _ => None,
        }
    }

    fn address(&mut self, range: ObjectId) -> Option<String> {
        self.string(
            range,
            "Address",
            &[Variant::Bool(false), Variant::Bool(false)],
        )
    }

    fn count(&mut self, coll: ObjectId) -> i32 {
        self.int(coll, "Count").unwrap_or(0) as i32
    }

    fn item(&mut self, coll: ObjectId, i: i32) -> Option<ObjectId> {
        self.obj(coll, "Item", &[Variant::I4(i)])
    }

    fn book(&mut self, book: ObjectId) -> J {
        let root = self.app.handle();
        let mut r = serde_json::Map::new();
        r.insert(
            "name".into(),
            J::from(self.string(book, "Name", &[]).unwrap_or_default()),
        );
        r.insert(
            "file_format".into(),
            self.int(book, "FileFormat").map_or(J::Null, J::from),
        );
        let has_project = self.bool(book, "HasVBProject");
        r.insert("has_vbproject".into(), has_project.map_or(J::Null, J::from));
        r.insert(
            "date1904".into(),
            self.bool(book, "Date1904").map_or(J::Null, J::from),
        );
        r.insert(
            "precision_as_displayed".into(),
            self.bool(book, "PrecisionAsDisplayed")
                .map_or(J::Null, J::from),
        );
        r.insert(
            "calc".into(),
            json!({
                "mode": self.int(root, "Calculation"),
                "iteration": self.bool(root, "Iteration"),
                "max_iterations": self.int(root, "MaxIterations"),
                "max_change": self.safe(Some(root), "MaxChange", &[]),
                "calc_version": self.int(root, "CalculationVersion"),
            }),
        );
        if has_project == Some(true) {
            match self.modules(book) {
                Ok(m) => r.insert("vba_modules".into(), J::Array(m)),
                Err(e) => r.insert("vba_error".into(), J::from(e)),
            };
        }
        r.insert("names".into(), J::Array(self.names(book)));
        let mut styles: Vec<J> = Vec::new();
        let mut sheets = Vec::new();
        if let Some(ws) = self.obj(book, "Worksheets", &[]) {
            for i in 1..=self.count(ws) {
                if let Some(sh) = self.item(ws, i) {
                    sheets.push(self.sheet(sh, &mut styles));
                }
            }
        }
        r.insert("sheets".into(), J::Array(sheets));
        r.insert("styles".into(), J::Array(styles));
        J::Object(r)
    }

    fn modules(&mut self, book: ObjectId) -> Result<Vec<J>, String> {
        let fail = || "VBProject not readable".to_string();
        let proj = self.obj(book, "VBProject", &[]).ok_or_else(fail)?;
        let comps = self.obj(proj, "VBComponents", &[]).ok_or_else(fail)?;
        let mut out = Vec::new();
        for i in 1..=self.count(comps) {
            let comp = self.item(comps, i).ok_or_else(fail)?;
            let cm = self.obj(comp, "CodeModule", &[]).ok_or_else(fail)?;
            let n = self.int(cm, "CountOfLines").unwrap_or(0);
            let text = if n > 0 {
                self.string(cm, "Lines", &[Variant::I4(1), Variant::I4(n as i32)])
                    .unwrap_or_default()
            } else {
                String::new()
            };
            out.push(json!({
                "name": self.string(comp, "Name", &[]).unwrap_or_default(),
                "type": self.int(comp, "Type").unwrap_or(0),
                "lines": n,
                "text": text,
            }));
        }
        Ok(out)
    }

    fn names(&mut self, book: ObjectId) -> Vec<J> {
        let book_name = self.string(book, "Name", &[]);
        let mut out = Vec::new();
        let Some(names) = self.obj(book, "Names", &[]) else {
            return out;
        };
        for i in 1..=self.count(names) {
            let Some(n) = self.item(names, i) else {
                continue;
            };
            let scope = match self
                .obj(n, "Parent", &[])
                .and_then(|p| self.string(p, "Name", &[]))
            {
                Some(p) if Some(&p) == book_name.as_ref() => J::from("book"),
                Some(p) => J::from(p),
                None => J::Null,
            };
            out.push(json!({
                "name": self.string(n, "Name", &[]).unwrap_or_default(),
                "refers_to": self.string(n, "RefersTo", &[]).unwrap_or_default(),
                "visible": self.bool(n, "Visible").unwrap_or(false),
                "scope": scope,
            }));
        }
        out
    }

    fn sheet(&mut self, ws: ObjectId, styles: &mut Vec<J>) -> J {
        let mut so = serde_json::Map::new();
        so.insert(
            "name".into(),
            J::from(self.string(ws, "Name", &[]).unwrap_or_default()),
        );
        so.insert(
            "visible".into(),
            J::from(self.int(ws, "Visible").unwrap_or(0)),
        );
        so.insert("code_name".into(), self.safe(Some(ws), "CodeName", &[]));
        let Some(used) = self.obj(ws, "UsedRange", &[]) else {
            return J::Object(so);
        };
        so.insert(
            "used".into(),
            J::from(self.address(used).unwrap_or_default()),
        );
        let r0 = self.int(used, "Row").unwrap_or(1) as i32;
        let c0 = self.int(used, "Column").unwrap_or(1) as i32;
        let rn = self.obj(used, "Rows", &[]).map_or(0, |r| self.count(r));
        let cn = self.obj(used, "Columns", &[]).map_or(0, |c| self.count(c));
        let (mut hr, mut hc) = (Vec::new(), Vec::new());
        if let Some(rows) = self.obj(ws, "Rows", &[]) {
            for i in r0..r0 + rn {
                if self
                    .item(rows, i)
                    .and_then(|r| self.bool(r, "Hidden"))
                    .unwrap_or(false)
                {
                    hr.push(i);
                }
            }
        }
        if let Some(cols) = self.obj(ws, "Columns", &[]) {
            for j in c0..c0 + cn {
                if self
                    .item(cols, j)
                    .and_then(|c| self.bool(c, "Hidden"))
                    .unwrap_or(false)
                {
                    hc.push(j);
                }
            }
        }
        so.insert("hidden_rows".into(), json!(hr));
        so.insert("hidden_cols".into(), json!(hc));
        let mut cells = Vec::new();
        if let Some(all) = self.obj(ws, "Cells", &[]) {
            for r in r0..r0 + rn {
                for c in c0..c0 + cn {
                    let mark = self.temps.len();
                    let Some(cell) = self.obj(all, "Item", &[Variant::I4(r), Variant::I4(c)])
                    else {
                        continue;
                    };
                    let mut o = self.cell(cell);
                    o.insert("text".into(), self.safe(Some(cell), "Text", &[]));
                    let st = self.style(cell);
                    let id = match styles.iter().position(|s| *s == st) {
                        Some(i) => i,
                        None => {
                            styles.push(st);
                            styles.len() - 1
                        }
                    };
                    o.insert("style_id".into(), J::from(id));
                    cells.push(J::Object(o));
                    self.release(mark);
                }
            }
        }
        so.insert("cells".into(), J::Array(cells));
        let mut tables = Vec::new();
        if let Some(los) = self.obj(ws, "ListObjects", &[]) {
            for i in 1..=self.count(los) {
                let Some(lo) = self.item(los, i) else {
                    continue;
                };
                let range = self
                    .obj(lo, "Range", &[])
                    .and_then(|r| self.address(r))
                    .unwrap_or_default();
                let mut columns = Vec::new();
                if let Some(lc) = self.obj(lo, "ListColumns", &[]) {
                    for k in 1..=self.count(lc) {
                        if let Some(col) = self.item(lc, k) {
                            columns
                                .push(J::from(self.string(col, "Name", &[]).unwrap_or_default()));
                        }
                    }
                }
                tables.push(json!({
                    "name": self.string(lo, "Name", &[]).unwrap_or_default(),
                    "range": range,
                    "headers": self.bool(lo, "ShowHeaders").unwrap_or(false),
                    "totals": self.bool(lo, "ShowTotals").unwrap_or(false),
                    "columns": columns,
                }));
            }
        }
        so.insert("tables".into(), J::Array(tables));
        J::Object(so)
    }

    fn cell(&mut self, cell: ObjectId) -> serde_json::Map<String, J> {
        let mut o = serde_json::Map::new();
        o.insert(
            "cell".into(),
            J::from(self.address(cell).unwrap_or_default()),
        );
        let has_formula = self.bool(cell, "HasFormula").unwrap_or(false);
        if has_formula {
            o.insert(
                "formula".into(),
                J::from(self.string(cell, "Formula2", &[]).unwrap_or_default()),
            );
        }
        let has_spill = self.bool(cell, "HasSpill").unwrap_or(false);
        let spill = if has_spill {
            self.obj(cell, "SpillingToRange", &[])
        } else {
            None
        };
        let anchor = has_formula
            && spill.is_some_and(|s| {
                let first = self
                    .obj(s, "Cells", &[])
                    .and_then(|c| self.obj(c, "Item", &[Variant::I4(1), Variant::I4(1)]))
                    .and_then(|f| self.string(f, "Address", &[]));
                first.is_some() && first == self.string(cell, "Address", &[])
            });
        match spill {
            Some(s) if anchor => {
                let v = self.get(s, "Value2", &[]).unwrap_or(Variant::Empty);
                o.insert("value".into(), value2(&v));
                let rows = self.obj(s, "Rows", &[]).map_or(0, |r| self.count(r));
                let cols = self.obj(s, "Columns", &[]).map_or(0, |c| self.count(c));
                o.insert("spill".into(), json!([rows, cols]));
            }
            _ => {
                let v = self.get(cell, "Value2", &[]).unwrap_or(Variant::Empty);
                o.insert("value".into(), scalar(&v));
                if has_spill {
                    if let Some(p) = self
                        .obj(cell, "SpillParent", &[])
                        .and_then(|p| self.address(p))
                    {
                        o.insert("spilled_from".into(), J::from(p));
                    }
                }
            }
        }
        o.insert(
            "format".into(),
            J::from(self.string(cell, "NumberFormat", &[]).unwrap_or_default()),
        );
        if self.bool(cell, "HasArray") == Some(true) {
            if let Some(a) = self
                .obj(cell, "CurrentArray", &[])
                .and_then(|a| self.address(a))
            {
                o.insert("array".into(), J::from(a));
            }
        }
        if self.bool(cell, "MergeCells") == Some(true) {
            if let Some(a) = self
                .obj(cell, "MergeArea", &[])
                .and_then(|a| self.address(a))
            {
                o.insert("merge".into(), J::from(a));
            }
        }
        o
    }

    fn style(&mut self, c: ObjectId) -> J {
        let style = self
            .obj(c, "Style", &[])
            .and_then(|s| self.string(s, "Name", &[]))
            .map_or(J::Null, J::from);
        let f = self.obj(c, "Font", &[]);
        let mut font = serde_json::Map::new();
        for (k, m) in [
            ("name", "Name"),
            ("size", "Size"),
            ("bold", "Bold"),
            ("italic", "Italic"),
            ("underline", "Underline"),
            ("strike", "Strikethrough"),
            ("sup", "Superscript"),
            ("sub", "Subscript"),
            ("color", "Color"),
            ("theme", "ThemeColor"),
            ("tint", "TintAndShade"),
            ("font_style", "FontStyle"),
        ] {
            font.insert(k.into(), self.safe(f, m, &[]));
        }
        let i = self.obj(c, "Interior", &[]);
        let mut fill = serde_json::Map::new();
        for (k, m) in [
            ("pattern", "Pattern"),
            ("color", "Color"),
            ("theme", "ThemeColor"),
            ("tint", "TintAndShade"),
            ("pattern_color", "PatternColor"),
        ] {
            fill.insert(k.into(), self.safe(i, m, &[]));
        }
        let mut borders = serde_json::Map::new();
        let bs = self.obj(c, "Borders", &[]);
        for (k, idx) in [
            ("left", 7),
            ("top", 8),
            ("bottom", 9),
            ("right", 10),
            ("diag_down", 5),
            ("diag_up", 6),
        ] {
            let bd = bs.and_then(|b| self.obj(b, "Item", &[Variant::I4(idx)]));
            if bd.is_none() {
                borders.insert(k.into(), J::Null);
                continue;
            }
            borders.insert(
                k.into(),
                json!({
                    "line": self.safe(bd, "LineStyle", &[]),
                    "weight": self.safe(bd, "Weight", &[]),
                    "color": self.safe(bd, "Color", &[]),
                    "theme": self.safe(bd, "ThemeColor", &[]),
                }),
            );
        }
        let c = Some(c);
        json!({
            "style": style,
            "font": font,
            "fill": fill,
            "borders": borders,
            "align": {
                "h": self.safe(c, "HorizontalAlignment", &[]),
                "v": self.safe(c, "VerticalAlignment", &[]),
                "wrap": self.safe(c, "WrapText", &[]),
                "orient": self.safe(c, "Orientation", &[]),
                "indent": self.safe(c, "IndentLevel", &[]),
                "shrink": self.safe(c, "ShrinkToFit", &[]),
            },
            "protect": {
                "locked": self.safe(c, "Locked", &[]),
                "hidden": self.safe(c, "FormulaHidden", &[]),
            },
        })
    }

    fn release(&mut self, mark: usize) {
        for id in self.temps.split_off(mark) {
            self.app.release(id);
        }
    }
}

fn to_json(v: &Variant) -> J {
    match v {
        Variant::Bool(b) => J::from(*b),
        Variant::I2(n) => J::from(*n),
        Variant::I4(n) => J::from(*n),
        Variant::I8(n) => J::from(*n),
        Variant::Ui1(n) => J::from(*n),
        Variant::R4(x) => J::from(f64::from(*x)),
        Variant::R8(x) | Variant::Date(x) => J::from(*x),
        Variant::Bstr(s) => J::from(s.as_str()),
        _ => J::Null,
    }
}

fn scalar(v: &Variant) -> J {
    match v {
        Variant::R8(x) => json!({ "t": "n", "bits": format!("{:016X}", x.to_bits()) }),
        Variant::Bstr(s) => json!({ "t": "s", "v": s }),
        Variant::Bool(b) => json!({ "t": "b", "v": b }),
        Variant::Error(code) => {
            let name = xlcalc_om::scode_error(*code)
                .map_or_else(|| format!("#ERR{}", code & 0xFFFF), |e| e.to_string());
            json!({ "t": "e", "v": name })
        }
        Variant::Empty | Variant::Null => json!({ "t": "blank" }),
        other => json!({ "t": "unknown", "v": format!("{other:?}") }),
    }
}

fn value2(v: &Variant) -> J {
    let Variant::Array(a) = v else {
        return scalar(v);
    };
    let rows = a.bounds.first().map_or(0, |b| b.1 as usize);
    let cols = a.bounds.get(1).map_or(1, |b| b.1 as usize);
    let mut out = Vec::new();
    for r in 0..rows {
        let mut row = Vec::new();
        for c in 0..cols {
            row.push(a.data.get(c * rows + r).map_or(J::Null, scalar));
        }
        out.push(J::Array(row));
    }
    json!({ "t": "a", "rows": rows, "cols": cols, "v": out })
}

fn strip_numbers(j: &mut J) {
    match j {
        J::Object(o) => {
            if o.get("t").and_then(J::as_str) == Some("n") {
                o.remove("v");
            }
            o.values_mut().for_each(strip_numbers);
        }
        J::Array(a) => a.iter_mut().for_each(strip_numbers),
        _ => {}
    }
}

fn vba_modules_code(list: &J) -> Vec<(J, J, String)> {
    list.as_array()
        .map(|a| {
            a.iter()
                .map(|m| {
                    (
                        m["name"].clone(),
                        m["type"].clone(),
                        crate::vba_code(m["text"].as_str().unwrap_or_default()),
                    )
                })
                .collect()
        })
        .unwrap_or_default()
}

fn cut(s: &str) -> String {
    if s.chars().count() > 160 {
        format!("{}...", s.chars().take(160).collect::<String>())
    } else {
        s.to_string()
    }
}

fn diff_books(want: &J, got: &J) -> Vec<String> {
    let mut d = Vec::new();
    let (Some(w), Some(g)) = (want.as_object(), got.as_object()) else {
        return vec!["no record".into()];
    };
    for (k, wv) in w {
        let gv = g.get(k).unwrap_or(&J::Null);
        match k.as_str() {
            "sheets" => {
                let ws = wv.as_array().cloned().unwrap_or_default();
                let gs = gv.as_array().cloned().unwrap_or_default();
                if ws.len() != gs.len() {
                    d.push(format!("{} sheet(s), xlcalc {}", ws.len(), gs.len()));
                }
                for (a, b) in ws.iter().zip(&gs) {
                    diff_sheet(a, b, (&want["styles"], &got["styles"]), &mut d);
                }
            }

            "styles" => {}
            "names" => {
                let before = d.len();
                let empty = Vec::new();
                let (wn, gn) = (
                    wv.as_array().unwrap_or(&empty),
                    gv.as_array().unwrap_or(&empty),
                );
                for n in wn {
                    match gn.iter().find(|x| x["name"] == n["name"]) {
                        Some(x) if x == n => {}
                        Some(x) => d.push(format!("name {}: Excel {n} | xlcalc {x}", n["name"])),
                        None => d.push(format!("name {} missing: Excel {n}", n["name"])),
                    }
                }
                for x in gn {
                    if !wn.iter().any(|n| n["name"] == x["name"]) {
                        d.push(format!("name {} only in xlcalc", x["name"]));
                    }
                }
                let order = |l: &Vec<J>| l.iter().map(|n| n["name"].clone()).collect::<Vec<_>>();
                if d.len() == before && order(wn) != order(gn) {
                    d.push(format!(
                        "names order: Excel {} | xlcalc {}",
                        J::Array(order(wn)),
                        J::Array(order(gn))
                    ));
                }
            }
            "vba_modules" if vba_modules_code(wv) == vba_modules_code(gv) => {}
            _ if wv != gv => d.push(format!(
                "{k}: Excel {} | xlcalc {}",
                cut(&wv.to_string()),
                cut(&gv.to_string())
            )),
            _ => {}
        }
    }
    d
}

fn style_diff(a: &J, b: &J) -> String {
    let mut parts = Vec::new();
    for (g, va) in a.as_object().into_iter().flatten() {
        let vb = &b[g];
        match (va.as_object(), vb.as_object()) {
            (Some(oa), Some(ob)) => {
                for (k, x) in oa {
                    let y = ob.get(k).unwrap_or(&J::Null);
                    if x != y {
                        parts.push(format!("{g}.{k}: {x} | {y}"));
                    }
                }
            }
            _ if va != vb => parts.push(format!("{g}: {va} | {vb}")),
            _ => {}
        }
    }
    cut(&parts.join("; "))
}

fn column_of(a1: &str) -> i64 {
    a1.chars()
        .take_while(char::is_ascii_alphabetic)
        .fold(0, |n, c| {
            n * 26 + i64::from(c.to_ascii_uppercase() as u8 - b'A' + 1)
        })
}

fn text_by_width(cell: &J, hidden_cols: &J) -> bool {
    let text = cell["text"].as_str().unwrap_or("");
    let hidden = hidden_cols.as_array().is_some_and(|h| {
        h.iter()
            .any(|c| c.as_i64() == Some(column_of(cell["cell"].as_str().unwrap_or(""))))
    });
    let general_number = cell["format"] == "General" && cell["value"]["t"] == "n";
    (!text.is_empty() && text.chars().all(|c| c == '#'))
        || hidden
        || general_number
        || cell["format"].as_str().is_some_and(|f| f.contains('*'))
}

fn diff_sheet(want: &J, got: &J, styles: (&J, &J), d: &mut Vec<String>) {
    let name = want["name"].as_str().unwrap_or("");
    for (k, wv) in want.as_object().into_iter().flatten() {
        let gv = &got[k];
        if k == "cells" {
            let gc: Vec<J> = gv.as_array().cloned().unwrap_or_default();
            let wc = wv.as_array().cloned().unwrap_or_default();
            for c in &wc {
                let a1 = c["cell"].as_str().unwrap_or("");
                let Some(x) = gc.iter().find(|x| x["cell"] == c["cell"]) else {
                    d.push(format!("{name}!{a1}: missing"));
                    continue;
                };
                for (ck, cv) in c.as_object().into_iter().flatten() {
                    let xv = &x[ck];
                    match ck.as_str() {
                        "style_id" => {
                            let sw = &styles.0[cv.as_u64().unwrap_or(0) as usize];
                            let sg = &styles.1[xv.as_u64().unwrap_or(0) as usize];
                            if sw != sg {
                                d.push(format!("{name}!{a1}: style {}", style_diff(sw, sg)));
                            }
                        }
                        "text" if text_by_width(c, &want["hidden_cols"]) => {}
                        _ if xv != cv => d.push(format!(
                            "{name}!{a1}: {ck} {} | {}",
                            cut(&cv.to_string()),
                            cut(&xv.to_string())
                        )),
                        _ => {}
                    }
                }
                for ck in x.as_object().into_iter().flatten().map(|e| e.0) {
                    if c.get(ck).is_none() {
                        d.push(format!("{name}!{a1}: {ck} only in xlcalc"));
                    }
                }
            }
            for x in &gc {
                if !wc.iter().any(|c| c["cell"] == x["cell"]) {
                    d.push(format!(
                        "{name}!{}: only in xlcalc",
                        x["cell"].as_str().unwrap_or("")
                    ));
                }
            }
        } else if wv != gv {
            d.push(format!(
                "{name} {k}: Excel {} | xlcalc {}",
                cut(&wv.to_string()),
                cut(&gv.to_string())
            ));
        }
    }
}

impl OmSession {
    pub fn run_file_step(
        &mut self,
        wb: &mut xlcalc::Workbook,
        op: &str,
        step: &J,
        rec: &J,
        n: usize,
        tally: &mut Tally,
    ) -> bool {
        if !matches!(op, "file_read" | "file_bytes") {
            return false;
        }
        self.app.swap_engine(self.book, wb);
        if op == "file_read" {
            if let (Some(dir), Some(file)) =
                (std::env::var_os("CONF_SAVED_DIR"), step["file"].as_str())
            {
                self.keep_saved(Path::new(&dir), file, step, rec, n);
            }
        }
        let r = if op == "file_read" {
            self.file_read(step, rec)
        } else {
            self.file_bytes(step, rec).map_err(|e| vec![e])
        };
        self.app.swap_engine(self.book, wb);

        let file = step["file"]
            .as_str()
            .or(rec["book"]["name"].as_str())
            .or(rec["before"]["name"].as_str())
            .unwrap_or("(active)");

        match r {
            Ok(()) => tally.add(Outcome::Exact, String::new),
            Err(why) => {
                for w in why {
                    tally.add(Outcome::Wrong, || {
                        format!("step {n}: {op} {file}: {}", cut(&w))
                    });
                }
            }
        }
        true
    }

    fn keep_saved(&mut self, dir: &Path, file: &str, step: &J, rec: &J, n: usize) {
        let Ok(path) = self.real_out(file) else {
            return;
        };
        let Some(leaf) = path.file_name() else {
            return;
        };
        if !path.is_file() {
            return;
        }
        let root = self.app.handle();
        let mut get = |name: &str| {
            self.app
                .invoke(root, name, GET, &[], &[])
                .map(|v| to_json(&v))
                .unwrap_or(J::Null)
        };
        let calc = json!({ "mode": get("Calculation"), "iteration": get("Iteration"),
            "max_iterations": get("MaxIterations"), "max_change": get("MaxChange") });
        let at = dir.join(format!("{n:04}"));
        let kept = at.join(leaf);
        if std::fs::create_dir_all(&at).is_ok() && std::fs::copy(&path, &kept).is_ok() {
            let item = json!({ "n": n, "file": file, "path": kept.to_string_lossy(),
                "step": step, "rec": rec, "calc": calc });
            let _ = std::fs::write(at.join("item.json"), item.to_string());
        }
    }

    fn real_out(&self, file: &str) -> Result<PathBuf, String> {
        OutFs::new(self.out.clone())
            .real(file)
            .ok_or_else(|| format!("not an @out path: {file}"))
    }

    fn file_bytes(&mut self, step: &J, rec: &J) -> Result<(), String> {
        let path = self.real_out(step["file"].as_str().unwrap_or(""))?;
        let bytes = std::fs::read(&path).ok();
        let want_exists = rec["exists"].as_bool().unwrap_or(false);
        if bytes.is_some() != want_exists {
            return Err(format!(
                "exists: Excel {want_exists}, xlcalc {}",
                bytes.is_some()
            ));
        }
        let Some(bytes) = bytes else {
            return Ok(());
        };
        if let Some(hex) = rec["bytes_hex"].as_str() {
            let got: String = bytes.iter().map(|b| format!("{b:02X}")).collect();
            if got != hex {
                let at = got
                    .as_bytes()
                    .chunks(2)
                    .zip(hex.as_bytes().chunks(2))
                    .position(|(a, b)| a != b)
                    .unwrap_or(got.len().min(hex.len()) / 2);
                let show = |h: &str| {
                    let s = (at * 2).saturating_sub(16).min(h.len());
                    h[s..(at * 2 + 24).min(h.len())].to_string()
                };
                return Err(format!(
                    "bytes differ at {at} (length Excel {}, xlcalc {}): Excel {} | xlcalc {}",
                    hex.len() / 2,
                    got.len() / 2,
                    show(hex),
                    show(&got)
                ));
            }
        } else if rec["parts"].is_array() && !bytes.starts_with(b"PK") {
            return Err("Excel wrote a package, xlcalc did not".into());
        }
        Ok(())
    }

    fn file_read(&mut self, step: &J, rec: &J) -> Result<(), Vec<String>> {
        if let Some(e) = rec["error"].as_str() {
            let root = self.app.handle();
            return match self.app_get(root, "ActiveWorkbook", &[]) {
                Ok(_) if step["file"].is_null() => {
                    Err(vec![format!("Excel failed ({e}), xlcalc read a workbook")])
                }
                _ => Ok(()),
            };
        }
        let fullcalc = step["fullcalc"].as_bool().unwrap_or(false);
        let missing = Variant::Error(xlcalc_variant::HResult::DISP_E_PARAMNOTFOUND.0);
        let root = self.app.handle();
        let mut opened = None;
        let mut copy = None;
        let mut real_name = None;
        let result = (|| {
            let book = match step["file"].as_str() {
                Some(file) => {
                    let path = self.real_out(file)?;
                    if !path.is_file() {
                        return Err(format!("no such file {file}"));
                    }
                    let leaf = path
                        .file_name()
                        .map(|n| n.to_string_lossy().to_string())
                        .unwrap_or_default();
                    let copied = path.with_file_name(format!("__read__{leaf}"));
                    std::fs::copy(&path, &copied).map_err(|e| e.to_string())?;
                    copy = Some(copied);
                    real_name = Some(leaf.clone());
                    let wbs = self.app_get(root, "Workbooks", &[])?;
                    let args = [
                        Variant::Bstr(format!("@out\\__read__{leaf}")),
                        Variant::I4(0),
                        Variant::Bool(true),
                        missing.clone(),
                        missing.clone(),
                        missing.clone(),
                        Variant::Bool(true),
                    ];
                    let b = self.app_get(wbs, "Open", &args)?;
                    opened = Some(b);
                    b
                }
                None => self.app_get(root, "ActiveWorkbook", &[])?,
            };
            let read = |s: &mut OmSession| {
                let mut r = Reader {
                    app: &mut s.app,
                    temps: Vec::new(),
                };
                let mut j = r.book(book);
                r.release(0);
                if let Some(n) = &real_name {
                    j["name"] = J::from(n.as_str());
                }
                j
            };
            let mut d = Vec::new();
            let compare = |want: &J, got: &J, tag: &str, d: &mut Vec<String>| {
                let mut want = want.clone();
                strip_numbers(&mut want);
                for x in diff_books(&want, got) {
                    d.push(format!("{tag}{x}"));
                }
            };
            let before = read(self);
            if fullcalc {
                self.app
                    .invoke(root, "CalculateFull", GET, &[], &[])
                    .map_err(|e| e.to_string())?;
                let after = read(self);
                compare(&rec["before"], &before, "before: ", &mut d);
                compare(&rec["after"], &after, "after: ", &mut d);
            } else {
                compare(&rec["book"], &before, "", &mut d);
            }
            if std::env::var_os("CONF_FILE_DIFF").is_some() {
                for x in &d {
                    eprintln!("  {x}");
                }
            }
            Ok(if d.is_empty() { Ok(()) } else { Err(d) })
        })();
        if let Some(b) = opened {
            let _ = self
                .app
                .invoke(b, "Close", GET, &[Variant::Bool(false)], &[]);
        }
        for id in std::mem::take(&mut self.temps) {
            self.app.release(id);
        }
        if let Some(c) = copy {
            let _ = std::fs::remove_file(c);
        }
        result.map_err(|e| vec![e]).and_then(|r| r)
    }

    fn app_get(&mut self, obj: ObjectId, name: &str, args: &[Variant]) -> Result<ObjectId, String> {
        match self.app.invoke(obj, name, GET, args, &[]) {
            Ok(Variant::Dispatch(Some(id))) => {
                self.temps.push(id);
                Ok(id)
            }
            Ok(v) => Err(format!("{name} gave {v:?}")),
            Err(e) => Err(format!("{name}: {e}")),
        }
    }
}

pub fn remove_out(dir: &Path) {
    if std::env::var_os("CONFORM_KEEP_OUT").is_none() {
        let _ = std::fs::remove_dir_all(dir);
    }
}

pub fn crossread_files(args: &[String]) -> std::process::ExitCode {
    use std::process::{Command, ExitCode, Stdio};
    let suites: Vec<String> = if args.is_empty() {
        crate::suites()
            .into_keys()
            .filter(|n| n.starts_with("file_") || n.starts_with("csv_"))
            .collect()
    } else {
        args.to_vec()
    };
    let base = std::env::temp_dir().join("xlcalc-crossread-files");
    let _ = std::fs::remove_dir_all(&base);
    let exe = std::env::current_exe().expect("conform path");
    let mut batches = Vec::new();
    let mut kept: Vec<(String, Vec<J>)> = Vec::new();
    let mut total_sdk_failed = 0;
    for s in &suites {
        let dir = base.join(s);
        std::fs::create_dir_all(&dir).expect("crossread folder");
        let _ = Command::new(&exe)
            .args(["run", s])
            .env("CONF_SAVED_DIR", &dir)
            .stdout(Stdio::null())
            .status();
        let mut at: Vec<PathBuf> = std::fs::read_dir(&dir)
            .map(|r| r.flatten().map(|e| e.path()).collect())
            .unwrap_or_default();
        at.sort();
        let items: Vec<J> = at
            .iter()
            .filter_map(|d| std::fs::read_to_string(d.join("item.json")).ok())
            .filter_map(|t| serde_json::from_str(&t).ok())
            .collect();
        if items.is_empty() {
            continue;
        }

        let items: Vec<J> = items
            .into_iter()
            .filter(|it| {
                let path = it["path"].as_str().unwrap_or("");
                let bytes = std::fs::read(path).unwrap_or_default();

                if !bytes.starts_with(b"PK") || path.ends_with(".xlsb") {
                    return true;
                }
                let problems = xlcalc_xlsx::check_package(&bytes);
                for p in &problems {
                    println!("FAIL {s} {path}: package check: {p}");
                }
                problems.is_empty()
            })
            .collect();

        let zips: Vec<PathBuf> = items
            .iter()
            .filter_map(|it| it["path"].as_str())
            .filter(|p| !p.ends_with(".xlsb"))
            .filter(|p| std::fs::read(p).is_ok_and(|b| b.starts_with(b"PK")))
            .map(PathBuf::from)
            .collect();
        let sdk_errs = match crate::filecmds::openxml_validate(&zips) {
            Some(e) => e,
            None => {
                println!("note: Open XML SDK not installed (oracle/fetch_openxml_sdk.ps1): not validated");
                Vec::new()
            }
        };
        let mut sdk_failed = std::collections::BTreeSet::new();
        for (p, e) in &sdk_errs {
            println!("FAIL {s} {p}: {e}");
            sdk_failed.insert(p.clone());
        }
        let items: Vec<J> = items
            .into_iter()
            .filter(|it| !sdk_failed.contains(it["path"].as_str().unwrap_or("")))
            .collect();
        total_sdk_failed += sdk_failed.len();
        let mut steps = Vec::new();
        for it in &items {
            let c = &it["calc"];
            let mode = match c["mode"].as_i64() {
                Some(-4135) => "manual",
                Some(2) => "semi",
                _ => "auto",
            };
            steps.push(json!({ "op": "mode", "value": mode }));
            steps.push(json!({ "op": "iteration", "enabled": c["iteration"],
                "max_iter": c["max_iterations"], "max_change": c["max_change"] }));
            steps.push(json!({ "op": "file_read", "file": it["path"],
                "fullcalc": it["step"]["fullcalc"].as_bool().unwrap_or(false) }));
        }
        batches.push(json!({ "kind": "scenario", "id": s, "steps": steps }));
        kept.push((s.clone(), items));
    }
    if batches.is_empty() {
        println!("crossread-files: no saved files");
        return ExitCode::FAILURE;
    }
    let req = base.join("request.json");
    let resp_path = base.join("response.json");
    std::fs::write(&req, json!({ "batches": batches }).to_string()).expect("request");
    let status = Command::new("pwsh")
        .args(["-NoProfile", "-File"])
        .arg(crate::root().join("oracle").join("excel_oracle.ps1"))
        .arg("-In")
        .arg(&req)
        .arg("-Out")
        .arg(&resp_path)
        .status()
        .expect("run pwsh");
    if !status.success() {
        eprintln!("oracle failed: {status}");
        return ExitCode::FAILURE;
    }
    let text = std::fs::read_to_string(&resp_path).expect("response");
    let resp: J = serde_json::from_str(text.trim_start_matches('\u{feff}')).expect("oracle JSON");

    let todo = std::fs::read_to_string(crate::conf_dir().join("todo.txt")).unwrap_or_default();
    let (mut failed, mut listed, mut files, mut todo_items) =
        (total_sdk_failed, 0, total_sdk_failed, 0);
    for (suite, items) in &kept {
        let got = &resp["results"][suite]["items"];
        for (k, it) in items.iter().enumerate() {
            files += 1;
            let label = format!(
                "step {}: file_read {}",
                it["n"],
                it["file"].as_str().unwrap_or("")
            );
            let read = &got[3 * k + 2];
            let mut d = Vec::new();
            if let Some(e) = read["error"].as_str() {
                d.push(format!("Excel could not read xlcalc's file: {e}"));
            } else {
                let pairs: &[(&str, &str)] = if it["step"]["fullcalc"].as_bool() == Some(true) {
                    &[("before", "before: "), ("after", "after: ")]
                } else {
                    &[("book", "")]
                };
                for (key, tag) in pairs {
                    let mut want = it["rec"][key].clone();
                    let mut have = read[key].clone();
                    strip_numbers(&mut want);
                    strip_numbers(&mut have);
                    mask_width_text(&want, &mut have);
                    for x in diff_books(&want, &have) {
                        d.push(format!("{tag}{x}"));
                    }
                }
            }
            let mut bad = 0;
            for x in &d {
                let line = format!("{label}: {}", cut(x));
                match crate::out_of_scope_reason(suite, &line) {
                    Some(why) => {
                        listed += 1;
                        println!("     {suite} {line}  (out of scope: {})", cut(&why));
                    }
                    None if todo.contains(&format!("{suite} | ✗ {line}")) => {
                        todo_items += 1;
                        println!("todo {suite} {line}");
                    }
                    None => {
                        bad += 1;
                        println!("FAIL {suite} {line}");
                    }
                }
            }
            if bad == 0 {
                println!("ok   {suite} {label}");
            } else {
                failed += 1;
            }
        }
    }
    println!(
        "crossread-files: {files} file(s), {failed} failed, {listed} out-of-scope and {todo_items} todo difference(s)"
    );
    if failed == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

fn mask_width_text(want: &J, have: &mut J) {
    let Some(sheets) = have["sheets"].as_array_mut() else {
        return;
    };
    for sh in sheets {
        let Some(ws) = want["sheets"]
            .as_array()
            .and_then(|w| w.iter().find(|w| w["name"] == sh["name"]))
        else {
            continue;
        };
        let Some(cells) = sh["cells"].as_array_mut() else {
            continue;
        };
        for c in cells {
            let hashes = c["text"]
                .as_str()
                .is_some_and(|t| !t.is_empty() && t.bytes().all(|b| b == b'#'));
            if !hashes {
                continue;
            }
            if let Some(w) = ws["cells"]
                .as_array()
                .and_then(|w| w.iter().find(|w| w["cell"] == c["cell"]))
            {
                c["text"] = w["text"].clone();
            }
        }
    }
}
