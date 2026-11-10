use super::ptg_out::put_chars;
use super::write::Out;
use crate::xml::Node;

pub(super) const SXVIEW: u16 = 0xB0;
const SXVD: u16 = 0xB1;
const SXVI: u16 = 0xB2;
const SXIVD: u16 = 0xB4;
const SXLI: u16 = 0xB5;
const SXPI: u16 = 0xB6;
const SXDI: u16 = 0xC5;
const SXDB: u16 = 0xC6;
const SXFDB: u16 = 0xC7;
const SXDBB: u16 = 0xC8;
const SXNUM: u16 = 0xC9;
const SXBOOL: u16 = 0xCA;
const SXERR: u16 = 0xCB;
const SXSTRING: u16 = 0xCD;
const SXDTR: u16 = 0xCE;
const SXNIL: u16 = 0xCF;
const SXDBEX: u16 = 0x122;
const SXFDBTYPE: u16 = 0x1BB;
const SXSTREAMID: u16 = 0xD5;
const SXVS: u16 = 0xE3;
const SXEX: u16 = 0xF1;
const SXVDEX: u16 = 0x100;
const DCONREF: u16 = 0x51;
const DCONNAME: u16 = 0x52;
const SXADDL: u16 = 0x864;

#[derive(Clone)]
pub(super) struct CacheIn {
    pub part: String,
    pub def: Node,
    pub records: Option<Node>,

    pub ext: Option<String>,
}

#[derive(Clone)]
pub(super) struct PivotIn {
    pub table: Node,
    pub cache: usize,
}

fn on(n: &Node, key: &str, default: bool) -> bool {
    match n.attr(key) {
        Some("0" | "false") => false,
        Some(_) => true,
        None => default,
    }
}

fn int(n: &Node, key: &str, default: i64) -> i64 {
    n.attr(key)
        .and_then(|v| v.parse::<i64>().ok())
        .unwrap_or(default)
}

#[derive(Default)]
struct Rec(Vec<u8>);

impl Rec {
    fn u8(&mut self, v: u8) -> &mut Self {
        self.0.push(v);
        self
    }
    fn u16(&mut self, v: u16) -> &mut Self {
        self.0.extend(v.to_le_bytes());
        self
    }
    fn u32(&mut self, v: u32) -> &mut Self {
        self.0.extend(v.to_le_bytes());
        self
    }
    fn raw(&mut self, v: &[u8]) -> &mut Self {
        self.0.extend_from_slice(v);
        self
    }

    fn st(&mut self, s: &str) -> &mut Self {
        self.u16(s.encode_utf16().count().min(255) as u16);
        put_chars(&mut self.0, s);
        self
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Val {
    Str(String),
    Num(f64),
    Date(String),
    Bool(bool),
    Err(u8),
    Nil,
}

fn error_code(s: &str) -> u8 {
    match s {
        "#NULL!" => 0,
        "#DIV/0!" => 7,
        "#VALUE!" => 15,
        "#REF!" => 23,
        "#NAME?" => 29,
        "#NUM!" => 36,
        _ => 42,
    }
}

fn val_of(n: &Node) -> Option<Val> {
    let v = n.attr("v").unwrap_or("");
    Some(match n.name.as_str() {
        "s" => Val::Str(v.to_string()),
        "n" => Val::Num(v.parse().unwrap_or(0.0)),
        "d" => Val::Date(v.to_string()),
        "b" => Val::Bool(v == "1" || v.eq_ignore_ascii_case("true")),
        "e" => Val::Err(error_code(v)),
        "m" => Val::Nil,
        _ => return None,
    })
}

fn days(y: i64, m: i64, d: i64) -> i64 {
    let (y, m) = if m <= 2 { (y - 1, m + 9) } else { (y, m - 3) };
    let era = y.div_euclid(400);
    let yoe = y.rem_euclid(400);
    let doy = (153 * m + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468 + 25_569
}

fn date_parts(s: &str) -> (u16, u16, u8, u8, u8, u8) {
    let (d, t) = s.split_once('T').unwrap_or((s, "00:00:00"));
    let mut dp = d.split('-').map(|v| v.parse::<i64>().unwrap_or(0));
    let (y, m, day) = (
        dp.next().unwrap_or(1900),
        dp.next().unwrap_or(1),
        dp.next().unwrap_or(1),
    );
    let mut tp = t.split(':').map(|v| {
        v.split('.')
            .next()
            .unwrap_or("0")
            .parse::<u8>()
            .unwrap_or(0)
    });
    let (h, mi, sec) = (
        tp.next().unwrap_or(0),
        tp.next().unwrap_or(0),
        tp.next().unwrap_or(0),
    );
    let serial = days(y, m, day);
    if (1..=59).contains(&serial) {
        let (m, dd) = if serial <= 31 {
            (1, serial)
        } else {
            (2, serial - 31)
        };
        return (1900, m, dd as u8, h, mi, sec);
    }
    (y as u16, m as u16, day as u8, h, mi, sec)
}

fn put_val(r: &mut Out, v: &Val) {
    let mut d = Rec::default();
    match v {
        Val::Str(s) => {
            d.st(s);
            r.rec(SXSTRING, &d.0);
        }
        Val::Num(x) => {
            d.raw(&x.to_le_bytes());
            r.rec(SXNUM, &d.0);
        }
        Val::Date(s) => {
            let (y, m, dd, h, mi, sec) = date_parts(s);
            d.u16(y).u16(m).u8(dd).u8(h).u8(mi).u8(sec);
            r.rec(SXDTR, &d.0);
        }
        Val::Bool(b) => {
            d.u8(u8::from(*b)).u8(0);
            r.rec(SXBOOL, &d.0);
        }
        Val::Err(e) => {
            d.u8(*e).u8(0);
            r.rec(SXERR, &d.0);
        }
        Val::Nil => r.rec(SXNIL, &[]),
    }
}

fn key(v: &Val) -> String {
    match v {
        Val::Str(s) => format!("s{s}"),
        Val::Num(x) => format!("n{:x}", x.to_bits()),
        Val::Date(s) => format!("d{s}"),
        Val::Bool(b) => format!("b{b}"),
        Val::Err(e) => format!("e{e}"),
        Val::Nil => "m".into(),
    }
}

fn sxaddl(o: &mut Out, sxc: u8, sxd: u8, data: &[u8]) {
    let mut d = Rec::default();
    d.u16(SXADDL).u16(0).u8(sxc).u8(sxd).raw(data);
    o.rec(SXADDL, &d.0);
}

fn sxaddl_id(o: &mut Out, sxc: u8, s: &str) {
    let mut d = Rec::default();
    d.u32(s.encode_utf16().count() as u32).u16(0).st(s);
    sxaddl(o, sxc, 0, &d.0);
}

fn six(v: u8) -> [u8; 6] {
    [v, 0, 0, 0, 0, 0]
}

fn fields(def: &Node) -> Vec<(String, Option<&Node>)> {
    def.child("cacheFields")
        .map(|c| {
            c.children("cacheField")
                .map(|f| {
                    (
                        f.attr("name").unwrap_or("").to_string(),
                        f.child("sharedItems"),
                    )
                })
                .collect()
        })
        .unwrap_or_default()
}

pub(super) struct CacheOut {
    pub stream: Vec<u8>,
    pub name: String,
}

fn field_flags(s: Option<&Node>, indexed: bool) -> u16 {
    let at = |k: &str, d: bool| s.map_or(d, |s| on(s, k, d));
    let mut f = 0u16;
    if at("containsNonDate", true) {
        f |= 0x400;
    } else {
        f |= 0x08;
    }
    if at("containsString", true) {
        f |= 0x80;
    }
    if at("containsNumber", false) {
        f |= 0x20;
    }
    if at("containsInteger", false) {
        f |= 0x40;
    }
    if !at("containsSemiMixedTypes", true) || at("containsMixedTypes", false) {
        f |= 0x100;
    }
    if at("containsDate", false) {
        f |= 0x800;
    }
    if indexed {
        f |= 1;
    }
    f
}

fn indexed_fields(def: &Node, tables: &[&Node]) -> Vec<bool> {
    let n = fields(def).len();
    let mut v = vec![false; n];
    for t in tables {
        if let Some(pf) = t.child("pivotFields") {
            for (i, f) in pf.children("pivotField").enumerate() {
                if i < n && f.child("items").is_some() {
                    v[i] = true;
                }
            }
        }
    }
    v
}

pub(super) fn write_caches(
    o: &mut Out,
    caches: &[CacheIn],
    tables: &[Vec<&Node>],
    warnings: &mut Vec<String>,
) -> Vec<CacheOut> {
    let mut outs = Vec::new();
    for (ci, c) in caches.iter().enumerate() {
        let id = (ci + 1) as u16;
        let src = c
            .def
            .child("cacheSource")
            .and_then(|s| s.child("worksheetSource"));
        let named = src.and_then(|s| s.attr("name"));
        let range = match (
            src.and_then(|s| s.attr("ref")).and_then(crate::parse_rect),
            src.and_then(|s| s.attr("sheet")),
        ) {
            (Some(rc), Some(sheet)) => Some((rc, sheet)),
            _ => None,
        };
        if named.is_none() && range.is_none() {
            warnings.push(format!(
                "pivot cache {}: only worksheet ranges are written",
                c.part
            ));
            continue;
        }
        let refreshed = c
            .def
            .attr("refreshedDate")
            .and_then(|v| v.parse::<f64>().ok())
            .unwrap_or(0.0);
        let mut d = Rec::default();
        d.u16(id);
        o.rec(SXSTREAMID, &d.0);
        o.rec(SXVS, &1u16.to_le_bytes());
        if let (Some(name), None) = (named, &c.ext) {
            let mut d = Rec::default();
            d.st(name).u16(0);
            o.rec(DCONNAME, &d.0);
        } else if let Some((rc, sheet)) = range {
            let (prefix, text) = match &c.ext {
                Some(file) => (1u8, format!("[{file}]{sheet}")),
                None => (2u8, sheet.to_string()),
            };
            let mut d = Rec::default();
            d.u16(rc.r0 as u16)
                .u16(rc.r1 as u16)
                .u8(rc.c0 as u8)
                .u8(rc.c1 as u8)
                .u16(text.encode_utf16().count() as u16 + 1)
                .u8(if text.is_ascii() { 0 } else { 1 });
            if text.is_ascii() {
                d.u8(prefix).raw(text.as_bytes());
            } else {
                d.u16(u16::from(prefix));
                for u in text.encode_utf16() {
                    d.u16(u);
                }
            }
            o.rec(DCONREF, &d.0);
        }
        sxaddl(o, 3, 0, &{
            let mut x = Rec::default();
            x.u32(u32::from(id)).u16(0);
            x.0
        });
        sxaddl(o, 3, 2, &{
            let mut x = Rec::default();
            x.raw(&[0; 6])
                .raw(&[0xFF; 4])
                .raw(&[4, 3])
                .raw(&refreshed.to_le_bytes())
                .u16(0);
            x.0
        });
        sxaddl(o, 3, 0x18, &six(4));
        sxaddl(o, 3, 1, &six(2));
        sxaddl(o, 3, 0x41, &six(0));
        sxaddl(o, 3, 0x34, &six(1));
        sxaddl(o, 3, 1, &six(0xFF));
        sxaddl(o, 3, 0xFF, &six(0));
        let name = format!("_SX_DB_CUR/{id:04X}");
        let stream = cache_stream(c, &tables[ci], id, refreshed);
        outs.push(CacheOut { stream, name });
    }
    outs
}

fn cache_stream(c: &CacheIn, tables: &[&Node], id: u16, refreshed: f64) -> Vec<u8> {
    let fs = fields(&c.def);
    let indexed = indexed_fields(&c.def, tables);
    let nsrc = c.def.child("cacheFields").map_or(0, |f| {
        f.children("cacheField")
            .filter(|x| x.attr("databaseField") != Some("0"))
            .count()
    });
    let groups: Vec<Option<&Node>> = c
        .def
        .child("cacheFields")
        .map(|f| {
            f.children("cacheField")
                .map(|x| x.child("fieldGroup"))
                .collect()
        })
        .unwrap_or_default();
    let names: Vec<String> = fs.iter().map(|f| f.0.clone()).collect();
    let formulas = field_formulas(&c.def);

    let shared: Vec<Vec<Val>> = fs
        .iter()
        .map(|(_, s)| {
            s.map(|s| s.kids.iter().filter_map(val_of).collect())
                .unwrap_or_default()
        })
        .collect();

    let mut rows: Vec<Vec<Val>> = Vec::new();
    if let Some(r) = &c.records {
        for row in r.children("r") {
            let mut vals = Vec::new();
            for cell in &row.kids {
                if cell.name == "x" {
                    let i = int(cell, "v", 0) as usize;
                    let f = vals.len();
                    vals.push(
                        shared
                            .get(f)
                            .and_then(|s| s.get(i))
                            .cloned()
                            .unwrap_or(Val::Nil),
                    );
                } else {
                    vals.push(val_of(cell).unwrap_or(Val::Nil));
                }
            }
            rows.push(vals);
        }
    }
    let calc_items = calculated_items(&c.def);
    let mut o = Out::default();
    let by = c.def.attr("refreshedBy").unwrap_or("");
    let mut d = Rec::default();
    d.u32(rows.len() as u32)
        .u16(id)
        .u16(if on(&c.def, "refreshOnLoad", false) {
            7
        } else {
            3
        })
        .u16((8190 / nsrc.max(1)) as u16)
        .u16(nsrc as u16)
        .u16(fs.len() as u16)
        .u16((rows.len() + calc_items.len()).min(0xFFFF) as u16)
        .u16(1)
        .st(by);
    o.rec(SXDB, &d.0);
    let mut d = Rec::default();
    d.raw(&refreshed.to_le_bytes()).u32(calc_items.len() as u32);
    o.rec(SXDBEX, &d.0);
    for ci in &calc_items {
        put_item_formula(&mut o, ci, ci.field, &names, &shared);
    }
    for (f, (name, s)) in fs.iter().enumerate() {
        let uniq = {
            let mut seen = std::collections::HashSet::new();
            for r in &rows {
                if let Some(v) = r.get(f) {
                    seen.insert(key(v));
                }
            }
            seen.len()
        };
        let idx = indexed[f];
        let group = groups[f];
        let child = f >= nsrc && formulas[f].is_none();
        let ranged = group.and_then(|g| g.child("rangePr"));
        let g_items: Vec<Val> = group
            .and_then(|g| g.child("groupItems"))
            .map(|g| g.kids.iter().filter_map(val_of).collect())
            .unwrap_or_default();
        let n_items = if idx && !child { shared[f].len() } else { 0 };
        let mut d = Rec::default();
        if formulas[f].is_some() {
            d.u16(0x8425)
                .u16(0xFFFF)
                .u16(f as u16)
                .u16(rows.len() as u16)
                .u16(0)
                .u16(0)
                .u16(0)
                .st(name);
        } else {
            let discrete = group.and_then(|g| g.child("discretePr"));
            let has_discrete_child = groups.iter().any(|g| {
                g.is_some_and(|g| g.child("discretePr").is_some() && int(g, "base", -1) == f as i64)
            });
            let fl = if discrete.is_some() {
                0x01
            } else if child {
                0x11
            } else if has_discrete_child {
                field_flags(*s, idx) | 0x08
            } else if g_items.is_empty() {
                field_flags(*s, idx)
            } else {
                field_flags(*s, idx) | 0x10
            };
            let g_at = |k: &str| group.map_or(0xFFFF, |g| int(g, k, 0xFFFF) as u16);
            let count = if g_items.is_empty() {
                uniq + calc_items.iter().filter(|c| c.field == f).count()
            } else {
                g_items.len()
            };
            d.u16(fl)
                .u16(g_at("par"))
                .u16(if child { g_at("base") } else { 0xFFFF })
                .u16(count as u16)
                .u16(if g_items.is_empty() { 0 } else { count as u16 })
                .u16(
                    discrete.map_or(0, |_| shared.get(g_at("base") as usize).map_or(0, Vec::len))
                        as u16,
                )
                .u16(n_items as u16)
                .st(name);
        }
        o.rec(SXFDB, &d.0);
        o.rec(SXFDBTYPE, &0u16.to_le_bytes());
        if let Some(formula) = formulas[f] {
            put_formula(&mut o, formula, &names);
        }
        if !g_items.is_empty() {
            for v in &g_items {
                put_val(&mut o, v);
            }
            put_range(&mut o, ranged);
            if let Some(dp) = group.and_then(|g| g.child("discretePr")) {
                let mut d = Rec::default();
                for x in dp.children("x") {
                    d.u16(int(x, "v", 0) as u16);
                }
                o.rec(0xD9, &d.0);
            }
        }
        if idx && !child {
            for v in &shared[f] {
                put_val(&mut o, v);
            }
        }
    }
    for r in &rows {
        let mut ix = Vec::new();
        for (f, v) in r.iter().enumerate().take(nsrc) {
            if indexed[f] {
                let i = shared[f].iter().position(|s| s == v).unwrap_or(0);
                ix.push(i as u8);
            }
        }
        if !ix.is_empty() {
            o.rec(SXDBB, &ix);
        }
        for (f, v) in r.iter().enumerate().take(nsrc) {
            if !indexed[f] {
                put_val(&mut o, v);
            }
        }
    }
    o.rec(0x0A, &[]);
    o.b
}

fn put_range(o: &mut Out, rp: Option<&Node>) {
    let Some(rp) = rp else {
        return;
    };
    let by = rp.attr("groupBy").unwrap_or("range");
    let by_type: u16 = match by {
        "seconds" => 1,
        "minutes" => 2,
        "hours" => 3,
        "days" => 4,
        "months" => 5,
        "quarters" => 6,
        "years" => 7,
        _ => 16,
    };
    let mut fl = by_type << 2;
    if on(rp, "autoStart", true) {
        fl |= 1;
    }
    if on(rp, "autoEnd", true) {
        fl |= 2;
    }
    o.rec(0xD8, &fl.to_le_bytes());
    if by == "range" {
        for k in ["startNum", "endNum", "groupInterval"] {
            let x = rp
                .attr(k)
                .and_then(|v| v.parse::<f64>().ok())
                .unwrap_or(0.0);
            put_val(o, &Val::Num(x));
        }
    } else {
        for k in ["startDate", "endDate"] {
            put_val(
                o,
                &Val::Date(rp.attr(k).unwrap_or("1900-01-01").to_string()),
            );
        }
        o.rec(0xCC, &(int(rp, "groupInterval", 1) as u16).to_le_bytes());
    }
}

fn item_type(t: &str) -> u16 {
    match t {
        "data" => 0,
        "default" => 1,
        "sum" => 2,
        "countA" => 3,
        "avg" => 4,
        "max" => 5,
        "min" => 6,
        "product" => 7,
        "count" => 8,
        "stdDev" => 9,
        "stdDevP" => 10,
        "var" => 11,
        "varP" => 12,
        "grand" => 13,
        "blank" => 14,
        _ => 0,
    }
}

const SUBTOTALS: [&str; 11] = [
    "sum", "countA", "avg", "max", "min", "product", "count", "stdDev", "stdDevP", "var", "varP",
];

const DATA_FUNCS: [&str; 11] = [
    "sum",
    "count",
    "average",
    "max",
    "min",
    "product",
    "countNums",
    "stdDev",
    "stdDevp",
    "var",
    "varp",
];

const SHOW_AS: [&str; 12] = [
    "normal",
    "difference",
    "percent",
    "percentDiff",
    "runTotal",
    "percentOfRow",
    "percentOfCol",
    "percentOfTotal",
    "index",
    "percentOfParentRow",
    "percentOfParentCol",
    "percentOfParent",
];

pub(super) fn write_view(o: &mut Out, p: &PivotIn, cache: &CacheIn, warnings: &mut Vec<String>) {
    let t = &p.table;
    let fs = fields(&cache.def);
    let pfs: Vec<&Node> = t
        .child("pivotFields")
        .map(|c| c.children("pivotField").collect())
        .unwrap_or_default();
    let loc = t.child("location");
    let rc = loc
        .and_then(|l| l.attr("ref"))
        .and_then(crate::parse_rect)
        .unwrap_or(xlcalc::Rect::new(0, 0, 0, 0));
    let first = |k: &str| loc.map_or(0, |l| int(l, k, 0)) as u16;
    let list = |tag: &str, item: &str| -> Vec<i64> {
        t.child(tag)
            .map(|c| c.children(item).map(|f| int(f, "x", 0)).collect())
            .unwrap_or_default()
    };
    let row_f = list("rowFields", "field");
    let col_f = list("colFields", "field");
    let pages: Vec<&Node> = t
        .child("pageFields")
        .map(|c| c.children("pageField").collect())
        .unwrap_or_default();
    let datas: Vec<&Node> = t
        .child("dataFields")
        .map(|c| c.children("dataField").collect())
        .unwrap_or_default();
    let nlines = |tag: &str| t.child(tag).map_or(0, |c| c.children("i").count());
    let name = t.attr("name").unwrap_or("");
    let data_name = t.attr("dataCaption").unwrap_or("Values");
    let mut grbit = 0x020Bu16;
    if !on(t, "rowGrandTotals", true) {
        grbit &= !1;
    }
    if !on(t, "colGrandTotals", true) {
        grbit &= !2;
    }
    if !on(t, "useAutoFormatting", true) {
        grbit &= !8;
    }
    let mut d = Rec::default();
    d.u16(rc.r0 as u16)
        .u16(rc.r1 as u16)
        .u16(rc.c0 as u16)
        .u16(rc.c1 as u16)
        .u16(rc.r0 as u16 + first("firstHeaderRow"))
        .u16(rc.r0 as u16 + first("firstDataRow"))
        .u16(rc.c0 as u16 + first("firstDataCol"))
        .u16(p.cache as u16)
        .u16(0)
        .u16(if on(t, "dataOnRows", false) { 1 } else { 2 })
        .u16(0xFFFF)
        .u16(pfs.len() as u16)
        .u16(row_f.len() as u16)
        .u16(col_f.len() as u16)
        .u16(pages.len() as u16)
        .u16(datas.len() as u16)
        .u16(nlines("rowItems") as u16)
        .u16(nlines("colItems") as u16)
        .u16(grbit)
        .u16(1)
        .u16(name.encode_utf16().count() as u16)
        .u16(data_name.encode_utf16().count() as u16);
    put_chars(&mut d.0, name);
    put_chars(&mut d.0, data_name);
    o.rec(SXVIEW, &d.0);
    for (i, f) in pfs.iter().enumerate() {
        let calc = i < fs.len() && cache_calc(&cache.def, i);
        let mut axis = match f.attr("axis") {
            Some("axisRow") => 1u16,
            Some("axisCol") => 2,
            Some("axisPage") => 4,
            _ => 0,
        };
        if on(f, "dataField", false) {
            axis |= 8;
        }
        let mut sub = 0u16;
        for (k, a) in SUBTOTALS.iter().enumerate() {
            if on(f, &format!("{a}Subtotal"), false) {
                sub |= 2 << k;
            }
        }
        let items: Vec<&Node> = f
            .child("items")
            .map(|c| c.children("item").collect())
            .unwrap_or_default();
        let has_default = items.iter().any(|i| i.attr("t") == Some("default"));
        if on(f, "defaultSubtotal", true) && (sub == 0 || has_default) {
            sub |= 1;
        }
        let mut d = Rec::default();
        d.u16(axis)
            .u16(sub.count_ones() as u16)
            .u16(sub)
            .u16(items.len() as u16);
        match f.attr("name") {
            Some(n) => {
                d.st(n);
            }
            None => {
                d.u16(0xFFFF);
            }
        }
        o.rec(SXVD, &d.0);
        for it in items {
            let ty = item_type(it.attr("t").unwrap_or("data"));
            let mut fl = 0u16;
            if on(it, "h", false) {
                fl |= 1;
            }
            if !on(it, "sd", true) {
                fl |= 2;
            }
            if on(it, "f", false) {
                fl |= 8;
            }
            if on(it, "m", false) {
                fl |= 0x10;
            }
            let mut d = Rec::default();
            d.u16(ty).u16(fl).u16(if ty == 0 {
                int(it, "x", 0) as u16
            } else {
                0xFFFF
            });
            match it.attr("n") {
                Some(n) => {
                    d.st(n);
                }
                None => {
                    d.u16(0xFFFF);
                }
            }
            o.rec(SXVI, &d.0);
        }
        let mut fl: u32 = if calc { 0x0AA0_3410 } else { 0x0AA0_141E };
        if on(f, "showAll", true) {
            fl |= 1;
        }
        for (k, bit) in [("dragToRow", 2u32), ("dragToCol", 4), ("dragToPage", 8)] {
            if !on(f, k, true) {
                fl &= !bit;
            }
        }
        let mut d = Rec::default();
        d.u32(fl)
            .u16(0xFFFF)
            .u16(0xFFFF)
            .u16(int(f, "numFmtId", 0) as u16)
            .u16(0xFFFF)
            .raw(&[0; 8]);
        o.rec(SXVDEX, &d.0);
    }
    let axis_list = |o: &mut Out, v: &[i64]| {
        if v.is_empty() {
            return;
        }
        let mut d = Rec::default();
        for &x in v {
            d.u16(if x < 0 { 0xFFFE } else { x as u16 });
        }
        o.rec(SXIVD, &d.0);
    };
    axis_list(o, &row_f);
    axis_list(o, &col_f);
    for (k, pg) in pages.iter().enumerate() {
        let mut d = Rec::default();
        d.u16(int(pg, "fld", 0) as u16)
            .u16(
                pg.attr("item")
                    .map_or(0x7FFD, |v| v.parse().unwrap_or(0x7FFD)),
            )
            .u16(k as u16 + 1);
        o.rec(SXPI, &d.0);
    }
    for df in &datas {
        let sub = df.attr("subtotal").unwrap_or("sum");
        let show = df.attr("showDataAs").unwrap_or("normal");
        let fld = int(df, "fld", 0) as usize;
        let cap = df
            .attr("name")
            .map(str::to_string)
            .unwrap_or_else(|| fs.get(fld).map_or(String::new(), |f| f.0.clone()));
        let sh = SHOW_AS.iter().position(|x| *x == show).unwrap_or_else(|| {
            warnings.push(format!("pivot data field show-as {show} not written"));
            0
        });
        let mut d = Rec::default();
        d.u16(fld as u16)
            .u16(DATA_FUNCS.iter().position(|x| *x == sub).unwrap_or(0) as u16)
            .u16(sh as u16)
            .u16(int(df, "baseField", 0) as u16)
            .u16(int(df, "baseItem", 0) as u16)
            .u16(int(df, "numFmtId", 0) as u16)
            .st(&cap);
        o.rec(SXDI, &d.0);
    }
    for (tag, nf) in [("rowItems", row_f.len()), ("colItems", col_f.len())] {
        let mut d = Rec::default();
        let mut prev: Vec<u16> = vec![0x7FFF; nf];
        if let Some(c) = t.child(tag) {
            for line in c.children("i") {
                let r = int(line, "r", 0) as usize;
                let ty = item_type(line.attr("t").unwrap_or("data"));
                let xs: Vec<u16> = line.children("x").map(|x| int(x, "v", 0) as u16).collect();
                let mut idx = vec![0x7FFFu16; nf];
                if ty != 0 && ty != 13 {
                    idx.clone_from(&prev);
                }
                if ty == 13 {
                    idx = vec![0; nf];
                } else {
                    idx[..r.min(nf)].copy_from_slice(&prev[..r.min(nf)]);
                }
                for (k, x) in xs.iter().enumerate() {
                    if r + k < nf {
                        idx[r + k] = *x;
                    }
                }
                let mut fl = 0u16;
                if ty == 13 {
                    fl |= 0x0A00;
                } else if ty != 0 {
                    fl |= 0x0200;
                }
                if tag == "colItems" && col_f.contains(&-2)
                    || tag == "rowItems" && row_f.contains(&-2)
                {
                    fl |= 0x1000;
                }
                fl |= (int(line, "i", 0) as u16) << 1;
                d.u16(r as u16).u16(ty).u16((r + xs.len()) as u16).u16(fl);
                for v in &idx {
                    d.u16(*v);
                }
                prev = idx;
            }
        }
        if d.0.is_empty() {
            d.raw(&[0; 8]);
        }
        o.rec(SXLI, &d.0);
    }

    let rows = loc.map_or(0, |l| int(l, "rowPageCount", 0)) as u16;
    let cols = loc.map_or(0, |l| int(l, "colPageCount", 0)) as u16;
    let mut d = Rec::default();
    d.u16(0)
        .raw(&[0xFF; 6])
        .u16(0)
        .u16(cols)
        .u16(rows)
        .raw(&[0x00, 0x02, 0x4F, 0x00])
        .raw(&[0xFF; 6]);
    o.rec(SXEX, &d.0);
    let mut d = Rec::default();
    d.u16(0x802)
        .u16(0)
        .raw(&[1, 0, 2, 0, 0, 0, 0, 0, 8, 3, 0x10, 0])
        .st(name)
        .u16(0xFFFF);
    o.rec(0x802, &d.0);
    o.rec(
        0x810,
        &[0x10, 8, 2, 0, 0, 0, 0, 0, 0x24, 0, 0, 0, 1, 0, 0, 0, 0],
    );
    sxaddl_id(o, 0, name);
    sxaddl(o, 0, 2, &[8, 0x41, 0x40, 0, 0, 0]);
    sxaddl(o, 0, 0x19, &[0x9F, 0, 0x40, 0, 0, 0]);
    for (i, (n, _)) in fs.iter().enumerate() {
        let shown = pfs.get(i).and_then(|f| f.attr("name")).unwrap_or(n);
        let _ = shown;
        sxaddl_id(o, 0x17, n);
        sxaddl(o, 0x17, 0x19, &six(0x28));
        sxaddl(o, 0x17, 1, &six(2));
        sxaddl(o, 0x17, 1, &six(0xFF));
        sxaddl(o, 0x17, 0xFF, &six(0));
    }
    let style = t
        .child("pivotTableStyleInfo")
        .and_then(|s| s.attr("name"))
        .unwrap_or("");
    if !style.is_empty() {
        let mut d = Rec::default();
        let n = style.encode_utf16().count() as u16;
        d.raw(&[0; 6]).u16(n * 3).u16(n);
        for u in style.encode_utf16() {
            d.u16(u);
        }
        sxaddl(o, 0, 0x1E, &d.0);
    }
    sxaddl(o, 0, 1, &six(2));
    sxaddl(o, 0, 1, &six(0xFF));
    sxaddl(o, 0, 0xFF, &six(0));
}

fn field_formulas(def: &Node) -> Vec<Option<&str>> {
    def.child("cacheFields")
        .map(|c| {
            c.children("cacheField")
                .map(|f| f.attr("formula"))
                .collect()
        })
        .unwrap_or_default()
}

fn put_formula(o: &mut Out, formula: &str, names: &[String]) {
    let (rgce, used) = super::wpcalc::compile(formula, names).unwrap_or_default();
    let mut d = Rec::default();
    d.u16(rgce.len() as u16).u16(used.len() as u16).raw(&rgce);
    o.rec(0xF9, &d.0);
    for u in used {
        let mut d = Rec::default();
        d.u16(0).u16(u as u16).u16(0xFFFF).u16(0);
        o.rec(0xF6, &d.0);
    }
}

fn cache_calc(def: &Node, i: usize) -> bool {
    def.child("cacheFields")
        .and_then(|c| c.children("cacheField").nth(i))
        .is_some_and(|f| f.attr("formula").is_some())
}

pub(super) fn read_table(
    pkg: &crate::package::Package,
    part: &str,
    caches: &mut Vec<CacheIn>,
) -> Result<PivotIn, String> {
    use crate::package::{dir_of, resolve};
    let table = super::write::rd_xml(pkg, part)?;
    let rel = pkg
        .rels(part)
        .into_iter()
        .find(|r| r.kind() == "pivotCacheDefinition")
        .ok_or("no cache")?;
    let cpart = resolve(dir_of(part), &rel.target);
    let cache = match caches.iter().position(|c| c.part == cpart) {
        Some(i) => i,
        None => {
            let def = super::write::rd_xml(pkg, &cpart)?;
            let ws = def
                .child("cacheSource")
                .and_then(|s| s.child("worksheetSource"));
            let rid = ws.and_then(|w| w.attr("r:id").or_else(|| w.attr("id")));
            let ext = rid.and_then(|id| {
                pkg.rels(&cpart)
                    .into_iter()
                    .find(|r| r.id == id)
                    .map(|r| r.target)
            });
            let ok = ws.is_some_and(|w| {
                w.attr("name").is_some() || (w.attr("ref").is_some() && w.attr("sheet").is_some())
            });
            if !ok || (rid.is_some() && ext.is_none()) {
                return Err("the source is not a worksheet range".into());
            }
            if let Some(e) = &ext {
                if e.contains('/') || e.contains('\\') {
                    return Err("the source is a workbook outside the saved file's folder".into());
                }
            }
            let fnames: Vec<String> = fields(&def).into_iter().map(|f| f.0).collect();
            for f in field_formulas(&def).into_iter().flatten() {
                super::wpcalc::compile(f, &fnames)?;
            }
            let special = def.child("cacheFields").is_some_and(|f| {
                f.children("cacheField").any(|x| {
                    x.child("fieldGroup").is_some_and(|g| {
                        g.child("rangePr").is_some_and(|r| {
                            r.attr("groupBy").is_some_and(|b| b != "range")
                                && r.attr("groupInterval").is_some_and(|i| i != "1")
                        })
                    })
                })
            });
            if special {
                return Err("day groups of several days are not written yet".into());
            }
            let records = pkg
                .rels(&cpart)
                .into_iter()
                .find(|r| r.kind() == "pivotCacheRecords")
                .and_then(|r| super::write::rd_xml(pkg, &resolve(dir_of(&cpart), &r.target)).ok());
            caches.push(CacheIn {
                part: cpart,
                def,
                records,
                ext,
            });
            caches.len() - 1
        }
    };
    Ok(PivotIn { table, cache })
}

struct CalcItem<'a> {
    field: usize,
    item: usize,
    formula: &'a str,
}

fn calculated_items(def: &Node) -> Vec<CalcItem<'_>> {
    let mut v: Vec<CalcItem<'_>> = def
        .child("calculatedItems")
        .map(|c| {
            c.children("calculatedItem")
                .filter_map(|ci| {
                    let r = ci
                        .child("pivotArea")?
                        .child("references")?
                        .child("reference")?;
                    Some(CalcItem {
                        field: r.attr("field")?.parse().ok()?,
                        item: r.child("x")?.attr("v")?.parse().ok()?,
                        formula: ci.attr("formula")?,
                    })
                })
                .collect()
        })
        .unwrap_or_default();
    v.sort_by_key(|c| (c.field, c.item));
    v
}

fn item_refs(formula: &str) -> (String, Vec<(String, String)>) {
    let c: Vec<char> = formula.chars().collect();
    let (mut out, mut refs, mut i) = (String::new(), Vec::new(), 0);
    while i < c.len() {
        if c[i] == '\'' {
            out.push(c[i]);
            i += 1;
            while i < c.len() {
                out.push(c[i]);
                i += 1;
                if c[i - 1] == '\'' {
                    break;
                }
            }
            continue;
        }
        if c[i].is_alphabetic() || c[i] == '_' {
            let st = i;
            while i < c.len() && (c[i].is_alphanumeric() || "_. ".contains(c[i])) {
                i += 1;
            }
            if i < c.len() && c[i] == '[' {
                if let Some(end) = c[i..].iter().position(|&x| x == ']') {
                    let fld: String = c[st..i].iter().collect();
                    let item: String = c[i + 1..i + end].iter().collect();
                    out.push_str(&format!("R{}Q", refs.len()));
                    refs.push((fld.trim().to_string(), item));
                    i += end + 1;
                    continue;
                }
            }
            out.extend(&c[st..i]);
            continue;
        }
        out.push(c[i]);
        i += 1;
    }
    (out, refs)
}

fn put_item_formula(
    o: &mut Out,
    ci: &CalcItem<'_>,
    f: usize,
    names: &[String],
    shared: &[Vec<Val>],
) {
    let (text, refs) = item_refs(ci.formula);
    let ph: Vec<String> = (0..refs.len()).map(|k| format!("R{k}Q")).collect();
    let (rgce, used) = super::wpcalc::compile(&text, &ph).unwrap_or_default();
    let mut d = Rec::default();
    d.u16(rgce.len() as u16).u16(used.len() as u16).raw(&rgce);
    o.rec(0xF9, &d.0);
    for u in used {
        let (fname, iname) = &refs[u];
        let fi = names
            .iter()
            .position(|n| n.eq_ignore_ascii_case(fname))
            .unwrap_or(f);
        let it = shared
            .get(fi)
            .and_then(|s| s.iter().position(|v| item_label(v) == *iname))
            .unwrap_or(0);
        o.rec(0xF6, &[0, 0, 0xFF, 0xFF, 0xFF, 0xFF, 1, 0]);
        let mut d = Rec::default();
        d.u16(fi as u16).u32(it as u32).u16(0);
        o.rec(0xF8, &d.0);
    }
    o.rec(0xF0, &[0, 0xFF, 0x10, 0x42, 0, 0, 1, 0]);
    o.rec(
        0xF2,
        &[1 | if f > 0 { 0x40 } else { 0 }, 0, f as u8, 4, 1, 0, 1, 0],
    );
    o.rec(0xF5, &(ci.item as u16).to_le_bytes());
    o.rec(0x103, &[0, 0, 0xFF, 0xFF]);
}

fn item_label(v: &Val) -> String {
    match v {
        Val::Str(s) | Val::Date(s) => s.clone(),
        Val::Num(x) => x.to_string(),
        Val::Bool(b) => b.to_string(),
        Val::Err(_) | Val::Nil => String::new(),
    }
}
