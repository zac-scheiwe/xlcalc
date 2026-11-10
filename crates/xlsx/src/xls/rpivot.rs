use super::read::records;
use super::Rec;
use crate::xlsb::esc;
use std::fmt::Write as _;
use xlcalc_vbaproject::cfb::Cfb;

#[derive(Clone, Debug, PartialEq)]
enum Val {
    Str(String),
    Num(f64),
    Date(String),
    Bool(bool),
    Err(&'static str),
    Nil,
}

struct Fld {
    par: Option<usize>,

    base: Option<usize>,
    group_items: Vec<Val>,
    group_map: Vec<usize>,
    name: String,
    flags: u16,
    indexed: bool,
    shared: Vec<Val>,
    formula: Option<String>,
}

pub(super) struct RCache {
    source_name: Option<String>,
    sheet: String,
    range: String,
    by: String,
    date: f64,
    fields: Vec<Fld>,
    rows: Vec<Vec<Val>>,

    calc: Vec<(usize, usize, String)>,
}

struct Rd<'a> {
    d: &'a [u8],
    p: usize,
}

impl<'a> Rd<'a> {
    fn new(d: &'a [u8]) -> Self {
        Rd { d, p: 0 }
    }
    fn u8(&mut self) -> u8 {
        let v = self.d.get(self.p).copied().unwrap_or(0);
        self.p += 1;
        v
    }
    fn u16(&mut self) -> u16 {
        let v = u16::from(self.u8());
        v | (u16::from(self.u8()) << 8)
    }
    fn u32(&mut self) -> u32 {
        let v = u32::from(self.u16());
        v | (u32::from(self.u16()) << 16)
    }
    fn f64(&mut self) -> f64 {
        let mut b = [0u8; 8];
        for x in &mut b {
            *x = self.u8();
        }
        f64::from_le_bytes(b)
    }

    fn chars(&mut self, cch: usize) -> String {
        let wide = self.u8() & 1 != 0;
        let mut u = Vec::with_capacity(cch);
        for _ in 0..cch {
            u.push(if wide {
                self.u16()
            } else {
                u16::from(self.u8())
            });
        }
        String::from_utf16_lossy(&u)
    }
    fn st(&mut self) -> String {
        let n = usize::from(self.u16());
        self.chars(n)
    }

    fn opt(&mut self) -> Option<String> {
        let n = self.u16();
        (n != 0xFFFF).then(|| self.chars(usize::from(n)))
    }
}

fn whole(r: &Rec<'_>) -> Vec<u8> {
    let mut d = r.d.to_vec();
    for c in &r.cont {
        d.extend_from_slice(c);
    }
    d
}

fn err_text(c: u8) -> &'static str {
    match c {
        0 => "#NULL!",
        7 => "#DIV/0!",
        15 => "#VALUE!",
        23 => "#REF!",
        29 => "#NAME?",
        36 => "#NUM!",
        _ => "#N/A",
    }
}

fn civil(serial: i64) -> (i64, i64, i64) {
    let z = serial - 25_569 + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    (yoe + era * 400 + i64::from(m <= 2), m, d)
}

fn date_text(y: u16, m: u16, d: u8, h: u8, mi: u8, s: u8) -> String {
    let (y, m, d) = if y == 1900 && m <= 2 {
        let serial = if m == 1 {
            i64::from(d)
        } else {
            31 + i64::from(d)
        };
        civil(serial)
    } else {
        (i64::from(y), i64::from(m), i64::from(d))
    };
    format!("{y:04}-{m:02}-{d:02}T{h:02}:{mi:02}:{s:02}")
}

fn value(r: &Rec<'_>) -> Option<Val> {
    let d = whole(r);
    let mut c = Rd::new(&d);
    Some(match r.id {
        0xCD => Val::Str(c.st()),
        0xC9 => Val::Num(c.f64()),
        0xCA => Val::Bool(c.u8() != 0),
        0xCB => Val::Err(err_text(c.u8())),
        0xCF => Val::Nil,
        0xCE => {
            let (y, m) = (c.u16(), c.u16());
            Val::Date(date_text(y, m, c.u8(), c.u8(), c.u8(), c.u8()))
        }
        _ => return None,
    })
}

#[derive(Default)]
struct Src {
    id: u16,
    range: String,
    sheet: String,
    name: Option<String>,
    external: bool,
}

pub(super) fn read_caches(
    globals: &[Rec<'_>],
    cfb: &Cfb,
    warnings: &mut Vec<String>,
) -> Vec<RCache> {
    let mut out = Vec::new();
    let mut cur: Option<Src> = None;
    let mut found: Vec<Src> = Vec::new();
    for r in globals {
        match r.id {
            0xD5 => {
                if let Some(c) = cur.take() {
                    found.push(c);
                }
                cur = Some(Src {
                    id: Rd::new(r.d).u16(),
                    ..Src::default()
                });
            }
            0x52 => {
                if let Some(c) = cur.as_mut() {
                    c.name = Some(Rd::new(r.d).st());
                }
            }
            0x51 => {
                if let Some(c) = cur.as_mut() {
                    let mut d = Rd::new(r.d);
                    let (r0, r1) = (u32::from(d.u16()), u32::from(d.u16()));
                    let (c0, c1) = (u32::from(d.u8()), u32::from(d.u8()));
                    let n = usize::from(d.u16());
                    let wide = d.u8() & 1 != 0;

                    if wide {
                        c.external = d.u16() != 2;
                        let u: Vec<u16> = (1..n).map(|_| d.u16()).collect();
                        c.sheet = String::from_utf16_lossy(&u);
                    } else {
                        c.external = d.u8() != 2;
                        c.sheet = (1..n).map(|_| char::from(d.u8())).collect();
                    }
                    c.range = crate::write::rect_text(&xlcalc::Rect::new(r0, c0, r1, c1));
                }
            }
            _ => {}
        }
    }
    if let Some(c) = cur.take() {
        found.push(c);
    }
    for Src {
        id,
        range,
        sheet,
        name: source_name,
        external,
    } in found
    {
        if external {
            warnings.push(format!(
                "pivot cache {id}: an external workbook source is not read"
            ));
            out.push(None);
            continue;
        }
        let name = format!("_SX_DB_CUR/{id:04X}");
        let Ok(stream) = cfb.read_stream(&name) else {
            warnings.push(format!("pivot cache {id}: stream missing"));
            out.push(None);
            continue;
        };
        out.push(parse_stream(&stream, range, sheet, source_name, warnings));
    }
    out.into_iter().flatten().collect()
}

fn parse_stream(
    stream: &[u8],
    range: String,
    sheet: String,
    source_name: Option<String>,
    warnings: &mut Vec<String>,
) -> Option<RCache> {
    let recs = records(stream);
    let mut cache = RCache {
        source_name,
        sheet,
        range,
        by: String::new(),
        date: 0.0,
        fields: Vec::new(),
        rows: Vec::new(),
        calc: Vec::new(),
    };

    type Pending = (Vec<u8>, Vec<(usize, usize)>, usize);
    let mut pending: Vec<Pending> = Vec::new();
    let mut nrows = 0usize;
    let (mut nsrc, mut ntotal) = (0usize, 0usize);
    let mut i = 0;
    let mut want_items = 0usize;
    let mut want_group = 0usize;
    let mut body = Vec::new();
    while i < recs.len() {
        let r = &recs[i];
        let d = whole(r);
        let mut c = Rd::new(&d);
        match r.id {
            0xC6 => {
                nrows = c.u32() as usize;
                c.u16();
                c.u16();
                c.u16();
                nsrc = usize::from(c.u16());
                ntotal = usize::from(c.u16());
                c.u16();
                c.u16();
                cache.by = c.st();
            }
            0x122 => cache.date = c.f64(),
            0xC7 => {
                let flags = c.u16();
                let par = c.u16();
                let base = c.u16();
                let n_group = usize::from(c.u16());
                c.u16();
                c.u16();
                let n_items = usize::from(c.u16());
                let name = c.st();

                let child = flags & 0x8000 == 0 && cache.fields.len() >= nsrc;
                cache.fields.push(Fld {
                    par: (par != 0xFFFF).then_some(usize::from(par)),
                    base: child.then_some(usize::from(base)),
                    group_items: Vec::new(),
                    group_map: Vec::new(),
                    name,
                    flags,
                    indexed: flags & 1 != 0 && flags & 0x8000 == 0 && !child,
                    shared: Vec::new(),
                    formula: None,
                });
                want_items = n_items;
                want_group = if child { n_group } else { 0 };
            }
            0x1BB => {
                if let Some(f) = cache.fields.last_mut() {
                    while want_group > 0 && i + 1 < recs.len() {
                        i += 1;
                        if let Some(v) = value(&recs[i]) {
                            f.group_items.push(v);
                        }
                        want_group -= 1;
                    }
                    if f.base.is_some() && recs.get(i + 1).is_some_and(|n| n.id == 0xD9) {
                        i += 1;
                        let md = whole(&recs[i]);
                        f.group_map = md
                            .chunks(2)
                            .map(|b| {
                                usize::from(b[0]) | usize::from(b.get(1).copied().unwrap_or(0)) << 8
                            })
                            .collect();
                    }
                    while want_items > 0 && i + 1 < recs.len() {
                        i += 1;
                        if let Some(v) = value(&recs[i]) {
                            f.shared.push(v);
                        }
                        want_items -= 1;
                    }
                }

                if recs.get(i + 1).is_some_and(|n| n.id == 0xF9) {
                    let fd = whole(&recs[i + 1]);
                    let mut fc = Rd::new(&fd);
                    let (cce, nn) = (usize::from(fc.u16()), usize::from(fc.u16()));
                    let rgce: Vec<u8> = (0..cce).map(|_| fc.u8()).collect();
                    let mut used = Vec::new();
                    i += 1;
                    while used.len() < nn && recs.get(i + 1).is_some_and(|n| n.id == 0xF6) {
                        i += 1;
                        let nd = whole(&recs[i]);
                        let mut nc = Rd::new(&nd);
                        nc.u16();
                        let k = usize::from(nc.u16());
                        used.push(
                            cache
                                .fields
                                .get(k)
                                .map(|f| f.name.clone())
                                .unwrap_or_default(),
                        );
                    }
                    if let Some(f) = cache.fields.last_mut() {
                        f.formula = super::wpcalc::decompile(&rgce, &used);
                    }
                }
                if cache.fields.len() >= ntotal {
                    body.extend(recs[i + 1..].iter().filter(|r| r.id != 0x0A));
                    break;
                }
            }

            0xF9 => {
                let mut fc = Rd::new(&d);
                let (cce, nn) = (usize::from(fc.u16()), usize::from(fc.u16()));
                let rgce: Vec<u8> = (0..cce).map(|_| fc.u8()).collect();
                let mut names = Vec::new();
                while names.len() < nn && recs.get(i + 1).is_some_and(|n| n.id == 0xF6) {
                    i += 1;
                    if recs.get(i + 1).is_some_and(|n| n.id == 0xF8) {
                        i += 1;
                        let pd = whole(&recs[i]);
                        let mut pc = Rd::new(&pd);
                        let fld = usize::from(pc.u16());
                        names.push((fld, pc.u32() as usize));
                    }
                }
                let mut item = 0;
                while recs
                    .get(i + 1)
                    .is_some_and(|n| matches!(n.id, 0xF0 | 0xF2 | 0xF5 | 0x103))
                {
                    i += 1;
                    if recs[i].id == 0xF5 {
                        item = usize::from(Rd::new(&whole(&recs[i])).u16());
                    }
                }
                pending.push((rgce, names, item));
            }
            0x0A => break,
            _ => {}
        }
        i += 1;
    }
    for (rgce, names, item) in pending {
        let ph: Vec<String> = (0..names.len()).map(|k| format!("R{k}Q")).collect();
        let (field, _) = names.first().copied().unwrap_or((0, 0));
        let Some(mut text) = super::wpcalc::decompile(&rgce, &ph) else {
            continue;
        };
        for (k, (f, it)) in names.iter().enumerate().rev() {
            let label = cache
                .fields
                .get(*f)
                .and_then(|fl| fl.shared.get(*it))
                .map(|v| match v {
                    Val::Str(s) | Val::Date(s) => s.clone(),
                    Val::Num(x) => x.to_string(),
                    Val::Bool(b) => b.to_string(),
                    Val::Err(e) => e.to_string(),
                    Val::Nil => String::new(),
                })
                .unwrap_or_default();
            let fname = cache
                .fields
                .get(*f)
                .map(|fl| fl.name.clone())
                .unwrap_or_default();
            text = text.replace(&format!("R{k}Q"), &format!("{fname}[{label}]"));
        }
        cache.calc.push((field, item, text));
    }
    if ntotal != cache.fields.len()
        || nsrc > ntotal
        || cache.fields[nsrc..]
            .iter()
            .any(|f| f.formula.is_none() && f.base.is_none())
    {
        warnings.push("pivot cache with fields this reader does not know is not read".into());
        return None;
    }

    let wide: Vec<bool> = cache.fields.iter().map(|f| f.shared.len() > 255).collect();
    let n_idx = cache.fields.iter().take(nsrc).filter(|f| f.indexed).count();
    let mut b = 0;
    for _ in 0..nrows {
        let mut row = vec![Val::Nil; nsrc];
        if n_idx > 0 {
            let r = body.get(b)?;
            b += 1;
            let mut c = Rd::new(r.d);
            for (f, fl) in cache.fields.iter().enumerate().take(nsrc) {
                if fl.indexed {
                    let x = if wide[f] {
                        usize::from(c.u16())
                    } else {
                        usize::from(c.u8())
                    };
                    row[f] = fl.shared.get(x).cloned().unwrap_or(Val::Nil);
                }
            }
        }
        for (f, fl) in cache.fields.iter().enumerate().take(nsrc) {
            if !fl.indexed {
                row[f] = value(body.get(b)?)?;
                b += 1;
            }
        }
        cache.rows.push(row);
    }
    Some(cache)
}

fn val_xml(v: &Val) -> String {
    match v {
        Val::Str(s) => format!("<s v=\"{}\"/>", esc(s)),
        Val::Num(x) => format!("<n v=\"{x}\"/>"),
        Val::Date(s) => format!("<d v=\"{s}\"/>"),
        Val::Bool(b) => format!("<b v=\"{}\"/>", u8::from(*b)),
        Val::Err(e) => format!("<e v=\"{}\"/>", esc(e)),
        Val::Nil => "<m/>".into(),
    }
}

const MAIN: &str = "http://schemas.openxmlformats.org/spreadsheetml/2006/main";
const REL: &str = "http://schemas.openxmlformats.org/officeDocument/2006/relationships";

pub(super) fn cache_parts(c: &RCache) -> (String, String) {
    let mut def = format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<pivotCacheDefinition xmlns=\"{MAIN}\" xmlns:r=\"{REL}\" r:id=\"rId1\" refreshedBy=\"{}\" refreshedDate=\"{}\" createdVersion=\"8\" refreshedVersion=\"8\" minRefreshableVersion=\"3\" recordCount=\"{}\"><cacheSource type=\"worksheet\"><worksheetSource {}/></cacheSource><cacheFields count=\"{}\">",
        esc(&c.by),
        c.date,
        c.rows.len(),
        match &c.source_name {
            Some(n) => format!("name=\"{}\"", esc(n)),
            None => format!("ref=\"{}\" sheet=\"{}\"", c.range, esc(&c.sheet)),
        },
        c.fields.len()
    );
    for (f, fl) in c.fields.iter().enumerate() {
        if let Some(fm) = &fl.formula {
            let _ = write!(
                def,
                "<cacheField name=\"{}\" numFmtId=\"0\" formula=\"{}\" databaseField=\"0\"/>",
                esc(&fl.name),
                esc(fm)
            );
            continue;
        }
        if let Some(base) = fl.base {
            let _ = write!(
                def,
                "<cacheField name=\"{}\" numFmtId=\"0\" databaseField=\"0\"><fieldGroup base=\"{base}\"><discretePr count=\"{}\">",
                esc(&fl.name),
                fl.group_map.len()
            );
            for x in &fl.group_map {
                let _ = write!(def, "<x v=\"{x}\"/>");
            }
            let _ = write!(
                def,
                "</discretePr><groupItems count=\"{}\">",
                fl.group_items.len()
            );
            for v in &fl.group_items {
                def.push_str(&val_xml(v));
            }
            def.push_str("</groupItems></fieldGroup></cacheField>");
            continue;
        }
        let _ = write!(
            def,
            "<cacheField name=\"{}\" numFmtId=\"0\">",
            esc(&fl.name)
        );
        let fg = fl.flags;
        let mut at = String::new();
        if fg & 0x400 == 0 {
            at.push_str(" containsNonDate=\"0\"");
        }
        if fg & 0x800 != 0 {
            at.push_str(" containsDate=\"1\"");
        }
        if fg & 0x80 == 0 {
            at.push_str(" containsString=\"0\"");
        }
        if fg & 0x100 != 0 {
            if fg & 0x80 != 0 && fg & 0x20 != 0 {
                at.push_str(" containsMixedTypes=\"1\"");
            } else {
                at.push_str(" containsSemiMixedTypes=\"0\"");
            }
        }
        if fg & 0x20 != 0 {
            at.push_str(" containsNumber=\"1\"");
        }
        if fg & 0x40 != 0 {
            at.push_str(" containsInteger=\"1\"");
        }
        if fg & 0x20 != 0 && fg & 0x80 == 0 && !fl.indexed {
            let nums: Vec<f64> = c
                .rows
                .iter()
                .filter_map(|r| match r.get(f) {
                    Some(Val::Num(x)) => Some(*x),
                    _ => None,
                })
                .collect();
            if let (Some(lo), Some(hi)) = (
                nums.iter().copied().reduce(f64::min),
                nums.iter().copied().reduce(f64::max),
            ) {
                let _ = write!(at, " minValue=\"{lo}\" maxValue=\"{hi}\"");
            }
        }
        if fl.indexed {
            let _ = write!(def, "<sharedItems{at} count=\"{}\">", fl.shared.len());
            for (k, v) in fl.shared.iter().enumerate() {
                let x = val_xml(v);
                if c.calc.iter().any(|(cf, ci, _)| *cf == f && *ci == k) {
                    def.push_str(&x.replacen("/>", " f=\"1\"/>", 1));
                } else {
                    def.push_str(&x);
                }
            }
            def.push_str("</sharedItems>");
        } else {
            let _ = write!(def, "<sharedItems{at}/>");
        }
        if let Some(par) = fl.par {
            let _ = write!(def, "<fieldGroup par=\"{par}\"/>");
        }
        def.push_str("</cacheField>");
    }
    def.push_str("</cacheFields>");
    if !c.calc.is_empty() {
        let _ = write!(def, "<calculatedItems count=\"{}\">", c.calc.len());
        for (f, x, fm) in &c.calc {
            let _ = write!(
                def,
                "<calculatedItem formula=\"{}\"><pivotArea cacheIndex=\"1\" outline=\"0\" fieldPosition=\"0\"><references count=\"1\"><reference field=\"{f}\" count=\"1\"><x v=\"{x}\"/></reference></references></pivotArea></calculatedItem>",
                esc(fm)
            );
        }
        def.push_str("</calculatedItems>");
    }
    def.push_str("</pivotCacheDefinition>");
    let mut rec = format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<pivotCacheRecords xmlns=\"{MAIN}\" xmlns:r=\"{REL}\" count=\"{}\">",
        c.rows.len()
    );
    for row in &c.rows {
        rec.push_str("<r>");
        for (f, v) in row.iter().enumerate() {
            let fl = &c.fields[f];
            if fl.indexed {
                let i = fl.shared.iter().position(|s| s == v).unwrap_or(0);
                let _ = write!(rec, "<x v=\"{i}\"/>");
            } else {
                rec.push_str(&val_xml(v));
            }
        }
        rec.push_str("</r>");
    }
    rec.push_str("</pivotCacheRecords>");
    (def, rec)
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

fn item_t(t: u16) -> &'static str {
    match t {
        1 => "default",
        2..=12 => SUBTOTALS[usize::from(t) - 2],
        13 => "grand",
        14 => "blank",
        _ => "data",
    }
}

pub(super) struct View {
    pub cache: usize,
    pub xml: String,
}

pub(super) fn read_views(
    slice: &[Rec<'_>],
    caches: &[RCache],
    sheet: &str,
    warnings: &mut Vec<String>,
) -> Vec<View> {
    let mut out = Vec::new();
    let mut i = 0;
    while i < slice.len() {
        if slice[i].id != 0xB0 {
            i += 1;
            continue;
        }
        let start = i;
        i += 1;
        while i < slice.len() && slice[i].id != 0xB0 && slice[i].id != 0x23E {
            i += 1;
        }
        match view(&slice[start..i], caches) {
            Some(v) => out.push(v),
            None => warnings.push(format!("{sheet}: pivot table not read")),
        }
    }
    out
}

fn view(recs: &[Rec<'_>], caches: &[RCache]) -> Option<View> {
    let mut c = Rd::new(recs[0].d);
    let (r0, r1, c0, c1) = (c.u16(), c.u16(), c.u16(), c.u16());
    let (hdr, dat, dcol) = (c.u16(), c.u16(), c.u16());
    let cache = usize::from(c.u16());
    c.u16();
    let axis4 = c.u16();
    c.u16();
    let (n_dim, n_rw, n_col, n_pg, n_data) = (
        usize::from(c.u16()),
        usize::from(c.u16()),
        usize::from(c.u16()),
        usize::from(c.u16()),
        usize::from(c.u16()),
    );
    let (_c_rw, _c_col) = (c.u16(), c.u16());
    let grbit = c.u16();
    c.u16();
    let (n_tbl, n_dc) = (usize::from(c.u16()), usize::from(c.u16()));
    let name = c.chars(n_tbl);
    let data_name = c.chars(n_dc);
    let cch = caches.get(cache)?;
    let mut fields = String::new();
    let mut nf = 0;
    let mut ivd: Vec<Vec<i64>> = Vec::new();
    let mut pages = String::new();
    let mut datas = String::new();
    let mut lines: Vec<&Rec<'_>> = Vec::new();
    let (mut page_cols, mut page_rows) = (0u16, 0u16);
    let mut style = String::new();
    let mut open = false;
    let mut items = String::new();
    let mut n_items = 0;
    let close = |fields: &mut String, items: &mut String, n: usize, open: &mut bool| {
        if *open {
            if n > 0 {
                let _ = write!(fields, "<items count=\"{n}\">{items}</items>");
            }
            fields.push_str("</pivotField>");
            items.clear();
            *open = false;
        }
    };
    for r in &recs[1..] {
        let d = whole(r);
        let mut c = Rd::new(&d);
        match r.id {
            0xB1 => {
                close(&mut fields, &mut items, n_items, &mut open);
                let axis = c.u16();
                c.u16();
                let sub = c.u16();
                n_items = usize::from(c.u16());
                let nm = c.opt();
                nf += 1;
                fields.push_str("<pivotField");
                if let Some(n) = nm {
                    let _ = write!(fields, " name=\"{}\"", esc(&n));
                }
                match axis & 7 {
                    1 => fields.push_str(" axis=\"axisRow\""),
                    2 => fields.push_str(" axis=\"axisCol\""),
                    4 => fields.push_str(" axis=\"axisPage\""),
                    _ => {}
                }
                if axis & 8 != 0 {
                    fields.push_str(" dataField=\"1\"");
                }
                fields.push_str(" showAll=\"0\"");
                for (k, a) in SUBTOTALS.iter().enumerate() {
                    if sub & (2 << k) != 0 {
                        let _ = write!(fields, " {a}Subtotal=\"1\"");
                    }
                }
                if sub & 1 == 0 {
                    fields.push_str(" defaultSubtotal=\"0\"");
                }
                fields.push('>');
                open = true;
            }
            0xB2 => {
                let t = c.u16();
                let fl = c.u16();
                let x = c.u16();
                let n = c.opt();
                items.push_str("<item");
                if let Some(n) = n {
                    let _ = write!(items, " n=\"{}\"", esc(&n));
                }
                if t != 0 {
                    let _ = write!(items, " t=\"{}\"", item_t(t));
                }
                if fl & 1 != 0 {
                    items.push_str(" h=\"1\"");
                }
                if fl & 2 != 0 {
                    items.push_str(" sd=\"0\"");
                }
                if fl & 8 != 0 {
                    items.push_str(" f=\"1\"");
                }
                if fl & 0x10 != 0 {
                    items.push_str(" m=\"1\"");
                }
                if t == 0 {
                    let _ = write!(items, " x=\"{x}\"");
                }
                items.push_str("/>");
            }
            0xB4 => {
                let v: Vec<i64> = (0..d.len() / 2)
                    .map(|_| {
                        let x = c.u16();
                        if x == 0xFFFE {
                            -2
                        } else {
                            i64::from(x)
                        }
                    })
                    .collect();
                ivd.push(v);
            }
            0xB6 => {
                let (f, it) = (c.u16(), c.u16());
                let _ = write!(pages, "<pageField fld=\"{f}\"");
                if it != 0x7FFD {
                    let _ = write!(pages, " item=\"{it}\"");
                }
                pages.push_str(" hier=\"-1\"/>");
            }
            0xC5 => {
                let (f, func, show, bf, bi, nf_) =
                    (c.u16(), c.u16(), c.u16(), c.u16(), c.u16(), c.u16());
                let cap = c.st();
                let _ = write!(datas, "<dataField name=\"{}\" fld=\"{f}\"", esc(&cap));
                if func != 0 {
                    let _ = write!(
                        datas,
                        " subtotal=\"{}\"",
                        DATA_FUNCS.get(usize::from(func)).unwrap_or(&"sum")
                    );
                }
                if show != 0 {
                    let _ = write!(
                        datas,
                        " showDataAs=\"{}\"",
                        SHOW_AS.get(usize::from(show)).unwrap_or(&"normal")
                    );
                }
                let _ = write!(datas, " baseField=\"{bf}\" baseItem=\"{bi}\"");
                if nf_ != 0 {
                    let _ = write!(datas, " numFmtId=\"{nf_}\"");
                }
                datas.push_str("/>");
            }
            0xB5 => lines.push(r),
            0xF1 => {
                c.u16();
                for _ in 0..6 {
                    c.u8();
                }
                c.u16();
                page_cols = c.u16();
                page_rows = c.u16();
            }

            0x864 if d.get(4) == Some(&0) && d.get(5) == Some(&0x1E) => {
                let mut c = Rd::new(&d[12..]);
                c.u16();
                let n = usize::from(c.u16());
                let s: Vec<u16> = (0..n).map(|_| c.u16()).collect();
                style = String::from_utf16_lossy(&s);
            }
            _ => {}
        }
    }
    close(&mut fields, &mut items, n_items, &mut open);
    if nf != n_dim {
        return None;
    }
    let mut ivd = ivd.into_iter();
    let rows_f = if n_rw > 0 { ivd.next()? } else { Vec::new() };
    let cols_f = if n_col > 0 { ivd.next()? } else { Vec::new() };
    let _ = n_pg;
    let _ = n_data;
    let line_xml = |r: &Rec<'_>, nf: usize| -> String {
        let d = whole(r);
        let mut s = String::new();
        let mut p = 0;
        let sz = 8 + 2 * nf;
        while p + 8 <= d.len() {
            let mut c = Rd::new(&d[p..]);
            let (rr, t, sic, fl) = (c.u16(), c.u16(), c.u16(), c.u16());
            let idx: Vec<u16> = (0..nf).map(|_| c.u16()).collect();
            s.push_str("<i");
            if rr != 0 {
                let _ = write!(s, " r=\"{rr}\"");
            }
            if t != 0 {
                let _ = write!(s, " t=\"{}\"", item_t(t));
            }
            let di = (fl & 0x01FE) >> 1;
            if di != 0 {
                let _ = write!(s, " i=\"{di}\"");
            }
            s.push('>');
            for k in usize::from(rr)..usize::from(sic) {
                match idx.get(k) {
                    Some(0) | None => s.push_str("<x/>"),
                    Some(v) => {
                        let _ = write!(s, "<x v=\"{v}\"/>");
                    }
                }
            }
            s.push_str("</i>");
            p += sz;
        }
        s
    };
    let mut ln = lines.into_iter();
    let rows_x = ln.next().map(|r| line_xml(r, rows_f.len()));
    let cols_x = ln.next().map(|r| line_xml(r, cols_f.len()));
    let mut x = format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<pivotTableDefinition xmlns=\"{MAIN}\" name=\"{}\" cacheId=\"{}\" applyNumberFormats=\"0\" applyBorderFormats=\"0\" applyFontFormats=\"0\" applyPatternFormats=\"0\" applyAlignmentFormats=\"0\" applyWidthHeightFormats=\"1\" dataCaption=\"{}\" updatedVersion=\"8\" minRefreshableVersion=\"3\"",
        esc(&name),
        cache + 1,
        esc(&data_name)
    );
    if axis4 == 1 {
        x.push_str(" dataOnRows=\"1\"");
    }
    if grbit & 1 == 0 {
        x.push_str(" rowGrandTotals=\"0\"");
    }
    if grbit & 2 == 0 {
        x.push_str(" colGrandTotals=\"0\"");
    }
    if grbit & 8 != 0 {
        x.push_str(" useAutoFormatting=\"1\"");
    }
    x.push_str(" itemPrintTitles=\"1\" createdVersion=\"8\" indent=\"0\" outline=\"1\" outlineData=\"1\" multipleFieldFilters=\"0\">");
    let _ = write!(
        x,
        "<location ref=\"{}\" firstHeaderRow=\"{}\" firstDataRow=\"{}\" firstDataCol=\"{}\"",
        crate::write::rect_text(&xlcalc::Rect::new(
            u32::from(r0),
            u32::from(c0),
            u32::from(r1),
            u32::from(c1)
        )),
        hdr.wrapping_sub(r0),
        dat.wrapping_sub(r0),
        dcol.wrapping_sub(c0)
    );
    if page_rows != 0 {
        let _ = write!(x, " rowPageCount=\"{page_rows}\"");
    }
    if page_cols != 0 {
        let _ = write!(x, " colPageCount=\"{page_cols}\"");
    }
    x.push_str("/>");
    let _ = write!(x, "<pivotFields count=\"{nf}\">{fields}</pivotFields>");
    let fl = |tag: &str, v: &[i64]| -> String {
        if v.is_empty() {
            return String::new();
        }
        let mut s = format!("<{tag} count=\"{}\">", v.len());
        for f in v {
            let _ = write!(s, "<field x=\"{f}\"/>");
        }
        let _ = write!(s, "</{tag}>");
        s
    };
    x.push_str(&fl("rowFields", &rows_f));
    if let Some(r) = rows_x {
        let _ = write!(
            x,
            "<rowItems count=\"{}\">{r}</rowItems>",
            r.matches("<i").count()
        );
    }
    x.push_str(&fl("colFields", &cols_f));
    if let Some(r) = cols_x {
        let _ = write!(
            x,
            "<colItems count=\"{}\">{r}</colItems>",
            r.matches("<i").count()
        );
    }
    if !pages.is_empty() {
        let _ = write!(
            x,
            "<pageFields count=\"{}\">{pages}</pageFields>",
            pages.matches("<pageField").count()
        );
    }
    if !datas.is_empty() {
        let _ = write!(
            x,
            "<dataFields count=\"{}\">{datas}</dataFields>",
            datas.matches("<dataField ").count()
        );
    }
    if !style.is_empty() {
        let _ = write!(
            x,
            "<pivotTableStyleInfo name=\"{}\" showRowHeaders=\"1\" showColHeaders=\"1\" showRowStripes=\"0\" showColStripes=\"0\" showLastColumn=\"1\"/>",
            esc(&style)
        );
    }
    x.push_str("</pivotTableDefinition>");
    let _ = cch;
    Some(View { cache, xml: x })
}
