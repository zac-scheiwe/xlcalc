use super::cur::Cur;
use super::rstyles::Styles;
use super::{ptg, Rec};
use crate::xlsb::{esc, rels_xml, xstr, Book, Dec, Formulas, Pkg};
use std::collections::HashMap;
use std::fmt::Write as _;
use std::io::Write as _;
use xlcalc_vbaproject::cfb::{Cfb, CfbBuilder};

const REL: &str = "http://schemas.openxmlformats.org/officeDocument/2006/relationships";
const MAIN: &str = "http://schemas.openxmlformats.org/spreadsheetml/2006/main";

struct NameRec {
    name: String,
    hidden: bool,
    scope: Option<u32>,
    rgce: Vec<u8>,
    extra: Vec<u8>,
}

struct SheetRec {
    name: String,
    state: u8,
    kind: u8,
    offset: usize,
}

#[derive(Default)]
struct SupBook {
    internal: bool,
    addin: bool,
    names: Vec<String>,

    url: Option<String>,
    sheets: Vec<String>,
    name_refs: Vec<(u32, String)>,
    cells: Vec<(u32, u32, u32, &'static str, String)>,
    xct: u32,
}

pub(super) const BUILTIN_NAMES: [&str; 14] = [
    "Consolidate_Area",
    "Auto_Open",
    "Auto_Close",
    "Extract",
    "Database",
    "Criteria",
    "Print_Area",
    "Print_Titles",
    "Recorder",
    "Data_Form",
    "Auto_Activate",
    "Auto_Deactivate",
    "Sheet_Title",
    "_FilterDatabase",
];

pub(super) fn records(b: &[u8]) -> Vec<Rec<'_>> {
    let mut out: Vec<Rec<'_>> = Vec::new();
    let mut p = 0usize;
    while p + 4 <= b.len() {
        let id = u16::from_le_bytes([b[p], b[p + 1]]);
        let len = usize::from(u16::from_le_bytes([b[p + 2], b[p + 3]]));
        let end = (p + 4 + len).min(b.len());
        let d = &b[p + 4..end];
        p = end;
        if id == 0x3C {
            if let Some(last) = out.last_mut() {
                last.cont.push(d);
                continue;
            }
        }
        out.push(Rec {
            id,
            d,
            cont: Vec::new(),
        });
    }
    out
}

pub(super) fn ref8(c: &mut Cur<'_>) -> Result<(u32, u32, u32, u32), String> {
    let r0 = u32::from(c.u16()?);
    let r1 = u32::from(c.u16()?);
    let c0 = u32::from(c.u16()?);
    let c1 = u32::from(c.u16()?);
    Ok((r0, c0, r1, c1))
}

fn read_sst(r: &Rec<'_>, styles: &Styles) -> Result<Vec<String>, String> {
    let mut chunks: Vec<&[u8]> = vec![r.d];
    chunks.extend(r.cont.iter().copied());
    let mut ci = 0usize;
    let mut pos = 8usize.min(chunks[0].len());
    let total = u32::from_le_bytes(chunks[0][4..8].try_into().map_err(|_| "short SST")?) as usize;
    let _ = total;
    let unique = u32::from_le_bytes(chunks[0][4..8].try_into().map_err(|_| "short SST")?) as usize;
    let mut out = Vec::new();

    macro_rules! avail {
        () => {
            while ci < chunks.len() && pos >= chunks[ci].len() {
                ci += 1;
                pos = 0;
                if ci >= chunks.len() {
                    break;
                }
            }
            if ci >= chunks.len() {
                return Ok(out);
            }
        };
    }
    for _ in 0..unique {
        avail!();
        let b = chunks[ci];
        if pos + 3 > b.len() {
            break;
        }
        let cch = usize::from(u16::from_le_bytes([b[pos], b[pos + 1]]));
        let flags = b[pos + 2];
        pos += 3;
        let mut runs = 0usize;
        let mut ext = 0usize;
        {
            let b = chunks[ci];
            if flags & 8 != 0 {
                runs = usize::from(u16::from_le_bytes([b[pos], b[pos + 1]]));
                pos += 2;
            }
            if flags & 4 != 0 {
                ext = u32::from_le_bytes([b[pos], b[pos + 1], b[pos + 2], b[pos + 3]]) as usize;
                pos += 4;
            }
        }
        let mut wide = flags & 1 != 0;
        let mut text = String::new();
        let mut left = cch;
        let mut units: Vec<u16> = Vec::new();
        while left > 0 {
            if pos >= chunks[ci].len() {
                ci += 1;
                if ci >= chunks.len() {
                    return Ok(out);
                }
                wide = chunks[ci].first().is_some_and(|f| f & 1 != 0);
                pos = 1;
            }
            let b = chunks[ci];
            if wide {
                let n = left.min((b.len() - pos) / 2);
                if n == 0 {
                    break;
                }
                for k in 0..n {
                    units.push(u16::from_le_bytes([b[pos + 2 * k], b[pos + 2 * k + 1]]));
                }
                pos += 2 * n;
                left -= n;
            } else {
                let n = left.min(b.len() - pos);
                for k in 0..n {
                    units.push(u16::from(b[pos + k]));
                }
                pos += n;
                left -= n;
            }
        }
        text.push_str(&String::from_utf16_lossy(&units));

        let mut run_list: Vec<(usize, u16)> = Vec::new();
        let mut need = runs * 4;
        let mut buf: Vec<u8> = Vec::new();
        while need > 0 {
            if pos >= chunks[ci].len() {
                ci += 1;
                pos = 0;
                if ci >= chunks.len() {
                    return Ok(out);
                }
            }
            let n = need.min(chunks[ci].len() - pos);
            buf.extend_from_slice(&chunks[ci][pos..pos + n]);
            pos += n;
            need -= n;
        }
        for q in buf.as_chunks::<4>().0 {
            run_list.push((
                usize::from(u16::from_le_bytes([q[0], q[1]])),
                u16::from_le_bytes([q[2], q[3]]),
            ));
        }
        let mut need = ext;
        while need > 0 {
            if pos >= chunks[ci].len() {
                ci += 1;
                pos = 0;
                if ci >= chunks.len() {
                    break;
                }
            }
            let n = need.min(chunks[ci].len() - pos);
            pos += n;
            need -= n;
        }
        out.push(si_xml(&text, &run_list, styles));
    }
    Ok(out)
}

pub(super) fn si_xml(text: &str, runs: &[(usize, u16)], styles: &Styles) -> String {
    if runs.is_empty() {
        return format!("<si><t xml:space=\"preserve\">{}</t></si>", xstr(text));
    }
    let chars: Vec<char> = text.chars().collect();
    let mut s = String::from("<si>");
    if runs[0].0 > 0 {
        let head: String = chars[..runs[0].0.min(chars.len())].iter().collect();
        let _ = write!(s, "<r><t xml:space=\"preserve\">{}</t></r>", xstr(&head));
    }
    for (i, (from, font)) in runs.iter().enumerate() {
        let to = runs
            .get(i + 1)
            .map_or(chars.len(), |r| r.0)
            .min(chars.len());
        let from = (*from).min(to);
        let part: String = chars[from..to].iter().collect();
        let _ = write!(
            s,
            "<r>{}<t xml:space=\"preserve\">{}</t></r>",
            styles.rpr(*font),
            xstr(&part)
        );
    }
    s.push_str("</si>");
    s
}

struct Book8 {
    cp: u16,
    date1904: bool,

    protect: (bool, bool, bool, u16),
    styles: Styles,
    sheets: Vec<SheetRec>,
    sst: Vec<String>,
    supbooks: Vec<SupBook>,
    xti: Vec<(u32, i32, i32)>,
    names: Vec<NameRec>,
    calc: String,
    active_tab: u16,
    first_tab: u16,
    code_name: Option<String>,
    window_flags: u16,
    blips: Vec<Option<super::rdraw::Blip>>,
}

fn read_globals(recs: &[Rec<'_>]) -> Result<Book8, String> {
    let mut b = Book8 {
        cp: 1252,
        date1904: false,
        protect: (false, false, false, 0),
        styles: Styles::default(),
        sheets: Vec::new(),
        sst: Vec::new(),
        supbooks: Vec::new(),
        xti: Vec::new(),
        names: Vec::new(),
        calc: String::new(),
        active_tab: 0,
        first_tab: 0,
        code_name: None,
        window_flags: 0x38,
        blips: Vec::new(),
    };
    let (mut calc_mode, mut calc_count, mut delta) = (1i16, 100u16, 0.001f64);
    let (mut iter, mut precision, mut recalc) = (false, true, false);
    let mut calc_id: Option<u32> = None;
    let mut sst_rec: Option<usize> = None;
    let mut depth = 0i32;
    for (i, r) in recs.iter().enumerate() {
        let mut c = Cur::new(r.d);
        match r.id {
            0x809 => depth += 1,
            0x0A => {
                depth -= 1;
                if depth <= 0 {
                    break;
                }
            }
            _ if depth > 1 => {}
            0xEB => {
                let mut d = r.d.to_vec();
                for k in &r.cont {
                    d.extend_from_slice(k);
                }
                b.blips = super::rdraw::blips(&d);
            }
            0x42 => b.cp = c.u16()?,
            0x22 => b.date1904 = c.u16()? != 0,
            0x12 => b.protect.0 = c.u16()? != 0,
            0x19 => b.protect.1 = c.u16()? != 0,
            0x1AF => b.protect.2 = c.u16()? != 0,
            0x13 => b.protect.3 = c.u16()?,
            0x31 => b.styles.font(r.d, b.cp)?,
            0x41E => {
                let id = c.u16()?;
                let s = c.long_str(b.cp)?;
                b.styles.formats.push((id, s));
            }
            0xE0 => b.styles.xf(r.d)?,
            0x87D => b.styles.xf_ext(r.d)?,
            0x293 => b.styles.style(r.d, b.cp)?,
            0x88D => b.styles.dxf_record(r.d)?,
            0x88E => b.styles.table_styles_record(r.d)?,
            0x88F => b.styles.table_style_record(r.d)?,
            0x890 => b.styles.table_style_element(r.d)?,
            0x92 => b.styles.palette(r.d)?,
            0x85 => {
                let off = c.u32()? as usize;
                let state = c.u8()?;
                let kind = c.u8()?;
                let name = c.short_str(b.cp)?;
                b.sheets.push(SheetRec {
                    name,
                    state,
                    kind,
                    offset: off,
                });
            }
            0xFC => sst_rec = Some(i),
            0x1AE => {
                let ctab = c.u16()?;
                let cch = c.u16()?;
                let mut sb = SupBook::default();
                if cch == 0x0401 {
                    sb.internal = true;
                } else if cch == 0x3A01 {
                    sb.addin = true;
                }
                if !sb.internal && !sb.addin {
                    let wide = c.u8()? & 1 != 0;
                    let raw = c.chars(usize::from(cch), wide, b.cp)?;
                    sb.url = Some(decode_url(&raw));
                    for _ in 0..ctab {
                        sb.sheets.push(c.long_str(b.cp)?);
                    }
                }
                b.supbooks.push(sb);
            }
            0x23 => {
                c.u16()?;
                c.u32()?;
                let name = c.short_str(b.cp).unwrap_or_default();
                let rest = c.rest();
                let refers = match rest {
                    [lo, hi, tail @ ..] => {
                        let cce = usize::from(u16::from_le_bytes([*lo, *hi]));
                        tail.get(..cce).map(ext_name_ref)
                    }
                    _ => None,
                };
                if let Some(sb) = b.supbooks.last_mut() {
                    sb.names.push(name);
                    sb.name_refs
                        .push(refers.flatten().unwrap_or((u32::MAX, String::new())));
                }
            }
            0x59 => {
                c.u16()?;
                let itab = u32::from(c.u16()?);
                if let Some(sb) = b.supbooks.last_mut() {
                    sb.xct = itab;
                }
            }
            0x5A => {
                let last = u32::from(c.u8()?);
                let first = u32::from(c.u8()?);
                let row = u32::from(c.u16()?);
                let sheet = b.supbooks.last().map_or(0, |s| s.xct);
                for col in first..=last {
                    let ty = c.u8()?;
                    let cell = match ty {
                        0 => {
                            c.take(8)?;
                            None
                        }
                        1 => Some(("n", format!("{}", c.f64()?))),
                        2 => Some(("str", xstr(&c.long_str(b.cp)?))),
                        4 => {
                            let v = c.u8()?;
                            c.take(7)?;
                            Some(("b", v.to_string()))
                        }
                        0x10 => {
                            let v = c.u8()?;
                            c.take(7)?;
                            crate::xlsb::error_name(v).map(|n| ("e", n.to_string()))
                        }
                        _ => break,
                    };
                    if let (Some((t, v)), Some(sb)) = (cell, b.supbooks.last_mut()) {
                        sb.cells.push((sheet, row, col, t, v));
                    }
                }
            }
            0x17 => {
                let n = c.u16()?;
                for _ in 0..n {
                    let sb = c.u16()?;
                    let a = c.i16()?;
                    let z = c.i16()?;
                    b.xti.push((u32::from(sb), i32::from(a), i32::from(z)));
                }
            }
            0x18 => {
                let flags = c.u16()?;
                c.u8()?;
                let cch = usize::from(c.u8()?);
                let cce = usize::from(c.u16()?);
                c.u16()?;
                let itab = c.u16()?;
                let skip = (0..4)
                    .map(|_| c.u8().map(usize::from))
                    .collect::<Result<Vec<_>, _>>()?;
                let raw = if cch > 0 {
                    let fl = c.u8()?;
                    c.chars(cch, fl & 1 != 0, b.cp)?
                } else {
                    String::new()
                };
                let name = if flags & 0x20 != 0 {
                    let code = raw.chars().next().map_or(0, |ch| ch as usize);
                    match BUILTIN_NAMES.get(code) {
                        Some(n) => format!("_xlnm.{n}"),
                        None => raw,
                    }
                } else {
                    raw
                };
                let _ = skip;
                let rgce = c.take(cce)?.to_vec();
                b.names.push(NameRec {
                    name,
                    hidden: flags & 1 != 0,
                    scope: (itab != 0).then(|| u32::from(itab) - 1),
                    rgce,
                    extra: c.rest().to_vec(),
                });
            }
            0x0D => calc_mode = c.i16()?,
            0x0C => calc_count = c.u16()?,
            0x11 => iter = c.u16()? != 0,
            0x10 => delta = c.f64()?,
            0x0E => precision = c.u16()? != 0,
            0x5F => recalc = c.u16()? != 0,
            0x1C1 => {
                c.u32()?;
                calc_id = Some(c.u32()? & 0x3FFFF);
            }
            0x3D => {
                c.take(8)?;
                b.window_flags = c.u16()?;
                b.active_tab = c.u16()?;
                b.first_tab = c.u16()?;
            }
            0x1BA => {
                let n = usize::from(c.u16()?);
                let n = c.xl_chars(n, b.cp)?;
                if !n.is_empty() {
                    b.code_name = Some(n);
                }
            }
            _ => {}
        }
    }
    if let Some(i) = sst_rec {
        b.sst = read_sst(&recs[i], &b.styles)?;
    }
    let mut calc = String::new();
    if let Some(id) = calc_id {
        let _ = write!(calc, " calcId=\"{id}\"");
    }
    match calc_mode {
        0 => calc.push_str(" calcMode=\"manual\""),
        -1 => calc.push_str(" calcMode=\"autoNoTable\""),
        _ => {}
    }
    if iter {
        let _ = write!(
            calc,
            " iterate=\"1\" iterateCount=\"{calc_count}\" iterateDelta=\"{delta}\""
        );
    }
    if !precision {
        calc.push_str(" fullPrecision=\"0\"");
    }
    if recalc {
        calc.push_str(" calcOnSave=\"1\"");
    } else {
        calc.push_str(" calcOnSave=\"0\"");
    }
    b.calc = calc;
    Ok(b)
}

pub(super) struct Fx {
    pub f: Formulas,

    addin: Vec<Vec<String>>,
    addin_at: HashMap<(u16, u16), u32>,
    supbook_addin: Vec<bool>,
    base_names: usize,
}

impl Fx {
    pub fn transcode(
        &mut self,
        rgce: &[u8],
        extra: &[u8],
        base: (u32, u32),
        cp: u16,
    ) -> Result<(Vec<u8>, Vec<u8>), String> {
        let xti = self.f.xti.clone();
        let mut hook = |ixti: u16, idx: u16| -> Option<u32> {
            let (sb, _, _) = *xti.get(usize::from(ixti))?;
            if !*self.supbook_addin.get(sb as usize)? {
                return None;
            }
            if let Some(&n) = self.addin_at.get(&(sb as u16, idx)) {
                return Some(n);
            }
            let name = self
                .addin
                .get(sb as usize)?
                .get(usize::from(idx).checked_sub(1)?)?;
            self.f.names.push((name.clone(), None));
            let n = self.f.names.len() as u32;
            self.addin_at.insert((sb as u16, idx), n);
            Some(n)
        };
        ptg::to_biff12(rgce, extra, base, cp, &mut hook)
    }

    pub fn text(
        &mut self,
        rgce: &[u8],
        extra: &[u8],
        base: (u32, u32),
        cur: Option<u32>,
        cp: u16,
    ) -> Result<Dec, String> {
        let (a, b) = self.transcode(rgce, extra, base, cp)?;
        self.f.decode(&a, &b, base, cur)
    }
}

pub(crate) fn is_cfb(b: &[u8]) -> bool {
    b.starts_with(&[0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1])
}

fn workbook_stream(cfb: &Cfb) -> Option<Vec<u8>> {
    cfb.read_stream("Workbook")
        .or_else(|_| cfb.read_stream("Book"))
        .ok()
}

pub(crate) fn to_xlsx(bytes: &[u8]) -> Result<(Vec<u8>, Vec<String>), String> {
    let cfb = Cfb::new(bytes.to_vec()).map_err(|e| e.to_string())?;
    let stream = workbook_stream(&cfb).ok_or("no Workbook stream: not an Excel workbook")?;
    let recs = records(&stream);
    let mut warnings: Vec<String> = Vec::new();

    let mut offsets: Vec<usize> = Vec::with_capacity(recs.len());
    {
        let mut p = 0usize;
        let mut ri = 0usize;
        while p + 4 <= stream.len() && ri < recs.len() {
            offsets.push(p);
            let mut q = p;

            let len = usize::from(u16::from_le_bytes([stream[q + 2], stream[q + 3]]));
            q += 4 + len;
            for _ in &recs[ri].cont {
                let l = usize::from(u16::from_le_bytes([stream[q + 2], stream[q + 3]]));
                q += 4 + l;
            }
            p = q;
            ri += 1;
        }
    }
    if recs.first().map(|r| r.id) != Some(0x809) {
        return Err("not a BIFF workbook (no BOF)".into());
    }
    let ver = recs[0]
        .d
        .get(..2)
        .map_or(0, |v| u16::from_le_bytes([v[0], v[1]]));
    if ver != 0x600 {
        return Err(format!(
            "BIFF version {ver:#x} is not supported (only Excel 97 and later, BIFF8)"
        ));
    }
    let mut book = read_globals(&recs)?;
    let sheet_names: Vec<String> = book.sheets.iter().map(|s| s.name.clone()).collect();

    let mut n_ext = 0;
    let mut fx = Fx {
        f: Formulas {
            sheets: sheet_names.clone(),
            xti: book.xti.clone(),
            books: book
                .supbooks
                .iter()
                .map(|s| {
                    if s.internal {
                        Book::This
                    } else if s.url.is_some() {
                        n_ext += 1;
                        Book::Ext(n_ext)
                    } else {
                        Book::Other
                    }
                })
                .collect(),
            names: Vec::new(),
            ext: ext_books(&book.supbooks),
        },
        addin: book.supbooks.iter().map(|s| s.names.clone()).collect(),
        addin_at: HashMap::new(),
        supbook_addin: book.supbooks.iter().map(|s| s.addin).collect(),
        base_names: book.names.len(),
    };
    for n in &book.names {
        fx.f.names.push((n.name.clone(), n.scope));
    }

    let mut pkg = Pkg { parts: Vec::new() };
    let mut wb_rel_items: Vec<(String, String, String)> = Vec::new();
    let mut sheets_xml = String::new();
    let mut sheet_no = 0usize;
    let mut vba_sheet_codes: Vec<(String, String)> = Vec::new();
    let pv_end = recs.iter().position(|r| r.id == 0x0A).unwrap_or(recs.len());
    let pv_caches = super::rpivot::read_caches(&recs[..pv_end], &cfb, &mut warnings);
    let mut pv_tables = 0usize;
    for (si, sh) in book.sheets.iter().enumerate() {
        if sh.kind == 6 {
            continue;
        }
        let start = offsets
            .iter()
            .position(|&o| o == sh.offset)
            .ok_or_else(|| format!("sheet {}: bad stream offset", sh.name))?;
        let mut end = start;
        let mut depth = 0i32;
        for (k, r) in recs.iter().enumerate().skip(start) {
            if r.id == 0x809 {
                depth += 1;
            } else if r.id == 0x0A {
                depth -= 1;
                if depth == 0 {
                    end = k;
                    break;
                }
            }
        }
        sheet_no += 1;
        let n = sheet_no;
        let slice = &recs[start..=end];
        let kind_dir;
        let xml;
        let mut items: Vec<(String, String, String)> = Vec::new();
        if sh.kind == 2 {
            kind_dir = "chartsheets";
            let mut crecs: super::rchart::ChartRecs = Vec::new();
            for r in slice {
                if r.id != 0x809 && r.id != 0x0A {
                    let mut d = r.d.to_vec();
                    for k in &r.cont {
                        d.extend_from_slice(k);
                    }
                    crecs.push((r.id, d));
                }
            }
            let what = format!("chart sheet {}", sh.name);
            let chart =
                super::rchart::to_xml(&crecs, &mut fx, book.cp, si as u32, &what, &mut warnings);
            let mut drawing_tag = String::new();
            if let Some(cx) = chart {
                let pt = |at: usize| {
                    crecs
                        .iter()
                        .find(|r| r.0 == 0x1002 && r.1.len() >= 16)
                        .map_or(0.0, |r| {
                            f64::from(u32::from_le_bytes([
                                r.1[at],
                                r.1[at + 1],
                                r.1[at + 2],
                                r.1[at + 3],
                            ])) / 65536.0
                        })
                };
                let (cx_emu, cy_emu) = ((pt(8) * 12700.0) as i64, (pt(12) * 12700.0) as i64);
                pkg.text(&format!("xl/charts/chartsheet{n}.xml"), cx);
                pkg.text(
                    &format!("xl/drawings/chartsheet{n}.xml"),
                    format!(
                        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<xdr:wsDr xmlns:xdr=\"http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><xdr:absoluteAnchor><xdr:pos x=\"0\" y=\"0\"/><xdr:ext cx=\"{cx_emu}\" cy=\"{cy_emu}\"/><xdr:graphicFrame macro=\"\"><xdr:nvGraphicFramePr><xdr:cNvPr id=\"2\" name=\"Chart 1\"/><xdr:cNvGraphicFramePr/></xdr:nvGraphicFramePr><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"0\" cy=\"0\"/></xdr:xfrm><a:graphic><a:graphicData uri=\"http://schemas.openxmlformats.org/drawingml/2006/chart\"><c:chart xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:r=\"{REL}\" r:id=\"rId1\"/></a:graphicData></a:graphic></xdr:graphicFrame><xdr:clientData/></xdr:absoluteAnchor></xdr:wsDr>"
                    ),
                );
                pkg.text(
                    &format!("xl/drawings/_rels/chartsheet{n}.xml.rels"),
                    rels_xml(&[(
                        "rId1".to_string(),
                        format!("{REL}/chart"),
                        format!("../charts/chartsheet{n}.xml"),
                    )]),
                );
                items.push((
                    "rId1".to_string(),
                    format!("{REL}/drawing"),
                    format!("../drawings/chartsheet{n}.xml"),
                ));
                drawing_tag = "<drawing r:id=\"rId1\"/>".to_string();
            }
            xml = format!(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<chartsheet xmlns=\"{MAIN}\" xmlns:r=\"{REL}\"><sheetViews><sheetView workbookViewId=\"0\"/></sheetViews><pageMargins left=\"0.7\" right=\"0.7\" top=\"0.75\" bottom=\"0.75\" header=\"0.3\" footer=\"0.3\"/>{drawing_tag}</chartsheet>"
            );
        } else {
            kind_dir = "worksheets";
            let out = super::sheet::read_sheet(
                slice,
                &book.sst,
                &mut fx,
                book.cp,
                si as u32,
                &sh.name,
                &mut warnings,
                &mut book.styles,
                &book.blips,
            )?;
            xml = out.xml;
            for (id, ty, target) in out.rels {
                items.push((id, ty, target));
            }
            for (name, bytes) in out.parts {
                if !pkg
                    .parts
                    .iter()
                    .any(|p| p.0 == name && name.starts_with("xl/media/"))
                {
                    pkg.parts.push((name, bytes));
                }
            }
            if let Some(code) = out.code_name {
                vba_sheet_codes.push((sh.name.clone(), code));
            }
            for v in super::rpivot::read_views(slice, &pv_caches, &sh.name, &mut warnings) {
                pv_tables += 1;
                let k = pv_tables;
                pkg.text(&format!("xl/pivotTables/pivotTable{k}.xml"), v.xml);
                pkg.text(
                    &format!("xl/pivotTables/_rels/pivotTable{k}.xml.rels"),
                    rels_xml(&[(
                        "rId1".to_string(),
                        format!("{REL}/pivotCacheDefinition"),
                        format!("../pivotCache/pivotCacheDefinition{}.xml", v.cache + 1),
                    )]),
                );
                items.push((
                    format!("rId{}", items.len() + 1),
                    format!("{REL}/pivotTable"),
                    format!("../pivotTables/pivotTable{k}.xml"),
                ));
            }
        }
        pkg.text(&format!("xl/{kind_dir}/sheet{n}.xml"), xml);
        if !items.is_empty() {
            pkg.text(
                &format!("xl/{kind_dir}/_rels/sheet{n}.xml.rels"),
                rels_xml(&items),
            );
        }
        let st = match sh.state {
            1 => " state=\"hidden\"",
            2 => " state=\"veryHidden\"",
            _ => "",
        };
        let _ = write!(
            sheets_xml,
            "<sheet name=\"{}\" sheetId=\"{n}\"{st} r:id=\"rId{n}\"/>",
            esc(&sh.name)
        );
        wb_rel_items.push((
            format!("rId{n}"),
            format!(
                "{REL}/{}",
                if sh.kind == 2 {
                    "chartsheet"
                } else {
                    "worksheet"
                }
            ),
            format!("/xl/{kind_dir}/sheet{n}.xml"),
        ));
    }

    let mut pv_caches_xml = String::new();
    for (i, c) in pv_caches.iter().enumerate() {
        let n = i + 1;
        let (def, rec) = super::rpivot::cache_parts(c);
        pkg.text(&format!("xl/pivotCache/pivotCacheDefinition{n}.xml"), def);
        pkg.text(&format!("xl/pivotCache/pivotCacheRecords{n}.xml"), rec);
        pkg.text(
            &format!("xl/pivotCache/_rels/pivotCacheDefinition{n}.xml.rels"),
            rels_xml(&[(
                "rId1".to_string(),
                format!("{REL}/pivotCacheRecords"),
                format!("pivotCacheRecords{n}.xml"),
            )]),
        );
        let rid = format!("rId{}", wb_rel_items.len() + 1);
        wb_rel_items.push((
            rid.clone(),
            format!("{REL}/pivotCacheDefinition"),
            format!("/xl/pivotCache/pivotCacheDefinition{n}.xml"),
        ));
        let _ = write!(
            pv_caches_xml,
            "<pivotCache cacheId=\"{n}\" r:id=\"{rid}\"/>"
        );
    }
    if !pv_caches_xml.is_empty() {
        pv_caches_xml = format!("<pivotCaches>{pv_caches_xml}</pivotCaches>");
    }

    let mut dn = String::new();
    for (i, n) in book.names.iter().enumerate() {
        let _ = i;
        if n.name.starts_with("_xlfn.") || n.name.starts_with("_xlpm.") || n.rgce.is_empty() {
            continue;
        }
        let text = match fx.text(&n.rgce, &n.extra, (0, 0), n.scope, book.cp) {
            Ok(Dec::Text(t)) => t,
            Ok(Dec::Exp { .. }) => continue,
            Err(e) => {
                warnings.push(format!("name {}: formula not supported: {e}", n.name));
                continue;
            }
        };
        let scope = n
            .scope
            .map(|s| format!(" localSheetId=\"{s}\""))
            .unwrap_or_default();
        let hidden = if n.hidden { " hidden=\"1\"" } else { "" };
        let _ = write!(
            dn,
            "<definedName name=\"{}\"{scope}{hidden}>{}</definedName>",
            esc(&n.name),
            esc(&text)
        );
    }
    let _ = fx.base_names;

    let mut ext_refs = String::new();
    for (i, e) in fx.f.ext.iter().enumerate() {
        let n = i + 1;
        let rid = format!("rId{}", wb_rel_items.len() + 1);
        wb_rel_items.push((
            rid.clone(),
            format!("{REL}/externalLink"),
            format!("/xl/externalLinks/externalLink{n}.xml"),
        ));
        let _ = write!(ext_refs, "<externalReference r:id=\"{rid}\"/>");
        pkg.text(
            &format!("xl/externalLinks/externalLink{n}.xml"),
            e.xml.clone(),
        );
        if let Some(r) = &e.rels {
            pkg.parts.push((
                format!("xl/externalLinks/_rels/externalLink{n}.xml.rels"),
                r.clone(),
            ));
        }
    }
    if !ext_refs.is_empty() {
        ext_refs = format!("<externalReferences>{ext_refs}</externalReferences>");
    }

    let n_items = wb_rel_items.len();
    wb_rel_items.push((
        format!("rId{}", n_items + 1),
        format!("{REL}/sharedStrings"),
        "/xl/sharedStrings.xml".into(),
    ));
    wb_rel_items.push((
        format!("rId{}", n_items + 2),
        format!("{REL}/styles"),
        "/xl/styles.xml".into(),
    ));
    let mut sst = format!("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<sst xmlns=\"{MAIN}\">");
    for s in &book.sst {
        sst.push_str(s);
    }
    sst.push_str("</sst>");
    pkg.text("xl/sharedStrings.xml", sst);
    pkg.text("xl/styles.xml", book.styles.to_xml());
    pkg.text("xl/_rels/workbook.xml.rels", rels_xml(&wb_rel_items));
    let code = book
        .code_name
        .as_ref()
        .map(|c| format!(" codeName=\"{}\"", esc(c)))
        .unwrap_or_default();
    let view = format!(
        "<bookViews><workbookView activeTab=\"{}\" firstSheet=\"{}\"{}{}{}/></bookViews>",
        book.active_tab,
        book.first_tab,
        if book.window_flags & 0x08 == 0 {
            " showHorizontalScroll=\"0\""
        } else {
            ""
        },
        if book.window_flags & 0x10 == 0 {
            " showVerticalScroll=\"0\""
        } else {
            ""
        },
        if book.window_flags & 0x20 == 0 {
            " showSheetTabs=\"0\""
        } else {
            ""
        }
    );
    pkg.text(
        "xl/workbook.xml",
        format!(
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<workbook xmlns=\"{MAIN}\" xmlns:r=\"{REL}\"><workbookPr{}{code}/>{}{view}<sheets>{sheets_xml}</sheets>{ext_refs}{}<calcPr{}/>{pv_caches_xml}</workbook>",
            if book.date1904 { " date1904=\"1\"" } else { "" },
            protect_xml(book.protect),
            if dn.is_empty() {
                String::new()
            } else {
                format!("<definedNames>{dn}</definedNames>")
            },
            book.calc
        ),
    );

    let prefix = "_VBA_PROJECT_CUR/";
    let vba: Vec<String> = cfb
        .streams()
        .into_iter()
        .filter(|s| s.starts_with(prefix))
        .collect();
    if !vba.is_empty() {
        let mut b = CfbBuilder::new();
        for s in vba {
            let data = cfb.read_stream(&s).map_err(|e| e.to_string())?;
            b.add_stream(&s[prefix.len()..], data);
        }
        pkg.parts.push((
            "xl/vbaProject.bin".into(),
            b.finish().map_err(|e| e.to_string())?,
        ));
    }

    let summary = cfb.read_stream(super::props::SUMMARY).ok();
    let docsum = cfb.read_stream(super::props::DOC_SUMMARY).ok();
    for (name, xml) in super::props::to_parts(summary.as_deref(), docsum.as_deref()) {
        pkg.text(name, xml);
    }
    {
        let has = |n: &str| pkg.parts.iter().any(|p| p.0 == n);
        let links: Vec<String> = pkg
            .parts
            .iter()
            .filter(|p| p.0.starts_with("xl/externalLinks/externalLink"))
            .map(|p| p.0.clone())
            .collect();
        let tables: Vec<String> = pkg
            .parts
            .iter()
            .filter(|p| p.0.starts_with("xl/tables/table"))
            .map(|p| p.0.clone())
            .collect();
        let comments: Vec<String> = pkg
            .parts
            .iter()
            .filter(|p| p.0.starts_with("xl/comments"))
            .map(|p| p.0.clone())
            .collect();
        let drawn: Vec<(String, &str)> = pkg
            .parts
            .iter()
            .filter_map(|p| {
                let n = p.0.as_str();
                if n.starts_with("xl/drawings/") && n.ends_with(".xml") {
                    Some((p.0.clone(), crate::drawings::CT_DRAWING))
                } else if n.starts_with("xl/charts/") && n.ends_with(".xml") {
                    Some((p.0.clone(), crate::drawings::CT_CHART))
                } else {
                    None
                }
            })
            .collect();
        let props = ["core", "app", "custom"].map(|k| has(&format!("docProps/{k}.xml")));
        let pivots: Vec<(String, &str)> = pkg
            .parts
            .iter()
            .filter_map(|p| {
                let n = p.0.as_str();
                let t = if n.starts_with("xl/pivotTables/pivotTable") && n.ends_with(".xml") {
                    "pivotTable"
                } else if n.starts_with("xl/pivotCache/pivotCacheDefinition") && n.ends_with(".xml")
                {
                    "pivotCacheDefinition"
                } else if n.starts_with("xl/pivotCache/pivotCacheRecords") {
                    "pivotCacheRecords"
                } else {
                    return None;
                };
                Some((p.0.clone(), t))
            })
            .collect();
        if !comments.is_empty()
            || !pivots.is_empty()
            || !tables.is_empty()
            || !links.is_empty()
            || !drawn.is_empty()
            || props.contains(&true)
        {
            let mut ct = String::from("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"><Default Extension=\"vml\" ContentType=\"application/vnd.openxmlformats-officedocument.vmlDrawing\"/>");
            let mut media_exts: Vec<&str> = Vec::new();
            for (n, _) in pkg.parts.iter().filter(|p| p.0.starts_with("xl/media/")) {
                let e = n.rsplit_once('.').map_or("", |(_, e)| e);
                let t = match e {
                    "png" => "image/png",
                    "jpg" | "jpeg" => "image/jpeg",
                    "gif" => "image/gif",
                    "bmp" => "image/bmp",
                    "tif" | "tiff" => "image/tiff",
                    "emf" => "image/x-emf",
                    "wmf" => "image/x-wmf",
                    _ => continue,
                };
                if !media_exts.contains(&e) {
                    media_exts.push(e);
                    let _ = write!(ct, "<Default Extension=\"{e}\" ContentType=\"{t}\"/>");
                }
            }
            for c in &comments {
                let _ = write!(ct, "<Override PartName=\"/{c}\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.comments+xml\"/>");
            }
            for c in &tables {
                let _ = write!(ct, "<Override PartName=\"/{c}\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.table+xml\"/>");
            }
            for (c, t) in &pivots {
                let _ = write!(ct, "<Override PartName=\"/{c}\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.{t}+xml\"/>");
            }
            for c in &links {
                let _ = write!(ct, "<Override PartName=\"/{c}\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.externalLink+xml\"/>");
            }
            for (c, t) in &drawn {
                let _ = write!(ct, "<Override PartName=\"/{c}\" ContentType=\"{t}\"/>");
            }
            let kinds = [
                (
                    "core",
                    "application/vnd.openxmlformats-package.core-properties+xml",
                ),
                (
                    "app",
                    "application/vnd.openxmlformats-officedocument.extended-properties+xml",
                ),
                (
                    "custom",
                    "application/vnd.openxmlformats-officedocument.custom-properties+xml",
                ),
            ];
            for (i, (k, t)) in kinds.iter().enumerate() {
                if props[i] {
                    let _ = write!(
                        ct,
                        "<Override PartName=\"/docProps/{k}.xml\" ContentType=\"{t}\"/>"
                    );
                }
            }
            ct.push_str("</Types>");
            pkg.text("[Content_Types].xml", ct);
        }
        if props.contains(&true) {
            let mut items = vec![(
                "rId1".to_string(),
                format!("{REL}/officeDocument"),
                "xl/workbook.xml".to_string(),
            )];
            let rels = [
                ("core", "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"),
                ("app", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties"),
                ("custom", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/custom-properties"),
            ];
            for (i, (k, t)) in rels.iter().enumerate() {
                if props[i] {
                    items.push((
                        format!("rId{}", items.len() + 1),
                        (*t).to_string(),
                        format!("docProps/{k}.xml"),
                    ));
                }
            }
            pkg.text("_rels/.rels", rels_xml(&items));
        }
    }

    let mut out = std::io::Cursor::new(Vec::new());
    {
        let mut zw = zip::ZipWriter::new(&mut out);
        let opts = zip::write::SimpleFileOptions::default()
            .compression_method(zip::CompressionMethod::Stored);
        for (name, bytes) in &pkg.parts {
            zw.start_file(name, opts).map_err(|e| e.to_string())?;
            zw.write_all(bytes).map_err(|e| e.to_string())?;
        }
        zw.finish().map_err(|e| e.to_string())?;
    }
    Ok((out.into_inner(), warnings))
}

fn decode_url(raw: &str) -> String {
    let mut out = String::new();
    let mut it = raw.chars();
    let mut seen_lead = false;
    while let Some(ch) = it.next() {
        let lead = !seen_lead;
        seen_lead = true;
        match ch {
            '\u{1}' => {
                let mut peek = it.clone();
                match peek.next() {
                    Some(d) if !lead && d.is_ascii_alphabetic() && out.is_empty() => {
                        out.push_str("file:///");
                        out.push(d);
                        out.push(':');
                        it = peek;
                        if it.clone().next() != Some('\u{3}') {
                            out.push('/');
                        }
                    }
                    _ => {}
                }
            }
            '\u{2}' => out.push('/'),
            '\u{3}' => out.push('/'),
            '\u{4}' => out.push_str("../"),
            '\u{5}' | '\u{6}' => {}
            _ => out.push(ch),
        }
    }
    out
}

fn ext_name_ref(rgce: &[u8]) -> Option<(u32, String)> {
    let u = |i: usize| {
        rgce.get(i..i + 2)
            .map(|b| u32::from(u16::from_le_bytes([b[0], b[1]])))
    };
    let a1 = |r: u32, c: u32| crate::xlsb::ref_text(r, c & 0x3FFF, false, false);
    match rgce.first()? & 0x1F | 0x20 {
        0x3A => Some((u(1)?, a1(u(5)?, u(7)?))),
        0x3B => Some((u(1)?, format!("{}:{}", a1(u(5)?, u(9)?), a1(u(7)?, u(11)?)))),
        _ => None,
    }
}

fn ext_books(supbooks: &[SupBook]) -> Vec<crate::xlsb::ext::ExtBook> {
    supbooks
        .iter()
        .filter_map(|s| {
            let url = s.url.as_ref()?;
            let names: Vec<(String, String)> = s
                .names
                .iter()
                .zip(&s.name_refs)
                .map(|(n, (sheet, r))| {
                    let text = match s.sheets.get(*sheet as usize) {
                        Some(sh) if !r.is_empty() => {
                            format!("='{}'!{r}", sh.replace('\'', "''"))
                        }
                        _ => "#REF!".to_string(),
                    };
                    (n.clone(), text)
                })
                .collect();
            let xml = crate::xlsb::ext::build("rId1", None, &s.sheets, &names, &s.cells);
            let rels = format!(
                "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"{REL}/externalLinkPath\" Target=\"{}\" TargetMode=\"External\"/></Relationships>",
                esc(url)
            );
            Some(crate::xlsb::ext::ExtBook {
                sheets: s.sheets.clone(),
                names: s.names.clone(),
                xml,
                rels: Some(rels.into_bytes()),
            })
        })
        .collect()
}

pub(crate) fn protect_xml((structure, windows, revision, pwd): (bool, bool, bool, u16)) -> String {
    if !(structure || windows || revision || pwd != 0) {
        return String::new();
    }
    let mut s = String::from("<workbookProtection");
    if pwd != 0 {
        let _ = write!(s, " workbookPassword=\"{pwd:04X}\"");
    }
    for (on, k) in [
        (revision, "lockRevision"),
        (structure, "lockStructure"),
        (windows, "lockWindows"),
    ] {
        if on {
            let _ = write!(s, " {k}=\"1\"");
        }
    }
    s.push_str("/>");
    s
}
