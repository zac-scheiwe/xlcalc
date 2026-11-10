use super::cur::Cur;
use super::rcf;
use super::rchart;
use super::rcomments;
use super::rdraw;
use super::read::{ref8, Fx};
use super::rextras;
use super::rstyles::Styles;
use super::Rec;
use crate::xlsb::{cell_name, error_name, esc, rect_name, rk_value, xstr, Dec};
use std::collections::BTreeMap;
use std::fmt::Write as _;

pub(super) const MAIN: &str = "http://schemas.openxmlformats.org/spreadsheetml/2006/main";
pub(super) const REL: &str = "http://schemas.openxmlformats.org/officeDocument/2006/relationships";

pub(super) struct SheetOut {
    pub xml: String,

    pub rels: Vec<(String, String, String)>,

    pub parts: Vec<(String, Vec<u8>)>,
    pub code_name: Option<String>,
}

pub(super) enum Val {
    Blank,
    Num(f64),
    Sst(u32),
    Bool(bool),
    Err(&'static str),
    Str(String),
}

pub(super) enum Form {
    None,
    Text(String),
    Master,
    Array(String, (u32, u32, u32, u32)),
    DataTable(String),
}

pub(super) struct Cell {
    pub col: u32,
    pub xf: u16,
    pub val: Val,
    pub formula: bool,
    pub form: Form,
}

#[derive(Default)]
struct RowInfo {
    height: Option<f64>,
    hidden: bool,
    level: u8,
    collapsed: bool,
    xf: Option<u16>,
}

struct Shared {
    rgce: Vec<u8>,
    extra: Vec<u8>,
}

fn write_cell(x: &mut String, row: u32, c: &Cell, sst: &[String]) {
    let mut attrs = format!("r=\"{}\"", cell_name(row, c.col));
    if c.xf != 0 {
        let _ = write!(attrs, " s=\"{}\"", c.xf);
    }
    let _ = sst;
    let (ty, v) = match &c.val {
        Val::Blank => ("", String::new()),
        Val::Num(n) => ("", format!("<v>{n}</v>")),
        Val::Bool(b) => ("b", format!("<v>{}</v>", u8::from(*b))),
        Val::Err(e) => ("e", format!("<v>{}</v>", esc(e))),
        Val::Sst(i) => ("s", format!("<v>{i}</v>")),
        Val::Str(t) if c.formula => ("str", format!("<v>{}</v>", xstr(t))),
        Val::Str(t) => (
            "inlineStr",
            format!("<is><t xml:space=\"preserve\">{}</t></is>", xstr(t)),
        ),
    };
    if !ty.is_empty() {
        let _ = write!(attrs, " t=\"{ty}\"");
    }
    let f = match &c.form {
        Form::Text(t) => format!("<f>{}</f>", esc(t)),
        Form::Array(t, (r0, c0, r1, c1)) => format!(
            "<f t=\"array\" ref=\"{}\">{}</f>",
            rect_name(*r0, *c0, *r1, *c1),
            esc(t)
        ),
        Form::DataTable(f) => f.clone(),
        Form::None | Form::Master => String::new(),
    };
    let _ = write!(x, "<c {attrs}>{f}{v}</c>");
}

fn formula_value(b: &[u8]) -> (Val, bool) {
    if b[6] == 0xFF && b[7] == 0xFF {
        match b[0] {
            0 => (Val::Str(String::new()), true),
            1 => (Val::Bool(b[2] != 0), false),
            2 => match error_name(b[2]) {
                Some(n) => (Val::Err(n), false),
                None => (Val::Blank, false),
            },
            _ => (Val::Str(String::new()), false),
        }
    } else {
        let mut a = [0u8; 8];
        a.copy_from_slice(b);
        (Val::Num(f64::from_le_bytes(a)), false)
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn read_sheet(
    recs: &[Rec<'_>],
    sst: &[String],
    fx: &mut Fx,
    cp: u16,
    idx: u32,
    sheet: &str,
    warnings: &mut Vec<String>,
    styles: &mut Styles,
    blips: &[Option<rdraw::Blip>],
) -> Result<SheetOut, String> {
    let mut rows: BTreeMap<u32, RowInfo> = BTreeMap::new();
    let mut cells: BTreeMap<u32, Vec<Cell>> = BTreeMap::new();
    let mut cols: Vec<(u32, u32, u16, u16, u16)> = Vec::new();
    let mut merges: Vec<(u32, u32, u32, u32)> = Vec::new();
    let mut shared: BTreeMap<(u32, u32), Shared> = BTreeMap::new();
    let mut array_masters: Vec<(u32, u32)> = Vec::new();
    let mut pending_string: Option<(u32, usize)> = None;
    let mut last_formula: Option<(u32, usize)> = None;
    let mut code_name = None;
    let mut wsbool = 0x04C1u16;
    let (mut def_col_width, mut def_row_h): (Option<u16>, Option<(u16, f64)>) = (None, None);
    let mut win2 = 0x06B6u16;
    let mut zoom: Option<(u16, u16)> = None;
    let mut pane: Option<(u16, u16, u16, u16)> = None;
    let mut pane_active = 0u8;

    let mut sels: Vec<String> = Vec::new();
    let mut margins = [0.75, 0.75, 1.0, 1.0];
    let mut setup: Option<Vec<u8>> = None;

    let mut print_opts = [false; 4];
    let mut hbreaks: Vec<u16> = Vec::new();
    let mut vbreaks: Vec<u16> = Vec::new();
    let (mut header, mut footer) = (String::new(), String::new());
    let mut links: Vec<rextras::Link> = Vec::new();
    let mut validations: Vec<String> = Vec::new();
    let mut cf_groups: Vec<rcf::Group> = Vec::new();
    let mut cf_exes: Vec<rcf::Ex> = Vec::new();
    let mut cf_no = 0u16;
    let mut note_objs: Vec<rcomments::NoteObj> = Vec::new();
    let mut notes: Vec<rcomments::Note> = Vec::new();
    let mut pend: Vec<rdraw::Pending> = Vec::new();
    let mut escs = rdraw::EscState::default();
    let mut shapes: Vec<rdraw::Shape> = Vec::new();
    let mut want_text = false;
    let mut chart_cur: Option<rchart::ChartRecs> = None;
    let mut in_note = false;
    let mut feats: Vec<super::rtable::Feat> = Vec::new();
    let mut depth = 0i32;
    let mut ctx_row = 0u32;
    let _ = &mut ctx_row;
    for r in recs {
        let mut c = Cur::new(r.d);
        if r.id == 0x809 {
            depth += 1;
            if depth == 2 {
                chart_cur = Some(Vec::new());
            }
            continue;
        }
        if r.id == 0x0A {
            depth -= 1;
            if depth == 1 {
                if let Some(recs) = chart_cur.take() {
                    let what = format!("{sheet}: chart");
                    let xml = rchart::to_xml(&recs, fx, cp, idx, &what, warnings);
                    if let Some(sh) = shapes
                        .iter_mut()
                        .rev()
                        .find(|s| s.ot == 5 && s.chart.is_none())
                    {
                        sh.chart = xml;
                    }
                }
            }
            continue;
        }
        if depth > 1 {
            if let Some(c) = chart_cur.as_mut() {
                let mut d = r.d.to_vec();
                for k in &r.cont {
                    d.extend_from_slice(k);
                }
                c.push((r.id, d));
            }
            continue;
        }
        match r.id {
            0x208 => {
                let row = u32::from(c.u16()?);
                c.take(4)?;
                let ht = c.u16()?;
                c.take(4)?;
                let flags = c.u32()?;
                let info = rows.entry(row).or_default();
                if flags & 0x40 != 0 && ht & 0x8000 == 0 {
                    info.height = Some(f64::from(ht & 0x7FFF) / 20.0);
                }
                info.hidden = flags & 0x20 != 0;
                info.level = (flags & 7) as u8;
                info.collapsed = flags & 0x10 != 0;
                if flags & 0x80 != 0 {
                    info.xf = Some(((flags >> 16) & 0xFFF) as u16);
                }
            }
            0xFD | 0x203 | 0x27E | 0x201 | 0x205 | 0x204 => {
                let row = u32::from(c.u16()?);
                let col = u32::from(c.u16()?);
                let xf = c.u16()?;
                let val = match r.id {
                    0xFD => Val::Sst(c.u32()?),
                    0x203 => Val::Num(c.f64()?),
                    0x27E => Val::Num(rk_value(c.u32()?)),
                    0x201 => Val::Blank,
                    0x205 => {
                        let v = c.u8()?;
                        if c.u8()? != 0 {
                            error_name(v).map_or(Val::Blank, Val::Err)
                        } else {
                            Val::Bool(v != 0)
                        }
                    }
                    _ => Val::Str(c.long_str(cp)?),
                };
                cells.entry(row).or_default().push(Cell {
                    col,
                    xf,
                    val,
                    formula: false,
                    form: Form::None,
                });
            }
            0xBD => {
                let row = u32::from(c.u16()?);
                let first = u32::from(c.u16()?);
                let n = (r.d.len().saturating_sub(6)) / 6;
                for k in 0..n {
                    let xf = c.u16()?;
                    let rk = c.u32()?;
                    cells.entry(row).or_default().push(Cell {
                        col: first + k as u32,
                        xf,
                        val: Val::Num(rk_value(rk)),
                        formula: false,
                        form: Form::None,
                    });
                }
            }
            0xBE => {
                let row = u32::from(c.u16()?);
                let first = u32::from(c.u16()?);
                let n = (r.d.len().saturating_sub(6)) / 2;
                for k in 0..n {
                    let xf = c.u16()?;
                    cells.entry(row).or_default().push(Cell {
                        col: first + k as u32,
                        xf,
                        val: Val::Blank,
                        formula: false,
                        form: Form::None,
                    });
                }
            }
            0x06 | 0x206 | 0x406 => {
                let row = u32::from(c.u16()?);
                let col = u32::from(c.u16()?);
                let xf = c.u16()?;
                let (val, str_follows) = formula_value(c.take(8)?);
                c.u16()?;
                c.u32()?;
                let cce = usize::from(c.u16()?);
                let rgce = c.take(cce)?;
                let extra = c.rest();
                let base = (row, col);
                let mut form = Form::None;
                match fx.text(rgce, extra, base, Some(idx), cp) {
                    Ok(Dec::Text(t)) => form = Form::Text(t),
                    Ok(Dec::Exp {
                        row: mr,
                        col: mc,
                        table,
                    }) => {
                        if table || (mr, mc) == (row, col) {
                            form = Form::Master;
                        } else if let Some(s) = shared.get(&(mr, mc)) {
                            match fx.text(&s.rgce.clone(), &s.extra.clone(), base, Some(idx), cp) {
                                Ok(Dec::Text(t)) => form = Form::Text(t),
                                Ok(Dec::Exp { .. }) => {}
                                Err(e) => warnings.push(format!(
                                    "{sheet}!{}: shared formula not supported: {e}",
                                    cell_name(row, col)
                                )),
                            }
                        } else if !array_masters.contains(&(mr, mc)) {
                            form = Form::Master;
                        }
                    }
                    Err(e) => warnings.push(format!(
                        "{sheet}!{}: formula not supported: {e}",
                        cell_name(row, col)
                    )),
                }
                let list = cells.entry(row).or_default();
                list.push(Cell {
                    col,
                    xf,
                    val,
                    formula: true,
                    form,
                });
                last_formula = Some((row, list.len() - 1));
                if str_follows {
                    pending_string = last_formula;
                }
            }
            0x207 => {
                if let Some((row, i)) = pending_string.take() {
                    let n = usize::from(c.u16()?);
                    let s = c.xl_chars(n, cp)?;
                    if let Some(cell) = cells.get_mut(&row).and_then(|l| l.get_mut(i)) {
                        cell.val = Val::Str(s);
                    }
                }
            }
            0x4BC => {
                let (r0, c0, _r1, _c1) = {
                    let (a, b, d, e) = (
                        u32::from(c.u16()?),
                        u32::from(c.u16()?),
                        u32::from(c.u8()?),
                        u32::from(c.u8()?),
                    );
                    (a, d, b, e)
                };
                c.u8()?;
                c.u8()?;
                let cce = usize::from(c.u16()?);
                let rgce = c.take(cce)?.to_vec();
                let extra = c.rest().to_vec();

                if let Some((row, i)) = last_formula {
                    if let Some(cell) = cells.get_mut(&row).and_then(|l| l.get_mut(i)) {
                        if matches!(cell.form, Form::Master) {
                            match fx.text(&rgce, &extra, (row, cell.col), Some(idx), cp) {
                                Ok(Dec::Text(t)) => cell.form = Form::Text(t),
                                Ok(Dec::Exp { .. }) => cell.form = Form::None,
                                Err(e) => {
                                    cell.form = Form::None;
                                    warnings.push(format!(
                                        "{sheet}!{}: formula not supported: {e}",
                                        cell_name(row, cell.col)
                                    ));
                                }
                            }
                        }
                    }
                }
                shared.insert((r0, c0), Shared { rgce, extra });
            }
            0x221 => {
                let (r0, c0, r1, c1) = {
                    let (a, b, d, e) = (
                        u32::from(c.u16()?),
                        u32::from(c.u16()?),
                        u32::from(c.u8()?),
                        u32::from(c.u8()?),
                    );
                    (a, d, b, e)
                };
                c.u16()?;
                c.u32()?;
                let cce = usize::from(c.u16()?);
                let rgce = c.take(cce)?.to_vec();
                let extra = c.rest().to_vec();
                if let Some((row, i)) = last_formula {
                    if let Some(cell) = cells.get_mut(&row).and_then(|l| l.get_mut(i)) {
                        if matches!(cell.form, Form::Master) {
                            let at = (row, cell.col);
                            match fx.text(&rgce, &extra, at, Some(idx), cp) {
                                Ok(Dec::Text(t)) => {
                                    array_masters.push(at);
                                    cell.form = Form::Array(t, (r0, c0, r1, c1));
                                }
                                Ok(Dec::Exp { .. }) => cell.form = Form::None,
                                Err(e) => {
                                    cell.form = Form::None;
                                    warnings.push(format!(
                                        "{sheet}!{}: array formula not supported: {e}",
                                        cell_name(row, cell.col)
                                    ));
                                }
                            }
                        }
                    }
                }
            }
            0x236 => {
                let (r0, c0, r1, c1) = {
                    let (a, b, d, e) = (
                        u32::from(c.u16()?),
                        u32::from(c.u16()?),
                        u32::from(c.u8()?),
                        u32::from(c.u8()?),
                    );
                    (a, d, b, e)
                };
                let flags = c.u16()?;
                let (ir1, ic1) = (u32::from(c.u16()?), u32::from(c.u16()?));
                let (ir2, ic2) = (u32::from(c.u16()?), u32::from(c.u16()?));
                let two = flags & 8 != 0;
                let mut f = format!(
                    "<f t=\"dataTable\" ref=\"{}\" r1=\"{}\"",
                    rect_name(r0, c0, r1, c1),
                    cell_name(ir1, ic1)
                );
                if two {
                    let _ = write!(f, " dt2D=\"1\" r2=\"{}\"", cell_name(ir2, ic2));
                } else if flags & 4 != 0 {
                    f.push_str(" dtr=\"1\"");
                }
                f.push_str("/>");
                if let Some((row, i)) = last_formula {
                    if let Some(cell) = cells.get_mut(&row).and_then(|l| l.get_mut(i)) {
                        if matches!(cell.form, Form::Master) {
                            cell.form = Form::DataTable(f);
                        }
                    }
                }
            }
            0x7D => {
                let a = u32::from(c.u16()?);
                let b = u32::from(c.u16()?);
                let w = c.u16()?;
                let xf = c.u16()?;
                let fl = c.u16()?;
                cols.push((a, b, w, xf, fl));
            }
            0xE5 => {
                let n = usize::from(c.u16()?);
                for _ in 0..n {
                    merges.push(ref8(&mut c)?);
                }
            }
            0x55 => def_col_width = Some(c.u16()?),
            0x225 => {
                let fl = c.u16()?;
                let h = c.u16()?;
                def_row_h = Some((fl, f64::from(h) / 20.0));
            }
            0x81 => wsbool = c.u16()?,
            0x23E => win2 = c.u16()?,
            0xA0 => zoom = Some((c.u16()?, c.u16()?)),
            0x41 => {
                pane = Some((c.u16()?, c.u16()?, c.u16()?, c.u16()?));
                pane_active = c.u8().unwrap_or(0);
            }
            0x1D => {
                let pnn = c.take(1)?[0];
                let (r, col) = (c.u16()?, c.u16()?);
                c.u16()?;
                let n = c.u16()?;

                let mut refs = Vec::new();
                for _ in 0..n {
                    let (r0, r1) = (c.u16()?, c.u16()?);
                    let cc = c.take(2)?;
                    refs.push(super::rextras::ref_str((
                        u32::from(r0),
                        u32::from(cc[0]),
                        u32::from(r1),
                        u32::from(cc[1]),
                    )));
                }
                let rng = if refs.is_empty() {
                    cell_name(u32::from(r), u32::from(col))
                } else {
                    refs.join(" ")
                };
                let pane = match pnn {
                    0 => " pane=\"bottomRight\"",
                    1 => " pane=\"topRight\"",
                    2 => " pane=\"bottomLeft\"",
                    _ => "",
                };
                sels.push(format!(
                    "<selection{pane} activeCell=\"{}\" sqref=\"{rng}\"/>",
                    cell_name(u32::from(r), u32::from(col))
                ));
            }
            0x26..=0x29 => margins[(r.id - 0x26) as usize] = c.f64()?,
            0xA1 => setup = Some(r.d.to_vec()),
            0x2A => print_opts[0] = c.u16()? != 0,
            0x2B => print_opts[1] = c.u16()? != 0,
            0x83 => print_opts[2] = c.u16()? != 0,
            0x84 => print_opts[3] = c.u16()? != 0,
            0x1B | 0x1A => {
                let n = c.u16()?;
                let list = if r.id == 0x1B {
                    &mut hbreaks
                } else {
                    &mut vbreaks
                };
                for _ in 0..n {
                    list.push(c.u16()?);
                    c.u16()?;
                    c.u16()?;
                }
            }
            0x14 | 0x15 => {
                let s = if r.d.is_empty() {
                    String::new()
                } else {
                    c.long_str(cp)?
                };
                if r.id == 0x14 {
                    header = s;
                } else {
                    footer = s;
                }
            }
            0x1BA => {
                let n = usize::from(c.u16()?);
                let s = c.xl_chars(n, cp)?;
                if !s.is_empty() {
                    code_name = Some(s);
                }
            }
            0x1B8 => match rextras::hlink(r.d) {
                Ok(l) => links.push(l),
                Err(e) => warnings.push(format!("{sheet}: hyperlink not read: {e}")),
            },
            0x800 => {
                if let Ok((rc, text)) = rextras::hlink_tooltip(r.d) {
                    if let Some(l) = links.iter_mut().rev().find(|l| l.rect == rc) {
                        l.tooltip = Some(text);
                    }
                }
            }
            0x1B0 => {
                cf_no += 1;
                match rcf::header(r.d, cf_no) {
                    Ok(g) => cf_groups.push(g),
                    Err(e) => warnings.push(format!("{sheet}: conditional format not read: {e}")),
                }
            }
            0x1B1 => {
                if let Some(g) = cf_groups.last_mut() {
                    g.rules.push(r.d.to_vec());
                }
            }
            0x879 => {
                cf_no += 1;
                match rcf::header(r.d.get(12..).unwrap_or(&[]), cf_no) {
                    Ok(g) => cf_groups.push(g),
                    Err(e) => warnings.push(format!("{sheet}: conditional format not read: {e}")),
                }
            }
            0x87A => {
                if let Some(g) = cf_groups.last_mut() {
                    g.rules12.push(r.d.to_vec());
                }
            }
            0x87B => cf_exes.extend(rcf::ex(r.d)),
            0xEC => {
                let mut full = r.d.to_vec();
                for c in &r.cont {
                    full.extend_from_slice(c);
                }
                rdraw::scan(&full, &mut pend, &mut escs);
            }
            0x5D => {
                in_note = false;
                want_text = false;
                let head = rdraw::obj_head(r.d);
                let p = pend.pop();
                if let Some(id) = rcomments::obj_note_id(r.d) {
                    in_note = true;
                    note_objs.push(rcomments::NoteObj {
                        id,
                        anchor: p.and_then(|p| p.anchor),
                        text: None,
                    });
                } else if let (Some((ot, id)), Some(p)) = (head, p) {
                    shapes.push(rdraw::Shape {
                        p,
                        ot,
                        id,
                        text: String::new(),
                        chart: None,
                    });
                    want_text = true;
                }

                for c in &r.cont {
                    if c.len() >= 8 && (0xF000..0xF200).contains(&u16::from_le_bytes([c[2], c[3]]))
                    {
                        rdraw::scan(c, &mut pend, &mut escs);
                    }
                }
            }
            0x1B6 => {
                if let (true, Some(o)) = (in_note, note_objs.last_mut()) {
                    o.text = Some(rcomments::txo_text(r));
                } else if let (true, Some(sh)) = (want_text, shapes.last_mut()) {
                    sh.text = rcomments::txo_text(r);
                }
                in_note = false;
                want_text = false;
            }
            0x872 | 0x878 => match super::rtable::feature(r, cp) {
                Ok(f) => feats.push(f),
                Err(e) => warnings.push(format!("{sheet}: table not read: {e}")),
            },
            0x877 => {
                if let Err(e) = super::rtable::list12(r, &mut feats, cp) {
                    warnings.push(format!("{sheet}: table style not read: {e}"));
                }
            }
            0x1C => notes.extend(rcomments::note(r.d)),
            0x1BE => match rextras::dv(r.d, cp, fx, idx) {
                Ok(x) => validations.push(x),
                Err(e) => warnings.push(format!("{sheet}: data validation not read: {e}")),
            },
            _ => {}
        }
    }
    let mut rels: Vec<(String, String, String)> = Vec::new();

    let mut x = String::new();
    let _ = write!(x, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<worksheet xmlns=\"{MAIN}\" xmlns:r=\"{REL}\">");

    let mut pr = String::new();
    if wsbool & 0x40 == 0 || wsbool & 0x80 == 0 {
        let _ = write!(
            pr,
            "<outlinePr{}{}/>",
            if wsbool & 0x40 == 0 {
                " summaryBelow=\"0\""
            } else {
                ""
            },
            if wsbool & 0x80 == 0 {
                " summaryRight=\"0\""
            } else {
                ""
            }
        );
    }
    if wsbool & 0x100 != 0 {
        pr.push_str("<pageSetUpPr fitToPage=\"1\"/>");
    }
    if code_name.is_some() || !pr.is_empty() {
        let _ = write!(
            x,
            "<sheetPr{}>{pr}</sheetPr>",
            code_name
                .as_ref()
                .map(|n| format!(" codeName=\"{}\"", esc(n)))
                .unwrap_or_default()
        );
    }

    {
        let mut v = String::from("<sheetView");
        if win2 & 0x02 == 0 {
            v.push_str(" showGridLines=\"0\"");
        }
        if win2 & 0x04 == 0 {
            v.push_str(" showRowColHeaders=\"0\"");
        }
        if win2 & 0x10 == 0 {
            v.push_str(" showZeros=\"0\"");
        }
        if win2 & 0x40 != 0 {
            v.push_str(" rightToLeft=\"1\"");
        }
        if win2 & 0x200 != 0 {
            v.push_str(" tabSelected=\"1\"");
        }
        if let Some((n, d)) = zoom {
            if d != 0 {
                let _ = write!(v, " zoomScale=\"{}\"", u32::from(n) * 100 / u32::from(d));
            }
        }
        v.push_str(" workbookViewId=\"0\"");
        match pane {
            Some((px, py, top, left)) if px > 0 || py > 0 => {
                let _ = write!(v, "><pane");
                if px > 0 {
                    let _ = write!(v, " xSplit=\"{px}\"");
                }
                if py > 0 {
                    let _ = write!(v, " ySplit=\"{py}\"");
                }
                let tl = cell_name(u32::from(top), u32::from(left));
                let active = match pane_active {
                    1 => "topRight",
                    2 => "bottomLeft",
                    3 => "topLeft",
                    _ => "bottomRight",
                };

                let state = if win2 & 0x08 != 0 {
                    " state=\"frozen\""
                } else {
                    ""
                };
                let _ = write!(v, " topLeftCell=\"{tl}\" activePane=\"{active}\"{state}/>");
                for s in &sels {
                    v.push_str(s);
                }
                v.push_str("</sheetView>");
            }
            _ => {
                let plain = sels.iter().all(|s| s.contains("sqref=\"A1\""));
                if plain {
                    v.push_str("/>");
                } else {
                    v.push('>');
                    for s in &sels {
                        v.push_str(s);
                    }
                    v.push_str("</sheetView>");
                }
            }
        }
        let _ = write!(x, "<sheetViews>{v}</sheetViews>");
    }

    {
        let mut f = String::from("<sheetFormatPr");
        if let Some(w) = def_col_width {
            let _ = write!(f, " baseColWidth=\"{w}\"");
        }
        match def_row_h {
            Some((fl, h)) => {
                let _ = write!(f, " defaultRowHeight=\"{h}\"");
                if fl & 1 != 0 {
                    f.push_str(" customHeight=\"1\"");
                }
                if fl & 2 != 0 {
                    f.push_str(" zeroHeight=\"1\"");
                }
            }
            None => f.push_str(" defaultRowHeight=\"12.75\""),
        }
        f.push_str("/>");
        x.push_str(&f);
    }
    if !cols.is_empty() {
        x.push_str("<cols>");
        for (min, max, w, xf, fl) in &cols {
            let _ = write!(
                x,
                "<col min=\"{}\" max=\"{}\" width=\"{}\"{}{}{}{}{}>",
                min + 1,
                (*max + 1).min(256),
                f64::from(*w) / 256.0,
                if *xf != 0 {
                    format!(" style=\"{xf}\"")
                } else {
                    String::new()
                },
                if fl & 1 != 0 { " hidden=\"1\"" } else { "" },
                if fl & 2 != 0 {
                    " customWidth=\"1\""
                } else {
                    ""
                },
                match (fl >> 8) & 7 {
                    0 => String::new(),
                    l => format!(" outlineLevel=\"{l}\""),
                },
                if fl & 0x1000 != 0 {
                    " collapsed=\"1\"/"
                } else {
                    "/"
                }
            );
        }
        x.push_str("</cols>");
    }
    x.push_str("<sheetData>");
    let mut all_rows: Vec<u32> = rows.keys().chain(cells.keys()).copied().collect();
    all_rows.sort_unstable();
    all_rows.dedup();
    for r in all_rows {
        let info = rows.get(&r);
        let list = cells.get(&r);
        let plain = info.is_none_or(|i| {
            i.height.is_none() && !i.hidden && i.level == 0 && !i.collapsed && i.xf.is_none()
        });
        if list.is_none() && plain {
            continue;
        }
        let mut a = format!("<row r=\"{}\"", r + 1);
        if let Some(i) = info {
            if let Some(xf) = i.xf {
                let _ = write!(a, " s=\"{xf}\" customFormat=\"1\"");
            }
            if let Some(h) = i.height {
                let _ = write!(a, " ht=\"{h}\" customHeight=\"1\"");
            }
            if i.hidden {
                a.push_str(" hidden=\"1\"");
            }
            if i.level > 0 {
                let _ = write!(a, " outlineLevel=\"{}\"", i.level);
            }
            if i.collapsed {
                a.push_str(" collapsed=\"1\"");
            }
        }
        a.push('>');
        x.push_str(&a);
        if let Some(l) = list {
            let mut l: Vec<&Cell> = l.iter().collect();
            l.sort_by_key(|c| c.col);
            for c in l {
                write_cell(&mut x, r, c, sst);
            }
        }
        x.push_str("</row>");
    }
    x.push_str("</sheetData>");
    if !merges.is_empty() {
        let _ = write!(x, "<mergeCells count=\"{}\">", merges.len());
        for (r0, c0, r1, c1) in &merges {
            let _ = write!(x, "<mergeCell ref=\"{}\"/>", rect_name(*r0, *c0, *r1, *c1));
        }
        x.push_str("</mergeCells>");
    }
    x.push_str(&rcf::groups_xml(
        &cf_groups, &cf_exes, fx, styles, cp, idx, warnings, sheet,
    ));
    if !validations.is_empty() {
        let _ = write!(x, "<dataValidations count=\"{}\">", validations.len());
        for v in &validations {
            x.push_str(v);
        }
        x.push_str("</dataValidations>");
    }
    let cell_text = |r: u32, c: u32| {
        let cell = cells.get(&r)?.iter().find(|x| x.col == c)?;
        match &cell.val {
            Val::Str(t) => Some(t.clone()),

            Val::Sst(i) => sst
                .get(*i as usize)
                .and_then(|si| crate::xml::parse(si.as_bytes()).ok())
                .map(|n| crate::si_text(&n)),
            _ => None,
        }
    };
    x.push_str(&rextras::links_xml(
        &links,
        rels.len() + 1,
        &mut rels,
        &cell_text,
    ));
    if print_opts.iter().any(|b| *b) {
        x.push_str("<printOptions");
        for (on, key) in print_opts.iter().zip([
            "headings",
            "gridLines",
            "horizontalCentered",
            "verticalCentered",
        ]) {
            if *on {
                let _ = write!(x, " {key}=\"1\"");
            }
        }
        x.push_str("/>");
    }

    let (hm, fm) = match &setup {
        Some(d) if d.len() >= 34 => {
            let mut c = Cur::new(&d[16..]);
            (c.f64()?, c.f64()?)
        }
        _ => (0.5, 0.5),
    };
    let _ = write!(
        x,
        "<pageMargins left=\"{}\" right=\"{}\" top=\"{}\" bottom=\"{}\" header=\"{hm}\" footer=\"{fm}\"/>",
        margins[0], margins[1], margins[2], margins[3]
    );
    if let Some(d) = &setup {
        if d.len() >= 34 {
            let mut c = Cur::new(d);
            let paper = c.u16()?;
            let scale = c.u16()?;
            let start = c.u16()?;
            let fw = c.u16()?;
            let fh = c.u16()?;
            let fl = c.u16()?;

            let mut s = String::from("<pageSetup");
            if fl & 0x04 == 0 {
                let _ = write!(s, " paperSize=\"{paper}\" scale=\"{scale}\"");
            }
            let _ = write!(s, " fitToWidth=\"{fw}\" fitToHeight=\"{fh}\"");
            if fl & 0x01 != 0 {
                s.push_str(" pageOrder=\"overThenDown\"");
            }
            if fl & 0x80 != 0 && fl & 0x04 == 0 {
                let _ = write!(s, " firstPageNumber=\"{start}\" useFirstPageNumber=\"1\"");
            }
            if fl & 0x08 != 0 {
                s.push_str(" blackAndWhite=\"1\"");
            }
            if fl & 0x10 != 0 {
                s.push_str(" draft=\"1\"");
            }
            if fl & 0x20 != 0 {
                s.push_str(" cellComments=\"asDisplayed\"");
            }
            if fl & 0x04 == 0 {
                let _ = write!(
                    s,
                    " orientation=\"{}\"",
                    if fl & 2 != 0 { "portrait" } else { "landscape" }
                );
            }
            s.push_str("/>");
            x.push_str(&s);
        }
    }
    if !header.is_empty() || !footer.is_empty() {
        x.push_str("<headerFooter>");
        if !header.is_empty() {
            let _ = write!(x, "<oddHeader>{}</oddHeader>", esc(&header));
        }
        if !footer.is_empty() {
            let _ = write!(x, "<oddFooter>{}</oddFooter>", esc(&footer));
        }
        x.push_str("</headerFooter>");
    }
    for (tag, list, max) in [
        ("rowBreaks", &hbreaks, 16383),
        ("colBreaks", &vbreaks, 1048575),
    ] {
        if !list.is_empty() {
            let _ = write!(
                x,
                "<{tag} count=\"{0}\" manualBreakCount=\"{0}\">",
                list.len()
            );
            for b in list {
                let _ = write!(x, "<brk id=\"{b}\" max=\"{max}\" man=\"1\"/>");
            }
            let _ = write!(x, "</{tag}>");
        }
    }
    let dims = {
        let mut probe = x.clone();
        probe.push_str("</worksheet>");
        crate::xml::parse(probe.as_bytes())
            .map(|n| super::wdraw::Dims::of(&n))
            .unwrap_or_else(|_| super::wdraw::Dims::of(&Default::default()))
    };
    let drawing = if shapes.is_empty() {
        None
    } else {
        rdraw::drawing_xml(&shapes, &dims, sheet, blips, warnings, idx as usize + 1)
    };
    if drawing.is_some() {
        let n = idx + 1;
        rels.push((
            format!("rId{}", rels.len() + 1),
            format!("{REL}/drawing"),
            format!("../drawings/drawing{n}.xml"),
        ));
        let _ = write!(x, "<drawing r:id=\"rId{}\"/>", rels.len());
    }
    if !notes.is_empty() {
        let n = idx + 1;
        rels.push((
            format!("rId{}", rels.len() + 1),
            format!("{REL}/comments"),
            format!("../comments{n}.xml"),
        ));
        let vid = format!("rId{}", rels.len() + 1);
        rels.push((
            vid.clone(),
            format!("{REL}/vmlDrawing"),
            format!("../drawings/vmlDrawing{n}.vml"),
        ));
        let _ = write!(x, "<legacyDrawing r:id=\"{vid}\"/>");
    }
    if !feats.is_empty() {
        x.push_str("<tableParts count=\"");
        let _ = write!(x, "{}\">", feats.len());
        for f in &feats {
            rels.push((
                format!("rId{}", rels.len() + 1),
                format!("{REL}/table"),
                format!("../tables/table{}.xml", f.id),
            ));
            let _ = write!(x, "<tablePart r:id=\"rId{}\"/>", rels.len());
        }
        x.push_str("</tableParts>");
    }
    x.push_str("</worksheet>");
    let mut parts = Vec::new();
    for f in &feats {
        let calc = |r: u32, c: u32| match cells.get(&r)?.iter().find(|x| x.col == c)?.form {
            Form::Text(ref t) => Some(t.clone()),
            _ => None,
        };
        let t = super::rtable::to_xml(f, fx, cp, idx, sheet, warnings, &calc);
        parts.push((format!("xl/tables/table{}.xml", f.id), t.into_bytes()));
    }
    if let Some((d, extra)) = drawing {
        parts.push((
            format!("xl/drawings/drawing{}.xml", idx + 1),
            d.into_bytes(),
        ));
        parts.extend(extra);
    }
    if !notes.is_empty() {
        let n = idx + 1;
        let pairs: Vec<(rcomments::Note, rcomments::NoteObj)> = notes
            .into_iter()
            .map(|nt| {
                let o = note_objs
                    .iter()
                    .find(|o| o.id == nt.id)
                    .cloned()
                    .unwrap_or_default();
                (nt, o)
            })
            .collect();
        let (cx, vml) = rcomments::parts(&pairs, n, &dims);
        parts.push((format!("xl/comments{n}.xml"), cx.into_bytes()));
        parts.push((format!("xl/drawings/vmlDrawing{n}.vml"), vml.into_bytes()));
    }
    Ok(SheetOut {
        xml: x,
        rels,
        parts,
        code_name,
    })
}
