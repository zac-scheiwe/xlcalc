use crate::package::rels_name;
use crate::xlsb_sheet::SheetXml;
use crate::xml::Node;
use crate::{external_targets, part, rels, tree, Zip};
use std::collections::HashMap;
use std::fmt::Write as _;
use std::io::Write as _;

pub(crate) mod ext;

const REL: &str = "http://schemas.openxmlformats.org/officeDocument/2006/relationships";

pub fn is_xlsb(zip: &mut Zip) -> bool {
    zip.by_name("xl/workbook.bin").is_ok()
}

pub(crate) struct Recs<'a> {
    b: &'a [u8],
    p: usize,
}

impl<'a> Iterator for Recs<'a> {
    type Item = (u32, &'a [u8]);
    fn next(&mut self) -> Option<Self::Item> {
        let b = self.b;
        let b0 = *b.get(self.p)?;
        self.p += 1;
        let mut id = u32::from(b0 & 0x7f);
        if b0 & 0x80 != 0 {
            let b1 = *b.get(self.p)?;
            self.p += 1;
            id |= u32::from(b1) << 7;
        }
        let mut len = 0usize;
        for i in 0..4 {
            let x = *b.get(self.p)?;
            self.p += 1;
            len |= usize::from(x & 0x7f) << (7 * i);
            if x & 0x80 == 0 {
                break;
            }
        }
        let end = (self.p + len).min(b.len());
        let d = &b[self.p..end];
        self.p = end;
        Some((id, d))
    }
}

pub(crate) fn recs(b: &[u8]) -> Recs<'_> {
    Recs { b, p: 0 }
}

pub(crate) struct Rd<'a> {
    b: &'a [u8],
    p: usize,
}

impl<'a> Rd<'a> {
    pub(crate) fn new(b: &'a [u8]) -> Self {
        Rd { b, p: 0 }
    }
    pub(crate) fn take(&mut self, n: usize) -> Result<&'a [u8], String> {
        let s = self.b.get(self.p..self.p + n).ok_or("record too short")?;
        self.p += n;
        Ok(s)
    }
    pub(crate) fn u8(&mut self) -> Result<u8, String> {
        Ok(self.take(1)?[0])
    }
    pub(crate) fn u16(&mut self) -> Result<u16, String> {
        let s = self.take(2)?;
        Ok(u16::from_le_bytes([s[0], s[1]]))
    }
    pub(crate) fn u32(&mut self) -> Result<u32, String> {
        let s = self.take(4)?;
        Ok(u32::from_le_bytes([s[0], s[1], s[2], s[3]]))
    }
    pub(crate) fn f64(&mut self) -> Result<f64, String> {
        let s = self.take(8)?;
        let mut a = [0u8; 8];
        a.copy_from_slice(s);
        Ok(f64::from_le_bytes(a))
    }

    pub(crate) fn utf16(&mut self, n: usize) -> Result<String, String> {
        let units: Vec<u16> = self
            .take(n * 2)?
            .as_chunks::<2>()
            .0
            .iter()
            .map(|c| u16::from_le_bytes(*c))
            .collect();
        Ok(String::from_utf16_lossy(&units))
    }

    pub(crate) fn wstr(&mut self) -> Result<String, String> {
        let n = self.u32()?;
        if n == 0xFFFF_FFFF {
            return Ok(String::new());
        }
        self.utf16(n as usize)
    }
    pub(crate) fn rest(&self) -> &'a [u8] {
        &self.b[self.p.min(self.b.len())..]
    }
}

pub(crate) fn esc(s: &str) -> String {
    let mut o = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => o.push_str("&amp;"),
            '<' => o.push_str("&lt;"),
            '>' => o.push_str("&gt;"),
            '"' => o.push_str("&quot;"),
            _ => o.push(c),
        }
    }
    o
}

pub(crate) fn xstr(s: &str) -> String {
    let mut o = String::with_capacity(s.len());
    let mut it = s.chars().peekable();
    while let Some(c) = it.next() {
        match c {
            '_' if it.peek() == Some(&'x') => o.push_str("_x005F_"),
            '\n' | '\t' => o.push(c),
            c if (c as u32) < 0x20 || c == '\u{fffe}' || c == '\u{ffff}' => {
                let _ = write!(o, "_x{:04X}_", c as u32);
            }
            c => o.push(c),
        }
    }
    esc(&o)
}

pub(crate) fn error_name(code: u8) -> Option<&'static str> {
    Some(match code {
        0x00 => "#NULL!",
        0x07 => "#DIV/0!",
        0x0F => "#VALUE!",
        0x17 => "#REF!",
        0x1D => "#NAME?",
        0x24 => "#NUM!",
        0x2A => "#N/A",
        0x2B => "#GETTING_DATA",
        0x2D => "#SPILL!",
        0x2E => "#CONNECT!",
        0x2F => "#BLOCKED!",
        0x30 => "#UNKNOWN!",
        0x31 => "#FIELD!",
        0x32 => "#CALC!",
        0x33 => "#BUSY!",
        _ => return None,
    })
}

pub(crate) fn cell_name(row: u32, col: u32) -> String {
    format!("{}{}", xlcalc::col_to_letters(col), row + 1)
}

pub(crate) fn rect_name(r0: u32, c0: u32, r1: u32, c1: u32) -> String {
    format!("{}:{}", cell_name(r0, c0), cell_name(r1, c1))
}

pub(crate) fn rk_value(rk: u32) -> f64 {
    let v = if rk & 2 != 0 {
        f64::from((rk as i32) >> 2)
    } else {
        f64::from_bits(u64::from(rk & 0xFFFF_FFFC) << 32)
    };
    if rk & 1 != 0 {
        v / 100.0
    } else {
        v
    }
}

pub(crate) fn quote_sheet(n: &str) -> String {
    let lower = n.to_lowercase();
    let bytes = lower.as_bytes();
    let mut plain = !n.is_empty()
        && n.chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '.')
        && !n
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_digit() || c == '.');
    if plain {
        let letters = bytes.iter().take_while(|b| b.is_ascii_lowercase()).count();
        let rest = &bytes[letters..];
        if letters > 0 && letters <= 3 && !rest.is_empty() && rest.iter().all(u8::is_ascii_digit) {
            plain = false;
        }
        if lower == "r" || lower == "c" {
            plain = false;
        }
        if let Some(t) = lower.strip_prefix('r') {
            let d = t.bytes().take_while(u8::is_ascii_digit).count();
            let t2 = &t[d..];
            if t2.is_empty() && d > 0 {
                plain = false;
            }
            if let Some(t3) = t2.strip_prefix('c') {
                if t3.bytes().all(|b| b.is_ascii_digit()) {
                    plain = false;
                }
            }
        }
        if let Some(t) = lower.strip_prefix('c') {
            if !t.is_empty() && t.bytes().all(|b| b.is_ascii_digit()) {
                plain = false;
            }
        }
        if lower == "true" || lower == "false" {
            plain = false;
        }
    }
    if plain {
        n.to_string()
    } else {
        format!("'{}'", n.replace('\'', "''"))
    }
}

pub(crate) struct NameDef {
    name: String,
    flags: u32,
    scope: Option<u32>,
    rgce: Vec<u8>,
    rgcb: Vec<u8>,
}

struct TableDef {
    id: u32,
    name: String,
    rect: (u32, u32, u32, u32),
    headers: u32,
    totals: u32,
    columns: Vec<String>,

    totals_cols: Vec<(u32, String)>,

    filter: Option<(u32, u32, u32, u32)>,

    style: Option<(u16, String)>,
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum Book {
    This,
    Other,

    Ext(usize),
}

pub(crate) struct Ctx<'a> {
    sheets: &'a [String],
    xti: &'a [(u32, i32, i32)],
    books: &'a [Book],
    ext: &'a [ext::ExtBook],
    names: &'a [NameDef],
    tables: &'a [TableDef],

    cur: std::cell::Cell<Option<u32>>,
}

pub(crate) enum Dec {
    Text(String),

    Exp { row: u32, col: u32, table: bool },
}

fn col_text(col: u32, rel: bool) -> String {
    format!(
        "{}{}",
        if rel { "" } else { "$" },
        xlcalc::col_to_letters(col)
    )
}

pub(crate) fn ref_text(row: u32, col: u32, row_rel: bool, col_rel: bool) -> String {
    format!(
        "{}{}{}",
        col_text(col, col_rel),
        if row_rel { "" } else { "$" },
        row + 1
    )
}

const MAX_ROW: u32 = 0xF_FFFF;
const MAX_COL: u32 = 0x3FFF;

fn rel_row(base: u32, off: u32) -> u32 {
    (i64::from(base) + i64::from(off as i32)).rem_euclid(i64::from(MAX_ROW) + 1) as u32
}

fn rel_col(base: u32, off: u32) -> u32 {
    let signed = if off & 0x2000 != 0 {
        i64::from(off) - 0x4000
    } else {
        i64::from(off)
    };
    (i64::from(base) + signed).rem_euclid(i64::from(MAX_COL) + 1) as u32
}

impl Ctx<'_> {
    fn sheet_prefix(&self, ixti: u16) -> Result<String, String> {
        let &(book, first, last) = self
            .xti
            .get(usize::from(ixti))
            .ok_or("extern sheet index out of range")?;
        if let Some(Book::Ext(n)) = self.books.get(book as usize) {
            let sheets = &self.ext.get(n - 1).ok_or("no such linked workbook")?.sheets;
            let name = |i: i32| -> Result<&str, String> {
                usize::try_from(i)
                    .ok()
                    .and_then(|i| sheets.get(i))
                    .map(String::as_str)
                    .ok_or_else(|| "sheet index out of range".to_string())
            };
            if first == -1 || last == -1 {
                return Ok("#REF!".into());
            }
            if first == -2 {
                return Ok(format!("[{n}]!"));
            }
            let (a, b) = (name(first)?, name(last)?);
            let both = if first == last {
                a.to_string()
            } else {
                format!("{a}:{b}")
            };
            let plain = quote_sheet(a) == a && quote_sheet(b) == b;
            return Ok(if plain {
                format!("[{n}]{both}!")
            } else {
                format!("'[{n}]{}'!", both.replace('\x27', "''"))
            });
        }
        if self.books.get(book as usize) != Some(&Book::This) {
            return Err("reference into another workbook".into());
        }
        let name = |i: i32| -> Result<&str, String> {
            usize::try_from(i)
                .ok()
                .and_then(|i| self.sheets.get(i))
                .map(String::as_str)
                .ok_or_else(|| "sheet index out of range".to_string())
        };
        if first == -1 || last == -1 {
            return Ok("#REF!".into());
        }
        if first == -2 {
            return Ok("[0]!".into());
        }
        if first == last {
            Ok(format!("{}!", quote_sheet(name(first)?)))
        } else {
            let both = format!("{}:{}", name(first)?, name(last)?);
            let q = quote_sheet(&both);
            let q = if q.starts_with('\'') {
                q
            } else {
                format!("'{}'", both.replace('\'', "''"))
            };

            let plain = !name(first)?.is_empty()
                && quote_sheet(name(first)?) == name(first)?
                && quote_sheet(name(last)?) == name(last)?;
            Ok(if plain {
                format!("{both}!")
            } else {
                format!("{q}!")
            })
        }
    }

    fn name_text(&self, idx: u32) -> Result<String, String> {
        let n = usize::try_from(idx)
            .ok()
            .and_then(|i| i.checked_sub(1))
            .and_then(|i| self.names.get(i))
            .ok_or("name index out of range")?;
        match n.scope {
            Some(s) if self.cur.get() != Some(s) => {
                let sheet = self
                    .sheets
                    .get(s as usize)
                    .ok_or("name scope out of range")?;
                Ok(format!("{}!{}", quote_sheet(sheet), n.name))
            }
            _ => Ok(n.name.clone()),
        }
    }

    fn list_text(&self, ixti_flags: &mut Rd<'_>) -> Result<String, String> {
        let _ixti = ixti_flags.u16()?;
        let flags = ixti_flags.u16()?;
        let list = ixti_flags.u32()?;
        let col_first = u32::from(ixti_flags.u16()?);
        let col_last = u32::from(ixti_flags.u16()?);
        if flags & 0x3000 != 0 {
            return Ok("#REF!".into());
        }
        let columns = flags & 3;
        let row_type = (flags >> 2) & 0x1f;
        let t = self
            .tables
            .iter()
            .find(|t| t.id == list)
            .ok_or("structured reference to an unknown table")?;
        let cname = |i: u32| -> Result<String, String> {
            let n = t
                .columns
                .get(i as usize)
                .ok_or("table column out of range")?;
            Ok(n.chars()
                .flat_map(|c| {
                    if matches!(c, '[' | ']' | '#' | '\'') {
                        vec!['\'', c]
                    } else {
                        vec![c]
                    }
                })
                .collect())
        };
        let rows: &[&str] = match row_type {
            0x00 | 0x04 => &[],
            0x01 => &["#All"],
            0x02 => &["#Headers"],
            0x06 => &["#Headers", "#Data"],
            0x08 => &["#Totals"],
            0x0C => &["#Data", "#Totals"],
            0x10 => &["#This Row"],
            _ => return Err(format!("structured reference row type {row_type:#x}")),
        };
        let cols = match columns {
            0 => None,
            1 => Some((cname(col_first)?, None)),
            _ => Some((cname(col_first)?, Some(cname(col_last)?))),
        };
        let mut parts: Vec<String> = rows.iter().map(|r| format!("[{r}]")).collect();
        match (&cols, parts.len()) {
            (None, 0) => return Ok(format!("{}[]", t.name)),
            (None, 1) => return Ok(format!("{}[{}]", t.name, rows[0])),
            (Some((a, None)), 0) => return Ok(format!("{}[{a}]", t.name)),
            (Some((a, Some(b))), 0) => return Ok(format!("{}[[{a}]:[{b}]]", t.name)),
            (Some((a, None)), _) => parts.push(format!("[{a}]")),
            (Some((a, Some(b))), _) => parts.push(format!("[{a}]:[{b}]")),
            (None, _) => {}
        }
        Ok(format!("{}[{}]", t.name, parts.join(",")))
    }
}

fn binop(ptg: u8) -> Option<&'static str> {
    Some(match ptg {
        0x03 => "+",
        0x04 => "-",
        0x05 => "*",
        0x06 => "/",
        0x07 => "^",
        0x08 => "&",
        0x09 => "<",
        0x0A => "<=",
        0x0B => "=",
        0x0C => ">=",
        0x0D => ">",
        0x0E => "<>",
        0x0F => " ",
        0x10 => ",",
        0x11 => ":",
        _ => return None,
    })
}

fn pop(st: &mut Vec<String>) -> Result<String, String> {
    st.pop().ok_or_else(|| "formula stack underflow".into())
}

fn pop_n(st: &mut Vec<String>, n: usize) -> Result<Vec<String>, String> {
    if st.len() < n {
        return Err("formula stack underflow".into());
    }
    Ok(st.split_off(st.len() - n))
}

pub(crate) fn decode(
    ctx: &Ctx<'_>,
    rgce: &[u8],
    rgcb: &[u8],
    base: (u32, u32),
) -> Result<Dec, String> {
    let mut rd = Rd::new(rgce);
    let mut ex = Rd::new(rgcb);
    let mut st: Vec<String> = Vec::new();
    let mut space = String::new();
    let mut first = true;
    while rd.p < rgce.len() {
        let ptg = rd.u8()?;
        let was_first = std::mem::take(&mut first);

        let push = |st: &mut Vec<String>, s: String, space: &mut String| {
            st.push(format!("{}{s}", std::mem::take(space)));
        };
        match ptg {
            0x01 | 0x02 => {
                let row = rd.u32()?;
                let col = ex.u32()?;
                if was_first && rd.p == rgce.len() {
                    return Ok(Dec::Exp {
                        row,
                        col,
                        table: ptg == 2,
                    });
                }
                return Err("PtgExp inside a larger formula".into());
            }
            0x03..=0x11 => {
                let b = pop(&mut st)?;
                let a = pop(&mut st)?;
                let op = binop(ptg).unwrap_or("");
                let w = std::mem::take(&mut space);
                st.push(format!("{a}{w}{op}{b}"));
            }
            0x12 => {
                let a = pop(&mut st)?;
                push(&mut st, format!("+{a}"), &mut space);
            }
            0x13 => {
                let a = pop(&mut st)?;
                push(&mut st, format!("-{a}"), &mut space);
            }
            0x14 => {
                let a = pop(&mut st)?;
                st.push(format!("{a}%"));
            }
            0x15 => {
                let a = pop(&mut st)?;
                push(&mut st, format!("({a})"), &mut space);
            }
            0x16 => push(&mut st, String::new(), &mut space),
            0x17 => {
                let n = usize::from(rd.u16()?);
                let s = rd.utf16(n)?.replace('"', "\"\"");
                push(&mut st, format!("\"{s}\""), &mut space);
            }
            0x18 => {
                let e = rd.u8()?;
                if e != 0x19 {
                    return Err(format!("PtgExtend {e:#x} (pivot table name)"));
                }
                let s = ctx.list_text(&mut rd)?;
                push(&mut st, s, &mut space);
            }
            0x19 => {
                let ty = rd.u8()?;
                match ty {
                    0x01 | 0x02 | 0x08 | 0x20 | 0x21 | 0x80 => {
                        rd.take(2)?;
                    }
                    0x04 => {
                        let n = usize::from(rd.u16()?);
                        rd.take((n + 1) * 2)?;
                    }
                    0x10 => {
                        rd.take(2)?;
                        let a = pop(&mut st)?;
                        push(&mut st, format!("SUM({a})"), &mut space);
                    }
                    0x40 | 0x41 => {
                        let kind = rd.u8()?;
                        let n = usize::from(rd.u8()?);
                        let ch = if kind & 1 == 1 { '\n' } else { ' ' };
                        space.extend(std::iter::repeat_n(ch, n));
                    }
                    _ => return Err(format!("PtgAttr {ty:#x}")),
                }
            }
            0x1C => {
                let e = rd.u8()?;
                let n = error_name(e).ok_or(format!("error code {e:#x}"))?;
                push(&mut st, n.into(), &mut space);
            }
            0x1D => {
                let b = rd.u8()?;
                push(
                    &mut st,
                    if b != 0 { "TRUE" } else { "FALSE" }.into(),
                    &mut space,
                );
            }
            0x1E => {
                let v = rd.u16()?;
                push(&mut st, v.to_string(), &mut space);
            }
            0x1F => {
                let v = rd.f64()?;
                push(&mut st, format!("{v}"), &mut space);
            }
            0x20 | 0x40 | 0x60 => {
                rd.take(14)?;
                let rows = ex.u32()?;
                let cols = ex.u32()?;
                let mut out = String::from("{");
                for r in 0..rows {
                    if r > 0 {
                        out.push(';');
                    }
                    for c in 0..cols {
                        if c > 0 {
                            out.push(',');
                        }
                        match ex.u8()? {
                            0 => {
                                let _ = write!(out, "{}", ex.f64()?);
                            }
                            1 => {
                                let n = usize::from(ex.u16()?);
                                let s = ex.utf16(n)?.replace('"', "\"\"");
                                let _ = write!(out, "\"{s}\"");
                            }
                            2 => out.push_str(if ex.u8()? != 0 { "TRUE" } else { "FALSE" }),
                            4 => {
                                let e = ex.u8()?;
                                ex.take(3)?;
                                out.push_str(error_name(e).ok_or(format!("error code {e:#x}"))?);
                            }
                            t => return Err(format!("array element type {t}")),
                        }
                    }
                }
                out.push('}');
                push(&mut st, out, &mut space);
            }
            0x21 | 0x41 | 0x61 => {
                let id = rd.u16()?;
                let (name, n) = crate::xlsb_ftab::ftab(id).ok_or(format!("function {id:#x}"))?;
                let args = pop_n(&mut st, n)?;
                push(&mut st, format!("{name}({})", args.join(",")), &mut space);
            }
            0x22 | 0x42 | 0x62 => {
                let n = usize::from(rd.u8()?);
                let id = rd.u16()? & 0x7fff;
                let mut args = pop_n(&mut st, n)?;
                let name = if id == 0xFF {
                    if args.is_empty() {
                        return Err("user-defined function without a name".into());
                    }
                    args.remove(0)
                } else {
                    crate::xlsb_ftab::ftab(id)
                        .ok_or(format!("function {id:#x}"))?
                        .0
                        .to_string()
                };
                push(&mut st, format!("{name}({})", args.join(",")), &mut space);
            }
            0x23 | 0x43 | 0x63 => {
                let n = ctx.name_text(rd.u32()?)?;
                push(&mut st, n, &mut space);
            }
            0x24 | 0x44 | 0x64 | 0x2C | 0x4C | 0x6C => {
                let row = rd.u32()?;
                let c = rd.u16()?;
                let (row_rel, col_rel) = (c & 0x8000 != 0, c & 0x4000 != 0);
                let (row, col) = if ptg & 0x1F == 0x0C {
                    (
                        if row_rel { rel_row(base.0, row) } else { row },
                        if col_rel {
                            rel_col(base.1, u32::from(c & 0x3FFF))
                        } else {
                            u32::from(c & 0x3FFF)
                        },
                    )
                } else {
                    (row, u32::from(c & 0x3FFF))
                };
                push(&mut st, ref_text(row, col, row_rel, col_rel), &mut space);
            }
            0x25 | 0x45 | 0x65 | 0x2D | 0x4D | 0x6D => {
                let s = area_text(&mut rd, ptg & 0x1F == 0x0D, base)?;
                push(&mut st, s, &mut space);
            }
            0x26 | 0x46 | 0x66 | 0x27 | 0x47 | 0x67 | 0x28 | 0x48 | 0x68 => {
                rd.take(6)?;
                if ptg & 0x1F == 0x06 {
                    let n = ex.u32()? as usize;
                    ex.take(n * 16)?;
                }
            }
            0x29 | 0x49 | 0x69 => {
                rd.take(2)?;
            }
            0x2A | 0x4A | 0x6A => {
                rd.take(6)?;
                push(&mut st, "#REF!".into(), &mut space);
            }
            0x2B | 0x4B | 0x6B => {
                rd.take(12)?;
                push(&mut st, "#REF!".into(), &mut space);
            }
            0x39 | 0x59 | 0x79 => {
                let ixti = rd.u16()?;
                let idx = rd.u32()?;
                if let Some(&(book, _, _)) = ctx.xti.get(usize::from(ixti)) {
                    if let Some(Book::Ext(n)) = ctx.books.get(book as usize) {
                        let name = (idx as usize)
                            .checked_sub(1)
                            .and_then(|i| ctx.ext.get(n - 1)?.names.get(i))
                            .ok_or("external name index out of range")?;
                        push(&mut st, format!("[{n}]!{name}"), &mut space);
                        continue;
                    }
                }
                let n = ctx.name_text(idx)?;

                let scoped = n.contains('!');
                let s = if scoped {
                    n
                } else {
                    format!("{}{n}", ctx.sheet_prefix(ixti)?)
                };
                push(&mut st, s, &mut space);
            }
            0x3A | 0x5A | 0x7A => {
                let pre = ctx.sheet_prefix(rd.u16()?)?;
                let row = rd.u32()?;
                let c = rd.u16()?;
                let t = ref_text(row, u32::from(c & 0x3FFF), c & 0x8000 != 0, c & 0x4000 != 0);
                push(&mut st, format!("{pre}{t}"), &mut space);
            }
            0x3B | 0x5B | 0x7B => {
                let pre = ctx.sheet_prefix(rd.u16()?)?;
                let t = area_text(&mut rd, false, base)?;
                push(&mut st, format!("{pre}{t}"), &mut space);
            }
            0x3C | 0x5C | 0x7C => {
                rd.take(8)?;
                push(&mut st, "#REF!".into(), &mut space);
            }
            0x3D | 0x5D | 0x7D => {
                rd.take(14)?;
                push(&mut st, "#REF!".into(), &mut space);
            }
            _ => return Err(format!("formula token {ptg:#x}")),
        }
    }
    if st.len() != 1 {
        return Err(format!("formula leaves {} values on the stack", st.len()));
    }
    Ok(Dec::Text(st.remove(0)))
}

fn area_text(rd: &mut Rd<'_>, relative: bool, base: (u32, u32)) -> Result<String, String> {
    let r0 = rd.u32()?;
    let r1 = rd.u32()?;
    let c0 = rd.u16()?;
    let c1 = rd.u16()?;
    let (rr0, rr1) = (c0 & 0x8000 != 0, c1 & 0x8000 != 0);
    let (cr0, cr1) = (c0 & 0x4000 != 0, c1 & 0x4000 != 0);
    let (mut a, mut b) = (r0, r1);
    let (mut ca, mut cb) = (u32::from(c0 & 0x3FFF), u32::from(c1 & 0x3FFF));
    if relative {
        if rr0 {
            a = rel_row(base.0, r0);
        }
        if rr1 {
            b = rel_row(base.0, r1);
        }
        if cr0 {
            ca = rel_col(base.1, ca);
        }
        if cr1 {
            cb = rel_col(base.1, cb);
        }
    }
    Ok(if a == 0 && b == MAX_ROW {
        format!("{}:{}", col_text(ca, cr0), col_text(cb, cr1))
    } else if ca == 0 && cb == MAX_COL {
        format!(
            "{}{}:{}{}",
            if rr0 { "" } else { "$" },
            a + 1,
            if rr1 { "" } else { "$" },
            b + 1
        )
    } else {
        format!(
            "{}:{}",
            ref_text(a, ca, rr0, cr0),
            ref_text(b, cb, rr1, cr1)
        )
    })
}

fn parsed_formula<'a>(rd: &mut Rd<'a>) -> Result<(&'a [u8], &'a [u8]), String> {
    let cce = rd.u32()? as usize;
    let rgce = rd.take(cce)?;
    let cb = rd.u32()? as usize;
    let rgcb = rd.take(cb.min(rd.rest().len()))?;
    Ok((rgce, rgcb))
}

pub(crate) struct Formulas {
    pub sheets: Vec<String>,
    pub xti: Vec<(u32, i32, i32)>,
    pub books: Vec<Book>,
    pub names: Vec<(String, Option<u32>)>,
    pub ext: Vec<ext::ExtBook>,
}

impl Formulas {
    pub(crate) fn decode(
        &self,
        rgce: &[u8],
        rgcb: &[u8],
        base: (u32, u32),
        cur: Option<u32>,
    ) -> Result<Dec, String> {
        let names: Vec<NameDef> = self
            .names
            .iter()
            .map(|(n, s)| NameDef {
                name: n.clone(),
                flags: 0,
                scope: *s,
                rgce: Vec::new(),
                rgcb: Vec::new(),
            })
            .collect();
        let ctx = Ctx {
            sheets: &self.sheets,
            xti: &self.xti,
            books: &self.books,
            ext: &self.ext,
            names: &names,
            tables: &[],
            cur: std::cell::Cell::new(cur),
        };
        decode(&ctx, rgce, rgcb, base)
    }
}

enum Val {
    Blank,
    Num(f64),
    Text(String),
    Sst(u32),
    Bool(bool),
    Err(&'static str),
}

enum Form {
    None,
    Text(String),

    Master,
    Array(String, (u32, u32, u32, u32)),
    DataTable(String),
}

struct Cell {
    row: u32,
    col: u32,
    xf: u32,
    val: Val,
    formula: bool,
    form: Form,
    cm: u32,
}

#[derive(Default)]
struct Shared {
    rgce: Vec<u8>,
    rgcb: Vec<u8>,
}

type HlIo<'a> = (
    &'a mut Vec<(String, String, String)>,
    &'a HashMap<String, String>,
);

fn read_sheet(
    ctx: &Ctx<'_>,
    data: &[u8],
    dynamic_cm: &[bool],
    table_rids: &[String],
    sheet: &str,
    warnings: &mut Vec<String>,
    (hl_rels, ext_targets): HlIo<'_>,
) -> Result<String, String> {
    let mut links = String::new();
    let mut rows: Vec<(u32, bool, Option<f64>, u16)> = Vec::new();

    let mut cols: Vec<(u32, u32, u32, bool, bool, u16)> = Vec::new();
    let mut sx = SheetXml::default();
    let mut cells: Vec<(usize, Cell)> = Vec::new();
    let mut shared: HashMap<(u32, u32), Shared> = HashMap::new();
    let mut array_masters: Vec<(u32, u32)> = Vec::new();
    let mut code_name = None;
    let mut cm = 0u32;
    let mut cur_row = 0u32;

    let mut pending_table: Option<(u32, u32, String)> = None;
    let mut filt = crate::xlsb_filter::Reader::default();
    let mut spark = crate::xlsb_spark::Reader::default();
    let mut cfdv = crate::xlsb_cfdv::Reader::default();
    let mut in_custom_view = false;
    let mut custom_views = 0usize;
    for (id, d) in recs(data) {
        let mut rd = Rd::new(d);

        match id {
            423 => {
                in_custom_view = true;
                custom_views += 1;
                continue;
            }
            424 => {
                in_custom_view = false;
                continue;
            }
            _ if in_custom_view => continue,
            _ => {}
        }
        match id {
            137 => sx.view(d),
            151 => sx.pane(d),
            152 => sx.select(d),
            176 => sx.merge(d),
            535 => sx.protection(d),
            id if crate::xlsb_filter::Reader::wants(id) => filt.rec(id, d)?,
            id if crate::xlsb_spark::Reader::wants(id) => spark.rec(id, d, ctx, warnings)?,
            id if crate::xlsb_cfdv::Reader::wants(id) => cfdv.rec(id, d, ctx, warnings)?,
            477 => sx.print_options(d),
            476 => sx.margins(d),
            478 => sx.page_setup(d),
            479 => sx.header_footer(d),
            392 => sx.breaks_begin(true),
            394 => sx.breaks_begin(false),
            396 => sx.brk(d),
            147 => {
                sx.ws_prop(d);

                if d.len() > 19 {
                    let mut r = Rd::new(&d[19..]);
                    if let Ok(n) = r.wstr() {
                        if !n.is_empty() {
                            code_name = Some(n);
                        }
                    }
                }
            }
            0 => {
                let r = rd.u32()?;
                rd.take(4)?;
                let twips = rd.u16()?;
                let flags = rd.u16()?;
                cur_row = r;

                let custom = (flags & 0x2000 != 0).then(|| f64::from(twips) / 20.0);
                rows.push((r, flags & 0x1000 != 0, custom, flags));
            }
            49 => cm = rd.u32()?,
            60 => {
                let (min, max, w) = (rd.u32()?, rd.u32()?, rd.u32()?);
                rd.u32()?;
                let fl = rd.u16()?;
                cols.push((min, max, w, fl & 1 != 0, fl & 2 != 0, fl));
            }
            1..=11 => {
                let col = rd.u32()?;
                let xf = rd.u32()? & 0x00FF_FFFF;
                let (val, formula) = match id {
                    1 => (Val::Blank, false),
                    2 => (Val::Num(rk_value(rd.u32()?)), false),
                    3 | 11 => {
                        let e = rd.u8()?;
                        match error_name(e) {
                            Some(n) => (Val::Err(n), id == 11),
                            None => {
                                warnings.push(format!(
                                    "{sheet}!{}: error code {e:#x} unsupported",
                                    cell_name(cur_row, col)
                                ));
                                (Val::Blank, id == 11)
                            }
                        }
                    }
                    4 | 10 => (Val::Bool(rd.u8()? != 0), id == 10),
                    5 | 9 => (Val::Num(rd.f64()?), id == 9),
                    6 => (Val::Text(rd.wstr()?), false),
                    7 => (Val::Sst(rd.u32()?), false),
                    8 => (Val::Text(rd.wstr()?), true),
                    _ => unreachable!(),
                };
                let mut form = Form::None;
                if formula {
                    rd.u16()?;
                    let (rgce, rgcb) = parsed_formula(&mut rd)?;
                    match decode(ctx, rgce, rgcb, (cur_row, col)) {
                        Ok(Dec::Text(t)) => form = Form::Text(t),
                        Ok(Dec::Exp {
                            row,
                            col: mc,
                            table,
                        }) => {
                            if table || (row, mc) == (cur_row, col) {
                                form = Form::Master;
                            } else if let Some(s) = shared.get(&(row, mc)) {
                                match decode(ctx, &s.rgce, &s.rgcb, (cur_row, col)) {
                                    Ok(Dec::Text(t)) => form = Form::Text(t),
                                    Ok(Dec::Exp { .. }) => {}
                                    Err(e) => warnings.push(format!(
                                        "{sheet}!{}: shared formula not supported: {e}",
                                        cell_name(cur_row, col)
                                    )),
                                }
                            } else if !array_masters.contains(&(row, mc)) {
                                warnings.push(format!(
                                    "{sheet}!{}: shared formula {} has no master",
                                    cell_name(cur_row, col),
                                    cell_name(row, mc)
                                ));
                            }
                        }
                        Err(e) => warnings.push(format!(
                            "{sheet}!{}: formula not supported: {e}",
                            cell_name(cur_row, col)
                        )),
                    }
                }
                if matches!(&pending_table, Some((r, c, _)) if (*r, *c) == (cur_row, col)) {
                    if let Some((_, _, f)) = pending_table.take() {
                        form = Form::DataTable(f);
                    }
                }
                let idx = rows.len();
                cells.push((
                    idx,
                    Cell {
                        row: cur_row,
                        col,
                        xf,
                        val,
                        formula,
                        form,
                        cm: std::mem::take(&mut cm),
                    },
                ));
            }
            427 | 426 => {
                let r0 = rd.u32()?;
                let r1 = rd.u32()?;
                let c0 = rd.u32()?;
                let c1 = rd.u32()?;
                if id == 426 {
                    rd.u8()?;
                }
                let (rgce, rgcb) = parsed_formula(&mut rd)?;
                let Some((_, cell)) = cells.last_mut() else {
                    continue;
                };
                if !matches!(cell.form, Form::Master) {
                    continue;
                }
                let at = (cell.row, cell.col);
                match decode(ctx, rgce, rgcb, at) {
                    Ok(Dec::Text(t)) => {
                        if id == 427 {
                            shared.insert(
                                at,
                                Shared {
                                    rgce: rgce.to_vec(),
                                    rgcb: rgcb.to_vec(),
                                },
                            );
                            cell.form = Form::Text(t);
                        } else {
                            array_masters.push(at);
                            cell.form = Form::Array(t, (r0, c0, r1, c1));
                        }
                    }
                    Ok(Dec::Exp { .. }) => cell.form = Form::None,
                    Err(e) => {
                        cell.form = Form::None;
                        warnings.push(format!(
                            "{sheet}!{}: formula not supported: {e}",
                            cell_name(at.0, at.1)
                        ));
                    }
                }
            }
            494 => {
                let (r0, r1) = (rd.u32()?, rd.u32()?);
                let (c0, c1) = (rd.u32()?, rd.u32()?);
                let (rid, loc, tip, disp) = (rd.wstr()?, rd.wstr()?, rd.wstr()?, rd.wstr()?);
                let _ = write!(links, "<hyperlink ref=\"{}\"", rect_name(r0, c0, r1, c1));
                if let Some(t) = ext_targets.get(&rid) {
                    let id = format!("rIdH{}", hl_rels.len() + 1);
                    let _ = write!(links, " r:id=\"{id}\"");
                    hl_rels.push((id, format!("{REL}/hyperlink"), t.clone()));
                }
                for (k, v) in [("location", &loc), ("tooltip", &tip), ("display", &disp)] {
                    if !v.is_empty() {
                        let _ = write!(links, " {k}=\"{}\"", esc(v));
                    }
                }
                links.push_str("/>");
            }
            428 => {
                let r0 = rd.u32()?;
                let r1 = rd.u32()?;
                let c0 = rd.u32()?;
                let c1 = rd.u32()?;
                let (ir1, ic1) = (rd.u32()?, rd.u32()?);
                let (ir2, ic2) = (rd.u32()?, rd.u32()?);
                let flags = rd.u8()?;
                let two = flags & 2 != 0;
                let row_table = flags & 1 != 0;
                let mut f = format!(
                    "<f t=\"dataTable\" ref=\"{}\" r1=\"{}\"",
                    rect_name(r0, c0, r1, c1),
                    cell_name(ir1, ic1)
                );
                if two {
                    let _ = write!(f, " dt2D=\"1\" r2=\"{}\"", cell_name(ir2, ic2));
                } else if row_table {
                    f.push_str(" dtr=\"1\"");
                }
                f.push_str("/>");
                match cells.last_mut() {
                    Some((_, cell)) if matches!(cell.form, Form::Master) => {
                        cell.form = Form::DataTable(f);
                    }
                    _ => pending_table = Some((r0, c0, f)),
                }
            }
            _ => {}
        }
    }
    let mut x = String::new();
    x.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"");
    x.push_str(REL);
    x.push_str("\">");
    x.push_str(&sx.sheet_pr(code_name.as_deref()));
    x.push_str(&sx.views());
    let (row_level, col_level) = (
        rows.iter().map(|r| (r.3 >> 8) & 7).max().unwrap_or(0),
        cols.iter().map(|c| (c.5 >> 8) & 7).max().unwrap_or(0),
    );
    if row_level > 0 || col_level > 0 {
        let _ = write!(x, "<sheetFormatPr defaultRowHeight=\"15\"");
        if row_level > 0 {
            let _ = write!(x, " outlineLevelRow=\"{row_level}\"");
        }
        if col_level > 0 {
            let _ = write!(x, " outlineLevelCol=\"{col_level}\"");
        }
        x.push_str("/>");
    }
    if !cols.is_empty() {
        x.push_str("<cols>");
        for (min, max, w, hidden, custom, fl) in &cols {
            let _ = write!(
                x,
                "<col min=\"{}\" max=\"{}\" width=\"{}\"{}{}",
                min + 1,
                max + 1,
                f64::from(*w) / 256.0,
                if *hidden { " hidden=\"1\"" } else { "" },
                if *custom { " customWidth=\"1\"" } else { "" }
            );
            if (fl >> 8) & 7 > 0 {
                let _ = write!(x, " outlineLevel=\"{}\"", (fl >> 8) & 7);
            }
            if fl & 0x800 != 0 {
                x.push_str(" collapsed=\"1\"");
            }
            x.push_str("/>");
        }
        x.push_str("</cols>");
    }
    x.push_str("<sheetData>");
    let mut ci = 0usize;
    for (i, (r, hidden, custom, fl)) in rows.iter().enumerate() {
        let _ = write!(
            x,
            "<row r=\"{}\"{}{}{}{}{}>",
            r + 1,
            custom.map_or(String::new(), |h| format!(" ht=\"{h}\" customHeight=\"1\"")),
            if *hidden { " hidden=\"1\"" } else { "" },
            if (fl >> 8) & 7 > 0 {
                format!(" outlineLevel=\"{}\"", (fl >> 8) & 7)
            } else {
                String::new()
            },
            if fl & 0x800 != 0 {
                " collapsed=\"1\""
            } else {
                ""
            },
            match fl & 3 {
                1 => " thickTop=\"1\"",
                2 => " thickBot=\"1\"",
                3 => " thickTop=\"1\" thickBot=\"1\"",
                _ => "",
            }
        );
        while ci < cells.len() && cells[ci].0 == i + 1 {
            write_cell(&mut x, &cells[ci].1, dynamic_cm);
            ci += 1;
        }
        x.push_str("</row>");
    }
    x.push_str("</sheetData>");
    if custom_views > 0 {
        warnings.push(format!(
            "sheet {sheet}: {custom_views} custom view(s) not carried"
        ));
    }
    x.push_str(sx.protection_xml());
    x.push_str(filt.xml());
    x.push_str(&sx.merges_xml());
    x.push_str(cfdv.cf_xml());
    x.push_str(&cfdv.dv_xml());
    if !links.is_empty() {
        let _ = write!(x, "<hyperlinks>{links}</hyperlinks>");
    }
    x.push_str(&sx.tail_xml());
    for (i, rid) in table_rids.iter().enumerate() {
        if i == 0 {
            x.push_str("<tableParts>");
        }
        let _ = write!(x, "<tablePart r:id=\"{rid}\"/>");
        if i + 1 == table_rids.len() {
            x.push_str("</tableParts>");
        }
    }
    x.push_str(spark.xml());
    x.push_str("</worksheet>");
    Ok(x)
}

fn write_cell(x: &mut String, c: &Cell, dynamic_cm: &[bool]) {
    let mut attrs = format!("r=\"{}\"", cell_name(c.row, c.col));
    if c.xf != 0 {
        let _ = write!(attrs, " s=\"{}\"", c.xf);
    }
    let (ty, v) = match &c.val {
        Val::Blank => ("", String::new()),
        Val::Num(n) => ("", format!("<v>{n}</v>")),
        Val::Bool(b) => ("b", format!("<v>{}</v>", u8::from(*b))),
        Val::Err(e) => ("e", format!("<v>{}</v>", esc(e))),
        Val::Sst(i) => ("s", format!("<v>{i}</v>")),
        Val::Text(t) if c.formula => ("str", format!("<v>{}</v>", xstr(t))),
        Val::Text(t) => (
            "inlineStr",
            format!("<is><t xml:space=\"preserve\">{}</t></is>", xstr(t)),
        ),
    };
    if !ty.is_empty() {
        let _ = write!(attrs, " t=\"{ty}\"");
    }
    let f = match &c.form {
        Form::Text(t) => format!("<f>{}</f>", esc(t)),
        Form::Array(t, (r0, c0, r1, c1)) => {
            if c.cm > 0 && dynamic_cm.get(c.cm as usize).copied().unwrap_or(false) {
                let _ = write!(attrs, " cm=\"{}\"", c.cm);
            }
            format!(
                "<f t=\"array\" ref=\"{}\">{}</f>",
                rect_name(*r0, *c0, *r1, *c1),
                esc(t)
            )
        }
        Form::DataTable(f) => f.clone(),
        Form::None | Form::Master => String::new(),
    };
    let _ = write!(x, "<c {attrs}>{f}{v}</c>");
}

pub(crate) struct Pkg {
    pub(crate) parts: Vec<(String, Vec<u8>)>,
}

impl Pkg {
    pub(crate) fn text(&mut self, name: &str, s: String) {
        self.parts.push((name.to_string(), s.into_bytes()));
    }
}

#[derive(Default)]
struct Pivots {
    caches: Vec<String>,
    tables: usize,
}

const PIVOT_TABLE_TYPE: &str =
    "application/vnd.openxmlformats-officedocument.spreadsheetml.pivotTable+xml";
const PIVOT_CACHE_TYPE: &str =
    "application/vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheDefinition+xml";

impl Pivots {
    fn add(
        &mut self,
        zip: &mut Zip,
        sheet: &str,
        pkg: &mut Pkg,
        types: &mut Vec<(String, String)>,
        warnings: &mut Vec<String>,
    ) -> Vec<(String, String, String)> {
        let mut out = Vec::new();
        for (_, ty, tp) in rels(zip, sheet).unwrap_or_default() {
            if !ty.ends_with("/pivotTable") {
                continue;
            }
            let Some(bin) = part(zip, &tp) else {
                warnings.push(format!("pivot table {tp}: missing part"));
                continue;
            };
            let xml = match crate::xlsb_pivot::table(&bin) {
                Ok((x, _)) => x,
                Err(e) => {
                    warnings.push(format!("pivot table {tp}: {e}"));
                    continue;
                }
            };
            let Some(cache_src) = rels(zip, &tp)
                .unwrap_or_default()
                .into_iter()
                .find(|r| r.1.ends_with("/pivotCacheDefinition"))
                .map(|r| r.2)
            else {
                warnings.push(format!("pivot table {tp}: no cache"));
                continue;
            };
            let k = if let Some(k) = self.caches.iter().position(|c| *c == cache_src) {
                k + 1
            } else {
                let Some(cbin) = part(zip, &cache_src) else {
                    warnings.push(format!("pivot cache {cache_src}: missing part"));
                    continue;
                };
                match crate::xlsb_pivot::cache_def(&cbin, "rId1", warnings) {
                    Ok(cx) => {
                        self.caches.push(cache_src.clone());
                        let k = self.caches.len();
                        let name = format!("xl/pivotCache/pivotCacheDefinition{k}.xml");
                        pkg.text(&name, cx);
                        types.push((name, PIVOT_CACHE_TYPE.into()));
                        k
                    }
                    Err(e) => {
                        warnings.push(format!("pivot cache {cache_src}: {e}"));
                        continue;
                    }
                }
            };
            self.tables += 1;
            let n = self.tables;
            let name = format!("xl/pivotTables/pivotTable{n}.xml");
            pkg.text(&name, xml);
            pkg.text(
                &format!("xl/pivotTables/_rels/pivotTable{n}.xml.rels"),
                rels_xml(&[(
                    "rId1".into(),
                    format!("{REL}/pivotCacheDefinition"),
                    format!("/xl/pivotCache/pivotCacheDefinition{k}.xml"),
                )]),
            );
            types.push((name.clone(), PIVOT_TABLE_TYPE.into()));
            out.push((
                format!("rIdPv{n}"),
                format!("{REL}/pivotTable"),
                format!("/{name}"),
            ));
        }
        out
    }
}

pub(crate) fn rels_xml(items: &[(String, String, String)]) -> String {
    let mut s = String::from(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
    );
    for (id, ty, target) in items {
        let _ = write!(
            s,
            "<Relationship Id=\"{id}\" Type=\"{ty}\" Target=\"{}\"{}/>",
            esc(target),
            if ty.ends_with("/hyperlink") {
                " TargetMode=\"External\""
            } else {
                ""
            }
        );
    }
    s.push_str("</Relationships>");
    s
}

fn read_table(zip: &mut Zip, path: &str) -> Result<TableDef, String> {
    let data = part(zip, path).ok_or_else(|| format!("missing table part {path}"))?;
    let mut t = TableDef {
        id: 0,
        name: String::new(),
        rect: (0, 0, 0, 0),
        headers: 1,
        totals: 0,
        columns: Vec::new(),
        totals_cols: Vec::new(),
        filter: None,
        style: None,
    };
    for (id, d) in recs(&data) {
        let mut rd = Rd::new(d);
        match id {
            161 => {
                let r0 = rd.u32()?;
                let r1 = rd.u32()?;
                let c0 = rd.u32()?;
                let c1 = rd.u32()?;
                t.filter = Some((r0, c0, r1, c1));
            }
            513 => {
                let flags = rd.u16()?;
                t.style = Some((flags, rd.wstr()?));
            }
            343 => {
                let r0 = rd.u32()?;
                let r1 = rd.u32()?;
                let c0 = rd.u32()?;
                let c1 = rd.u32()?;
                t.rect = (r0, c0, r1, c1);
                rd.u32()?;
                t.id = rd.u32()?;
                t.headers = rd.u32()?;
                t.totals = rd.u32()?;
                rd.take(4 + 6 * 4 + 4)?;
                let name = rd.wstr()?;
                let display = rd.wstr()?;
                t.name = if name.is_empty() { display } else { name };
            }
            347 => {
                rd.take(4)?;
                let ilta = rd.u32()?;
                rd.take(5 * 4)?;
                t.columns.push(rd.wstr()?);
                t.totals_cols.push((ilta, rd.wstr().unwrap_or_default()));
            }
            _ => {}
        }
    }
    Ok(t)
}

fn legacy_rid(data: &[u8]) -> Option<String> {
    recs(data)
        .find(|(id, _)| *id == 551)
        .and_then(|(_, d)| Rd::new(d).wstr().ok())
}

fn comments_xml(bin: &[u8]) -> String {
    let mut authors = String::new();
    let mut list = String::new();
    let mut cur: Option<(u32, String)> = None;
    for (id, d) in recs(bin) {
        let mut rd = Rd::new(d);
        match id {
            632 => {
                if let Ok(a) = rd.wstr() {
                    let _ = write!(authors, "<author>{}</author>", xstr(&a));
                }
            }
            635 => {
                let author = rd.u32().unwrap_or(0);
                let rows = (rd.u32(), rd.u32(), rd.u32(), rd.u32());
                if let (Ok(r0), Ok(_), Ok(c0), Ok(_)) = rows {
                    cur = Some((author, rect_name(r0, c0, r0, c0)));
                }
            }
            637 => {
                if let (Some((author, at)), Ok(_), Ok(t)) = (cur.take(), rd.u8(), rd.wstr()) {
                    let _ = write!(
                        list,
                        "<comment ref=\"{at}\" authorId=\"{author}\"><text><t xml:space=\"preserve\">{}</t></text></comment>",
                        xstr(&t)
                    );
                }
            }
            _ => {}
        }
    }
    format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<comments xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><authors>{authors}</authors><commentList>{list}</commentList></comments>"
    )
}

fn drawing_rid(data: &[u8]) -> Option<String> {
    recs(data)
        .find(|(id, _)| *id == 550)
        .and_then(|(_, d)| Rd::new(d).wstr().ok())
}

fn copy_drawing(
    zip: &mut Zip,
    part_name: &str,
    types: &Node,
    pkg: &mut Pkg,
    done: &mut Vec<String>,
    ov: &mut Vec<(String, String)>,
) {
    if done.iter().any(|d| d == part_name) {
        return;
    }
    let Some(bytes) = part(zip, part_name) else {
        return;
    };
    done.push(part_name.to_string());
    pkg.parts.push((part_name.to_string(), bytes));
    let ct = types
        .children("Override")
        .find(|o| {
            o.attr("PartName")
                .is_some_and(|p| p.trim_start_matches('/').eq_ignore_ascii_case(part_name))
        })
        .and_then(|o| o.attr("ContentType"))
        .map(str::to_string)
        .or_else(|| {
            let ext = part_name
                .rsplit_once('.')
                .map(|x| x.1.to_ascii_lowercase())?;
            types
                .children("Default")
                .find(|d| {
                    d.attr("Extension")
                        .is_some_and(|e| e.eq_ignore_ascii_case(&ext))
                })
                .and_then(|d| d.attr("ContentType"))
                .map(str::to_string)
        });
    if let Some(c) = ct {
        ov.push((part_name.to_string(), c));
    }
    let rn = rels_name(part_name);
    if let Some(r) = part(zip, &rn) {
        pkg.parts.push((rn, r));
    }
    for (_, _, target) in rels(zip, part_name).unwrap_or_default() {
        copy_drawing(zip, &target, types, pkg, done, ov);
    }
}

pub fn to_xlsx(zip: &mut Zip) -> Result<(Vec<u8>, Vec<String>), String> {
    let mut warnings = Vec::new();
    let wb_rels = rels(zip, "xl/workbook.bin")?;
    let wbin = part(zip, "xl/workbook.bin").ok_or("no xl/workbook.bin")?;

    let mut date1904 = false;
    let mut protect = (false, false, false, 0u16);
    let mut wb_code = None;
    let mut bundles: Vec<(u32, String, String)> = Vec::new();
    let mut calc = String::new();
    let mut xti: Vec<(u32, i32, i32)> = Vec::new();
    let mut books: Vec<Book> = Vec::new();
    let mut exts: Vec<ext::ExtBook> = Vec::new();
    let mut names: Vec<NameDef> = Vec::new();
    for (id, d) in recs(&wbin) {
        let mut rd = Rd::new(d);
        match id {
            534 => {
                let pwd = rd.u16()?;
                rd.u16()?;
                let f = rd.u16()?;
                protect = (f & 1 != 0, f & 2 != 0, f & 4 != 0, pwd);
            }
            153 => {
                date1904 = rd.u32()? & 1 != 0;
                rd.u32()?;
                if let Ok(n) = rd.wstr() {
                    if !n.is_empty() {
                        wb_code = Some(n);
                    }
                }
            }
            156 => {
                let state = rd.u32()?;
                rd.u32()?;
                let rid = rd.wstr()?;
                let name = rd.wstr()?;
                bundles.push((state, rid, name));
            }
            157 => {
                let calc_id = rd.u32()?;
                let _ = write!(calc, " calcId=\"{calc_id}\"");
                let auto = rd.u32()?;
                let count = rd.u32()?;
                let delta = rd.f64()?;
                rd.u32()?;
                let flags = rd.u16()?;
                if auto == 0 {
                    calc.push_str(" calcMode=\"manual\"");
                }
                if flags & 1 != 0 {
                    calc.push_str(" fullCalcOnLoad=\"1\"");
                }
                if flags & 4 != 0 {
                    let _ = write!(
                        calc,
                        " iterate=\"1\" iterateCount=\"{count}\" iterateDelta=\"{delta}\""
                    );
                }
                if flags & 8 == 0 {
                    calc.push_str(" fullPrecision=\"0\"");
                }
            }
            355 => {
                let rid = rd.wstr()?;
                let path = wb_rels.iter().find(|r| r.0 == rid).map(|r| r.2.clone());
                match path {
                    Some(p) => {
                        exts.push(ext::read(zip, &p)?);
                        books.push(Book::Ext(exts.len()));
                    }
                    None => books.push(Book::Other),
                }
            }
            357 | 358 => books.push(Book::This),
            667 => books.push(Book::Other),
            362 => {
                let n = rd.u32()?;
                for _ in 0..n {
                    let b = rd.u32()?;
                    let a = rd.u32()? as i32;
                    let z = rd.u32()? as i32;
                    xti.push((b, a, z));
                }
            }
            39 => {
                let flags = rd.u32()?;
                rd.u8()?;
                let itab = rd.u32()?;
                let name = rd.wstr()?;
                let (rgce, rgcb) = match parsed_formula(&mut rd) {
                    Ok((a, b)) => (a.to_vec(), b.to_vec()),
                    Err(_) => (Vec::new(), Vec::new()),
                };
                names.push(NameDef {
                    name,
                    flags,
                    scope: (itab != 0xFFFF_FFFF).then_some(itab),
                    rgce,
                    rgcb,
                });
            }
            _ => {}
        }
    }
    let sheet_names: Vec<String> = bundles.iter().map(|b| b.2.clone()).collect();

    let mut sst = String::from(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<sst xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">",
    );
    if let Some((_, _, p)) = wb_rels.iter().find(|r| r.1.ends_with("/sharedStrings")) {
        if let Some(b) = part(zip, p) {
            for (id, d) in recs(&b) {
                if id == 19 {
                    let mut rd = Rd::new(d);
                    rd.u8()?;
                    let _ = write!(
                        sst,
                        "<si><t xml:space=\"preserve\">{}</t></si>",
                        xstr(&rd.wstr()?)
                    );
                }
            }
        }
    }
    sst.push_str("</sst>");

    let mut styles = String::from(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"/>",
    );
    if let Some((_, _, p)) = wb_rels.iter().find(|r| r.1.ends_with("/styles")) {
        if let Some(b) = part(zip, p) {
            styles = crate::xlsb_styles::to_xml(recs(&b).map(|(i, d)| (i, d.to_vec())))?;
        }
    }

    let mut meta = String::new();
    let mut dynamic_cm = vec![false];
    if let Some((_, _, p)) = wb_rels.iter().find(|r| r.1.ends_with("/sheetMetadata")) {
        if let Some(b) = part(zip, p) {
            let mut types: Vec<String> = Vec::new();
            let mut in_cell = false;
            let mut blocks: Vec<u32> = Vec::new();
            for (id, d) in recs(&b) {
                let mut rd = Rd::new(d);
                match id {
                    335 => {
                        rd.take(8)?;
                        types.push(rd.wstr()?);
                    }
                    337 => in_cell = true,
                    338 => in_cell = false,
                    51 if in_cell => blocks.push(rd.u32()?),
                    _ => {}
                }
            }
            meta.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<metadata xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><metadataTypes>");
            for t in &types {
                let _ = write!(meta, "<metadataType name=\"{}\"/>", esc(t));
            }
            meta.push_str("</metadataTypes><cellMetadata>");
            for t in &blocks {
                let dynamic = types
                    .get((*t as usize).wrapping_sub(1))
                    .is_some_and(|n| n == "XLDAPR");
                dynamic_cm.push(dynamic);
                let _ = write!(meta, "<bk><rc t=\"{t}\" v=\"0\"/></bk>");
            }
            meta.push_str("</cellMetadata></metadata>");
        }
    }

    let mut tables: Vec<TableDef> = Vec::new();
    let mut sheet_tables: Vec<Vec<usize>> = Vec::new();
    for (_, rid, _) in &bundles {
        let mut mine = Vec::new();
        if let Some((_, _, path)) = wb_rels.iter().find(|r| &r.0 == rid) {
            for (_, ty, tp) in rels(zip, path)? {
                if ty.ends_with("/table") {
                    match read_table(zip, &tp) {
                        Ok(t) => {
                            mine.push(tables.len());
                            tables.push(t);
                        }
                        Err(e) => warnings.push(format!("table {tp}: {e}")),
                    }
                }
            }
        }
        sheet_tables.push(mine);
    }

    let ctx = Ctx {
        sheets: &sheet_names,
        xti: &xti,
        books: &books,
        ext: &exts,
        names: &names,
        tables: &tables,
        cur: std::cell::Cell::new(None),
    };

    let mut dn = String::new();
    for n in &names {
        let auto = n.flags & 0x20 != 0;
        if n.name.starts_with("_xlnm.")
            || n.name.starts_with("_xlfn.")
            || n.name.starts_with("_xlpm.")
        {
            continue;
        }
        ctx.cur.set(n.scope);
        let text = match decode(&ctx, &n.rgce, &n.rgcb, (0, 0)) {
            Ok(Dec::Text(t)) => t,
            Ok(Dec::Exp { .. }) => {
                warnings.push(format!("name {}: unsupported formula", n.name));
                continue;
            }
            Err(e) => {
                warnings.push(format!("name {}: formula not supported: {e}", n.name));
                continue;
            }
        };
        let scope = n
            .scope
            .map(|s| format!(" localSheetId=\"{s}\""))
            .unwrap_or_default();
        let mut attrs = String::new();
        if n.flags & 1 != 0 {
            attrs.push_str(" hidden=\"1\"");
        }
        if n.flags & 2 != 0 && n.flags & 8 != 0 {
            attrs.push_str(" function=\"1\"");
        }
        if n.flags & 8 != 0 {
            attrs.push_str(" xlm=\"1\"");
            let group = (n.flags >> 6) & 0x1ff;
            if group != 0 {
                let _ = write!(attrs, " functionGroupId=\"{group}\"");
            }
        }
        let shown = if auto {
            format!("_xlnm.{}", n.name)
        } else {
            n.name.clone()
        };
        let _ = write!(
            dn,
            "<definedName name=\"{}\"{attrs}{scope}>{}</definedName>",
            esc(&shown),
            esc(&text)
        );
    }

    let mut pkg = Pkg { parts: Vec::new() };
    let mut sheets_xml = String::new();
    let mut wb_rel_items: Vec<(String, String, String)> = Vec::new();
    let mut copied: Vec<String> = Vec::new();
    let mut copied_types: Vec<(String, String)> = Vec::new();
    let mut pivots = Pivots::default();
    for (i, (state, rid, name)) in bundles.iter().enumerate() {
        let n = i + 1;
        let Some((_, _, path)) = wb_rels.iter().find(|r| &r.0 == rid) else {
            warnings.push(format!("sheet {name}: no part"));
            continue;
        };
        let Some(data) = part(zip, path) else {
            warnings.push(format!("sheet {name}: missing part {path}"));
            continue;
        };
        let mine = &sheet_tables[i];
        let table_rids: Vec<String> = (0..mine.len()).map(|k| format!("rId{}", k + 1)).collect();
        ctx.cur.set(Some(i as u32));
        let is_chart = wb_rels
            .iter()
            .any(|r| &r.0 == rid && r.1.ends_with("/chartsheet"));
        let macro_kind = wb_rels.iter().find(|r| &r.0 == rid).map_or(0, |r| {
            if r.1.ends_with("/xlMacrosheet") {
                1
            } else if r.1.ends_with("/xlIntlMacrosheet") {
                2
            } else {
                0
            }
        });
        let drawing_target = drawing_rid(&data).and_then(|d| {
            rels(zip, path)
                .unwrap_or_default()
                .into_iter()
                .find(|r| r.0 == d)
                .map(|r| r.2)
        });
        let drawing_rid_out = format!("rId{}", mine.len() + 1);
        let mut hl_rels: Vec<(String, String, String)> = Vec::new();
        let mut xml = if is_chart {
            String::from("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<chartsheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><sheetViews><sheetView workbookViewId=\"0\"/></sheetViews><pageMargins left=\"0.7\" right=\"0.7\" top=\"0.75\" bottom=\"0.75\" header=\"0.3\" footer=\"0.3\"/></chartsheet>")
        } else {
            read_sheet(
                &ctx,
                &data,
                &dynamic_cm,
                &table_rids,
                name,
                &mut warnings,
                (&mut hl_rels, &external_targets(zip, path)),
            )?
        };
        if macro_kind > 0 {
            let tag = if macro_kind == 2 {
                "xm:intlmacrosheet"
            } else {
                "xm:macrosheet"
            };
            xml = xml
                .replacen(
                    "<worksheet ",
                    &format!(
                        "<{tag} xmlns:xm=\"http://schemas.microsoft.com/office/excel/2006/main\" "
                    ),
                    1,
                )
                .replace("</worksheet>", &format!("</{tag}>"));
        }
        let mut items: Vec<(String, String, String)> = mine
            .iter()
            .enumerate()
            .map(|(k, &ti)| {
                (
                    format!("rId{}", k + 1),
                    format!("{REL}/table"),
                    format!("/xl/tables/table{}.xml", ti + 1),
                )
            })
            .collect();
        items.extend(pivots.add(zip, path, &mut pkg, &mut copied_types, &mut warnings));
        items.extend(hl_rels);
        if let Some(target) = drawing_target {
            let types = match tree(zip, "[Content_Types].xml") {
                Ok(Some(t)) => t,
                _ => Node::default(),
            };
            copy_drawing(
                zip,
                &target,
                &types,
                &mut pkg,
                &mut copied,
                &mut copied_types,
            );
            items.push((
                drawing_rid_out.clone(),
                format!("{REL}/drawing"),
                format!("/{target}"),
            ));
            let tag = format!("<drawing r:id=\"{drawing_rid_out}\"/>");
            let at = [
                "<legacyDrawing",
                "<oleObjects",
                "<controls",
                "<tableParts",
                "<extLst",
            ]
            .iter()
            .filter_map(|t| xml.find(t))
            .min()
            .or_else(|| xml.rfind("</worksheet>"))
            .or_else(|| xml.rfind("</chartsheet>"));
            if let Some(at) = at {
                xml.insert_str(at, &tag);
            }
        }
        let all_rels = rels(zip, path).unwrap_or_default();
        let comments_bin = all_rels
            .iter()
            .find(|r| r.1.ends_with("/comments"))
            .and_then(|r| part(zip, &r.2));
        let vml_target = legacy_rid(&data).and_then(|l| {
            all_rels
                .iter()
                .find(|r| r.0 == l && r.1.ends_with("/vmlDrawing"))
                .map(|r| r.2.clone())
        });
        if let Some(bin) = comments_bin {
            let cpart = format!("xl/comments{n}.xml");
            pkg.text(&cpart, comments_xml(&bin));
            copied_types.push((
                cpart.clone(),
                "application/vnd.openxmlformats-officedocument.spreadsheetml.comments+xml"
                    .to_string(),
            ));
            items.push((
                format!("rIdCm{n}"),
                format!("{REL}/comments"),
                format!("/{cpart}"),
            ));
        }
        if let Some(target) = vml_target {
            let types = match tree(zip, "[Content_Types].xml") {
                Ok(Some(t)) => t,
                _ => Node::default(),
            };
            copy_drawing(
                zip,
                &target,
                &types,
                &mut pkg,
                &mut copied,
                &mut copied_types,
            );
            items.push((
                format!("rIdVml{n}"),
                format!("{REL}/vmlDrawing"),
                format!("/{target}"),
            ));
            let tag = format!("<legacyDrawing r:id=\"rIdVml{n}\"/>");
            let at = ["<oleObjects", "<controls", "<tableParts", "<extLst"]
                .iter()
                .filter_map(|t| xml.find(t))
                .min()
                .or_else(|| xml.rfind("</worksheet>"));
            if let Some(at) = at {
                xml.insert_str(at, &tag);
            }
        }
        let kind_dir = if is_chart {
            "chartsheets"
        } else if macro_kind > 0 {
            "macrosheets"
        } else {
            "worksheets"
        };
        pkg.text(&format!("xl/{kind_dir}/sheet{n}.xml"), xml);
        if !items.is_empty() {
            pkg.text(
                &format!("xl/{kind_dir}/_rels/sheet{n}.xml.rels"),
                rels_xml(&items),
            );
        }
        let st = match state {
            1 => " state=\"hidden\"",
            2 => " state=\"veryHidden\"",
            _ => "",
        };
        let _ = write!(
            sheets_xml,
            "<sheet name=\"{}\" sheetId=\"{n}\"{st} r:id=\"rId{n}\"/>",
            esc(name)
        );
        let rel_type = match macro_kind {
            1 => "http://schemas.microsoft.com/office/2006/relationships/xlMacrosheet".to_string(),
            2 => "http://schemas.microsoft.com/office/2006/relationships/xlIntlMacrosheet"
                .to_string(),
            _ => format!(
                "{REL}/{}",
                if is_chart { "chartsheet" } else { "worksheet" }
            ),
        };
        wb_rel_items.push((
            format!("rId{n}"),
            rel_type,
            format!("/xl/{kind_dir}/sheet{n}.xml"),
        ));
    }
    for (i, t) in tables.iter().enumerate() {
        let (r0, c0, r1, c1) = t.rect;
        let mut x = format!(
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<table xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" id=\"{}\" name=\"{n}\" displayName=\"{d}\" ref=\"{}\" headerRowCount=\"{}\"",
            t.id,
            rect_name(r0, c0, r1, c1),
            t.headers,
            n = esc(&t.name),
            d = esc(&crate::write::display_name(&t.name))
        );
        if t.totals > 0 {
            let _ = write!(x, " totalsRowCount=\"{}\"", t.totals);
        }
        x.push('>');
        if let Some((f0, g0, f1, g1)) = t.filter {
            let _ = write!(x, "<autoFilter ref=\"{}\"/>", rect_name(f0, g0, f1, g1));
        }
        let _ = write!(x, "<tableColumns count=\"{}\">", t.columns.len());
        for (k, c) in t.columns.iter().enumerate() {
            let _ = write!(x, "<tableColumn id=\"{}\" name=\"{}\"", k + 1, esc(c));
            if let Some((ilta, label)) = t.totals_cols.get(k) {
                const FUNCS: [&str; 9] = [
                    "",
                    "average",
                    "count",
                    "countNums",
                    "max",
                    "min",
                    "sum",
                    "stdDev",
                    "var",
                ];
                if !label.is_empty() {
                    let _ = write!(x, " totalsRowLabel=\"{}\"", esc(label));
                }
                if let Some(f) = FUNCS.get(*ilta as usize).filter(|f| !f.is_empty()) {
                    let _ = write!(x, " totalsRowFunction=\"{f}\"");
                }
            }
            x.push_str("/>");
        }
        x.push_str("</tableColumns>");
        if let Some((fl, name)) = &t.style {
            let b = |bit: u16| u8::from(fl & bit != 0);
            let _ = write!(
                x,
                "<tableStyleInfo name=\"{}\" showFirstColumn=\"{}\" showLastColumn=\"{}\" showRowStripes=\"{}\" showColumnStripes=\"{}\"/>",
                esc(name),
                b(1),
                b(2),
                b(4),
                b(8)
            );
        }
        x.push_str("</table>");
        pkg.text(&format!("xl/tables/table{}.xml", i + 1), x);
    }

    let mut ext_refs = String::new();
    for (i, e) in exts.iter().enumerate() {
        let n = i + 1;
        let rid = format!("rIdExt{n}");
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
    if !meta.is_empty() {
        wb_rel_items.push((
            format!("rId{}", n_items + 3),
            format!("{REL}/sheetMetadata"),
            "/xl/metadata.xml".into(),
        ));
        pkg.text("xl/metadata.xml", meta);
    }

    if let Some((_, _, tp)) = wb_rels.iter().find(|r| r.1.ends_with("/theme")) {
        if let Some(b) = part(zip, tp) {
            let n = wb_rel_items.len();
            wb_rel_items.push((
                format!("rIdTheme{n}"),
                format!("{REL}/theme"),
                format!("/{tp}"),
            ));
            pkg.parts.push((tp.clone(), b));
            copied_types.push((
                tp.clone(),
                "application/vnd.openxmlformats-officedocument.theme+xml".into(),
            ));
        }
    }
    let mut root_items = vec![(
        "rId1".to_string(),
        format!("{REL}/officeDocument"),
        "/xl/workbook.xml".to_string(),
    )];
    for (name, ty, ct) in [
        (
            "docProps/core.xml",
            "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties",
            "application/vnd.openxmlformats-package.core-properties+xml",
        ),
        (
            "docProps/app.xml",
            "http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties",
            "application/vnd.openxmlformats-officedocument.extended-properties+xml",
        ),
        (
            "docProps/custom.xml",
            "http://schemas.openxmlformats.org/officeDocument/2006/relationships/custom-properties",
            "application/vnd.openxmlformats-officedocument.custom-properties+xml",
        ),
    ] {
        if let Some(b) = part(zip, name) {
            pkg.parts.push((name.to_string(), b));
            copied_types.push((name.to_string(), ct.into()));
            root_items.push((
                format!("rId{}", root_items.len() + 1),
                ty.to_string(),
                format!("/{name}"),
            ));
        }
    }
    pkg.text("_rels/.rels", rels_xml(&root_items));
    if !copied_types.is_empty() {
        let mut ct = String::from("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">");
        let mut exts: Vec<String> = Vec::new();
        for (p, t) in &copied_types {
            let e = p.rsplit_once('.').map(|x| x.1.to_ascii_lowercase());
            match e.as_deref() {
                Some("xml") => {
                    let _ = write!(
                        ct,
                        "<Override PartName=\"/{p}\" ContentType=\"{}\"/>",
                        esc(t)
                    );
                }
                Some("rels") | None => {}
                Some(e) => {
                    if !exts.iter().any(|x| x == e) {
                        let _ = write!(
                            ct,
                            "<Default Extension=\"{e}\" ContentType=\"{}\"/>",
                            esc(t)
                        );
                        exts.push(e.to_string());
                    }
                }
            }
        }
        ct.push_str("</Types>");
        pkg.text("[Content_Types].xml", ct);
    }
    pkg.text("xl/sharedStrings.xml", sst);
    pkg.text("xl/styles.xml", styles);
    pkg.text("xl/_rels/workbook.xml.rels", rels_xml(&wb_rel_items));
    let code = wb_code
        .map(|c| format!(" codeName=\"{}\"", esc(&c)))
        .unwrap_or_default();
    pkg.text(
        "xl/workbook.xml",
        format!(
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"{REL}\"><workbookPr{}{code}/>{}<sheets>{sheets_xml}</sheets>{ext_refs}<definedNames>{dn}</definedNames><calcPr{calc}/></workbook>",
            if date1904 { " date1904=\"1\"" } else { "" },
            crate::xls::protect_xml(protect)
        ),
    );
    if let Some(v) = part(zip, "xl/vbaProject.bin") {
        pkg.parts.push(("xl/vbaProject.bin".into(), v));
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

#[cfg(test)]
mod tests {
    use super::*;

    fn ctx<'a>(sheets: &'a [String], books: &'a [Book]) -> Ctx<'a> {
        Ctx {
            sheets,
            xti: &[],
            books,
            ext: &[],
            names: &[],
            tables: &[],
            cur: std::cell::Cell::new(None),
        }
    }

    fn text(rgce: &[u8], base: (u32, u32)) -> Result<String, String> {
        let sheets = [];
        match decode(&ctx(&sheets, &[]), rgce, &[], base)? {
            Dec::Text(t) => Ok(t),
            Dec::Exp { .. } => Err("exp".into()),
        }
    }

    #[test]
    fn rk_numbers() {
        assert_eq!(rk_value((5 << 2) | 2), 5.0);
        assert_eq!(rk_value(((-3i32 as u32) << 2) | 2), -3.0);
        assert_eq!(rk_value((12345 << 2) | 3), 123.45);
    }

    #[test]
    fn sheet_names_are_quoted_when_needed() {
        assert_eq!(quote_sheet("Data"), "Data");
        assert_eq!(quote_sheet("C"), "'C'");
        assert_eq!(quote_sheet("R2C3"), "'R2C3'");
        assert_eq!(quote_sheet("AB12"), "'AB12'");
        assert_eq!(quote_sheet("My Sheet"), "'My Sheet'");
        assert_eq!(quote_sheet("it's"), "'it''s'");
    }

    #[test]
    fn tokens_become_text() {
        let rgce = [
            0x1E, 1, 0, 0x1E, 2, 0, 0x1E, 3, 0, 0x05, 0x03, 0x2C, 1, 0, 0, 0, 0x01, 0xC0, 0x03,
        ];
        assert_eq!(text(&rgce, (4, 4)).unwrap(), "1+2*3+F6");

        let mut area = vec![0x25, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0];
        area.extend_from_slice(&[0x19, 0x10, 0, 0]);
        assert_eq!(text(&area, (0, 0)).unwrap(), "SUM($A$1:$B$2)");
    }

    #[test]
    fn unsupported_tokens_are_reported() {
        assert!(text(&[0x18, 0x1D, 0, 0, 0, 0], (0, 0)).is_err());
        assert!(text(&[0x70], (0, 0)).is_err());
        let sheets = ["A".to_string()];
        let c = Ctx {
            xti: &[(0, 0, 0)],
            ..ctx(&sheets, &[Book::Other])
        };
        let rgce = [0x3A, 0, 0, 0, 0, 0, 0, 0, 0, 0];
        let err = decode(&c, &rgce, &[], (0, 0)).err().unwrap();
        assert!(err.contains("another workbook"));
    }
}
