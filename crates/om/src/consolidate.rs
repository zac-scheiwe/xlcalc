use xlcalc::{CellAddr, Rect, SheetId, Value, XlError};
use xlcalc_variant::Variant;

use crate::app::{Application, BookId};
use crate::conv::Args;
use crate::error::OmError;

#[derive(Clone, Debug, Default)]
pub(crate) struct ConsolidateState {
    pub sources: Vec<(SheetId, Rect)>,
    pub top: bool,
    pub left: bool,
}

#[derive(Clone, Copy, PartialEq)]
enum Agg {
    Sum,
    Count,
    Average,
    Max,
    Min,
    Product,
    StDev,
    StDevP,
    Var,
    VarP,
}

fn agg_of(code: i32) -> Option<Agg> {
    Some(match code {
        -4157 => Agg::Sum,
        -4112 | -4113 => Agg::Count,
        -4106 => Agg::Average,
        -4136 => Agg::Max,
        -4139 => Agg::Min,
        -4149 => Agg::Product,
        -4155 => Agg::StDev,
        -4156 => Agg::StDevP,
        -4164 => Agg::Var,
        -4165 => Agg::VarP,
        _ => return None,
    })
}

fn aggregate(f: Agg, xs: &[f64]) -> Value {
    if xs.is_empty() {
        return Value::Blank;
    }
    let n = xs.len() as f64;
    let mean = xs.iter().sum::<f64>() / n;
    let ss = xs.iter().map(|x| (x - mean) * (x - mean)).sum::<f64>();
    match f {
        Agg::Sum => Value::Number(xs.iter().sum()),
        Agg::Count => Value::Number(n),
        Agg::Average => Value::Number(mean),
        Agg::Max => Value::Number(xs.iter().copied().fold(f64::MIN, f64::max)),
        Agg::Min => Value::Number(xs.iter().copied().fold(f64::MAX, f64::min)),
        Agg::Product => Value::Number(xs.iter().product()),
        Agg::StDev | Agg::Var if xs.len() < 2 => Value::Error(XlError::DIV0),
        Agg::StDev => Value::Number((ss / (n - 1.0)).sqrt()),
        Agg::Var => Value::Number(ss / (n - 1.0)),
        Agg::StDevP => Value::Number((ss / n).sqrt()),
        Agg::VarP => Value::Number(ss / n),
    }
}

#[derive(Default)]
struct Keys {
    labels: Vec<(String, Value)>,
}

impl Keys {
    fn merge(&mut self, src: &[(String, Value)], append: bool) {
        let earlier: Vec<String> = self.labels.iter().map(|x| x.0.to_lowercase()).collect();
        for (i, l) in src.iter().enumerate() {
            if l.0.is_empty() || self.find(&l.0).is_some() {
                continue;
            }
            let at = if append {
                self.labels.len()
            } else {
                src[i + 1..]
                    .iter()
                    .filter(|n| earlier.contains(&n.0.to_lowercase()))
                    .find_map(|n| self.find(&n.0))
                    .unwrap_or(self.labels.len())
            };
            self.labels.insert(at, l.clone());
        }
    }

    fn find(&self, l: &str) -> Option<usize> {
        let l = l.to_lowercase();
        self.labels.iter().position(|x| x.0.to_lowercase() == l)
    }
}

fn label_of(v: &Value) -> (String, Value) {
    (label_key(v), v.clone())
}

fn label_key(v: &Value) -> String {
    match v {
        Value::Text(t) => t.to_string(),
        Value::Number(n) => format!("{n}"),
        Value::Bool(b) => if *b { "TRUE" } else { "FALSE" }.to_string(),
        _ => String::new(),
    }
}

struct Src {
    sheet: SheetId,
    rect: Rect,
    col_labels: Vec<(String, Value)>,
    row_labels: Vec<(String, Value)>,
}

type Group = (Option<Value>, Vec<(usize, u32)>);

struct Merged<'a> {
    sources: &'a [(SheetId, Rect)],
    cols: &'a Keys,
    rows: &'a Keys,
    srcs: &'a [Src],
}

struct Links {
    func: Agg,

    counta: bool,
    top: bool,
    left: bool,
}

fn fn_name(f: Agg, counta: bool) -> &'static str {
    match f {
        Agg::Sum => "SUM",
        Agg::Count if counta => "COUNTA",
        Agg::Count => "COUNT",
        Agg::Average => "AVERAGE",
        Agg::Max => "MAX",
        Agg::Min => "MIN",
        Agg::Product => "PRODUCT",
        Agg::StDev => "STDEV",
        Agg::StDevP => "STDEVP",
        Agg::Var => "VAR",
        Agg::VarP => "VARP",
    }
}

fn quote_sheet(n: &str) -> String {
    if !n.is_empty()
        && n.chars().all(|c| c.is_alphanumeric() || c == '_')
        && !n.starts_with(|c: char| c.is_ascii_digit())
    {
        n.to_string()
    } else {
        format!("'{}'", n.replace('\'', "''"))
    }
}

impl Application<'_> {
    fn consolidate_links(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        o: &Links,
        m: &Merged,
    ) -> Result<Variant, OmError> {
        let Merged {
            sources,
            cols,
            rows,
            srcs,
        } = m;
        let (top, left) = (o.top, o.left);
        let (h, w) = (u32::from(top), u32::from(left));
        let off = if left { 2 } else { 0 };
        let wb = &self.book(book)?.wb;
        let ncols = if top {
            cols.labels.len()
        } else {
            srcs.iter()
                .map(|s| s.rect.cols().saturating_sub(w) as usize)
                .max()
                .unwrap_or(0)
        };

        let mut groups: Vec<Group> = Vec::new();
        if left {
            for l in &rows.labels {
                let key = l.0.to_lowercase();
                let at: Vec<(usize, u32)> = srcs
                    .iter()
                    .enumerate()
                    .flat_map(|(k, s)| {
                        let key = &key;
                        s.row_labels
                            .iter()
                            .enumerate()
                            .filter(move |(_, x)| x.0.to_lowercase() == *key)
                            .map(move |(i, _)| (k, i as u32))
                    })
                    .collect();
                groups.push((Some(l.1.clone()), at));
            }
        } else {
            let n = srcs
                .iter()
                .map(|s| s.rect.rows().saturating_sub(h))
                .max()
                .unwrap_or(0);
            for i in 0..n {
                let at = srcs
                    .iter()
                    .enumerate()
                    .filter(|(_, s)| i < s.rect.rows().saturating_sub(h))
                    .map(|(k, _)| (k, i))
                    .collect();
                groups.push((None, at));
            }
        }
        let mut cells: Vec<(u32, u32, Result<String, Value>)> = Vec::new();
        let mut detail: Vec<u32> = Vec::new();
        let mut row = h;
        if top {
            for (j, l) in cols.labels.iter().enumerate() {
                cells.push((0, off + j as u32, Err(l.1.clone())));
            }
        }
        for (label, at) in &groups {
            let first = row;
            let mut linked = vec![false; ncols];
            for &(k, i) in at {
                let s = &srcs[k];
                let name = wb
                    .sheet(s.sheet)
                    .map_or(String::new(), |x| x.name.to_string());
                let mut line = Vec::new();
                for j in w..s.rect.cols() {
                    let (sr, sc) = (s.rect.r0 + h + i, s.rect.c0 + j);
                    if !matches!(wb.value(CellAddr::new(s.sheet, sr, sc)), Value::Number(_)) {
                        continue;
                    }
                    let cj = if top {
                        match cols.find(&s.col_labels[(j - w) as usize].0) {
                            Some(c) => c,
                            None => continue,
                        }
                    } else {
                        (j - w) as usize
                    };
                    linked[cj] = true;
                    line.push((
                        off + cj as u32,
                        format!(
                            "={}!${}${}",
                            quote_sheet(&name),
                            xlcalc::col_to_letters(sc),
                            sr + 1
                        ),
                    ));
                }
                if !line.is_empty() {
                    for (c, t) in line {
                        cells.push((row, c, Ok(t)));
                    }
                    detail.push(row);
                    row += 1;
                }
            }
            let f = fn_name(o.func, o.counta);
            for (cj, _) in linked.iter().enumerate().filter(|(_, on)| **on) {
                let c = off + cj as u32;
                let col = xlcalc::col_to_letters(rect.c0 + c);
                let range = if row - first <= 1 {
                    format!("{col}{}", rect.r0 + first + 1)
                } else {
                    format!("{col}{}:{col}{}", rect.r0 + first + 1, rect.r0 + row)
                };
                cells.push((row, c, Ok(format!("={f}({range})"))));
            }
            if let Some(l) = label {
                cells.push((row, 0, Err(l.clone())));
            }
            row += 1;
        }
        let bk = self.book_mut(book)?;
        for (i, j, v) in cells {
            let addr = CellAddr::new(sheet, rect.r0 + i, rect.c0 + j);
            match v {
                Ok(t) => {
                    bk.wb
                        .set_formula(addr, &t)
                        .map_err(crate::objects::edit_error)?;
                }
                Err(v) => {
                    bk.wb.set_value(addr, v);
                }
            }
        }
        for r in detail {
            bk.wb
                .set_subtotal_levels(sheet, rect.r0 + r, rect.r0 + r, Some(1));
            bk.wb
                .set_rows_hidden(sheet, rect.r0 + r, 1, true)
                .map_err(crate::objects::edit_error)?;
        }
        bk.consolidate.insert(
            sheet,
            ConsolidateState {
                sources: sources.to_vec(),
                top,
                left,
            },
        );
        Ok(Variant::Bool(true))
    }
}

impl Application<'_> {
    fn consolidate_sources(
        &self,
        book: BookId,
        sheet: SheetId,
        v: &Variant,
    ) -> Result<Vec<(SheetId, Rect)>, OmError> {
        let texts: Vec<String> = match v {
            Variant::Bstr(s) => vec![s.to_string()],
            Variant::Array(sa) => sa
                .data
                .iter()
                .map(|e| match e {
                    Variant::Bstr(s) => Ok(s.to_string()),
                    _ => Err(OmError::type_mismatch()),
                })
                .collect::<Result<_, _>>()?,
            _ => return Err(OmError::type_mismatch()),
        };
        let wb = &self.book(book)?.wb;
        let mut out: Vec<(SheetId, Rect)> = Vec::new();
        for t in &texts {
            match crate::pivot::parse_r1c1(wb, sheet, t) {
                Some(src) => {
                    if out.contains(&src) {
                        return Err(OmError::app_defined_with(
                            "Cannot add duplicate source reference.",
                        ));
                    }
                    out.push(src);
                }

                None if t.split_once('!').is_some_and(|(n, _)| {
                    let n = n.trim_matches('\'');
                    !n.is_empty() && wb.sheet_by_name(n).is_none()
                }) => {}
                None => return Err(OmError::app_defined_with("Reference isn't valid.")),
            }
        }
        Ok(out)
    }

    pub(crate) fn consolidate(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let func = match a.i32(1)? {
            None => Agg::Sum,
            Some(c) => agg_of(c).ok_or_else(|| {
                OmError::app_defined_with("Consolidate method of Range class failed")
            })?,
        };
        let old = self
            .book(book)?
            .consolidate
            .get(&sheet)
            .cloned()
            .unwrap_or_default();
        let sources = match a.get(0) {
            Some(v) => self.consolidate_sources(book, sheet, v)?,
            None if old.sources.is_empty() => {
                return Err(OmError::app_defined_with(
                    "No source references specified for consolidation.",
                ))
            }
            None => old.sources.clone(),
        };
        let top = a.bool(2)?.unwrap_or(old.top);
        let left = a.bool(3)?.unwrap_or(old.left);
        let mut create_links = false;
        if a.bool(4)?.unwrap_or(false) {
            if sources.iter().any(|&(s, _)| s == sheet) {
                return Err(OmError::app_defined_with(
                    "Cannot create links to consolidation sheet.",
                ));
            }
            create_links = true;
        }
        let (h, w) = (u32::from(top), u32::from(left));
        let wb = &self.book(book)?.wb;
        let mut cols = Keys::default();
        let mut rows = Keys::default();
        let mut srcs = Vec::new();
        for &(s, r) in &sources {
            let val = |i: u32, j: u32| wb.value(CellAddr::new(s, r.r0 + i, r.c0 + j));
            let col_labels: Vec<(String, Value)> = if top {
                (w..r.cols()).map(|j| label_of(&val(0, j))).collect()
            } else {
                Vec::new()
            };
            let row_labels: Vec<(String, Value)> = if left {
                (h..r.rows()).map(|i| label_of(&val(i, 0))).collect()
            } else {
                Vec::new()
            };
            cols.merge(&col_labels, !rect.is_cell());
            rows.merge(&row_labels, !rect.is_cell());
            srcs.push(Src {
                sheet: s,
                rect: r,
                col_labels,
                row_labels,
            });
        }
        if create_links {
            let counta = a.i32(1)? != Some(-4113);
            let l = Links {
                func,
                counta,
                top,
                left,
            };
            let m = Merged {
                sources: &sources,
                cols: &cols,
                rows: &rows,
                srcs: &srcs,
            };
            return self.consolidate_links(book, sheet, rect, &l, &m);
        }
        let mut groups: Vec<Vec<Vec<f64>>> = Vec::new();
        let (mut nr, mut nc) = (rows.labels.len(), cols.labels.len());
        for src in &srcs {
            let r = src.rect;
            if !top {
                nc = nc.max(r.cols().saturating_sub(w) as usize);
            }
            if !left {
                nr = nr.max(r.rows().saturating_sub(h) as usize);
            }
            for i in h..r.rows() {
                for j in w..r.cols() {
                    let ri = if left {
                        rows.find(&src.row_labels[(i - h) as usize].0)
                    } else {
                        Some((i - h) as usize)
                    };
                    let ci = if top {
                        cols.find(&src.col_labels[(j - w) as usize].0)
                    } else {
                        Some((j - w) as usize)
                    };
                    let (Some(ri), Some(ci)) = (ri, ci) else {
                        continue;
                    };
                    if let Value::Number(x) = wb.value(CellAddr::new(src.sheet, r.r0 + i, r.c0 + j))
                    {
                        if groups.len() <= ri {
                            groups.resize(ri + 1, Vec::new());
                        }
                        if groups[ri].len() <= ci {
                            groups[ri].resize(ci + 1, Vec::new());
                        }
                        groups[ri][ci].push(x);
                    }
                }
            }
        }
        let mut out: Vec<(u32, u32, Value)> = Vec::new();
        if top {
            for (j, l) in cols.labels.iter().enumerate() {
                out.push((0, w + j as u32, l.1.clone()));
            }
        }
        if left {
            for (i, l) in rows.labels.iter().enumerate() {
                out.push((h + i as u32, 0, l.1.clone()));
            }
        }
        for i in 0..nr {
            for j in 0..nc {
                let xs = groups
                    .get(i)
                    .and_then(|g| g.get(j))
                    .map_or(&[][..], Vec::as_slice);
                out.push((h + i as u32, w + j as u32, aggregate(func, xs)));
            }
        }

        let (lim_r, lim_c) = if rect.is_cell() {
            (u32::MAX, u32::MAX)
        } else {
            (rect.rows(), rect.cols())
        };
        let bk = self.book_mut(book)?;
        for (i, j, v) in out {
            if i < lim_r && j < lim_c {
                bk.wb
                    .set_value(CellAddr::new(sheet, rect.r0 + i, rect.c0 + j), v);
            }
        }
        bk.consolidate
            .insert(sheet, ConsolidateState { sources, top, left });
        Ok(Variant::Bool(true))
    }
}
