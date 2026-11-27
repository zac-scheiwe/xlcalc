use crate::sheetprot::ProtObject;
use xlcalc::cfv::{CfKind, CfRule, Dv, Dxf, IconSetSpec, ScaleCrit};
use xlcalc::{CellAddr, Rect, SheetId};
use xlcalc_variant::{HResult, Variant};

use crate::app::{unsupported, Application, BookId, Object, RangeRef};
use crate::conv::Args;
use crate::error::OmError;
use crate::style::StyleOwner;
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum CfIface {
    Plain,
    Top10,
    Unique,
    Above,
    Scale,
    DataBar,
    IconSet,
}

impl CfIface {
    pub(crate) fn interface_name(self) -> &'static str {
        match self {
            CfIface::Plain => "FormatCondition",
            CfIface::Top10 => "Top10",
            CfIface::Unique => "UniqueValues",
            CfIface::Above => "AboveAverage",
            CfIface::Scale => "ColorScale",
            CfIface::DataBar => "Databar",
            CfIface::IconSet => "IconSetCondition",
        }
    }

    fn of(kind: &CfKind) -> CfIface {
        match kind {
            CfKind::Top10 { .. } => CfIface::Top10,
            CfKind::Unique { .. } => CfIface::Unique,
            CfKind::AboveAverage { .. } => CfIface::Above,
            CfKind::ColorScale(_) => CfIface::Scale,
            CfKind::DataBar(_) => CfIface::DataBar,
            CfKind::IconSet(_) => CfIface::IconSet,
            _ => CfIface::Plain,
        }
    }
}

fn swap_rb(c: u32) -> u32 {
    ((c & 255) << 16) | (c & 0xFF00) | ((c >> 16) & 255)
}

impl Application<'_> {
    fn scale_of(&self, book: BookId, sheet: SheetId, id: u32) -> Result<Vec<ScaleCrit>, OmError> {
        let wb = &self.book(book)?.wb;
        let gone = OmError::bad_index;
        let sh = wb.cfv_sheet(sheet).ok_or_else(gone)?;
        let i = sh.index_of(id).ok_or_else(gone)?;
        match &sh.rules[i].kind {
            CfKind::ColorScale(c) => Ok(c.clone()),
            _ => Err(gone()),
        }
    }

    fn scale_edit(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        k: usize,
        f: impl FnOnce(&mut ScaleCrit),
    ) -> Result<Variant, OmError> {
        self.cf_edit(book, sheet, id, |r| {
            if let CfKind::ColorScale(c) = &mut r.kind {
                f(&mut c[k]);
            }
        })
    }

    pub(crate) fn scale_criteria_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let crits = self.scale_of(book, sheet, id)?;
        match (m.name, m.kind) {
            ("Count", Get) => Ok(i4(crits.len() as i32)),
            ("Item" | "_Default", _) => {
                let i = a.i32(0)?.ok_or_else(OmError::app_defined)?;
                if i < 1 || i as usize > crits.len() {
                    return Err(OmError::bad_index());
                }
                Ok(self.alloc(Object::ScaleCriterion(book, sheet, id, i as usize - 1)))
            }
            ("_NewEnum", Get) => {
                let items = (0..crits.len())
                    .map(|k| self.alloc(Object::ScaleCriterion(book, sheet, id, k)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            _ => Err(unsupported("ColorScaleCriteria", m)),
        }
    }

    pub(crate) fn scale_criterion_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        k: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let crits = self.scale_of(book, sheet, id)?;
        let c = crits.get(k).ok_or_else(OmError::bad_index)?;
        match (m.name, m.kind) {
            ("Index", Get) => Ok(i4(k as i32 + 1)),
            ("Type", Get) => Ok(i4(c.ty)),

            ("Type", Put) => {
                let t = Application::put_value(a).i32(0)?.ok_or_else(invalid_arg)?;
                let last = crits.len() - 1;
                let bad = !(0..=5).contains(&t) || (t == 2 && k != last) || (t == 1 && k != 0);
                if bad {
                    return Err(invalid_arg());
                }
                let (first, end) = (k == 0, k == last);
                self.scale_edit(book, sheet, id, k, |c| {
                    c.ty = t;
                    if t == 4 {
                        return;
                    }

                    c.value = match t {
                        3 if first => 0.0,
                        3 if end => 100.0,
                        5 if first => 10.0,
                        5 if end => 90.0,
                        3 | 5 => 50.0,
                        _ => 0.0,
                    };
                })
            }
            ("Value", Get) => match &c.formula {
                Some(f) => Ok(Variant::Bstr(f.clone())),
                None => Ok(Variant::R8(c.value)),
            },

            ("Value", Put) => {
                if matches!(c.ty, 1 | 2) {
                    return Err(OmError::app_defined());
                }
                let anchor = self.cf_rule_anchor(book, sheet, id);
                match self.cf_threshold_value(book, anchor, a)? {
                    CfValue::Num(n) => self.scale_edit(book, sheet, id, k, |c| c.value = n),
                    CfValue::Formula(t) => {
                        self.scale_edit(book, sheet, id, k, |c| c.formula = Some(t))
                    }
                }
            }
            ("FormatColor", Get) => Ok(self.alloc(Object::ScaleColor(book, sheet, id, k))),
            _ => Err(unsupported("ColorScaleCriterion", m)),
        }
    }

    pub(crate) fn scale_color_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        k: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let crits = self.scale_of(book, sheet, id)?;
        let c = crits.get(k).ok_or_else(OmError::bad_index)?;
        match (m.name, m.kind) {
            ("Color", Get) => Ok(i4(swap_rb(c.color) as i32)),
            ("Color", Put) => {
                let n = match Application::put_value(a).get(0) {
                    Some(Variant::Bool(_)) => return Err(OmError::app_defined()),
                    Some(Variant::R8(x)) => x.round_ties_even() as i32,
                    _ => Application::put_value(a)
                        .i32(0)?
                        .ok_or_else(OmError::app_defined)?,
                };
                self.scale_edit(book, sheet, id, k, |c| {
                    c.color = swap_rb(n as u32 & 0xFFFFFF)
                })
            }
            _ => Err(unsupported("FormatColor", m)),
        }
    }
}

pub(crate) enum CfValue {
    Num(f64),
    Formula(String),
}

impl Application<'_> {
    pub(crate) fn cf_threshold_value(
        &self,
        book: BookId,
        anchor: CellAddr,
        a: &Args,
    ) -> Result<CfValue, OmError> {
        let refused = OmError::app_defined;
        let v = Application::put_value(a);
        match v.get(0) {
            None | Some(Variant::Empty) => Err(refused()),
            Some(Variant::Bool(b)) => {
                Ok(CfValue::Formula(if *b { "=TRUE" } else { "=FALSE" }.into()))
            }
            Some(Variant::Bstr(t)) => {
                let bare = t.trim();
                if t.is_empty() {
                    Err(refused())
                } else if t.starts_with('=') && t.len() > 1 {
                    let wb = &self.book(book)?.wb;
                    wb.cfv_threshold_formula(t, anchor)
                        .map(CfValue::Formula)
                        .ok_or_else(refused)
                } else if let Ok(n) = bare.parse::<f64>() {
                    Ok(CfValue::Num(n))
                } else if bare.eq_ignore_ascii_case("TRUE") || bare.eq_ignore_ascii_case("FALSE") {
                    Ok(CfValue::Formula(format!("={}", bare.to_ascii_uppercase())))
                } else {
                    Ok(CfValue::Formula(format!("=\"{}\"", t.replace('"', "\"\""))))
                }
            }
            Some(_) => Ok(CfValue::Num(v.f64(0)?.ok_or_else(refused)?)),
        }
    }
}

fn invalid_arg() -> OmError {
    OmError::exception(0x8007_0057_u32 as i32, None)
}

fn i4(n: i32) -> Variant {
    Variant::I4(n)
}

fn bstr(s: &str) -> Variant {
    if s.is_empty() {
        Variant::BstrNull
    } else {
        Variant::Bstr(s.to_string())
    }
}

fn scale_kind(n: i32) -> Option<CfKind> {
    let crit = |ty, color| ScaleCrit {
        ty,
        value: 0.0,
        formula: None,
        color,
    };
    let crits = match n {
        2 => vec![crit(1, 0xFF7128), crit(2, 0xFFEF9C)],
        3 => vec![
            crit(1, 0xF8696B),
            ScaleCrit {
                value: 50.0,
                ..crit(5, 0xFFEB84)
            },
            crit(2, 0x63BE7B),
        ],
        _ => return None,
    };
    Some(CfKind::ColorScale(crits))
}

const PARAM_NOT_OPTIONAL: i32 = 0x8002_000F_u32 as i32;
const E_UNEXPECTED: i32 = 0x8000_FFFF_u32 as i32;

fn formula_arg(a: &Args, i: usize) -> Result<Option<String>, OmError> {
    Ok(match a.get(i) {
        None => None,
        Some(Variant::Bstr(s)) => Some(s.clone()),
        Some(Variant::Dispatch(_)) => return Err(OmError::app_defined()),
        Some(_) => a.string(i)?,
    })
}

fn cf_formula_arg(a: &Args, i: usize) -> Result<Option<String>, OmError> {
    let f = formula_arg(a, i)?;
    Ok(f.map(|t| {
        let bare = t.trim();
        let literal = bare.parse::<f64>().is_ok()
            || bare.eq_ignore_ascii_case("TRUE")
            || bare.eq_ignore_ascii_case("FALSE")
            || (bare.starts_with('#') && bare.len() > 1);
        if t.starts_with('=')
            || bare.is_empty()
            || literal
            || !matches!(a.get(i), Some(Variant::Bstr(_)))
        {
            t
        } else {
            format!("=\"{}\"", t.replace('"', "\"\""))
        }
    }))
}

impl Application<'_> {
    fn cf_handle(&mut self, book: BookId, sheet: SheetId, id: u32, scope: &[Rect]) -> Variant {
        let iface = self
            .book(book)
            .ok()
            .and_then(|b| b.wb.cfv_sheet(sheet))
            .and_then(|s| s.index_of(id).map(|i| CfIface::of(&s.rules[i].kind)))
            .unwrap_or(CfIface::Plain);
        let scope = scope.to_vec();
        self.alloc(Object::FormatCondition(book, sheet, id, iface, scope))
    }

    pub(crate) fn cf_rule_anchor(&self, book: BookId, sheet: SheetId, id: u32) -> CellAddr {
        let (r, c) = self
            .book(book)
            .ok()
            .and_then(|b| b.wb.cfv_sheet(sheet))
            .map(|s| s.anchor(id))
            .unwrap_or((0, 0));
        CellAddr::new(sheet, r, c)
    }

    fn cf_target(&self, r: &RangeRef) -> Result<(SheetId, Rect), OmError> {
        self.resolve(r)
    }

    fn cf_ids(&self, r: &RangeRef, rects: &[Rect]) -> Result<Vec<u32>, OmError> {
        let (sheet, _) = self.cf_target(r)?;
        let wb = &self.book(r.book)?.wb;
        Ok(wb
            .cfv_sheet(sheet)
            .map_or_else(Vec::new, |s| s.listing(rects)))
    }

    pub(crate) fn format_conditions_member(
        &mut self,
        r: &RangeRef,
        more: &[RangeRef],
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        if self.fc_poisoned {
            return Err(OmError::app_defined());
        }
        let (sheet, rect) = self.cf_target(r)?;

        let mut rects = vec![rect];
        for o in more {
            if let Ok((_, q)) = self.cf_target(o) {
                rects.push(q);
            }
        }
        match (m.name, m.kind) {
            ("Count", Get) => Ok(i4(self.cf_ids(r, &rects)?.len() as i32)),
            ("Item" | "_Default", _) => {
                let ids = self.cf_ids(r, &rects)?;
                let i = a
                    .i32(0)
                    .map_err(|_| OmError::bad_index())?
                    .ok_or_else(OmError::app_defined)?;
                if i < 1 || i as usize > ids.len() {
                    return Err(OmError::bad_index());
                }
                Ok(self.cf_handle(r.book, sheet, ids[i as usize - 1], &rects))
            }
            ("_NewEnum", Get) => {
                let items = self
                    .cf_ids(r, &rects)?
                    .into_iter()
                    .map(|id| self.cf_handle(r.book, sheet, id, &rects))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            ("Delete", Func) => {
                self.object_guard(r.book, sheet, ProtObject::FormatCondition, None)?;
                let wb = &mut self.book_mut(r.book)?.wb;
                wb.cfv_clear_range(sheet, &rects);
                Ok(Variant::Empty)
            }
            ("Add", Func) => {
                self.object_guard(r.book, sheet, ProtObject::FormatCondition, None)?;
                self.cf_add(r, sheet, &rects, a)
            }
            ("AddTop10", Func) => self.cf_push(
                r.book,
                sheet,
                &rects,
                CfKind::Top10 {
                    bottom: false,
                    percent: false,
                    rank: 10,
                },
                None,
                None,
            ),
            ("AddColorScale", Func) => {
                let n = a.i32(0)?.ok_or_else(OmError::app_defined)?;
                let kind = scale_kind(n).ok_or_else(OmError::bad_index)?;
                self.cf_push(r.book, sheet, &rects, kind, None, None)
            }
            ("AddDatabar", Func) => self.cf_push(
                r.book,
                sheet,
                &rects,
                CfKind::DataBar(Box::default()),
                None,
                None,
            ),
            ("AddIconSetCondition", Func) => self.cf_push(
                r.book,
                sheet,
                &rects,
                CfKind::IconSet(IconSetSpec::new(4, false).ok_or_else(OmError::app_defined)?),
                None,
                None,
            ),
            ("AddUniqueValues", Func) => self.cf_push(
                r.book,
                sheet,
                &rects,
                CfKind::Unique { duplicate: false },
                None,
                None,
            ),
            ("AddAboveAverage", Func) => self.cf_push(
                r.book,
                sheet,
                &rects,
                CfKind::AboveAverage {
                    below: false,
                    equal: false,
                    std: false,
                    num_std: 1,
                },
                None,
                None,
            ),
            _ => Err(unsupported("FormatConditions", m)),
        }
    }

    fn cf_add(
        &mut self,
        r: &RangeRef,
        sheet: SheetId,
        rects: &[Rect],
        a: &Args,
    ) -> Result<Variant, OmError> {
        let needed = || OmError::exception(PARAM_NOT_OPTIONAL, None);
        let invalid = || OmError::exception(HResult::E_INVALIDARG.0, None);
        let ty = a.i32(0)?.ok_or_else(needed)?;
        let given = |i: usize| a.get(i).is_some_and(|v| !matches!(v, Variant::Empty));
        let op = if given(1) {
            Some(a.i32(1).map_err(|_| invalid())?.ok_or_else(invalid)?)
        } else {
            None
        };
        let f1 = cf_formula_arg(a, 2)?.filter(|_| given(2));
        let f2 = cf_formula_arg(a, 3)?.filter(|_| given(3));

        if (ty == 9 && (!given(4) || a.get(5).is_none())) || (ty == 11 && a.get(6).is_none()) {
            return Err(needed());
        }
        if given(7) && number_arg(a, 7).is_some() {
            return Err(invalid());
        }
        let kind = match ty {
            1 => {
                let op = op.ok_or_else(needed)?;
                if !(1..=8).contains(&op) {
                    return Err(invalid());
                }
                if f1.is_none() || (matches!(op, 1 | 2) && f2.is_none()) {
                    return Err(needed());
                }
                CfKind::CellValue(op)
            }
            2 => {
                if matches!(a.get(2), Some(Variant::Empty)) {
                    return Err(invalid());
                }
                let f = f1.as_ref().ok_or_else(needed)?;
                if f.is_empty() {
                    return Err(invalid());
                }
                CfKind::Expression
            }
            9 => {
                let text = a.string(4)?.filter(|_| given(4)).ok_or_else(needed)?;

                let op = strict_int(a, 5)?.ok_or_else(needed)?;
                if text.encode_utf16().count() >= 256 {
                    return Err(invalid());
                }
                if !(0..=3).contains(&op) {
                    return Err(OmError::exception(E_UNEXPECTED, None));
                }
                CfKind::Text { op, text }
            }

            10 | 14 | 15 => CfKind::Blanks,
            11 => {
                let period = strict_int(a, 6)?.ok_or_else(needed)?;

                if !(0..=9).contains(&period) {
                    return Err(invalid());
                }
                CfKind::TimePeriod(period)
            }
            13 => CfKind::NoBlanks,
            16 => CfKind::Errors,
            17 => CfKind::NoErrors,

            3 => scale_kind(2).ok_or_else(OmError::app_defined)?,
            4 => CfKind::DataBar(Box::default()),
            5 => CfKind::Top10 {
                bottom: false,
                percent: false,
                rank: 10,
            },
            6 => CfKind::IconSet(IconSetSpec::new(4, false).ok_or_else(OmError::app_defined)?),
            8 => CfKind::Unique { duplicate: false },
            12 => CfKind::AboveAverage {
                below: false,
                equal: false,
                std: false,
                num_std: 1,
            },
            _ => return Err(invalid()),
        };
        let (f1, f2) = if matches!(ty, 1 | 2) {
            let anchor = CellAddr::new(
                sheet,
                rects.iter().map(|q| q.r0).min().unwrap_or(0),
                rects.iter().map(|q| q.c0).min().unwrap_or(0),
            );
            let wb = &self.book(r.book)?.wb;
            let f2 = f2.filter(|_| matches!(op, Some(1 | 2)));
            for f in [&f1, &f2].into_iter().flatten() {
                if wb.cfv_bare_area(f, anchor) {
                    return Err(invalid());
                }
            }
            (f1, f2)
        } else {
            (None, None)
        };
        self.cf_push(r.book, sheet, rects, kind, f1, f2)
    }

    fn cf_push(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rects: &[Rect],
        kind: CfKind,
        f1: Option<String>,
        f2: Option<String>,
    ) -> Result<Variant, OmError> {
        let anchor = CellAddr::new(
            sheet,
            rects.iter().map(|q| q.r0).min().unwrap_or(0),
            rects.iter().map(|q| q.c0).min().unwrap_or(0),
        );
        let wb = &mut self.book_mut(book)?.wb;
        let conv = |wb: &xlcalc::Workbook, f: Option<String>| -> Result<Option<String>, OmError> {
            match f {
                None => Ok(None),
                Some(t) => wb
                    .cfv_to_relative(&t, anchor)
                    .map(Some)
                    .map_err(|_| OmError::app_defined()),
            }
        };
        let f1 = conv(wb, f1)?;
        let f2 = conv(wb, f2)?;
        let rule = CfRule {
            id: 0,
            prio: 0,
            stop: matches!(
                kind,
                CfKind::CellValue(_)
                    | CfKind::Expression
                    | CfKind::Text { .. }
                    | CfKind::Blanks
                    | CfKind::NoBlanks
                    | CfKind::Errors
                    | CfKind::NoErrors
                    | CfKind::TimePeriod(_)
                    | CfKind::AboveAverage { .. }
            ),
            kind,
            f1,
            f2,
            dxf: Dxf::default(),
        };
        let sh = wb.cfv_sheet_mut(sheet);
        let id = sh.add_on_range(rule, rects);
        Ok(self.cf_handle(book, sheet, id, rects))
    }

    pub(crate) fn format_condition_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        scope: &[Rect],
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let gone = || OmError::exception(crate::error::SCODE_BAD_INDEX, None);
        let (pos, rule, areas, at) = {
            let wb = &self.book(book)?.wb;
            let sh = wb.cfv_sheet(sheet).ok_or_else(gone)?;
            let pos = sh.index_of(id).ok_or_else(gone)?;
            (
                pos,
                sh.rules[pos].clone(),
                sh.areas(id).to_vec(),
                sh.anchor(id),
            )
        };
        let anchor = CellAddr::new(sheet, at.0, at.1);
        let owner = StyleOwner::Cond(book, sheet, id, scope.to_vec());
        if let Some(r) = self.cf_deco_member(book, sheet, id, &rule, m, a) {
            return r;
        }
        match (m.name, m.kind) {
            ("Type", Get) => Ok(i4(rule.kind.type_code())),
            ("Priority", Get) => Ok(i4(rule.prio)),
            ("Priority", Put) => {
                let p = Application::put_value(a)
                    .i32(0)?
                    .ok_or_else(OmError::app_defined)?;
                if p < 1 {
                    return Err(invalid_arg());
                }
                self.object_guard(book, sheet, ProtObject::FormatCondition, None)?;
                let wb = &mut self.book_mut(book)?.wb;
                let sh = wb.cfv_sheet_mut(sheet);
                sh.set_priority(pos, p);
                Ok(Variant::Empty)
            }
            ("SetFirstPriority", Func) | ("SetLastPriority", Func) => {
                self.object_guard(book, sheet, ProtObject::FormatCondition, None)?;
                let wb = &mut self.book_mut(book)?.wb;
                let sh = wb.cfv_sheet_mut(sheet);

                let p = match m.name {
                    "SetFirstPriority" => 1,
                    _ => sh.next_priority(),
                };
                if p < 1 {
                    return Err(invalid_arg());
                }
                sh.set_priority(pos, p);
                Ok(Variant::Empty)
            }
            ("ModifyAppliesToRange", Func) => {
                if let Some(v) = a.get(0) {
                    let is_range = matches!(v, Variant::Dispatch(Some(id))
                        if matches!(self.arena.get(*id), Some(Object::Range(_) | Object::Multi(_))));
                    if !is_range {
                        return Err(OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0));
                    }
                }
                let (_, _, rects) = self.range_areas(a.get(0))?;

                if rects.is_empty() {
                    return Err(OmError::app_defined());
                }

                let old = anchor;
                let new = CellAddr::new(
                    sheet,
                    rects.iter().map(|r| r.r0).min().unwrap_or(0),
                    rects.iter().map(|r| r.c0).min().unwrap_or(0),
                );
                let wb = &mut self.book_mut(book)?.wb;
                let moved = [&rule.f1, &rule.f2].map(|f| {
                    f.as_ref()
                        .map(|t| wb.cfv_retarget(t, old, new).unwrap_or_else(|| t.clone()))
                });
                let done = self.cf_edit(book, sheet, id, |r| {
                    let [f1, f2] = moved;
                    r.f1 = f1;
                    r.f2 = f2;
                })?;
                let wb = &mut self.book_mut(book)?.wb;
                wb.cfv_sheet_mut(sheet).set_applies_to(id, &rects);
                Ok(done)
            }
            ("ReverseOrder" | "ShowIconOnly", Get) => Ok(Variant::Bool(false)),
            ("ShowValue", Get) => Ok(Variant::Bool(true)),
            ("StopIfTrue", Get) => Ok(Variant::Bool(rule.stop)),
            ("StopIfTrue", Put) => {
                if Application::put_value(a)
                    .get(0)
                    .is_none_or(crate::conv::is_missing)
                {
                    return Err(OmError::hresult(crate::error::DISP_E_PARAMNOTOPTIONAL));
                }
                let b = Application::put_value(a).bool(0)?.unwrap_or(false);
                self.object_guard(book, sheet, ProtObject::FormatCondition, None)?;
                self.cf_edit(book, sheet, id, |r| r.stop = b)
            }
            ("Formula1" | "Formula2", Get) => {
                let f = if m.name == "Formula1" {
                    &rule.f1
                } else {
                    &rule.f2
                };
                let wb = &self.book(book)?.wb;
                match f {
                    Some(f) => Ok(Variant::Bstr(
                        wb.cfv_from_relative(f, anchor).unwrap_or_default(),
                    )),
                    None if matches!(&rule.kind, CfKind::Text { text, .. } if text.is_empty()) => {
                        Err(OmError::app_defined())
                    }
                    None if m.name == "Formula1" && !matches!(rule.kind, CfKind::CellValue(_)) => {
                        Ok(Variant::Bstr(wb.cf_condition_text(sheet, &rule)))
                    }
                    None => Err(OmError::app_defined()),
                }
            }
            ("Rank", Get) | ("Percent", Get) | ("TopBottom", Get) => match rule.kind {
                CfKind::Top10 {
                    bottom,
                    percent,
                    rank,
                } => Ok(match m.name {
                    "Rank" => i4(rank as i32),
                    "Percent" => Variant::Bool(percent),
                    _ => i4(if bottom { 0 } else { 1 }),
                }),
                _ => Err(OmError::app_defined()),
            },
            ("Rank" | "Percent" | "TopBottom", Put) => {
                let v = Application::put_value(a);
                let CfKind::Top10 {
                    mut bottom,
                    mut percent,
                    mut rank,
                } = rule.kind
                else {
                    return Err(OmError::app_defined());
                };
                match m.name {
                    "Rank" => {
                        let n = v.i32(0)?.ok_or_else(OmError::app_defined)?;
                        if !(1..=1000).contains(&n) || (percent && n > 100) {
                            return Err(invalid_arg());
                        }
                        rank = n as u32;
                    }
                    "Percent" => {
                        percent = v.bool(0)?.unwrap_or(false);
                        if percent {
                            rank = rank.min(100);
                        }
                    }
                    _ => {
                        let n = v
                            .i32(0)
                            .map_err(|_| OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0))?
                            .unwrap_or(1);
                        if !(0..=1).contains(&n) {
                            return Err(invalid_arg());
                        }
                        bottom = n == 0;
                    }
                }
                self.cf_edit(book, sheet, id, |r| {
                    r.kind = CfKind::Top10 {
                        bottom,
                        percent,
                        rank,
                    }
                })
            }
            ("DupeUnique", Get) => match rule.kind {
                CfKind::Unique { duplicate } => Ok(i4(i32::from(duplicate))),
                _ => Err(OmError::app_defined()),
            },
            ("DupeUnique", Put) => {
                if !matches!(rule.kind, CfKind::Unique { .. }) {
                    return Err(OmError::app_defined());
                }
                let n = Application::put_value(a).i32(0)?.unwrap_or(0);
                if !(0..=1).contains(&n) {
                    return Err(invalid_arg());
                }
                let d = n == 1;
                self.cf_edit(book, sheet, id, |r| {
                    r.kind = CfKind::Unique { duplicate: d }
                })
            }
            ("AboveBelow", Get) => match rule.kind {
                CfKind::AboveAverage {
                    below, equal, std, ..
                } => Ok(i4(match (std, below, equal) {
                    (true, false, _) => 4,
                    (true, true, _) => 5,
                    (false, false, false) => 0,
                    (false, true, false) => 1,
                    (false, false, true) => 2,
                    (false, true, true) => 3,
                })),
                _ => Err(OmError::app_defined()),
            },
            ("AboveBelow", Put) => {
                let n = Application::put_value(a).i32(0)?.unwrap_or(0);
                let CfKind::AboveAverage { num_std, .. } = rule.kind else {
                    return Err(OmError::app_defined());
                };
                if !(0..=5).contains(&n) {
                    return Err(invalid_arg());
                }
                self.cf_edit(book, sheet, id, |r| {
                    r.kind = CfKind::AboveAverage {
                        below: matches!(n, 1 | 3 | 5),
                        equal: matches!(n, 2 | 3),
                        std: n >= 4,
                        num_std,
                    }
                })
            }
            ("NumStdDev", Get) => match rule.kind {
                CfKind::AboveAverage { num_std, .. } => Ok(i4(num_std as i32)),
                _ => Err(OmError::app_defined()),
            },
            ("NumStdDev", Put) => {
                let n = Application::put_value(a).i32(0)?.unwrap_or(0);
                if !(1..=3).contains(&n) {
                    return Err(invalid_arg());
                }
                self.cf_edit(book, sheet, id, |r| {
                    if let CfKind::AboveAverage { num_std, .. } = &mut r.kind {
                        *num_std = n as u32;
                    }
                })
            }
            ("CalcFor", Get) => Ok(i4(1)),
            ("ColorScaleCriteria", Get) => match rule.kind {
                CfKind::ColorScale(_) => Ok(self.alloc(Object::ScaleCriteria(book, sheet, id))),
                _ => Err(OmError::app_defined()),
            },
            ("Operator", Get) => match rule.kind {
                CfKind::CellValue(op) => Ok(i4(op)),
                _ => Err(OmError::app_defined()),
            },
            ("Text", Get) => match &rule.kind {
                CfKind::Text { text, .. } => Ok(Variant::Bstr(text.clone())),
                _ => Err(OmError::app_defined()),
            },
            ("TextOperator", Get) => match rule.kind {
                CfKind::Text { op, .. } => Ok(i4(op)),
                _ => Err(OmError::app_defined()),
            },
            ("TextOperator", Put) if !matches!(&rule.kind, CfKind::Text { text, .. } if !text.is_empty()) =>
            {
                self.coerce_wb_put(m, a)?;
                let v = self.put_i32(a)?;
                if !(0..=3).contains(&v) {
                    return Err(invalid_arg());
                }
                self.cf_become_text(book, sheet, id, v)?;
                Err(OmError::app_defined())
            }
            ("DateOperator", Get) => match rule.kind {
                CfKind::TimePeriod(p) => Ok(i4(p)),
                _ => Err(invalid_arg()),
            },
            ("DateOperator", Put) if !matches!(rule.kind, CfKind::TimePeriod(_)) => {
                self.coerce_wb_put(m, a)?;
                if (0..=9).contains(&self.put_i32(a)?) {
                    Ok(Variant::Empty)
                } else {
                    Err(invalid_arg())
                }
            }
            ("Text", Put) if !matches!(rule.kind, CfKind::Text { .. }) => {
                Err(OmError::app_defined())
            }
            ("PTCondition", Get) => Ok(Variant::Bool(false)),
            ("ScopeType", Get) => Err(invalid_arg()),
            ("ScopeType", Put) => {
                self.coerce_wb_put(m, a)?;
                if (0..=2).contains(&self.put_i32(a)?) {
                    Err(OmError::app_defined())
                } else {
                    Err(invalid_arg())
                }
            }
            ("AppliesTo", Get) => self.new_multi(book, sheet, &areas, crate::app::Mode::Cells),
            ("NumberFormat", Get) => match self.book(book)?.wb.cfv_number_format(&rule.dxf) {
                xlcalc::CfNumberFormat::Code(f) => Ok(bstr(&f)),
                xlcalc::CfNumberFormat::Unset => Ok(Variant::Empty),
                xlcalc::CfNumberFormat::Gone => Err(OmError::app_defined()),
            },
            ("NumberFormat", Put) => {
                let f = match Application::put_value(a).get(0) {
                    None | Some(Variant::Empty) => ";;".to_string(),
                    Some(Variant::Bool(t)) => if *t { "-1" } else { "0" }.to_string(),
                    _ => Application::put_value(a).string(0)?.unwrap_or_default(),
                };

                let f = if f.is_empty() { ";;".to_string() } else { f };
                self.object_guard(book, sheet, ProtObject::FormatCondition, None)?;

                self.cf_edit(book, sheet, id, |_| ())?;
                self.book_mut(book)?.wb.cfv_put_number_format(sheet, id, &f);
                Ok(Variant::Empty)
            }
            ("Font", Get) => Ok(self.alloc(Object::Font(owner))),
            ("Interior", Get) => Ok(self.alloc(Object::Interior(owner))),
            ("Borders", Get) => match a.get(0) {
                Some(_) => {
                    let i = a.i32(0)?.ok_or_else(OmError::app_defined)?;
                    Ok(self.alloc(Object::Border(owner, i)))
                }
                None => Ok(self.alloc(Object::Borders(owner))),
            },
            ("Delete", Func) => {
                self.object_guard(book, sheet, ProtObject::FormatCondition, None)?;
                let wb = &mut self.book_mut(book)?.wb;
                wb.cfv_delete_rule_at(sheet, id, scope);
                Ok(Variant::Empty)
            }
            ("Modify" | "_Modify", Func) => self.cf_modify(book, sheet, scope, &rule, anchor, a),
            _ => Err(unsupported("FormatCondition", m)),
        }
    }

    fn cf_become_text(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        op: i32,
    ) -> Result<(), OmError> {
        self.cf_edit(book, sheet, id, |r| {
            r.kind = CfKind::Text {
                op,
                text: String::new(),
            };
            r.f1 = None;
            r.f2 = None;
        })
        .map(|_| ())
    }

    fn cf_modify(
        &mut self,
        book: BookId,
        sheet: SheetId,
        scope: &[Rect],
        rule: &CfRule,
        anchor: CellAddr,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let ty = a
            .i32(0)?
            .ok_or_else(|| OmError::exception(PARAM_NOT_OPTIONAL, None))?;
        let id = rule.id;
        self.object_guard(book, sheet, ProtObject::FormatCondition, None)?;
        let needed = || OmError::exception(PARAM_NOT_OPTIONAL, None);
        let invalid = || OmError::exception(HResult::E_INVALIDARG.0, None);
        match ty {
            1 | 2 => {
                let op = if ty == 1 {
                    a.get(1).ok_or_else(needed)?;
                    let n = a.i32(1).map_err(|_| invalid())?.ok_or_else(invalid)?;
                    if !(1..9).contains(&n) {
                        return Err(invalid());
                    }
                    n
                } else {
                    1
                };
                let wb = &self.book(book)?.wb;
                let conv = |i: usize| -> Result<String, OmError> {
                    a.get(i).ok_or_else(needed)?;
                    let t = cf_formula_arg(a, i)
                        .map_err(|_| invalid())?
                        .ok_or_else(invalid)?;
                    if wb.cfv_bare_area(&t, anchor) {
                        return Err(invalid());
                    }
                    wb.cfv_to_relative(&t, anchor).map_err(|_| invalid())
                };
                let f1 = conv(2)?;
                let f2 = if ty == 1 && matches!(op, 1 | 2) {
                    Some(conv(3)?)
                } else {
                    None
                };
                let kind = if ty == 1 {
                    CfKind::CellValue(op)
                } else {
                    CfKind::Expression
                };
                let wb = &mut self.book_mut(book)?.wb;
                let prio = wb.cfv_sheet_mut(sheet).next_priority();
                wb.cfv_put_on_scope(sheet, id, scope, None, |sh| {
                    if let Some(pos) = sh.index_of(id) {
                        let r = &mut sh.rules[pos];
                        (r.kind, r.f1, r.f2, r.stop, r.prio) = (kind, Some(f1), f2, true, prio);
                        sh.rules.sort_by_key(|r| r.prio);
                    }
                });
                Ok(Variant::Empty)
            }
            9 => {
                if a.get(4).is_some() {
                    let text = a.string(4)?.unwrap_or_default();
                    let CfKind::Text { .. } = rule.kind else {
                        return Err(OmError::app_defined());
                    };
                    self.cf_edit(book, sheet, id, |r| {
                        if let CfKind::Text { text: t, .. } = &mut r.kind {
                            *t = text;
                        }
                    })?;
                }
                if a.get(5).is_some() {
                    let op = a.i32(5)?.unwrap_or(0);
                    if !(0..4).contains(&op) {
                        return Err(OmError::app_defined());
                    }
                    let CfKind::Text { .. } = rule.kind else {
                        self.cf_become_text(book, sheet, id, op)?;
                        return Err(OmError::app_defined());
                    };
                    self.cf_edit(book, sheet, id, |r| {
                        if let CfKind::Text { op: o, .. } = &mut r.kind {
                            *o = op;
                        }
                    })?;
                }
                Ok(Variant::Empty)
            }
            _ => Ok(Variant::Empty),
        }
    }

    pub(crate) fn cf_edit(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        f: impl FnOnce(&mut CfRule),
    ) -> Result<Variant, OmError> {
        let wb = &mut self.book_mut(book)?.wb;
        let sh = wb.cfv_sheet_mut(sheet);
        let pos = sh
            .index_of(id)
            .ok_or_else(|| OmError::exception(crate::error::SCODE_BAD_INDEX, None))?;
        f(&mut sh.rules[pos]);
        Ok(Variant::Empty)
    }

    pub(crate) fn display_format_member(
        &mut self,
        r: &RangeRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let owner = StyleOwner::Display(*r);
        match (m.name, m.kind) {
            ("Font", Get) => Ok(self.alloc(Object::Font(owner))),
            ("Interior", Get) => Ok(self.alloc(Object::Interior(owner))),
            ("Borders", Get) => Ok(self.alloc(Object::Borders(owner))),
            ("NumberFormat", Get) => self
                .agree_display(r, |s| s.number_format.to_string())
                .map(|v| v.map_or(Variant::Null, |s| bstr(&s))),
            ("Locked", Get) => self
                .agree_display(r, |s| s.locked)
                .map(|v| v.map_or(Variant::Null, Variant::Bool)),
            (
                "AddIndent"
                | "FormulaHidden"
                | "HorizontalAlignment"
                | "IndentLevel"
                | "MergeCells"
                | "NumberFormatLocal"
                | "Orientation"
                | "ReadingOrder"
                | "ShrinkToFit"
                | "VerticalAlignment"
                | "WrapText"
                | "Characters"
                | "Style",
                Get,
            ) => self.range_member(r, m, a),
            _ => Err(unsupported("DisplayFormat", m)),
        }
    }

    fn agree_display<T: PartialEq>(
        &self,
        r: &RangeRef,
        f: impl Fn(&xlcalc::style::Style) -> T,
    ) -> Result<Option<T>, OmError> {
        self.agree_on(&StyleOwner::Display(*r), f)
    }

    pub(crate) fn validation_exists(&self, r: &RangeRef) -> bool {
        let Ok((sheet, rect)) = self.resolve(r) else {
            return false;
        };
        self.book(r.book)
            .is_ok_and(|b| b.wb.dv_at(CellAddr::new(sheet, rect.r0, rect.c0)).is_some())
    }

    pub(crate) fn validation_multi_member(
        &mut self,
        r: &RangeRef,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        match (m.name, m.kind) {
            ("Value", InvokeKind::Get) => {
                let (sheet, rect) = self.resolve(r).map_err(|_| OmError::app_defined())?;
                let wb = &self.book(r.book)?.wb;
                Ok(Variant::Bool(
                    wb.dv_value(CellAddr::new(sheet, rect.r0, rect.c0))
                        .unwrap_or(true),
                ))
            }
            ("ErrorTitle" | "InputTitle", InvokeKind::Get) => Ok(Variant::BstrNull),
            ("IMEMode", InvokeKind::Get) => Ok(i4(0)),
            _ => Err(OmError::app_defined()),
        }
    }

    pub(crate) fn validation_member(
        &mut self,
        r: &RangeRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let (sheet, rect) = self.resolve(r).map_err(|_| OmError::app_defined())?;
        let top = CellAddr::new(sheet, rect.r0, rect.c0);
        let none = || OmError::app_defined();
        let find = |app: &Self| -> Result<Option<Dv>, OmError> {
            Ok(app.book(r.book)?.wb.dv_at(top).cloned())
        };
        match (m.name, m.kind) {
            ("Add" | "Modify", Func) => {
                self.object_guard(r.book, sheet, ProtObject::Validation, None)?;
                let modify = m.name == "Modify";
                let old = find(self)?;
                if modify && old.is_none() {
                    return Err(none());
                }
                if modify {
                    let wb = &self.book(r.book)?.wb;
                    let all = (rect.r0..=rect.r1.min(rect.r0 + 10_000)).all(|row| {
                        (rect.c0..=rect.c1.min(rect.c0 + 1_000))
                            .all(|col| wb.dv_at(CellAddr::new(sheet, row, col)).is_some())
                    });
                    if !all {
                        return Err(none());
                    }
                }
                {
                    let wb = &self.book(r.book)?.wb;
                    let sh = wb.cfv_sheet(sheet);
                    if !modify
                        && sh.is_some_and(|s| {
                            s.dvs
                                .iter()
                                .any(|d| d.applies.iter().any(|x| x.intersects(&rect)))
                        })
                    {
                        return Err(none());
                    }
                }

                if modify && (0..5).all(|i| a.get(i).is_none()) {
                    return Err(none());
                }
                let vint = |i: usize| a.i32(i).map_err(|_| OmError::app_defined());
                let ty = match (if modify { vint(0)? } else { a.i32(0)? }, &old) {
                    (Some(t), _) => t,
                    (None, Some(o)) if modify => o.kind,
                    _ => return Err(none()),
                };
                if !(0..=7).contains(&ty) {
                    return Err(none());
                }
                let keep = old.as_ref().filter(|_| modify);

                if modify {
                    let wb = &self.book(r.book)?.wb;
                    if let Some(d0) = wb.dv_at(top) {
                        let same = (rect.r0..=rect.r1.min(rect.r0 + 10_000)).all(|row| {
                            (rect.c0..=rect.c1.min(rect.c0 + 1_000)).all(|col| {
                                wb.dv_at(CellAddr::new(sheet, row, col))
                                    .is_some_and(|d| std::ptr::eq(d, d0))
                            })
                        });
                        if !same {
                            return Err(none());
                        }
                    }
                }
                let alert = vint(1)?.unwrap_or(keep.map_or(1, |o| o.alert));
                let op = vint(2)?.unwrap_or(keep.map_or(1, |o| o.op));
                if !(1..=3).contains(&alert) || !(1..=8).contains(&op) {
                    return Err(none());
                }
                let f1 = formula_arg(a, 3)?.or_else(|| keep.and_then(|o| o.f1.clone()));
                let f2 = formula_arg(a, 4)?.or_else(|| keep.and_then(|o| o.f2.clone()));

                let (f1, f2) = if ty == 0 { (None, None) } else { (f1, f2) };
                if ty != 0 && f1.as_deref().is_none_or(str::is_empty) {
                    return Err(none());
                }
                let two = matches!(ty, 1 | 2 | 4 | 5 | 6) && matches!(op, 1 | 2);
                if two && f2.as_deref().is_none_or(str::is_empty) {
                    return Err(none());
                }
                if ty == 3
                    && f1.as_deref().is_some_and(|t| t.starts_with('='))
                    && !self
                        .book(r.book)?
                        .wb
                        .dv_list_source_ok(f1.as_deref().unwrap_or_default(), top)
                {
                    return Err(OmError::app_defined());
                }
                if matches!(ty, 1 | 4..=6) {
                    let wb = &self.book(r.book)?.wb;
                    let limits = [(&f1, true), (&f2, two)];
                    if limits.iter().any(|(f, used)| {
                        *used && f.as_deref().is_some_and(|t| !wb.dv_limit_ok(ty, t, top))
                    }) {
                        return Err(none());
                    }
                }
                let wb = &mut self.book_mut(r.book)?.wb;

                if ty != 0 && (ty != 3 || f1.as_deref().is_some_and(|t| t.starts_with('='))) {
                    wb.note_number_format(None);
                }
                let conv = |f: Option<String>| -> Result<Option<String>, OmError> {
                    match f {
                        Some(t) if t.starts_with('=') => wb
                            .cfv_to_relative(&t, top)
                            .map(Some)
                            .map_err(|_| OmError::app_defined()),
                        Some(t) if matches!(ty, 1 | 2 | 4 | 5 | 6) => wb
                            .dv_literal(ty, &t)
                            .map(|n| Some(n.to_string()))
                            .ok_or_else(OmError::app_defined),
                        other => Ok(other),
                    }
                };

                if ty == 4
                    && [&f1, &f2].iter().any(|f| {
                        f.as_deref().is_some_and(|t| {
                            !t.starts_with('=')
                                && t.parse::<f64>().is_err()
                                && t.contains(':')
                                && !t.contains(['/', '-'])
                        })
                    })
                {
                    return Err(OmError::app_defined());
                }

                let plain = |f: Option<String>| {
                    f.map(|t| match t.strip_prefix('=') {
                        Some(n)
                            if matches!(ty, 1 | 2 | 6)
                                && !n.is_empty()
                                && n.chars().all(|c| c.is_ascii_digit() || c == '.')
                                && n.parse::<f64>().is_ok() =>
                        {
                            n.to_string()
                        }
                        _ => t,
                    })
                };
                let (f1, f2) = (conv(plain(f1))?, if two { conv(plain(f2))? } else { None });

                if matches!(ty, 1 | 2 | 4 | 5 | 6) && matches!(op, 1 | 2) {
                    if let (Some(Ok(x)), Some(Ok(y))) = (
                        f1.as_deref().map(str::parse::<f64>),
                        f2.as_deref().map(str::parse::<f64>),
                    ) {
                        if x > y {
                            return Err(none());
                        }
                    }
                }
                let dv = Dv {
                    applies: vec![rect],
                    kind: ty,
                    alert,
                    op,
                    f1,
                    f2,
                    ignore_blank: true,
                    dropdown: true,
                    show_input: true,
                    show_error: true,
                    input_title: String::new(),
                    input_message: String::new(),
                    error_title: String::new(),
                    error_message: String::new(),
                };
                let sh = wb.cfv_sheet_mut(sheet);
                match keep {
                    Some(o) => {
                        let dv = Dv {
                            applies: vec![rect],
                            ignore_blank: o.ignore_blank,
                            dropdown: o.dropdown,
                            show_input: o.show_input,
                            show_error: o.show_error,
                            input_title: o.input_title.clone(),
                            input_message: o.input_message.clone(),
                            error_title: o.error_title.clone(),
                            error_message: o.error_message.clone(),
                            ..dv
                        };
                        if let Some(d) = sh.dvs.iter_mut().find(|d| d.applies == o.applies) {
                            let rest: Vec<Rect> =
                                o.applies.iter().flat_map(|x| x.subtract(&rect)).collect();
                            if rest.is_empty() {
                                *d = Dv {
                                    applies: o.applies.clone(),
                                    ..dv
                                };
                            } else {
                                d.applies = rest;
                                sh.dvs.push(dv);
                            }
                        }
                    }
                    None => sh.dvs.push(dv),
                }
                Ok(Variant::Empty)
            }
            ("Delete", Func) => {
                let wb = &mut self.book_mut(r.book)?.wb;
                let mut dvs = wb.cfv_sheet_mut(sheet).dvs.clone();
                for d in &mut dvs {
                    let (r0, c0) = d.anchor();
                    d.applies = d.applies.iter().flat_map(|x| x.subtract(&rect)).collect();
                    if d.applies.is_empty() {
                        continue;
                    }

                    let (r1, c1) = d.anchor();
                    if (r0, c0) != (r1, c1) {
                        let (from, to) =
                            (CellAddr::new(sheet, r0, c0), CellAddr::new(sheet, r1, c1));
                        for f in [&mut d.f1, &mut d.f2] {
                            if let Some(t) = f.as_deref().filter(|t| t.starts_with('=')) {
                                if let Some(n) = wb.cfv_retarget(t, from, to) {
                                    *f = Some(n);
                                }
                            }
                        }
                    }
                }
                dvs.retain(|d| !d.applies.is_empty());
                wb.cfv_sheet_mut(sheet).dvs = dvs;
                Ok(Variant::Empty)
            }

            ("Value", Get) => {
                let wb = &self.book(r.book)?.wb;
                Ok(Variant::Bool(wb.dv_value(top).unwrap_or(true)))
            }
            ("ErrorTitle" | "InputTitle", Get) if find(self)?.is_none() => Ok(Variant::BstrNull),

            ("IMEMode", Get) => Ok(i4(0)),
            ("IMEMode", Put) => {
                self.coerce_wb_put(m, a)?;
                Ok(Variant::Empty)
            }
            _ => {
                let dv = match find(self)? {
                    Some(d) => d,

                    None if matches!(m.name, "ErrorTitle" | "InputTitle") && m.kind == Get => {
                        return Ok(Variant::BstrNull);
                    }
                    None => return Err(none()),
                };

                let covered = {
                    let wb = &self.book(r.book)?.wb;
                    let Some(dv_ref) = wb.dv_at(top) else {
                        return Err(none());
                    };
                    let area = u64::from(rect.r1 - rect.r0 + 1) * u64::from(rect.c1 - rect.c0 + 1);
                    area > 100_000
                        || (rect.r0..=rect.r1).all(|row| {
                            (rect.c0..=rect.c1).all(|col| {
                                wb.dv_at(CellAddr::new(sheet, row, col))
                                    .is_some_and(|d| std::ptr::eq(d, dv_ref))
                            })
                        })
                };
                if !covered {
                    if matches!(m.name, "ErrorTitle" | "InputTitle") && m.kind == Get {
                        return Ok(Variant::BstrNull);
                    }
                    return Err(none());
                }
                let anchor = CellAddr::new(sheet, dv.anchor().0, dv.anchor().1);
                let reader = CellAddr::new(sheet, rect.r0, rect.c0);
                match (m.name, m.kind) {
                    ("Type", Get) => Ok(i4(dv.kind)),
                    ("AlertStyle", Get) => Ok(i4(dv.alert)),
                    ("Operator", Get) => Ok(i4(dv.op)),
                    ("Formula1" | "Formula2", Get) => {
                        let f = if m.name == "Formula1" { &dv.f1 } else { &dv.f2 };
                        match f {
                            Some(f) if f.starts_with('=') => {
                                let wb = &self.book(r.book)?.wb;
                                Ok(Variant::Bstr(
                                    wb.cfv_retarget(f, anchor, reader)
                                        .unwrap_or_else(|| f.clone()),
                                ))
                            }
                            Some(f) if matches!(dv.kind, 4 | 5) => {
                                let wb = &self.book(r.book)?.wb;
                                Ok(Variant::Bstr(match f.parse::<f64>() {
                                    Ok(n) => wb.dv_literal_shown(dv.kind, n),
                                    Err(_) => f.clone(),
                                }))
                            }

                            Some(f) if dv.kind == 3 && f.contains(['/', '-']) => {
                                let wb = &self.book(r.book)?.wb;
                                Ok(Variant::Bstr(match wb.dv_literal(4, f) {
                                    Some(n) => wb.dv_literal_shown(4, n),
                                    None => f.clone(),
                                }))
                            }
                            Some(f) => Ok(Variant::Bstr(f.clone())),
                            None => Ok(Variant::Bstr(String::new())),
                        }
                    }

                    ("IMEMode", Get) => Ok(i4(0)),
                    ("IMEMode", Put) => Ok(Variant::Empty),
                    ("IgnoreBlank", Get) => Ok(Variant::Bool(dv.ignore_blank)),
                    ("InCellDropdown", Get) => Ok(Variant::Bool(dv.dropdown)),
                    ("ShowInput", Get) => Ok(Variant::Bool(dv.show_input)),
                    ("ShowError", Get) => Ok(Variant::Bool(dv.show_error)),
                    ("ErrorTitle", Get) => Ok(bstr(&dv.error_title)),
                    ("ErrorMessage", Get) => Ok(bstr(&dv.error_message)),
                    ("InputTitle", Get) => Ok(bstr(&dv.input_title)),
                    ("InputMessage", Get) => Ok(bstr(&dv.input_message)),
                    ("Value", Get) => {
                        let wb = &self.book(r.book)?.wb;
                        Ok(Variant::Bool(wb.dv_value(top).unwrap_or(true)))
                    }
                    ("IgnoreBlank" | "InCellDropdown" | "ShowInput" | "ShowError", Put) => {
                        let b = Application::put_value(a).bool(0)?.unwrap_or(false);
                        self.dv_edit(r, sheet, top, |d| match m.name {
                            "IgnoreBlank" => d.ignore_blank = b,
                            "InCellDropdown" => d.dropdown = b,
                            "ShowInput" => d.show_input = b,
                            _ => d.show_error = b,
                        })
                    }
                    ("ErrorTitle" | "ErrorMessage" | "InputTitle" | "InputMessage", Put) => {
                        let s = self.put_string_alpha(a)?;
                        let limit = match m.name {
                            "ErrorMessage" => 225,

                            "InputMessage" => 254,
                            _ => 32,
                        };
                        if s.encode_utf16().count() > limit {
                            return Err(none());
                        }
                        self.dv_edit(r, sheet, top, |d| match m.name {
                            "ErrorTitle" => d.error_title = s,
                            "ErrorMessage" => d.error_message = s,
                            "InputTitle" => d.input_title = s,
                            _ => d.input_message = s,
                        })
                    }
                    _ => Err(unsupported("Validation", m)),
                }
            }
        }
    }

    fn dv_edit(
        &mut self,
        r: &RangeRef,
        sheet: SheetId,
        at: CellAddr,
        f: impl FnOnce(&mut Dv),
    ) -> Result<Variant, OmError> {
        let wb = &mut self.book_mut(r.book)?.wb;
        let sh = wb.cfv_sheet_mut(sheet);
        let d = sh
            .dvs
            .iter_mut()
            .find(|d| d.applies.iter().any(|x| x.contains(at.row, at.col)))
            .ok_or_else(OmError::app_defined)?;
        f(d);
        Ok(Variant::Empty)
    }
}

fn number_arg(a: &Args, i: usize) -> Option<f64> {
    match a.get(i)? {
        Variant::I2(n) => Some(f64::from(*n)),
        Variant::I4(n) => Some(f64::from(*n)),
        Variant::R4(x) => Some(f64::from(*x)),
        Variant::R8(x) | Variant::Date(x) => Some(*x),
        Variant::Cy(c) => Some(*c as f64 / 10_000.0),
        _ => None,
    }
}

fn strict_int(a: &Args, i: usize) -> Result<Option<i32>, OmError> {
    match a.get(i) {
        None => Ok(None),
        Some(_) => match number_arg(a, i) {
            Some(x) if x.trunc().abs() < 2_147_483_648.0 => Ok(Some(x.trunc() as i32)),
            _ => Err(OmError::exception(crate::error::SCODE_TYPE_MISMATCH, None)),
        },
    }
}
