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
                let t = Application::put_value(a)
                    .i32(0)?
                    .ok_or_else(OmError::app_defined)?;
                if !(0..=5).contains(&t) {
                    return Err(OmError::app_defined());
                }
                self.scale_edit(book, sheet, id, k, |c| c.ty = t)
            }
            ("Value", Get) => match (&c.formula, c.ty) {
                (Some(f), 4) => Ok(Variant::Bstr(f.clone())),
                _ => Ok(Variant::R8(c.value)),
            },
            ("Value", Put) => {
                let v = Application::put_value(a);
                if let Ok(Some(n)) = v.f64(0) {
                    self.scale_edit(book, sheet, id, k, |c| {
                        c.value = n;
                        c.formula = None;
                    })
                } else {
                    let t = v.string(0)?.ok_or_else(OmError::app_defined)?;
                    self.scale_edit(book, sheet, id, k, |c| c.formula = Some(t))
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
                let n = Application::put_value(a)
                    .i32(0)?
                    .ok_or_else(OmError::app_defined)?;
                self.scale_edit(book, sheet, id, k, |c| {
                    c.color = swap_rb(n as u32 & 0xFFFFFF)
                })
            }
            _ => Err(unsupported("FormatColor", m)),
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

impl Application<'_> {
    fn cf_handle(&mut self, book: BookId, sheet: SheetId, id: u32, scope: Rect) -> Variant {
        let iface = self
            .book(book)
            .ok()
            .and_then(|b| b.wb.cfv_sheet(sheet))
            .and_then(|s| s.index_of(id).map(|i| CfIface::of(&s.rules[i].kind)))
            .unwrap_or(CfIface::Plain);
        self.alloc(Object::FormatCondition(book, sheet, id, iface, scope))
    }

    fn cf_target(&self, r: &RangeRef) -> Result<(SheetId, Rect), OmError> {
        self.resolve(r)
    }

    fn cf_ids(&self, r: &RangeRef) -> Result<Vec<u32>, OmError> {
        let (sheet, rect) = self.cf_target(r)?;
        let wb = &self.book(r.book)?.wb;
        Ok(wb.cfv_sheet(sheet).map_or_else(Vec::new, |s| {
            s.rules
                .iter()
                .filter(|x| x.applies.iter().any(|a| a.intersects(&rect)))
                .map(|x| x.id)
                .collect()
        }))
    }

    pub(crate) fn format_conditions_member(
        &mut self,
        r: &RangeRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let (sheet, rect) = self.cf_target(r)?;
        match (m.name, m.kind) {
            ("Count", Get) => Ok(i4(self.cf_ids(r)?.len() as i32)),
            ("Item" | "_Default", _) => {
                let ids = self.cf_ids(r)?;
                let i = a
                    .i32(0)
                    .map_err(|_| OmError::bad_index())?
                    .ok_or_else(OmError::app_defined)?;
                if i < 1 || i as usize > ids.len() {
                    return Err(OmError::bad_index());
                }
                Ok(self.cf_handle(r.book, sheet, ids[i as usize - 1], rect))
            }
            ("_NewEnum", Get) => {
                let items = self
                    .cf_ids(r)?
                    .into_iter()
                    .map(|id| self.cf_handle(r.book, sheet, id, rect))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            ("Delete", Func) => {
                let wb = &mut self.book_mut(r.book)?.wb;
                let sh = wb.cfv_sheet_mut(sheet);
                for rule in &mut sh.rules {
                    rule.applies = rule
                        .applies
                        .iter()
                        .flat_map(|x| x.subtract(&rect))
                        .collect();
                }
                sh.rules.retain(|x| !x.applies.is_empty());
                Ok(Variant::Empty)
            }
            ("Add", Func) => self.cf_add(r, sheet, rect, a),
            ("AddTop10", Func) => self.cf_push(
                r.book,
                sheet,
                rect,
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
                let kind = scale_kind(n).ok_or_else(OmError::app_defined)?;
                self.cf_push(r.book, sheet, rect, kind, None, None)
            }
            ("AddDatabar", Func) => self.cf_push(
                r.book,
                sheet,
                rect,
                CfKind::DataBar(Box::default()),
                None,
                None,
            ),
            ("AddIconSetCondition", Func) => self.cf_push(
                r.book,
                sheet,
                rect,
                CfKind::IconSet(IconSetSpec::new(4, false).ok_or_else(OmError::app_defined)?),
                None,
                None,
            ),
            ("AddUniqueValues", Func) => self.cf_push(
                r.book,
                sheet,
                rect,
                CfKind::Unique { duplicate: false },
                None,
                None,
            ),
            ("AddAboveAverage", Func) => self.cf_push(
                r.book,
                sheet,
                rect,
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
        rect: Rect,
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
        let f1 = formula_arg(a, 2)?.filter(|_| given(2));
        let f2 = formula_arg(a, 3)?.filter(|_| given(3));
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
                let f = f1.as_ref().ok_or_else(needed)?;
                if f.is_empty() {
                    return Err(invalid());
                }
                CfKind::Expression
            }
            9 => {
                let text = a.string(4)?.filter(|_| given(4)).ok_or_else(needed)?;
                let op = a.i32(5)?.unwrap_or(0);
                if !(0..=3).contains(&op) {
                    return Err(OmError::exception(E_UNEXPECTED, None));
                }
                CfKind::Text { op, text }
            }

            10 | 14 | 15 => CfKind::Blanks,
            11 => {
                let period = a.i32(6)?.filter(|_| given(6)).ok_or_else(needed)?;
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
            (f1, if matches!(op, Some(1 | 2)) { f2 } else { None })
        } else {
            (None, None)
        };
        self.cf_push(r.book, sheet, rect, kind, f1, f2)
    }

    fn cf_push(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        kind: CfKind,
        f1: Option<String>,
        f2: Option<String>,
    ) -> Result<Variant, OmError> {
        let anchor = CellAddr::new(sheet, rect.r0, rect.c0);
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
            seq: 0,
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
            applies: vec![rect],
            f1,
            f2,
            dxf: Dxf::default(),
        };
        let id = wb.cfv_sheet_mut(sheet).add_rule(rule);
        Ok(self.cf_handle(book, sheet, id, rect))
    }

    pub(crate) fn format_condition_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        scope: Rect,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let gone = || OmError::exception(crate::error::SCODE_BAD_INDEX, None);
        let (pos, rule) = {
            let wb = &self.book(book)?.wb;
            let sh = wb.cfv_sheet(sheet).ok_or_else(gone)?;
            let pos = sh.index_of(id).ok_or_else(gone)?;
            (pos, sh.rules[pos].clone())
        };
        let anchor = CellAddr::new(sheet, rule.anchor().0, rule.anchor().1);
        let owner = StyleOwner::Cond(book, sheet, id);
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
                let wb = &mut self.book_mut(book)?.wb;
                let sh = wb.cfv_sheet_mut(sheet);
                sh.set_priority(pos, p);
                Ok(Variant::Empty)
            }
            ("SetFirstPriority", Func) | ("SetLastPriority", Func) => {
                let wb = &mut self.book_mut(book)?.wb;
                let sh = wb.cfv_sheet_mut(sheet);
                if m.name == "SetFirstPriority" {
                    sh.set_priority(pos, 1);
                } else {
                    let top = sh.rules.iter().map(|r| r.prio).max().unwrap_or(0);
                    sh.rules[pos].prio = top + 1;
                    sh.rules.sort_by_key(|r| r.prio);
                }
                Ok(Variant::Empty)
            }
            ("ModifyAppliesToRange", Func) => {
                let (_, to_sheet, rects) = self.range_areas(a.get(0))?;
                if to_sheet != sheet || rects.len() != 1 {
                    return Err(OmError::app_defined());
                }
                let rect = rects[0];
                let old = anchor;
                let new = CellAddr::new(sheet, rect.r0, rect.c0);
                let wb = &mut self.book_mut(book)?.wb;
                let moved = [&rule.f1, &rule.f2].map(|f| {
                    f.as_ref()
                        .map(|t| wb.cfv_retarget(t, old, new).unwrap_or_else(|| t.clone()))
                });
                self.cf_edit(book, sheet, id, |r| {
                    r.applies = vec![rect];
                    let [f1, f2] = moved;
                    r.f1 = f1;
                    r.f2 = f2;
                })
            }
            ("ReverseOrder" | "ShowIconOnly", Get) => Ok(Variant::Bool(false)),
            ("ShowValue", Get) => Ok(Variant::Bool(true)),
            ("StopIfTrue", Get) => Ok(Variant::Bool(rule.stop)),
            ("StopIfTrue", Put) => {
                let b = Application::put_value(a).bool(0)?.unwrap_or(false);
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
                CfKind::Text { text, .. } => Ok(bstr(text)),
                _ => Err(OmError::app_defined()),
            },
            ("TextOperator", Get) => match rule.kind {
                CfKind::Text { op, .. } => Ok(i4(op)),

                _ => self
                    .opts
                    .get(&format!("fc{}:{}:{}:to", book.0, sheet.0, id))
                    .cloned()
                    .ok_or_else(OmError::app_defined),
            },
            ("TextOperator", Put) if !matches!(rule.kind, CfKind::Text { .. }) => {
                self.coerce_wb_put(m, a)?;
                let v = self.put_i32(a)?;
                if !(0..=3).contains(&v) {
                    return Err(invalid_arg());
                }
                self.opts
                    .insert(format!("fc{}:{}:{}:to", book.0, sheet.0, id), i4(v));
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
            ("AppliesTo", Get) => {
                let rects = rule.applies.clone();
                self.new_multi(book, sheet, &rects, crate::app::Mode::Cells)
            }
            ("NumberFormat", Get) => {
                if rule.dxf.mask.number_format {
                    Ok(bstr(&rule.dxf.style.number_format))
                } else {
                    Ok(Variant::Empty)
                }
            }
            ("NumberFormat", Put) => {
                let f = match Application::put_value(a).get(0) {
                    None | Some(Variant::Empty) => ";;".to_string(),
                    Some(Variant::Bool(t)) => if *t { "-1" } else { "0" }.to_string(),
                    _ => Application::put_value(a).string(0)?.unwrap_or_default(),
                };
                self.cf_edit(book, sheet, id, |r| {
                    r.dxf.mask.number_format = true;
                    r.dxf.style.number_format = f.as_str().into();
                })
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
                let wb = &mut self.book_mut(book)?.wb;
                let sh = wb.cfv_sheet_mut(sheet);
                let applies = std::mem::take(&mut sh.rules[pos].applies);
                sh.rules[pos].applies = applies.iter().flat_map(|x| x.subtract(&scope)).collect();
                if sh.rules[pos].applies.is_empty() {
                    sh.rules.remove(pos);
                }
                Ok(Variant::Empty)
            }
            ("Modify" | "_Modify", Func) => {
                let ty = a.i32(0)?.ok_or_else(OmError::app_defined)?;
                let op = a.i32(1)?;
                let f1 = formula_arg(a, 2)?;
                let f2 = formula_arg(a, 3)?;
                if ty != rule.kind.type_code() && !matches!(ty, 1 | 2) {
                    return Err(OmError::app_defined());
                }
                let kind = match ty {
                    1 => CfKind::CellValue(
                        op.filter(|o| (1..=8).contains(o))
                            .ok_or_else(OmError::app_defined)?,
                    ),
                    2 => CfKind::Expression,
                    _ => rule.kind.clone(),
                };
                let wb = &self.book(book)?.wb;
                let conv = |f: Option<String>| -> Result<Option<String>, OmError> {
                    match f {
                        None => Ok(None),
                        Some(t) => wb
                            .cfv_to_relative(&t, anchor)
                            .map(Some)
                            .map_err(|_| OmError::app_defined()),
                    }
                };
                let (f1, f2) = (conv(f1)?, conv(f2)?);
                self.cf_edit(book, sheet, id, |r| {
                    r.kind = kind;
                    r.f1 = f1;
                    r.f2 = f2;
                })
            }
            _ => Err(unsupported("FormatCondition", m)),
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
                let ty = match (a.i32(0)?, &old) {
                    (Some(t), _) => t,
                    (None, Some(o)) if modify => o.kind,
                    _ => return Err(none()),
                };
                if !(0..=7).contains(&ty) {
                    return Err(none());
                }
                let keep = old.as_ref().filter(|_| modify);
                let alert = a.i32(1)?.unwrap_or(keep.map_or(1, |o| o.alert));
                let op = a.i32(2)?.unwrap_or(keep.map_or(1, |o| o.op));
                if !(1..=3).contains(&alert) || !(1..=8).contains(&op) {
                    return Err(none());
                }
                let f1 = formula_arg(a, 3)?.or_else(|| keep.and_then(|o| o.f1.clone()));
                let f2 = formula_arg(a, 4)?.or_else(|| keep.and_then(|o| o.f2.clone()));
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
                let wb = &mut self.book_mut(r.book)?.wb;
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
                let (f1, f2) = (conv(f1)?, if two { conv(f2)? } else { None });
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
                    touched: false,
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
                            touched: o.touched,
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
                let sh = wb.cfv_sheet_mut(sheet);
                for d in &mut sh.dvs {
                    d.applies = d.applies.iter().flat_map(|x| x.subtract(&rect)).collect();
                }
                sh.dvs.retain(|d| !d.applies.is_empty());
                Ok(Variant::Empty)
            }
            ("Value", Get) => {
                let wb = &self.book(r.book)?.wb;
                let ok = (rect.r0..=rect.r1.min(rect.r0 + 10_000)).all(|row| {
                    (rect.c0..=rect.c1.min(rect.c0 + 1_000))
                        .all(|col| wb.dv_value(CellAddr::new(sheet, row, col)).unwrap_or(true))
                });
                Ok(Variant::Bool(ok))
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
                        let s = Application::put_value(a).string(0)?.unwrap_or_default();
                        let limit = match m.name {
                            "ErrorMessage" => 225,
                            "InputMessage" => 255,
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
