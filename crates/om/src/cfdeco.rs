use xlcalc::cfv::{BarColor, CfKind, CfRule, CondValue, DataBarSpec, IconSetSpec};
use xlcalc::style::{cv_color_index, indexed_rgb, stored_tint};
use xlcalc::SheetId;
use xlcalc_variant::{HResult, Variant};

use crate::app::{unsupported, Application, BookId, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

type CritEdit = Box<dyn FnOnce(&mut xlcalc::cfv::IconCrit, bool)>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum CfPart {
    Point(bool),
    BarColor,
    AxisColor,
    BarBorder,
    BorderColor,
    NegFormat,
    NegColor,
    NegBorderColor,
    IconSet,
    IconCriteria,

    IconCriterion(usize),
}

impl CfPart {
    pub(crate) fn interface_name(self) -> &'static str {
        match self {
            CfPart::Point(_) => "ConditionValue",
            CfPart::BarColor
            | CfPart::AxisColor
            | CfPart::BorderColor
            | CfPart::NegColor
            | CfPart::NegBorderColor => "FormatColor",
            CfPart::BarBorder => "DataBarBorder",
            CfPart::NegFormat => "NegativeBarFormat",
            CfPart::IconSet => "IconSet",
            CfPart::IconCriteria => "IconCriteria",
            CfPart::IconCriterion(_) => "IconCriterion",
        }
    }
}

fn i4(n: i32) -> Variant {
    Variant::I4(n)
}

fn invalid_arg() -> OmError {
    OmError::exception(HResult::E_INVALIDARG.0, None)
}

fn swap_rb(c: u32) -> u32 {
    ((c & 255) << 16) | (c & 0xFF00) | ((c >> 16) & 255)
}

fn bar_of(rule: &CfRule) -> Result<&DataBarSpec, OmError> {
    match &rule.kind {
        CfKind::DataBar(d) => Ok(d),
        _ => Err(OmError::app_defined()),
    }
}

fn icons_of(rule: &CfRule) -> Result<&IconSetSpec, OmError> {
    match &rule.kind {
        CfKind::IconSet(s) => Ok(s),
        _ => Err(OmError::app_defined()),
    }
}

fn color_of(d: &DataBarSpec, part: CfPart) -> BarColor {
    match part {
        CfPart::BarColor => d.bar,
        CfPart::AxisColor => d.axis,
        CfPart::BorderColor => d.border,
        CfPart::NegColor if d.neg_color_type == 1 => d.bar,
        CfPart::NegColor => d.neg,
        CfPart::NegBorderColor if d.neg_border_type == 1 => d.border,
        _ => d.neg_border,
    }
}

fn color_mut(d: &mut DataBarSpec, part: CfPart) -> &mut BarColor {
    match part {
        CfPart::BarColor => &mut d.bar,
        CfPart::AxisColor => &mut d.axis,
        CfPart::BorderColor => &mut d.border,
        CfPart::NegColor => &mut d.neg,
        _ => &mut d.neg_border,
    }
}

fn value_variant(c: &CondValue) -> Variant {
    match &c.formula {
        Some(f) => Variant::Bstr(f.clone()),
        None => Variant::R8(c.value),
    }
}

impl Application<'_> {
    fn rule_of(&self, book: BookId, sheet: SheetId, id: u32) -> Result<CfRule, OmError> {
        let wb = &self.book(book)?.wb;
        let gone = OmError::bad_index;
        let sh = wb.cfv_sheet(sheet).ok_or_else(gone)?;
        let i = sh.index_of(id).ok_or_else(gone)?;
        Ok(sh.rules[i].clone())
    }

    fn kind_edit(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        f: impl FnOnce(&mut CfKind),
    ) -> Result<Variant, OmError> {
        self.cf_edit(book, sheet, id, |r| f(&mut r.kind))
    }

    fn bar_edit(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        f: impl FnOnce(&mut DataBarSpec),
    ) -> Result<Variant, OmError> {
        self.kind_edit(book, sheet, id, |k| {
            if let CfKind::DataBar(d) = k {
                f(d);
            }
        })
    }

    fn icons_edit(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        f: impl FnOnce(&mut IconSetSpec),
    ) -> Result<Variant, OmError> {
        self.kind_edit(book, sheet, id, |k| {
            if let CfKind::IconSet(s) = k {
                f(s);
            }
        })
    }

    pub(crate) fn cf_deco_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        rule: &CfRule,
        m: &'static Member,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        use InvokeKind::*;
        let part = |s: &mut Self, p: CfPart| Ok(s.alloc(Object::CfPart(book, sheet, id, p)));
        let put = || Application::put_value(a);
        let r = match (&rule.kind, m.name, m.kind) {
            (CfKind::DataBar(_), "MinPoint", Get) => part(self, CfPart::Point(false)),
            (CfKind::DataBar(_), "MaxPoint", Get) => part(self, CfPart::Point(true)),
            (CfKind::DataBar(_), "BarColor", Get) => part(self, CfPart::BarColor),
            (CfKind::DataBar(_), "AxisColor", Get) => part(self, CfPart::AxisColor),
            (CfKind::DataBar(_), "BarBorder", Get) => part(self, CfPart::BarBorder),
            (CfKind::DataBar(_), "NegativeBarFormat", Get) => part(self, CfPart::NegFormat),
            (CfKind::DataBar(d), "ShowValue", Get) => Ok(Variant::Bool(d.show_value)),
            (CfKind::DataBar(_), "ShowValue", Put) => put().bool(0).and_then(|b| {
                let b = b.ok_or_else(OmError::app_defined)?;
                self.bar_edit(book, sheet, id, |d| d.show_value = b)
            }),
            (CfKind::DataBar(d), "BarFillType", Get) => Ok(i4(d.fill_type)),
            (CfKind::DataBar(d), "Direction", Get) => Ok(i4(d.direction)),
            (CfKind::DataBar(d), "AxisPosition", Get) => Ok(i4(d.axis_position)),
            (CfKind::DataBar(_), "BarFillType" | "Direction" | "AxisPosition", Put) => {
                let ok = |n: i32| match m.name {
                    "BarFillType" => (0..=1).contains(&n),
                    "Direction" => (-5004..=-5002).contains(&n),
                    _ => (0..=2).contains(&n),
                };
                put().i32(0).and_then(|n| {
                    let n = n.filter(|n| ok(*n)).ok_or_else(invalid_arg)?;
                    self.bar_edit(book, sheet, id, |d| match m.name {
                        "BarFillType" => d.fill_type = n,
                        "Direction" => d.direction = n,
                        _ => d.axis_position = n,
                    })
                })
            }
            (CfKind::IconSet(_), "IconCriteria", Get) => part(self, CfPart::IconCriteria),
            (CfKind::IconSet(_), "IconSet", Get) => part(self, CfPart::IconSet),
            (CfKind::IconSet(_), "IconSet", Put) => {
                let v = put();
                let Some(Variant::Dispatch(Some(h))) = v.get(0) else {
                    return Some(Err(OmError::app_defined()));
                };
                let Some(Object::IconSetDef(_, n)) = self.arena.get(*h) else {
                    return Some(Err(OmError::app_defined()));
                };
                let n = *n;
                self.icons_edit(book, sheet, id, |s| {
                    if let Some(new) = IconSetSpec::new(n, s.reverse) {
                        let (icon_only, percentile) = (s.icon_only, s.percentile);
                        *s = IconSetSpec {
                            icon_only,
                            percentile,
                            ..new
                        };
                    }
                })
            }
            (CfKind::IconSet(s), "ReverseOrder", Get) => Ok(Variant::Bool(s.reverse)),
            (CfKind::IconSet(s), "ShowIconOnly", Get) => Ok(Variant::Bool(s.icon_only)),
            (CfKind::IconSet(s), "PercentileValues", Get) => Ok(Variant::Bool(s.percentile)),
            (CfKind::IconSet(s), "ReverseOrder", Put) => put().bool(0).and_then(|b| {
                let b = b.ok_or_else(OmError::app_defined)?;
                if s.custom {
                    return Err(invalid_arg());
                }
                self.icons_edit(book, sheet, id, |s| {
                    if s.reverse != b {
                        s.reverse = b;
                        let icons: Vec<i32> = s.crits.iter().rev().map(|c| c.icon).collect();
                        for (c, i) in s.crits.iter_mut().zip(icons) {
                            c.icon = i;
                        }
                    }
                })
            }),
            (CfKind::IconSet(_), "ShowIconOnly" | "PercentileValues", Put) => {
                put().bool(0).and_then(|b| {
                    let b = b.ok_or_else(OmError::app_defined)?;
                    self.icons_edit(book, sheet, id, |s| {
                        if m.name == "ShowIconOnly" {
                            s.icon_only = b;
                        } else {
                            s.percentile = b;
                        }
                    })
                })
            }
            _ => return None,
        };
        Some(r)
    }

    pub(crate) fn cf_part_member(
        &mut self,
        book: BookId,
        sheet: SheetId,
        id: u32,
        part: CfPart,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let rule = self.rule_of(book, sheet, id)?;
        let put = || Application::put_value(a);
        let alloc = |s: &mut Self, p: CfPart| Ok(s.alloc(Object::CfPart(book, sheet, id, p)));
        match part {
            CfPart::Point(max) => {
                let d = bar_of(&rule)?;
                let c = if max { &d.max } else { &d.min };
                match (m.name, m.kind) {
                    ("Type", Get) => Ok(i4(c.ty)),
                    ("Value", Get) => Ok(value_variant(c)),
                    ("Modify", Func) => {
                        let ty = a.i32(0)?.ok_or_else(OmError::app_defined)?;

                        let banned = if max { [1, 6] } else { [2, 7] };
                        if !(0..=7).contains(&ty) || banned.contains(&ty) {
                            return Err(OmError::app_defined());
                        }
                        let (mut value, mut formula) = (None, None);
                        match a.get(1) {
                            Some(v) if !crate::conv::is_missing(v) => {
                                if ty == 4 {
                                    formula = a.string(1)?;
                                } else {
                                    value = Some(a.f64(1)?.ok_or_else(OmError::app_defined)?);
                                }
                            }

                            _ => formula = Some("=".to_string()),
                        }
                        if matches!(ty, 3 | 5) && value.is_some_and(|v| !(0.0..=100.0).contains(&v))
                        {
                            return Err(invalid_arg());
                        }
                        self.bar_edit(book, sheet, id, |d| {
                            let c = if max { &mut d.max } else { &mut d.min };
                            c.ty = ty;
                            c.value = value.unwrap_or(if matches!(ty, 1 | 2 | 6 | 7) {
                                0.0
                            } else {
                                c.value
                            });
                            c.formula = formula;
                        })
                    }
                    _ => Err(unsupported("ConditionValue", m)),
                }
            }
            CfPart::BarColor
            | CfPart::AxisColor
            | CfPart::BorderColor
            | CfPart::NegColor
            | CfPart::NegBorderColor => {
                let d = bar_of(&rule)?;
                let c = color_of(d, part);
                match (m.name, m.kind) {
                    ("Color", Get) => Ok(i4(swap_rb(c.shown()) as i32)),
                    ("Color", Put) => {
                        let n = put().i32(0)?.ok_or_else(OmError::app_defined)?;
                        self.bar_edit(book, sheet, id, |d| {
                            *color_mut(d, part) = BarColor::new(swap_rb(n as u32 & 0xFF_FFFF));
                        })
                    }
                    ("ColorIndex", Get) => Ok(i4(i32::from(cv_color_index(c.shown())))),
                    ("ColorIndex", Put) => {
                        let n = put().i32(0)?.ok_or_else(OmError::app_defined)?;
                        let rgb = u8::try_from(n + 7)
                            .ok()
                            .filter(|_| (1..=56).contains(&n))
                            .and_then(indexed_rgb)
                            .ok_or_else(OmError::app_defined)?;
                        self.bar_edit(book, sheet, id, |d| {
                            *color_mut(d, part) = BarColor::new(rgb);
                        })
                    }
                    ("TintAndShade", Get) => Ok(Variant::R8(c.tint)),
                    ("TintAndShade", Put) => {
                        let t = put().f64(0)?.ok_or_else(OmError::app_defined)?;
                        if !(-1.0..=1.0).contains(&t) {
                            return Err(invalid_arg());
                        }
                        self.bar_edit(book, sheet, id, |d| {
                            color_mut(d, part).tint = stored_tint(t);
                        })
                    }
                    _ => Err(unsupported("FormatColor", m)),
                }
            }
            CfPart::BarBorder => {
                let d = bar_of(&rule)?;
                match (m.name, m.kind) {
                    ("Type", Get) => Ok(i4(d.border_type)),
                    ("Type", Put) => {
                        let n = put().i32(0)?.filter(|n| (0..=1).contains(n));
                        let n = n.ok_or_else(invalid_arg)?;
                        self.bar_edit(book, sheet, id, |d| d.border_type = n)
                    }
                    ("Color", Get) => alloc(self, CfPart::BorderColor),
                    _ => Err(unsupported("DataBarBorder", m)),
                }
            }
            CfPart::NegFormat => {
                let d = bar_of(&rule)?;
                match (m.name, m.kind) {
                    ("ColorType", Get) => Ok(i4(d.neg_color_type)),
                    ("BorderColorType", Get) => Ok(i4(d.neg_border_type)),
                    ("ColorType" | "BorderColorType", Put) => {
                        let n = put().i32(0)?.filter(|n| (0..=1).contains(n));
                        let n = n.ok_or_else(invalid_arg)?;
                        self.bar_edit(book, sheet, id, |d| {
                            if m.name == "ColorType" {
                                d.neg_color_type = n;
                            } else {
                                d.neg_border_type = n;
                            }
                        })
                    }
                    ("Color", Get) => alloc(self, CfPart::NegColor),
                    ("BorderColor", Get) => alloc(self, CfPart::NegBorderColor),
                    _ => Err(unsupported("NegativeBarFormat", m)),
                }
            }
            CfPart::IconSet => {
                let s = icons_of(&rule)?;
                match (m.name, m.kind) {
                    ("ID", Get) => Ok(i4(s.id)),
                    ("Count", Get) => Ok(i4(s.crits.len() as i32)),
                    _ => Err(unsupported("IconSet", m)),
                }
            }
            CfPart::IconCriteria => {
                let s = icons_of(&rule)?;
                let n = s.crits.len();
                match (m.name, m.kind) {
                    ("Count", Get) => Ok(i4(n as i32)),
                    ("Item" | "_Default", _) => {
                        let i = a.i32(0)?.ok_or_else(OmError::app_defined)?;
                        if i < 1 || i as usize > n {
                            return Err(OmError::bad_index());
                        }
                        alloc(self, CfPart::IconCriterion(i as usize - 1))
                    }
                    ("_NewEnum", Get) => {
                        let items = (0..n)
                            .map(|k| {
                                self.alloc(Object::CfPart(
                                    book,
                                    sheet,
                                    id,
                                    CfPart::IconCriterion(k),
                                ))
                            })
                            .collect();
                        Ok(self.alloc_enumerator(items))
                    }
                    _ => Err(unsupported("IconCriteria", m)),
                }
            }
            CfPart::IconCriterion(k) => {
                let s = icons_of(&rule)?;
                let c = s.crits.get(k).ok_or_else(OmError::bad_index)?;
                let edit = |app: &mut Self, f: CritEdit| {
                    app.icons_edit(book, sheet, id, |s| {
                        if let Some(c) = s.crits.get_mut(k) {
                            f(c, false);
                        }
                    })
                };
                match (m.name, m.kind) {
                    ("Index", Get) => Ok(i4(k as i32 + 1)),
                    ("Type", Get) => Ok(i4(c.ty)),
                    ("Value", Get) => Ok(Variant::R8(c.value)),
                    ("Operator", Get) => Ok(i4(c.op)),
                    ("Icon", Get) => Ok(i4(c.icon)),
                    ("Type", Put) => {
                        let t = put().i32(0)?.ok_or_else(OmError::app_defined)?;

                        if k == 0 || !(0..=5).contains(&t) {
                            return Err(OmError::app_defined());
                        }
                        edit(
                            self,
                            Box::new(move |c, _| {
                                c.ty = t;
                                c.value = 0.0;
                            }),
                        )
                    }
                    ("Value", Put) => {
                        let v = put().f64(0)?.ok_or_else(OmError::app_defined)?;
                        edit(self, Box::new(move |c, _| c.value = v))
                    }
                    ("Operator", Put) => {
                        let o = put().i32(0)?.ok_or_else(OmError::app_defined)?;
                        if !matches!(o, 5 | 7) {
                            return Err(OmError::app_defined());
                        }
                        edit(self, Box::new(move |c, _| c.op = o))
                    }
                    ("Icon", Put) => {
                        let i = put().i32(0)?.ok_or_else(OmError::app_defined)?;
                        if !(-1..=52).contains(&i) {
                            return Err(OmError::app_defined());
                        }
                        self.icons_edit(book, sheet, id, |s| {
                            if let Some(c) = s.crits.get_mut(k) {
                                c.icon = i;
                            }
                            s.custom = true;
                        })
                    }
                    _ => Err(unsupported("IconCriterion", m)),
                }
            }
        }
    }

    pub(crate) fn icon_sets_member(
        &mut self,
        book: BookId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("Count", Get) => Ok(i4(20)),
            ("Item" | "_Default", _) => {
                let i = a.i32(0)?.ok_or_else(OmError::app_defined)?;
                if !(1..=20).contains(&i) {
                    return Err(OmError::bad_index());
                }
                Ok(self.alloc(Object::IconSetDef(book, i)))
            }
            ("_NewEnum", Get) => {
                let items = (1..=20)
                    .map(|k| self.alloc(Object::IconSetDef(book, k)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            _ => Err(unsupported("IconSets", m)),
        }
    }

    pub(crate) fn icon_set_def_member(
        &mut self,
        id: i32,
        m: &'static Member,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        match (m.name, m.kind) {
            ("ID", Get) => Ok(i4(id)),
            ("Count", Get) => Ok(i4(
                IconSetSpec::new(id, false).map_or(0, |s| s.crits.len() as i32)
            )),
            _ => Err(unsupported("IconSet", m)),
        }
    }
}
