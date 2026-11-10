use xlcalc::pivot::{
    display_order, Ax, DataField, Filter, Form, Func, Orient, PivotCache, PivotTable, Sort,
    OPT_BOOLS,
};
use xlcalc::{Rect, SheetId, Value, Workbook};
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application, BookId, Mode, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum FieldsKind {
    All,
    Row,
    Column,
    Page,
    Data,
    Hidden,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum FieldRef {
    Src(usize),
    Data(usize),
    Values,
}

#[derive(Clone, Debug)]
pub(crate) enum PivotRef {
    Caches(BookId),
    Cache(BookId, usize),
    Tables(BookId, SheetId),
    Table(BookId, String),
    Fields(BookId, String, FieldsKind),
    Field(BookId, String, FieldRef),

    Items(BookId, String, usize),

    Item(BookId, String, usize, u32),

    Filters(BookId, String, usize),

    CalcFields(BookId, String),

    CalcItems(BookId, String, usize),

    Filter(BookId, String, usize),

    Style(BookId, String),
}

impl PivotRef {
    pub(crate) fn interface_name(&self) -> &'static str {
        match self {
            PivotRef::Caches(_) => "PivotCaches",
            PivotRef::Cache(..) => "PivotCache",
            PivotRef::Tables(..) => "PivotTables",
            PivotRef::Table(..) => "PivotTable",
            PivotRef::Fields(..) => "PivotFields",
            PivotRef::Field(..) => "PivotField",
            PivotRef::Items(..) => "PivotItems",
            PivotRef::Item(..) => "PivotItem",
            PivotRef::Filters(..) => "PivotFilters",
            PivotRef::CalcFields(..) => "CalculatedFields",
            PivotRef::CalcItems(..) => "CalculatedItems",
            PivotRef::Filter(..) => "PivotFilter",
            PivotRef::Style(..) => "TableStyle",
        }
    }

    pub(crate) fn book(&self) -> BookId {
        match self {
            PivotRef::Caches(b)
            | PivotRef::Cache(b, _)
            | PivotRef::Tables(b, _)
            | PivotRef::Table(b, _)
            | PivotRef::Fields(b, ..)
            | PivotRef::Field(b, ..)
            | PivotRef::Items(b, ..)
            | PivotRef::Item(b, ..)
            | PivotRef::Filters(b, ..)
            | PivotRef::CalcFields(b, ..)
            | PivotRef::CalcItems(b, ..)
            | PivotRef::Filter(b, ..)
            | PivotRef::Style(b, _) => *b,
        }
    }
}

fn remap_table_fields(t: &mut xlcalc::pivot::PivotTable, map: &[Option<usize>], nnew: usize) {
    let mut fields = vec![xlcalc::pivot::PtField::default(); nnew];
    for (o, n) in map.iter().enumerate() {
        if let (Some(n), Some(f)) = (n, t.fields.get(o)) {
            fields[*n] = f.clone();
        }
    }
    let ax = |v: &[Ax]| -> Vec<Ax> {
        v.iter()
            .filter_map(|a| match a {
                Ax::Values => Some(Ax::Values),
                Ax::Field(f) => map.get(*f).copied().flatten().map(Ax::Field),
            })
            .collect()
    };
    t.rows = ax(&t.rows);
    t.cols = ax(&t.cols);
    t.pages = t
        .pages
        .iter()
        .filter_map(|f| map.get(*f).copied().flatten())
        .collect();
    t.data = t
        .data
        .iter()
        .filter_map(|d| {
            let mut d = d.clone();
            d.src = map.get(d.src).copied().flatten()?;
            Some(d)
        })
        .collect();
    t.fields = fields;
}

fn unable(property: &str, class: &str) -> OmError {
    OmError::app_defined_with(&format!(
        "Unable to get the {property} property of the {class} class"
    ))
}

fn invalid_arg() -> OmError {
    OmError::exception(0x8007_0057_u32 as i32, None)
}

pub(crate) fn parse_r1c1(wb: &Workbook, default: SheetId, text: &str) -> Option<(SheetId, Rect)> {
    let text = text.trim();
    let (sheet, body) = match text.rfind('!') {
        Some(i) => {
            let name = text[..i].trim_matches('\'').replace("''", "'");
            (wb.sheet_by_name(&name)?.id, &text[i + 1..])
        }
        None => (default, text),
    };
    let one = |s: &str| -> Option<(u32, u32)> {
        let s = s.strip_prefix(['R', 'r'])?;
        let (r, c) = s.split_once(['C', 'c'])?;
        let r: u32 = r.parse().ok()?;
        let c: u32 = c.parse().ok()?;
        (r >= 1 && c >= 1).then(|| (r - 1, c - 1))
    };
    let mut parts = body.split(':');
    if let Some((r0, c0)) = parts.next().and_then(one) {
        let (r1, c1) = match parts.next() {
            Some(p) => one(p)?,
            None => (r0, c0),
        };
        return Some((sheet, Rect::new(r0, c0, r1, c1)));
    }
    crate::addr::parse_range_text(wb, default, text)
}

pub(crate) fn resolve_source(
    wb: &Workbook,
    default: SheetId,
    s: &str,
) -> Option<(String, SheetId, Rect, bool)> {
    let t = s.trim();
    if let Some(tb) = wb.table_by_name(t) {
        let mut rect = tb.rect;
        if tb.totals && rect.r1 > rect.r0 {
            rect.r1 -= 1;
        }
        return Some((t.to_string(), tb.sheet, rect, true));
    }
    let (sheet, body) = match t.rfind('!') {
        Some(i) => {
            let name = t[..i].trim_matches('\'').replace("''", "'");
            (wb.sheet_by_name(&name)?.id, &t[i + 1..])
        }
        None => (default, t),
    };
    if !body.contains(':') {
        if let Some(def) = wb.names().lookup(&body.to_uppercase(), sheet) {
            let target = def.refers_to.trim_start_matches('=').to_string();
            let (sh, rect) = parse_r1c1(wb, sheet, &target)?;
            return Some((t.to_string(), sh, rect, true));
        }
    }
    let (sh, rect) = parse_r1c1(wb, default, t)?;
    let name = wb.sheet(sh).map(|x| crate::addr::quote_sheet(&x.name))?;
    let text = if rect.is_full_columns() {
        format!("{name}!C{}:C{}", rect.c0 + 1, rect.c1 + 1)
    } else if (rect.r0, rect.c0) == (rect.r1, rect.c1) {
        format!("{name}!R{}C{}", rect.r0 + 1, rect.c0 + 1)
    } else {
        format!(
            "{name}!R{}C{}:R{}C{}",
            rect.r0 + 1,
            rect.c0 + 1,
            rect.r1 + 1,
            rect.c1 + 1
        )
    };
    Some((text, sh, rect, false))
}

fn func_name_default(src: &str, func: Func) -> String {
    format!("{} of {}", func.word(), src)
}

impl Application<'_> {
    fn table_idx(&self, b: BookId, name: &str) -> Result<usize, OmError> {
        self.book(b)?
            .wb
            .pivots()
            .tables
            .iter()
            .position(|t| t.name.eq_ignore_ascii_case(name))
            .ok_or_else(OmError::object_required)
    }

    fn pt(&self, b: BookId, name: &str) -> Result<PivotTable, OmError> {
        let i = self.table_idx(b, name)?;
        Ok(self.book(b)?.wb.pivots().tables[i].clone())
    }

    fn cache_of(&self, b: BookId, t: &PivotTable) -> Result<PivotCache, OmError> {
        self.book(b)?
            .wb
            .pivots()
            .caches
            .get(t.cache)
            .map(|c| {
                if t.fields.iter().any(|f| f.group.is_some()) {
                    c.grouped(t)
                } else {
                    c.clone()
                }
            })
            .ok_or_else(OmError::object_required)
    }

    fn pt_commit(&mut self, b: BookId, i: usize, t: PivotTable) -> Result<(), OmError> {
        let wb = &mut self.book_mut(b)?.wb;
        wb.pivots_mut().tables[i] = t;
        wb.pivot_update(i).map_err(crate::objects::edit_error)
    }

    fn pt_commit_typed(&mut self, b: BookId, i: usize, t: PivotTable) -> Result<(), OmError> {
        let wb = &mut self.book_mut(b)?.wb;
        wb.pivots_mut().tables[i] = t;
        wb.pivot_update_with(i, false)
            .map_err(crate::objects::edit_error)
    }

    pub(crate) fn pivot_intercept(
        &mut self,
        obj: &Object,
        m: &'static Member,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        match (obj, m.name) {
            (Object::Workbook(b), "PivotCaches") => {
                Some(Ok(self.alloc(Object::Pivot(PivotRef::Caches(*b)))))
            }
            (Object::Worksheet(b, s), "PivotTables") => {
                let tables = PivotRef::Tables(*b, *s);
                let v = self.alloc(Object::Pivot(tables));
                match a.get(0) {
                    None => Some(Ok(v)),
                    Some(i) => Some(self.pivot_tables_item(*b, *s, i, a.lcid)),
                }
            }
            (Object::Range(r), "Group" | "Ungroup") => self.pivot_range_group(r, m.name, a),
            (Object::Range(r), "Sort") => self.pivot_range_sort(r, a),
            (Object::Range(r), "Insert") => self.pivot_range_insert(r),
            (Object::Range(r), "Value" | "Value2" | "_Default") if m.kind == InvokeKind::Put => {
                self.pivot_range_type(r, a)
            }
            _ => None,
        }
    }

    fn pivot_range_type(
        &mut self,
        r: &crate::app::RangeRef,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        use xlcalc::pivot::CellRole;
        let (sheet, rect) = self.resolve(r).ok()?;
        if rect.r0 != rect.r1 || rect.c0 != rect.c1 {
            return None;
        }
        let b = r.book;
        let wb = &self.book(b).ok()?.wb;
        let role = wb.pivot_cell_role(xlcalc::CellAddr::new(sheet, rect.r0, rect.c0))?;
        let new = match &a.put {
            Some(Variant::Bstr(s)) if !s.starts_with('=') => s.clone(),
            Some(Variant::R8(v)) => xlcalc::pivot::number_label(*v),
            Some(Variant::I4(v)) => v.to_string(),
            _ => return None,
        };
        let ti = match &role {
            CellRole::Label(ti, ..) | CellRole::Header(ti, _) => *ti,
        };
        let mut t = wb.pivots().tables[ti].clone();
        let c = wb.pivots().caches[t.cache].grouped(&t);
        let done: Result<(), OmError> = match role {
            CellRole::Label(_, f, Some(it)) => {
                let total = c.fields[f].items.len() + t.fields[f].calc_items.len();
                let other = (0..total as u32).find(|&k| {
                    k != it && Self::item_label(&t, &c, f, k).eq_ignore_ascii_case(&new)
                });
                match other {
                    Some(o) => {
                        let mut order = display_order(&t, &c, f);
                        order.retain(|x| *x != o);
                        let at = order.iter().position(|x| *x == it).unwrap_or(0);
                        order.insert(at, o);
                        t.fields[f].sort = Sort::Manual;
                        t.fields[f].manual = Some(order);
                        Ok(())
                    }
                    None => Self::rename_item(&mut t, &c, f, it, &new),
                }
            }
            CellRole::Label(_, _, None) => Err(OmError::app_defined()),
            CellRole::Header(_, 1) => {
                let clash =
                    (0..c.fields.len()).any(|k| t.field_name(&c, k).eq_ignore_ascii_case(&new));
                if t.data.len() != 1 || clash || new.is_empty() {
                    Err(invalid_arg())
                } else {
                    t.data[0].caption = new.clone();
                    Ok(())
                }
            }
            CellRole::Header(_, 2) => {
                t.row_caption = (new != "Row Labels").then(|| new.clone());
                Ok(())
            }
            CellRole::Header(..) => {
                t.col_caption = (new != "Column Labels").then(|| new.clone());
                Ok(())
            }
        };
        if let Err(e) = done {
            return Some(Err(e));
        }
        Some(self.pt_commit_typed(b, ti, t).map(|()| Variant::Empty))
    }

    fn pivot_range_insert(&mut self, r: &crate::app::RangeRef) -> Option<Result<Variant, OmError>> {
        use xlcalc::pivot::CellRole;
        let (sheet, rect) = self.resolve(r).ok()?;
        if rect.r0 == 0 || rect.c0 == 0 {
            return None;
        }
        let b = r.book;
        let wb = &self.book(b).ok()?.wb;

        let mut row = rect.r0;
        let role = loop {
            let at = xlcalc::CellAddr::new(sheet, row, rect.c0);
            if let Some(role) = wb.pivot_cell_role(at) {
                break role;
            }
            if row == 0 || wb.pivot_meeting(sheet, &Rect::cell(row, rect.c0)).is_none() {
                return None;
            }
            row -= 1;
        };
        let CellRole::Label(ti, f, item) = role else {
            return None;
        };
        let mut t = wb.pivots().tables[ti].clone();
        let c = wb.pivots().caches[t.cache].grouped(&t);
        let mut order = display_order(&t, &c, f);
        let at = item
            .and_then(|it| order.iter().position(|x| *x == it))
            .unwrap_or(order.len());
        let n = c.fields[f].items.len();
        for (j, _) in (rect.r0..=rect.r1).enumerate() {
            let mut k = t.fields[f].calc_items.len() + 1;
            let mut name = format!("Formula{k}");
            while (0..(n + t.fields[f].calc_items.len()) as u32)
                .any(|i| Self::item_label(&t, &c, f, i).eq_ignore_ascii_case(&name))
            {
                k += 1;
                name = format!("Formula{k}");
            }
            order.insert(at + j, (n + t.fields[f].calc_items.len()) as u32);
            t.fields[f].calc_items.push(xlcalc::pivot::CalcItem {
                name,
                formula: String::new(),
            });
        }
        t.fields[f].sort = Sort::Manual;
        t.fields[f].manual = Some(order);
        Some(self.pt_commit(b, ti, t).map(|()| Variant::Bool(true)))
    }

    fn pivot_range_sort(
        &mut self,
        r: &crate::app::RangeRef,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        let (sheet, rect) = self.resolve(r).ok()?;
        let b = r.book;
        let wb = &self.book(b).ok()?.wb;
        wb.pivot_meeting(sheet, &rect)?;
        let addr = xlcalc::CellAddr::new(sheet, rect.r0, rect.c0);
        let desc = matches!(a.get(1), Some(Variant::I4(2)));
        let Some((ti, f, order)) = wb.pivot_sort_order(addr, desc) else {
            return Some(Err(OmError::app_defined()));
        };
        let mut t = wb.pivots().tables[ti].clone();
        t.fields[f].sort = xlcalc::pivot::Sort::Manual;
        t.fields[f].manual = Some(order);
        Some(self.pt_commit(b, ti, t).map(|()| Variant::Bool(true)))
    }

    fn pivot_range_group(
        &mut self,
        r: &crate::app::RangeRef,
        which: &str,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        let (sheet, rect) = self.resolve(r).ok()?;
        let b = r.book;
        let addr = xlcalc::CellAddr::new(sheet, rect.r0, rect.c0);
        let wb = &self.book(b).ok()?.wb;
        wb.pivot_meeting(sheet, &rect)?;
        let Some((ti, f, Some(_))) = wb.pivot_label_at(addr) else {
            return Some(Err(OmError::app_defined()));
        };
        let mut t = wb.pivots().tables[ti].clone();
        let c = wb.pivots().caches[t.cache].grouped(&t);
        if which == "Ungroup" {
            let grouped_from = t.fields.iter().any(|pf| {
                matches!(&pf.group,
                    Some(xlcalc::pivot::Group::Manual { src, .. } | xlcalc::pivot::Group::Derived { src, .. })
                        if *src == f)
            });
            if t.fields[f].group.is_none() && !grouped_from {
                return Some(Err(OmError::app_defined()));
            }

            let manual = (0..t.fields.len()).find(|&g| {
                t.fields[f].group.is_none()
                    && matches!(&t.fields[g].group,
                        Some(xlcalc::pivot::Group::Manual { src, .. }) if *src == f)
            });
            t.remove_group(manual.unwrap_or(f));
        } else {
            if rect.r0 != rect.r1 || rect.c0 != rect.c1 {
                if matches!(
                    t.fields[f].group,
                    Some(xlcalc::pivot::Group::Num { .. } | xlcalc::pivot::Group::Derived { .. })
                ) {
                    return Some(Ok(Variant::Bool(true)));
                }

                let mut members = Vec::new();
                for row in rect.r0..=rect.r1 {
                    for col in rect.c0..=rect.c1 {
                        match wb.pivot_label_at(xlcalc::CellAddr::new(sheet, row, col)) {
                            Some((_, g, Some(i))) if g == f => {
                                members.push(c.fields[f].items[i as usize].label.clone())
                            }
                            _ => {
                                return Some(Err(OmError::app_defined_with(
                                    "Cannot group that selection.",
                                )))
                            }
                        }
                    }
                }
                let mut k = 1;
                let used = |k: usize, t: &PivotTable| {
                    t.fields.iter().any(|pf| {
                        matches!(&pf.group, Some(xlcalc::pivot::Group::Manual { groups, .. })
                            if groups.iter().any(|g| g.0 == format!("Group{k}")))
                    })
                };
                while used(k, &t) {
                    k += 1;
                }
                t.add_manual_group(f, &format!("Group{k}"), members);
                return Some(self.pt_commit(b, ti, t).map(|()| Variant::Bool(true)));
            }
            if let Some(Variant::Array(arr)) = a.get(3) {
                let flags: Vec<bool> = arr
                    .data
                    .iter()
                    .map(|v| matches!(v, Variant::Bool(true)))
                    .collect();
                let on: Vec<usize> = (0..flags.len()).filter(|&i| flags[i]).collect();
                let opt = |k: usize| match a.get(k) {
                    Some(Variant::I4(v)) => Some(f64::from(*v)),
                    Some(Variant::I2(v)) => Some(f64::from(*v)),
                    Some(Variant::R8(v)) => Some(*v),
                    Some(Variant::Date(v)) => Some(*v),
                    _ => None,
                };
                use xlcalc::pivot::DateUnit;

                let mut f = f;
                if let Some(xlcalc::pivot::Group::Derived { src, .. }) = t.fields[f].group {
                    t.remove_group(f);
                    f = src;
                }
                let units: Vec<DateUnit> = match on.as_slice() {
                    [] => return Some(Err(OmError::app_defined())),
                    _ if on.iter().any(|&i| !(3..=6).contains(&i)) => {
                        return Some(Err(OmError::app_defined()))
                    }
                    _ => on
                        .iter()
                        .map(|&i| match i {
                            3 => {
                                DateUnit::Days(opt(2).filter(|v| *v >= 1.0).map_or(1, |v| v as u32))
                            }
                            4 => DateUnit::Month,
                            5 => DateUnit::Quarter,
                            _ => DateUnit::Year,
                        })
                        .collect(),
                };
                t.add_date_levels(f, &units, opt(0), opt(1));
                return Some(self.pt_commit(b, ti, t).map(|()| Variant::Bool(true)));
            }
            let nums: Vec<f64> = wb.pivots().caches[t.cache].fields[f]
                .items
                .iter()
                .filter_map(|i| match &i.value {
                    Value::Number(n) => Some(*n),
                    _ => None,
                })
                .collect();
            if nums.is_empty() {
                return Some(Err(OmError::app_defined()));
            }

            if wb.pivots().caches[t.cache].fields[f]
                .items
                .iter()
                .any(|i| matches!(i.value, Value::Text(_)))
            {
                return Some(Err(OmError::app_defined_with(
                    "Cannot group that selection.",
                )));
            }
            let num = |k: usize| match a.get(k) {
                Some(Variant::I4(v)) => Some(f64::from(*v)),
                Some(Variant::I2(v)) => Some(f64::from(*v)),
                Some(Variant::R8(v)) => Some(*v),
                _ => None,
            };
            let start = num(0);
            let end = num(1);
            let lo = start.unwrap_or_else(|| nums.iter().cloned().fold(f64::INFINITY, f64::min));
            let hi = end.unwrap_or_else(|| nums.iter().cloned().fold(f64::NEG_INFINITY, f64::max));

            let no_args = (0..3).all(|k| a.get(k).is_none());
            if no_args && matches!(t.fields[f].group, Some(xlcalc::pivot::Group::Num { .. })) {
                return Some(Ok(Variant::Bool(true)));
            }
            let auto_by = no_args && hi > lo;
            let Some(by) = (if auto_by {
                Some(10f64.powf(((hi - lo).log10().ceil() - 1.0).max(0.0)))
            } else {
                num(2).filter(|v| *v > 0.0)
            }) else {
                return Some(Err(OmError::app_defined()));
            };
            if hi < lo {
                return Some(Err(OmError::app_defined()));
            }
            t.fields[f].group = Some(xlcalc::pivot::Group::Num { start, end, by });
        }
        Some(self.pt_commit(b, ti, t).map(|()| Variant::Bool(true)))
    }

    fn pv_tables_on(&self, b: BookId, s: SheetId) -> Result<Vec<String>, OmError> {
        Ok(self
            .book(b)?
            .wb
            .pivots()
            .tables
            .iter()
            .filter(|t| t.sheet == s)
            .map(|t| t.name.clone())
            .collect())
    }

    fn pivot_tables_item(
        &mut self,
        b: BookId,
        s: SheetId,
        idx: &Variant,
        lcid: u32,
    ) -> Result<Variant, OmError> {
        let names = self.pv_tables_on(b, s)?;
        let i = self
            .item_index(Some(idx), lcid, &names, |n| n.as_str())
            .map_err(|_| unable("PivotTables", "Worksheet"))?;
        Ok(self.alloc(Object::Pivot(PivotRef::Table(b, names[i].clone()))))
    }

    pub(crate) fn pivot_member(
        &mut self,
        r: &PivotRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match r {
            PivotRef::Caches(b) => self.caches_member(*b, m, a),
            PivotRef::Cache(b, i) => self.cache_member(*b, *i, m, a),
            PivotRef::Tables(b, s) => self.tables_member(*b, *s, m, a),
            PivotRef::Table(b, n) => self.table_member(*b, n, m, a),
            PivotRef::Fields(b, n, k) => self.fields_member(*b, n, *k, m, a),
            PivotRef::Field(b, n, f) => self.field_member(*b, n, *f, m, a),
            PivotRef::Items(b, n, s) => self.items_member(*b, n, *s, m, a),
            PivotRef::Item(b, n, s, it) => self.item_member(*b, n, *s, *it, m, a),
            PivotRef::Filters(b, n, s) => self.filters_member(*b, n, *s, m, a),
            PivotRef::CalcFields(b, n) => self.calc_fields_member(*b, n, m, a),
            PivotRef::CalcItems(b, n, s) => self.calc_items_member(*b, n, *s, m, a),
            PivotRef::Filter(b, n, s) => self.filter_member(*b, n, *s, m, a),
            PivotRef::Style(b, n) => {
                let t = self.pt(*b, n)?;
                match m.name {
                    "Name" | "NameLocal" => Ok(Variant::Bstr(t.style.clone())),
                    _ => Err(unsupported("TableStyle", m)),
                }
            }
        }
    }

    fn source_arg(
        &self,
        b: BookId,
        v: Option<&Variant>,
    ) -> Result<(String, SheetId, Rect), OmError> {
        let wb = &self.book(b)?.wb;
        match v {
            Some(Variant::Bstr(s)) => {
                let (text, sh, rect, _) =
                    resolve_source(wb, wb.active_sheet(), s).ok_or_else(invalid_arg)?;
                Ok((text, sh, rect))
            }
            Some(Variant::Dispatch(Some(_))) => {
                let (sh, rect) = self.range_arg(v)?;
                let name = wb.sheet(sh).map(|s| s.name.clone()).unwrap_or_default();
                let text = format!(
                    "{name}!R{}C{}:R{}C{}",
                    rect.r0 + 1,
                    rect.c0 + 1,
                    rect.r1 + 1,
                    rect.c1 + 1
                );
                Ok((text, sh, rect))
            }
            _ => Err(invalid_arg()),
        }
    }

    fn caches_member(
        &mut self,
        b: BookId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match m.name {
            "Count" => Ok(Variant::I4(self.book(b)?.wb.pivots().caches.len() as i32)),
            "Item" | "_Default" => {
                let n = self.book(b)?.wb.pivots().caches.len();
                match a.i32(0)? {
                    Some(i) if i >= 1 && (i as usize) <= n => {
                        Ok(self.alloc(Object::Pivot(PivotRef::Cache(b, i as usize - 1))))
                    }
                    _ => Err(OmError::bad_index()),
                }
            }
            "Create" => {
                if a.i32(0)? != Some(1) {
                    return Err(unsupported("PivotCaches", m));
                }
                let (text, sheet, rect) = self.source_arg(b, a.get(1))?;
                let cache = {
                    let wb = &self.book(b)?.wb;
                    PivotCache::read(wb, &text, sheet, rect).ok_or_else(invalid_arg)?
                };
                let pv = self.book_mut(b)?.wb.pivots_mut();
                pv.caches.push(cache);
                let i = pv.caches.len() - 1;
                Ok(self.alloc(Object::Pivot(PivotRef::Cache(b, i))))
            }
            _ => Err(unsupported("PivotCaches", m)),
        }
    }

    fn make_table(
        &mut self,
        b: BookId,
        cache: usize,
        dest: Option<&Variant>,
        name: Option<String>,
    ) -> Result<String, OmError> {
        let (sheet, rect) = match dest {
            Some(Variant::Bstr(s)) => {
                let wb = &self.book(b)?.wb;
                parse_r1c1(wb, wb.active_sheet(), s).ok_or_else(invalid_arg)?
            }
            Some(Variant::Dispatch(Some(_))) => self.range_arg(dest)?,
            _ => return Err(invalid_arg()),
        };
        {
            let wb = &self.book(b)?.wb;
            let at = Rect::new(rect.r0, rect.c0, rect.r0, rect.c0);
            if let Some(k) = wb.pivot_meeting(sheet, &at) {
                let other = &wb.pivots().tables[k].name;
                return Err(OmError::app_defined_with(&format!(
                    "We couldn't insert your PivotTable at the selected location in the sheet \"{}\" because there's already a PivotTable \"{other}\" there. Make space and try again.",
                    wb.sheet(sheet).map(|s| s.name.to_string()).unwrap_or_default()
                )));
            }
        }
        let (nfields, taken): (usize, Vec<String>) = {
            let pv = self.book(b)?.wb.pivots();
            (
                pv.caches[cache].fields.len(),
                pv.tables.iter().map(|t| t.name.clone()).collect(),
            )
        };
        let name = match name {
            Some(n) if !n.is_empty() => {
                if taken.iter().any(|t| t.eq_ignore_ascii_case(&n)) {
                    return Err(invalid_arg());
                }
                n
            }
            _ => {
                let mut k = 1;
                loop {
                    let n = format!("PivotTable{k}");
                    if !taken.iter().any(|t| t.eq_ignore_ascii_case(&n)) {
                        break n;
                    }
                    k += 1;
                }
            }
        };
        let t = PivotTable::new(&name, cache, nfields, sheet, rect.r0, rect.c0);
        let wb = &mut self.book_mut(b)?.wb;
        wb.pivots_mut().tables.push(t);
        let i = wb.pivots().tables.len() - 1;
        wb.pivot_update(i).map_err(crate::objects::edit_error)?;
        Ok(name)
    }

    fn cache_member(
        &mut self,
        b: BookId,
        i: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        if self
            .book(b)?
            .wb
            .pivots()
            .caches
            .get(i)
            .is_none_or(|c| c.orphaned)
        {
            return Err(OmError::object_required());
        }
        match (m.name, m.kind) {
            ("CreatePivotTable", _) => {
                let name = a.string(1)?;
                let n = self.make_table(b, i, a.get(0), name)?;
                Ok(self.alloc(Object::Pivot(PivotRef::Table(b, n))))
            }
            ("Index", Get) => Ok(Variant::I4(i as i32 + 1)),
            ("SourceData", Get) => {
                let pv = self.book(b)?.wb.pivots();
                Ok(Variant::Bstr(pv.caches[i].source.clone()))
            }
            ("SourceData", Put) => {
                self.set_cache_source(b, i, a.put.as_ref())?;
                Ok(Variant::Empty)
            }
            ("MissingItemsLimit", Get) => Ok(Variant::I4(
                self.book(b)?.wb.pivots().caches[i].missing_limit,
            )),
            ("MissingItemsLimit", Put) => {
                let v = self.put_i32(a)?;
                if !(-1..=32500).contains(&v) {
                    return Err(invalid_arg());
                }
                self.book_mut(b)?.wb.pivots_mut().caches[i].missing_limit = v;
                Ok(Variant::Empty)
            }
            ("RefreshOnFileOpen", Get) => Ok(Variant::Bool(
                self.book(b)?.wb.pivots().caches[i].refresh_open,
            )),
            ("RefreshOnFileOpen", Put) => {
                let v = self.put_bool(a)?;
                self.book_mut(b)?.wb.pivots_mut().caches[i].refresh_open = v;
                Ok(Variant::Empty)
            }
            ("SourceType", Get) => Ok(Variant::I4(1)),
            ("Version", Get) => Ok(Variant::I4(8)),
            ("RecordCount", Get) => {
                let pv = self.book(b)?.wb.pivots();
                Ok(Variant::I4(pv.caches[i].records.len() as i32))
            }
            ("Refresh", _) => {
                self.refresh_cache(b, i)?;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("PivotCache", m)),
        }
    }

    fn set_cache_source(
        &mut self,
        b: BookId,
        i: usize,
        v: Option<&Variant>,
    ) -> Result<(), OmError> {
        let (text, sheet, rect) = self.source_arg(b, v)?;
        {
            let c = &mut self.book_mut(b)?.wb.pivots_mut().caches[i];
            c.source = text;
            c.sheet = sheet;
            c.rect = rect;
        }
        self.refresh_cache(b, i)
    }

    fn change_cache(&mut self, b: BookId, ti: usize, new: usize) -> Result<(), OmError> {
        let wb = &mut self.book_mut(b)?.wb;
        let pv = wb.pivots_mut();
        let old = pv.tables[ti].cache;
        if old != new {
            let map: Vec<Option<usize>> = pv.caches[old]
                .fields
                .iter()
                .map(|f| {
                    pv.caches[new]
                        .fields
                        .iter()
                        .position(|g| g.name.eq_ignore_ascii_case(&f.name))
                })
                .collect();
            let mut t = pv.tables[ti].clone();
            remap_table_fields(&mut t, &map, pv.caches[new].fields.len());
            t.cache = new;
            pv.tables[ti] = t;
            if !pv.tables.iter().any(|t| t.cache == old) {
                pv.caches[old].orphaned = true;
            }
        }
        wb.pivot_update(ti).map_err(crate::objects::edit_error)?;
        self.refresh_cache(b, new)
    }

    fn refresh_cache(&mut self, b: BookId, i: usize) -> Result<(), OmError> {
        let new = {
            let wb = &self.book(b)?.wb;
            let c = &wb.pivots().caches[i];

            let (sheet, rect) = match resolve_source(wb, c.sheet, &c.source) {
                Some((_, sh, r, true)) => (sh, r),
                _ => (c.sheet, c.rect),
            };
            let mut new =
                PivotCache::read(wb, &c.source, sheet, rect).ok_or_else(OmError::app_defined)?;

            let ranged: Vec<usize> = wb
                .pivots()
                .tables
                .iter()
                .filter(|t| t.cache == i)
                .flat_map(|t| {
                    t.fields
                        .iter()
                        .enumerate()
                        .filter_map(|(f, pf)| match pf.group {
                            Some(xlcalc::pivot::Group::Num { .. }) => Some(f),
                            Some(xlcalc::pivot::Group::Derived { src, .. }) => Some(src),
                            _ => None,
                        })
                })
                .collect();
            new.keep_item_order(c, &ranged);
            new.keep_calcs(c);
            new
        };
        let wb = &mut self.book_mut(b)?.wb;

        let orders: Vec<(usize, usize, Vec<String>)> = {
            let pv = wb.pivots();
            let mut v = Vec::new();
            for (k, t) in pv.tables.iter().enumerate().filter(|(_, t)| t.cache == i) {
                let og = pv.caches[i].grouped(t);
                for (f, pf) in t.fields.iter().enumerate() {
                    let year = matches!(
                        pf.group,
                        Some(xlcalc::pivot::Group::Derived {
                            unit: xlcalc::pivot::DateUnit::Year,
                            ..
                        })
                    );
                    if let (true, Some(cf)) = (year, og.fields.get(f)) {
                        v.push((k, f, cf.items.iter().map(|x| x.label.clone()).collect()));
                    }
                }
            }
            v
        };
        for (k, f, o) in orders {
            wb.pivots_mut().tables[k].fields[f].group_order = o;
        }

        {
            let pv = wb.pivots_mut();
            let old = &pv.caches[i];
            let map: Vec<Option<usize>> = old
                .fields
                .iter()
                .map(|f| {
                    new.fields
                        .iter()
                        .position(|g| g.name.eq_ignore_ascii_case(&f.name))
                })
                .collect();
            let same =
                map.len() == new.fields.len() && map.iter().enumerate().all(|(k, m)| *m == Some(k));
            if !same {
                let nnew = new.fields.len();
                for t in pv.tables.iter_mut().filter(|t| t.cache == i) {
                    if t.fields.len() == map.len() {
                        remap_table_fields(t, &map, nnew);
                    }
                }
            }
        }
        wb.pivots_mut().caches[i] = new;
        let tables: Vec<usize> = wb
            .pivots()
            .tables
            .iter()
            .enumerate()
            .filter(|(_, t)| t.cache == i)
            .map(|(k, _)| k)
            .collect();
        for k in tables {
            let t = &mut wb.pivots_mut().tables[k];
            for f in 0..t.fields.len() {
                let on = t.rows.contains(&xlcalc::pivot::Ax::Field(f))
                    || t.cols.contains(&xlcalc::pivot::Ax::Field(f))
                    || t.pages.contains(&f);
                if !on {
                    t.fields[f].hidden.clear();
                }
            }
            wb.pivot_update(k).map_err(crate::objects::edit_error)?;
        }
        Ok(())
    }

    fn tables_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let names = self.pv_tables_on(b, s)?;
        match m.name {
            "Count" => Ok(Variant::I4(names.len() as i32)),
            "Item" | "_Default" => match a.get(0) {
                Some(i) => self.pivot_tables_item(b, s, &i.clone(), a.lcid),
                None => Err(OmError::bad_index()),
            },
            "_NewEnum" => {
                let items = names
                    .iter()
                    .map(|n| self.alloc(Object::Pivot(PivotRef::Table(b, n.clone()))))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            "Add" => {
                let cache = match a.get(0) {
                    Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                        Some(Object::Pivot(PivotRef::Cache(_, c))) => *c,
                        _ => return Err(invalid_arg()),
                    },
                    _ => return Err(invalid_arg()),
                };
                let name = a.string(2)?;
                let n = self.make_table(b, cache, a.get(1), name)?;
                Ok(self.alloc(Object::Pivot(PivotRef::Table(b, n))))
            }
            _ => Err(unsupported("PivotTables", m)),
        }
    }

    fn opt_range(
        &mut self,
        b: BookId,
        sheet: SheetId,
        r: Option<Rect>,
        what: &str,
    ) -> Result<Variant, OmError> {
        match r {
            Some(rect) => self.new_range(b, sheet, rect, Mode::Cells),
            None => Err(unable(what, "PivotTable")),
        }
    }

    fn fields_obj(&mut self, b: BookId, n: &str, k: FieldsKind) -> Variant {
        self.alloc(Object::Pivot(PivotRef::Fields(b, n.to_string(), k)))
    }

    fn table_member(
        &mut self,
        b: BookId,
        n: &str,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let i = self.table_idx(b, n)?;
        let mut t = self.pt(b, n)?;
        let cache = self.cache_of(b, &t)?;
        match (m.name, m.kind) {
            ("Name", Get) => Ok(Variant::Bstr(t.name.clone())),
            ("Name", Put) => {
                let new = match &a.put {
                    Some(Variant::Bstr(s)) if !s.is_empty() => s.clone(),
                    _ => return Err(OmError::unable_to_set("Name", "PivotTable")),
                };
                let clash = self
                    .book(b)?
                    .wb
                    .pivots()
                    .tables
                    .iter()
                    .enumerate()
                    .any(|(k, o)| k != i && o.name.eq_ignore_ascii_case(&new));
                if clash {
                    return Err(OmError::unable_to_set("Name", "PivotTable"));
                }
                let old = t.name.clone();
                t.name = new.clone();
                self.book_mut(b)?.wb.pivots_mut().tables[i] = t;
                self.arena.for_each_mut(|o| {
                    if let Object::Pivot(
                        PivotRef::Table(ob, nm)
                        | PivotRef::Fields(ob, nm, _)
                        | PivotRef::Field(ob, nm, _)
                        | PivotRef::Items(ob, nm, _)
                        | PivotRef::Item(ob, nm, _, _)
                        | PivotRef::Filters(ob, nm, _)
                        | PivotRef::CalcFields(ob, nm)
                        | PivotRef::CalcItems(ob, nm, _)
                        | PivotRef::Filter(ob, nm, _)
                        | PivotRef::Style(ob, nm),
                    ) = o
                    {
                        if *ob == b && nm.eq_ignore_ascii_case(&old) {
                            *nm = new.clone();
                        }
                    }
                });
                Ok(Variant::Empty)
            }
            ("TableRange1", Get) => {
                let r = t.ranges.table1;
                self.opt_range(b, t.sheet, r, m.name)
            }
            ("TableRange2", Get) => {
                let r = t.ranges.table2;
                self.opt_range(b, t.sheet, r, m.name)
            }
            ("RowRange", Get) => {
                let r = t.ranges.row;
                self.opt_range(b, t.sheet, r, m.name)
            }
            ("ColumnRange", Get) => {
                let r = t.ranges.column;
                self.opt_range(b, t.sheet, r, m.name)
            }
            ("DataBodyRange", Get) => {
                let r = t.ranges.data_body;
                self.opt_range(b, t.sheet, r, m.name)
            }
            ("DataLabelRange", Get) => {
                let r = t.ranges.data_label;
                self.opt_range(b, t.sheet, r, m.name)
            }
            ("PageRange", Get) => {
                let r = t.ranges.page;
                self.opt_range(b, t.sheet, r, m.name)
            }
            ("PivotFields", _)
            | ("RowFields", _)
            | ("ColumnFields", _)
            | ("PageFields", _)
            | ("DataFields", _)
            | ("HiddenFields", _)
            | ("VisibleFields", _) => {
                let k = match m.name {
                    "PivotFields" => FieldsKind::All,
                    "RowFields" => FieldsKind::Row,
                    "ColumnFields" => FieldsKind::Column,
                    "PageFields" => FieldsKind::Page,
                    "DataFields" => FieldsKind::Data,
                    "HiddenFields" => FieldsKind::Hidden,
                    _ => return Err(unsupported("PivotTable", m)),
                };
                let coll = self.fields_obj(b, n, k);
                match a.get(0) {
                    None => Ok(coll),
                    Some(idx) => {
                        let idx = idx.clone();
                        self.fields_item(b, &t, &cache, k, &idx, a.lcid)
                    }
                }
            }
            ("CalculatedFields", _) => {
                let coll = self.alloc(Object::Pivot(PivotRef::CalcFields(b, t.name.clone())));
                match a.get(0) {
                    None => Ok(coll),
                    Some(idx) => {
                        let idx = idx.clone();
                        self.calc_item(b, &t, &cache, &idx)
                    }
                }
            }
            ("DataPivotField", Get) => {
                if t.data.len() < 2 {
                    return Err(unable("DataPivotField", "PivotTable"));
                }
                Ok(self.alloc(Object::Pivot(PivotRef::Field(
                    b,
                    t.name.clone(),
                    FieldRef::Values,
                ))))
            }
            ("AddDataField", _) => {
                let field = match a.get(0) {
                    Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                        Some(Object::Pivot(PivotRef::Field(_, _, FieldRef::Src(f)))) => *f,
                        _ => return Err(invalid_arg()),
                    },
                    _ => return Err(invalid_arg()),
                };
                let func = match a.get(2) {
                    Some(Variant::I2(_) | Variant::I4(_)) => a
                        .i32(2)?
                        .and_then(xlcalc::pivot::Func::from_code)
                        .ok_or_else(OmError::bad_index)?,
                    Some(Variant::Error(_)) => default_func(&cache, field),
                    Some(_) => return Err(invalid_arg()),
                    None => default_func(&cache, field),
                };
                let src = cache.fields[field].name.clone();
                let explicit = match a.get(1) {
                    Some(Variant::Bstr(s)) => Some(s.clone()),
                    Some(Variant::Error(_)) | None => None,
                    Some(_) => return Err(invalid_arg()),
                };
                let taken = |c: &str| {
                    cache.fields.iter().any(|f| f.name.eq_ignore_ascii_case(c))
                        || t.data.iter().any(|d| d.caption.eq_ignore_ascii_case(c))
                };
                let caption = match explicit {
                    Some(s) => {
                        if taken(&s) {
                            return Err(OmError::app_defined());
                        }
                        s
                    }
                    None => {
                        let base = func_name_default(&src, func);
                        let same = t.data.iter().filter(|d| d.src == field).count();
                        let mut n = same + 1;
                        let mut c = if same == 0 {
                            base.clone()
                        } else {
                            format!("{base}{n}")
                        };
                        while taken(&c) {
                            n += 1;
                            c = format!("{base}{n}");
                        }
                        c
                    }
                };

                t.set_orient(field, Orient::Hidden);
                t.add_data(DataField {
                    src: field,
                    caption,
                    func,
                    numfmt: None,
                    show: Default::default(),
                });
                let pos = t.data.len() - 1;
                self.pt_commit(b, i, t.clone())?;
                Ok(self.alloc(Object::Pivot(PivotRef::Field(
                    b,
                    t.name.clone(),
                    FieldRef::Data(pos),
                ))))
            }
            ("RepeatAllLabels", _) => {
                let k = a.i32(0)?.ok_or_else(invalid_arg)?;
                t.repeat_all = k == 2;
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("RowAxisLayout", _) => {
                let f = a
                    .i32(0)?
                    .and_then(Form::from_code)
                    .ok_or_else(invalid_arg)?;
                t.form = f;
                for pf in &mut t.fields {
                    pf.form = None;
                    pf.sub_top = None;
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            (
                "ShowTableStyleColumnHeaders"
                | "ShowTableStyleRowHeaders"
                | "ShowTableStyleColumnStripes"
                | "ShowTableStyleRowStripes"
                | "ShowTableStyleLastColumn",
                k,
            ) => {
                let ix = match m.name {
                    "ShowTableStyleColumnHeaders" => 0,
                    "ShowTableStyleRowHeaders" => 1,
                    "ShowTableStyleColumnStripes" => 2,
                    "ShowTableStyleRowStripes" => 3,
                    _ => 4,
                };
                if k == Get {
                    return Ok(Variant::Bool(t.style_flags[ix]));
                }
                match &a.put {
                    Some(Variant::Bool(v)) => t.style_flags[ix] = *v,
                    _ => return Err(OmError::type_mismatch()),
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("TableStyle2", Get) => {
                if t.style.is_empty() {
                    return Ok(Variant::Bstr("\0".to_string()));
                }
                Ok(self.alloc(Object::Pivot(PivotRef::Style(b, t.name.clone()))))
            }
            ("TableStyle2", Put) => {
                match &a.put {
                    Some(Variant::Bstr(s)) if s.is_empty() || s.starts_with("PivotStyle") => {
                        t.style = s.clone()
                    }
                    Some(Variant::Bstr(_)) => {}
                    _ => return Err(OmError::type_mismatch()),
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("CompactLayoutRowHeader", Get) => Ok(Variant::Bstr(
                t.row_caption.clone().unwrap_or_else(|| "Row Labels".into()),
            )),
            ("CompactLayoutColumnHeader", Get) => Ok(Variant::Bstr(
                t.col_caption
                    .clone()
                    .unwrap_or_else(|| "Column Labels".into()),
            )),
            ("CompactLayoutRowHeader" | "CompactLayoutColumnHeader", Put) => {
                let Some(Variant::Bstr(s)) = a.put.clone() else {
                    return Err(OmError::type_mismatch());
                };
                if m.name == "CompactLayoutRowHeader" {
                    t.row_caption = (s != "Row Labels").then_some(s);
                } else {
                    t.col_caption = (s != "Column Labels").then_some(s);
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("RowGrand", Get) => Ok(Variant::Bool(t.row_grand)),
            ("ColumnGrand", Get) => Ok(Variant::Bool(t.col_grand)),
            ("RowGrand", Put) | ("ColumnGrand", Put) => {
                let v = match &a.put {
                    Some(Variant::Bool(v)) => *v,
                    _ => return Err(OmError::type_mismatch()),
                };
                if m.name == "RowGrand" {
                    t.row_grand = v;
                } else {
                    t.col_grand = v;
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("DisplayErrorString", Get) => Ok(Variant::Bool(t.opts.display_error)),
            ("DisplayNullString", Get) => Ok(Variant::Bool(t.opts.display_null)),
            ("ErrorString", Get) => Ok(Variant::Bstr(t.opts.error_string.clone())),
            ("NullString", Get) => Ok(Variant::Bstr(t.opts.null_string.clone())),
            ("DisplayErrorString" | "DisplayNullString", Put) => {
                let Some(Variant::Bool(v)) = a.put else {
                    return Err(OmError::type_mismatch());
                };
                if m.name == "DisplayErrorString" {
                    t.opts.display_error = v;
                } else {
                    t.opts.display_null = v;
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("ErrorString" | "NullString", Put) => {
                let Some(Variant::Bstr(v)) = a.put.clone().map(Variant::canon) else {
                    return Err(OmError::type_mismatch());
                };
                if m.name == "ErrorString" {
                    t.opts.error_string = v;
                } else {
                    t.opts.null_string = v;
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            (name, Get | Put) if OPT_BOOLS.iter().any(|(n, _)| *n == name) => {
                let ix = OPT_BOOLS.iter().position(|(n, _)| *n == name).unwrap_or(0);
                if m.kind == Get {
                    return Ok(Variant::Bool(t.opts.bools[ix]));
                }
                let Some(Variant::Bool(v)) = a.put else {
                    return Err(OmError::type_mismatch());
                };
                match name {
                    "VisualTotals" => return Ok(Variant::Empty),
                    "TotalsAnnotation" if v => return Err(OmError::app_defined()),
                    "DisplayImmediateItems" if !v => return Err(invalid_arg()),
                    _ => t.opts.bools[ix] = v,
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("DisplayEmptyRow" | "DisplayEmptyColumn" | "ViewCalculatedMembers", _) => {
                Err(OmError::app_defined())
            }
            ("Tag", Get) => Ok(Variant::Bstr(t.opts.tag.clone())),
            ("AlternativeText", Get) => Ok(t
                .opts
                .alt_text
                .clone()
                .map_or(Variant::BstrNull, Variant::Bstr)),
            ("Summary", Get) => Ok(t
                .opts
                .summary
                .clone()
                .map_or(Variant::BstrNull, Variant::Bstr)),
            ("Tag" | "AlternativeText" | "Summary", Put) => {
                let Some(Variant::Bstr(v)) = a.put.clone().map(Variant::canon) else {
                    return Err(OmError::type_mismatch());
                };
                match m.name {
                    "Tag" => t.opts.tag = v,
                    "AlternativeText" => t.opts.alt_text = Some(v),
                    _ => t.opts.summary = Some(v),
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("CompactRowIndent", Get) => Ok(Variant::I4(t.opts.compact_indent)),
            ("CompactRowIndent", Put) => {
                let v = self.put_i32(a)?;
                if !(0..=127).contains(&v) {
                    return Err(invalid_arg());
                }
                t.opts.compact_indent = v;
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("CacheIndex", Get) => {
                let pv = self.book(b)?.wb.pivots();
                let k = pv.caches[..t.cache].iter().filter(|c| !c.orphaned).count();
                Ok(Variant::I4(k as i32 + 1))
            }
            ("CacheIndex", Put) => Err(OmError::app_defined()),
            ("Version", Get) => Ok(Variant::I4(8)),
            ("Version", Put) => Err(OmError::hresult(crate::error::DISP_E_BADPARAMCOUNT)),
            ("Location", Get) => Err(OmError::exception(crate::error::E_NOTIMPL.0, None)),
            ("Location", Put) => {
                let Some(Variant::Bstr(text)) = a.put.clone().map(Variant::canon) else {
                    return Err(OmError::type_mismatch());
                };
                let (sheet, rect) = {
                    let wb = &self.book(b)?.wb;
                    parse_r1c1(wb, t.sheet, &text).ok_or_else(OmError::app_defined)?
                };
                let (r, c) = (rect.r0, rect.c0);
                {
                    let cc = &self.book(b)?.wb.pivots().caches[t.cache];
                    if cc.sheet == sheet && cc.rect.contains(r, c) {
                        return Err(OmError::app_defined());
                    }
                }
                let moved = self
                    .book_mut(b)?
                    .wb
                    .pivot_move(i, sheet, r, c)
                    .map_err(crate::objects::edit_error)?;
                if !moved {
                    return Err(OmError::app_defined());
                }
                Ok(Variant::Empty)
            }
            ("ClearTable", _) => {
                t.rows.clear();
                t.cols.clear();
                t.pages.clear();
                t.data.clear();
                for f in &mut t.fields {
                    *f = xlcalc::pivot::PtField {
                        group: f.group.take(),
                        calc_items: std::mem::take(&mut f.calc_items),
                        ..Default::default()
                    };
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("SourceData", Get) => {
                let pv = self.book(b)?.wb.pivots();
                Ok(Variant::Bstr(pv.caches[t.cache].source.clone()))
            }
            ("SourceData", Put) => {
                let cache = t.cache;
                self.set_cache_source(b, cache, a.put.as_ref())?;
                Ok(Variant::Empty)
            }
            ("ChangePivotCache", _) => {
                let new = match a.get(0) {
                    Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                        Some(Object::Pivot(PivotRef::Cache(nb, k))) if *nb == b => *k,
                        _ => return Err(invalid_arg()),
                    },
                    _ => return Err(invalid_arg()),
                };
                self.change_cache(b, i, new)?;
                Ok(Variant::Empty)
            }
            ("GetPivotData", _) => {
                let Some(dv) = a.get(0) else {
                    return Err(OmError::app_defined());
                };
                let data = crate::vba::variant_value(dv)?;
                let mut pairs = Vec::new();
                let mut k = 1;
                while let Some(f) = a.get(k) {
                    let Some(it) = a.get(k + 1) else {
                        return Err(OmError::app_defined());
                    };
                    pairs.push((
                        crate::vba::variant_value(f)?,
                        crate::vba::variant_value(it)?,
                    ));
                    k += 2;
                }
                let ti = self.table_idx(b, &t.name)?;
                let hit = self.book(b)?.wb.pivot_data_cell(ti, &data, &pairs);
                match hit {
                    Some(at) => {
                        self.new_range(b, at.sheet, Rect::cell(at.row, at.col), Mode::Cells)
                    }
                    None => Err(OmError::app_defined()),
                }
            }
            ("RefreshTable", _) => {
                let c = t.cache;
                self.refresh_cache(b, c)?;
                Ok(Variant::Bool(true))
            }
            ("PivotSelect", _) => match a.get(0) {
                Some(Variant::Bstr(s)) => {
                    let (fname, item) = match s.split_once('[') {
                        Some((f, rest)) => (f, rest.strip_suffix(']')),
                        None => (s.as_str(), None),
                    };
                    let f = (0..cache.fields.len())
                        .find(|&k| t.field_name(&cache, k).eq_ignore_ascii_case(fname))
                        .ok_or_else(OmError::app_defined)?;
                    if let Some(it) = item {
                        let n = cache.fields[f].items.len() + t.fields[f].calc_items.len();
                        (0..n as u32)
                            .any(|k| Self::item_label(&t, &cache, f, k).eq_ignore_ascii_case(it))
                            .then_some(())
                            .ok_or_else(OmError::app_defined)?;
                    }
                    Ok(Variant::Empty)
                }
                Some(Variant::Bool(_)) => Err(OmError::app_defined()),
                _ => Ok(Variant::Empty),
            },
            ("ShowPages", _) => self.show_pages(b, &t, &cache, a.get(0)),
            ("PageFieldOrder", Get) => Ok(Variant::I4(if t.page_over { 2 } else { 1 })),
            ("PageFieldOrder", Put) => {
                t.page_over = match self.put_i32(a)? {
                    1 => false,
                    2 => true,
                    _ => return Err(OmError::unable_to_set("PageFieldOrder", "PivotTable")),
                };
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("PageFieldWrapCount", Get) => Ok(Variant::I4(t.page_wrap as i32)),
            ("PageFieldWrapCount", Put) => {
                t.page_wrap = match self.put_i32(a)? {
                    n if n >= 0 => n as u32,
                    _ => return Err(OmError::unable_to_set("PageFieldWrapCount", "PivotTable")),
                };
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("PivotCache", _) => Ok(self.alloc(Object::Pivot(PivotRef::Cache(b, t.cache)))),
            ("Parent", Get) => Ok(self.sheet_object(b, t.sheet)),
            ("Application", Get) => Ok(Variant::Dispatch(Some(self.handle()))),
            _ => Err(unsupported("PivotTable", m)),
        }
    }

    fn show_pages(
        &mut self,
        b: BookId,
        t: &PivotTable,
        c: &PivotCache,
        field: Option<&Variant>,
    ) -> Result<Variant, OmError> {
        let f = match field {
            Some(Variant::Bstr(s)) => (0..c.fields.len())
                .find(|&k| t.field_name(c, k).eq_ignore_ascii_case(s))
                .filter(|k| t.pages.contains(k)),
            None | Some(Variant::Empty) => t.pages.first().copied(),
            _ => None,
        }
        .ok_or_else(OmError::app_defined)?;
        let items: Vec<u32> = display_order(t, c, f)
            .into_iter()
            .filter(|&k| !t.fields[f].hidden.contains(&(k as usize)))
            .filter(|&k| t.fields[f].page_item != Some(k))
            .collect();
        let names: Vec<String> = items
            .iter()
            .map(|&k| Self::item_label(t, c, f, k))
            .collect();
        let wb = &mut self.book_mut(b)?.wb;
        let pos = wb
            .sheet_position(t.sheet)
            .ok_or_else(OmError::app_defined)?;
        let before = (pos + 1 < wb.sheet_count()).then(|| wb.sheet_at(pos + 1).id);
        let mut sheets = Vec::new();
        for name in &names {
            sheets.push(
                wb.add_sheet_before(name, before)
                    .map_err(crate::objects::edit_error)?,
            );
        }
        for (k, &item) in items.iter().enumerate().rev() {
            let taken: Vec<String> = wb.pivots().tables.iter().map(|x| x.name.clone()).collect();
            let name = (1..)
                .map(|n| format!("PivotTable{n}"))
                .find(|n| !taken.iter().any(|x| x.eq_ignore_ascii_case(n)))
                .unwrap_or_default();
            let mut nt = t.clone();
            nt.name = name;
            nt.sheet = sheets[k];
            nt.row0 = 2;
            nt.col0 = 0;
            nt.written = None;
            nt.ranges = xlcalc::pivot::Ranges::default();
            nt.fields[f].page_item = Some(item);
            nt.fields[f].multi_page = false;
            wb.pivots_mut().tables.push(nt);
            let i = wb.pivots().tables.len() - 1;
            wb.pivot_update(i).map_err(crate::objects::edit_error)?;
        }
        if let Some(&first) = sheets.first() {
            wb.set_active_sheet(first)
                .map_err(crate::objects::edit_error)?;
        }
        Ok(Variant::Bool(true))
    }

    fn field_ref_name(t: &PivotTable, c: &PivotCache, f: FieldRef) -> String {
        match f {
            FieldRef::Src(i) => t.field_name(c, i).to_string(),
            FieldRef::Data(i) => t.data[i].caption.clone(),
            FieldRef::Values => "Values".to_string(),
        }
    }

    fn field_list(t: &PivotTable, c: &PivotCache, k: FieldsKind) -> Vec<FieldRef> {
        let src = |a: &Ax| match a {
            Ax::Field(f) => FieldRef::Src(*f),
            Ax::Values => FieldRef::Values,
        };
        match k {
            FieldsKind::All => (0..c.fields.len()).map(FieldRef::Src).collect(),
            FieldsKind::Row => {
                let mut rows: Vec<Ax> = t.rows.clone();

                if rows.iter().any(|a| {
                    matches!(a, Ax::Field(f) if t.fields[*f].group.as_ref().is_some_and(|g|
                        matches!(g, xlcalc::pivot::Group::Derived { .. } | xlcalc::pivot::Group::Manual { .. })))
                }) {
                    rows.sort_by_key(|a| match a {
                        Ax::Field(f) => *f,
                        Ax::Values => usize::MAX,
                    });
                }
                rows.iter().map(src).collect()
            }
            FieldsKind::Column => t.cols.iter().map(src).collect(),
            FieldsKind::Page => t.pages.iter().map(|f| FieldRef::Src(*f)).collect(),
            FieldsKind::Data => (0..t.data.len()).map(FieldRef::Data).collect(),
            FieldsKind::Hidden => (0..c.fields.len())
                .filter(|f| t.fields[*f].orient == Orient::Hidden)
                .map(FieldRef::Src)
                .collect(),
        }
    }

    fn fields_item(
        &mut self,
        b: BookId,
        t: &PivotTable,
        c: &PivotCache,
        k: FieldsKind,
        idx: &Variant,
        lcid: u32,
    ) -> Result<Variant, OmError> {
        let list = Self::field_list(t, c, k);
        let names: Vec<String> = list
            .iter()
            .map(|f| Self::field_ref_name(t, c, *f))
            .collect();
        let class = match k {
            FieldsKind::Data => "DataFields",
            _ => "PivotFields",
        };
        let pos = match self.item_index(Some(idx), lcid, &names, |n| n.as_str()) {
            Ok(p) => Some(p),

            Err(_) => match (k, idx) {
                (FieldsKind::All, Variant::Bstr(s)) => {
                    let d = t
                        .data
                        .iter()
                        .position(|d| d.caption.eq_ignore_ascii_case(s));
                    return match d {
                        Some(d) => Ok(self.alloc(Object::Pivot(PivotRef::Field(
                            b,
                            t.name.clone(),
                            FieldRef::Data(d),
                        )))),
                        None => Err(unable(class, "PivotTable")),
                    };
                }
                _ => None,
            },
        };
        let pos = pos.ok_or_else(|| unable(class, "PivotTable"))?;
        Ok(self.alloc(Object::Pivot(PivotRef::Field(b, t.name.clone(), list[pos]))))
    }

    fn fields_member(
        &mut self,
        b: BookId,
        n: &str,
        k: FieldsKind,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let t = self.pt(b, n)?;
        let c = self.cache_of(b, &t)?;
        let list = Self::field_list(&t, &c, k);
        match m.name {
            "Count" => {
                let extra = usize::from(k == FieldsKind::All && t.data.len() > 1);
                Ok(Variant::I4((list.len() + extra) as i32))
            }
            "Item" | "_Default" => match a.get(0) {
                Some(i) => {
                    let i = i.clone();
                    self.fields_item(b, &t, &c, k, &i, a.lcid)
                }
                None => Err(OmError::bad_index()),
            },
            "_NewEnum" => {
                let items = list
                    .iter()
                    .map(|f| self.alloc(Object::Pivot(PivotRef::Field(b, t.name.clone(), *f))))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            _ => Err(unsupported("PivotFields", m)),
        }
    }

    fn field_member(
        &mut self,
        b: BookId,
        n: &str,
        f: FieldRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let i = self.table_idx(b, n)?;
        let mut t = self.pt(b, n)?;
        let c = self.cache_of(b, &t)?;
        match (m.name, m.kind, f) {
            ("Name" | "Caption", Get, _) => Ok(Variant::Bstr(Self::field_ref_name(&t, &c, f))),
            ("SourceName", Get, FieldRef::Data(d)) => {
                Ok(Variant::Bstr(c.fields[t.data[d].src].name.clone()))
            }
            ("SourceName", Get, FieldRef::Src(s)) => Ok(Variant::Bstr(c.fields[s].name.clone())),
            ("SourceName", Get, _) => Ok(Variant::Bstr(Self::field_ref_name(&t, &c, f))),
            ("Name" | "Caption", Put, FieldRef::Src(s)) if c.fields[s].formula.is_none() => {
                let Some(Variant::Bstr(new)) = &a.put else {
                    return Err(invalid_arg());
                };
                if new.is_empty() {
                    t.fields[s].caption = None;
                } else {
                    Self::rename_field(&mut t, &c, s, new)?;
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("Orientation", Get, FieldRef::Src(s)) => {
                Ok(Variant::R8(f64::from(t.fields[s].orient.code())))
            }
            ("Orientation", Get, FieldRef::Data(_)) => Ok(Variant::R8(4.0)),
            ("Orientation", Get, FieldRef::Values) => {
                let rows = t.rows.contains(&Ax::Values);
                Ok(Variant::R8(if rows { 1.0 } else { 2.0 }))
            }
            ("Name" | "Caption", Put, FieldRef::Data(d)) => {
                let Some(Variant::Bstr(new)) = &a.put else {
                    return Err(OmError::unable_to_set(m.name, "PivotField"));
                };
                if new.is_empty() {
                    return Err(OmError::unable_to_set(m.name, "PivotField"));
                }
                let clash = c.fields.iter().any(|x| x.name.eq_ignore_ascii_case(new))
                    || t.data
                        .iter()
                        .enumerate()
                        .any(|(k, x)| k != d && x.caption.eq_ignore_ascii_case(new));
                if clash {
                    return Err(OmError::app_defined());
                }
                t.data[d].caption = new.clone();
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("CalculatedItems", _, FieldRef::Src(s)) => {
                let coll = self.alloc(Object::Pivot(PivotRef::CalcItems(b, t.name.clone(), s)));
                match a.get(0) {
                    None => Ok(coll),
                    Some(idx) => {
                        let idx = idx.clone();
                        self.calc_item_obj(b, &t, &c, s, &idx)
                    }
                }
            }
            ("Formula", Get, FieldRef::Data(_)) => Err(OmError::app_defined_with("")),
            ("IsCalculated", Get, FieldRef::Src(s)) => {
                Ok(Variant::Bool(c.fields[s].formula.is_some()))
            }
            ("IsCalculated", Get, _) => Ok(Variant::Bool(false)),
            ("Formula", Get, FieldRef::Src(src)) if c.fields[src].formula.is_some() => {
                Ok(Variant::Bstr(format!(
                    "={}",
                    c.fields[src].formula.clone().unwrap_or_default()
                )))
            }
            ("Formula", Put, FieldRef::Src(s)) if c.fields[s].formula.is_some() => {
                let text = match &a.put {
                    Some(Variant::Bstr(x)) => x.trim().to_string(),
                    _ => return Err(OmError::unable_to_set("Formula", "PivotField")),
                };
                let body = text.trim_start_matches('=').to_string();
                if !xlcalc::pivot::calc_formula_valid(&c, &body) {
                    return Err(OmError::unable_to_set("Formula", "PivotField"));
                }
                let ci = t.cache;
                self.book_mut(b)?.wb.pivots_mut().caches[ci].fields[s].formula = Some(body);
                self.pt_commit(b, i, t)?;
                self.refresh_tables_of(b, ci)?;
                Ok(Variant::Empty)
            }
            ("Delete", _, FieldRef::Src(s)) if c.fields[s].formula.is_some() => {
                let ci = t.cache;
                self.delete_calc(b, ci, s)?;
                Ok(Variant::Empty)
            }
            ("Name", Put, FieldRef::Src(s)) if c.fields[s].formula.is_some() => {
                Err(OmError::unable_to_set("Name", "PivotField"))
            }
            ("Orientation", Put, FieldRef::Src(s)) if c.fields[s].formula.is_some() => {
                let o = match &a.put {
                    Some(Variant::I4(v)) => *v,
                    Some(Variant::I2(v)) => i32::from(*v),
                    Some(Variant::R8(v)) => *v as i32,
                    _ => return Err(OmError::unable_to_set("Orientation", "PivotField")),
                };
                match o {
                    0 => {
                        t.set_orient(s, Orient::Hidden);
                    }
                    4 => {
                        let base = func_name_default(&c.fields[s].name, xlcalc::pivot::Func::Sum);
                        let taken = |x: &str| {
                            c.fields.iter().any(|f| f.name.eq_ignore_ascii_case(x))
                                || t.data.iter().any(|d| d.caption.eq_ignore_ascii_case(x))
                        };
                        let mut k = 1;
                        let mut cap = base.clone();
                        while taken(&cap) {
                            k += 1;
                            cap = format!("{base}{k}");
                        }
                        t.add_data(DataField {
                            src: s,
                            caption: cap,
                            func: xlcalc::pivot::Func::Sum,
                            numfmt: None,
                            show: Default::default(),
                        });
                    }
                    _ => return Err(OmError::unable_to_set("Orientation", "PivotField")),
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("Orientation", Put, FieldRef::Src(s)) => {
                let o = a
                    .i32(0)
                    .ok()
                    .flatten()
                    .or_else(|| match &a.put {
                        Some(Variant::I4(v)) => Some(*v),
                        Some(Variant::I2(v)) => Some(i32::from(*v)),
                        Some(Variant::R8(v)) => Some(*v as i32),
                        _ => None,
                    })
                    .and_then(Orient::from_code)
                    .ok_or_else(|| OmError::unable_to_set("Orientation", "PivotField"))?;

                if t.fields.iter().any(|pf| {
                    matches!(&pf.group, Some(xlcalc::pivot::Group::Derived { src, .. }) if *src == s)
                }) {
                    return Err(OmError::unable_to_set("Orientation", "PivotField"));
                }
                if o == Orient::Data {
                    return Err(unsupported("PivotField", m));
                }
                t.set_orient(s, o);

                if o == Orient::Page {
                    t.fields[s].filter = None;
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("Orientation", Put, FieldRef::Values) => {
                let o = match &a.put {
                    Some(Variant::I4(v)) => *v,
                    Some(Variant::I2(v)) => i32::from(*v),
                    Some(Variant::R8(v)) => *v as i32,
                    _ => return Err(OmError::unable_to_set("Orientation", "PivotField")),
                };
                t.cols.retain(|x| *x != Ax::Values);
                t.rows.retain(|x| *x != Ax::Values);
                match o {
                    1 => t.rows.push(Ax::Values),
                    2 => t.cols.push(Ax::Values),
                    _ => return Err(OmError::unable_to_set("Orientation", "PivotField")),
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("Position", Put, FieldRef::Src(_) | FieldRef::Values) => {
                let pos = match &a.put {
                    Some(Variant::I4(v)) => *v,
                    Some(Variant::I2(v)) => i32::from(*v),
                    Some(Variant::R8(v)) => *v as i32,
                    _ => return Err(OmError::unable_to_set("Position", "PivotField")),
                };
                let me = match f {
                    FieldRef::Src(s) => Ax::Field(s),
                    _ => Ax::Values,
                };
                if me == Ax::Values && !t.rows.contains(&me) && !t.cols.contains(&me) {
                    t.cols.push(me);
                }
                if pos < 1 {
                    return Err(OmError::unable_to_set("Position", "PivotField"));
                }
                let at = pos as usize - 1;

                if t.data.len() > 1
                    && !t.rows.contains(&Ax::Values)
                    && !t.cols.contains(&Ax::Values)
                    && t.cols.contains(&me)
                {
                    t.cols.push(Ax::Values);
                }
                let list: Option<&mut Vec<Ax>> = if t.rows.contains(&me) {
                    Some(&mut t.rows)
                } else if t.cols.contains(&me) {
                    Some(&mut t.cols)
                } else {
                    None
                };
                if let Some(l) = list {
                    if at >= l.len() {
                        return Err(OmError::unable_to_set("Position", "PivotField"));
                    }
                    l.retain(|x| *x != me);
                    l.insert(at, me);
                } else if let FieldRef::Src(s) = f {
                    if let Some(p) = t.pages.iter().position(|x| *x == s) {
                        t.pages.remove(p);
                        t.pages.insert(at.min(t.pages.len()), s);
                    } else {
                        return Err(OmError::unable_to_set("Position", "PivotField"));
                    }
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("Position", Get, FieldRef::Data(d)) => Ok(Variant::R8(d as f64 + 1.0)),
            ("Position", Get, FieldRef::Values) => {
                let pos = t
                    .cols
                    .iter()
                    .position(|x| *x == Ax::Values)
                    .or_else(|| t.rows.iter().position(|x| *x == Ax::Values))
                    .unwrap_or(0);
                Ok(Variant::R8(pos as f64 + 1.0))
            }
            ("Position", Get, FieldRef::Src(s)) => {
                let o = t.fields[s].orient;
                let pos = match o {
                    Orient::Row => t.rows.iter().position(|x| *x == Ax::Field(s)),
                    Orient::Column => t.cols.iter().position(|x| *x == Ax::Field(s)),
                    Orient::Page => t.pages.iter().position(|x| *x == s),
                    _ => None,
                };
                match pos {
                    Some(p) => Ok(Variant::R8(p as f64 + 1.0)),
                    None => Ok(Variant::Error(0x800A_07FAu32 as i32)),
                }
            }
            ("NumberFormat", Get, FieldRef::Data(d)) => {
                let wb = &self.book(b)?.wb;
                Ok(Variant::Bstr(wb.pivot_data_format(&t, &c, d)))
            }
            ("NumberFormat", Put, FieldRef::Data(d)) => {
                t.data[d].numfmt = a.string(0)?.or_else(|| match &a.put {
                    Some(Variant::Bstr(s)) => Some(s.clone()),
                    _ => None,
                });
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("Function", Get, FieldRef::Data(d)) => Ok(Variant::I4(t.data[d].func.code())),
            ("Function", Put, FieldRef::Data(d)) => {
                let code = match &a.put {
                    Some(Variant::I4(v)) => *v,
                    Some(Variant::I2(v)) => i32::from(*v),
                    Some(Variant::R8(v)) => *v as i32,
                    _ => return Err(OmError::unable_to_set("Function", "PivotField")),
                };

                let func = match code {
                    0 => Some(xlcalc::pivot::Func::Sum),
                    1 => Some(xlcalc::pivot::Func::Count),
                    2 => Some(xlcalc::pivot::Func::Average),
                    c => xlcalc::pivot::Func::from_code(c),
                };
                let Some(func) = func else {
                    return Err(OmError::unable_to_set("Function", "PivotField"));
                };

                let src = c.fields[t.data[d].src].name.clone();
                let old = func_name_default(&src, t.data[d].func);
                let suffix = t.data[d]
                    .caption
                    .strip_prefix(&old)
                    .filter(|r| r.bytes().all(|x| x.is_ascii_digit()))
                    .unwrap_or("")
                    .to_string();
                t.data[d].func = func;
                t.data[d].caption = format!("{}{suffix}", func_name_default(&src, func));
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("Function", Get, _) => Err(unable("Function", "PivotField")),

            ("CurrentPageName", _, FieldRef::Src(_)) => Err(OmError::app_defined()),
            ("CurrentPage", Get, FieldRef::Src(s)) if !t.pages.contains(&s) => {
                Err(unable("CurrentPage", "PivotField"))
            }
            ("CurrentPage", Put, FieldRef::Src(s)) if !t.pages.contains(&s) => {
                Err(OmError::unable_to_set("CurrentPage", "PivotField"))
            }
            ("CurrentPage", Get, FieldRef::Src(s)) => {
                let pf = &t.fields[s];
                let it = match pf.page_item {
                    Some(i) => i,
                    None => {
                        let txt = xlcalc::pivot::page_text(&t, &c, s);
                        if txt == "(Multiple Items)" {
                            u32::MAX - 1
                        } else if txt == "(All)" {
                            u32::MAX
                        } else {
                            c.fields[s]
                                .items
                                .iter()
                                .position(|x| x.label == txt)
                                .map_or(u32::MAX, |p| p as u32)
                        }
                    }
                };
                Ok(self.alloc(Object::Pivot(PivotRef::Item(b, t.name.clone(), s, it))))
            }
            ("CurrentPage", Put, FieldRef::Src(s)) => {
                if let Some(Variant::Bstr(name)) = &a.put {
                    if name == "(All)" {
                        t.fields[s].page_item = None;
                        t.fields[s].hidden.clear();
                    } else if let Some(it) = Self::item_arg(&t, &c, s, &Variant::Bstr(name.clone()))
                    {
                        t.fields[s].page_item = Some(it);
                        t.fields[s].hidden.clear();
                    } else if t.fields[s].page_item.is_none() {
                        return Err(OmError::unable_to_set("CurrentPage", "PivotField"));
                    }
                    self.pt_commit(b, i, t)?;
                }
                Ok(Variant::Empty)
            }
            ("EnableMultiplePageItems", Get, FieldRef::Src(s)) => {
                Ok(Variant::Bool(t.fields[s].multi_page))
            }
            ("EnableMultiplePageItems", Put, FieldRef::Src(s)) => {
                t.fields[s].multi_page = self.put_bool(a)?;
                if t.fields[s].multi_page {
                    t.fields[s].page_item = None;
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("PivotFilters", _, FieldRef::Src(s)) => {
                let coll = self.alloc(Object::Pivot(PivotRef::Filters(b, t.name.clone(), s)));
                match a.get(0) {
                    None => Ok(coll),
                    Some(_) => {
                        if t.fields[s].filter.is_none() {
                            return Err(unable("PivotFilters", "PivotField"));
                        }
                        Ok(self.alloc(Object::Pivot(PivotRef::Filter(b, t.name.clone(), s))))
                    }
                }
            }
            (
                "ClearAllFilters" | "ClearLabelFilters" | "ClearValueFilters" | "ClearManualFilter",
                _,
                FieldRef::Src(s),
            ) => {
                let pf = &mut t.fields[s];
                match m.name {
                    "ClearAllFilters" => {
                        pf.filter = None;
                        pf.hidden.clear();

                        pf.page_item = None;
                        pf.multi_page = false;
                    }
                    "ClearLabelFilters" => {
                        if pf
                            .filter
                            .as_ref()
                            .is_some_and(|f| (15..=28).contains(&f.kind))
                        {
                            pf.filter = None;
                        }
                    }
                    "ClearValueFilters" => {
                        if pf.filter.as_ref().is_some_and(|f| f.data.is_some()) {
                            pf.filter = None;
                        }
                    }
                    _ => pf.hidden.clear(),
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("PivotItems", _, FieldRef::Src(s)) => {
                let Some(idx) = a.get(0) else {
                    return Ok(self.alloc(Object::Pivot(PivotRef::Items(b, t.name.clone(), s))));
                };
                let it = Self::item_arg(&t, &c, s, idx)
                    .ok_or_else(|| unable("PivotItems", "PivotField"))?;
                Ok(self.alloc(Object::Pivot(PivotRef::Item(b, t.name.clone(), s, it))))
            }
            ("HiddenItemsList", _, _) => Err(OmError::app_defined_with("")),
            ("Subtotals", Get | Func, FieldRef::Src(_)) if a.get(0).is_none() => {
                Err(OmError::app_defined_with(""))
            }
            ("Subtotals", Get | Func, FieldRef::Src(s)) => {
                let ix = a.i32(0)?.unwrap_or(1);
                match ix {
                    1 => Ok(Variant::Bool(t.fields[s].subtotals)),
                    2..=12 => Ok(Variant::Bool(t.fields[s].custom.contains(&(ix as u8)))),
                    _ => Err(invalid_arg()),
                }
            }
            ("Subtotals", Put, FieldRef::Src(s)) => {
                let ix = a.i32(0)?.unwrap_or(1);
                let on = self.put_bool(a)?;
                if !t.fields[s].calc_items.is_empty() {
                    return Err(OmError::unable_to_set("Subtotals", "PivotField"));
                }
                let pf = &mut t.fields[s];
                match ix {
                    1 => {
                        pf.subtotals = on;
                        pf.custom.clear();
                    }
                    2..=12 => {
                        pf.custom.retain(|x| *x != ix as u8);
                        if on {
                            pf.custom.push(ix as u8);
                            pf.custom.sort_unstable();
                            pf.subtotals = false;
                        }
                    }
                    _ => return Err(invalid_arg()),
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("LayoutBlankLine", Get, FieldRef::Src(s)) => Ok(Variant::Bool(t.fields[s].blank_line)),
            ("LayoutBlankLine", Put, FieldRef::Src(s)) => {
                t.fields[s].blank_line = self.put_bool(a)?;
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }

            ("LayoutForm", Get, FieldRef::Src(s)) => Ok(Variant::I4(i32::from(
                t.eff_form(s) != xlcalc::pivot::Form::Tabular,
            ))),
            ("LayoutForm", Put, FieldRef::Src(s)) => {
                use xlcalc::pivot::Form;
                t.fields[s].form = Some(match self.put_i32(a)? {
                    0 => Form::Tabular,
                    1 if t.eff_form(s) == Form::Compact => Form::Compact,
                    1 => Form::Outline,
                    _ => return Err(OmError::unable_to_set("LayoutForm", "PivotField")),
                });
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("LayoutCompactRow", Get, FieldRef::Src(s)) => {
                Ok(Variant::Bool(t.eff_form(s) == xlcalc::pivot::Form::Compact))
            }
            ("LayoutCompactRow", Put, FieldRef::Src(s)) => {
                use xlcalc::pivot::Form;
                let on = self.put_bool(a)?;
                t.fields[s].form = Some(match (on, t.eff_form(s)) {
                    (true, _) => Form::Compact,
                    (false, Form::Compact) => Form::Outline,
                    (false, f) => f,
                });
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("LayoutSubtotalLocation", Get, FieldRef::Src(s)) => {
                Ok(Variant::I4(if t.sub_at_top(s) { 1 } else { 2 }))
            }
            ("LayoutSubtotalLocation", Put, FieldRef::Src(s)) => {
                t.fields[s].sub_top = Some(match self.put_i32(a)? {
                    1 => true,
                    2 => false,
                    _ => {
                        return Err(OmError::unable_to_set(
                            "LayoutSubtotalLocation",
                            "PivotField",
                        ))
                    }
                });
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("RepeatLabels", Get, FieldRef::Src(s)) => Ok(Variant::Bool(t.fields[s].repeat)),
            ("RepeatLabels", Put, FieldRef::Src(s)) => {
                t.fields[s].repeat = self.put_bool(a)?;
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }

            ("AutoShow", Func, FieldRef::Src(_))
                if !matches!(a.get(3), None | Some(Variant::Bstr(_))) =>
            {
                Err(OmError::arg(crate::error::DISP_E_TYPEMISMATCH, 0))
            }
            ("AutoSort", Func, FieldRef::Src(s)) => {
                let order = a.i32(0)?.ok_or_else(invalid_arg)?;
                let key = a.string(1)?.unwrap_or_default();
                let frozen = (order == -4135 && t.fields[s].sort != Sort::Manual)
                    .then(|| xlcalc::pivot::shown_order(&t, &c, s));
                t.fields[s].sort = match order {
                    -4135 => Sort::Manual,
                    1 | 2 => {
                        let desc = order == 2;
                        if key.eq_ignore_ascii_case(&c.fields[s].name) {
                            Sort::Name(desc)
                        } else if let Some(d) = t
                            .data
                            .iter()
                            .position(|d| d.caption.eq_ignore_ascii_case(&key))
                        {
                            Sort::Data(desc, d)
                        } else {
                            Sort::Name(desc)
                        }
                    }
                    _ => return Err(invalid_arg()),
                };
                if frozen.is_some() {
                    t.fields[s].manual = frozen;
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("AutoSortOrder", Get, FieldRef::Src(s)) => Ok(Variant::I4(match t.fields[s].sort {
                Sort::Manual => -4135,
                Sort::Name(true) | Sort::Data(true, _) => 2,
                _ => 1,
            })),
            ("AutoSortField", Get, FieldRef::Src(s)) => Ok(Variant::Bstr(match t.fields[s].sort {
                Sort::Manual | Sort::Name(_) => c.fields[s].name.clone(),
                Sort::Data(_, d) => t.data[d].caption.clone(),
            })),
            ("Calculation", Get, FieldRef::Data(d)) => Ok(Variant::I4(t.data[d].show.kind)),
            ("Calculation", Put, FieldRef::Data(d)) => {
                let k = self.put_i32(a)?;
                t.data[d].show.kind = if matches!(k, -4143 | 2..=15) {
                    k
                } else {
                    -4143
                };
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("BaseField", Get, FieldRef::Data(d))
                if matches!(t.data[d].show.kind, -4143 | 6..=11) =>
            {
                Err(unable("BaseField", "PivotField"))
            }

            ("BaseItem", Get, FieldRef::Data(d)) if !(2..=4).contains(&t.data[d].show.kind) => {
                Err(unable("BaseItem", "PivotField"))
            }
            ("BaseItem", Put, FieldRef::Data(d)) if !(2..=4).contains(&t.data[d].show.kind) => {
                Err(OmError::unable_to_set("BaseItem", "PivotField"))
            }
            ("BaseField", Get, FieldRef::Data(d)) => match Self::base_field(&t, d) {
                Some(f) => Ok(self.alloc(Object::Pivot(PivotRef::Field(
                    b,
                    t.name.clone(),
                    FieldRef::Src(f),
                )))),
                None => Err(unable("BaseField", "PivotField")),
            },
            ("BaseField", Put, FieldRef::Data(d)) => {
                if matches!(t.data[d].show.kind, -4143 | 6..=11) {
                    return Err(OmError::unable_to_set("BaseField", "PivotField"));
                }
                let name = match &a.put {
                    Some(Variant::Bstr(s)) => s.clone(),
                    _ => return Err(OmError::unable_to_set("BaseField", "PivotField")),
                };
                let f = c
                    .fields
                    .iter()
                    .position(|x| x.name.eq_ignore_ascii_case(&name))
                    .ok_or_else(|| OmError::unable_to_set("BaseField", "PivotField"))?;
                t.data[d].show.base_field = Some(f);
                t.data[d].show.base_item = None;
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("BaseItem", Get, FieldRef::Data(d)) => {
                let Some(f) = Self::base_field(&t, d) else {
                    return Err(unable("BaseItem", "PivotField"));
                };
                let it = t.data[d]
                    .show
                    .base_item
                    .or_else(|| display_order(&t, &c, f).first().copied())
                    .ok_or_else(|| unable("BaseItem", "PivotField"))?;
                Ok(self.alloc(Object::Pivot(PivotRef::Item(b, t.name.clone(), f, it))))
            }
            ("BaseItem", Put, FieldRef::Data(d)) => {
                let Some(f) = Self::base_field(&t, d) else {
                    return Err(OmError::unable_to_set("BaseItem", "PivotField"));
                };
                if let Some(Variant::Bstr(s)) = &a.put {
                    let it = Self::item_arg(&t, &c, f, &Variant::Bstr(s.clone()));
                    if it.is_some() {
                        t.data[d].show.base_item = it;
                        self.pt_commit(b, i, t)?;
                    }
                }
                Ok(Variant::Empty)
            }
            ("Parent", Get, _) => Ok(self.alloc(Object::Pivot(PivotRef::Table(b, t.name.clone())))),
            _ => Err(unsupported("PivotField", m)),
        }
    }
}

impl Application<'_> {
    fn base_field(t: &PivotTable, d: usize) -> Option<usize> {
        t.data[d].show.base_field.or_else(|| {
            let (rax, cax) = t.axes();
            rax.iter().chain(cax.iter()).find_map(|x| match x {
                Ax::Field(f) => Some(*f),
                Ax::Values => None,
            })
        })
    }

    fn item_arg(t: &PivotTable, c: &PivotCache, s: usize, v: &Variant) -> Option<u32> {
        let order = display_order(t, c, s);
        match v {
            Variant::Bstr(n) => order
                .into_iter()
                .find(|&i| Self::item_label(t, c, s, i).eq_ignore_ascii_case(n)),
            Variant::I4(i) => order.get((*i as usize).checked_sub(1)?).copied(),
            Variant::I2(i) => order.get((*i as usize).checked_sub(1)?).copied(),
            Variant::R8(i) => order.get((*i as usize).checked_sub(1)?).copied(),
            _ => None,
        }
    }

    fn refresh_tables_of(&mut self, b: BookId, ci: usize) -> Result<(), OmError> {
        let wb = &mut self.book_mut(b)?.wb;
        let tables: Vec<usize> = wb
            .pivots()
            .tables
            .iter()
            .enumerate()
            .filter(|(_, t)| t.cache == ci)
            .map(|(k, _)| k)
            .collect();
        for k in tables {
            wb.pivot_update(k).map_err(crate::objects::edit_error)?;
        }
        Ok(())
    }

    fn delete_calc(&mut self, b: BookId, ci: usize, f: usize) -> Result<(), OmError> {
        let wb = &mut self.book_mut(b)?.wb;
        wb.pivots_mut().caches[ci].del_calc(f);
        for t in wb.pivots_mut().tables.iter_mut().filter(|t| t.cache == ci) {
            t.remove_field(f);
        }
        self.refresh_tables_of(b, ci)
    }

    fn calc_item(
        &mut self,
        b: BookId,
        t: &PivotTable,
        c: &PivotCache,
        idx: &Variant,
    ) -> Result<Variant, OmError> {
        let calcs: Vec<usize> = (0..c.fields.len())
            .filter(|&f| c.fields[f].formula.is_some())
            .collect();
        let pick = match idx {
            Variant::Bstr(n) => calcs
                .iter()
                .copied()
                .find(|&f| c.fields[f].name.eq_ignore_ascii_case(n)),
            Variant::I4(k) => calcs.get((*k as usize).wrapping_sub(1)).copied(),
            Variant::I2(k) => calcs.get((*k as usize).wrapping_sub(1)).copied(),
            Variant::R8(k) => calcs.get((*k as usize).wrapping_sub(1)).copied(),
            _ => None,
        };

        let f = pick.ok_or_else(|| match idx {
            Variant::Bstr(_) => unable("CalculatedFields", "PivotTable"),
            _ => OmError::bad_index(),
        })?;
        Ok(self.alloc(Object::Pivot(PivotRef::Field(
            b,
            t.name.clone(),
            FieldRef::Src(f),
        ))))
    }

    fn calc_fields_member(
        &mut self,
        b: BookId,
        n: &str,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let t = self.pt(b, n)?;
        let c = self.cache_of(b, &t)?;
        match (m.name, m.kind) {
            ("Count", Get) => Ok(Variant::I4(
                c.fields.iter().filter(|f| f.formula.is_some()).count() as i32,
            )),
            ("Item" | "_Default", _) => {
                let idx = a.get(0).ok_or_else(invalid_arg)?.clone();
                self.calc_item(b, &t, &c, &idx)
            }
            ("Add", _) => {
                let (Some(Variant::Bstr(name)), Some(Variant::Bstr(formula))) =
                    (a.get(0), a.get(1))
                else {
                    return Err(invalid_arg());
                };
                if name.is_empty() {
                    return Err(OmError::app_defined());
                }
                if c.fields.iter().any(|f| f.name.eq_ignore_ascii_case(name)) {
                    return Err(OmError::app_defined());
                }
                let body = formula.trim().trim_start_matches('=').to_string();
                if !xlcalc::pivot::calc_formula_valid(&c, &body) {
                    return Ok(Variant::Dispatch(None));
                }
                let ci = t.cache;
                let wb = &mut self.book_mut(b)?.wb;
                wb.pivots_mut().caches[ci].add_calc(name, &body);
                for t in wb.pivots_mut().tables.iter_mut().filter(|t| t.cache == ci) {
                    t.add_field_slot();
                }
                let f = c.fields.len();
                Ok(self.alloc(Object::Pivot(PivotRef::Field(
                    b,
                    n.to_string(),
                    FieldRef::Src(f),
                ))))
            }
            _ => Err(unsupported("CalculatedFields", m)),
        }
    }

    fn filters_member(
        &mut self,
        b: BookId,
        n: &str,
        s: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let i = self.table_idx(b, n)?;
        let mut t = self.pt(b, n)?;
        match (m.name, m.kind) {
            ("Count", Get) => Ok(Variant::I4(i32::from(t.fields[s].filter.is_some()))),
            ("Item" | "_Default", _) => {
                if t.fields[s].filter.is_none() {
                    return Err(unable("PivotFilters", "PivotField"));
                }
                Ok(self.alloc(Object::Pivot(PivotRef::Filter(b, t.name.clone(), s))))
            }
            ("Add2" | "Add", _) => {
                let kind = a.i32(0)?.ok_or_else(invalid_arg)?;
                let data = match a.get(1) {
                    Some(Variant::Dispatch(Some(id))) => match self.arena.get(*id) {
                        Some(Object::Pivot(PivotRef::Field(_, _, FieldRef::Data(d)))) => Some(*d),
                        _ => None,
                    },
                    _ => None,
                };
                let val = |k: usize| match a.get(k) {
                    Some(Variant::Bstr(x)) => Value::text(x.as_str()),
                    Some(Variant::I4(x)) => Value::Number(f64::from(*x)),
                    Some(Variant::I2(x)) => Value::Number(f64::from(*x)),
                    Some(Variant::R8(x)) => Value::Number(*x),
                    _ => Value::Blank,
                };
                let two = matches!(kind, 13 | 14 | 27 | 28);
                let valid = (1..=28).contains(&kind);
                let value_kind = (1..=14).contains(&kind);

                if kind > 28 {
                    return Err(OmError::app_defined());
                }
                if !valid
                    || (value_kind && data.is_none())
                    || matches!(val(2), Value::Blank)
                    || (two && matches!(val(3), Value::Blank))
                {
                    return Err(invalid_arg());
                }

                if let Value::Number(x) = val(2) {
                    let bad = match kind {
                        1 | 2 => x < 1.0,
                        3 | 4 => !(0.0..=100.0).contains(&x),
                        _ => false,
                    };
                    if bad {
                        return Err(invalid_arg());
                    }
                }

                if (15..=28).contains(&kind) && self.book(b)?.wb.pivot_field_is_date(t.cache, s) {
                    return Err(OmError::app_defined());
                }

                if t.fields[s].filter.is_some() || t.pages.contains(&s) {
                    return Err(OmError::app_defined_with(""));
                }

                if (17..=22).contains(&kind) && matches!(val(2), Value::Number(_)) {
                    return Err(invalid_arg());
                }
                t.fields[s].filter = Some(Filter {
                    kind,
                    data: if value_kind { data } else { None },
                    v1: val(2),
                    v2: val(3),
                });
                self.pt_commit(b, i, t)?;
                Ok(self.alloc(Object::Pivot(PivotRef::Filter(b, n.to_string(), s))))
            }
            _ => Err(unsupported("PivotFilters", m)),
        }
    }

    fn filter_member(
        &mut self,
        b: BookId,
        n: &str,
        s: usize,
        m: &'static Member,
        _a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let t = self.pt(b, n)?;
        let Some(f) = t.fields[s].filter.clone() else {
            return Err(OmError::object_required());
        };
        let to_variant = |v: &Value| match v {
            Value::Text(x) => Variant::Bstr(x.to_string()),
            Value::Number(x) => Variant::Bstr(xlcalc::pivot::number_label(*x)),
            _ => Variant::Bstr(String::new()),
        };
        match (m.name, m.kind) {
            ("FilterType", Get) => Ok(Variant::I4(f.kind)),
            ("Value1", Get) => Ok(to_variant(&f.v1)),
            ("Value2", Get) => Ok(to_variant(&f.v2)),
            _ => Err(unsupported("PivotFilter", m)),
        }
    }

    fn item_label(t: &PivotTable, c: &PivotCache, s: usize, it: u32) -> String {
        let n = c.fields[s].items.len();
        match (it as usize).checked_sub(n) {
            Some(j) => t.fields[s].calc_items[j].name.clone(),
            None => match t.fields[s].item_caps.iter().find(|x| x.0 == it) {
                Some((_, cap)) => cap.clone(),
                None => c.fields[s].items[it as usize].label.clone(),
            },
        }
    }

    fn rename_field(
        t: &mut PivotTable,
        c: &PivotCache,
        s: usize,
        new: &str,
    ) -> Result<(), OmError> {
        let clash = (0..c.fields.len())
            .any(|k| k != s && t.field_name(c, k).eq_ignore_ascii_case(new))
            || t.data.iter().any(|d| d.caption.eq_ignore_ascii_case(new));
        if clash {
            return Err(invalid_arg());
        }
        t.fields[s].caption = if c.fields[s].name == new {
            None
        } else {
            Some(new.to_string())
        };
        Ok(())
    }

    fn rename_item(
        t: &mut PivotTable,
        c: &PivotCache,
        s: usize,
        it: u32,
        new: &str,
    ) -> Result<(), OmError> {
        let n = c.fields[s].items.len();
        let total = n + t.fields[s].calc_items.len();
        if new.is_empty()
            || (0..total as u32)
                .any(|k| k != it && Self::item_label(t, c, s, k).eq_ignore_ascii_case(new))
        {
            return Err(invalid_arg());
        }
        if (it as usize) >= n {
            t.fields[s].calc_items[it as usize - n].name = new.to_string();
            return Ok(());
        }
        let pf = &mut t.fields[s];
        pf.item_caps.retain(|x| x.0 != it);
        if c.fields[s].items[it as usize].label != new {
            pf.item_caps.push((it, new.to_string()));
        }
        Ok(())
    }

    fn check_item_formula(
        t: &PivotTable,
        c: &PivotCache,
        s: usize,
        body: &str,
        _skip: Option<usize>,
    ) -> Result<(), OmError> {
        let mut known = true;
        let names: Vec<String> = (0..(c.fields[s].items.len() + t.fields[s].calc_items.len())
            as u32)
            .map(|i| Self::item_label(t, c, s, i))
            .collect();
        let ok = xlcalc::pivot::item_formula_names(body, |name| {
            if !names.iter().any(|x| x.eq_ignore_ascii_case(name)) {
                known = false;
            }
        });
        if !ok || !known {
            return Err(OmError::app_defined_with(
                "An item name cannot be found. Check that you've typed the name correctly and the item is present in the PivotTable report.",
            ));
        }
        Ok(())
    }

    fn calc_item_obj(
        &mut self,
        b: BookId,
        t: &PivotTable,
        c: &PivotCache,
        s: usize,
        idx: &Variant,
    ) -> Result<Variant, OmError> {
        let n = c.fields[s].items.len();
        let k = t.fields[s].calc_items.len();
        let shown: Vec<usize> = display_order(t, c, s)
            .into_iter()
            .filter(|&i| i as usize >= n)
            .map(|i| i as usize - n)
            .collect();
        let nth = |v: usize| v.checked_sub(1).filter(|&j| j < k).map(|j| shown[j]);
        let pick = match idx {
            Variant::Bstr(x) => t.fields[s]
                .calc_items
                .iter()
                .position(|ci| ci.name.eq_ignore_ascii_case(x)),

            Variant::I4(v) => nth(*v as usize),
            Variant::I2(v) => nth(*v as usize),
            Variant::R8(v) => nth(*v as usize),
            _ => None,
        };
        let j = pick.ok_or_else(|| match idx {
            Variant::Bstr(_) => unable("CalculatedItems", "PivotField"),
            _ => OmError::bad_index(),
        })?;
        Ok(self.alloc(Object::Pivot(PivotRef::Item(
            b,
            t.name.clone(),
            s,
            (n + j) as u32,
        ))))
    }

    fn calc_items_member(
        &mut self,
        b: BookId,
        n: &str,
        s: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let i = self.table_idx(b, n)?;
        let mut t = self.pt(b, n)?;
        let c = self.cache_of(b, &t)?;
        match (m.name, m.kind) {
            ("Count", Get) => Ok(Variant::I4(t.fields[s].calc_items.len() as i32)),
            ("Item" | "_Default", _) => {
                let idx = a.get(0).ok_or_else(invalid_arg)?.clone();
                self.calc_item_obj(b, &t, &c, s, &idx)
            }
            ("Add", _) => {
                let (Some(Variant::Bstr(name)), Some(Variant::Bstr(formula))) =
                    (a.get(0), a.get(1))
                else {
                    return Err(invalid_arg());
                };
                let total = c.fields[s].items.len() + t.fields[s].calc_items.len();

                use xlcalc::pivot::Func;
                if t.data.iter().any(|d| {
                    matches!(
                        d.func,
                        Func::Average | Func::StdDev | Func::StdDevP | Func::Var | Func::VarP
                    )
                }) || name.is_empty()
                    || (0..total as u32)
                        .any(|k| Self::item_label(&t, &c, s, k).eq_ignore_ascii_case(name))
                {
                    return Err(OmError::app_defined());
                }
                let body = formula.trim().trim_start_matches('=').to_string();
                Self::check_item_formula(&t, &c, s, &body, None)?;
                t.fields[s].calc_items.push(xlcalc::pivot::CalcItem {
                    name: name.clone(),
                    formula: body,
                });
                self.pt_commit(b, i, t)?;
                Ok(self.alloc(Object::Pivot(PivotRef::Item(
                    b,
                    n.to_string(),
                    s,
                    total as u32,
                ))))
            }
            _ => Err(unsupported("CalculatedItems", m)),
        }
    }

    fn items_member(
        &mut self,
        b: BookId,
        n: &str,
        s: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let t = self.pt(b, n)?;
        let c = self.cache_of(b, &t)?;
        match (m.name, m.kind) {
            ("Count", Get) => Ok(Variant::I4(
                (c.fields[s].items.len() + t.fields[s].calc_items.len()) as i32,
            )),
            ("Item" | "_Default", _) => {
                let idx = a.get(0).ok_or_else(invalid_arg)?;
                let it = Self::item_arg(&t, &c, s, idx)
                    .ok_or_else(|| unable("PivotItems", "PivotField"))?;
                Ok(self.alloc(Object::Pivot(PivotRef::Item(b, t.name.clone(), s, it))))
            }
            _ => Err(unsupported("PivotItems", m)),
        }
    }

    fn item_member(
        &mut self,
        b: BookId,
        n: &str,
        s: usize,
        it: u32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let i = self.table_idx(b, n)?;
        let mut t = self.pt(b, n)?;
        let c = self.cache_of(b, &t)?;
        match (m.name, m.kind) {
            ("SourceName", Get) if (it as usize) < c.fields[s].items.len() => {
                Ok(Variant::Bstr(c.fields[s].items[it as usize].label.clone()))
            }
            ("Name" | "Caption", Put) if it < u32::MAX - 1 => {
                let Some(Variant::Bstr(new)) = &a.put else {
                    return Err(invalid_arg());
                };
                Self::rename_item(&mut t, &c, s, it, new)?;
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("Name" | "Caption" | "Value" | "SourceName", Get) => Ok(Variant::Bstr(match it {
                u32::MAX => "(All)".to_string(),
                x if x == u32::MAX - 1 => "(Multiple Items)".to_string(),
                _ => Self::item_label(&t, &c, s, it),
            })),
            ("Position", Get) if it < u32::MAX - 1 => {
                let order = display_order(&t, &c, s);
                let p = order
                    .iter()
                    .position(|x| *x == it)
                    .ok_or_else(invalid_arg)?;
                Ok(Variant::R8(p as f64 + 1.0))
            }
            ("Position", Put) if it < u32::MAX - 1 => {
                let to = match &a.put {
                    Some(Variant::R8(x)) => *x as usize,
                    Some(Variant::I4(x)) => *x as usize,
                    _ => return Err(invalid_arg()),
                };
                let mut order = display_order(&t, &c, s);
                if to < 1 || to > order.len() {
                    return Err(invalid_arg());
                }
                let p = order
                    .iter()
                    .position(|x| *x == it)
                    .ok_or_else(invalid_arg)?;
                let x = order.remove(p);
                order.insert(to - 1, x);
                t.fields[s].sort = Sort::Manual;
                t.fields[s].manual = Some(order);
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("IsCalculated", Get) => Ok(Variant::Bool(
                it as usize >= c.fields[s].items.len() && it < u32::MAX - 1,
            )),
            ("Formula", Get) if (it as usize) >= c.fields[s].items.len() && it < u32::MAX - 1 => {
                let j = it as usize - c.fields[s].items.len();
                let body = &t.fields[s].calc_items[j].formula;

                Ok(Variant::Bstr(if body.is_empty() {
                    String::new()
                } else {
                    format!("={body}")
                }))
            }
            ("Formula", Put) if (it as usize) >= c.fields[s].items.len() && it < u32::MAX - 1 => {
                let j = it as usize - c.fields[s].items.len();
                let Some(Variant::Bstr(x)) = &a.put else {
                    return Err(OmError::unable_to_set("Formula", "PivotItem"));
                };
                let body = x.trim().trim_start_matches('=').to_string();
                Self::check_item_formula(&t, &c, s, &body, Some(j))?;
                t.fields[s].calc_items[j].formula = body;
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("Delete", _) if (it as usize) >= c.fields[s].items.len() && it < u32::MAX - 1 => {
                let n0 = c.fields[s].items.len();
                let j = it as usize - n0;
                let pf = &mut t.fields[s];
                pf.calc_items.remove(j);
                pf.hidden.retain(|x| *x != it as usize);
                for x in &mut pf.hidden {
                    if *x > it as usize {
                        *x -= 1;
                    }
                }
                if pf.page_item == Some(it) {
                    pf.page_item = None;
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            ("Visible", Get) => Ok(Variant::Bool(!t.fields[s].hidden.contains(&(it as usize)))),
            ("Visible", Put) => {
                let vis = self.put_bool(a)?;
                let h = &mut t.fields[s].hidden;
                h.retain(|x| *x != it as usize);
                if !vis {
                    h.push(it as usize);
                }
                self.pt_commit(b, i, t)?;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("PivotItem", m)),
        }
    }
}

fn default_func(c: &PivotCache, field: usize) -> Func {
    let all_num = c
        .records
        .iter()
        .all(|r| matches!(r[field], Value::Number(_) | Value::Blank));
    if all_num {
        Func::Sum
    } else {
        Func::Count
    }
}

const AFFECTS: &str = "We can't make this change for the selected cells because it will affect a PivotTable. Use the field list to change the report. If you are trying to insert or delete cells, move the PivotTable and try again.";

impl Application<'_> {
    pub(crate) fn pivot_guard(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rect: Rect,
        m: &'static Member,
        a: &Args,
    ) -> Result<(), OmError> {
        use InvokeKind::*;
        let Ok(bk) = self.book(book) else {
            return Ok(());
        };
        let wb = &bk.wb;
        if wb.pivots().tables.is_empty() {
            return Ok(());
        }
        let affects = || OmError::app_defined_with(AFFECTS);
        match (m.name, m.kind) {
            ("Value" | "Value2" | "_Default" | "Formula" | "Formula2", Put) => {
                let Some(ti) = wb.pivot_meeting(sheet, &rect) else {
                    return Ok(());
                };
                let t = &wb.pivots().tables[ti];
                if t.ranges.data_body.is_some_and(|d| d.intersects(&rect)) {
                    return Err(OmError::app_defined_with(
                        "We can't change this part of the PivotTable.",
                    ));
                }
                if matches!(&a.put, Some(Variant::Bstr(s)) if s.starts_with('=')) {
                    return Err(OmError::app_defined_with(
                        "Cannot enter a formula for an item or field name in a PivotTable report.",
                    ));
                }
            }
            ("ClearContents" | "Clear", Func) => {
                if let Some(ti) = wb.pivot_meeting(sheet, &rect) {
                    let w = wb.pivots().tables[ti].written.unwrap_or(rect);
                    if !covers(&rect, &w) {
                        return Err(affects());
                    }
                    self.book_mut(book)?.wb.pivot_forget(ti, m.name == "Clear");
                }
            }
            ("Insert" | "Delete", Func) => {
                let Some(ti) = wb.pivot_meeting(sheet, &rect) else {
                    let dir = a.i32(0).ok().flatten();
                    let cut = wb.pivots().tables.iter().any(|t| {
                        let Some(w) = t.written.filter(|_| t.sheet == sheet) else {
                            return false;
                        };
                        match dir {
                            Some(-4159 | -4161) => {
                                w.c1 >= rect.c0
                                    && w.r1 >= rect.r0
                                    && w.r0 <= rect.r1
                                    && (w.r0 < rect.r0 || w.r1 > rect.r1)
                            }
                            Some(-4162 | -4121) => {
                                w.r1 >= rect.r0
                                    && w.c1 >= rect.c0
                                    && w.c0 <= rect.c1
                                    && (w.c0 < rect.c0 || w.c1 > rect.c1)
                            }
                            _ => false,
                        }
                    });
                    return if cut { Err(affects()) } else { Ok(()) };
                };
                let w = wb.pivots().tables[ti].written.unwrap_or(rect);
                let whole = rect.c0 == 0 || rect.r0 == 0;
                if m.name == "Insert" && rect.r0 <= w.r0 && rect.c0 <= w.c0 {
                    return Ok(());
                }
                if m.name == "Delete" && !whole && covers(&rect, &w) {
                    self.book_mut(book)?.wb.pivot_forget(ti, true);
                    return Ok(());
                }
                return Err(affects());
            }
            ("AutoFilter", Func) => {
                if wb.pivot_meeting(sheet, &rect).is_some() {
                    return Err(OmError::app_defined_with(
                        "AutoFilter method of Range class failed",
                    ));
                }
            }
            ("Cut" | "Copy", Func) => {
                if let Some(Variant::Dispatch(Some(id))) = a.get(0) {
                    if let Some(Object::Range(dest)) = self.arena.get(*id) {
                        let dest = *dest;
                        let (ds, dr) = self.resolve(&dest)?;
                        let hit = self.book(book)?.wb.pivot_meeting(ds, &dr);
                        if hit.is_some() {
                            return Err(affects());
                        }
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }
}

fn covers(outer: &Rect, inner: &Rect) -> bool {
    outer.r0 <= inner.r0 && outer.c0 <= inner.c0 && outer.r1 >= inner.r1 && outer.c1 >= inner.c1
}
