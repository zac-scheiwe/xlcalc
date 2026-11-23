use xlcalc::chart_src::series_from_block;
use xlcalc::drawing::{BagVal, Chart, HideTrack, Part, Series};
use xlcalc::{CellAddr, Rect, SheetId, Value, Workbook};
use xlcalc_variant::{SafeArray, VarType, Variant};

use crate::addr::{parse_range_text, quote_sheet, range_text, AddressStyle};
use crate::app::{unsupported, Application};
use crate::conv::Args;
use crate::error::OmError;
use crate::shapes::{DrawKind, DrawRef};
use crate::typelib::{InvokeKind, Member};

fn abs_style() -> AddressStyle {
    AddressStyle {
        row_abs: true,
        col_abs: true,
        r1c1: false,
        relative_to: (0, 0),
    }
}

pub(crate) fn part_text(wb: &Workbook, p: &Part, is_name: bool) -> String {
    match p {
        Part::None | Part::Dead if !is_name => String::new(),
        Part::None => String::new(),
        Part::Dead => "#REF!".to_string(),
        Part::External(t) => t.clone(),
        Part::Text(t) => {
            if is_name && !t.starts_with('{') {
                format!("\"{t}\"")
            } else {
                t.clone()
            }
        }
        Part::Refs(v) => {
            let one = |(s, r): &(SheetId, Rect)| {
                let name = wb.sheet(*s).map(|x| x.name.to_string()).unwrap_or_default();
                let q = quote_sheet(&name);
                let q = if q == name {
                    xlcalc::testing::sheet_name_text(&name)
                } else {
                    q
                };
                format!("{q}!{}", range_text(*r, &abs_style()))
            };
            if v.len() == 1 {
                one(&v[0])
            } else {
                format!("({})", v.iter().map(one).collect::<Vec<_>>().join(","))
            }
        }
    }
}

pub(crate) struct HideView<'a> {
    wb: &'a Workbook,
    snap: Option<&'a std::collections::BTreeSet<(SheetId, bool, u32)>>,
}

impl<'a> HideView<'a> {
    pub(crate) fn new(wb: &'a Workbook, chart: &'a Chart) -> Self {
        let snap = match &chart.hide_track {
            HideTrack::Frozen(s) => Some(s),
            _ => None,
        };
        HideView { wb, snap }
    }

    pub(crate) fn live(wb: &'a Workbook) -> Self {
        HideView { wb, snap: None }
    }

    pub(crate) fn row(&self, s: SheetId, row: u32) -> bool {
        match self.snap {
            Some(x) => x.contains(&(s, true, row)),
            None => self.wb.row_hidden(s, row),
        }
    }

    pub(crate) fn col(&self, s: SheetId, col: u32) -> bool {
        match self.snap {
            Some(x) => x.contains(&(s, false, col)),
            None => self.wb.col_hidden(s, col),
        }
    }
}

fn hide_snapshot(wb: &Workbook, chart: &Chart) -> std::collections::BTreeSet<(SheetId, bool, u32)> {
    let mut out = std::collections::BTreeSet::new();
    for ser in &chart.series {
        for p in [&ser.name, &ser.x, &ser.values] {
            let Part::Refs(v) = p else { continue };
            for (s, r) in v {
                for row in r.r0..=r.r1.min(r.r0 + 20_000) {
                    if wb.row_hidden(*s, row) {
                        out.insert((*s, true, row));
                    }
                }
                for col in r.c0..=r.c1.min(r.c0 + 20_000) {
                    if wb.col_hidden(*s, col) {
                        out.insert((*s, false, col));
                    }
                }
            }
        }
    }
    out
}

fn x_all_hidden(hv: &HideView, x: &Part, visible_only: bool) -> bool {
    let Part::Refs(v) = x else { return false };
    visible_only
        && !v.is_empty()
        && v.iter().all(|(s, r)| {
            (r.r0..=r.r1).all(|row| hv.row(*s, row)) || (r.c0..=r.c1).all(|col| hv.col(*s, col))
        })
}

pub(crate) fn series_formula(wb: &Workbook, s: &Series, order: usize) -> String {
    let mut values = part_text(wb, &s.values, false);
    if s.fmt.values_ranged && !values.contains(':') && !values.starts_with('(') {
        if let Some(i) = values.find('!') {
            let cell = values[i + 1..].to_string();
            values = format!("{values}:{cell}");
        }
    }
    format!(
        "=SERIES({},{},{},{})",
        part_text(wb, &s.name, true),
        part_text(wb, &s.x, false),
        values,
        order
    )
}

fn absolute_r1c1(f: &str) -> String {
    let b: Vec<char> = f.chars().collect();
    let (mut out, mut i, mut quoted) = (String::new(), 0, false);
    while i < b.len() {
        if b[i] == '"' {
            quoted = !quoted;
        }
        if !quoted && b[i] == '$' {
            let mut j = i + 1;
            let mut col = 0u32;
            while j < b.len() && b[j].is_ascii_uppercase() {
                col = col * 26 + (b[j] as u32 - 'A' as u32 + 1);
                j += 1;
            }
            if col > 0 && j < b.len() && b[j] == '$' {
                let mut k = j + 1;
                let mut row = String::new();
                while k < b.len() && b[k].is_ascii_digit() {
                    row.push(b[k]);
                    k += 1;
                }
                if !row.is_empty() {
                    out.push_str(&format!("R{row}C{col}"));
                    i = k;
                    continue;
                }
            }
        }
        out.push(b[i]);
        i += 1;
    }
    out
}

fn split_args(t: &str) -> Vec<String> {
    let (mut out, mut cur) = (Vec::new(), String::new());
    let (mut depth, mut quoted) = (0i32, false);
    for c in t.chars() {
        match c {
            '"' => {
                quoted = !quoted;
                cur.push(c);
            }
            '(' | '{' if !quoted => {
                depth += 1;
                cur.push(c);
            }
            ')' | '}' if !quoted => {
                depth -= 1;
                cur.push(c);
            }
            ',' if !quoted && depth == 0 => out.push(std::mem::take(&mut cur)),
            _ => cur.push(c),
        }
    }
    out.push(cur);
    out
}

pub(crate) fn parse_part(wb: &Workbook, home: SheetId, t: &str) -> Result<Part, OmError> {
    let t = t.trim();
    if t.is_empty() {
        return Ok(Part::None);
    }
    if let Some(q) = t.strip_prefix('"').and_then(|x| x.strip_suffix('"')) {
        return Ok(Part::Text(q.replace("\"\"", "\"")));
    }
    if t.starts_with('{') {
        return Ok(Part::Text(t.to_string()));
    }
    let inner = t
        .strip_prefix('(')
        .and_then(|x| x.strip_suffix(')'))
        .unwrap_or(t);
    let mut areas = Vec::new();
    for piece in split_args(inner) {
        let (s, r) = parse_range_text(wb, home, piece.trim()).ok_or_else(OmError::app_defined)?;
        areas.push((s, r));
    }
    Ok(Part::Refs(areas))
}

type ParsedSeries = ([Part; 3], usize);

fn parse_series(wb: &Workbook, home: SheetId, f: &str) -> Result<ParsedSeries, OmError> {
    let t = f.trim();
    let body = t
        .get(..8)
        .filter(|h| h.eq_ignore_ascii_case("=SERIES("))
        .and_then(|_| t[8..].strip_suffix(')'))
        .ok_or_else(OmError::app_defined)?;
    let parts = split_args(body);
    if parts.len() != 4 {
        return Err(OmError::app_defined());
    }
    let parsed = [
        parse_part(wb, home, &parts[0])?,
        parse_part(wb, home, &parts[1])?,
        parse_part(wb, home, &parts[2])?,
    ];

    let order = match parts[3].trim().parse::<f64>() {
        Ok(o) if o.fract() == 0.0 && (0.0..=255.0).contains(&o) => o as usize,
        _ => return Err(OmError::exception(0x800A_03EC_u32 as i32, None)),
    };
    Ok((parsed, order))
}

pub(crate) const BETWEEN: &str = "Axis1.#between";

pub(crate) fn between_categories(c: &Chart) -> bool {
    match (c.bag.get("Axis1.AxisBetweenCategories"), c.bag.get(BETWEEN)) {
        (Some(BagVal::B(b)), _) => *b,
        (_, Some(BagVal::I(n))) => *n != 0,
        _ => crate::bag::family(crate::chartparts::axis_type(c)) != "area",
    }
}

const WAS_BUBBLE: &str = "Chart.#was_bubble";

fn c_was_bubble(c: &Chart) -> bool {
    matches!(c.bag.get(WAS_BUBBLE), Some(BagVal::I(1)))
}

fn retype_chart(c: &mut Chart, t: i32) {
    c.chart_type = t;

    c.bag.retain(|k, _| {
        k.starts_with("Chart.")
            || matches!(
                k.as_str(),
                "Axis1.BaseUnit"
                    | "Axis1.BaseUnitIsAuto"
                    | crate::chart_date::KEPT_BASE
                    | crate::chart_geom::CAT_CROSS
            )
    });
    if crate::chartparts::is_radar(t) {
        c.axes[0].grid_major = true;
        c.axes[1].reverse = false;
    }
    let bubble = matches!(t, 15 | 87 | 88);
    for s in &mut c.series {
        if matches!(t, 1 | 76 | 77) {
            s.fmt.marker_style = None;
        }
        s.chart_type = t;

        if s.fmt.marker_fg == Some(-2) {
            s.fmt.marker_fg = None;
        }
        if !bubble {
            s.fmt.bubbles = None;
        }
    }
}

fn move_series(c: &mut Chart, k: usize, target: usize) {
    for i in 0..c.series.len() {
        c.bag
            .entry(format!("Series{i}.PlotColorIndex"))
            .or_insert(BagVal::I(i as i32 + 1));
    }
    let mut order: Vec<usize> = (0..c.series.len()).collect();
    let moved = order.remove(k);
    order.insert(target, moved);
    let ser: Vec<_> = order.iter().map(|i| c.series[*i].clone()).collect();
    c.series = ser;
    let old = std::mem::take(&mut c.bag);
    for (key, v) in old {
        let moved_key = key
            .strip_prefix("Series")
            .and_then(|t| t.split_once('.'))
            .and_then(|(i, rest)| Some((i.parse::<usize>().ok()?, rest)))
            .and_then(|(i, rest)| {
                let to = order.iter().position(|o| *o == i)?;
                Some(format!("Series{to}.{rest}"))
            });
        c.bag.insert(moved_key.unwrap_or(key), v);
    }
}

pub(crate) fn cell_variant(v: &Value) -> Variant {
    match v {
        Value::Blank => Variant::Empty,
        Value::Number(n) => Variant::R8(*n),
        Value::Text(t) => Variant::Bstr(t.to_string()),
        Value::Bool(b) => Variant::Bool(*b),
        Value::Error(e) => Variant::Error(crate::conv::error_scode(*e)),
        _ => Variant::Empty,
    }
}

pub(crate) fn part_values(wb: &Workbook, p: &Part) -> Vec<Variant> {
    part_values_vis(&HideView::live(wb), p, false)
}

pub(crate) fn part_values_vis(hv: &HideView, p: &Part, visible_only: bool) -> Vec<Variant> {
    let wb = hv.wb;
    match p {
        Part::Refs(v) => {
            let mut out = Vec::new();
            for (s, r) in v {
                for row in r.r0..=r.r1 {
                    for col in r.c0..=r.c1 {
                        if visible_only && (hv.row(*s, row) || hv.col(*s, col)) {
                            continue;
                        }
                        out.push(cell_variant(&wb.value(CellAddr::new(*s, row, col))));
                    }
                }
            }
            out
        }
        Part::Text(t) if t.starts_with('{') => t
            .trim_matches(|c| c == '{' || c == '}')
            .split([',', ';'])
            .map(|x| {
                let x = x.trim();
                match x.parse::<f64>() {
                    Ok(n) => Variant::R8(n),
                    Err(_) => Variant::Bstr(x.trim_matches('"').to_string()),
                }
            })
            .collect(),
        _ => Vec::new(),
    }
}

pub(crate) fn series_x_values(wb: &Workbook, chart: &Chart, ser: &Series) -> Variant {
    let hv = HideView::new(wb, chart);
    let mut v = part_values_vis(&hv, &ser.x, chart.visible_only);

    let nv = if matches!(ser.values, Part::Dead) {
        1
    } else {
        part_values_vis(&hv, &ser.values, chart.visible_only).len()
    };
    if nv > 0 && v.len() > nv {
        v.truncate(nv);
    }
    if v.is_empty() && x_all_hidden(&hv, &ser.x, chart.visible_only) {
        vec_variant((0..nv).map(|_| Variant::Bstr(String::new())).collect())
    } else if v.is_empty() {
        let n = nv;
        vec_variant((1..=n).map(|i| Variant::R8(i as f64)).collect())
    } else if matches!(&ser.x, Part::Text(_)) {
        let mut out: Vec<Variant> = v
            .into_iter()
            .map(|x| match x {
                Variant::R8(n) => Variant::Bstr(xlcalc::format_general(n)),
                o => o,
            })
            .collect();
        while out.len() < nv {
            out.push(Variant::Bstr(String::new()));
        }
        vec_variant(out)
    } else if matches!(chart.chart_type, -4169 | 72..=75 | 15 | 87 | 88)
        && v.iter().any(|x| matches!(x, Variant::Bstr(_)))
    {
        vec_variant((1..=nv).map(|i| Variant::R8(i as f64)).collect())
    } else if v.iter().any(|x| {
        matches!(x, Variant::Bstr(s) if !s.is_empty())
            || matches!(x, Variant::Empty) && v.iter().all(|y| matches!(y, Variant::Empty))
    }) {
        let mut v: Vec<Variant> = v
            .into_iter()
            .map(|x| match x {
                Variant::R8(n) => Variant::Bstr(xlcalc::format_general(n)),
                Variant::Empty => Variant::Bstr(String::new()),
                o => o,
            })
            .collect();
        if v.len() < nv {
            v.resize(nv, Variant::Bstr(String::new()));
        }
        vec_variant(v)
    } else {
        vec_variant(v)
    }
}

fn vec_variant(items: Vec<Variant>) -> Variant {
    Variant::Array(Box::new(SafeArray {
        elem: VarType::Variant,
        bounds: vec![(1, items.len() as u32)],
        data: items,
    }))
}

impl Application<'_> {
    pub(crate) fn chart_of(&self, r: &DrawRef) -> Result<Chart, OmError> {
        self.shape_clone(r)?
            .chart
            .ok_or_else(OmError::object_required)
    }

    pub(crate) fn chart_edit<T>(
        &mut self,
        r: &DrawRef,
        f: impl FnOnce(&mut Workbook, &mut Chart) -> Result<T, OmError>,
    ) -> Result<T, OmError> {
        let wb = &mut self.book_mut(r.book)?.wb;
        let mut chart = wb
            .drawing(r.sheet)
            .and_then(|d| d.get(r.id))
            .and_then(|s| s.chart.clone())
            .ok_or_else(OmError::object_required)?;
        let out = f(wb, &mut chart)?;
        if let Some(s) = wb.drawing_mut(r.sheet).get_mut(r.id) {
            s.chart = Some(chart.clone());
        }
        self.keep_base_unit(r, &chart)?;
        Ok(out)
    }

    pub(crate) fn keep_base_unit(&mut self, r: &DrawRef, c: &Chart) -> Result<(), OmError> {
        if matches!(c.bag.get("Axis1.BaseUnitIsAuto"), Some(BagVal::B(false))) {
            return Ok(());
        }
        let Some(base) = self.date_axis(r, c).ok().flatten().and_then(|d| d.base) else {
            return Ok(());
        };
        if c.bag.get(crate::chart_date::KEPT_BASE) == Some(&BagVal::I(base)) {
            return Ok(());
        }
        let wb = &mut self.book_mut(r.book)?.wb;
        if let Some(ch) = wb
            .drawing_mut(r.sheet)
            .get_mut(r.id)
            .and_then(|s| s.chart.as_mut())
        {
            ch.bag
                .insert(crate::chart_date::KEPT_BASE.into(), BagVal::I(base));

            if !Self::scatter_first(ch) {
                ch.bag
                    .insert(crate::chart_geom::CAT_CROSS.into(), BagVal::I(1));
            }
        }
        Ok(())
    }

    pub(crate) fn chart_draw_member(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        self.chart_undo_wrap(r, m, |s| s.chart_draw_member_inner(r, m, a))
    }

    pub(crate) fn chart_undo_wrap(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        f: impl FnOnce(&mut Self) -> Result<Variant, OmError>,
    ) -> Result<Variant, OmError> {
        if !matches!(m.kind, InvokeKind::Put | InvokeKind::PutRef) {
            return f(self);
        }
        let before = self.chart_of(r).ok();
        let res = f(self);
        if let Some(before) = before {
            self.chart_undo_after(r, m, before, &res)?;
        }
        res
    }

    fn chart_undo_after(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        mut before: Chart,
        res: &Result<Variant, OmError>,
    ) -> Result<(), OmError> {
        const PANICS: [&str; 4] = [
            "PictureUnit",
            "PictureUnit2",
            "BubbleSizes",
            "SecondPlotSize",
        ];
        let now = self.chart_of(r)?;
        let panics = res.as_ref().err().is_some_and(|e| {
            e.hresult == crate::error::DISP_E_EXCEPTION
                && PANICS.contains(&m.name)
                && matches!(
                    r.kind,
                    DrawKind::Series(_) | DrawKind::Sub(crate::chartparts::Sub::Group(_))
                )
        });
        let unbalanced = res.as_ref().err().is_some_and(|e| {
            e.scode == 0x8000_4005_u32 as i32
                && m.name == "ScaleType"
                && r.kind == DrawKind::Sub(crate::chartparts::Sub::Axis(1))
        });
        let change = res.is_ok() && {
            let mut a = now.clone();
            a.undo = Default::default();
            before.undo = Default::default();
            a != before
        };
        if !(panics || unbalanced || (change && now.undo.open && now.undo.snap.is_none())) {
            return Ok(());
        }
        self.chart_edit(r, |_, c| {
            if unbalanced {
                c.undo.open = true;
            } else if panics {
                if let Some(snap) = c.undo.snap.take() {
                    let laid = c.bag.get("#laid").cloned();
                    let undo = std::mem::take(&mut c.undo);
                    *c = *snap;
                    c.undo = undo;
                    c.bag.remove("#laid");
                    if let Some(v) = laid {
                        c.bag.insert("#laid".into(), v);
                    }
                }
            } else {
                c.undo.snap = Some(Box::new(before));
            }
            Ok(())
        })
    }

    fn chart_draw_member_inner(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let put = m.kind == Put || m.kind == PutRef;
        let mut chart = self.chart_of(r)?;

        let reads_series = !put
            && match r.kind {
                DrawKind::Series(_) => true,
                DrawKind::SeriesColl => m.name == "Count",
                _ => false,
            };
        if chart.hide_track == HideTrack::Armed && reads_series {
            let snap = hide_snapshot(&self.book(r.book)?.wb, &chart);
            chart.hide_track = HideTrack::Frozen(snap);
            let track = chart.hide_track.clone();
            self.chart_edit(r, |_, c| {
                c.hide_track = track;
                Ok(())
            })?;
        } else if chart.hide_track == HideTrack::Armed
            && r.kind == DrawKind::Chart
            && m.name == "PlotBy"
            && !put
        {
            chart.hide_track = HideTrack::Live;
            self.chart_edit(r, |_, c| {
                c.hide_track = HideTrack::Live;
                Ok(())
            })?;
        }

        if m.kind == InvokeKind::Get
            && !matches!(
                m.ret,
                crate::typelib::Ty::Object(_) | crate::typelib::Ty::Dispatch
            )
            && !chart.bag.contains_key("#laid")
        {
            chart
                .bag
                .insert("#laid".into(), xlcalc::drawing::BagVal::I(1));
            self.chart_edit(r, |_, c| {
                c.bag.insert("#laid".into(), xlcalc::drawing::BagVal::I(1));
                Ok(())
            })?;
        }
        if chart.pb_stale && matches!(r.kind, DrawKind::Series(_) | DrawKind::SeriesColl) {
            self.chart_edit(r, |_, c| {
                c.plot_by = 0;
                c.pb_stale = false;
                Ok(())
            })?;
            chart.plot_by = 0;
            chart.pb_stale = false;
        }

        if r.kind == DrawKind::Chart
            && m.name == "Copy"
            && self
                .book(r.book)?
                .wb
                .sheet(r.sheet)
                .is_some_and(|s| s.kind == xlcalc::SheetKind::Worksheet)
        {
            return Err(OmError::exception(
                0x800A_03EC_u32 as i32,
                Some("Copy method of Chart class failed"),
            ));
        }
        if r.kind == DrawKind::Chart {
            if let Some(res) = self.chart_extra(r, m, a, &chart) {
                return res;
            }
        }
        if let DrawKind::Series(k) = r.kind {
            if let Some(res) = self.series_member(&chart, r, k, m, a) {
                return res;
            }
        }
        match r.kind {
            DrawKind::Chart => match (m.name, put) {
                ("ChartType", false) => {
                    let mixed = chart.series.windows(2).any(|w| {
                        w[0].chart_type != w[1].chart_type || w[0].axis_group != w[1].axis_group
                    });
                    Ok(Variant::I4(if mixed { -4111 } else { chart.chart_type }))
                }
                ("ChartType", true) => {
                    let t = a
                        .put
                        .as_ref()
                        .map(|v| crate::controls::variant_i4(v, a.lcid))
                        .transpose()?
                        .ok_or_else(OmError::type_mismatch)?;
                    self.chart_edit(r, |_, c| {
                        if let Some(hr) =
                            crate::chart_types::chart_type_put_error(t, c.series.len())
                        {
                            return Err(OmError::exception(hr as i32, None));
                        }
                        retype_chart(c, crate::chart_types::canonical(t));

                        for s in &mut c.series {
                            s.axis_group = 1;
                        }
                        Ok(Variant::Empty)
                    })
                }
                ("PlotBy", false) => Ok(Variant::I4(chart.plot_by)),
                ("HasPivotFields", false) => Ok(Variant::Bool(chart.pivot.is_some())),
                ("PivotLayout", false) => match chart.pivot {
                    Some(n) => Ok(self.alloc(crate::app::Object::Pivot(
                        crate::pivot::PivotRef::Layout(r.book, n),
                    ))),
                    None => Ok(Variant::Dispatch(None)),
                },
                ("HasTitle", false) => Ok(Variant::Bool(chart.has_title)),
                ("HasLegend", false) => Ok(Variant::Bool(chart.has_legend)),
                ("HasTitle" | "HasLegend", true) => {
                    let v = match a.put.as_ref() {
                        Some(Variant::Bool(x)) => *x,
                        _ => return Err(OmError::type_mismatch()),
                    };
                    let title = m.name == "HasTitle";
                    self.chart_edit(r, |_, c| {
                        if title {
                            c.has_title = v;
                            c.bag.insert(
                                "#title_off".into(),
                                xlcalc::drawing::BagVal::I(i32::from(!v)),
                            );
                        } else {
                            c.has_legend = v;
                        }
                        Ok(Variant::Empty)
                    })
                }
                ("SeriesCollection", _) => {
                    let coll = self.alloc(crate::app::Object::Draw(DrawRef {
                        kind: DrawKind::SeriesColl,
                        ..*r
                    }));
                    match a.get(0) {
                        Some(i) if m.kind == Func => {
                            let Variant::Dispatch(Some(id)) = coll else {
                                return Err(OmError::app_defined());
                            };
                            let Some(crate::app::Object::Draw(cr)) = self.arena.get(id).cloned()
                            else {
                                return Err(OmError::app_defined());
                            };
                            self.series_item(&cr, i, a.lcid)
                        }
                        _ => Ok(coll),
                    }
                }
                ("SetSourceData", _) => {
                    let (sheet, rect) = self.range_arg(a.get(0))?;
                    let fail = || OmError::exception(0x8000_4005_u32 as i32, None);
                    let by = match a.get(1) {
                        None => None,
                        Some(Variant::Null | Variant::Bool(_)) => return Err(fail()),
                        Some(Variant::Bstr(_)) => {
                            return Err(OmError::exception(0x8002_0005_u32 as i32, None))
                        }
                        Some(_) => a.i32(1)?,
                    };
                    if matches!(by, Some(x) if x != 1 && x != 2) {
                        return Err(fail());
                    }
                    let pivot = {
                        let wb = &self.book(r.book)?.wb;
                        wb.pivot_meeting(sheet, &rect)
                            .filter(|&ti| wb.pivots().tables[ti].ranges.table1 == Some(rect))
                            .map(|ti| wb.pivots().tables[ti].name.clone())
                    };
                    self.chart_edit(r, |wb, c| {
                        c.pivot = pivot;
                        let (by, series) =
                            series_from_block(wb, sheet, rect, by, c.chart_type, &c.series);
                        c.plot_by = by;
                        c.series = series;
                        c.source = Some((sheet, rect));
                        if c.has_title && c.title == "Chart Title" && c.title_ref.is_none() {
                            c.title = crate::chartparts::auto_title(wb, c);
                        }
                        Ok(Variant::Empty)
                    })
                }
                ("Parent", false) => Ok(self.alloc(crate::app::Object::Draw(DrawRef {
                    kind: DrawKind::ChartObject,
                    ..*r
                }))),
                _ => match self.bag_member(crate::bag::Store::Chart(r), "Chart", "Chart", m, a) {
                    Some(Ok(v)) => {
                        if put && m.name == "Type" {
                            let t = self.chart_of(r)?.bag.get("Chart.Type").cloned();
                            if matches!(t, Some(xlcalc::drawing::BagVal::I(4))) {
                                self.chart_edit(r, |_, c| {
                                    c.chart_type = 65;
                                    for s in &mut c.series {
                                        s.chart_type = 65;
                                    }
                                    c.bag.remove("Chart.SubType");
                                    Ok(())
                                })?;
                            }
                        }
                        Ok(v)
                    }
                    Some(res) => res,
                    None => Err(unsupported("_Chart", m)),
                },
            },
            DrawKind::SeriesColl => match (m.name, put) {
                ("Count", false) => {
                    let wb = &self.book(r.book)?.wb;
                    let pivot = chart.pivot.as_ref().and_then(|n| {
                        wb.pivots()
                            .tables
                            .iter()
                            .position(|t| t.name.eq_ignore_ascii_case(n))
                    });
                    Ok(Variant::I4(match pivot {
                        Some(ti) => wb.pivot_chart_series(ti) as i32,
                        None => visible_series(wb, &chart).len() as i32,
                    }))
                }
                ("Item" | "_Default", _) => match a.get(0) {
                    Some(i) => self.series_item(r, i, a.lcid),
                    None => Err(OmError::app_defined()),
                },
                ("Add", _) => {
                    let (sheet, rect) = match a.get(0) {
                        Some(Variant::Dispatch(_)) => self.range_arg(a.get(0))?,
                        Some(Variant::Bstr(t)) => {
                            let bad = || OmError::exception(0x8007_0057_u32 as i32, None);
                            let wb = &self.book(r.book)?.wb;
                            let t = t.trim().trim_start_matches('=');
                            r1c1_text(wb, r.sheet, t)
                                .or_else(|| crate::addr::parse_range_text(wb, r.sheet, t))
                                .ok_or_else(bad)?
                        }
                        Some(_) => return Err(OmError::exception(0x8007_0057_u32 as i32, None)),
                        None => return Err(OmError::exception(0x8002_0005_u32 as i32, None)),
                    };
                    let rowcol = match a.get(1) {
                        None | Some(Variant::Null) => None,
                        Some(_) => a.i32(1)?,
                    };
                    let flag = |i: usize| matches!(a.get(i), Some(Variant::Bool(true)));
                    let by_rows = match rowcol {
                        Some(1) => true,
                        Some(2) => false,
                        _ => rect.rows() < rect.cols(),
                    };

                    let room = if by_rows { rect.rows() } else { rect.cols() } > 1;
                    let (labels, cats) = (flag(2), flag(3) && room);
                    let block = |r0: u32, c0: u32, r1: u32, c1: u32| {
                        (r0 <= r1 && c0 <= c1)
                            .then(|| Part::Refs(vec![(sheet, Rect::new(r0, c0, r1, c1))]))
                    };
                    let inherited = chart.series.first().map_or(Part::None, |s| s.x.clone());
                    let t = chart.chart_type;
                    let mut made = Vec::new();

                    let bubble = matches!(t, 15 | 87);

                    let paired = bubble || c_was_bubble(&chart);
                    let step = if paired { 2 } else { 1 };
                    let mk = |name, x, values, bubbles| Series {
                        name,
                        x,
                        values,
                        chart_type: t,
                        axis_group: 1,
                        fmt: xlcalc::drawing::SeriesFmt {
                            bubbles,
                            ..Default::default()
                        },
                    };
                    if by_rows {
                        let (first, v0) = (rect.r0 + u32::from(cats), rect.c0 + u32::from(labels));
                        for row in (first..=rect.r1).step_by(step) {
                            let x = if cats {
                                block(rect.r0, v0, rect.r0, rect.c1).unwrap_or(Part::None)
                            } else {
                                inherited.clone()
                            };
                            let name = if labels {
                                block(row, rect.c0, row, rect.c0).unwrap_or(Part::None)
                            } else {
                                Part::None
                            };
                            let size = (bubble && row < rect.r1)
                                .then(|| block(row + 1, v0, row + 1, rect.c1))
                                .flatten();
                            if let Some(v) = block(row, v0, row, rect.c1) {
                                made.push(mk(name, x, v, size));
                            }
                        }
                    } else {
                        let (first, v0) = (rect.c0 + u32::from(cats), rect.r0 + u32::from(labels));
                        for col in (first..=rect.c1).step_by(step) {
                            let x = if cats {
                                block(v0, rect.c0, rect.r1, rect.c0).unwrap_or(Part::None)
                            } else {
                                inherited.clone()
                            };
                            let name = if labels {
                                block(rect.r0, col, rect.r0, col).unwrap_or(Part::None)
                            } else {
                                Part::None
                            };
                            let size = (bubble && col < rect.c1)
                                .then(|| block(v0, col + 1, rect.r1, col + 1))
                                .flatten();
                            if let Some(v) = block(v0, col, rect.r1, col) {
                                made.push(mk(name, x, v, size));
                            }
                        }
                    }
                    if made.is_empty() {
                        return Err(OmError::exception(0x8007_0057_u32 as i32, None));
                    }
                    let last = chart.series.len() + made.len() - 1;
                    self.chart_edit(r, |_, c| {
                        c.plot_by = 0;
                        c.series.extend(made);
                        Ok(Variant::Empty)
                    })?;
                    Ok(self.alloc(crate::app::Object::Draw(DrawRef {
                        kind: DrawKind::Series(last),
                        ..*r
                    })))
                }
                ("NewSeries", _) => {
                    let n = chart.series.len();
                    let t = chart.chart_type;
                    self.chart_edit(r, |_, c| {
                        c.plot_by = 0;
                        let number = Some(c.free_series_number());
                        c.series.push(Series {
                            name: Part::None,
                            x: Part::None,
                            values: Part::None,
                            chart_type: t,
                            axis_group: 1,
                            fmt: xlcalc::drawing::SeriesFmt {
                                number,
                                ..Default::default()
                            },
                        });
                        Ok(Variant::Empty)
                    })?;
                    Ok(self.alloc(crate::app::Object::Draw(DrawRef {
                        kind: DrawKind::Series(n),
                        ..*r
                    })))
                }
                ("_NewEnum", _) => {
                    let vis = visible_series(&self.book(r.book)?.wb, &chart);
                    let items = vis
                        .into_iter()
                        .map(|k| {
                            self.alloc(crate::app::Object::Draw(DrawRef {
                                kind: DrawKind::Series(k),
                                ..*r
                            }))
                        })
                        .collect();
                    Ok(self.alloc_enumerator(items))
                }
                _ => Err(unsupported("SeriesCollection", m)),
            },
            DrawKind::Series(k) => {
                let ser = chart
                    .series
                    .get(k)
                    .cloned()
                    .ok_or_else(OmError::object_required)?;
                let wb = &self.book(r.book)?.wb;
                match (m.name, put) {
                    ("Name", false) => Ok(Variant::Bstr(match &ser.name {
                        Part::None => format!("Series{}", ser.number(k)),
                        Part::Dead if matches!(ser.values, Part::Dead) => "#REF!".to_string(),
                        Part::Dead => format!("Series{}", ser.number(k)),
                        Part::Text(t) => t.clone(),
                        Part::Refs(_) => crate::chartparts::series_name_text(wb, k, &ser),
                        p => part_values(wb, p)
                            .first()
                            .map(|v| match v {
                                Variant::Bstr(s) => s.clone(),
                                Variant::R8(n) => xlcalc::format_general(*n),
                                _ => String::new(),
                            })
                            .unwrap_or_default(),
                    })),
                    ("Formula" | "FormulaLocal" | "FormulaR1C1" | "FormulaR1C1Local", false) => {
                        if matches!(&ser.name, Part::Dead)
                            || matches!(&ser.name, Part::Text(t) if t == "#REF!")
                            || matches!(ser.values, Part::Dead)
                        {
                            return Err(OmError::exception(0x800A_07E7_u32 as i32, None));
                        }
                        let mut ser = ser.clone();
                        if x_all_hidden(&HideView::new(wb, &chart), &ser.x, chart.visible_only) {
                            ser.x = Part::None;
                        }
                        let mut f = series_formula(wb, &ser, k + 1);

                        if matches!(chart.chart_type, 15 | 87 | 88) && f.ends_with(')') {
                            f.pop();
                            match &ser.fmt.bubbles {
                                Some(b @ Part::Refs(_)) => {
                                    f.push_str(&format!(",{})", part_text(wb, b, false)))
                                }
                                _ => {
                                    let n = part_values(wb, &ser.values).len();
                                    f.push_str(&format!(",{{{}}})", vec!["1"; n].join(",")));
                                }
                            }
                        }
                        Ok(Variant::Bstr(if m.name.contains("R1C1") {
                            absolute_r1c1(&f)
                        } else {
                            f
                        }))
                    }
                    ("IsFiltered", true)
                        if matches!(
                            a.put.as_ref(),
                            Some(Variant::Bool(true) | Variant::I4(-1))
                        ) =>
                    {
                        self.chart_edit(r, |_, c| {
                            c.series[k].fmt.filtered = true;
                            Ok(Variant::Empty)
                        })
                    }
                    ("Formula", true) => {
                        let f = match a.put.as_ref() {
                            Some(Variant::Bstr(f)) => f.clone(),
                            _ => return Err(OmError::type_mismatch()),
                        };
                        let ([n, x, v], order) = parse_series(wb, r.sheet, &f)?;
                        self.chart_edit(r, |_, c| {
                            let before = series_refs(c);

                            if !xlcalc::chart_src::is_xy(c.series[k].chart_type) {
                                let g = c.series[k].axis_group;
                                for s in c.series.iter_mut().filter(|s| {
                                    s.axis_group == g && !xlcalc::chart_src::is_xy(s.chart_type)
                                }) {
                                    s.x = x.clone();
                                }
                            }
                            let s = &mut c.series[k];
                            (s.name, s.x, s.values) = (n, x, v);

                            let target = order.clamp(1, c.series.len()) - 1;
                            if target != k {
                                move_series(c, k, target);
                            }
                            if order == 1 && k > 0 {
                                for s in &mut c.series {
                                    s.axis_group = 1;
                                }
                            }
                            if series_refs(c) != before && !still_plotted_by(c, c.plot_by) {
                                c.plot_by = 0;
                            }
                            Ok(Variant::Empty)
                        })
                    }
                    ("Values" | "XValues" | "Name", true) => {
                        let f = match a.put.as_ref() {
                            Some(Variant::Bstr(f)) => f.clone(),
                            _ => return Err(OmError::type_mismatch()),
                        };

                        let part = match parse_part(wb, r.sheet, f.trim_start_matches('=')) {
                            Err(_) if m.name == "Name" && !f.starts_with('=') => {
                                Part::Text(f.clone())
                            }
                            p => p?,
                        };

                        if m.name == "Values"
                            && matches!(&part, Part::Refs(v) if v.iter().any(|(_, q)| q.rows() > 1 && q.cols() > 1))
                        {
                            return Err(OmError::exception(0x800A_03EC_u32 as i32, None));
                        }
                        let which = m.name;
                        self.chart_edit(r, |_, c| {
                            let before = series_refs(c);
                            let s = &mut c.series[k];
                            match which {
                                "Values" => s.values = part,
                                "XValues" => s.x = part,
                                _ => s.name = part,
                            }
                            if series_refs(c) != before && !still_plotted_by(c, c.plot_by) {
                                c.plot_by = 0;
                            }
                            Ok(Variant::Empty)
                        })
                    }
                    ("Values", false) if matches!(ser.values, Part::Dead) => {
                        Ok(vec_variant(vec![Variant::R8(1.0)]))
                    }
                    ("Values", false) => Ok(vec_variant(
                        part_values_vis(
                            &HideView::new(wb, &chart),
                            &ser.values,
                            chart.visible_only,
                        )
                        .into_iter()
                        .map(|x| {
                            if matches!(x, Variant::Bstr(_)) && matches!(ser.values, Part::Refs(_))
                            {
                                Variant::R8(0.0)
                            } else {
                                x
                            }
                        })
                        .collect(),
                    )),
                    ("XValues", false) => Ok(series_x_values(wb, &chart, &ser)),
                    ("PlotOrder", false) => {
                        let fam = crate::chartparts::group_family(ser.chart_type);
                        let key = (ser.fmt.plot_rank, k);
                        let before = chart
                            .series
                            .iter()
                            .enumerate()
                            .filter(|(i, x)| {
                                x.axis_group == ser.axis_group
                                    && crate::chartparts::group_family(x.chart_type) == fam
                                    && (x.fmt.plot_rank, *i) < key
                            })
                            .count();
                        Ok(Variant::I4(before as i32 + 1))
                    }
                    ("ChartType", false) => {
                        let t = ser.chart_type;
                        let smooth = ser.fmt.smooth.unwrap_or(matches!(t, 72 | 73));
                        Ok(Variant::I4(match t {
                            -4169 if smooth => 72,
                            72 | 74 => {
                                if smooth {
                                    72
                                } else {
                                    74
                                }
                            }
                            73 | 75 => {
                                if smooth {
                                    73
                                } else {
                                    75
                                }
                            }
                            _ => t,
                        }))
                    }
                    ("AxisGroup", false) => Ok(Variant::I4(ser.axis_group)),

                    ("AxisGroup", true) => {
                        let g = if crate::chartparts::put_num(a)?.round_ties_even() == 2.0 {
                            2
                        } else {
                            1
                        };
                        self.chart_edit(r, |_, c| {
                            if g == 2 && c.series.iter().all(|s| s.axis_group != 2) {
                                c.bag.remove(xlcalc::drawing::SECONDARY_ZERO);
                            }
                            c.series[k].axis_group = g;
                            Ok(Variant::Empty)
                        })
                    }

                    ("PlotOrder", true) => {
                        let n = crate::chartparts::put_num(a)?.round_ties_even();
                        self.chart_edit(r, |_, c| {
                            let fam = crate::chartparts::group_family(c.series[k].chart_type);
                            let group: Vec<usize> = (0..c.series.len())
                                .filter(|i| {
                                    c.series[*i].axis_group == c.series[k].axis_group
                                        && crate::chartparts::group_family(c.series[*i].chart_type)
                                            == fam
                                        && !c.series[*i].fmt.filtered
                                })
                                .collect();
                            if n < 1.0 || n as usize > group.len() {
                                return Err(OmError::exception(0x800A_03EC_u32 as i32, None));
                            }
                            let target = group[n as usize - 1];
                            move_series(c, k, target);
                            Ok(Variant::Empty)
                        })
                    }

                    ("ChartType" | "Type", true) => {
                        let n = if m.name == "ChartType" {
                            let v = a.put.as_ref().unwrap_or(&Variant::Empty);
                            crate::controls::variant_i4(v, a.lcid)?
                        } else {
                            crate::chartparts::put_num(a)?.round_ties_even() as i32
                        };
                        let t = if m.name == "Type" {
                            match n {
                                1 => 1,
                                2 => 57,
                                3 => 51,
                                4 => 65,
                                5 => 5,
                                -4169 => -4169,
                                _ => return Err(OmError::exception(0x8000_4005_u32 as i32, None)),
                            }
                        } else if let Some(hr) = crate::chart_types::series_type_put_error(n) {
                            return Err(OmError::exception(hr as i32, None));
                        } else {
                            crate::chart_types::canonical(n)
                        };
                        self.chart_edit(r, |_, c| {
                            let bubble = |t: i32| matches!(t, 15 | 87 | 88);
                            if bubble(c.chart_type) && !bubble(t) && m.name == "ChartType" {
                                retype_chart(c, t);
                                c.bag.insert(WAS_BUBBLE.into(), BagVal::I(1));
                                let last = c.series.iter().map(|s| s.fmt.plot_rank).max();
                                for s in &mut c.series {
                                    if s.axis_group == 2 {
                                        s.axis_group = 1;
                                        s.fmt.plot_rank = last.unwrap_or(0) + 1;
                                    }
                                }
                                return Ok(Variant::Empty);
                            }
                            c.series[k].chart_type = t;

                            let between = c
                                .series
                                .iter()
                                .any(|s| crate::bag::family(s.chart_type) != "area");
                            c.bag.insert(BETWEEN.into(), BagVal::I(i32::from(between)));
                            if matches!(t, 1 | 76 | 77) {
                                c.series[k].fmt.marker_style = None;
                            }
                            c.series[k].fmt.smooth = None;
                            let prefix = format!("Series{k}.");

                            let keep = c.bag.get(&format!("{prefix}PlotColorIndex")).cloned();
                            c.bag.retain(|key, _| !key.starts_with(&prefix));
                            if let Some(v) = keep {
                                c.bag.insert(format!("{prefix}PlotColorIndex"), v);
                            }

                            c.bag.retain(|key, _| !key.starts_with("ChartGroup"));
                            if m.name == "Type" {
                                c.bag.insert(format!("{prefix}Type"), BagVal::I(n));
                            }
                            Ok(Variant::Empty)
                        })
                    }
                    ("Delete", _) => self.chart_edit(r, |_, c| {
                        for (j, s) in c.series.iter_mut().enumerate() {
                            s.fmt.number = Some(s.number(j) as u32);
                        }
                        let cat = crate::chartparts::axis_type(c);
                        c.series.remove(k);
                        if !xlcalc::chart_src::is_xy(cat)
                            && xlcalc::chart_src::is_xy(crate::chartparts::axis_type(c))
                        {
                            c.bag
                                .insert(crate::chartparts::KEPT_CAT.into(), BagVal::I(cat));

                            c.bag.remove(crate::chart_geom::CAT_CROSS);
                        }
                        Ok(Variant::Bool(true))
                    }),
                    _ => {
                        let slot = format!("Series{k}");
                        match self.bag_member(crate::bag::Store::Chart(r), "Series", &slot, m, a) {
                            Some(res) => res,
                            None => Err(unsupported("Series", m)),
                        }
                    }
                }
            }
            _ => Err(unsupported(r.kind.interface_name(), m)),
        }
    }

    fn series_item(&mut self, r: &DrawRef, index: &Variant, lcid: u32) -> Result<Variant, OmError> {
        let chart = self.chart_of(r)?;
        let k = match index {
            Variant::Bstr(name) => {
                let mut found = None;
                for (i, s) in chart.series.iter().enumerate() {
                    let wb = &self.book(r.book)?.wb;
                    let n = match &s.name {
                        Part::Text(t) => t.clone(),
                        Part::None => format!("Series{}", s.number(i)),
                        p => match part_values(wb, p).first() {
                            Some(Variant::Bstr(t)) => t.clone(),
                            _ => String::new(),
                        },
                    };
                    if n.eq_ignore_ascii_case(name) {
                        found = Some(i);
                        break;
                    }
                }
                found.ok_or_else(|| crate::chartparts::range_err("Parameter not valid"))?
            }
            Variant::Null | Variant::Bool(_) => {
                return Err(crate::chartparts::range_err("Parameter not valid"))
            }
            v => {
                let n = crate::controls::num_arg(v, lcid)?.trunc();
                let vis = visible_series(&self.book(r.book)?.wb, &chart);
                if n < 1.0 || n as usize > vis.len() {
                    return Err(crate::chartparts::range_err("Parameter not valid"));
                }
                vis[n as usize - 1]
            }
        };
        Ok(self.alloc(crate::app::Object::Draw(DrawRef {
            kind: DrawKind::Series(k),
            ..*r
        })))
    }
}

fn visible_series(wb: &Workbook, chart: &Chart) -> Vec<usize> {
    let mut v = unhidden_series(wb, chart);
    v.retain(|&k| !chart.series[k].fmt.filtered);
    v
}

fn unhidden_series(wb: &Workbook, chart: &Chart) -> Vec<usize> {
    let all = || (0..chart.series.len()).collect::<Vec<_>>();
    if !chart.visible_only || chart.pivot.is_some() {
        return all();
    }

    let known = chart.plot_by != 0 && chart.source.is_some();
    let hv = HideView::new(wb, chart);
    let vis: Vec<usize> = (0..chart.series.len())
        .filter(|&k| {
            let Part::Refs(v) = &chart.series[k].values else {
                return true;
            };
            let [(s, r)] = v.as_slice() else { return true };
            let by_cols = if known {
                chart.plot_by == 2
            } else if r.r0 == r.r1 && r.c0 < r.c1 {
                false
            } else if r.c0 == r.c1 && r.r0 < r.r1 {
                true
            } else {
                return true;
            };
            if by_cols {
                !(r.c0 == r.c1 && hv.col(*s, r.c0))
            } else {
                !(r.r0 == r.r1 && hv.row(*s, r.r0))
            }
        })
        .collect();
    if vis.is_empty() {
        all()
    } else {
        vis
    }
}

pub(crate) fn external_text(wb: &Workbook, p: &Part, book: &str) -> Option<String> {
    let Part::Refs(v) = p else { return None };
    let one = |(s, r): &(SheetId, Rect)| {
        let name = wb.sheet(*s).map(|x| x.name.to_string()).unwrap_or_default();
        let q = quote_sheet(&name);
        let cells = range_text(*r, &abs_style());
        match q.strip_prefix('\'') {
            Some(rest) => format!("'[{book}]{rest}!{cells}"),
            None => format!("[{book}]{q}!{cells}"),
        }
    };
    Some(if v.len() == 1 {
        one(&v[0])
    } else {
        format!("({})", v.iter().map(one).collect::<Vec<_>>().join(","))
    })
}

fn r1c1_text(wb: &Workbook, default: SheetId, text: &str) -> Option<(SheetId, Rect)> {
    let (sheet, body) = match text.rfind('!') {
        Some(i) => {
            let name = text[..i].trim_matches('\'').replace("''", "'");
            (wb.sheet_by_name(&name)?.id, &text[i + 1..])
        }
        None => (default, text),
    };
    let cell = |t: &str| -> Option<(u32, u32)> {
        let t = t.strip_prefix(['R', 'r'])?;
        let (row, col) = t.split_once(['C', 'c'])?;
        let (row, col): (u32, u32) = (row.parse().ok()?, col.parse().ok()?);
        (row >= 1 && col >= 1).then(|| (row - 1, col - 1))
    };
    let (a, b) = body.split_once(':').unwrap_or((body, body));
    let (a, b) = (cell(a)?, cell(b)?);
    Some((
        sheet,
        Rect::new(a.0.min(b.0), a.1.min(b.1), a.0.max(b.0), a.1.max(b.1)),
    ))
}

fn still_plotted_by(c: &Chart, by: i32) -> bool {
    if !matches!(by, 1 | 2) || c.series.is_empty() {
        return false;
    }
    let cols = by == 2;

    let mut prev: Option<(SheetId, u32, u32, u32)> = None;
    for s in &c.series {
        let Part::Refs(v) = &s.values else {
            return false;
        };
        let [(sh, q)] = v.as_slice() else {
            return false;
        };
        let (line, a, b, l2) = if cols {
            (q.c0, q.r0, q.r1, q.c1)
        } else {
            (q.r0, q.c0, q.c1, q.r1)
        };
        if line != l2 || b <= a || s.x != c.series[0].x {
            return false;
        }
        if let Some((psh, pl, pa, pb)) = prev {
            if psh != *sh || line <= pl || (a, b) != (pa, pb) {
                return false;
            }
        }
        match &s.name {
            Part::None => {}
            Part::Refs(n) => {
                let [(nsh, nq)] = n.as_slice() else {
                    return false;
                };
                let at = if cols {
                    (a.wrapping_sub(1), line)
                } else {
                    (line, a.wrapping_sub(1))
                };
                if nsh != sh || (nq.r0, nq.c0) != at || nq.r1 != nq.r0 || nq.c1 != nq.c0 {
                    return false;
                }
            }
            _ => return false,
        }
        prev = Some((*sh, line, a, b));
    }
    true
}

fn series_refs(c: &Chart) -> Vec<(Part, Part, Part)> {
    c.series
        .iter()
        .map(|s| (s.name.clone(), s.x.clone(), s.values.clone()))
        .collect()
}
