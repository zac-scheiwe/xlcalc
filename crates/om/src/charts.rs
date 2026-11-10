use xlcalc::drawing::{Chart, Part, Series};
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
                format!("{}!{}", quote_sheet(&name), range_text(*r, &abs_style()))
            };
            if v.len() == 1 {
                one(&v[0])
            } else {
                format!("({})", v.iter().map(one).collect::<Vec<_>>().join(","))
            }
        }
    }
}

pub(crate) fn series_formula(wb: &Workbook, s: &Series, order: usize) -> String {
    format!(
        "=SERIES({},{},{},{})",
        part_text(wb, &s.name, true),
        part_text(wb, &s.x, false),
        part_text(wb, &s.values, false),
        order
    )
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

fn parse_series(wb: &Workbook, home: SheetId, f: &str) -> Result<[Part; 3], OmError> {
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
    Ok([
        parse_part(wb, home, &parts[0])?,
        parse_part(wb, home, &parts[1])?,
        parse_part(wb, home, &parts[2])?,
    ])
}

fn cell_variant(v: &Value) -> Variant {
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
    match p {
        Part::Refs(v) => {
            let mut out = Vec::new();
            for (s, r) in v {
                for row in r.r0..=r.r1 {
                    for col in r.c0..=r.c1 {
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

fn vec_variant(items: Vec<Variant>) -> Variant {
    Variant::Array(Box::new(SafeArray {
        elem: VarType::Variant,
        bounds: vec![(1, items.len() as u32)],
        data: items,
    }))
}

#[derive(Clone, Copy, PartialEq)]
enum Cell {
    Text,
    Date,
    Num,
    Other,
    Blank,
}

fn first_data(rows: u32, cols: u32, kind: &dyn Fn(u32, u32) -> Cell) -> (u32, u32) {
    let (rl, cl) = (rows - 1, cols - 1);
    if kind(0, 0) == Cell::Blank {
        let mut edi = 0;
        let mut j = 0;
        while j <= cl && kind(0, j) == Cell::Blank {
            edi = j;
            j += 1;
        }
        if edi == cl {
            edi = 0;
        }
        let mut ebx = 0;
        let mut i = 0;
        while i <= rl && kind(i, edi) == Cell::Blank {
            ebx = i;
            i += 1;
        }
        if ebx == rl {
            ebx = 0;
        }
        return (rl.min(ebx + 1), cl.min(edi + 1));
    }
    if (rows == 1 || cols == 1) && kind(0, 0) == Cell::Num {
        return (0, 0);
    }
    let br_date = kind(rl, cl) == Cell::Date;

    let label = |c: Cell, date_label: bool| c == Cell::Text || (date_label && c == Cell::Date);
    let scan = |date_label: bool| {
        let mut r0 = 0;
        for i in (0..=rl).rev() {
            if label(kind(i, cl), date_label) {
                r0 = rl.min(i + 1);
                break;
            }
        }
        let mut c0 = 0;
        for j in (0..=cl).rev() {
            if label(kind(rl, j), date_label) {
                c0 = cl.min(j + 1);
                break;
            }
        }
        (r0, c0)
    };
    scan(!br_date)
}

fn prev_rcwise(prev: &[Series]) -> i32 {
    let mut out = 0;
    for s in prev {
        let Part::Refs(v) = &s.values else { continue };
        for (_, r) in v {
            let (dc, dr) = (r.c1 != r.c0, r.r1 != r.r0);
            if out == 0 {
                if dc {
                    out = 1;
                } else if dr {
                    out = 2;
                }
            } else if (out == 1 && dr) || (out == 2 && dc) {
                return 0;
            }
        }
    }
    out
}

fn prev_rowwise(prev: &[Series]) -> bool {
    let Some(Part::Refs(v)) = prev.first().map(|s| &s.values) else {
        return true;
    };
    let cols = v.iter().flat_map(|(_, r)| [r.c0, r.c1]);
    let rows = v.iter().flat_map(|(_, r)| [r.r0, r.r1]);
    if cols.clone().min() != cols.max() {
        true
    } else {
        rows.clone().min() == rows.max()
    }
}

fn auto_by_rows(
    rows: u32,
    cols: u32,
    (r0, c0): (u32, u32),
    kind: &dyn Fn(u32, u32) -> Cell,
    prev: &[Series],
) -> bool {
    let pref = prev_rcwise(prev);
    let tie = !prev_rowwise(prev);

    let by_cols = if kind(0, 0) == Cell::Blank {
        let mut f = cols < rows;
        let (dr, dc) = (rows - r0.min(rows), cols - c0.min(cols));
        if f && dc > dr {
            f = false;
        } else if !f && dc < dr {
            f = true;
        }
        f
    } else if rows == 1 || cols == 1 {
        if cols == 1 {
            pref != 1
        } else {
            pref == 2
        }
    } else if pref == 2 {
        true
    } else if pref == 1 {
        false
    } else {
        let date_a = kind(r0.min(rows - 1), 0) == Cell::Date;
        let date_b = kind(0, c0.min(cols - 1)) == Cell::Date;
        if date_a != date_b {
            date_a
        } else {
            let (dr, dc) = (rows - r0, cols - c0);
            if dc > dr {
                false
            } else if dc < dr {
                true
            } else {
                tie
            }
        }
    };
    !by_cols
}

pub(crate) fn series_from_block(
    wb: &Workbook,
    sheet: SheetId,
    r: Rect,
    plot_by: Option<i32>,
    chart_type: i32,
    prev: &[Series],
) -> (i32, Vec<Series>) {
    let (rows, cols) = (r.rows(), r.cols());
    let kind = |i: u32, j: u32| {
        let a = CellAddr::new(sheet, r.r0 + i, r.c0 + j);
        match wb.value(a) {
            Value::Blank => Cell::Blank,
            Value::Text(_) => Cell::Text,
            Value::Number(_)
                if xlcalc::NumberFormat::parse(wb.number_format(a)).is_ok_and(|f| f.is_date()) =>
            {
                Cell::Date
            }
            Value::Number(_) => Cell::Num,
            _ => Cell::Other,
        }
    };
    let xy = crate::chartparts::is_xy(chart_type);
    let (r0, mut c0) = first_data(rows, cols, &kind);
    if xy && cols > 1 && c0 == 0 {
        c0 = 1;
    }
    let (rl, cl) = (rows - 1, cols - 1);

    let by_rows = match plot_by {
        Some(p) => p == 1,
        None => !xy && auto_by_rows(rows, cols, (r0, c0), &kind, prev),
    };
    let by = if by_rows { 1 } else { 2 };
    let area = |i0: u32, j0: u32, i1: u32, j1: u32| {
        Part::Refs(vec![(
            sheet,
            Rect::new(r.r0 + i0, r.c0 + j0, r.r0 + i1, r.c0 + j1),
        )])
    };
    let mut out = Vec::new();
    let mk = |name, x, values| Series {
        name,
        x,
        values,
        chart_type,
        axis_group: 1,
        fmt: Default::default(),
    };
    if by == 2 {
        for j in c0..=cl {
            out.push(mk(
                if r0 > 0 {
                    area(0, j, r0 - 1, j)
                } else {
                    Part::None
                },
                if c0 > 0 {
                    area(r0, 0, rl, c0 - 1)
                } else {
                    Part::None
                },
                area(r0, j, rl, j),
            ));
        }
    } else {
        for i in r0..=rl {
            out.push(mk(
                if c0 > 0 {
                    area(i, 0, i, c0 - 1)
                } else {
                    Part::None
                },
                if r0 > 0 {
                    area(0, c0, r0 - 1, cl)
                } else {
                    Part::None
                },
                area(i, c0, i, cl),
            ));
        }
    }
    if rows == 1 && cols == 1 {
        if kind(0, 0) == Cell::Blank {
            return (0, Vec::new());
        }
        return (1, out);
    }
    (by, out)
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
            s.chart = Some(chart);
        }
        Ok(out)
    }

    pub(crate) fn chart_draw_member(
        &mut self,
        r: &DrawRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        use InvokeKind::*;
        let put = m.kind == Put || m.kind == PutRef;
        let chart = self.chart_of(r)?;

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
                ("ChartType", false) => Ok(Variant::I4(chart.chart_type)),
                ("ChartType", true) => {
                    let t = a
                        .put
                        .as_ref()
                        .map(|v| crate::controls::num_arg(v, a.lcid))
                        .transpose()?
                        .ok_or_else(OmError::type_mismatch)? as i32;
                    self.chart_edit(r, |_, c| {
                        c.chart_type = t;
                        if crate::chartparts::is_radar(t) {
                            c.axes[0].grid_major = true;
                            c.axes[1].reverse = false;
                        }
                        for s in &mut c.series {
                            if matches!(t, 1 | 76 | 77) {
                                s.fmt.marker_style = None;
                            }
                            s.chart_type = t;

                            if s.fmt.marker_fg == Some(-2) {
                                s.fmt.marker_fg = None;
                            }
                        }
                        Ok(Variant::Empty)
                    })
                }
                ("PlotBy", false) => Ok(Variant::I4(chart.plot_by)),
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
                    let by = a.i32(1)?;
                    if matches!(by, Some(x) if x != 1 && x != 2) {
                        return Err(OmError::app_defined());
                    }
                    self.chart_edit(r, |wb, c| {
                        let (by, series) =
                            series_from_block(wb, sheet, rect, by, c.chart_type, &c.series);
                        c.plot_by = by;
                        c.series = series;
                        c.source = Some((sheet, rect));
                        Ok(Variant::Empty)
                    })
                }
                ("Parent", false) => Ok(self.alloc(crate::app::Object::Draw(DrawRef {
                    kind: DrawKind::ChartObject,
                    ..*r
                }))),
                _ => Err(unsupported("_Chart", m)),
            },
            DrawKind::SeriesColl => match (m.name, put) {
                ("Count", false) => Ok(Variant::I4(chart.series.len() as i32)),
                ("Item" | "_Default", _) => match a.get(0) {
                    Some(i) => self.series_item(r, i, a.lcid),
                    None => Err(OmError::app_defined()),
                },
                ("NewSeries", _) => {
                    let n = chart.series.len();
                    let t = chart.chart_type;
                    self.chart_edit(r, |_, c| {
                        c.plot_by = 0;
                        c.series.push(Series {
                            name: Part::None,
                            x: Part::None,
                            values: Part::None,
                            chart_type: t,
                            axis_group: 1,
                            fmt: Default::default(),
                        });
                        Ok(Variant::Empty)
                    })?;
                    Ok(self.alloc(crate::app::Object::Draw(DrawRef {
                        kind: DrawKind::Series(n),
                        ..*r
                    })))
                }
                ("_NewEnum", _) => {
                    let items = (0..chart.series.len())
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
                        Part::None => format!("Series{}", k + 1),
                        Part::Text(t) => t.clone(),
                        Part::Refs(v) => {
                            let mut parts = Vec::new();
                            for (s, r) in v {
                                for row in r.r0..=r.r1 {
                                    for col in r.c0..=r.c1 {
                                        let addr = CellAddr::new(*s, row, col);
                                        parts.push(match cell_variant(&wb.value(addr)) {
                                            Variant::Bstr(t) => t,
                                            Variant::R8(n) => {
                                                let t = wb.display_text(addr);
                                                if t.is_empty() || t.contains('#') {
                                                    xlcalc::format_general(n)
                                                } else {
                                                    t
                                                }
                                            }
                                            _ => String::new(),
                                        });
                                    }
                                }
                            }
                            parts.join(" ")
                        }
                        p => part_values(wb, p)
                            .first()
                            .map(|v| match v {
                                Variant::Bstr(s) => s.clone(),
                                Variant::R8(n) => xlcalc::format_general(*n),
                                _ => String::new(),
                            })
                            .unwrap_or_default(),
                    })),
                    ("Formula", false) => {
                        if matches!(ser.name, Part::Dead) || matches!(ser.values, Part::Dead) {
                            return Err(OmError::exception(0x800A_07E7_u32 as i32, None));
                        }
                        Ok(Variant::Bstr(series_formula(wb, &ser, k + 1)))
                    }
                    ("Formula", true) => {
                        let f = match a.put.as_ref() {
                            Some(Variant::Bstr(f)) => f.clone(),
                            _ => return Err(OmError::type_mismatch()),
                        };
                        let [n, x, v] = parse_series(wb, r.sheet, &f)?;
                        self.chart_edit(r, |_, c| {
                            let s = &mut c.series[k];
                            (s.name, s.x, s.values) = (n, x, v);
                            Ok(Variant::Empty)
                        })
                    }
                    ("Values" | "XValues" | "Name", true) => {
                        let f = match a.put.as_ref() {
                            Some(Variant::Bstr(f)) => f.clone(),
                            _ => return Err(OmError::type_mismatch()),
                        };
                        let part = parse_part(wb, r.sheet, f.trim_start_matches('='))?;
                        let which = m.name;
                        self.chart_edit(r, |_, c| {
                            let s = &mut c.series[k];
                            match which {
                                "Values" => s.values = part,
                                "XValues" => s.x = part,
                                _ => s.name = part,
                            }
                            Ok(Variant::Empty)
                        })
                    }
                    ("Values", false) if matches!(ser.values, Part::Dead) => {
                        Ok(vec_variant(vec![Variant::R8(1.0)]))
                    }
                    ("Values", false) => Ok(vec_variant(part_values(wb, &ser.values))),
                    ("XValues", false) => {
                        let v = part_values(wb, &ser.x);
                        if v.is_empty() {
                            let n = part_values(wb, &ser.values).len();
                            Ok(vec_variant(
                                (1..=n).map(|i| Variant::R8(i as f64)).collect(),
                            ))
                        } else if matches!(&ser.x, Part::Text(_)) {
                            Ok(vec_variant(
                                v.into_iter()
                                    .map(|x| match x {
                                        Variant::R8(n) => Variant::Bstr(xlcalc::format_general(n)),
                                        o => o,
                                    })
                                    .collect(),
                            ))
                        } else {
                            Ok(vec_variant(v))
                        }
                    }
                    ("PlotOrder", false) => Ok(Variant::I4(k as i32 + 1)),
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
                    ("Delete", _) => self.chart_edit(r, |_, c| {
                        c.series.remove(k);
                        Ok(Variant::Bool(true))
                    }),
                    _ => Err(unsupported("Series", m)),
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
                        Part::None => format!("Series{}", i + 1),
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
            v => {
                let n = crate::controls::num_arg(v, lcid)?.trunc();
                if n < 1.0 || n as usize > chart.series.len() {
                    return Err(crate::chartparts::range_err("Parameter not valid"));
                }
                n as usize - 1
            }
        };
        Ok(self.alloc(crate::app::Object::Draw(DrawRef {
            kind: DrawKind::Series(k),
            ..*r
        })))
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
