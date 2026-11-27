use xlcalc::drawing::{Chart, HeldLabel, Part};
use xlcalc::{CellAddr, SheetId, Workbook};
use xlcalc_variant::Variant;

use crate::app::{Application, BookId};
use crate::charts::HideView;

struct Area {
    sheet: SheetId,
    rows: Vec<u32>,
    cols: Vec<u32>,
}

impl Area {
    fn len(&self) -> usize {
        if self.cols.len() == 1 {
            self.rows.len()
        } else {
            self.cols.len()
        }
    }

    fn cell(&self, i: usize) -> Option<CellAddr> {
        let (row, col) = if self.cols.len() == 1 {
            (*self.rows.get(i)?, self.cols[0])
        } else {
            (*self.rows.first()?, *self.cols.get(i)?)
        };
        Some(CellAddr::new(self.sheet, row, col))
    }

    fn index_of(&self, c: CellAddr) -> Option<usize> {
        if c.sheet != self.sheet {
            return None;
        }
        let row = self.rows.binary_search(&c.row).ok()?;
        let col = self.cols.binary_search(&c.col).ok()?;
        Some(if self.cols.len() == 1 { row } else { col })
    }
}

fn areas(wb: &Workbook, chart: &Chart) -> Vec<Vec<Area>> {
    let hv = HideView::new(wb, chart);
    let all = !chart.visible_only;
    chart
        .series
        .iter()
        .map(|s| match &*wb.resolve_part(&s.values) {
            Part::Refs(v) => v
                .iter()
                .map(|&(sheet, r)| Area {
                    sheet,
                    rows: (r.r0..=r.r1)
                        .filter(|&x| all || !hv.row(sheet, x))
                        .collect(),
                    cols: (r.c0..=r.c1)
                        .filter(|&x| all || !hv.col(sheet, x))
                        .collect(),
                })
                .filter(|a| !a.rows.is_empty() && !a.cols.is_empty())
                .collect(),
            _ => Vec::new(),
        })
        .collect()
}

fn cell_of(areas: &[Area], n: u16) -> Option<CellAddr> {
    let mut i = usize::from(n).checked_sub(1)?;
    for a in areas {
        if i < a.len() {
            return a.cell(i);
        }
        i -= a.len();
    }
    None
}

fn point_of(all: &[Vec<Area>], c: CellAddr, k: usize, now: Option<u16>) -> Option<(usize, u16)> {
    let mut hit = None;
    for (j, series) in all.iter().enumerate() {
        let mut before = 0;
        for a in series {
            let Some(i) = a.index_of(c) else {
                before += a.len();
                continue;
            };
            let Ok(point) = u16::try_from(before + i + 1) else {
                break;
            };
            hit = Some((j, point));
            let span = before + 1..=before + a.len();
            if j == k && now.is_some_and(|n| span.contains(&usize::from(n))) {
                return hit;
            }
            break;
        }
    }
    hit
}

fn bag_prefix(k: usize, n: u16) -> String {
    format!("DataLabel{k}_{n}.")
}

fn take_label(chart: &mut Chart, k: usize, n: u16, cell: Option<CellAddr>) -> Option<HeldLabel> {
    let fmt = &mut chart.series[k].fmt;
    let own = fmt.point_labels.contains(&n);
    fmt.point_labels.retain(|&p| p != n);
    let text = fmt
        .label_texts
        .iter()
        .position(|t| t.point == n)
        .map(|i| fmt.label_texts.remove(i));
    let prefix = bag_prefix(k, n);
    let keys: Vec<String> = chart
        .bag
        .keys()
        .filter(|key| key.starts_with(&prefix))
        .cloned()
        .collect();
    let format: Vec<_> = keys
        .iter()
        .filter_map(|key| Some((key[prefix.len()..].to_string(), chart.bag.remove(key)?)))
        .collect();
    (own || text.is_some() || !format.is_empty()).then_some(HeldLabel {
        cell,
        own,
        text,
        format,
    })
}

fn put_label(chart: &mut Chart, k: usize, n: u16, label: HeldLabel) {
    let fmt = &mut chart.series[k].fmt;
    if label.own && !fmt.point_labels.contains(&n) {
        fmt.point_labels.push(n);
        fmt.point_labels.sort_unstable();
    }
    if let Some(mut text) = label.text {
        text.point = n;
        fmt.label_texts.retain(|t| t.point != n);
        fmt.label_texts.push(text);
        fmt.label_texts.sort_by_key(|t| t.point);
    }
    fmt.label_cells.retain(|&(p, _)| p != n);
    fmt.label_cells.push((n, label.cell));
    for (key, value) in label.format {
        chart.bag.insert(bag_prefix(k, n) + &key, value);
    }
}

pub(crate) fn retrack(wb: &Workbook, chart: &mut Chart) {
    let labelled = chart.series.iter().any(|s| {
        !s.fmt.point_labels.is_empty()
            || !s.fmt.label_texts.is_empty()
            || !s.fmt.held_labels.is_empty()
            || !s.fmt.label_cells.is_empty()
    });
    if chart.pivot.is_some() || !labelled {
        return;
    }
    let all = areas(wb, chart);
    let mut loose = Vec::new();
    for k in 0..chart.series.len() {
        if matches!(chart.series[k].values, Part::External(_)) {
            continue;
        }
        for (n, cell) in std::mem::take(&mut chart.series[k].fmt.label_cells) {
            if let Some(label) = take_label(chart, k, n, cell) {
                loose.push((k, Some(n), label));
            }
        }
        for label in std::mem::take(&mut chart.series[k].fmt.held_labels) {
            loose.push((k, None, label));
        }
    }
    for (k, now, label) in loose {
        match label.cell.and_then(|c| point_of(&all, c, k, now)) {
            Some((to, n)) => put_label(chart, to, n, label),
            None => chart.series[k].fmt.held_labels.push(label),
        }
    }
    for (s, areas) in chart.series.iter_mut().zip(&all) {
        let fmt = &mut s.fmt;
        let points: Vec<u16> = fmt
            .point_labels
            .iter()
            .copied()
            .chain(fmt.label_texts.iter().map(|t| t.point))
            .collect();
        for n in points {
            if fmt.label_cells.iter().all(|&(p, _)| p != n) {
                if let Some(c) = cell_of(areas, n) {
                    fmt.label_cells.push((n, Some(c)));
                }
            }
        }
    }
}

impl Application<'_> {
    fn tracks_points(&self, b: BookId) -> bool {
        let key = format!("{}:ChartDataPointTrack", b.0 + 1);
        !matches!(self.opts.get(&key), Some(Variant::Bool(false)))
    }

    pub(crate) fn retrack_chart(&self, b: BookId, chart: &mut Chart) {
        if let (true, Some(wb)) = (self.tracks_points(b), self.engine(b)) {
            retrack(wb, chart);
        }
    }

    pub(crate) fn retrack_book(&mut self, b: BookId) {
        if !self.tracks_points(b) {
            return;
        }
        let Ok(book) = self.book_mut(b) else { return };
        let sheets: Vec<SheetId> = book.wb.sheets().map(|s| s.id).collect();
        for sheet in sheets {
            let charts: Vec<(u32, Chart)> = book
                .wb
                .drawing(sheet)
                .into_iter()
                .flat_map(|d| &d.shapes)
                .filter_map(|s| Some((s.id, s.chart.clone()?)))
                .collect();
            for (id, was) in charts {
                let mut chart = was.clone();
                retrack(&book.wb, &mut chart);
                if chart != was {
                    if let Some(s) = book.wb.drawing_mut(sheet).get_mut(id) {
                        s.chart = Some(chart);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use xlcalc::drawing::{Series, SeriesFmt};
    use xlcalc::Rect;

    fn column(s: SheetId, col: u32) -> Part {
        Part::Refs(vec![(s, Rect::new(1, col, 3, col))])
    }

    #[test]
    fn labels_follow_their_cells() {
        let mut wb = Workbook::empty();
        let s = wb.add_sheet("Sheet1").unwrap();
        let mut chart = Chart::new();
        chart.series.push(Series {
            name: Part::None,
            x: Part::None,
            values: column(s, 1),
            chart_type: 51,
            axis_group: 1,
            fmt: SeriesFmt {
                point_labels: vec![1],
                ..Default::default()
            },
        });
        retrack(&wb, &mut chart);
        let cell = Some(CellAddr::new(s, 1, 1));
        assert_eq!(chart.series[0].fmt.label_cells, [(1, cell)]);
        chart.series[0].values = column(s, 2);
        retrack(&wb, &mut chart);
        assert!(chart.series[0].fmt.point_labels.is_empty());
        assert_eq!(chart.series[0].fmt.held_labels.len(), 1);

        chart.series[0].values = Part::Refs(vec![(s, Rect::new(0, 0, 1, 1))]);
        retrack(&wb, &mut chart);
        assert_eq!(chart.series[0].fmt.point_labels, [2]);
        chart.series[0].values = column(s, 1);
        retrack(&wb, &mut chart);
        assert_eq!(chart.series[0].fmt.point_labels, [1]);
        assert!(chart.series[0].fmt.held_labels.is_empty());
    }
}
