use xlcalc::{SheetId, Workbook};

use crate::window::SheetView;

const EPS: f64 = 1e-9;

pub(crate) struct Lines<'a> {
    size: Box<dyn Fn(u32) -> f64 + 'a>,
    max: u32,
}

impl<'a> Lines<'a> {
    pub(crate) fn new(hidden: impl Fn(u32) -> bool + 'a, max: u32) -> Self {
        Lines::sized(move |n| if hidden(n) { 0.0 } else { 1.0 }, max)
    }

    pub(crate) fn sized(size: impl Fn(u32) -> f64 + 'a, max: u32) -> Self {
        Lines {
            size: Box::new(size),
            max,
        }
    }

    pub(crate) fn rows(wb: &'a Workbook, sheet: SheetId) -> Self {
        Lines::new(move |n| wb.row_hidden(sheet, n - 1), 1_048_576)
    }

    pub(crate) fn cols(wb: &'a Workbook, sheet: SheetId) -> Self {
        Lines::new(move |n| wb.col_hidden(sheet, n - 1), 16_384)
    }

    fn size(&self, n: u32) -> f64 {
        (self.size)(n)
    }

    pub(crate) fn first_shown(&self, n: u32) -> u32 {
        (n..=self.max).find(|&l| self.size(l) > 0.0).unwrap_or(n)
    }

    fn span(&self, a: u32, b: u32) -> f64 {
        (a..b.min(self.max + 1)).map(|l| self.size(l)).sum()
    }

    fn last_full(&self, first: u32, pos: f64) -> u32 {
        let mut used = 0.0;
        for line in first..=self.max {
            used += self.size(line);
            if used > pos + EPS {
                return line - 1;
            }
        }
        self.max
    }

    fn line_at(&self, first: u32, pos: f64) -> (u32, f64) {
        let mut used = 0.0;
        for line in first..=self.max {
            let size = self.size(line);
            if used + size >= pos - EPS {
                return (line, used);
            }
            used += size;
        }
        (self.max, used)
    }

    pub(crate) fn position(&self, first: u32, count: f64) -> f64 {
        let end = first.saturating_add(count.trunc() as u32);
        let part = if end <= self.max { self.size(end) } else { 0.0 };
        self.span(first, end) + count.fract() * part
    }

    pub(crate) fn next_pane(&self, first: u32, pos: f64) -> u32 {
        (self.last_full(first, pos) + 1).clamp(first, self.max)
    }

    pub(crate) fn count_at(&self, first: u32, pos: f64) -> f64 {
        if pos <= 0.0 {
            return 0.0;
        }
        let (line, before) = self.line_at(first, pos);
        let size = self.size(line);
        let part = if size > 0.0 {
            (pos - before).max(0.0) / size
        } else {
            0.0
        };
        f64::from(line - first) + part
    }

    fn frozen_last(&self, first: u32, pos: f64) -> u32 {
        let last = self.last_full(first, pos);
        if last < first {
            first
        } else if last < self.max && self.span(first, last + 1) + self.size(last + 1) / 2.0 < pos {
            last + 1
        } else {
            last
        }
    }
}

pub(crate) fn freeze_point(
    to_cell: (f64, f64),
    window: (f64, f64),
    middle: (f64, f64),
) -> (f64, f64) {
    let (y, x) = to_cell;
    let (height, width) = window;
    let inside = |v: f64, edge: f64| v > 0.0 && v < edge;
    if x <= 0.0 && inside(y, height) {
        (y, 0.0)
    } else if y <= 0.0 && inside(x, width) {
        (0.0, x)
    } else if inside(x, width) && inside(y, height) {
        (y, x)
    } else {
        middle
    }
}

impl SheetView {
    fn axis(&self, rows: bool) -> (f64, u32, u32) {
        if rows {
            (self.split_row, self.scroll_row, self.pane_row)
        } else {
            (self.split_col, self.scroll_col, self.pane_col)
        }
    }

    fn set_axis(&mut self, rows: bool, (split, scroll, pane): (f64, u32, u32)) {
        if rows {
            (self.split_row, self.scroll_row, self.pane_row) = (split, scroll, pane);
        } else {
            (self.split_col, self.scroll_col, self.pane_col) = (split, scroll, pane);
        }
    }

    fn frozen_on(&self, rows: bool) -> bool {
        self.frozen && self.axis(rows).0 > 0.0
    }

    pub(crate) fn shown_scroll(&self, rows: bool, lines: &Lines) -> u32 {
        let scroll = self.axis(rows).1;
        if self.frozen_on(rows) {
            scroll
        } else {
            lines.first_shown(scroll)
        }
    }

    pub(crate) fn scroll_to(&mut self, rows: bool, n: u32) {
        let (split, _, pane) = self.axis(rows);
        let n = if self.frozen_on(rows) {
            n.max(pane + split as u32)
        } else {
            n
        };
        self.set_axis(rows, (split, n, pane));
    }

    pub(crate) fn split_read(&self, rows: bool, lines: &Lines) -> (f64, f64) {
        let (split, scroll, pane) = self.axis(rows);
        if split <= 0.0 {
            return (0.0, 0.0);
        }
        if !self.frozen {
            return (lines.count_at(lines.first_shown(scroll), split), split);
        }

        let span = split as u32;
        let last = pane + span.saturating_sub(1);
        (
            f64::from(span.saturating_sub(1)) + lines.size(last),
            lines.span(pane, pane + span),
        )
    }

    pub(crate) fn freeze(&mut self, rows: &Lines, cols: &Lines) {
        for (r, lines) in [(true, rows), (false, cols)] {
            let (split, scroll, pane) = self.axis(r);
            if split <= 0.0 {
                self.set_axis(r, (0.0, scroll, pane));
                continue;
            }
            let first = lines.first_shown(scroll);
            let last = lines.frozen_last(first, split);
            let next = (last + 1).min(lines.max);
            self.set_axis(r, (f64::from(last - first + 1), next, first));
        }
        self.frozen = self.split_row > 0.0 || self.split_col > 0.0;
    }

    pub(crate) fn unfreeze(&mut self, rows: &Lines, cols: &Lines) {
        for (r, lines) in [(true, rows), (false, cols)] {
            if self.frozen_on(r) {
                let (split, _, pane) = self.axis(r);
                let pos = lines.span(pane, pane + split as u32);
                self.set_axis(r, (pos, pane, pane));
            }
        }
        self.frozen = false;
    }

    pub(crate) fn visible_lines(&self, rows: bool, total: u32, lines: &Lines) -> (u32, u32) {
        let (split, scroll, _) = self.axis(rows);
        if split <= 0.0 {
            (scroll - 1, scroll + total - 2)
        } else if self.frozen {
            (
                scroll - 1,
                (scroll + total).saturating_sub(split as u32 + 2),
            )
        } else {
            let first = lines.first_shown(scroll);
            (first - 1, lines.line_at(first, split).0 - 1)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lines(hidden: &'static [u32]) -> Lines<'static> {
        Lines::new(move |n| hidden.contains(&n), 16_384)
    }

    fn frozen_at(rows: &Lines, cols: &Lines, cell: (u32, u32)) -> SheetView {
        let mut v = SheetView::default();
        let to = |l: &Lines, n: u32| l.span(l.first_shown(1), n);
        let (y, x) = freeze_point(
            (to(rows, cell.0), to(cols, cell.1)),
            (46.4, 29.4),
            (23.0, 15.0),
        );
        (v.split_row, v.split_col) = (y, x);
        v.freeze(rows, cols);
        v
    }

    #[test]
    fn freeze_over_hidden_columns() {
        let (rows, cols) = (lines(&[]), lines(&[2, 3]));
        let v = frozen_at(&rows, &cols, (2, 2));
        assert_eq!((v.split_col, v.pane_col, v.scroll_col), (3.0, 1, 4));
        assert_eq!(v.split_read(false, &cols), (2.0, 1.0));

        let cols = lines(&[1]);
        let v = frozen_at(&rows, &cols, (2, 2));
        assert_eq!((v.split_col, v.split_row), (0.0, 1.0));
        assert_eq!(v.shown_scroll(false, &cols), 2);
    }

    #[test]
    fn hidden_lines_at_the_origin() {
        let cols = lines(&[1, 2]);
        let v = frozen_at(&lines(&[]), &cols, (4, 4));
        assert_eq!((v.split_col, v.pane_col, v.scroll_col), (1.0, 3, 4));

        let both = lines(&[1]);
        let v = frozen_at(&both, &both, (2, 2));
        assert_eq!((v.split_row, v.split_col), (23.0, 15.0));
        assert_eq!((v.scroll_row, v.scroll_col), (25, 17));
    }

    #[test]
    fn hidden_after_freezing_then_unfrozen() {
        let open = lines(&[]);
        let mut v = frozen_at(&open, &open, (4, 4));
        let cols = lines(&[2]);
        assert_eq!(v.split_read(false, &cols), (3.0, 2.0));

        assert_eq!(v.split_read(false, &lines(&[3])), (2.0, 2.0));
        assert_eq!(v.split_read(false, &lines(&[1, 2, 3])), (2.0, 0.0));
        v.scroll_to(false, 1);
        assert_eq!(v.scroll_col, 4);
        v.unfreeze(&open, &cols);
        assert_eq!((v.split_col, v.scroll_col), (2.0, 1));
        assert_eq!(v.split_read(false, &cols), (3.0, 2.0));
    }

    #[test]
    fn counts_over_hidden_lines() {
        let cols = lines(&[2]);
        assert!((cols.position(1, 3.0) - 2.0).abs() < EPS);
        assert!((cols.count_at(1, 2.0) - 3.0).abs() < EPS);

        assert!((lines(&[3]).count_at(1, 2.0) - 2.0).abs() < EPS);

        let open = lines(&[]);
        assert!((open.position(1, 2.5) - 2.5).abs() < EPS);
        assert!((open.count_at(1, 2.5) - 2.5).abs() < EPS);

        assert_eq!(open.frozen_last(1, 0.4), 1);
        assert_eq!(open.frozen_last(1, 2.5), 2);
        assert_eq!(open.frozen_last(1, 2.6), 3);
    }

    #[test]
    fn freeze_point_edges() {
        let ask = |y, x| freeze_point((y, x), (46.4, 29.4), (23.0, 15.0));
        assert_eq!(ask(1.0, 0.0), (1.0, 0.0));
        assert_eq!(ask(0.0, 1.0), (0.0, 1.0));
        assert_eq!(ask(1.0, 1.0), (1.0, 1.0));
        assert_eq!(ask(0.0, 0.0), (23.0, 15.0));
        assert_eq!(ask(47.0, 0.0), (23.0, 15.0));
        assert_eq!(ask(0.0, 30.0), (23.0, 15.0));
        assert_eq!(ask(47.0, 1.0), (23.0, 15.0));
    }
}
