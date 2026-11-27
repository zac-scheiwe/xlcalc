use crate::address::{CellAddr, Rect, SheetId};
use crate::drawing::{Part, Series};
use crate::value::Value;
use crate::workbook::Workbook;

pub fn is_xy(t: i32) -> bool {
    matches!(t, -4169 | 72..=75 | 15 | 87 | 88)
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

fn bubble_split(
    rows: u32,
    cols: u32,
    kind: &dyn Fn(u32, u32) -> Cell,
    plot_by: Option<i32>,
) -> Option<(bool, u32, u32)> {
    let non_num = |i: u32, j: u32| matches!(kind(i, j), Cell::Text | Cell::Date);
    let hr = rows > 1 && non_num(0, u32::from(cols > 1));
    let hc = cols > 1 && non_num(u32::from(rows > 1), 0);
    let (dr, dc) = (rows - u32::from(hr), cols - u32::from(hc));
    let fixed = plot_by.map(|p| p == 1);

    let pick = |auto: bool, f: &dyn Fn(bool) -> (bool, bool)| {
        let br = fixed.unwrap_or(auto);
        let (n8, n9) = f(br);
        (br, n8, n9)
    };
    let (br, n8, n9) = if (dc == 1 || dr == 1) && dc != dr {
        pick(dc == 1, &|br| {
            (
                if br { hc } else { hr },
                (if br { dr } else { dc }) & 1 == 1,
            )
        })
    } else {
        let a = |br: bool| (if br { hc } else { hr }, br);

        let c = |br: bool| (if br { hr } else { hc }, !br);

        let b = |br: bool| {
            let h = if br { hc } else { hr };
            (h, h)
        };
        if dr % 2 == 1 && dr > 1 && dc % 2 == 0 {
            if !hr {
                pick(true, &a)
            } else if !hc {
                pick(dc >= dr, &b)
            } else {
                pick(false, &c)
            }
        } else if dc % 2 == 1 {
            if dc <= 1 || dr % 2 == 1 {
                return None;
            }
            if !hc {
                pick(false, &c)
            } else if !hr {
                pick(dc >= dr, &b)
            } else {
                pick(true, &a)
            }
        } else if dr % 2 == 1 {
            return None;
        } else {
            pick(dc >= dr, &b)
        }
    };
    let (n8, n9) = (u32::from(n8), u32::from(n9));
    Some(if br { (true, n9, n8) } else { (false, n8, n9) })
}

pub fn series_from_block(
    wb: &Workbook,
    sheet: SheetId,
    r: Rect,
    plot_by: Option<i32>,
    chart_type: i32,
    prev: &[Series],
) -> (i32, Vec<Series>) {
    let rmap: Vec<u32> = (r.r0..=r.r1).collect();
    let cmap: Vec<u32> = (r.c0..=r.c1).collect();
    series_from_grid(wb, sheet, (&rmap, &cmap), plot_by, chart_type, prev)
}

pub fn series_from_used(
    wb: &Workbook,
    sheet: SheetId,
    r: Rect,
    plot_by: Option<i32>,
    chart_type: i32,
    current: &[Series],
) -> (i32, Vec<Series>) {
    let areas: Vec<Rect> = current
        .iter()
        .flat_map(|s| [&s.name, &s.x, &s.values])
        .filter_map(|p| match p {
            Part::Refs(v) => Some(v),
            _ => None,
        })
        .flatten()
        .filter(|(sh, _)| *sh == sheet)
        .map(|(_, q)| *q)
        .collect();

    let r = areas.iter().fold(r, |a, q| {
        Rect::new(
            a.r0.min(q.r0),
            a.c0.min(q.c0),
            a.r1.max(q.r1),
            a.c1.max(q.c1),
        )
    });
    let mut rmap: Vec<u32> = (r.r0..=r.r1)
        .filter(|&i| {
            areas
                .iter()
                .any(|q| q.r0 <= i && i <= q.r1 && q.c1 >= r.c0 && q.c0 <= r.c1)
        })
        .collect();
    let mut cmap: Vec<u32> = (r.c0..=r.c1)
        .filter(|&j| {
            areas
                .iter()
                .any(|q| q.c0 <= j && j <= q.c1 && q.r1 >= r.r0 && q.r0 <= r.r1)
        })
        .collect();
    if rmap.is_empty() || cmap.is_empty() {
        rmap = (r.r0..=r.r1).collect();
        cmap = (r.c0..=r.c1).collect();
    }
    series_from_grid(wb, sheet, (&rmap, &cmap), plot_by, chart_type, current)
}

fn series_from_grid(
    wb: &Workbook,
    sheet: SheetId,
    (rmap, cmap): (&[u32], &[u32]),
    plot_by: Option<i32>,
    chart_type: i32,
    prev: &[Series],
) -> (i32, Vec<Series>) {
    let (rows, cols) = (rmap.len() as u32, cmap.len() as u32);

    let hidden_header = |a: CellAddr| {
        wb.tables().iter().any(|t| {
            !t.headers
                && t.sheet == a.sheet
                && t.rect.r0 > 0
                && a.row + 1 == t.rect.r0
                && (t.rect.c0..=t.rect.c1).contains(&a.col)
        })
    };
    let kind = |i: u32, j: u32| {
        let a = CellAddr::new(sheet, rmap[i as usize], cmap[j as usize]);
        match wb.value(a) {
            Value::Blank if hidden_header(a) => Cell::Other,
            Value::Blank => Cell::Blank,
            Value::Text(_) => Cell::Text,
            Value::Number(_)
                if crate::NumberFormat::parse_stored(wb.number_format(a))
                    .is_ok_and(|f| f.is_date()) =>
            {
                Cell::Date
            }
            Value::Number(_) => Cell::Num,
            _ => Cell::Other,
        }
    };
    let xy = is_xy(chart_type);
    let bubble = matches!(chart_type, 15 | 87);
    let (by_rows, r0, c0) = match bubble.then(|| bubble_split(rows, cols, &kind, plot_by)) {
        Some(Some(split)) => split,
        _ => {
            let (mut r0, c00) = first_data(rows, cols, &kind);
            let mut c0 = c00;
            if xy && cols > 1 && c0 == 0 {
                c0 = 1;
            }

            let by_rows = match plot_by {
                Some(p) => p == 1,
                None => auto_by_rows(rows, cols, (r0, c0), &kind, prev),
            };

            if xy && by_rows {
                c0 = c00;
                if rows > 1 && r0 == 0 {
                    r0 = 1;
                }
            }
            (by_rows, r0, c0)
        }
    };
    let (rl, cl) = (rows - 1, cols - 1);
    let (r0, c0) = (r0.min(rl), c0.min(cl));
    let by = if by_rows { 1 } else { 2 };

    let runs = |map: &[u32], a: u32, b: u32| {
        let mut out: Vec<(u32, u32)> = Vec::new();
        for &x in &map[a as usize..=b as usize] {
            match out.last_mut() {
                Some((_, e)) if *e + 1 == x => *e = x,
                _ => out.push((x, x)),
            }
        }
        out
    };
    let area = |i0: u32, j0: u32, i1: u32, j1: u32| {
        let mut v = Vec::new();
        for (ra, rb) in runs(rmap, i0, i1) {
            for &(ca, cb) in &runs(cmap, j0, j1) {
                v.push((sheet, Rect::new(ra, ca, rb, cb)));
            }
        }
        Part::Refs(v)
    };
    let mut out = Vec::new();

    let step = if bubble { 2 } else { 1 };
    let mk = |name, x, values, bubbles: Option<Part>| Series {
        name,
        x,
        values,
        chart_type,
        axis_group: 1,
        fmt: crate::drawing::SeriesFmt {
            bubbles,
            ..Default::default()
        },
    };
    if by == 2 {
        for j in (c0..=cl).step_by(step) {
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
                (bubble && j < cl).then(|| area(r0, j + 1, rl, j + 1)),
            ));
        }
    } else {
        for i in (r0..=rl).step_by(step) {
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
                (bubble && i < rl).then(|| area(i + 1, c0, i + 1, cl)),
            ));
        }
    }

    let last = prev.iter().map(|s| s.fmt.plot_rank).max().unwrap_or(0);

    let combo = prev
        .iter()
        .any(|p| (p.chart_type, p.axis_group) != (prev[0].chart_type, prev[0].axis_group));
    for (i, s) in out.iter_mut().enumerate() {
        s.fmt.plot_rank = prev.get(i).map_or(last, |p| p.fmt.plot_rank);
        if let Some(p) = prev.get(i).filter(|_| combo) {
            s.chart_type = p.chart_type;
            s.axis_group = p.axis_group;
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
