use super::{abs_ref, eval, flag, func, input, number, output, text, Arg, AtpError, Out, Run, Src};
use crate::address::{Rect, SheetId};
use crate::style::Edge;
use crate::value::Value;
use crate::Workbook;

pub(super) fn num(v: &Value) -> f64 {
    match v {
        Value::Number(x) => *x,
        _ => f64::NAN,
    }
}

pub(super) fn sub(s: &Src, r0: u32, c0: u32, r1: u32, c1: u32) -> Src {
    Src {
        sheet: s.sheet,
        rect: Rect::new(
            s.rect.r0 + r0,
            s.rect.c0 + c0,
            s.rect.r0 + r1,
            s.rect.c0 + c1,
        ),
    }
}

pub(super) fn summary_row(wb: &Workbook, o: &mut Out, row: u32, name: &Value, g: &Src) {
    let r = abs_ref(wb, g);
    o.val(row, 0, name.clone());
    for (j, f) in ["COUNT", "SUM", "AVERAGE", "VAR"].iter().enumerate() {
        o.val(row, 1 + j as u32, eval(wb, g.sheet, &format!("{f}({r})")));
    }
}

pub(super) fn label_text(wb: &Workbook, s: &Src, r: u32, c: u32, fallback: String) -> Value {
    match wb.value(s.at(r, c)) {
        v @ (Value::Text(_) | Value::Number(_)) => v,
        _ => Value::text(fallback.as_str()),
    }
}

pub(super) struct Row {
    pub name: &'static str,
    pub ss: f64,
    pub df: f64,
}

pub(super) fn anova_table(
    wb: &Workbook,
    o: &mut Out,
    at: u32,
    sheet: SheetId,
    rows: &[Row],
    total: Row,
    alpha: f64,
) {
    o.text(at, 0, "ANOVA");
    let heads = [
        "Source of Variation",
        "SS",
        "df",
        "MS",
        "F",
        "P-value",
        "F crit",
    ];
    for (j, h) in heads.iter().enumerate() {
        o.text(at + 1, j as u32, h);
    }
    let error = &rows[rows.len() - 1];
    let mse = error.ss / error.df;
    for (i, r) in rows.iter().enumerate() {
        let row = at + 2 + i as u32;
        o.text(row, 0, r.name);
        o.num(row, 1, r.ss);
        o.num(row, 2, r.df);
        let ms = r.ss / r.df;
        o.num(row, 3, ms);
        if i + 1 < rows.len() {
            let f = ms / mse;
            o.num(row, 4, f);
            o.val(row, 5, func(wb, sheet, "FDIST", &[f, r.df, error.df]));
            o.val(row, 6, func(wb, sheet, "FINV", &[alpha, r.df, error.df]));
        }
    }
    let trow = at + 3 + rows.len() as u32;
    o.text(trow, 0, total.name);
    o.num(trow, 1, total.ss);
    o.num(trow, 2, total.df);
    o.line(Rect::new(at, 0, at, 6), Edge::Bottom, true);
    o.header(Rect::new(at + 1, 0, at + 1, 6), false);
    o.line(Rect::new(at + 1, 0, at + 1, 6), Edge::Top, true);
    o.line(Rect::new(at + 1, 0, at + 1, 6), Edge::Bottom, false);
    o.line(Rect::new(trow, 0, trow, 6), Edge::Bottom, true);
}

pub(super) fn anova1(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let src = input(args, 0)?;
    let by_rows = text(args, 2).is_some_and(|s| s.eq_ignore_ascii_case("R"));
    let labels = flag(args, 3);
    let alpha = number(args, 4).unwrap_or(0.05);
    let lab = u32::from(labels);
    let data = if by_rows {
        sub(&src, 0, lab, src.rows() - 1, src.cols() - 1)
    } else {
        sub(&src, lab, 0, src.rows() - 1, src.cols() - 1)
    };
    let groups = if by_rows { data.rows() } else { data.cols() };
    let mut o = Out::default();
    o.text(0, 0, "Anova: Single Factor");
    o.text(2, 0, "SUMMARY");
    for (j, h) in ["Groups", "Count", "Sum", "Average", "Variance"]
        .iter()
        .enumerate()
    {
        o.text(3, j as u32, h);
    }
    let mut devsq = Vec::new();
    let mut n_total = 0.0;
    for g in 0..groups {
        let gs = if by_rows {
            sub(&data, g, 0, g, data.cols() - 1)
        } else {
            sub(&data, 0, g, data.rows() - 1, g)
        };
        let fallback = format!("{} {}", if by_rows { "Row" } else { "Column" }, g + 1);
        let name = if !labels {
            Value::text(fallback.as_str())
        } else if by_rows {
            label_text(wb, &src, g, 0, fallback)
        } else {
            label_text(wb, &src, 0, g, fallback)
        };
        summary_row(wb, &mut o, 4 + g, &name, &gs);
        let r = abs_ref(wb, &gs);
        devsq.push(num(&eval(wb, src.sheet, &format!("DEVSQ({r})"))));
        n_total += num(&eval(wb, src.sheet, &format!("COUNT({r})")));
    }
    let last = 3 + groups;
    let all = abs_ref(wb, &data);
    let sst = num(&eval(wb, src.sheet, &format!("DEVSQ({all})")));
    let ssw = devsq.iter().skip(1).fold(devsq[0], |a, b| a + b);
    let ssb = sst - ssw;
    let k = f64::from(groups);
    let rows = [
        Row {
            name: "Between Groups",
            ss: ssb,
            df: k - 1.0,
        },
        Row {
            name: "Within Groups",
            ss: ssw,
            df: n_total - k,
        },
    ];
    let total = Row {
        name: "Total",
        ss: sst,
        df: n_total - 1.0,
    };
    anova_table(wb, &mut o, last + 3, src.sheet, &rows, total, alpha);
    o.line(Rect::new(2, 0, 2, 4), Edge::Bottom, true);
    o.header(Rect::new(3, 0, 3, 4), false);
    o.line(Rect::new(3, 0, 3, 4), Edge::Top, true);
    o.line(Rect::new(3, 0, 3, 4), Edge::Bottom, false);
    o.line(Rect::new(last, 0, last, 4), Edge::Bottom, true);
    output(args, 1, o)
}

pub(super) fn anova3(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let src = input(args, 0)?;
    let labels = flag(args, 2);
    let alpha = number(args, 3).unwrap_or(0.05);
    let lab = u32::from(labels);
    let data = sub(&src, lab, lab, src.rows() - 1, src.cols() - 1);
    let (r, c) = (data.rows(), data.cols());
    let mut o = Out::default();
    o.text(0, 0, "Anova: Two-Factor Without Replication");
    o.text(2, 0, "SUMMARY");
    for (j, h) in ["Count", "Sum", "Average", "Variance"].iter().enumerate() {
        o.text(2, 1 + j as u32, h);
    }
    for i in 0..r {
        let g = sub(&data, i, 0, i, c - 1);
        let name = if labels {
            label_text(wb, &src, lab + i, 0, format!("Row {}", i + 1))
        } else {
            Value::text(format!("Row {}", i + 1).as_str())
        };
        summary_row(wb, &mut o, 3 + i, &name, &g);
    }
    let crow = 3 + r + 1;
    let mut devsq = Vec::new();
    for j in 0..c {
        let g = sub(&data, 0, j, r - 1, j);
        let name = if labels {
            label_text(wb, &src, 0, lab + j, format!("Column {}", j + 1))
        } else {
            Value::text(format!("Column {}", j + 1).as_str())
        };
        summary_row(wb, &mut o, crow + j, &name, &g);
        let d = format!("DEVSQ({})", abs_ref(wb, &g));
        devsq.push(num(&eval(wb, src.sheet, &d)));
    }
    let all = abs_ref(wb, &data);
    let sheet = src.sheet;
    let t = num(&eval(wb, sheet, &format!("DEVSQ({all})")));
    let sumd = devsq.iter().skip(1).fold(devsq[0], |a, b| a + b);
    let ssc = t - sumd;
    let (nr, nc) = (f64::from(r), f64::from(c));
    let mut rdev = Vec::new();
    for i in 0..r {
        let g = sub(&data, i, 0, i, c - 1);
        rdev.push(num(&eval(
            wb,
            sheet,
            &format!("DEVSQ({})", abs_ref(wb, &g)),
        )));
    }
    let sumr = rdev.iter().skip(1).fold(rdev[0], |a, b| a + b);
    let ssr = t - sumr;
    let e = t - ssc - ssr;
    let end = crow + c - 1;
    let rows = [
        Row {
            name: "Rows",
            ss: ssr,
            df: nr - 1.0,
        },
        Row {
            name: "Columns",
            ss: ssc,
            df: nc - 1.0,
        },
        Row {
            name: "Error",
            ss: e,
            df: (nr - 1.0) * (nc - 1.0),
        },
    ];
    let total = Row {
        name: "Total",
        ss: t,
        df: nr * nc - 1.0,
    };
    anova_table(wb, &mut o, end + 3, sheet, &rows, total, alpha);
    o.line(Rect::new(1, 0, 1, 4), Edge::Bottom, true);
    o.header(Rect::new(2, 0, 2, 4), false);
    o.line(Rect::new(2, 0, 2, 4), Edge::Top, true);
    o.line(Rect::new(2, 0, 2, 4), Edge::Bottom, false);
    o.line(Rect::new(end, 0, end, 4), Edge::Bottom, true);
    output(args, 1, o)
}

pub(super) fn anova2(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let src = input(args, 0)?;
    let per = number(args, 2).unwrap_or(0.0).trunc();
    let alpha = number(args, 3).unwrap_or(0.05);
    let data = sub(&src, 1, 1, src.rows() - 1, src.cols() - 1);
    if per < 1.0 || !data.rows().is_multiple_of(per as u32) {
        return Err(AtpError::Message(
            "The number of rows per sample is not valid.".into(),
        ));
    }
    let per = per as u32;
    let samples = data.rows() / per;
    let c = data.cols();
    let sheet = src.sheet;
    let mut o = Out::default();
    o.text(0, 0, "Anova: Two-Factor With Replication");
    o.text(2, 0, "SUMMARY");
    for j in 0..c {
        o.val(2, 1 + j, wb.value(src.at(0, 1 + j)));
    }
    o.text(2, 1 + c, "Total");
    let stat = |o: &mut Out, row: u32, col: u32, g: &Src| {
        let r = abs_ref(wb, g);
        for (i, f) in ["COUNT", "SUM", "AVERAGE", "VAR"].iter().enumerate() {
            o.val(row + i as u32, col, eval(wb, sheet, &format!("{f}({r})")));
        }
    };
    let mut row = 3;
    for s in 0..samples {
        o.val(row, 0, wb.value(src.at(1 + s * per, 0)));
        o.label(Rect::new(row, 0, row, c + 1));
        o.navy(Rect::new(row, 0, row, c + 1), Edge::Bottom);
        for (i, n) in ["Count", "Sum", "Average", "Variance"].iter().enumerate() {
            o.text(row + 1 + i as u32, 0, n);
        }
        for j in 0..c {
            let g = sub(&data, s * per, j, s * per + per - 1, j);
            stat(&mut o, row + 1, 1 + j, &g);
        }
        let g = sub(&data, s * per, 0, s * per + per - 1, c - 1);
        stat(&mut o, row + 1, 1 + c, &g);
        row += 6;
    }
    o.text(row, 0, "Total");
    let w = samples + 1;
    o.label(Rect::new(row, 0, row, w));
    o.navy(Rect::new(row, 0, row, w), Edge::Bottom);
    for (i, n) in ["Count", "Sum", "Average", "Variance"].iter().enumerate() {
        o.text(row + 1 + i as u32, 0, n);
    }

    let mut within = 0.0;
    for j in 0..c {
        for s in 0..samples {
            let g = sub(&data, s * per, j, s * per + per - 1, j);
            within += num(&eval(wb, sheet, &format!("DEVSQ({})", abs_ref(wb, &g))));
        }
    }
    let mut cols_dev = 0.0;
    for j in 0..c {
        let g = sub(&data, 0, j, data.rows() - 1, j);
        stat(&mut o, row + 1, 1 + j, &g);
        let mut tot = 0.0;
        for s in 0..samples {
            let b = sub(&data, s * per, j, s * per + per - 1, j);
            tot += num(&eval(wb, sheet, &format!("SUM({})", abs_ref(wb, &b))));
        }
        o.num(row + 2, 1 + j, tot);
        cols_dev += num(&eval(wb, sheet, &format!("DEVSQ({})", abs_ref(wb, &g))));
    }
    let all = abs_ref(wb, &data);
    let t = num(&eval(wb, sheet, &format!("DEVSQ({all})")));
    let mut samp_dev = 0.0;
    for s in 0..samples {
        let g = sub(&data, s * per, 0, s * per + per - 1, c - 1);
        samp_dev += num(&eval(wb, sheet, &format!("DEVSQ({})", abs_ref(wb, &g))));
    }
    let ss_sample = t - samp_dev;
    let ss_cols = t - cols_dev;
    let ss_int = t - ss_sample - ss_cols - within;
    let (ns, nc, nr) = (f64::from(samples), f64::from(c), f64::from(per));
    let rows = [
        Row {
            name: "Sample",
            ss: ss_sample,
            df: ns - 1.0,
        },
        Row {
            name: "Columns",
            ss: ss_cols,
            df: nc - 1.0,
        },
        Row {
            name: "Interaction",
            ss: ss_int,
            df: (ns - 1.0) * (nc - 1.0),
        },
        Row {
            name: "Within",
            ss: within,
            df: ns * nc * (nr - 1.0),
        },
    ];
    let total = Row {
        name: "Total",
        ss: t,
        df: ns * nc * nr - 1.0,
    };
    anova_table(wb, &mut o, row + 7, sheet, &rows, total, alpha);
    output(args, 1, o)
}
