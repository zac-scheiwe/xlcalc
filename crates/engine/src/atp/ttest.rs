use super::anova::{num, sub};
use super::{abs_ref, eval, flag, func, input, number, output, Arg, AtpError, Out, Run, Src};
use crate::address::Rect;
use crate::style::Edge;
use crate::value::Value;
use crate::Workbook;

struct Two {
    a: Src,
    b: Src,
    names: [String; 2],
}

fn two(wb: &Workbook, args: &[Arg]) -> Result<Two, AtpError> {
    let (a, b) = (input(args, 0)?, input(args, 1)?);
    let labels = flag(args, 3);
    let one = |s: Src| {
        if labels {
            let name = match wb.value(s.at(0, 0)) {
                Value::Text(t) => t.to_string(),
                Value::Number(x) => crate::format_general(x),
                _ => String::new(),
            };
            (sub(&s, 1, 0, s.rows() - 1, s.cols() - 1), name)
        } else {
            (s, String::new())
        }
    };
    let (a, na) = one(a);
    let (b, nb) = one(b);
    let (na, nb) = if labels {
        (na, nb)
    } else {
        ("Variable 1".into(), "Variable 2".into())
    };
    Ok(Two {
        a,
        b,
        names: [na, nb],
    })
}

fn scalar(wb: &Workbook, s: &Src, f: &str, more: &str) -> f64 {
    num(&eval(
        wb,
        s.sheet,
        &format!("{f}({}{more})", abs_ref(wb, s)),
    ))
}

fn head(o: &mut Out, title: &str, t: &Two) {
    o.text(0, 0, title);
    o.text(2, 1, &t.names[0]);
    o.text(2, 2, &t.names[1]);
    o.line(Rect::new(1, 0, 1, 2), Edge::Bottom, true);
    o.header(Rect::new(2, 0, 2, 2), false);
    o.line(Rect::new(2, 0, 2, 2), Edge::Top, true);
    o.line(Rect::new(2, 0, 2, 2), Edge::Bottom, false);
}

fn tail(o: &mut Out, last: u32) {
    o.line(Rect::new(last, 0, last, 2), Edge::Bottom, true);
}

fn tdist(wb: &Workbook, a: &Src, t: f64, df: f64, tails: u8) -> Value {
    func(wb, a.sheet, "TDIST", &[t.abs(), df, f64::from(tails)])
}

fn tinv(wb: &Workbook, a: &Src, p: f64, df: f64) -> Value {
    func(wb, a.sheet, "TINV", &[p, df])
}

fn t_rows(wb: &Workbook, o: &mut Out, row: u32, a: &Src, t: f64, df: f64, alpha: f64) -> u32 {
    o.text(row, 0, "P(T<=t) one-tail");
    o.val(row, 1, tdist(wb, a, t, df, 1));
    o.text(row + 1, 0, "t Critical one-tail");
    o.val(row + 1, 1, tinv(wb, a, 2.0 * alpha, df));
    o.text(row + 2, 0, "P(T<=t) two-tail");
    o.val(row + 2, 1, tdist(wb, a, t, df, 2));
    o.text(row + 3, 0, "t Critical two-tail");
    o.val(row + 3, 1, tinv(wb, a, alpha, df));
    row + 3
}

fn mean_var_n(wb: &Workbook, o: &mut Out, t: &Two, var: Option<(f64, f64)>) -> [f64; 6] {
    o.text(3, 0, "Mean");
    o.text(
        4,
        0,
        if var.is_some() {
            "Known Variance"
        } else {
            "Variance"
        },
    );
    o.text(5, 0, "Observations");
    let mut r = [0.0; 6];
    for (j, s) in [&t.a, &t.b].into_iter().enumerate() {
        let c = 1 + j as u32;
        let m = scalar(wb, s, "AVERAGE", "");
        let n = scalar(wb, s, "COUNT", "");
        o.num(3, c, m);
        match var {
            Some((v1, v2)) => o.num(4, c, if j == 0 { v1 } else { v2 }),
            None => o.num(4, c, scalar(wb, s, "VAR", "")),
        }
        o.num(5, c, n);
        r[j] = m;
        r[2 + j] = match var {
            Some((v1, v2)) => {
                if j == 0 {
                    v1
                } else {
                    v2
                }
            }
            None => scalar(wb, s, "VAR", ""),
        };
        r[4 + j] = n;
    }
    r
}

pub(super) fn ttest_equal(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let t = two(wb, args)?;
    let alpha = number(args, 4).unwrap_or(0.05);
    let hyp = number(args, 5).unwrap_or(0.0);
    let mut o = Out::default();
    head(&mut o, "t-Test: Two-Sample Assuming Equal Variances", &t);
    let [m1, m2, v1, v2, n1, n2] = mean_var_n(wb, &mut o, &t, None);
    let pooled = ((n1 - 1.0) * v1 + (n2 - 1.0) * v2) / (n1 + n2 - 2.0);
    o.text(6, 0, "Pooled Variance");
    o.num(6, 1, pooled);
    o.text(7, 0, "Hypothesized Mean Difference");
    o.num(7, 1, hyp);
    let df = n1 + n2 - 2.0;
    o.text(8, 0, "df");
    o.num(8, 1, df);
    let tt = (m1 - m2 - hyp) / (pooled * (1.0 / n1 + 1.0 / n2)).sqrt();
    o.text(9, 0, "t Stat");
    o.num(9, 1, tt);
    let last = t_rows(wb, &mut o, 10, &t.a, tt, df, alpha);
    tail(&mut o, last);
    output(args, 2, o)
}

pub(super) fn ttest_unequal(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let t = two(wb, args)?;
    let alpha = number(args, 4).unwrap_or(0.05);
    let hyp = number(args, 5).unwrap_or(0.0);
    let mut o = Out::default();
    head(&mut o, "t-Test: Two-Sample Assuming Unequal Variances", &t);
    let [m1, m2, v1, v2, n1, n2] = mean_var_n(wb, &mut o, &t, None);
    o.text(6, 0, "Hypothesized Mean Difference");
    o.num(6, 1, hyp);
    let (q1, q2) = (v1 / n1, v2 / n2);
    let df = ((q1 + q2) * (q1 + q2) / (q1 * q1 / (n1 - 1.0) + q2 * q2 / (n2 - 1.0))).round();
    o.text(7, 0, "df");
    o.num(7, 1, df);
    let tt = (m1 - m2 - hyp) / (q1 + q2).sqrt();
    o.text(8, 0, "t Stat");
    o.num(8, 1, tt);
    let last = t_rows(wb, &mut o, 9, &t.a, tt, df, alpha);
    tail(&mut o, last);
    output(args, 2, o)
}

pub(super) fn ttest_paired(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let t = two(wb, args)?;
    let alpha = number(args, 4).unwrap_or(0.05);
    let hyp = number(args, 5).unwrap_or(0.0);
    let mut o = Out::default();
    head(&mut o, "t-Test: Paired Two Sample for Means", &t);
    let [m1, m2, v1, v2, n1, _] = mean_var_n(wb, &mut o, &t, None);
    let (ra, rb) = (abs_ref(wb, &t.a), abs_ref(wb, &t.b));
    o.text(6, 0, "Pearson Correlation");
    let r = num(&eval(wb, t.a.sheet, &format!("CORREL({ra},{rb})")));
    o.num(6, 1, r);
    o.text(7, 0, "Hypothesized Mean Difference");
    o.num(7, 1, hyp);
    let df = n1 - 1.0;
    o.text(8, 0, "df");
    o.num(8, 1, df);
    let cov = r * (v1 * v2).sqrt();
    let vd = v1 + v2 - 2.0 * cov;
    let tt = (m1 - m2 - hyp) / (vd / n1).sqrt();
    o.text(9, 0, "t Stat");
    o.num(9, 1, tt);
    let last = t_rows(wb, &mut o, 10, &t.a, tt, df, alpha);
    tail(&mut o, last);
    output(args, 2, o)
}

pub(super) fn ztest(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let t = two(wb, args)?;
    let alpha = number(args, 4).unwrap_or(0.05);
    let hyp = number(args, 5).unwrap_or(0.0);
    let var = (
        number(args, 6).unwrap_or(0.0),
        number(args, 7).unwrap_or(0.0),
    );
    let mut o = Out::default();
    head(&mut o, "z-Test: Two Sample for Means", &t);
    let [m1, m2, v1, v2, n1, n2] = mean_var_n(wb, &mut o, &t, Some(var));
    o.text(6, 0, "Hypothesized Mean Difference");
    o.num(6, 1, hyp);
    let z = (m1 - m2 - hyp) / (v1 / n1 + v2 / n2).sqrt();
    o.text(7, 0, "z");
    o.num(7, 1, z);
    let sh = t.a.sheet;
    let p1 = if z < 0.0 {
        func(wb, sh, "NORMSDIST", &[z])
    } else {
        Value::Number(1.0 - num(&func(wb, sh, "NORMSDIST", &[z])))
    };
    o.text(8, 0, "P(Z<=z) one-tail");
    o.val(8, 1, p1.clone());
    o.text(9, 0, "z Critical one-tail");
    o.val(9, 1, func(wb, sh, "NORMSINV", &[1.0 - alpha]));
    o.text(10, 0, "P(Z<=z) two-tail");
    o.num(10, 1, 2.0 * num(&p1));
    o.text(11, 0, "z Critical two-tail");
    o.val(11, 1, func(wb, sh, "NORMSINV", &[1.0 - alpha / 2.0]));
    tail(&mut o, 11);
    output(args, 2, o)
}

pub(super) fn ftest(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let t = two(wb, args)?;
    let alpha = number(args, 4).unwrap_or(0.05);
    let mut o = Out::default();
    head(&mut o, "F-Test Two-Sample for Variances", &t);
    let [_, _, v1, v2, n1, n2] = mean_var_n(wb, &mut o, &t, None);
    o.text(6, 0, "df");
    o.num(6, 1, n1 - 1.0);
    o.num(6, 2, n2 - 1.0);
    let f = v1 / v2;
    o.text(7, 0, "F");
    o.num(7, 1, f);
    let sh = t.a.sheet;
    o.text(8, 0, "P(F<=f) one-tail");
    let (d1, d2) = (n1 - 1.0, n2 - 1.0);
    let p = func(wb, sh, "FDIST", &[f, d1, d2]);
    o.val(
        8,
        1,
        if f < 1.0 {
            Value::Number(1.0 - num(&p))
        } else {
            p
        },
    );
    o.text(9, 0, "F Critical one-tail");
    let a = if f < 1.0 { 1.0 - alpha } else { alpha };
    o.val(9, 1, func(wb, sh, "FINV", &[a, d1, d2]));
    tail(&mut o, 9);
    output(args, 2, o)
}
