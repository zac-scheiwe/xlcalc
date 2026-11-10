use super::{abs_ref, eval, flag, input, number, output, Arg, AtpError, Out, Run, Src};
use crate::address::Rect;
use crate::style::Edge;
use crate::value::Value;
use crate::Workbook;

fn num(v: &Value) -> f64 {
    match v {
        Value::Number(x) => *x,
        _ => f64::NAN,
    }
}

fn label(wb: &Workbook, s: &Src, i: u32) -> Option<String> {
    match wb.value(s.at(0, i)) {
        Value::Text(t) => Some(t.to_string()),
        Value::Number(x) => Some(crate::format_general(x)),
        _ => None,
    }
}

pub(super) fn regress(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let ys = input(args, 0)?;
    let xs = input(args, 1)?;
    let zero = flag(args, 2);
    let labels = flag(args, 3);
    let conf = number(args, 4).unwrap_or(95.0);
    let want_res = flag(args, 6);
    let want_std = flag(args, 7);
    let lab = u32::from(labels);
    let n = ys.rows() - lab;
    let k = xs.cols();
    let sub = |s: &Src| Src {
        sheet: s.sheet,
        rect: Rect::new(s.rect.r0 + lab, s.rect.c0, s.rect.r1, s.rect.c1),
    };
    let (yr, xr) = (abs_ref(wb, &sub(&ys)), abs_ref(wb, &sub(&xs)));
    let l = format!(
        "LINEST({yr},{xr},{},TRUE)",
        if zero { "FALSE" } else { "TRUE" }
    );
    let at = |r: u32, c: u32| format!("INDEX({l},{r},{c})");
    let ev = |f: String| eval(wb, ys.sheet, &f);
    let df = at(4, 2);
    let mut o = Out::default();

    o.text(0, 0, "SUMMARY OUTPUT");
    o.text(2, 0, "Regression Statistics");
    let stats = [
        ("Multiple R", format!("SQRT({})", at(3, 1))),
        ("R Square", at(3, 1)),
        (
            "Adjusted R Square",
            format!("(({n}-1)*{}-{k})/{df}", at(3, 1)),
        ),
        ("Standard Error", at(3, 2)),
    ];
    for (i, (name, f)) in stats.into_iter().enumerate() {
        o.text(3 + i as u32, 0, name);
        o.val(3 + i as u32, 1, ev(f));
    }
    o.text(7, 0, "Observations");
    o.num(7, 1, f64::from(n));

    o.text(9, 0, "ANOVA");
    for (j, h) in ["df", "SS", "MS", "F", "Significance F"].iter().enumerate() {
        o.text(10, 1 + j as u32, h);
    }
    o.text(11, 0, "Regression");
    o.num(11, 1, f64::from(k));
    o.val(11, 2, ev(at(5, 1)));
    o.val(11, 3, ev(format!("{}/{k}", at(5, 1))));
    o.val(11, 4, ev(at(4, 1)));
    o.val(11, 5, ev(format!("FDIST({},{k},{n}-{k}-1)", at(4, 1))));
    o.text(12, 0, "Residual");
    o.val(12, 1, ev(df.clone()));
    o.val(12, 2, ev(at(5, 2)));
    o.val(12, 3, ev(format!("{}/{df}", at(5, 2))));
    o.text(13, 0, "Total");
    o.val(13, 1, ev(format!("{k}+{df}")));
    o.val(13, 2, ev(format!("{}+{}", at(5, 1), at(5, 2))));

    let heads = [
        "Coefficients".to_string(),
        "Standard Error".to_string(),
        "t Stat".to_string(),
        "P-value".to_string(),
        "Lower 95%".to_string(),
        "Upper 95%".to_string(),
        format!("Lower {conf:.1}%"),
        format!("Upper {conf:.1}%"),
    ];
    for (j, h) in heads.iter().enumerate() {
        o.text(15, 1 + j as u32, h);
    }
    let alpha = format!("(100-{})/100", crate::format_general(conf));
    for i in 0..=k {
        let row = 16 + i;
        let col = if i == 0 { k + 1 } else { k + 1 - i };
        let name = if i == 0 {
            "Intercept".to_string()
        } else if labels {
            label(wb, &xs, i - 1).unwrap_or_else(|| format!("X Variable {i}"))
        } else {
            format!("X Variable {i}")
        };
        o.text(row, 0, &name);
        let (b, se) = (at(1, col), at(2, col));
        o.val(row, 1, ev(b.clone()));
        o.val(row, 2, ev(se.clone()));
        o.val(row, 3, ev(format!("{b}/{se}")));
        o.val(row, 4, ev(format!("TDIST(ABS({b}/{se}),{df},2)")));
        o.val(row, 5, ev(format!("{b}-TINV(0.05,{df})*{se}")));
        o.val(row, 6, ev(format!("{b}+TINV(0.05,{df})*{se}")));
        o.val(row, 7, ev(format!("{b}-TINV({alpha},{df})*{se}")));
        o.val(row, 8, ev(format!("{b}+TINV({alpha},{df})*{se}")));
    }
    let last = 16 + k;

    if want_res || want_std {
        let r0 = last + 4;
        let std_col = want_std;
        let ycell = |i: u32| num(&wb.value(ys.at(lab + i, 0)));
        let coef: Vec<f64> = (0..=k)
            .map(|i| num(&ev(at(1, if i == 0 { k + 1 } else { k + 1 - i }))))
            .collect();
        let pred: Vec<f64> = (0..n)
            .map(|i| {
                let mut p = coef[0];
                for j in 0..k {
                    p += coef[1 + j as usize] * num(&wb.value(xs.at(lab + i, j)));
                }
                p
            })
            .collect();
        let res: Vec<f64> = (0..n).map(|i| ycell(i) - pred[i as usize]).collect();
        let nf = f64::from(n);
        let ss: f64 = res.iter().map(|r| r * r).sum();
        let sd = (ss / (nf - f64::from(u8::from(!zero)))).sqrt();
        o.text(r0, 0, "RESIDUAL OUTPUT");
        let ylabel = if labels {
            label(wb, &ys, 0).unwrap_or_else(|| "Y".into())
        } else {
            "Y".into()
        };
        o.text(r0 + 2, 0, "Observation");
        o.text(r0 + 2, 1, &format!("Predicted {ylabel}"));
        o.text(r0 + 2, 2, "Residuals");
        if std_col {
            o.text(r0 + 2, 3, "Standard Residuals");
        }
        for i in 0..n {
            let row = r0 + 3 + i;
            o.num(row, 0, f64::from(i + 1));
            o.num(row, 1, pred[i as usize]);
            o.num(row, 2, res[i as usize]);
            if std_col {
                o.num(row, 3, res[i as usize] / sd);
            }
        }
        let rw = 3 + u32::from(std_col);
        o.line(Rect::new(r0 + 1, 0, r0 + 1, rw - 1), Edge::Bottom, true);
        o.header(Rect::new(r0 + 2, 0, r0 + 2, rw - 1), false);
        o.line(Rect::new(r0 + 2, 0, r0 + 2, rw - 1), Edge::Top, true);
        o.line(Rect::new(r0 + 2, 0, r0 + 2, rw - 1), Edge::Bottom, false);
        let end = r0 + 2 + n;
        o.line(Rect::new(end, 0, end, rw - 1), Edge::Bottom, true);
    }
    o.line(Rect::new(1, 0, 1, 1), Edge::Bottom, true);
    o.header(Rect::new(2, 0, 2, 1), true);
    o.line(Rect::new(2, 0, 2, 1), Edge::Top, true);
    o.line(Rect::new(2, 0, 2, 1), Edge::Bottom, false);
    o.line(Rect::new(7, 0, 7, 1), Edge::Bottom, true);
    o.line(Rect::new(9, 0, 9, 5), Edge::Bottom, true);
    o.header(Rect::new(10, 0, 10, 5), false);
    o.line(Rect::new(10, 0, 10, 5), Edge::Top, true);
    o.line(Rect::new(10, 0, 10, 5), Edge::Bottom, false);
    o.line(Rect::new(13, 0, 13, 5), Edge::Bottom, true);
    o.line(Rect::new(14, 0, 14, 8), Edge::Bottom, true);
    o.header(Rect::new(15, 0, 15, 8), false);
    o.line(Rect::new(15, 0, 15, 8), Edge::Top, true);
    o.line(Rect::new(15, 0, 15, 8), Edge::Bottom, false);
    o.line(Rect::new(last, 0, last, 8), Edge::Bottom, true);
    output(args, 5, o)
}
