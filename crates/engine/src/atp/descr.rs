use super::{abs_ref, eval, flag, input, number, output, text, Arg, AtpError, Out, Run, Src};
use crate::address::Rect;
use crate::style::Edge;
use crate::value::Value;
use crate::Workbook;

fn k_arg(args: &[Arg], i: usize) -> Option<f64> {
    match args.get(i) {
        Some(Arg::Num(n)) => Some(*n),
        _ => None,
    }
}

fn k_text(k: f64) -> String {
    crate::format_general(k.trunc())
}

pub(super) fn descr(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let src = input(args, 0)?;
    let by_rows = text(args, 2).is_some_and(|s| s.eq_ignore_ascii_case("R"));
    let labels = flag(args, 3);
    let summary = flag(args, 4);
    let large = k_arg(args, 5);
    let small = k_arg(args, 6);
    let conf = number_only(args, 7);
    let n = if by_rows { src.rows() } else { src.cols() };
    let mut o = Out::default();
    let mut last = 1;
    for i in 0..n {
        let (data, label) = if by_rows {
            let c0 = u32::from(labels);
            let r = Src {
                sheet: src.sheet,
                rect: Rect::new(
                    src.rect.r0 + i,
                    src.rect.c0 + c0,
                    src.rect.r0 + i,
                    src.rect.c1,
                ),
            };
            (r, labels.then(|| (src.at(i, 0), src.sheet)))
        } else {
            let r0 = u32::from(labels);
            let r = Src {
                sheet: src.sheet,
                rect: Rect::new(
                    src.rect.r0 + r0,
                    src.rect.c0 + i,
                    src.rect.r1,
                    src.rect.c0 + i,
                ),
            };
            (r, labels.then(|| (src.at(0, i), src.sheet)))
        };
        let c = 2 * i;
        let head = match label {
            Some((addr, _)) => match wb.value(addr) {
                Value::Text(t) => t.to_string(),
                Value::Blank => String::new(),
                v => crate::format_general(match v {
                    Value::Number(x) => x,
                    _ => 0.0,
                }),
            },
            None => format!("{}{}", if by_rows { "Row" } else { "Column" }, i + 1),
        };
        o.text(0, c, &head);
        let r = abs_ref(wb, &data);
        let ev = |f: String| eval(wb, src.sheet, &f);
        let mut row = 2;
        let mut put = |o: &mut Out, name: String, v: Value| {
            o.text(row, c, &name);
            o.val(row, c + 1, v);
            row += 1;
        };
        if summary {
            let sd = format!("STDEV({r})");
            let rows: [(&str, String); 13] = [
                ("Mean", format!("AVERAGE({r})")),
                ("Standard Error", format!("{sd}/SQRT(COUNT({r}))")),
                ("Median", format!("MEDIAN({r})")),
                ("Mode", format!("MODE({r})")),
                ("Standard Deviation", sd),
                ("Sample Variance", format!("VAR({r})")),
                ("Kurtosis", format!("KURT({r})")),
                ("Skewness", format!("SKEW({r})")),
                ("Range", format!("MAX({r})-MIN({r})")),
                ("Minimum", format!("MIN({r})")),
                ("Maximum", format!("MAX({r})")),
                ("Sum", format!("SUM({r})")),
                ("Count", format!("COUNT({r})")),
            ];
            for (name, f) in rows {
                put(&mut o, name.to_string(), ev(f));
            }
        }
        if let Some(k) = large {
            put(
                &mut o,
                format!("Largest({})", k_text(k)),
                ev(format!("LARGE({r},{})", crate::format_general(k))),
            );
        }
        if let Some(k) = small {
            put(
                &mut o,
                format!("Smallest({})", k_text(k)),
                ev(format!("SMALL({r},{})", crate::format_general(k))),
            );
        }
        if let Some(p) = conf {
            put(
                &mut o,
                format!("Confidence Level({p:.1}%)"),
                ev(format!(
                    "CONFIDENCE.T(1-{}/100,STDEV({r}),COUNT({r}))",
                    crate::format_general(p)
                )),
            );
        }
        last = row;
    }
    let width = 2 * n;
    o.header(Rect::new(0, 0, 0, width - 1), n == 1);
    o.line(Rect::new(0, 0, 0, width - 1), Edge::Top, true);
    o.line(Rect::new(0, 0, 0, width - 1), Edge::Bottom, false);
    o.line(
        Rect::new(last - 1, 0, last - 1, width - 1),
        Edge::Bottom,
        true,
    );
    output(args, 1, o)
}

fn number_only(args: &[Arg], i: usize) -> Option<f64> {
    match args.get(i) {
        Some(Arg::Num(_)) => number(args, i),
        _ => None,
    }
}
