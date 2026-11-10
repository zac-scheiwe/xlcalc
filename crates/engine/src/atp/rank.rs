use super::anova::{label_text, num, sub};
use super::{abs_ref, eval, flag, input, output, text, Arg, AtpError, Out, Run};
use crate::address::Rect;
use crate::style::Edge;
use crate::value::Value;
use crate::Workbook;

pub(super) fn rankperc(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let src = input(args, 0)?;
    let by_rows = text(args, 2).is_some_and(|s| s.eq_ignore_ascii_case("R"));
    let labels = flag(args, 3);
    let lab = u32::from(labels);
    let data = if by_rows {
        sub(&src, 0, lab, src.rows() - 1, src.cols() - 1)
    } else {
        sub(&src, lab, 0, src.rows() - 1, src.cols() - 1)
    };
    let k = if by_rows { data.rows() } else { data.cols() };
    let mut o = Out::default();
    let mut longest = 0;
    for g in 0..k {
        let gs = if by_rows {
            sub(&data, g, 0, g, data.cols() - 1)
        } else {
            sub(&data, 0, g, data.rows() - 1, g)
        };
        let n = if by_rows { gs.cols() } else { gs.rows() };
        let fallback = format!("{}{}", if by_rows { "Row" } else { "Column" }, g + 1);
        let name = if !labels {
            Value::text(fallback.as_str())
        } else if by_rows {
            label_text(wb, &src, g, 0, fallback)
        } else {
            label_text(wb, &src, 0, g, fallback)
        };
        let cell = |i: u32| wb.value(if by_rows { gs.at(0, i) } else { gs.at(i, 0) });
        let mut v: Vec<(u32, f64)> = (0..n)
            .filter_map(|i| match cell(i) {
                Value::Number(x) => Some((i + 1, x)),
                _ => None,
            })
            .collect();
        v.sort_by(|a, b| b.1.total_cmp(&a.1));
        let blanks: Vec<u32> = (0..n)
            .filter(|i| !matches!(cell(*i), Value::Number(_)))
            .map(|i| i + 1)
            .collect();
        let c0 = 4 * g;
        o.text(0, c0, "Point");
        o.val(0, c0 + 1, name);
        o.text(0, c0 + 2, "Rank");
        o.text(0, c0 + 3, "Percent");
        let range = abs_ref(wb, &gs);
        for (r, (pt, x)) in v.iter().enumerate() {
            let row = 1 + r as u32;
            o.num(row, c0, f64::from(*pt));
            o.num(row, c0 + 1, *x);
            let rank = num(&eval(wb, src.sheet, &format!("RANK({x:?},{range})")));
            o.num(row, c0 + 2, rank);
            o.val(
                row,
                c0 + 3,
                eval(wb, src.sheet, &format!("PERCENTRANK({range},{x:?})")),
            );
            o.format(row, c0 + 3, "0.00%");
        }
        for (b, pt) in blanks.iter().enumerate() {
            let row = 1 + (v.len() + b) as u32;
            o.num(row, c0, f64::from(*pt));
            o.na(row, c0 + 2);
            o.na(row, c0 + 3);
            o.format(row, c0 + 3, "0.00%");
        }
        longest = longest.max((v.len() + blanks.len()) as u32);
    }
    let last = 4 * k - 1;
    o.header(Rect::new(0, 0, 0, last), false);
    o.line(Rect::new(0, 0, 0, last), Edge::Top, true);
    o.line(Rect::new(0, 0, 0, last), Edge::Bottom, false);
    o.line(Rect::new(longest, 0, longest, last), Edge::Bottom, true);
    output(args, 1, o)
}
