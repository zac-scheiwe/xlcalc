use super::anova::{label_text, sub};
use super::{abs_ref, eval, flag, input, output, text, Arg, AtpError, Cell, Item, Out, Run};
use crate::address::Rect;
use crate::style::Edge;
use crate::value::Value;
use crate::Workbook;

fn matrix(wb: &Workbook, args: &[Arg], covar: bool) -> Result<Run, AtpError> {
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
    let group = |g: u32| {
        if by_rows {
            sub(&data, g, 0, g, data.cols() - 1)
        } else {
            sub(&data, 0, g, data.rows() - 1, g)
        }
    };
    let mut o = Out::default();
    for g in 0..k {
        let fallback = format!("{} {}", if by_rows { "Row" } else { "Column" }, g + 1);
        let name = if !labels {
            Value::text(fallback.as_str())
        } else if by_rows {
            label_text(wb, &src, g, 0, fallback)
        } else {
            label_text(wb, &src, 0, g, fallback)
        };
        o.val(0, 1 + g, name.clone());
        o.val(1 + g, 0, name);
        for h in 0..=g {
            let (a, b) = (abs_ref(wb, &group(g)), abs_ref(wb, &group(h)));
            if covar && g == h {
                o.items.push(Item {
                    row: 1 + g,
                    col: 1 + h,
                    cell: Cell::Formula(format!("VARP({a})")),
                });
            } else if !covar && g == h {
                o.num(1 + g, 1 + h, 1.0);
            } else {
                let f = if covar { "COVAR" } else { "CORREL" };
                o.val(1 + g, 1 + h, eval(wb, src.sheet, &format!("{f}({a},{b})")));
            }
        }
    }
    o.header(Rect::new(0, 0, 0, k), false);
    o.line(Rect::new(0, 0, 0, k), Edge::Top, true);
    o.line(Rect::new(0, 0, 0, k), Edge::Bottom, false);
    o.line(Rect::new(k, 0, k, k), Edge::Bottom, true);
    output(args, 1, o)
}

pub(super) fn correl(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    matrix(wb, args, false)
}

pub(super) fn covar(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    matrix(wb, args, true)
}
