use super::{a1_col, flag, input, number, output, Arg, AtpError, Out, Run};
use crate::format_general;

fn constant(x: f64) -> String {
    format_general(x)
}

fn std_errors(
    o: &mut Out,
    m: u32,
    first: u32,
    n: u32,
    data: impl Fn(u32, u32) -> String,
    ocol: u32,
    orow: u32,
) {
    for j in 0..m {
        if j < first {
            o.na(j, 1);
        } else {
            let e = a1_col(ocol, orow + j + 1 - n, orow + j);
            o.formula(
                j,
                1,
                format!("SQRT(SUMXMY2({},{e})/{n})", data(j + 1 - n, j)),
            );
        }
    }
}

pub(super) fn moveavg(args: &[Arg]) -> Result<Run, AtpError> {
    let src = input(args, 0)?;
    let interval = number(args, 2).unwrap_or(3.0);
    let (std, labels) = (flag(args, 3), flag(args, 5));
    let n = interval as u32;
    let lab = u32::from(labels);
    let m = src.rows().saturating_sub(lab);
    let out = output(args, 1, Out::default())?;
    let mut o = Out::default();
    let dcol = src.rect.c0;
    let ocol = out.col;
    let drow = |k: u32| src.rect.r0 + lab + k;
    for j in 0..m {
        if j + 1 < n {
            o.na(j, 0);
        } else {
            o.formula(
                j,
                0,
                format!("AVERAGE({})", a1_col(dcol, drow(j + 1 - n), drow(j))),
            );
        }
    }
    if std {
        std_errors(
            &mut o,
            m,
            2 * n.saturating_sub(1),
            n,
            |a, b| a1_col(dcol, drow(a), drow(b)),
            ocol,
            out.row,
        );
    }
    output(args, 1, o)
}

pub(super) fn expon(args: &[Arg]) -> Result<Run, AtpError> {
    let src = input(args, 0)?;
    let damping = number(args, 2).unwrap_or(0.3);
    let (std, labels) = (flag(args, 3), flag(args, 5));
    let lab = u32::from(labels);
    let m = src.rows().saturating_sub(lab);
    let out = output(args, 1, Out::default())?;
    let mut o = Out::default();
    let (dcol, ocol) = (src.rect.c0, out.col);
    let drow = |k: u32| src.rect.r0 + lab + k;
    let (alpha, d) = (constant(1.0 - damping), constant(damping));
    for j in 0..m {
        if j == 0 {
            o.na(0, 0);
        } else if j == 1 {
            o.formula(j, 0, a1_col(dcol, drow(0), drow(0)));
        } else {
            o.formula(
                j,
                0,
                format!(
                    "{alpha}*{}+{d}*{}",
                    a1_col(dcol, drow(j - 1), drow(j - 1)),
                    a1_col(ocol, out.row + j - 1, out.row + j - 1)
                ),
            );
        }
    }
    if std {
        for j in 0..m {
            if j < 4 {
                o.na(j, 1);
            } else {
                o.formula(
                    j,
                    1,
                    format!(
                        "SQRT(SUMXMY2({},{})/3)",
                        a1_col(dcol, drow(j - 3), drow(j - 1)),
                        a1_col(ocol, out.row + j - 3, out.row + j - 1)
                    ),
                );
            }
        }
    }
    output(args, 1, o)
}
