use super::{flag, input, number, output, text, Arg, AtpError, Cell, Item, Out, Run};
use crate::Workbook;

pub(super) fn sample(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let src = input(args, 0)?;
    let periodic = text(args, 2).is_some_and(|s| s.eq_ignore_ascii_case("P"));
    if !periodic {
        return Err(AtpError::Message(
            "Random sampling is not reproducible.".into(),
        ));
    }
    let rate = number(args, 3).unwrap_or(0.0).trunc();
    if rate < 1.0 {
        return Err(AtpError::Message("The period must be at least 1.".into()));
    }
    let rate = rate as u32;
    let lab = u32::from(flag(args, 4));
    let mut o = Out::default();
    let mut n = 0;
    let mut k = 0;
    for c in 0..src.cols() {
        for r in lab..src.rows() {
            n += 1;
            if n % rate == 0 {
                o.items.push(Item {
                    row: k,
                    col: 0,
                    cell: Cell::Val(wb.value(src.at(r, c))),
                });
                k += 1;
            }
        }
    }
    output(args, 1, o)
}
