use crate::address::{Rect, SheetId};

use super::Workbook;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Op {
    NoAutoExpand,

    DeferPasteRecalc,

    HoldIter,

    DtBlock,

    ReplaceEntry,

    TextToColumns,

    PasteHeader,
}

impl Op {
    const fn bit(self) -> u8 {
        1 << self as u8
    }
}

pub(crate) struct OpFlags {
    on: u8,

    paste_block: Option<(SheetId, Rect)>,
}

impl OpFlags {
    pub(super) const IDLE: OpFlags = OpFlags {
        on: 0,
        paste_block: None,
    };
}

struct Scope<'a> {
    wb: &'a mut Workbook,
    saved_on: u8,
    saved_block: Option<(SheetId, Rect)>,
}

impl<'a> Scope<'a> {
    fn enter(wb: &'a mut Workbook) -> Scope<'a> {
        let (saved_on, saved_block) = (wb.op.on, wb.op.paste_block);
        Scope {
            wb,
            saved_on,
            saved_block,
        }
    }
}

impl Drop for Scope<'_> {
    fn drop(&mut self) {
        self.wb.op.on = self.saved_on;
        self.wb.op.paste_block = self.saved_block;
    }
}

impl Workbook {
    #[inline]
    pub(crate) fn op_on(&self, op: Op) -> bool {
        self.op.on & op.bit() != 0
    }

    #[inline]
    pub(crate) fn paste_block(&self) -> Option<(SheetId, Rect)> {
        self.op.paste_block
    }

    pub(crate) fn ops_idle(&self) -> bool {
        self.op.on == 0 && self.op.paste_block.is_none()
    }

    pub(crate) fn with_op<R>(&mut self, op: Op, on: bool, f: impl FnOnce(&mut Workbook) -> R) -> R {
        let scope = Scope::enter(self);
        if on {
            scope.wb.op.on |= op.bit();
        } else {
            scope.wb.op.on &= !op.bit();
        }
        f(scope.wb)
    }

    pub(crate) fn with_paste_block<R>(
        &mut self,
        block: (SheetId, Rect),
        f: impl FnOnce(&mut Workbook) -> R,
    ) -> R {
        let scope = Scope::enter(self);
        scope.wb.op.paste_block = Some(block);
        f(scope.wb)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scopes_restore_the_previous_state() {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        assert!(wb.ops_idle());
        let r: Result<(), ()> = wb.with_op(Op::HoldIter, true, |wb| {
            assert!(wb.op_on(Op::HoldIter) && !wb.op_on(Op::DtBlock));
            wb.with_op(Op::HoldIter, false, |wb| assert!(!wb.op_on(Op::HoldIter)));

            assert!(wb.op_on(Op::HoldIter));
            wb.with_paste_block((s, Rect::new(0, 0, 1, 1)), |wb| {
                assert!(wb.paste_block().is_some());
                Err(())
            })?;
            unreachable!()
        });
        assert!(r.is_err());
        assert!(
            wb.ops_idle(),
            "an early return through `?` leaves nothing set"
        );
        let p = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            wb.with_op(Op::DtBlock, true, |_| panic!("inside an operation"))
        }));
        assert!(p.is_err());
        assert!(wb.ops_idle(), "a panic leaves nothing set");
    }
}
