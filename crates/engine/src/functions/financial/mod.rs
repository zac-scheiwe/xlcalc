mod atp;
mod bonds;
mod cash;
mod days;
mod dep;
mod tvm;
mod xmath;

use super::{args, scalar, FnDef, MANY};
use crate::eval::{EvalCtx, Operand};

macro_rules! atp_fn {
    ($name:literal, $min:expr, $max:expr, $f:path) => {{
        fn wrapper(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
            atp::call(ctx, ops, &[], true, $f)
        }
        args($name, $min, $max, "R", wrapper)
    }};
    ($name:literal, $min:expr, $max:expr, $f:path, [$($arr:expr),*]) => {{
        fn wrapper(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
            atp::call(ctx, ops, &[$($arr),*], true, $f)
        }
        args($name, $min, $max, "R", wrapper)
    }};
}

macro_rules! atp_fa {
    ($name:literal, $min:expr, $max:expr, $f:path) => {{
        fn wrapper(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
            atp::call(ctx, ops, &[], false, $f)
        }
        args($name, $min, $max, "R", wrapper)
    }};
}

pub const DEFS: &[FnDef] = &[
    scalar("PMT", 3, 5, tvm::pmt),
    scalar("IPMT", 4, 6, tvm::ipmt),
    scalar("PPMT", 4, 6, tvm::ppmt),
    scalar("PV", 3, 5, tvm::pv),
    scalar("FV", 3, 5, tvm::fv),
    scalar("NPER", 3, 5, tvm::nper),
    scalar("RATE", 3, 6, tvm::rate),
    scalar("ISPMT", 4, 4, tvm::ispmt),
    scalar("PDURATION", 3, 3, tvm::pduration),
    scalar("RRI", 3, 3, tvm::rri),
    atp_fn!("CUMIPMT", 6, 6, tvm::cumipmt),
    atp_fn!("CUMPRINC", 6, 6, tvm::cumprinc),
    atp_fn!("EFFECT", 2, 2, tvm::effect),
    atp_fn!("NOMINAL", 2, 2, tvm::nominal),
    atp_fn!("FVSCHEDULE", 2, 2, tvm::fvschedule, [1]),
    args("NPV", 2, MANY, "VR", cash::npv),
    args("IRR", 1, 2, "RV", cash::irr),
    args("MIRR", 3, 3, "RVV", cash::mirr),
    atp_fn!("XNPV", 3, 3, cash::xnpv, [1, 2]),
    atp_fn!("XIRR", 2, 3, cash::xirr, [0, 1]),
    scalar("SLN", 3, 3, dep::sln),
    scalar("SYD", 4, 4, dep::syd),
    scalar("DDB", 4, 5, dep::ddb),
    scalar("DB", 4, 5, dep::db),
    scalar("VDB", 5, 7, dep::vdb),
    atp_fn!("AMORDEGRC", 6, 7, dep::amordegrc),
    atp_fn!("AMORLINC", 6, 7, dep::amorlinc),
    atp_fn!("DOLLARDE", 2, 2, dep::dollarde),
    atp_fn!("DOLLARFR", 2, 2, dep::dollarfr),
    args("YEARFRAC", 2, 3, "R", |c, o| {
        atp::call_ex(c, o, &[], true, true, days::yearfrac)
    }),
    scalar("DAYS360", 2, 3, days::days360),
    atp_fn!("COUPDAYBS", 3, 4, days::coupdaybs),
    atp_fn!("COUPDAYS", 3, 4, days::coupdays),
    atp_fn!("COUPDAYSNC", 3, 4, days::coupdaysnc),
    atp_fn!("COUPNCD", 3, 4, days::coupncd),
    atp_fa!("COUPNUM", 3, 4, days::coupnum),
    atp_fn!("COUPPCD", 3, 4, days::couppcd),
    atp_fa!("ACCRINT", 6, 8, bonds::accrint),
    atp_fn!("ACCRINTM", 4, 5, bonds::accrintm),
    atp_fn!("DISC", 4, 5, bonds::disc),
    atp_fn!("INTRATE", 4, 5, bonds::intrate),
    atp_fn!("RECEIVED", 4, 5, bonds::received),
    atp_fn!("PRICEDISC", 4, 5, bonds::pricedisc),
    atp_fn!("YIELDDISC", 4, 5, bonds::yielddisc),
    atp_fn!("PRICEMAT", 5, 6, bonds::pricemat),
    atp_fn!("YIELDMAT", 5, 6, bonds::yieldmat),
    atp_fa!("TBILLEQ", 3, 3, bonds::tbilleq),
    atp_fa!("TBILLPRICE", 3, 3, bonds::tbillprice),
    atp_fa!("TBILLYIELD", 3, 3, bonds::tbillyield),
    atp_fn!("PRICE", 6, 7, bonds::price),
    atp_fn!("YIELD", 6, 7, bonds::yield_),
    atp_fn!("DURATION", 5, 6, bonds::duration),
    atp_fn!("MDURATION", 5, 6, bonds::mduration),
    atp_fn!("ODDFPRICE", 8, 9, bonds::oddfprice),
    atp_fn!("ODDFYIELD", 8, 9, bonds::oddfyield),
    atp_fn!("ODDLPRICE", 7, 8, bonds::oddlprice),
    atp_fn!("ODDLYIELD", 7, 8, bonds::oddlyield),
];
