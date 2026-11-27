pub mod crit;
pub mod db;
pub mod ifs;
pub mod stats;
pub mod subtotal;

#[cfg(test)]
mod tests;

use self::db::{dfunc, DKind};
use self::ifs::Agg;
use super::{args, checked, FnDef};
use crate::parser::ast::Expr;

const PAIRS: &str = concat!(
    "RVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRV",
    "RVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRV",
    "RVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRV",
    "RVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRV",
);

const VALUE_PAIRS: &str = concat!(
    "R",
    "RVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRV",
    "RVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRV",
    "RVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRV",
    "RVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRVRV",
);

fn aggregate_form(a: &[Expr]) -> Result<(), &'static str> {
    if a.iter()
        .skip(4)
        .any(|e| !crate::functions::is_reference_expr(e))
    {
        return Err("AGGREGATE reference arguments");
    }
    if let Expr::Number(n) = *a[0].unwrap_ws() {
        if (14.0..20.0).contains(&n) && a.len() > 4 {
            return Err("functions 14-19 take an array and k");
        }
    }
    Ok(())
}

pub const DEFS: &[FnDef] = &[
    args("COUNTIF", 2, 2, "RV", ifs::countif),
    args("SUMIF", 2, 3, "RVR", |c, o| ifs::sumif(c, o, Agg::Sum)),
    args("AVERAGEIF", 2, 3, "RVR", |c, o| {
        ifs::sumif(c, o, Agg::Average)
    }),
    args("COUNTIFS", 2, 255, PAIRS, |c, o| ifs::ifs(c, o, Agg::Count)),
    args("SUMIFS", 3, 255, VALUE_PAIRS, |c, o| {
        ifs::ifs(c, o, Agg::Sum)
    }),
    args("AVERAGEIFS", 3, 255, VALUE_PAIRS, |c, o| {
        ifs::ifs(c, o, Agg::Average)
    }),
    args("MAXIFS", 3, 254, VALUE_PAIRS, |c, o| {
        ifs::ifs(c, o, Agg::Max)
    }),
    args("MINIFS", 3, 254, VALUE_PAIRS, |c, o| {
        ifs::ifs(c, o, Agg::Min)
    }),
    args("DSUM", 3, 3, "RRR", |c, o| dfunc(c, o, DKind::Sum)),
    args("DAVERAGE", 3, 3, "RRR", |c, o| dfunc(c, o, DKind::Average)),
    args("DCOUNT", 3, 3, "RRR", |c, o| dfunc(c, o, DKind::Count)),
    args("DCOUNTA", 3, 3, "RRR", |c, o| dfunc(c, o, DKind::CountA)),
    args("DGET", 3, 3, "RRR", |c, o| dfunc(c, o, DKind::Get)),
    args("DMAX", 3, 3, "RRR", |c, o| dfunc(c, o, DKind::Max)),
    args("DMIN", 3, 3, "RRR", |c, o| dfunc(c, o, DKind::Min)),
    args("DPRODUCT", 3, 3, "RRR", |c, o| dfunc(c, o, DKind::Product)),
    args("DSTDEV", 3, 3, "RRR", |c, o| dfunc(c, o, DKind::StDev)),
    args("DSTDEVP", 3, 3, "RRR", |c, o| dfunc(c, o, DKind::StDevP)),
    args("DVAR", 3, 3, "RRR", |c, o| dfunc(c, o, DKind::Var)),
    args("DVARP", 3, 3, "RRR", |c, o| dfunc(c, o, DKind::VarP)),
    args("SUBTOTAL", 2, 255, "VR", subtotal::subtotal),
    checked(
        args("AGGREGATE", 3, 254, "RRR", subtotal::aggregate),
        aggregate_form,
    ),
];
