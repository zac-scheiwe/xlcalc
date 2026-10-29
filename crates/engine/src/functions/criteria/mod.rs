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
    "QVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQV",
    "QVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQV",
    "QVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQV",
    "QVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQV",
);

const VALUE_PAIRS: &str = concat!(
    "Q",
    "QVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQV",
    "QVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQV",
    "QVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQV",
    "QVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQVQV",
);

fn even_args(a: &[Expr]) -> Result<(), &'static str> {
    if a.len().is_multiple_of(2) {
        Ok(())
    } else {
        Err("needs range/criteria pairs")
    }
}

fn odd_args(a: &[Expr]) -> Result<(), &'static str> {
    if a.len() % 2 == 1 {
        Ok(())
    } else {
        Err("needs range/criteria pairs")
    }
}

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
    args("COUNTIF", 2, 2, "QV", ifs::countif),
    args("SUMIF", 2, 3, "QVQ", |c, o| ifs::sumif(c, o, Agg::Sum)),
    args("AVERAGEIF", 2, 3, "QVQ", |c, o| {
        ifs::sumif(c, o, Agg::Average)
    }),
    checked(
        args("COUNTIFS", 2, 254, PAIRS, |c, o| ifs::ifs(c, o, Agg::Count)),
        even_args,
    ),
    checked(
        args("SUMIFS", 3, 255, VALUE_PAIRS, |c, o| {
            ifs::ifs(c, o, Agg::Sum)
        }),
        odd_args,
    ),
    checked(
        args("AVERAGEIFS", 3, 255, VALUE_PAIRS, |c, o| {
            ifs::ifs(c, o, Agg::Average)
        }),
        odd_args,
    ),
    checked(
        args("MAXIFS", 3, 255, VALUE_PAIRS, |c, o| {
            ifs::ifs(c, o, Agg::Max)
        }),
        odd_args,
    ),
    checked(
        args("MINIFS", 3, 255, VALUE_PAIRS, |c, o| {
            ifs::ifs(c, o, Agg::Min)
        }),
        odd_args,
    ),
    args("DSUM", 3, 3, "QRR", |c, o| dfunc(c, o, DKind::Sum)),
    args("DAVERAGE", 3, 3, "QRR", |c, o| dfunc(c, o, DKind::Average)),
    args("DCOUNT", 3, 3, "QRR", |c, o| dfunc(c, o, DKind::Count)),
    args("DCOUNTA", 3, 3, "QRR", |c, o| dfunc(c, o, DKind::CountA)),
    args("DGET", 3, 3, "QRR", |c, o| dfunc(c, o, DKind::Get)),
    args("DMAX", 3, 3, "QRR", |c, o| dfunc(c, o, DKind::Max)),
    args("DMIN", 3, 3, "QRR", |c, o| dfunc(c, o, DKind::Min)),
    args("DPRODUCT", 3, 3, "QRR", |c, o| dfunc(c, o, DKind::Product)),
    args("DSTDEV", 3, 3, "QRR", |c, o| dfunc(c, o, DKind::StDev)),
    args("DSTDEVP", 3, 3, "QRR", |c, o| dfunc(c, o, DKind::StDevP)),
    args("DVAR", 3, 3, "QRR", |c, o| dfunc(c, o, DKind::Var)),
    args("DVARP", 3, 3, "QRR", |c, o| dfunc(c, o, DKind::VarP)),
    args("SUBTOTAL", 2, 255, "VQ", subtotal::subtotal),
    checked(
        args("AGGREGATE", 3, 255, "RRR", subtotal::aggregate),
        aggregate_form,
    ),
];
