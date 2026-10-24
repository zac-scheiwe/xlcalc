pub(crate) mod atp;
mod base;
mod bessel;
mod complex;
mod convert;
mod extra;
pub(crate) mod xlmath;

use super::{args, scalar, FnDef, MANY};
use base::Radix;
use bessel::Kind;
use complex as cx;
use extra::SumKind;

pub const DEFS: &[FnDef] = &[
    args("BIN2DEC", 1, 1, "R", |c, o| {
        base::convert(c, o, Radix::Bin, Radix::Dec)
    }),
    args("BIN2HEX", 1, 2, "R", |c, o| {
        base::convert(c, o, Radix::Bin, Radix::Hex)
    }),
    args("BIN2OCT", 1, 2, "R", |c, o| {
        base::convert(c, o, Radix::Bin, Radix::Oct)
    }),
    args("DEC2BIN", 1, 2, "R", |c, o| {
        base::convert(c, o, Radix::Dec, Radix::Bin)
    }),
    args("DEC2HEX", 1, 2, "R", |c, o| {
        base::convert(c, o, Radix::Dec, Radix::Hex)
    }),
    args("DEC2OCT", 1, 2, "R", |c, o| {
        base::convert(c, o, Radix::Dec, Radix::Oct)
    }),
    args("HEX2BIN", 1, 2, "R", |c, o| {
        base::convert(c, o, Radix::Hex, Radix::Bin)
    }),
    args("HEX2DEC", 1, 1, "R", |c, o| {
        base::convert(c, o, Radix::Hex, Radix::Dec)
    }),
    args("HEX2OCT", 1, 2, "R", |c, o| {
        base::convert(c, o, Radix::Hex, Radix::Oct)
    }),
    args("OCT2BIN", 1, 2, "R", |c, o| {
        base::convert(c, o, Radix::Oct, Radix::Bin)
    }),
    args("OCT2DEC", 1, 1, "R", |c, o| {
        base::convert(c, o, Radix::Oct, Radix::Dec)
    }),
    args("OCT2HEX", 1, 2, "R", |c, o| {
        base::convert(c, o, Radix::Oct, Radix::Hex)
    }),
    scalar("BASE", 2, 3, base::base_fn),
    scalar("DECIMAL", 2, 2, base::decimal),
    scalar("ROMAN", 1, 2, base::roman),
    scalar("ARABIC", 1, 1, base::arabic),
    scalar("BITAND", 2, 2, |c, a| base::bitop(c, a, |x, y| x & y)),
    scalar("BITOR", 2, 2, |c, a| base::bitop(c, a, |x, y| x | y)),
    scalar("BITXOR", 2, 2, |c, a| base::bitop(c, a, |x, y| x ^ y)),
    scalar("BITLSHIFT", 2, 2, |c, a| base::bitshift(c, a, 1.0)),
    scalar("BITRSHIFT", 2, 2, |c, a| base::bitshift(c, a, -1.0)),
    args("DELTA", 1, 2, "R", |c, o| base::delta(c, o, false)),
    args("GESTEP", 1, 2, "R", |c, o| base::delta(c, o, true)),
    args("CONVERT", 3, 3, "R", convert::convert),
    args("BESSELJ", 2, 2, "R", |c, o| bessel::bessel(c, o, Kind::J)),
    args("BESSELY", 2, 2, "R", |c, o| bessel::bessel(c, o, Kind::Y)),
    args("BESSELI", 2, 2, "R", |c, o| bessel::bessel(c, o, Kind::I)),
    args("BESSELK", 2, 2, "R", |c, o| bessel::bessel(c, o, Kind::K)),
    args("COMPLEX", 2, 3, "R", cx::complex_fn),
    args("IMABS", 1, 1, "R", |c, o| {
        cx::to_number(c, o, |z| Ok(z.abs()))
    }),
    args("IMREAL", 1, 1, "R", |c, o| {
        cx::to_number(c, o, |z| Ok(z.re))
    }),
    args("IMAGINARY", 1, 1, "R", |c, o| {
        cx::to_number(c, o, |z| Ok(z.im))
    }),
    args("IMARGUMENT", 1, 1, "R", |c, o| {
        cx::to_number(c, o, cx::imargument)
    }),
    args("IMCONJUGATE", 1, 1, "R", |c, o| {
        cx::unary(c, o, |z| Ok(cx::C::new(z.re, -z.im)))
    }),
    args("IMSQRT", 1, 1, "R", |c, o| cx::unary(c, o, cx::imsqrt)),
    args("IMEXP", 1, 1, "R", |c, o| cx::unary(c, o, cx::imexp)),
    args("IMLN", 1, 1, "R", |c, o| cx::unary(c, o, cx::imln)),
    args("IMLOG10", 1, 1, "R", |c, o| cx::unary(c, o, cx::imlog10)),
    args("IMLOG2", 1, 1, "R", |c, o| cx::unary(c, o, cx::imlog2)),
    args("IMSIN", 1, 1, "R", |c, o| cx::unary(c, o, cx::imsin)),
    args("IMCOS", 1, 1, "R", |c, o| cx::unary(c, o, cx::imcos)),
    args("IMTAN", 1, 1, "R", |c, o| cx::unary(c, o, cx::imtan)),
    args("IMCOT", 1, 1, "R", |c, o| cx::unary(c, o, cx::imcot)),
    args("IMSEC", 1, 1, "R", |c, o| cx::unary(c, o, cx::imsec)),
    args("IMCSC", 1, 1, "R", |c, o| cx::unary(c, o, cx::imcsc)),
    args("IMSINH", 1, 1, "R", |c, o| cx::unary(c, o, cx::imsinh)),
    args("IMCOSH", 1, 1, "R", |c, o| cx::unary(c, o, cx::imcosh)),
    args("IMSECH", 1, 1, "R", |c, o| cx::unary(c, o, cx::imsech)),
    args("IMCSCH", 1, 1, "R", |c, o| cx::unary(c, o, cx::imcsch)),
    args("IMPOWER", 2, 2, "R", cx::impower),
    args("IMSUB", 2, 2, "R", cx::imsub),
    args("IMDIV", 2, 2, "R", cx::imdiv),
    args("IMSUM", 1, MANY, "R", |c, o| cx::fold(c, o, false)),
    args("IMPRODUCT", 1, MANY, "R", |c, o| cx::fold(c, o, true)),
    args("MULTINOMIAL", 1, MANY, "R", extra::multinomial),
    args("SERIESSUM", 4, 4, "R", extra::seriessum),
    args("SUMX2MY2", 2, 2, "RR", |c, o| {
        extra::sumx(c, o, SumKind::X2MY2)
    }),
    args("SUMX2PY2", 2, 2, "RR", |c, o| {
        extra::sumx(c, o, SumKind::X2PY2)
    }),
    args("SUMXMY2", 2, 2, "RR", |c, o| {
        extra::sumx(c, o, SumKind::XMY2)
    }),
    scalar("COMBINA", 2, 2, extra::combina),
    scalar("PERMUTATIONA", 2, 2, extra::permutationa),
    scalar("COT", 1, 1, extra::cot),
    scalar("COTH", 1, 1, extra::coth),
    scalar("CSC", 1, 1, extra::csc),
    scalar("CSCH", 1, 1, extra::csch),
    scalar("SEC", 1, 1, extra::sec),
    scalar("SECH", 1, 1, extra::sech),
    scalar("ACOT", 1, 1, extra::acot),
    scalar("ACOTH", 1, 1, extra::acoth),
];
