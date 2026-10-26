pub(crate) mod dd;
mod desc;
mod dist;
mod ets;
mod ets_seas;
pub(crate) use dist::{xl_expm1, xl_log1p};
mod hp;
mod htest;
mod ibeta;
mod invnorm;
pub(crate) mod regress;
pub mod special;
#[cfg(test)]
mod tests;

use super::args::*;
use super::{args, scalar, scalar_p, FnDef, MANY, MANY254};
use crate::eval::{EvalCtx, Operand};
use crate::value::{Value, XlError};

pub const DEFS: &[FnDef] = &[
    args("CORREL", 2, 2, "RR", desc::correl),
    args("PEARSON", 2, 2, "RR", desc::correl),
    args("RSQ", 2, 2, "RR", desc::rsq),
    args("SLOPE", 2, 2, "RR", desc::slope),
    args("INTERCEPT", 2, 2, "RR", desc::intercept),
    args("STEYX", 2, 2, "RR", desc::steyx),
    args("FORECAST", 3, 3, "VRR", desc::forecast),
    args("FORECAST.LINEAR", 3, 3, "VRR", desc::forecast),
    args("FORECAST.ETS", 3, 6, "VRRV", |c, o| {
        ets::ets(c, o, ets::Kind::Value)
    }),
    args("FORECAST.ETS.CONFINT", 3, 7, "VRRV", |c, o| {
        ets::ets(c, o, ets::Kind::Confint)
    }),
    args("FORECAST.ETS.SEASONALITY", 2, 4, "RRV", |c, o| {
        ets::ets(c, o, ets::Kind::Seasonality)
    }),
    args("FORECAST.ETS.STAT", 3, 6, "RRV", |c, o| {
        ets::ets(c, o, ets::Kind::Stat)
    }),
    args("COVAR", 2, 2, "RR", |c, o| desc::covar(c, o, false)),
    args("COVARIANCE.P", 2, 2, "RR", |c, o| desc::covar(c, o, false)),
    args("COVARIANCE.S", 2, 2, "RR", |c, o| desc::covar(c, o, true)),
    args("PERCENTILE", 2, 2, "XV", |c, o| {
        desc::percentile(c, o, false)
    }),
    args("PERCENTILE.INC", 2, 2, "XV", |c, o| {
        desc::percentile(c, o, false)
    }),
    args("PERCENTILE.EXC", 2, 2, "XV", |c, o| {
        desc::percentile(c, o, true)
    }),
    args("QUARTILE", 2, 2, "XV", |c, o| desc::quartile(c, o, false)),
    args("QUARTILE.INC", 2, 2, "XV", |c, o| {
        desc::quartile(c, o, false)
    }),
    args("QUARTILE.EXC", 2, 2, "XV", |c, o| {
        desc::quartile(c, o, true)
    }),
    args("PERCENTRANK", 2, 3, "XVB", |c, o| {
        desc::percentrank(c, o, false)
    }),
    args("PERCENTRANK.INC", 2, 3, "XVB", |c, o| {
        desc::percentrank(c, o, false)
    }),
    args("PERCENTRANK.EXC", 2, 3, "XVB", |c, o| {
        desc::percentrank(c, o, true)
    }),
    args("RANK", 2, 3, "VQV", |c, o| desc::rank(c, o, false)),
    args("RANK.EQ", 2, 3, "VQV", |c, o| desc::rank(c, o, false)),
    args("RANK.AVG", 2, 3, "VQV", |c, o| desc::rank(c, o, true)),
    args("MODE", 1, MANY, "R", |c, o| desc::mode(c, o, false)),
    args("MODE.SNGL", 1, MANY254, "R", |c, o| desc::mode(c, o, false)),
    args("MODE.MULT", 1, MANY254, "R", |c, o| desc::mode(c, o, true)),
    args("TRIMMEAN", 2, 2, "XV", desc::trimmean),
    args("FREQUENCY", 2, 2, "MM", desc::frequency),
    args("KURT", 1, MANY, "R", desc::kurt),
    args("SKEW", 1, MANY, "R", |c, o| desc::skew(c, o, false)),
    args("SKEW.P", 1, MANY254, "R", |c, o| desc::skew(c, o, true)),
    scalar("STANDARDIZE", 3, 3, desc::standardize),
    scalar("FISHER", 1, 1, desc::fisher),
    scalar("FISHERINV", 1, 1, desc::fisherinv),
    args("PROB", 3, 4, "RRVV", desc::prob),
    args("LINEST", 1, 4, "RRRR", |c, o| {
        regress::linest_lifted(c, o, false)
    }),
    args("LOGEST", 1, 4, "RRRR", |c, o| {
        regress::linest_lifted(c, o, true)
    }),
    args("TREND", 1, 4, "RRRR", |c, o| {
        regress::trend_lifted(c, o, false)
    }),
    args("GROWTH", 1, 4, "RRRR", |c, o| {
        regress::trend_lifted(c, o, true)
    }),
    args("ERF", 1, 2, "RR", dist::erf_fn),
    args("ERF.PRECISE", 1, 1, "R", dist::erf_precise),
    args("ERFC", 1, 1, "R", dist::erfc_fn),
    args("ERFC.PRECISE", 1, 1, "R", dist::erfc_fn),
    scalar("GAMMA", 1, 1, dist::gamma_fn),
    scalar("GAMMALN", 1, 1, dist::gammaln),
    scalar("GAMMALN.PRECISE", 1, 1, dist::gammaln),
    scalar("PHI", 1, 1, dist::phi),
    scalar("GAUSS", 1, 1, dist::gauss),
    scalar("NORM.DIST", 4, 4, dist::norm_dist),
    scalar("NORMDIST", 4, 4, dist::norm_dist),
    scalar("NORM.INV", 3, 3, dist::norm_inv),
    scalar("NORMINV", 3, 3, dist::norm_inv),
    scalar("NORM.S.DIST", 2, 2, dist::norm_s_dist),
    scalar("NORMSDIST", 1, 1, dist::normsdist),
    scalar("NORM.S.INV", 1, 1, dist::norm_s_inv),
    scalar("NORMSINV", 1, 1, dist::norm_s_inv),
    scalar("LOGNORM.DIST", 4, 4, dist::lognorm_dist),
    scalar("LOGNORMDIST", 3, 3, dist::lognormdist),
    scalar("LOGNORM.INV", 3, 3, dist::lognorm_inv),
    scalar("LOGINV", 3, 3, dist::lognorm_inv),
    scalar("T.DIST", 3, 3, dist::t_dist),
    scalar("T.DIST.2T", 2, 2, dist::t_dist_2t),
    scalar("T.DIST.RT", 2, 2, dist::t_dist_rt),
    scalar("TDIST", 3, 3, dist::tdist),
    scalar("T.INV", 2, 2, dist::t_inv),
    scalar("T.INV.2T", 2, 2, dist::t_inv_2t),
    scalar("TINV", 2, 2, dist::t_inv_2t),
    scalar("CHISQ.DIST", 3, 3, dist::chisq_dist),
    scalar("CHISQ.DIST.RT", 2, 2, dist::chisq_dist_rt),
    scalar("CHIDIST", 2, 2, dist::chisq_dist_rt),
    scalar("CHISQ.INV", 2, 2, dist::chisq_inv),
    scalar("CHISQ.INV.RT", 2, 2, dist::chisq_inv_rt),
    scalar("CHIINV", 2, 2, dist::chisq_inv_rt),
    scalar("F.DIST", 4, 4, dist::f_dist),
    scalar("F.DIST.RT", 3, 3, dist::f_dist_rt),
    scalar("FDIST", 3, 3, dist::f_dist_rt),
    scalar("F.INV", 3, 3, dist::f_inv),
    scalar("F.INV.RT", 3, 3, dist::f_inv_rt),
    scalar("FINV", 3, 3, dist::f_inv_rt),
    scalar_p("BETA.DIST", 4, 6, "VVVVBB", dist::beta_dist),
    scalar_p("BETADIST", 3, 5, "VVVBB", dist::betadist),
    scalar_p("BETA.INV", 3, 5, "VVVBB", dist::beta_inv),
    scalar_p("BETAINV", 3, 5, "VVVBB", dist::beta_inv),
    scalar("GAMMA.DIST", 4, 4, dist::gamma_dist),
    scalar("GAMMADIST", 4, 4, dist::gamma_dist),
    scalar("GAMMA.INV", 3, 3, dist::gamma_inv),
    scalar("GAMMAINV", 3, 3, dist::gamma_inv),
    scalar("EXPON.DIST", 3, 3, dist::expon_dist),
    scalar("EXPONDIST", 3, 3, dist::expon_dist),
    scalar("WEIBULL.DIST", 4, 4, dist::weibull_dist),
    scalar("WEIBULL", 4, 4, dist::weibull_dist),
    scalar("BINOM.DIST", 4, 4, dist::binom_dist),
    scalar("BINOMDIST", 4, 4, dist::binom_dist),
    scalar_p("BINOM.DIST.RANGE", 3, 4, "VVVB", dist::binom_dist_range),
    scalar("BINOM.INV", 3, 3, dist::binom_inv),
    scalar("CRITBINOM", 3, 3, dist::binom_inv),
    scalar("NEGBINOM.DIST", 4, 4, dist::negbinom_dist),
    scalar("NEGBINOMDIST", 3, 3, dist::negbinomdist),
    scalar("HYPGEOM.DIST", 5, 5, dist::hypgeom_dist),
    scalar("HYPGEOMDIST", 4, 4, dist::hypgeomdist),
    scalar("POISSON.DIST", 3, 3, dist::poisson_dist),
    scalar("POISSON", 3, 3, dist::poisson_dist),
    scalar("CONFIDENCE", 3, 3, htest::confidence_norm),
    scalar("CONFIDENCE.NORM", 3, 3, htest::confidence_norm),
    scalar("CONFIDENCE.T", 3, 3, htest::confidence_t),
    args("Z.TEST", 2, 3, "EVV", htest::z_test),
    args("ZTEST", 2, 3, "EVV", htest::z_test),
    args("T.TEST", 4, 4, "EEVV", htest::t_test),
    args("TTEST", 4, 4, "EEVV", htest::t_test),
    args("F.TEST", 2, 2, "RR", htest::f_test),
    args("FTEST", 2, 2, "RR", htest::f_test),
    args("CHISQ.TEST", 2, 2, "RR", htest::chisq_test),
    args("CHITEST", 2, 2, "RR", htest::chisq_test),
];

fn fin(r: R<f64>) -> Value {
    match r {
        Ok(x) => Value::num(x),
        Err(e) => Value::Error(e),
    }
}

fn opfin(r: R<f64>) -> Operand {
    Operand::Val(fin(r))
}

fn opnum(ctx: &EvalCtx, o: &Operand) -> R<f64> {
    match o {
        Operand::Val(v) => ctx.to_number(v),
        Operand::Missing => Ok(0.0),
        Operand::Ref(_) => Err(XlError::Value),
    }
}

fn flat_values(ctx: &mut EvalCtx, op: &Operand) -> R<Vec<Value>> {
    match op {
        Operand::Val(Value::Error(e)) => Err(*e),
        Operand::Missing => Ok(vec![Value::Blank]),
        _ => {
            let g = Grid::from_operand_3d(op)?;
            let a = g.to_array(ctx);
            Ok(a.data.clone())
        }
    }
}

fn grid_values(ctx: &mut EvalCtx, op: &Operand) -> R<(u32, u32, Vec<Value>)> {
    match op {
        Operand::Val(Value::Error(e)) => Err(*e),
        Operand::Missing => Ok((1, 1, vec![Value::Blank])),
        _ => {
            let g = Grid::from_operand_3d(op)?;
            let a = g.to_array(ctx);
            Ok((a.rows, a.cols, a.data.clone()))
        }
    }
}

fn numbers_only(ctx: &mut EvalCtx, op: &Operand) -> R<Vec<f64>> {
    let mut out = Vec::new();
    for v in flat_values(ctx, op)? {
        match v {
            Value::Number(n) => out.push(n),
            Value::Error(e) => return Err(e),
            _ => {}
        }
    }
    Ok(out)
}

fn sum(v: &[f64]) -> f64 {
    let mut s = 0.0;
    for &x in v {
        s += x;
    }
    s
}

fn mean(v: &[f64]) -> f64 {
    sum(v) / v.len() as f64
}
