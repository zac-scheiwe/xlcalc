use std::sync::Arc;

use super::{EvalCtx, Operand, MAX_CALL_DEPTH};
use crate::formula::{LambdaDef, Node};
use crate::value::{Value, XlError};

#[derive(Debug)]
pub struct LambdaValue {
    pub def: Arc<LambdaDef>,
    pub captured: Vec<Operand>,
}

impl LambdaValue {
    pub fn arity(&self) -> usize {
        self.def.params.len()
    }
}

pub fn invoke(ctx: &mut EvalCtx, lam: &LambdaValue, args: Vec<Operand>) -> Operand {
    if args.len() != lam.def.params.len() {
        return Operand::err(XlError::Value);
    }
    if ctx.depth >= MAX_CALL_DEPTH {
        return Operand::err(XlError::Num);
    }
    let mut env = Vec::with_capacity(lam.captured.len() + lam.def.params.len());
    env.extend(lam.captured.iter().cloned());
    env.truncate(lam.def.base as usize);
    let n = args.len();
    env.extend(args);
    for _ in n..lam.def.params.len() {
        env.push(Operand::Missing);
    }
    let saved = std::mem::replace(&mut ctx.env, env);
    ctx.depth += 1;
    let out = ctx.eval(&lam.def.body);
    ctx.depth -= 1;
    ctx.env = saved;
    out
}

fn eval_args(ctx: &mut EvalCtx, args: &[Node]) -> Vec<Operand> {
    args.iter().map(|a| ctx.eval(a)).collect()
}

pub fn call_value(ctx: &mut EvalCtx, f: Value, args: &[Node]) -> Operand {
    match f {
        Value::Lambda(lam) => {
            let args = eval_args(ctx, args);
            invoke(ctx, &lam, args)
        }
        Value::Error(e) => Operand::err(e),

        Value::Array(a) if a.data.len() > 1 => Operand::Val(
            crate::value::Array::new(
                a.rows,
                a.cols,
                vec![Value::Error(XlError::Value); a.data.len()],
            )
            .into_value(),
        ),
        _ => Operand::err(XlError::Value),
    }
}

pub fn call_named(ctx: &mut EvalCtx, key: &str, args: &[Node]) -> Operand {
    if ctx.wb.names.lookup(key, ctx.host.sheet).is_none() {
        let _ = eval_args(ctx, args);
        return Operand::err(XlError::Name);
    }
    let f = super::refs::eval_name(ctx, key, None);

    if matches!(f, Operand::Ref(_)) {
        return Operand::err(XlError::Ref);
    }
    let f = ctx.deref(f);
    call_value(ctx, f, args)
}
