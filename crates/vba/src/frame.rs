use crate::ast::ProcKind;
use crate::ir::{ArrayDecl, CArg, CExpr, LocalInfo, NamedArg, Op, ParamInfo, Place, SType, EK};
use crate::types::{BinOp, VType};

pub(crate) const STACK_UNITS: u32 = 15360;

pub(crate) const CALL_MARGIN: u32 = 5;

pub(crate) const MAX_FRAMES: usize = 2520;

const FIXED_STRING_INLINE: u32 = 32;

fn local_units(ty: &VType, array: &ArrayDecl, fixed: Option<u32>) -> u32 {
    match array {
        ArrayDecl::Dynamic => 1,
        ArrayDecl::Fixed(b) => 4 + b.len() as u32,
        ArrayDecl::Scalar => match (ty, fixed) {
            (VType::Variant, _) => 3,
            (VType::String, Some(n)) if n <= FIXED_STRING_INLINE => (n * 2).div_ceil(8).max(1),
            _ => 1,
        },
    }
}

pub(crate) fn frame_units(
    kind: ProcKind,
    params: &[ParamInfo],
    ret: &SType,
    locals: &[LocalInfo],
    ops: &[Op],
) -> u32 {
    let mut n = 1;
    if kind.returns_value() {
        n += if ret.array || ret.ty != VType::Variant {
            1
        } else {
            9
        };
    }
    for p in params {
        n += if p.by_val && !p.array && p.ty == VType::Variant {
            3
        } else {
            1
        };
        n += u32::from(p.optional);
    }
    for l in locals.iter().skip(1 + params.len()) {
        n += local_units(&l.ty, &l.array, l.fixed);
    }
    n + ops.iter().map(op_temps).max().unwrap_or(0)
}

fn op_temps(op: &Op) -> u32 {
    match op {
        Op::Let(p, e) | Op::Set(p, e) => place_temps(p, true).max(temps(e)),
        Op::Bind(_, e) | Op::Eval(e) | Op::JumpIfNot(e, _) | Op::JumpIf(e, _) => temps(e),
        Op::ErrRaise(_) => 25,
        Op::ErrClear => 1,
        _ => 0,
    }
}

fn place_temps(p: &Place, store: bool) -> u32 {
    match p {
        Place::Member(b, name, args) => {
            let own = if store && name.eq_ignore_ascii_case("value") {
                13
            } else {
                0
            };
            own.max(temps(b)).max(args_temps(args))
        }
        Place::Elem(_, ix) => ix.iter().map(temps).max().unwrap_or(0),
        _ => 0,
    }
}

fn args_temps(args: &[NamedArg]) -> u32 {
    args.iter().map(|a| temps(&a.value)).max().unwrap_or(0)
}

fn flatten_cat<'a>(e: &'a CExpr, out: &mut Vec<&'a CExpr>) {
    match &e.kind {
        EK::Binary(BinOp::Cat, l, r) => {
            flatten_cat(l, out);
            flatten_cat(r, out);
        }
        _ => out.push(e),
    }
}

fn temps(e: &CExpr) -> u32 {
    match &e.kind {
        EK::Err(crate::ir::ErrProp::Number | crate::ir::ErrProp::Description) => 2,
        EK::Binary(BinOp::Cat, ..) => {
            let mut ops = Vec::new();
            flatten_cat(e, &mut ops);
            let inner = ops.iter().map(|o| temps(o)).max().unwrap_or(0);
            let base = (ops.len() as u32).saturating_sub(2);
            base + inner + u32::from(inner > 0)
        }
        EK::Binary(_, l, r) => temps(l).max(temps(r)),
        EK::Unary(_, x) => temps(x),
        EK::Builtin(b, args) => {
            let own = match b.0.name.to_ascii_lowercase().as_str() {
                "ucase" => 6,
                "mid" | "format" => 12,
                "chr" | "space" => 3,
                "string" => 9,
                _ => 0,
            };
            own.max(args.iter().map(temps).max().unwrap_or(0))
        }
        EK::Intrinsic(i, args) => {
            let own = match i {
                crate::ir::Intrinsic::Ask(crate::host::Interaction::MsgBox) => 21,
                crate::ir::Intrinsic::CreateObject => 3,
                _ => 0,
            };
            own.max(args.iter().map(temps).max().unwrap_or(0))
        }
        EK::Call(_, args) => args
            .iter()
            .map(|a| match a {
                CArg::Value(e) => temps(e),
                CArg::Rest(v) => v.iter().map(temps).max().unwrap_or(0),
                _ => 0,
            })
            .max()
            .unwrap_or(0),
        EK::Member(b, name, args) => {
            let own = if name.eq_ignore_ascii_case("run") {
                97
            } else if name.eq_ignore_ascii_case("value") {
                10
            } else {
                0
            };
            own.max(temps(b)).max(args_temps(args))
        }
        EK::Index(b, args) => temps(b).max(args_temps(args)),
        EK::Global(_, args) => args_temps(args),
        EK::Elem(_, ix) => ix.iter().map(temps).max().unwrap_or(0),
        _ => 0,
    }
}
