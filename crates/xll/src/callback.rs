use xlcalc::{CellAddr, SheetKind, Value, Workbook, XlError};

use crate::args;
use crate::host::{self, Frame, FRAMES};
use crate::xlfn;
use crate::xloper::{self, Alloc, Val, Xlmref12, Xloper12, Xlref12, ERR_NA, ERR_VALUE, REF, SREF};

pub const XLF_CALLER: i32 = 89;
pub const XLF_REGISTER: i32 = 149;

const XLF_RTD: i32 = 379;
pub const XLF_EVALUATE: i32 = 257;
pub const XL_FREE: i32 = 0x4000;
pub const XL_COERCE: i32 = 0x4002;
pub const XL_SHEET_ID: i32 = 0x4004;
pub const XL_GET_NAME: i32 = 0x4009;
pub(crate) const XL_ASYNC_RETURN: i32 = 0x4010;
pub(crate) const XL_EVENT_REGISTER: i32 = 0x4011;

const RET_SUCCESS: i32 = 0;
const RET_INV_XLFN: i32 = 2;
const RET_INV_COUNT: i32 = 4;
const RET_INV_XLOPER: i32 = 8;
pub(crate) const RET_FAILED: i32 = 32;

#[no_mangle]
pub unsafe extern "system" fn MdCallBack12(
    xlfn: i32,
    count: i32,
    opers: *mut *mut Xloper12,
    result: *mut Xloper12,
) -> i32 {
    let args: Vec<*mut Xloper12> = if count > 0 && !opers.is_null() {
        std::slice::from_raw_parts(opers, count as usize).to_vec()
    } else {
        Vec::new()
    };

    let outcome = if xlfn == 0x4005 && !args.is_empty() {
        Ok((RET_INV_XLOPER, Reply::Error(ERR_VALUE)))
    } else {
        std::panic::catch_unwind(|| dispatch(xlfn, &args))
    };
    let (code, value) = outcome.unwrap_or((RET_FAILED, Reply::Error(ERR_VALUE)));
    if !result.is_null() {
        *result = reply_oper(value);
    }
    code
}

pub(crate) enum Reply {
    Oper(Xloper12, Option<Alloc>),
    Error(i32),
    Nothing,
}

fn reply_oper(r: Reply) -> Xloper12 {
    match r {
        Reply::Oper(x, alloc) => {
            if let Some(a) = alloc {
                keep_in_frame(a);
            }
            x
        }
        Reply::Error(code) => Xloper12::error(code),
        Reply::Nothing => Xloper12::nil(),
    }
}

fn keep_in_frame(a: Alloc) {
    FRAMES.with(|f| {
        if let Some(top) = f.borrow_mut().last_mut() {
            top.keep.push(a);
        }
    });
}

thread_local! {

    pub(crate) static OLD_CALL: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

pub(crate) fn keep_in_frame_old(a: crate::xloper_old::Alloc) {
    FRAMES.with(|f| {
        if let Some(top) = f.borrow_mut().last_mut() {
            top.keep_old.push(a);
        }
    });
}

struct Ctx {
    inner: std::sync::Arc<host::Inner>,
    wb: Option<*const Workbook>,
    caller: Option<CellAddr>,
    async_handle: Option<u64>,
    macro_sheet: bool,
}

fn context() -> Option<Ctx> {
    FRAMES.with(|f| {
        f.borrow().last().map(|x: &Frame| Ctx {
            inner: x.inner.clone(),
            wb: x.wb,
            caller: x.caller,
            async_handle: x.async_handle,
            macro_sheet: x.macro_sheet,
        })
    })
}

unsafe fn arg_value(ctx: &Ctx, p: *mut Xloper12) -> Option<Value> {
    if p.is_null() {
        return None;
    }
    let x = *p;
    match (ctx.wb, ctx.caller) {
        (Some(wb), Some(caller)) => Some(args::read(&*wb, caller, &x)),
        _ => xloper::to_value(&x),
    }
}

fn text_oper(s: &str) -> Reply {
    let mut alloc = Alloc::default();
    let x = alloc.convert(&Value::Text(s.into()));
    Reply::Oper(x, Some(alloc))
}

pub(crate) fn dispatch(xlfn: i32, a: &[*mut Xloper12]) -> (i32, Reply) {
    let Some(ctx) = context() else {
        if xlfn == XL_ASYNC_RETURN {
            return unsafe { async_return_foreign(a) };
        }
        return (RET_FAILED, Reply::Nothing);
    };
    unsafe {
        match xlfn {
            XL_FREE => (RET_SUCCESS, Reply::Nothing),
            XL_GET_NAME => (RET_SUCCESS, text_oper(&ctx.inner.path)),
            XLF_CALLER => {
                if !a.is_empty() {
                    return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
                }

                let block = match (ctx.wb, ctx.caller) {
                    (Some(wb), Some(c)) => (*wb).array_at(c).map(|(_, r)| r),
                    _ => None,
                };
                match ctx.caller {
                    Some(c) => (
                        RET_SUCCESS,
                        Reply::Oper(
                            Xloper12::with(
                                Val {
                                    sref: xloper::SRef {
                                        count: 1,
                                        rect: Xlref12 {
                                            row_first: block.map_or(c.row, |r| r.r0) as i32,
                                            row_last: block.map_or(c.row, |r| r.r1) as i32,
                                            col_first: block.map_or(c.col, |r| r.c0) as i32,
                                            col_last: block.map_or(c.col, |r| r.c1) as i32,
                                        },
                                    },
                                },
                                SREF,
                            ),
                            None,
                        ),
                    ),
                    None => (RET_FAILED, Reply::Error(ERR_VALUE)),
                }
            }

            XLF_REGISTER if ctx.caller.is_some() && ctx.macro_sheet => call_function(&ctx, xlfn, a),
            XLF_REGISTER if ctx.caller.is_some() => (RET_INV_XLFN, Reply::Error(ERR_VALUE)),
            XLF_REGISTER => register(&ctx, a),
            XLF_EVALUATE => evaluate(&ctx, a),
            XL_COERCE => coerce(&ctx, a),
            XL_SHEET_ID => sheet_id(&ctx, a),
            XL_ASYNC_RETURN => async_return(&ctx, a),
            XL_EVENT_REGISTER => event_register(&ctx, a),
            0x4001 => (RET_SUCCESS, int_reply(STACK_HEADROOM)),

            0x4003 => (RET_SUCCESS, bool_reply(false)),
            0x4005 => sheet_name(&ctx, a),
            0x4006 => abort(a),

            0x4007 => (RET_FAILED, Reply::Error(ERR_VALUE)),

            0x4008 => (RET_SUCCESS, int_reply(0)),
            0x400a | 0x400b => (RET_SUCCESS, Reply::Nothing),
            0x400c => define_binary_name(&ctx, a),
            0x400d => get_binary_name(&ctx, a),

            0x4012 => (RET_SUCCESS, int_reply(0)),

            0x4013 => (
                RET_SUCCESS,
                Reply::Oper(Xloper12::with(Val { int: 0 }, xloper::BIGDATA), None),
            ),
            0x4014 if a.len() != 1 => (RET_INV_COUNT, Reply::Error(ERR_VALUE)),
            0x4014 => (RET_FAILED, Reply::Error(ERR_VALUE)),
            n if n >= 0 && n & 0x4000 == 0 => call_function(&ctx, xlfn, a),
            _ => (RET_INV_XLFN, Reply::Error(ERR_VALUE)),
        }
    }
}

unsafe fn call_function(ctx: &Ctx, xlfn: i32, a: &[*mut Xloper12]) -> (i32, Reply) {
    let (Some(wb), Some(caller)) = (ctx.wb, ctx.caller) else {
        return (RET_FAILED, Reply::Error(ERR_VALUE));
    };
    let mut values = Vec::with_capacity(a.len());
    let mut refs = Vec::with_capacity(a.len());
    for &p in a {
        if p.is_null() {
            return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
        }
        values.push(if (*p).kind() == xloper::MISSING {
            None
        } else {
            arg_value(ctx, p)
        });

        refs.push(if OLD_CALL.with(std::cell::Cell::get) {
            reference_text(&*wb, caller, &*p)
        } else {
            None
        });
    }
    if xlfn == XLF_RTD {
        FRAMES.with(|f| {
            if let Some(top) = f.borrow_mut().last_mut() {
                top.used_rtd = true;
            }
        });
    }
    match xlfn::call(&*wb, caller, xlfn, &values, &refs, ctx.macro_sheet) {
        xlfn::Outcome::Value(v) => {
            let mut alloc = Alloc::default();
            let x = alloc.convert(&v);
            (RET_SUCCESS, Reply::Oper(x, Some(alloc)))
        }
        xlfn::Outcome::Invalid => (RET_INV_XLFN, Reply::Error(ERR_VALUE)),
        xlfn::Outcome::BadOperand => (RET_INV_XLOPER, Reply::Error(ERR_VALUE)),
        xlfn::Outcome::BadCount => (RET_INV_COUNT, Reply::Error(ERR_VALUE)),
        xlfn::Outcome::Failed(code) => (RET_FAILED, Reply::Error(code)),
        xlfn::Outcome::Flow => (
            RET_SUCCESS,
            Reply::Oper(Xloper12::with(crate::xloper::Val { int: 0 }, 0x20), None),
        ),
    }
}

unsafe fn reference_text(wb: &Workbook, caller: CellAddr, x: &Xloper12) -> Option<String> {
    let (sheet, rects): (xlcalc::SheetId, Vec<Xlref12>) = match x.kind() {
        SREF => (caller.sheet, vec![x.val.sref.rect]),
        REF => {
            let m = x.val.mref;
            if m.refs.is_null() {
                return None;
            }
            let n = usize::from((*m.refs).count);
            let table = std::ptr::addr_of!((*m.refs).table).cast::<Xlref12>();
            (
                xlcalc::SheetId(m.sheet.checked_sub(1)? as u32),
                (0..n).map(|i| *table.add(i)).collect(),
            )
        }
        _ => return None,
    };
    let name = wb.sheets().find(|s| s.id == sheet)?.name.to_string();
    let col = |mut c: u32| {
        let mut s = String::new();
        loop {
            s.insert(0, char::from(b'A' + (c % 26) as u8));
            if c < 26 {
                return s;
            }
            c = c / 26 - 1;
        }
    };
    let parts: Vec<String> = rects
        .iter()
        .map(|r| {
            let first = format!("${}${}", col(r.col_first as u32), r.row_first + 1);
            if r.row_first == r.row_last && r.col_first == r.col_last {
                first
            } else {
                format!("{first}:${}${}", col(r.col_last as u32), r.row_last + 1)
            }
        })
        .collect();
    let q = name.replace('\'', "''");
    let text: Vec<String> = parts.iter().map(|p| format!("'{q}'!{p}")).collect();
    Some(if text.len() == 1 {
        text[0].clone()
    } else {
        format!("({})", text.join(","))
    })
}

unsafe fn text_arg(ctx: &Ctx, p: *mut Xloper12) -> Option<String> {
    match arg_value(ctx, p)? {
        Value::Text(t) => Some(t.to_string()),
        _ => None,
    }
}

unsafe fn register(ctx: &Ctx, a: &[*mut Xloper12]) -> (i32, Reply) {
    if a.len() < 4 {
        return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
    }
    let procedure = arg_value(ctx, a[1]).unwrap_or(Value::Blank);
    let (Some(types), Some(name)) = (text_arg(ctx, a[2]), text_arg(ctx, a[3])) else {
        return (RET_SUCCESS, Reply::Error(ERR_VALUE));
    };
    match host::register(&ctx.inner, &procedure, &types, &name) {
        Some(id) => (
            RET_SUCCESS,
            Reply::Oper(Xloper12::number(f64::from(id)), None),
        ),
        None => (RET_SUCCESS, Reply::Error(ERR_VALUE)),
    }
}

unsafe fn evaluate(ctx: &Ctx, a: &[*mut Xloper12]) -> (i32, Reply) {
    if a.len() != 1 {
        return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
    }
    let (Some(wb), Some(caller)) = (ctx.wb, ctx.caller) else {
        return (RET_FAILED, Reply::Error(ERR_VALUE));
    };
    let Some(arg) = arg_value(ctx, a[0]) else {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
    };
    if !ctx.macro_sheet {
        if let Value::Text(text) = &arg {
            return evaluate_on_sheet(&*wb, caller, text);
        }
    }
    let one = |v: &Value| match v {
        Value::Text(text) if ctx.macro_sheet && has_reference(text) => Value::Error(XlError::REF),
        Value::Text(text) => (*wb).cfv_eval(text, caller, caller),

        Value::Blank => Value::Error(XlError::VALUE),
        other => other.clone(),
    };
    let v = match &arg {
        Value::Array(arr) => {
            let data = arr.data.iter().map(one).collect();
            xlcalc::Array::new(arr.rows, arr.cols, data).into_value()
        }
        other => one(other),
    };
    let mut alloc = Alloc::default();
    let x = alloc.convert(&v);
    (RET_SUCCESS, Reply::Oper(x, Some(alloc)))
}

fn evaluate_on_sheet(wb: &Workbook, caller: CellAddr, text: &str) -> (i32, Reply) {
    let mut open = 0i32;
    let mut in_str = false;
    for c in text.chars() {
        match c {
            '"' => in_str = !in_str,
            '(' if !in_str => open += 1,
            ')' if !in_str => open -= 1,
            _ => {}
        }
    }
    let mut full = text.to_string();
    for _ in 0..open.max(0) {
        full.push(')');
    }
    let mut alloc = Alloc::default();
    let x = match wb.evaluate_text(&full, caller) {
        xlcalc::Evaluated::Ref(areas) if areas.first().is_some_and(|a| a.sheet == caller.sheet) => {
            let r = areas[0].rect;
            Xloper12::with(
                Val {
                    sref: xloper::SRef {
                        count: 1,
                        rect: Xlref12 {
                            row_first: r.r0 as i32,
                            row_last: r.r1 as i32,
                            col_first: r.c0 as i32,
                            col_last: r.c1 as i32,
                        },
                    },
                },
                SREF,
            )
        }
        xlcalc::Evaluated::Ref(areas) => {
            let v = areas.first().map_or(Value::Error(XlError::REF), |a| {
                wb.value(CellAddr {
                    sheet: a.sheet,
                    row: a.rect.r0,
                    col: a.rect.c0,
                })
            });
            alloc.convert(&v)
        }
        xlcalc::Evaluated::Value(v) => alloc.convert(&v),
    };
    (RET_SUCCESS, Reply::Oper(x, Some(alloc)))
}

fn has_reference(text: &str) -> bool {
    let b = text.as_bytes();
    let mut i = 0;
    let mut in_str = false;
    while i < b.len() {
        let c = b[i];
        if c == b'"' {
            in_str = !in_str;
            i += 1;
            continue;
        }
        if !in_str && (c.is_ascii_alphabetic() || c == b'_' || c == b'$') {
            let start = i;
            while i < b.len()
                && (b[i].is_ascii_alphanumeric() || b[i] == b'_' || b[i] == b'.' || b[i] == b'$')
            {
                i += 1;
            }
            let word = &text[start..i];
            let call = b.get(i) == Some(&b'(');
            let literal = word.eq_ignore_ascii_case("true") || word.eq_ignore_ascii_case("false");
            let exponent = start > 0
                && (b[start - 1].is_ascii_digit() || b[start - 1] == b'.')
                && word.starts_with(['E', 'e']);
            if !call && !literal && !exponent && !word.starts_with('#') && cell_like(word) {
                return true;
            }
            continue;
        }
        i += 1;
    }
    false
}

fn cell_like(word: &str) -> bool {
    let w = word.replace('$', "");
    let letters = w.bytes().take_while(u8::is_ascii_alphabetic).count();
    (1..=3).contains(&letters)
        && w.len() > letters
        && w.bytes().skip(letters).all(|c| c.is_ascii_digit())
}

fn type_bit(v: &Value) -> u32 {
    match v {
        Value::Number(_) => 0x1,
        Value::Text(_) => 0x2,
        Value::Bool(_) => 0x4,
        Value::Error(_) => 0x10,
        Value::Array(_) => 0x40,
        Value::Blank => 0x100,
        _ => 0,
    }
}

fn sfix(x: f64) -> Option<i32> {
    let t = x + 2_147_483_648.0;
    if t.is_nan() {
        return None;
    }
    if t < 0.0 {
        return Some(i32::MIN);
    }
    Some((t as u32).wrapping_add(0x8000_0000) as i32)
}

unsafe fn parse_number(ctx: &Ctx, t: &str, at: CellAddr) -> Option<f64> {
    let wb = ctx.wb?;
    match (*wb).cfv_eval(&format!("=--\"{}\"", t.replace('"', "\"\"")), at, at) {
        Value::Number(n) => Some(n),
        _ => None,
    }
}

fn wrap_1x1(one: Value) -> Value {
    xlcalc::Array::new(1, 1, vec![one]).into_value()
}

unsafe fn coerce_to_mask(ctx: &Ctx, v: Value, mask: u32, at: CellAddr, int: &mut bool) -> Value {
    if mask == 0x40 {
        return match v {
            Value::Array(_) => v,
            one => wrap_1x1(one),
        };
    }

    let v = match v {
        Value::Array(a) if mask & 0x40 == 0 && a.rows > 0 && a.cols > 0 => a.get(0, 0).clone(),
        v => v,
    };
    if type_bit(&v) & mask != 0 {
        return v;
    }
    let failed = || Value::Error(XlError::VALUE);
    match v {
        Value::Error(_) => v,

        Value::Blank => {
            if mask & 1 != 0 {
                Value::Number(0.0)
            } else if mask & 2 != 0 {
                Value::Text("".into())
            } else if mask & 0x800 != 0 {
                *int = true;
                Value::Number(0.0)
            } else if mask & 4 != 0 {
                Value::Bool(false)
            } else {
                failed()
            }
        }

        Value::Number(n) => {
            if mask & 2 != 0 {
                Value::Text(xlcalc::format_general(n).into())
            } else if mask & 0x800 != 0 {
                match sfix(n) {
                    Some(i) => {
                        *int = true;
                        Value::Number(f64::from(i))
                    }
                    None => failed(),
                }
            } else if mask & 4 != 0 {
                Value::Bool(n != 0.0)
            } else if mask & 0x40 != 0 {
                wrap_1x1(v)
            } else {
                failed()
            }
        }

        Value::Bool(b) => {
            if mask & 1 != 0 {
                Value::Number(f64::from(u8::from(b)))
            } else if mask & 2 != 0 {
                Value::Text(if b { "TRUE" } else { "FALSE" }.into())
            } else if mask & 0x800 != 0 {
                *int = true;
                Value::Number(f64::from(u8::from(b)))
            } else if mask & 0x40 != 0 {
                wrap_1x1(v)
            } else {
                failed()
            }
        }

        Value::Text(ref t) => {
            if mask & 0x801 != 0 {
                match parse_number(ctx, t, at) {
                    Some(n) if mask & 1 != 0 => Value::Number(n),
                    Some(n) => match sfix(n) {
                        Some(i) => {
                            *int = true;
                            Value::Number(f64::from(i))
                        }
                        None => failed(),
                    },
                    None => failed(),
                }
            } else if mask & 4 != 0 && t.eq_ignore_ascii_case("TRUE") {
                Value::Bool(true)
            } else if mask & 4 != 0 && t.eq_ignore_ascii_case("FALSE") {
                Value::Bool(false)
            } else if mask & 0x40 != 0 {
                wrap_1x1(v)
            } else {
                failed()
            }
        }

        _ => failed(),
    }
}

unsafe fn int_operand(ctx: &Ctx, p: *mut Xloper12, at: CellAddr) -> Option<i32> {
    match arg_value(ctx, p)? {
        Value::Number(n) => sfix(n),
        Value::Bool(b) => Some(i32::from(b)),
        Value::Blank => Some(0),
        Value::Text(t) => sfix(parse_number(ctx, &t, at)?),
        Value::Array(a) if a.len() == 1 => match a.get(0, 0) {
            Value::Number(n) => sfix(*n),
            _ => None,
        },
        _ => None,
    }
}

unsafe fn coerce(ctx: &Ctx, a: &[*mut Xloper12]) -> (i32, Reply) {
    if a.is_empty() {
        return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
    }
    let at = ctx.caller.unwrap_or(CellAddr {
        sheet: xlcalc::SheetId(0),
        row: 0,
        col: 0,
    });
    let mut mask: u32 = 0x157;
    if let Some(&t) = a.get(1) {
        if !t.is_null() && (*t).kind() != xloper::MISSING {
            let Some(n) = int_operand(ctx, t, at) else {
                return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
            };
            mask = n as u32;
            if mask & 0x400 != 0 {
                mask |= 8;
            }
        }
    }
    if !a[0].is_null() {
        let raw = *a[0];
        match raw.kind() {
            xloper::INT if mask & 0x800 != 0 => return (RET_SUCCESS, Reply::Oper(raw, None)),

            SREF if mask & 0x400 != 0 => return (RET_SUCCESS, Reply::Oper(raw, None)),
            SREF if mask & 8 != 0 => {
                let r = raw.val.sref.rect;
                let mut alloc = Alloc::default();
                let sheet = ctx.caller.map_or(0, |c| c.sheet.0 as usize) + 1;
                let x = alloc.reference(sheet, &[r]);
                return (RET_SUCCESS, Reply::Oper(x, Some(alloc)));
            }
            REF if mask & 8 != 0 => {
                let m = raw.val.mref;
                let mut rects = Vec::new();
                if !m.refs.is_null() {
                    let n = (*m.refs).count as usize;
                    let table = m.refs.cast::<u8>().add(4).cast::<Xlref12>();
                    rects.extend((0..n).map(|i| table.add(i).read_unaligned()));
                }
                let mut alloc = Alloc::default();
                let x = alloc.reference(m.sheet, &rects);
                return (RET_SUCCESS, Reply::Oper(x, Some(alloc)));
            }
            _ => {}
        }
    }
    let Some(v) = arg_value(ctx, a[0]) else {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
    };
    let mut int = false;
    let out = coerce_to_mask(ctx, v, mask, at, &mut int);
    let bit = if int { 0x800 } else { type_bit(&out) };
    if bit & mask == 0 {
        let e = match out {
            Value::Error(e) => e,
            _ => XlError::VALUE,
        };
        return (RET_FAILED, Reply::Error(xloper::error_code(e)));
    }
    if int {
        if let Value::Number(n) = out {
            return (RET_SUCCESS, int_reply(n as i32));
        }
    }
    let mut alloc = Alloc::default();
    let x = alloc.convert(&out);
    (RET_SUCCESS, Reply::Oper(x, Some(alloc)))
}

unsafe fn sheet_id(ctx: &Ctx, a: &[*mut Xloper12]) -> (i32, Reply) {
    let Some(wb) = ctx.wb else {
        return (RET_FAILED, Reply::Error(ERR_VALUE));
    };

    let omitted = a
        .first()
        .is_none_or(|&p| p.is_null() || (*p).kind() == xloper::MISSING);
    let found = if omitted {
        Some((*wb).active_sheet().0 as usize)
    } else {
        let Some(name) = text_arg(ctx, a[0]) else {
            return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
        };
        (*wb)
            .sheets()
            .find(|s| s.name.eq_ignore_ascii_case(&name))
            .filter(|s| !matches!(s.kind, SheetKind::Chart | SheetKind::Module))
            .map(|s| s.id.0 as usize)
    };
    match found {
        Some(i) => (
            RET_SUCCESS,
            Reply::Oper(
                Xloper12::with(
                    Val {
                        mref: xloper::MRef {
                            refs: std::ptr::null_mut::<Xlmref12>(),
                            sheet: i + 1,
                        },
                    },
                    REF,
                ),
                None,
            ),
        ),
        None => (RET_FAILED, Reply::Error(ERR_VALUE)),
    }
}

const STACK_HEADROOM: i32 = 0x0010_0000;

fn int_reply(n: i32) -> Reply {
    Reply::Oper(Xloper12::with(Val { int: n }, xloper::INT), None)
}

fn bool_reply(b: bool) -> Reply {
    Reply::Oper(
        Xloper12::with(
            Val {
                boolean: i32::from(b),
            },
            xloper::BOOL,
        ),
        None,
    )
}

unsafe fn sheet_name(ctx: &Ctx, a: &[*mut Xloper12]) -> (i32, Reply) {
    let Some(&p) = a.first() else {
        return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
    };
    let (Some(wb), Some(caller)) = (ctx.wb, ctx.caller) else {
        return (RET_FAILED, Reply::Error(ERR_VALUE));
    };
    if p.is_null() {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
    }
    let sheet = match (*p).kind() {
        SREF => caller.sheet,
        REF => match (*p).val.mref.sheet.checked_sub(1) {
            Some(i) => xlcalc::SheetId(i as u32),
            None => return (RET_INV_XLOPER, Reply::Error(ERR_VALUE)),
        },
        _ => return (RET_INV_XLOPER, Reply::Error(ERR_VALUE)),
    };
    match (*wb).sheets().find(|s| s.id == sheet) {
        Some(s) => (
            RET_SUCCESS,
            text_oper(&format!("[{}]{}", (*wb).book_name(), s.name)),
        ),
        None => (RET_FAILED, Reply::Error(ERR_VALUE)),
    }
}

unsafe fn abort(a: &[*mut Xloper12]) -> (i32, Reply) {
    if let Some(&p) = a.first() {
        if !p.is_null() && (*p).kind() != xloper::MISSING && (*p).kind() != xloper::BOOL {
            return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
        }
    }
    (RET_SUCCESS, bool_reply(false))
}

fn find_binary(names: &[(String, usize)], name: &str) -> Option<usize> {
    let lower = name.to_lowercase();
    names.iter().position(|(n, _)| n.to_lowercase() == lower)
}

unsafe fn define_binary_name(ctx: &Ctx, a: &[*mut Xloper12]) -> (i32, Reply) {
    use windows::Win32::Foundation::{GlobalFree, HGLOBAL};
    use windows::Win32::System::Memory::{
        GlobalAlloc, GlobalLock, GlobalReAlloc, GlobalUnlock, GMEM_MOVEABLE,
    };
    if a.is_empty() || a.len() > 2 {
        return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
    }
    let Some(name) = text_arg(ctx, a[0]) else {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
    };
    let failed = (RET_FAILED, Reply::Error(ERR_VALUE));
    let Some(book) = ctx.wb else { return failed };
    let mut books = ctx
        .inner
        .binary_names
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let names = books.entry(book as usize).or_default();
    if a.len() == 1 {
        let Some(i) = find_binary(names, &name) else {
            return failed;
        };
        let (_, h) = names.remove(i);
        let _ = GlobalFree(Some(HGLOBAL(h as *mut _)));
        return (RET_SUCCESS, bool_reply(true));
    }
    if a[1].is_null() || (*a[1]).xltype != xloper::BIGDATA {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
    }

    let base = a[1].cast::<u8>();
    let data = base.cast::<*const u8>().read_unaligned();
    let size = base.add(8).cast::<i32>().read_unaligned();
    if size < 0 || (data.is_null() && size > 0) {
        return failed;
    }
    let size = size as usize;
    let h = match find_binary(names, &name) {
        Some(i) => match GlobalReAlloc(HGLOBAL(names[i].1 as *mut _), size, 0) {
            Ok(h) => {
                names[i].1 = h.0 as usize;
                h
            }
            Err(_) => return failed,
        },
        None => match GlobalAlloc(GMEM_MOVEABLE, size) {
            Ok(h) => {
                names.push((name, h.0 as usize));
                h
            }
            Err(_) => return failed,
        },
    };
    let p = GlobalLock(h);
    if !p.is_null() && size > 0 {
        std::ptr::copy_nonoverlapping(data, p.cast::<u8>(), size);
    }
    let _ = GlobalUnlock(h);
    (RET_SUCCESS, bool_reply(true))
}

unsafe fn get_binary_name(ctx: &Ctx, a: &[*mut Xloper12]) -> (i32, Reply) {
    use windows::Win32::Foundation::HGLOBAL;
    use windows::Win32::System::Memory::GlobalSize;
    if a.len() != 1 {
        return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
    }
    let Some(name) = text_arg(ctx, a[0]) else {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
    };
    let Some(book) = ctx.wb else {
        return (RET_FAILED, Reply::Error(ERR_VALUE));
    };
    let books = ctx
        .inner
        .binary_names
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let names = books.get(&(book as usize)).map_or(&[][..], Vec::as_slice);
    match find_binary(names, &name) {
        Some(i) => {
            let h = names[i].1;
            let size = GlobalSize(HGLOBAL(h as *mut _));
            (
                RET_SUCCESS,
                Reply::Oper(Xloper12::bigdata(h, size as u32), None),
            )
        }
        None => (RET_FAILED, Reply::Error(ERR_VALUE)),
    }
}

unsafe fn async_return(ctx: &Ctx, a: &[*mut Xloper12]) -> (i32, Reply) {
    if a.len() != 2 {
        return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
    }
    let handle = if a[0].is_null() {
        None
    } else {
        Some(u64::from((*a[0]).val.int as u32))
    };
    let Some(handle) = handle else {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
    };
    let value = arg_value(ctx, a[1]).unwrap_or(Value::Error(XlError::VALUE));
    if ctx.async_handle == Some(handle) {
        FRAMES.with(|f| {
            if let Some(top) = f.borrow_mut().last_mut() {
                top.async_result = Some(value);
            }
        });
    } else {
        host::deliver_late(handle, value);
    }
    (
        RET_SUCCESS,
        Reply::Oper(Xloper12::with(Val { boolean: 1 }, xloper::BOOL), None),
    )
}

unsafe fn async_return_foreign(a: &[*mut Xloper12]) -> (i32, Reply) {
    if a.len() != 2 {
        return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
    }
    if a[0].is_null() || a[1].is_null() {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
    }
    let handle = u64::from((*a[0]).val.int as u32);
    let value = xloper::to_value(&*a[1]).unwrap_or(Value::Error(XlError::VALUE));
    host::deliver_late(handle, value);
    (
        RET_SUCCESS,
        Reply::Oper(Xloper12::with(Val { boolean: 1 }, xloper::BOOL), None),
    )
}

unsafe fn event_register(ctx: &Ctx, a: &[*mut Xloper12]) -> (i32, Reply) {
    if a.len() < 2 {
        return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
    }
    let Some(Value::Number(event)) = arg_value(ctx, a[1]) else {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
    };
    if event != 1.0 && event != 2.0 {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
    }
    let Some(name) = text_arg(ctx, a[0]) else {
        return (RET_SUCCESS, int_reply(0));
    };
    let Some(address) = ctx.inner.symbol(&name) else {
        return (RET_SUCCESS, Reply::Error(ERR_NA));
    };
    ctx.inner
        .events
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .push((event as i32, address));
    (
        RET_SUCCESS,
        Reply::Oper(Xloper12::with(Val { boolean: 1 }, xloper::BOOL), None),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn references_in_evaluated_text() {
        assert!(has_reference("B1"));
        assert!(has_reference("A1+1"));
        assert!(has_reference("Sheet1!A1"));
        assert!(!has_reference("abc"));
        assert!(!has_reference("1+2"));
        assert!(!has_reference("SUM(1,2)"));
        assert!(!has_reference("\"B1\""));
        assert!(!has_reference("1E5"));
        assert!(!has_reference("TRUE"));
    }
}
