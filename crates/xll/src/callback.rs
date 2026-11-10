use xlcalc::{CellAddr, Value, Workbook, XlError};

use crate::args;
use crate::host::{self, Frame, FRAMES};
use crate::xlfn;
use crate::xloper::{
    self, Alloc, Val, Xlmref12, Xloper12, Xlref12, ERR_NA, ERR_VALUE, MULTI, REF, SREF,
};

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
    let outcome = std::panic::catch_unwind(|| dispatch(xlfn, &args));
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
            XL_GET_NAME => {
                if !a.is_empty() {
                    return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
                }
                (RET_SUCCESS, text_oper(&ctx.inner.path))
            }
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
            0..=0x0FFF => call_function(&ctx, xlfn, a),
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

unsafe fn coerce(ctx: &Ctx, a: &[*mut Xloper12]) -> (i32, Reply) {
    if a.is_empty() || a.len() > 2 {
        return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
    }
    let Some(v) = arg_value(ctx, a[0]) else {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
    };
    let target = if a.len() == 2 {
        match arg_value(ctx, a[1]) {
            Some(Value::Number(n)) => n as u32,
            _ => return (RET_INV_XLOPER, Reply::Error(ERR_VALUE)),
        }
    } else {
        0
    };
    let at = ctx.caller.unwrap_or(CellAddr {
        sheet: xlcalc::SheetId(0),
        row: 0,
        col: 0,
    });
    let converted = match (target, &v) {
        (0, _) => Ok(v.clone()),
        (xloper::NUM, Value::Error(e)) | (xloper::STR, Value::Error(e)) => Err(*e),
        (xloper::NUM, x) => match x {
            Value::Number(_) => Ok(x.clone()),
            Value::Bool(b) => Ok(Value::Number(f64::from(u8::from(*b)))),
            Value::Blank => Ok(Value::Number(0.0)),
            Value::Text(t) => match ctx.wb {
                Some(wb) => {
                    match (*wb).cfv_eval(&format!("=--\"{}\"", t.replace('"', "\"\"")), at, at) {
                        n @ Value::Number(_) => Ok(n),
                        _ => Err(XlError::VALUE),
                    }
                }
                None => Err(XlError::VALUE),
            },
            _ => Err(XlError::VALUE),
        },
        (xloper::STR, Value::Number(n)) => Ok(Value::Text(xlcalc::format_general(*n).into())),
        (xloper::STR, Value::Bool(b)) => Ok(Value::Text(if *b { "TRUE" } else { "FALSE" }.into())),
        (xloper::STR, Value::Blank) => Ok(Value::Text("".into())),
        (xloper::STR, t @ Value::Text(_)) => Ok(t.clone()),
        (xloper::BOOL, Value::Number(n)) => Ok(Value::Bool(*n != 0.0)),
        (xloper::BOOL, b @ Value::Bool(_)) => Ok(b.clone()),
        (xloper::BOOL, Value::Blank) => Ok(Value::Bool(false)),
        (MULTI, Value::Array(_)) => Ok(v.clone()),
        (MULTI, one) => Ok(xlcalc::Array::new(1, 1, vec![one.clone()]).into_value()),
        (_, Value::Error(e)) => Err(*e),
        _ => Err(XlError::VALUE),
    };
    match converted {
        Ok(v) => {
            let mut alloc = Alloc::default();
            let x = alloc.convert(&v);
            (RET_SUCCESS, Reply::Oper(x, Some(alloc)))
        }
        Err(e) => (RET_FAILED, Reply::Error(xloper::error_code(e))),
    }
}

unsafe fn sheet_id(ctx: &Ctx, a: &[*mut Xloper12]) -> (i32, Reply) {
    if a.len() != 1 {
        return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
    }
    let Some(name) = text_arg(ctx, a[0]) else {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
    };
    let Some(wb) = ctx.wb else {
        return (RET_FAILED, Reply::Error(ERR_VALUE));
    };
    let found = (*wb)
        .sheets()
        .find(|s| s.name.eq_ignore_ascii_case(&name.replace('\'', "")))
        .map(|s| s.id.0 as usize);
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
    if a.len() != 2 {
        return (RET_INV_COUNT, Reply::Error(ERR_VALUE));
    }
    let (Some(name), Some(Value::Number(event))) = (text_arg(ctx, a[0]), arg_value(ctx, a[1]))
    else {
        return (RET_INV_XLOPER, Reply::Error(ERR_VALUE));
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
