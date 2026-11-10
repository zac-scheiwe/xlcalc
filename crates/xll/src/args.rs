use xlcalc::{format_general, Area, Array, CellAddr, SheetId, Value, Workbook};

use crate::sig::Ty;
use crate::xloper::{
    self, Alloc, MRef, SRef, Val, Xlmref12, Xloper12, Xlref12, ERR_NUM, ERR_VALUE, MISSING,
};
use crate::xloper_old::{self, Xloper};

#[derive(Default)]
pub struct Arena {
    alloc: Alloc,
    opers: Vec<Box<[Xloper12]>>,
    old: Vec<Box<[Xloper]>>,
    old_alloc: xloper_old::Alloc,
    wide: Vec<Vec<u16>>,
    ansi: Vec<Vec<u8>>,
    ints: Vec<Box<[i32]>>,
    doubles: Vec<Box<[f64]>>,
    raw: Vec<Vec<u64>>,
    handle: Option<u64>,
}

impl Arena {
    fn oper(&mut self, x: Xloper12) -> u64 {
        let mut b = vec![x].into_boxed_slice();
        let p = b.as_mut_ptr() as u64;
        self.opers.push(b);
        p
    }

    fn old_oper(&mut self, x: Xloper) -> u64 {
        let mut b = vec![x].into_boxed_slice();
        let p = b.as_mut_ptr() as u64;
        self.old.push(b);
        p
    }

    pub fn async_handle(&mut self, id: u64) -> u64 {
        self.handle = Some(id);

        self.oper(Xloper12::with(Val { int: id as i32 }, 0x802))
    }

    pub fn handle(&self) -> Option<u64> {
        self.handle
    }
}

type Fail = i32;

fn sheet_id(sheet: SheetId) -> usize {
    sheet.0 as usize + 1
}

fn id_sheet(id: usize) -> SheetId {
    SheetId(id.saturating_sub(1) as u32)
}

fn area_value(wb: &Workbook, a: &Area) -> Value {
    let r = a.rect;
    if r.r0 == r.r1 && r.c0 == r.c1 {
        return wb.value(CellAddr {
            sheet: a.sheet,
            row: r.r0,
            col: r.c0,
        });
    }
    let (rows, cols) = (r.r1 - r.r0 + 1, r.c1 - r.c0 + 1);
    let mut data = Vec::with_capacity(rows as usize * cols as usize);
    for row in r.r0..=r.r1 {
        for col in r.c0..=r.c1 {
            data.push(wb.value(CellAddr {
                sheet: a.sheet,
                row,
                col,
            }));
        }
    }
    Array::new(rows, cols, data).into_value()
}

fn xlref(r: xlcalc::Rect) -> Xlref12 {
    Xlref12 {
        row_first: r.r0 as i32,
        row_last: r.r1 as i32,
        col_first: r.c0 as i32,
        col_last: r.c1 as i32,
    }
}

fn oper_for(
    arena: &mut Arena,
    wb: &Workbook,
    caller: CellAddr,
    thread_safe: bool,
    arg: &xlcalc::Arg,
) -> Xloper12 {
    match arg {
        xlcalc::Arg::Missing => Xloper12::with(Val { int: 0 }, MISSING),
        xlcalc::Arg::Value(v) => arena.alloc.convert(v),
        xlcalc::Arg::Ref(areas) | xlcalc::Arg::XRef(areas) if thread_safe => match areas.as_slice()
        {
            [one] => arena.alloc.convert(&area_value(wb, one)),
            _ => Xloper12::error(ERR_VALUE),
        },
        xlcalc::Arg::Ref(areas) | xlcalc::Arg::XRef(areas) => match areas.as_slice() {
            [one] if one.sheet == caller.sheet && matches!(arg, xlcalc::Arg::Ref(_)) => {
                Xloper12::with(
                    Val {
                        sref: SRef {
                            count: 1,
                            rect: xlref(one.rect),
                        },
                    },
                    xloper::SREF,
                )
            }
            areas => {
                let n = areas.len();
                let mut words = vec![0u64; (4 + 16 * n).div_ceil(8)];
                let table = words.as_mut_ptr().cast::<Xlmref12>();

                unsafe {
                    (*table).count = n as u16;
                    let first = std::ptr::addr_of_mut!((*table).table).cast::<Xlref12>();
                    for (i, a) in areas.iter().enumerate() {
                        *first.add(i) = xlref(a.rect);
                    }
                }
                arena.raw.push(words);
                let refs = arena
                    .raw
                    .last_mut()
                    .map_or(std::ptr::null_mut(), |w| w.as_mut_ptr().cast::<Xlmref12>());
                Xloper12::with(
                    Val {
                        mref: MRef {
                            refs,
                            sheet: areas.first().map_or(0, |a| sheet_id(a.sheet)),
                        },
                    },
                    xloper::REF,
                )
            }
        },
    }
}

fn scalar(wb: &Workbook, caller: CellAddr, legacy: bool, arg: &xlcalc::Arg) -> Result<Value, Fail> {
    match arg {
        xlcalc::Arg::Missing => Ok(Value::Blank),
        xlcalc::Arg::Value(Value::Array(a)) => Ok(a.data.first().cloned().unwrap_or(Value::Blank)),
        xlcalc::Arg::Value(v) => Ok(v.clone()),
        xlcalc::Arg::Ref(areas) | xlcalc::Arg::XRef(areas) => {
            let [a] = areas.as_slice() else {
                return Err(ERR_VALUE);
            };
            let r = a.rect;
            if r.r0 == r.r1 && r.c0 == r.c1 {
                return Ok(area_value(wb, a));
            }
            let at = if !legacy {
                Some((r.r0, r.c0))
            } else if a.sheet != caller.sheet {
                None
            } else if r.c0 == r.c1 && (r.r0..=r.r1).contains(&caller.row) {
                Some((caller.row, r.c0))
            } else if r.r0 == r.r1 && (r.c0..=r.c1).contains(&caller.col) {
                Some((r.r0, caller.col))
            } else {
                None
            };
            let (row, col) = at.ok_or(ERR_VALUE)?;
            Ok(wb.value(CellAddr {
                sheet: a.sheet,
                row,
                col,
            }))
        }
    }
}

fn text_number(wb: &Workbook, at: CellAddr, s: &str) -> Result<f64, Fail> {
    let formula = format!("=--\"{}\"", s.replace('"', "\"\""));
    match wb.cfv_eval(&formula, at, at) {
        Value::Number(n) => Ok(n),
        _ => Err(ERR_VALUE),
    }
}

fn number(wb: &Workbook, at: CellAddr, v: &Value) -> Result<f64, Fail> {
    match v {
        Value::Number(n) => Ok(*n),
        Value::Bool(b) => Ok(f64::from(u8::from(*b))),
        Value::Blank => Ok(0.0),
        Value::Text(t) => text_number(wb, at, t),
        Value::Error(_) | Value::Array(_) | Value::Lambda(_) => Err(ERR_VALUE),
    }
}

fn whole(n: f64, min: f64, max: f64, slack: bool) -> Result<i64, Fail> {
    let r = if n == -0.5 { 0.0 } else { n.round() };
    if r.is_nan() || r > max || (r < min && !(slack && n > min - 1.0)) {
        Err(ERR_NUM)
    } else {
        Ok(r.max(min) as i64)
    }
}

fn boolean(wb: &Workbook, at: CellAddr, v: &Value) -> Result<bool, Fail> {
    match v {
        Value::Bool(b) => Ok(*b),
        Value::Text(t) if t.eq_ignore_ascii_case("true") => Ok(true),
        Value::Text(t) if t.eq_ignore_ascii_case("false") => Ok(false),
        Value::Text(_) => Err(ERR_VALUE),
        other => Ok(number(wb, at, other)? != 0.0),
    }
}

fn text(v: &Value) -> Result<String, Fail> {
    match v {
        Value::Text(t) => Ok(t.to_string()),
        Value::Number(n) => Ok(format_general(*n)),
        Value::Bool(b) => Ok(if *b { "TRUE" } else { "FALSE" }.to_string()),
        Value::Blank => Ok(String::new()),
        Value::Error(_) | Value::Array(_) | Value::Lambda(_) => Err(ERR_VALUE),
    }
}

pub fn slot(
    arena: &mut Arena,
    wb: &Workbook,
    caller: CellAddr,
    thread_safe: bool,
    legacy: bool,
    ty: Ty,
    arg: &xlcalc::Arg,
) -> Result<u64, Fail> {
    if matches!(ty, Ty::Oper | Ty::OperRef) {
        let x = oper_for(arena, wb, caller, thread_safe, arg);
        return Ok(arena.oper(x));
    }
    if matches!(ty, Ty::Fp12 | Ty::Fp) {
        return fp(arena, wb, caller, arg, ty == Ty::Fp12);
    }
    if matches!(ty, Ty::OperOld | Ty::OperRefOld) {
        let x = old_oper_for(arena, wb, caller, ty == Ty::OperRefOld, arg);
        return Ok(arena.old_oper(x));
    }
    let v = scalar(wb, caller, legacy, arg)?;
    Ok(match ty {
        Ty::Double => number(wb, caller, &v)?.to_bits(),
        Ty::DoublePtr => {
            let mut b = vec![number(wb, caller, &v)?].into_boxed_slice();
            let p = b.as_mut_ptr() as u64;
            arena.doubles.push(b);
            p
        }
        Ty::Int => whole(
            number(wb, caller, &v)?,
            f64::from(i32::MIN),
            f64::from(i32::MAX),
            true,
        )? as u64,
        Ty::Short => {
            whole(
                number(wb, caller, &v)?,
                f64::from(i16::MIN),
                f64::from(i16::MAX),
                false,
            )? as u64
                & 0xFFFF
        }

        Ty::Ushort => whole(
            number(wb, caller, &v)?.abs(),
            0.0,
            f64::from(u16::MAX),
            false,
        )? as u64,
        Ty::Short01 => u64::from(boolean(wb, caller, &v)?),
        Ty::BoolPtr => {
            let mut b = vec![i32::from(boolean(wb, caller, &v)?)].into_boxed_slice();
            let p = b.as_mut_ptr() as u64;
            arena.ints.push(b);
            p
        }
        Ty::Wide => {
            let mut u: Vec<u16> = text(&v)?.encode_utf16().take(32767).collect();
            u.push(0);
            let p = u.as_ptr() as u64;
            arena.wide.push(u);
            p
        }
        Ty::WideCounted => {
            let t: Vec<u16> = text(&v)?.encode_utf16().take(32767).collect();
            let mut u = vec![t.len() as u16];
            u.extend(t);
            let p = u.as_ptr() as u64;
            arena.wide.push(u);
            p
        }

        Ty::Ansi => {
            let mut b = ansi_bytes(&text(&v)?)?;
            b.push(0);
            let p = b.as_ptr() as u64;
            arena.ansi.push(b);
            p
        }
        Ty::AnsiCounted => {
            let t = ansi_bytes(&text(&v)?)?;
            let mut b = vec![t.len() as u8];
            b.extend(t);
            let p = b.as_ptr() as u64;
            arena.ansi.push(b);
            p
        }
        Ty::Fp12
        | Ty::Fp
        | Ty::Oper
        | Ty::OperRef
        | Ty::OperOld
        | Ty::OperRefOld
        | Ty::AsyncHandle
        | Ty::Void => return Err(ERR_VALUE),
    })
}

fn fp(
    arena: &mut Arena,
    wb: &Workbook,
    caller: CellAddr,
    arg: &xlcalc::Arg,
    wide: bool,
) -> Result<u64, Fail> {
    let (rows, cols, data) = match arg {
        xlcalc::Arg::Missing => return Err(ERR_VALUE),
        xlcalc::Arg::Value(Value::Array(a)) => (a.rows, a.cols, a.data.clone()),
        xlcalc::Arg::Ref(areas) | xlcalc::Arg::XRef(areas) if areas.len() == 1 => {
            match area_value(wb, &areas[0]) {
                Value::Array(a) => (a.rows, a.cols, a.data.clone()),
                one => (1, 1, vec![one]),
            }
        }
        xlcalc::Arg::Value(v) => (1, 1, vec![v.clone()]),
        xlcalc::Arg::Ref(_) | xlcalc::Arg::XRef(_) => return Err(ERR_VALUE),
    };
    let mut words = vec![0u64; 1 + data.len()];
    words[0] = if wide {
        u64::from(rows) | (u64::from(cols) << 32)
    } else {
        u64::from(rows & 0xFFFF) | (u64::from(cols & 0xFFFF) << 16)
    };
    for (i, d) in data.iter().enumerate() {
        words[1 + i] = number(wb, caller, d)?.to_bits();
    }
    let p = words.as_ptr() as u64;
    arena.raw.push(words);
    Ok(p)
}

fn ansi_bytes(s: &str) -> Result<Vec<u8>, Fail> {
    if s.encode_utf16().count() > 255 {
        return Err(ERR_VALUE);
    }
    Ok(ansi_bytes_lossy(s))
}

pub(crate) fn ansi_bytes_lossy(s: &str) -> Vec<u8> {
    let units: Vec<u16> = s.encode_utf16().collect();
    if units.is_empty() {
        return Vec::new();
    }
    use windows::Win32::Globalization::{WideCharToMultiByte, CP_ACP};

    unsafe {
        let n = WideCharToMultiByte(CP_ACP, 0, &units, None, windows::core::PCSTR::null(), None);
        let mut out = vec![0u8; usize::try_from(n).unwrap_or(0)];
        let n = WideCharToMultiByte(
            CP_ACP,
            0,
            &units,
            Some(&mut out),
            windows::core::PCSTR::null(),
            None,
        );
        out.truncate(usize::try_from(n).unwrap_or(0));
        out
    }
}

pub(crate) fn ansi_string(b: &[u8]) -> String {
    if b.is_empty() {
        return String::new();
    }
    use windows::Win32::Globalization::{
        MultiByteToWideChar, CP_ACP, MULTI_BYTE_TO_WIDE_CHAR_FLAGS,
    };

    unsafe {
        let n = MultiByteToWideChar(CP_ACP, MULTI_BYTE_TO_WIDE_CHAR_FLAGS(0), b, None);
        let mut out = vec![0u16; usize::try_from(n).unwrap_or(0)];
        let n = MultiByteToWideChar(CP_ACP, MULTI_BYTE_TO_WIDE_CHAR_FLAGS(0), b, Some(&mut out));
        out.truncate(usize::try_from(n).unwrap_or(0));
        String::from_utf16_lossy(&out)
    }
}

fn old_oper_for(
    arena: &mut Arena,
    wb: &Workbook,
    caller: CellAddr,
    refs: bool,
    arg: &xlcalc::Arg,
) -> Xloper {
    match arg {
        xlcalc::Arg::Missing => Xloper::missing(),
        xlcalc::Arg::Value(v) => arena.old_alloc.convert(v),
        xlcalc::Arg::Ref(areas) | xlcalc::Arg::XRef(areas) if refs => match areas.as_slice() {
            [one] if one.sheet == caller.sheet && matches!(arg, xlcalc::Arg::Ref(_)) => {
                arena.old_alloc.sref(one.rect)
            }
            areas => {
                let rects: Vec<xlcalc::Rect> = areas.iter().map(|a| a.rect).collect();
                let sheet = areas.first().map_or(0, |a| sheet_id(a.sheet) as u32);
                arena.old_alloc.mref(&rects, sheet)
            }
        },
        xlcalc::Arg::Ref(areas) | xlcalc::Arg::XRef(areas) => match areas.as_slice() {
            [one] => arena.old_alloc.convert(&area_value(wb, one)),
            _ => Xloper::error(ERR_VALUE),
        },
    }
}

pub fn read(wb: &Workbook, caller: CellAddr, x: &Xloper12) -> Value {
    unsafe {
        match x.kind() {
            xloper::SREF => {
                let r = x.val.sref.rect;
                area_value(
                    wb,
                    &Area {
                        sheet: caller.sheet,
                        rect: xlcalc::Rect {
                            r0: r.row_first.max(0) as u32,
                            c0: r.col_first.max(0) as u32,
                            r1: r.row_last.max(0) as u32,
                            c1: r.col_last.max(0) as u32,
                        },
                    },
                )
            }
            xloper::REF => {
                let m = x.val.mref;
                if m.refs.is_null() || (*m.refs).count == 0 {
                    return Value::Error(xlcalc::XlError::VALUE);
                }
                let r = (*m.refs).table[0];
                area_value(
                    wb,
                    &Area {
                        sheet: id_sheet(m.sheet),
                        rect: xlcalc::Rect {
                            r0: r.row_first.max(0) as u32,
                            c0: r.col_first.max(0) as u32,
                            r1: r.row_last.max(0) as u32,
                            c1: r.col_last.max(0) as u32,
                        },
                    },
                )
            }
            _ => xloper::to_value(x).unwrap_or(Value::Error(xlcalc::XlError::VALUE)),
        }
    }
}

pub fn read_old(wb: &Workbook, caller: CellAddr, x: &Xloper) -> Value {
    unsafe {
        match x.kind() {
            xloper_old::SREF | xloper_old::REF => match xloper_old::first_rect(x) {
                Some((rect, id)) => {
                    let sheet = if x.kind() == xloper_old::SREF {
                        caller.sheet
                    } else {
                        id_sheet(id as usize)
                    };
                    area_value(wb, &Area { sheet, rect })
                }
                None => Value::Error(xlcalc::XlError::VALUE),
            },
            _ => xloper_old::to_value(x).unwrap_or(Value::Error(xlcalc::XlError::VALUE)),
        }
    }
}

pub unsafe fn wide_text(p: *const u16) -> Value {
    if p.is_null() {
        return Value::Error(xlcalc::XlError::VALUE);
    }
    let mut n = 0;
    while *p.add(n) != 0 {
        n += 1;
    }
    Value::Text(String::from_utf16_lossy(std::slice::from_raw_parts(p, n)).into())
}

pub unsafe fn counted_text(p: *const u16) -> Value {
    if p.is_null() {
        return Value::Error(xlcalc::XlError::VALUE);
    }
    Value::Text(xloper::counted(p).into())
}

pub unsafe fn ansi_text(p: *const u8) -> Value {
    if p.is_null() {
        return Value::Error(xlcalc::XlError::VALUE);
    }
    let mut n = 0;
    while *p.add(n) != 0 {
        n += 1;
    }
    Value::Text(ansi_string(std::slice::from_raw_parts(p, n)).into())
}

pub unsafe fn ansi_counted_text(p: *const u8) -> Value {
    if p.is_null() {
        return Value::Error(xlcalc::XlError::VALUE);
    }
    Value::Text(ansi_string(std::slice::from_raw_parts(p.add(1), usize::from(*p))).into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn whole_rounds_away_from_zero_and_checks_range() {
        assert_eq!(whole(2.5, -10.0, 10.0, false), Ok(3));
        assert_eq!(whole(-2.5, -10.0, 10.0, false), Ok(-3));
        assert_eq!(whole(-0.5, -10.0, 10.0, false), Ok(0));
        assert_eq!(whole(-10.5, -10.0, 10.0, true), Ok(-10));
        assert_eq!(whole(-10.5, -10.0, 10.0, false), Err(ERR_NUM));
        assert_eq!(whole(11.0, -10.0, 10.0, false), Err(ERR_NUM));
        assert_eq!(whole(f64::NAN, -10.0, 10.0, false), Err(ERR_NUM));
    }

    #[test]
    fn text_forms() {
        assert_eq!(text(&Value::Number(12.0)), Ok("12".into()));
        assert_eq!(text(&Value::Bool(true)), Ok("TRUE".into()));
        assert_eq!(text(&Value::Blank), Ok(String::new()));
        assert_eq!(text(&Value::Error(xlcalc::XlError::DIV0)), Err(ERR_VALUE));
    }
}
