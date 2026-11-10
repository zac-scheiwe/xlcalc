use crate::callback::{dispatch, keep_in_frame_old, Reply, RET_FAILED};
use crate::xloper::{self, Alloc, Val, Xlmref12, Xloper12, Xlref12, ERR_NA, ERR_VALUE, REF, SREF};
use crate::xloper_old::{self, Alloc as OldAlloc, Xloper};

#[no_mangle]
pub unsafe extern "system" fn MdCallBack(
    xlfn: i32,
    count: i32,
    opers: *mut *mut Xloper,
    result: *mut Xloper,
) -> i32 {
    let olds: Vec<*mut Xloper> = if count > 0 && !opers.is_null() {
        std::slice::from_raw_parts(opers, count as usize).to_vec()
    } else {
        Vec::new()
    };

    if xlfn == crate::callback::XL_ASYNC_RETURN || xlfn == crate::callback::XL_EVENT_REGISTER {
        let async_call = xlfn == crate::callback::XL_ASYNC_RETURN;
        if !result.is_null() {
            *result = if async_call {
                Xloper::error(ERR_VALUE)
            } else {
                Xloper::with(0, 0, xloper_old::INT)
            };
        }
        return if async_call { 256 } else { 0 };
    }
    let outcome = std::panic::catch_unwind(|| {
        let mut args = Args {
            opers: Vec::with_capacity(olds.len()),
            ..Args::default()
        };
        let ptrs: Vec<*mut Xloper12> = olds.iter().map(|&p| args.add(p)).collect();
        crate::callback::OLD_CALL.with(|o| o.set(true));
        let r = dispatch(xlfn, &ptrs);
        crate::callback::OLD_CALL.with(|o| o.set(false));
        r
    });
    let (mut code, reply) = outcome.unwrap_or((RET_FAILED, Reply::Error(ERR_VALUE)));
    if !result.is_null() {
        let mut alloc = OldAlloc::default();
        let mut x = from_reply(reply, &mut alloc);
        if code == RET_FAILED {
            x = Xloper::nil();
        } else if x.kind() == xloper_old::ERR && x.val[0] as u16 == ERR_NA as u16 && code == 0 {
            code = RET_FAILED;
        }
        *result = x;
        keep_in_frame_old(alloc);
    }
    code
}

#[derive(Default)]
struct Args {
    opers: Vec<Xloper12>,
    values: Alloc,
    tables: Vec<Vec<u32>>,
}

impl Args {
    unsafe fn add(&mut self, p: *mut Xloper) -> *mut Xloper12 {
        if p.is_null() {
            return std::ptr::null_mut();
        }
        let x = *p;
        let converted = match x.kind() {
            xloper_old::SREF => match xloper_old::rects(&x) {
                Some((rects, _)) if !rects.is_empty() => Xloper12::with(
                    Val {
                        sref: xloper::SRef {
                            count: 1,
                            rect: rect12(rects[0]),
                        },
                    },
                    SREF,
                ),
                _ => Xloper12::error(ERR_VALUE),
            },
            xloper_old::REF => match xloper_old::rects(&x) {
                Some((rects, sheet)) => {
                    let mut t = vec![0u32; 1 + 4 * rects.len().max(1)];
                    t[0] = rects.len() as u32 & 0xFFFF;
                    for (i, r) in rects.iter().enumerate() {
                        let r = rect12(*r);
                        t[1 + 4 * i] = r.row_first as u32;
                        t[2 + 4 * i] = r.row_last as u32;
                        t[3 + 4 * i] = r.col_first as u32;
                        t[4 + 4 * i] = r.col_last as u32;
                    }
                    let refs = t.as_mut_ptr().cast::<Xlmref12>();
                    self.tables.push(t);
                    Xloper12::with(
                        Val {
                            mref: xloper::MRef {
                                refs,
                                sheet: sheet as usize,
                            },
                        },
                        REF,
                    )
                }
                None => Xloper12::error(ERR_VALUE),
            },
            _ => match xloper_old::to_value(&x) {
                Some(v) => self.values.convert(&v),
                None => Xloper12::error(ERR_VALUE),
            },
        };
        self.opers.push(converted);
        self.opers
            .last_mut()
            .map_or(std::ptr::null_mut(), std::ptr::from_mut)
    }
}

fn rect12(r: xlcalc::Rect) -> Xlref12 {
    Xlref12 {
        row_first: r.r0 as i32,
        row_last: r.r1 as i32,
        col_first: r.c0 as i32,
        col_last: r.c1 as i32,
    }
}

fn rect_of(r: Xlref12) -> xlcalc::Rect {
    xlcalc::Rect {
        r0: r.row_first as u32,
        r1: r.row_last as u32,
        c0: r.col_first as u32,
        c1: r.col_last as u32,
    }
}

unsafe fn from_reply(reply: Reply, alloc: &mut OldAlloc) -> Xloper {
    match reply {
        Reply::Error(code) => Xloper::error(code),
        Reply::Nothing => Xloper::nil(),
        Reply::Oper(x, _) => match x.kind() {
            SREF => {
                let s = x.val.sref;
                alloc.sref(rect_of(s.rect))
            }
            REF => {
                let m = x.val.mref;
                if m.refs.is_null() {
                    Xloper::with(0, m.sheet as u64, xloper_old::REF)
                } else {
                    let n = usize::from((*m.refs).count);
                    let table = std::ptr::addr_of!((*m.refs).table).cast::<Xlref12>();
                    let rects: Vec<xlcalc::Rect> = (0..n).map(|i| rect_of(*table.add(i))).collect();
                    alloc.mref(&rects, m.sheet as u32)
                }
            }
            _ => match xloper::to_value(&x) {
                Some(v) => alloc.convert(&v),
                None => Xloper::error(15),
            },
        },
    }
}
