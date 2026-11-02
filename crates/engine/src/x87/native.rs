use std::arch::asm;

static CW: u16 = 0x137F;

macro_rules! x87 {
    ($($tmpl:expr),* ; $($ops:tt)*) => {{
        let mut env = [0u32; 7];

        unsafe {
            asm!(
                "fnstenv [{env}]",
                "fldcw word ptr [{cw}]",
                $($tmpl,)*
                "fldenv [{env}]",
                cw = in(reg) &CW,
                env = in(reg) &mut env,
                $($ops)*
                out("st(0)") _, out("st(1)") _, out("st(2)") _, out("st(3)") _,
                out("st(4)") _, out("st(5)") _, out("st(6)") _, out("st(7)") _,
                options(nostack)
            );
        }
    }};
}

pub fn ln(x: f64) -> f64 {
    let mut r = 0.0f64;
    x87!("fldln2", "fld qword ptr [{x}]", "fyl2x", "fstp qword ptr [{r}]";
        x = in(reg) &x, r = in(reg) &mut r,);
    r
}

pub fn log10(x: f64) -> f64 {
    let mut r = 0.0f64;
    x87!("fldlg2", "fld qword ptr [{x}]", "fyl2x", "fstp qword ptr [{r}]";
        x = in(reg) &x, r = in(reg) &mut r,);
    r
}

pub fn exp(x: f64) -> f64 {
    let mut r = 0.0f64;
    x87!(
        "fldl2e",
        "fmul qword ptr [{x}]",
        "fld st(0)",
        "frndint",
        "fxch st(1)",
        "fsub st(0), st(1)",
        "fxam",
        "wait",
        "fnstsw ax",
        "fabs",
        "f2xm1",
        "fld1",
        "faddp st(1), st(0)",
        "test ah, 2",
        "je 2f",
        "fld1",
        "fdivrp st(1), st(0)",
        "2:",
        "fscale",
        "fstp qword ptr [{r}]",
        "fstp st(0)";
        x = in(reg) &x, r = in(reg) &mut r, out("ax") _,
    );
    r
}

pub fn atan_abs(a: f64) -> f64 {
    let mut r = 0.0f64;
    x87!(
        "fld qword ptr [{x}]",
        "fld1",
        "fcom st(1)",
        "wait",
        "fnstsw ax",
        "sahf",
        "ja 3f",
        "jb 4f",
        "faddp st(1), st(0)",
        "fchs",
        "fldpi",
        "fscale",
        "fstp qword ptr [{r}]",
        "fstp st(0)",
        "jmp 6f",
        "3:",
        "fpatan",
        "jmp 5f",
        "4:",
        "fxch st(1)",
        "fpatan",
        "fld1",
        "fchs",
        "fldpi",
        "fscale",
        "fsubrp st(2), st(0)",
        "fstp st(0)",
        "5:",
        "fstp qword ptr [{r}]",
        "6:";
        x = in(reg) &a, r = in(reg) &mut r, out("ax") _,
    );
    r
}

macro_rules! trig_core {
    () => {
        "fld1
        fadd st(0), st(0)
        fchs
        fldpi
        fscale
        fstp st(1)
        fld qword ptr [{x}]
        fprem
        wait
        fnstsw ax
        mov edx, eax
        ftst
        wait
        fnstsw ax
        sahf
        je 7f
        test dh, 2
        je 3f
        fsubp st(1), st(0)
        jmp 4f
        3:
        fstp st(1)
        4:
        fptan
        test dh, 0x42
        jp 8f
        fxch st(1)
        jmp 8f
        7:
        fstp st(1)
        fld1
        test dh, 1
        je 5f
        fst st(1)
        5:
        test dh, 0x42
        jp 8f
        fxch st(1)
        8:"
    };
}

pub fn sin_abs(a: f64) -> f64 {
    let mut r = 0.0f64;
    x87!(
        trig_core!(),
        "fmul st(0), st(0)",
        "fxch st(1)",
        "fmul st(0), st(0)",
        "fadd st(1), st(0)",
        "fdivrp st(1), st(0)",
        "fsqrt",
        "test dh, 1",
        "je 9f",
        "fchs",
        "9:",
        "fstp qword ptr [{r}]";
        x = in(reg) &a, r = in(reg) &mut r, out("ax") _, out("dx") _,
    );
    r
}

pub fn cos_abs(a: f64) -> f64 {
    let mut r = 0.0f64;
    x87!(
        trig_core!(),
        "fmul st(0), st(0)",
        "fxch st(1)",
        "fmul st(0), st(0)",
        "fadd st(0), st(1)",
        "fdivp st(1), st(0)",
        "fsqrt",
        "test dh, 0x41",
        "jp 9f",
        "fchs",
        "9:",
        "fstp qword ptr [{r}]";
        x = in(reg) &a, r = in(reg) &mut r, out("ax") _, out("dx") _,
    );
    r
}

pub fn tan_abs(a: f64) -> f64 {
    let mut r = 0.0f64;
    x87!(
        trig_core!(),
        "test dh, 0x40",
        "je 9f",
        "fchs",
        "9:",
        "fdivp st(1), st(0)",
        "fstp qword ptr [{r}]";
        x = in(reg) &a, r = in(reg) &mut r, out("ax") _, out("dx") _,
    );
    r
}

#[cfg(test)]
pub fn arith(op: u8, a: f64, b: f64) -> f64 {
    let mut r = 0.0f64;
    match op {
        b'+' => x87!("fld qword ptr [{a}]", "fadd qword ptr [{b}]", "fstp qword ptr [{r}]";
            a = in(reg) &a, b = in(reg) &b, r = in(reg) &mut r,),
        b'*' => x87!("fld qword ptr [{a}]", "fmul qword ptr [{b}]", "fstp qword ptr [{r}]";
            a = in(reg) &a, b = in(reg) &b, r = in(reg) &mut r,),
        b'/' => x87!("fld qword ptr [{a}]", "fdiv qword ptr [{b}]", "fstp qword ptr [{r}]";
            a = in(reg) &a, b = in(reg) &b, r = in(reg) &mut r,),
        _ => x87!("fld qword ptr [{a}]", "fsqrt", "fstp qword ptr [{r}]";
            a = in(reg) &a, r = in(reg) &mut r,),
    }
    r
}
