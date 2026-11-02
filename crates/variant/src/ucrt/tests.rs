use super::{pow, Errno};
use crate::oleaut::Rng;
use std::ffi::c_void;

extern "system" {
    fn LoadLibraryW(name: *const u16) -> *mut c_void;
    fn GetProcAddress(module: *mut c_void, name: *const u8) -> *mut c_void;
}

type Pow = extern "C" fn(f64, f64) -> f64;
type ErrnoFn = extern "C" fn() -> *mut i32;
type SetFma = extern "C" fn(i32) -> i32;
type GetFma = extern "C" fn() -> i32;

struct Ucrt {
    pow: Pow,
    errno: ErrnoFn,
    set_fma: SetFma,
    get_fma: GetFma,
}

fn load() -> Ucrt {
    let wide: Vec<u16> = "ucrtbase.dll".encode_utf16().chain([0]).collect();

    let module = unsafe { LoadLibraryW(wide.as_ptr()) };
    assert!(!module.is_null(), "LoadLibraryW(ucrtbase.dll) failed");
    let sym = |name: &str| {
        let c: Vec<u8> = name.bytes().chain([0]).collect();

        let p = unsafe { GetProcAddress(module, c.as_ptr()) };
        assert!(!p.is_null(), "ucrtbase.dll has no {name}");
        p
    };

    unsafe {
        Ucrt {
            pow: std::mem::transmute::<*mut c_void, Pow>(sym("pow")),
            errno: std::mem::transmute::<*mut c_void, ErrnoFn>(sym("_errno")),
            set_fma: std::mem::transmute::<*mut c_void, SetFma>(sym("_set_FMA3_enable")),
            get_fma: std::mem::transmute::<*mut c_void, GetFma>(sym("_get_FMA3_enable")),
        }
    }
}

impl Ucrt {
    fn call(&self, x: f64, y: f64) -> (f64, Errno) {
        let e = (self.errno)();

        unsafe { *e = 0 };
        let z = (self.pow)(x, y);

        let errno = match unsafe { *e } {
            0 => Errno::None,
            33 => Errno::Domain,
            34 => Errno::Range,
            n => panic!("pow({x:e}, {y:e}) set errno {n}"),
        };
        (z, errno)
    }
}

const SPECIAL: [f64; 30] = [
    0.0,
    -0.0,
    1.0,
    -1.0,
    0.5,
    -0.5,
    2.0,
    -2.0,
    3.0,
    -3.0,
    1.5,
    10.0,
    0.1,
    f64::INFINITY,
    f64::NEG_INFINITY,
    f64::NAN,
    -f64::NAN,
    f64::MAX,
    f64::MIN_POSITIVE,
    5e-324,
    -5e-324,
    1e-310,
    9_007_199_254_740_992.0,
    9_007_199_254_740_993.0,
    -9_007_199_254_740_991.0,
    1.0000000000000002,
    0.9999999999999999,
    1e300,
    -1e300,
    4.9e-20,
];

const NANS: [u64; 3] = [
    0x7ff0_0000_0000_0001,
    0xfff4_0000_0000_0000,
    0x7ff8_0000_dead_beef,
];

fn sign(rng: &mut Rng) -> f64 {
    if rng.below(2) == 0 {
        1.0
    } else {
        -1.0
    }
}

fn unit(rng: &mut Rng) -> f64 {
    (rng.next() >> 11) as f64 * f64::powi(2.0, -53)
}

fn pair(rng: &mut Rng, c: u32) -> (f64, f64) {
    match c {
        0 => (rng.any_double(), rng.any_double()),
        1 => (rng.double(-1022, 1023).abs(), rng.double(-10, 10)),
        2 => (1.0 + rng.double(-60, -1), rng.double(0, 60)),
        3 => {
            let x = rng.double(-30, 30).abs();
            let t = sign(rng) * (690.0 + 60.0 * unit(rng));
            (x, t / x.ln())
        }
        4 => {
            let x = -rng.double(-30, 30).abs();
            let y = match rng.below(3) {
                0 => rng.below(2000) as f64 - 1000.0,
                1 => (rng.below(1 << 20) as f64 + 9_007_199_254_740_992.0) * sign(rng),
                _ => rng.double(-4, 70).trunc(),
            };
            (x, y)
        }
        5 => (
            f64::from_bits(rng.next() & 0x800f_ffff_ffff_ffff),
            rng.double(-6, 3),
        ),
        6 => {
            let y = if rng.below(2) == 0 {
                rng.double(-80, -60)
            } else {
                rng.double(55, 70)
            };
            (rng.double(-40, 40), y)
        }
        7 => (
            rng.below(1000) as f64,
            (rng.below(64) as f64 - 32.0) / [1.0, 2.0, 3.0, 4.0, 8.0][rng.below(5) as usize],
        ),
        _ => {
            let x = f64::from_bits(
                1.0f64
                    .to_bits()
                    .wrapping_add(rng.below(4096))
                    .wrapping_sub(2048),
            );
            (x, rng.double(-20, 1100))
        }
    }
}

fn samples() -> usize {
    std::env::var("XLCALC_POW_SAMPLES")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(20_000)
}

fn same(a: (f64, Errno), b: (f64, Errno)) -> bool {
    a.0.to_bits() == b.0.to_bits() && a.1 == b.1
}

fn run(dll: &Ucrt, per_class: usize) -> Vec<String> {
    let mut found = Vec::new();
    let mut check = |x: f64, y: f64| {
        let (ours, theirs) = (pow(x, y), dll.call(x, y));
        if !same(ours, theirs) && found.len() < 30 {
            found.push(format!(
                "pow({x:e} [{:#x}], {y:e} [{:#x}]): ours {:e} [{:#x}] {:?}, ucrtbase {:e} [{:#x}] {:?}",
                x.to_bits(),
                y.to_bits(),
                ours.0,
                ours.0.to_bits(),
                ours.1,
                theirs.0,
                theirs.0.to_bits(),
                theirs.1
            ));
        }
    };
    let specials: Vec<f64> = SPECIAL
        .iter()
        .copied()
        .chain(NANS.map(f64::from_bits))
        .collect();
    for &x in &specials {
        for &y in &specials {
            check(x, y);
        }
    }
    let mut rng = Rng(0x0F0E_D1A7);
    for c in 0..9 {
        for _ in 0..per_class {
            let (x, y) = pair(&mut rng, c);
            check(x, y);
            check(-x, y);
        }
    }
    found
}

fn assert_fma(dll: &Ucrt) {
    assert_eq!(
        (dll.get_fma)(),
        1,
        "ucrtbase does not use its FMA3 build on this CPU: the port models that build only"
    );
}

#[test]
fn pow_matches_ucrtbase() {
    let dll = load();
    assert_fma(&dll);
    let found = run(&dll, samples());
    assert!(found.is_empty(), "{}", found.join("\n"));
}

#[test]
#[ignore = "36 million pairs: a few seconds in release, minutes in debug"]
fn pow_bulk() {
    let dll = load();
    assert_fma(&dll);
    let found = run(&dll, 2_000_000);
    assert!(found.is_empty(), "{}", found.join("\n"));

    let mut rng = Rng(77);
    let n = 2_000_000;
    let mut fma = Vec::with_capacity(n);
    let pairs: Vec<(f64, f64)> = (0..n).map(|i| pair(&mut rng, i as u32 % 9)).collect();
    for &(x, y) in &pairs {
        fma.push(dll.call(x, y));
    }
    (dll.set_fma)(0);
    let differ = pairs
        .iter()
        .zip(&fma)
        .filter(|(&(x, y), &f)| !same(dll.call(x, y), f))
        .count();
    (dll.set_fma)(1);
    eprintln!("ucrtbase pow, SSE2 build against FMA3 build: {differ} of {n} results differ");
}
