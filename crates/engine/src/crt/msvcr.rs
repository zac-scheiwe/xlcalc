use std::ffi::c_void;

const DLL: &str = r"C:\Program Files\Microsoft Office\root\vfs\System\msvcr100.dll";

extern "system" {
    fn LoadLibraryW(name: *const u16) -> *mut c_void;
    fn GetProcAddress(module: *mut c_void, name: *const u8) -> *mut c_void;
}

type F1 = extern "C" fn(f64) -> f64;
type F2 = extern "C" fn(f64, f64) -> f64;

type Gen<'a> = dyn Fn(&mut Rng) -> f64 + 'a;

type PairGen<'a> = dyn Fn(&mut Rng) -> (f64, f64) + 'a;

struct Crt {
    exp: F1,
    log: F1,
    sqrt: F1,
    atan: F1,
    sin: F1,
    cos: F1,
    pow: F2,
}

fn load() -> Option<Crt> {
    if !std::path::Path::new(DLL).exists() {
        eprintln!("msvcr100.dll not found at {DLL}: skipping the differential test");
        return None;
    }
    let wide: Vec<u16> = DLL.encode_utf16().chain([0]).collect();

    let module = unsafe { LoadLibraryW(wide.as_ptr()) };
    assert!(!module.is_null(), "LoadLibraryW({DLL}) failed");
    let sym = |name: &str| {
        let c: Vec<u8> = name.bytes().chain([0]).collect();

        let p = unsafe { GetProcAddress(module, c.as_ptr()) };
        assert!(!p.is_null(), "msvcr100.dll has no {name}");
        p
    };

    unsafe {
        Some(Crt {
            exp: std::mem::transmute::<*mut c_void, F1>(sym("exp")),
            log: std::mem::transmute::<*mut c_void, F1>(sym("log")),
            sqrt: std::mem::transmute::<*mut c_void, F1>(sym("sqrt")),
            atan: std::mem::transmute::<*mut c_void, F1>(sym("atan")),
            sin: std::mem::transmute::<*mut c_void, F1>(sym("sin")),
            cos: std::mem::transmute::<*mut c_void, F1>(sym("cos")),
            pow: std::mem::transmute::<*mut c_void, F2>(sym("pow")),
        })
    }
}

struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 >> 12;
        self.0 ^= self.0 << 25;
        self.0 ^= self.0 >> 27;
        self.0.wrapping_mul(0x2545_f491_4f6c_dd1d)
    }

    fn unit(&mut self) -> f64 {
        (self.next() >> 11) as f64 * f64::powi(2.0, -53)
    }

    fn range(&mut self, lo: f64, hi: f64) -> f64 {
        lo + (hi - lo) * self.unit()
    }

    fn bits(&mut self) -> f64 {
        f64::from_bits(self.next())
    }

    fn magnitude(&mut self, lo: i32, hi: i32) -> f64 {
        let s = if self.next() & 1 == 0 { 1.0 } else { -1.0 };
        s * f64::powf(2.0, self.range(f64::from(lo), f64::from(hi)))
    }
    fn pick(&mut self, xs: &[f64]) -> f64 {
        xs[(self.next() % xs.len() as u64) as usize]
    }
}

fn specials() -> Vec<f64> {
    let mut v = vec![
        0.0,
        -0.0,
        f64::MIN_POSITIVE,
        -f64::MIN_POSITIVE,
        f64::from_bits(1),
        f64::from_bits(0x8000_0000_0000_0001),
        f64::from_bits(0x000f_ffff_ffff_ffff),
        f64::MAX,
        f64::MIN,
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::NAN,
        -f64::NAN,
        f64::from_bits(0x7ff0_0000_0000_0001),
        f64::from_bits(0xfff4_0000_0000_0000),
        1.0,
        -1.0,
        0.5,
        2.0,
        std::f64::consts::E,
        std::f64::consts::PI,
        std::f64::consts::FRAC_PI_2,
        std::f64::consts::FRAC_PI_4,
        -std::f64::consts::FRAC_PI_4,
        709.782712893384,
        709.7827128933841,
        -708.3964185322641,
        -744.4400719213812,
        -745.1332191019411,
        -745.1332191019412,
        -744.0346068132731,
        500000.0,
        499999.99999999994,
        1e22,
        1e300,
        6381956970095103.0 * f64::powi(2.0, 797),
        5.319372648326541e255,
        2.4375,
        1.1875,
        0.6875,
        0.4375,
        4.86e18,
        0.0625,
        1.0625,
        0.9375,
        1.125,
        0.875,
    ];
    for e in [
        -1074, -1022, -60, -33, -27, -13, -1, 1, 19, 52, 62, 63, 64, 1023,
    ] {
        let p = f64::powi(2.0, e);
        for x in [
            p,
            -p,
            p * (1.0 + f64::EPSILON),
            p * (1.0 - f64::EPSILON / 2.0),
        ] {
            v.push(x);
        }
    }
    v
}

fn sample_count(default: usize) -> usize {
    std::env::var("XLCALC_CRT_SAMPLES")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(default)
}

fn check1(name: &str, ours: fn(f64) -> f64, theirs: F1, n: usize, rng: &mut Rng) -> Vec<String> {
    let mut bad = Vec::new();
    let mut count = 0usize;
    let mut test = |x: f64, bad: &mut Vec<String>| {
        count += 1;
        let (a, b) = (ours(x), theirs(x));
        if a.to_bits() != b.to_bits() && bad.len() < 20 {
            bad.push(format!(
                "{name}({x:e} = {:#018x}): ours {a:e} ({:#018x}), msvcr100 {b:e} ({:#018x})",
                x.to_bits(),
                a.to_bits(),
                b.to_bits()
            ));
        }
    };
    for x in specials() {
        test(x, &mut bad);
    }
    let gens: [&Gen<'_>; 7] = [
        &|r| r.bits(),
        &|r| r.magnitude(-1074, 1024),
        &|r| r.range(-8.0, 8.0),
        &|r| r.range(-800.0, 800.0),
        &|r| r.range(0.8, 1.2),
        &|r| r.magnitude(-40, 70),
        &|r| {
            let k = (r.next() % 400_000) as f64;
            let x = k * std::f64::consts::FRAC_PI_2;
            f64::from_bits(x.to_bits().wrapping_add(r.next() % 129).wrapping_sub(64))
        },
    ];
    for g in gens {
        for _ in 0..n {
            let x = g(rng);
            test(x, &mut bad);
        }
    }
    eprintln!("{name}: {count} arguments");
    bad
}

fn check_pow(ours: fn(f64, f64) -> f64, theirs: F2, n: usize, rng: &mut Rng) -> Vec<String> {
    let mut bad = Vec::new();
    let mut count = 0usize;
    let mut test = |x: f64, y: f64, bad: &mut Vec<String>| {
        count += 1;
        let (a, b) = (ours(x, y), theirs(x, y));
        if a.to_bits() != b.to_bits() && bad.len() < 20 {
            bad.push(format!(
                "pow({x:e}, {y:e}) [{:#018x}, {:#018x}]: ours {a:e} ({:#018x}), msvcr100 {b:e} ({:#018x})",
                x.to_bits(),
                y.to_bits(),
                a.to_bits(),
                b.to_bits()
            ));
        }
    };
    let mut sp = specials();
    sp.extend([
        3.0,
        -3.0,
        1.5,
        -1.5,
        2.5,
        -2.5,
        53.0,
        54.0,
        1e15,
        4503599627370497.0,
        -2.0,
        -10.0,
        1023.0,
        1024.0,
        -1074.0,
        -1075.0,
        0.1,
        1.0 / 3.0,
    ]);
    for &x in &sp {
        for &y in &sp {
            test(x, y, &mut bad);
        }
    }
    let ints: Vec<f64> = (-40..=40).map(f64::from).collect();
    let gens: [&PairGen<'_>; 12] = [
        &|r| (r.bits(), r.bits()),
        &|r| (r.range(0.0, 10.0), r.range(-50.0, 50.0)),
        &|r| (r.magnitude(-1074, 1024).abs(), r.range(-2.0, 2.0)),
        &|r| (r.range(0.9, 1.1), r.magnitude(-20, 40)),
        &|r| (r.range(-20.0, 0.0), r.pick(&ints)),
        &|r| (r.magnitude(-60, 60), r.pick(&ints) + 0.5),
        &|r| (r.range(0.5, 2.0), r.magnitude(0, 12)),
        &|r| {
            let x = r.range(1.0001, 4.0);
            (x, r.range(-1080.0, 1030.0) / x.log2())
        },
        &|r| (r.magnitude(-1074, 1024), r.magnitude(-70, 70)),
        &|r| {
            let x = f64::from_bits(r.next() & 0x000f_ffff_ffff_ffff);
            (x, r.range(-1.5, 1.5))
        },
        &|r| {
            let y = (r.next() >> 11) as f64 * r.pick(&[1.0, 2.0, 2048.0]);
            (-r.range(0.5, 2.0), y)
        },
        &|r| {
            (
                r.pick(&[2.0, 0.5, -2.0, 4.0, -0.5, 10.0]),
                r.pick(&ints) * 27.0,
            )
        },
    ];
    for g in gens {
        for _ in 0..n {
            let (x, y) = g(rng);
            test(x, y, &mut bad);
        }
    }
    eprintln!("pow: {count} pairs");
    bad
}

fn run(n: usize) {
    let Some(crt) = load() else { return };
    let mut rng = Rng(0x9e37_79b9_7f4a_7c15);
    let mut bad = Vec::new();
    bad.extend(check1("exp", super::exp, crt.exp, n, &mut rng));
    bad.extend(check1("log", super::log, crt.log, n, &mut rng));
    bad.extend(check1("sqrt", super::sqrt, crt.sqrt, n, &mut rng));
    bad.extend(check1("atan", super::atan, crt.atan, n, &mut rng));
    bad.extend(check1("sin", super::sin, crt.sin, n, &mut rng));
    bad.extend(check1("cos", super::cos, crt.cos, n, &mut rng));
    bad.extend(check_pow(super::pow, crt.pow, n, &mut rng));

    let tan_dll = |x: f64| (crt.sin)(x) / (crt.cos)(x);
    for _ in 0..n {
        let x = rng.magnitude(-30, 30);
        if super::tan(x).to_bits() != tan_dll(x).to_bits() && bad.len() < 200 {
            bad.push(format!("tan({x:e})"));
        }
    }
    assert!(
        bad.is_empty(),
        "{} differences:\n{}",
        bad.len(),
        bad.join("\n")
    );
}

#[test]
fn crt_matches_msvcr100() {
    run(sample_count(3000));
}

#[test]
#[ignore = "slow: millions of arguments per function"]
fn crt_bulk() {
    run(sample_count(2_000_000));
}
