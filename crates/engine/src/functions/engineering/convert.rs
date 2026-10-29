use super::atp::{self, R};
use crate::eval::{EvalCtx, Operand};
use crate::functions::args::val;
use crate::value::{Value, XlError};
use crate::x87;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Cat {
    Mass,
    Length,
    Time,
    Pressure,
    Force,
    Energy,
    Power,
    Magnetism,
    Temperature,
    Volume,
    Area,
    Information,
    Speed,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Temp {
    C,
    F,
    K,
    Rank,
    Reau,
}

struct Unit {
    name: &'static str,
    cat: Cat,
    factor: u64,

    prefix: u8,

    binary: bool,
}

const fn u(name: &'static str, cat: Cat, factor: u64) -> Unit {
    Unit {
        name,
        cat,
        factor,
        prefix: 0,
        binary: false,
    }
}

const fn up(name: &'static str, cat: Cat, factor: u64, p: u8) -> Unit {
    Unit {
        name,
        cat,
        factor,
        prefix: p,
        binary: false,
    }
}

const ONE: u64 = 0x3FF0000000000000;

use Cat::*;

static UNITS: &[Unit] = &[
    up("g", Mass, ONE, 1),
    u("sg", Mass, 0x40CC80F3937245D6),
    u("lbm", Mass, 0x407C597A58F7121B),
    up("u", Mass, 0x3B000F4B7DEA07E2, 1),
    u("ozm", Mass, 0x403C597A58F7121B),
    u("grain", Mass, 0x3FB096A94F4435E5),
    u("cwt", Mass, 0x40E625E795810625),
    u("shweight", Mass, 0x40E625E795810625),
    u("uk_cwt", Mass, 0x40E8CE4B0DD82FD7),
    u("lcwt", Mass, 0x40E8CE4B0DD82FD7),
    u("hweight", Mass, 0x40E8CE4B0DD82FD7),
    u("stone", Mass, 0x40B8CE4B0DD82FD7),
    u("ton", Mass, 0x412BAF617AE147AE),
    u("uk_ton", Mass, 0x412F01DDD14E3BCD),
    u("LTON", Mass, 0x412F01DDD14E3BCD),
    u("brton", Mass, 0x412F01DDD14E3BCD),
    up("m", Length, 0x4202A05F20000000, 1),
    u("mi", Length, 0x42AD46178C800000),
    u("Nmi", Length, 0x42B0D80605700000),
    u("in", Length, 0x41AE477700000000),
    u("ft", Length, 0x41E6B59940000000),
    u("yd", Length, 0x42010832F0000000),
    up("ang", Length, ONE, 1),
    u("ell", Length, 0x42054A3FAC000000),
    up("ly", Length, 0x45539078110374D3, 1),
    up("parsec", Length, 0x456FE7B875F45676, 1),
    up("pc", Length, 0x456FE7B875F45676, 1),
    u("Picapt", Length, 0x414AEA30E38E38E4),
    u("Pica", Length, 0x414AEA30E38E38E4),
    u("pica", Length, 0x41842FA4AAAAAAAB),
    u("survey_mi", Length, 0x42AD461B62C480BF),
    u("yr", Time, 0x417E187E00000000),
    u("day", Time, 0x40F5180000000000),
    u("d", Time, 0x40F5180000000000),
    u("hr", Time, 0x40AC200000000000),
    u("mn", Time, 0x404E000000000000),
    u("min", Time, 0x404E000000000000),
    up("sec", Time, ONE, 1),
    up("s", Time, ONE, 1),
    up("Pa", Pressure, ONE, 1),
    up("p", Pressure, ONE, 1),
    up("atm", Pressure, 0x40F8BCD000000000, 1),
    up("at", Pressure, 0x40F8BCD000000000, 1),
    up("mmHg", Pressure, 0x4060AA4DD2F1A9FC, 1),
    u("psi", Pressure, 0x40BAEEC1DDF70F99),
    u("Torr", Pressure, 0x4060AA50D79435E5),
    up("N", Force, 0x40F86A0000000000, 1),
    up("dyn", Force, ONE, 1),
    up("dy", Force, ONE, 1),
    u("lbf", Force, 0x411B2658A56715B9),
    up("pond", Force, 0x408EA551EB851EB8, 1),
    up("J", Energy, 0x416312D000000000, 1),
    up("e", Energy, ONE, 1),
    up("c", Energy, 0x4183F36C00000000, 1),
    up("cal", Energy, 0x4183F6D700000000, 1),
    up("eV", Energy, 0x3D7C2F8FB9DD9C32, 1),
    up("ev", Energy, 0x3D7C2F8FB9DD9C32, 1),
    u("HPh", Energy, 0x42B86A62559941BB),
    u("hh", Energy, 0x42B86A62559941BB),
    up("Wh", Energy, 0x4220C388D0000000, 1),
    up("wh", Energy, 0x4220C388D0000000, 1),
    u("flb", Energy, 0x4169DC346F774EEE),
    u("BTU", Energy, 0x4203A6E5F9F1999A),
    u("btu", Energy, 0x4203A6E5F9F1999A),
    u("HP", Power, 0x40874D995645AA01),
    u("h", Power, 0x40874D995645AA01),
    u("PS", Power, 0x4086FBFD70A3D70A),
    up("W", Power, ONE, 1),
    up("w", Power, ONE, 1),
    up("T", Magnetism, 0x40C3880000000000, 1),
    up("ga", Magnetism, ONE, 1),
    u("C", Temperature, ONE),
    u("cel", Temperature, ONE),
    u("F", Temperature, ONE),
    u("fah", Temperature, ONE),
    up("K", Temperature, ONE, 1),
    up("kel", Temperature, ONE, 1),
    u("Rank", Temperature, ONE),
    u("Reau", Temperature, ONE),
    u("tsp", Volume, 0x3F74305950CB96E4),
    u("tspm", Volume, 0x3F747AE147AE147B),
    u("tbs", Volume, 0x3F8E4885F9316256),
    u("oz", Volume, 0x3F9E4885F9316256),
    u("cup", Volume, 0x3FCE4885F9316256),
    u("pt", Volume, 0x3FDE4885F9316256),
    u("us_pt", Volume, 0x3FDE4885F9316256),
    up("uk_pt", Volume, 0x3FE22F32378AB0C9, 1),
    u("qt", Volume, 0x3FEE4885F9316256),
    u("uk_qt", Volume, 0x3FF22F32378AB0C9),
    u("gal", Volume, 0x400E4885F9316256),
    u("uk_gal", Volume, 0x40122F32378AB0C9),
    up("l", Volume, ONE, 1),
    up("L", Volume, ONE, 1),
    up("lt", Volume, ONE, 1),
    up("ang3", Volume, 0x3A53CE9A36F23C10, 3),
    up("ang^3", Volume, 0x3A53CE9A36F23C10, 3),
    u("barrel", Volume, 0x4063DF97EB886889),
    u("bushel", Volume, 0x40419E99D9EA1974),
    u("ft3", Volume, 0x403C511CDBB67D33),
    u("ft^3", Volume, 0x403C511CDBB67D33),
    u("in3", Volume, 0x3F90C7C53FD4701E),
    u("in^3", Volume, 0x3F90C7C53FD4701E),
    u("ly3", Volume, 0x4A821B28DA4DB07C),
    u("ly^3", Volume, 0x4A821B28DA4DB07C),
    up("m3", Volume, 0x408F400000000000, 3),
    up("m^3", Volume, 0x408F400000000000, 3),
    u("mi3", Volume, 0x428E53D7F9FD04A3),
    u("mi^3", Volume, 0x428E53D7F9FD04A3),
    u("yd3", Volume, 0x4087E4705961F9A3),
    u("yd^3", Volume, 0x4087E4705961F9A3),
    u("Nmi3", Volume, 0x42971BEE41980000),
    u("Nmi^3", Volume, 0x42971BEE41980000),
    u("Picapt3", Volume, 0x3E67921D2555F2D3),
    u("Picapt^3", Volume, 0x3E67921D2555F2D3),
    u("Pica3", Volume, 0x3E67921D2555F2D3),
    u("Pica^3", Volume, 0x3E67921D2555F2D3),
    u("GRT", Volume, 0x40A61F5E8BA691D0),
    u("regton", Volume, 0x40A61F5E8BA691D0),
    u("MTON", Volume, 0x4091B2B209520E40),
    u("uk_acre", Area, 0x44D56C835E3336B4),
    u("us_acre", Area, 0x44D56C88FBEFC3EF),
    up("ang2", Area, ONE, 2),
    up("ang^2", Area, ONE, 2),
    up("ar", Area, 0x4480F0CF064DD592, 1),
    u("ft2", Area, 0x43E01DB94881EC80),
    u("ft^2", Area, 0x43E01DB94881EC80),
    u("ha", Area, 0x44EA784379D99DB4),
    u("in2", Area, 0x436CA69EB9CA8800),
    u("in^2", Area, 0x436CA69EB9CA8800),
    u("ly2", Area, 0x4AB7EC359D1F72EF),
    u("ly^2", Area, 0x4AB7EC359D1F72EF),
    up("m2", Area, 0x4415AF1D78B58C40, 2),
    up("m^2", Area, 0x4415AF1D78B58C40, 2),
    u("Morgen", Area, 0x44CA784379D99DB4),
    u("mi2", Area, 0x456AC7A435C00461),
    u("mi^2", Area, 0x456AC7A435C00461),
    u("Nmi2", Area, 0x4571BB70AD751416),
    u("Nmi^2", Area, 0x4571BB70AD751416),
    u("Picapt2", Area, 0x42A6A3415D11CD6E),
    u("Pica2", Area, 0x42A6A3415D11CD6E),
    u("Pica^2", Area, 0x42A6A3415D11CD6E),
    u("Picapt^2", Area, 0x42A6A3415D11CD6E),
    u("yd2", Area, 0x4412217071922A10),
    u("yd^2", Area, 0x4412217071922A10),
    Unit {
        name: "bit",
        cat: Information,
        factor: ONE,
        prefix: 1,
        binary: true,
    },
    Unit {
        name: "byte",
        cat: Information,
        factor: 0x4020000000000000,
        prefix: 1,
        binary: true,
    },
    u("admkn", Speed, 0x3FE07905ECF0A058),
    u("kn", Speed, 0x3FE07654320FEDCC),
    up("m/h", Speed, 0x3F323456789ABCDF, 1),
    up("m/hr", Speed, 0x3F323456789ABCDF, 1),
    up("m/s", Speed, ONE, 1),
    up("m/sec", Speed, ONE, 1),
    up("mph", Speed, 0x3FDC9C4DA9003EEA, 1),
];

const DECIMAL_PREFIXES: &[(&str, i32)] = &[
    ("Y", 24),
    ("Z", 21),
    ("E", 18),
    ("P", 15),
    ("T", 12),
    ("G", 9),
    ("M", 6),
    ("k", 3),
    ("h", 2),
    ("da", 1),
    ("e", 1),
    ("d", -1),
    ("c", -2),
    ("m", -3),
    ("u", -6),
    ("n", -9),
    ("p", -12),
    ("f", -15),
    ("a", -18),
    ("z", -21),
    ("y", -24),
];

const BINARY_PREFIXES: &[(&str, i32)] = &[
    ("Yi", 80),
    ("Zi", 70),
    ("Ei", 60),
    ("Pi", 50),
    ("Ti", 40),
    ("Gi", 30),
    ("Mi", 20),
    ("ki", 10),
];

struct Parsed {
    unit: &'static Unit,

    dec: i32,

    bin: i32,
}

fn exact(name: &str) -> Option<&'static Unit> {
    UNITS.iter().find(|u| u.name == name)
}

fn lookup(name: &str) -> Option<Parsed> {
    if let Some(unit) = exact(name) {
        return Some(Parsed {
            unit,
            dec: 0,
            bin: 0,
        });
    }
    for (p, e) in BINARY_PREFIXES {
        if let Some(unit) = name.strip_prefix(p).and_then(exact) {
            if unit.binary {
                return Some(Parsed {
                    unit,
                    dec: 0,
                    bin: *e,
                });
            }
        }
    }
    for (p, e) in DECIMAL_PREFIXES {
        if let Some(unit) = name.strip_prefix(p).and_then(exact) {
            if unit.prefix > 0 {
                return Some(Parsed {
                    unit,
                    dec: e * unit.prefix as i32,
                    bin: 0,
                });
            }
        }
    }
    None
}

fn pow10(e: i32) -> f64 {
    format!("1e{e}").parse().unwrap()
}

fn scale10(x: f64, e: i32) -> f64 {
    if e >= 0 {
        x * pow10(e)
    } else {
        x / pow10(-e)
    }
}

fn temp_kind(u: &Unit) -> Temp {
    match u.name {
        "C" | "cel" => Temp::C,
        "F" | "fah" => Temp::F,
        "K" | "kel" => Temp::K,
        "Rank" => Temp::Rank,
        _ => Temp::Reau,
    }
}

fn temperature(x: f64, a: Temp, b: Temp) -> f64 {
    use crate::x87::{add, div, mul, sub};
    use Temp::*;
    match (a, b) {
        (C, F) => add(mul(x, 1.8), 32.0),
        (C, K) => add(273.15, x),
        (C, Rank) => mul(add(x, 273.15), 1.8),
        (C, Reau) => mul(x, 0.8),
        (F, C) => div(sub(x, 32.0), 1.8),
        (F, K) => add(div(sub(x, 32.0), 1.8), 273.15),
        (F, Rank) => add(x, 459.67),
        (F, Reau) => div(mul(sub(x, 32.0), 0.8), 1.8),
        (K, C) => sub(x, 273.15),
        (K, F) => add(mul(sub(x, 273.15), 1.8), 32.0),
        (K, Rank) => mul(x, 1.8),
        (K, Reau) => mul(sub(x, 273.15), 0.8),
        (Rank, C) => sub(div(x, 1.8), 273.15),
        (Rank, F) => sub(x, 459.67),
        (Rank, K) => div(x, 1.8),
        (Rank, Reau) => mul(sub(div(x, 1.8), 273.15), 0.8),
        (Reau, C) => div(x, 0.8),
        (Reau, F) => add(div(mul(x, 1.8), 0.8), 32.0),
        (Reau, K) => add(div(x, 0.8), 273.15),
        (Reau, Rank) => mul(add(div(x, 0.8), 273.15), 1.8),
        _ => x,
    }
}

fn convert_value(x: f64, from: &Parsed, to: &Parsed) -> R<f64> {
    if from.unit.cat != to.unit.cat {
        return Err(XlError::NA);
    }
    let r = if from.unit.cat == Cat::Temperature && temp_kind(from.unit) != temp_kind(to.unit) {
        let (a, b) = (temp_kind(from.unit), temp_kind(to.unit));

        let mut x = x;
        if from.dec != 0 && matches!(b, Temp::C | Temp::F | Temp::K) {
            x = scale10(x, from.dec);
        }
        let mut r = temperature(x, a, b);
        if to.dec != 0 && matches!(a, Temp::C | Temp::F | Temp::K | Temp::Rank) {
            r /= pow10(to.dec);
        }
        r
    } else {
        let fa = f64::from_bits(from.unit.factor);
        let fb = f64::from_bits(to.unit.factor);

        let m = x87::mul(x, fa);
        let mut r = if m.is_infinite() && x.is_finite() {
            x87::mul(x87::div(x, fb), fa)
        } else {
            x87::div(m, fb)
        };
        if (from.bin != 0 || to.bin != 0) && to.unit.name == "bit" {
            if from.dec != 0 {
                r = x87::mul(r, pow10(from.dec));
            }
            if to.dec != 0 {
                r = x87::div(r, pow10(to.dec));
            }
        } else if from.dec != to.dec {
            r *= pow10(from.dec - to.dec);
        }
        if from.bin != to.bin {
            r *= 2f64.powi(from.bin - to.bin);
        }
        r
    };
    if r.is_finite() {
        Ok(if r.abs() < f64::MIN_POSITIVE { 0.0 } else { r })
    } else {
        Err(XlError::NUM)
    }
}

pub fn convert(ctx: &mut EvalCtx, ops: &[Operand]) -> Operand {
    atp::call(ctx, ops, "a", |ctx, a, _| {
        val((|| {
            let x = atp::req_num(ctx, a, 0)?;
            let from = atp::req_text(a, 1)?;
            let to = atp::req_text(a, 2)?;
            let from = lookup(&from).ok_or(XlError::NA)?;
            let to = lookup(&to).ok_or(XlError::NA)?;
            convert_value(x, &from, &to).map(Value::num)
        })())
    })
}
