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
    up("SI_at", Pressure, 0x40F7F12828F5C28F, 1),
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

const DECIMAL_PREFIXES: &[(char, i32)] = &[
    ('Y', 24),
    ('Z', 21),
    ('E', 18),
    ('P', 15),
    ('T', 12),
    ('G', 9),
    ('M', 6),
    ('k', 3),
    ('h', 2),
    ('e', 1),
    ('d', -1),
    ('c', -2),
    ('m', -3),
    ('u', -6),
    ('n', -9),
    ('p', -12),
    ('f', -15),
    ('a', -18),
    ('z', -21),
    ('y', -24),
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

    bin: Option<f64>,
}

fn exact(name: &str) -> Option<&'static Unit> {
    UNITS.iter().find(|u| u.name == name)
}

fn decimal(name: &str, e: i32) -> Option<Parsed> {
    let unit = exact(name).filter(|u| u.prefix > 0)?;
    Some(Parsed {
        unit,
        dec: e * i32::from(unit.prefix),
        bin: None,
    })
}

fn prefixed(name: &str) -> Option<Parsed> {
    let one = DECIMAL_PREFIXES
        .iter()
        .find_map(|&(p, e)| name.strip_prefix(p).and_then(|rest| decimal(rest, e)));
    if one.is_some() {
        return one;
    }
    if let Some(rest) = name.strip_prefix("da") {
        return decimal(rest, 1);
    }
    let (rest, k) = BINARY_PREFIXES
        .iter()
        .find_map(|&(p, k)| name.strip_prefix(p).map(|rest| (rest, k)))?;
    let unit = exact(rest).filter(|u| u.binary)?;
    Some(Parsed {
        unit,
        dec: 0,
        bin: Some(2f64.powi(k)),
    })
}

fn lookup(name: &str) -> Option<Parsed> {
    match exact(name) {
        Some(unit) => Some(Parsed {
            unit,
            dec: 0,
            bin: None,
        }),
        None => prefixed(name),
    }
}

fn reread_pc(parsed: Parsed, name: &str, other: Cat) -> Option<Parsed> {
    if parsed.unit.cat == Cat::Length && other == Cat::Energy && name == "pc" {
        prefixed(name)
    } else {
        Some(parsed)
    }
}

fn parse(from: &str, to: &str) -> Option<(Parsed, Parsed)> {
    let a = lookup(from)?;
    let b = lookup(to)?;
    let a = reread_pc(a, from, b.unit.cat)?;
    let b = reread_pc(b, to, a.unit.cat)?;
    Some((a, b))
}

fn tento(e: i32) -> f64 {
    crate::number::pack_decimal(1, i64::from(e))
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

fn temperature(x: f64, from: &Parsed, to: &Parsed) -> f64 {
    use crate::x87::{add, div, mul, sub};
    use Temp::*;
    let (f0, t0) = (tento(from.dec), tento(to.dec));

    let v = || mul(mul(f0, 1.0), x);
    match (temp_kind(from.unit), temp_kind(to.unit)) {
        (C, F) => add(mul(x, 1.8), 32.0),
        (C, K) => div(add(273.15, x), t0),
        (C, Rank) => mul(add(x, 273.15), 1.8),
        (C, Reau) => mul(x, 0.8),
        (F, C) => div(sub(x, 32.0), 1.8),
        (F, K) => div(add(div(sub(x, 32.0), 1.8), 273.15), t0),
        (F, Rank) => add(x, 459.67),
        (F, Reau) => div(mul(sub(x, 32.0), 0.8), 1.8),
        (K, C) => sub(v(), 273.15),
        (K, F) => add(mul(sub(v(), 273.15), 1.8), 32.0),
        (K, K) => div(v(), mul(t0, 1.0)),
        (K, Rank) => mul(x, 1.8),
        (K, Reau) => mul(sub(x, 273.15), 0.8),
        (Rank, C) => sub(div(x, 1.8), 273.15),
        (Rank, F) => sub(x, 459.67),
        (Rank, K) => div(div(x, 1.8), t0),
        (Rank, Reau) => mul(sub(div(x, 1.8), 273.15), 0.8),
        (Reau, C) => div(x, 0.8),
        (Reau, F) => add(div(mul(x, 1.8), 0.8), 32.0),
        (Reau, K) => add(div(x, 0.8), 273.15),
        (Reau, Rank) => mul(add(div(x, 0.8), 273.15), 1.8),
        (C, C) | (F, F) | (Rank, Rank) | (Reau, Reau) => x,
    }
}

fn convert_value(x: f64, from: &Parsed, to: &Parsed) -> R<f64> {
    if from.unit.cat != to.unit.cat {
        return Err(XlError::NA);
    }
    let r = if from.unit.cat == Cat::Temperature {
        temperature(x, from, to)
    } else {
        let scale = match (from.bin, to.bin) {
            (Some(a), Some(b)) => x87::div(a, b),
            (None, Some(b)) => x87::div(tento(from.dec), b),
            (Some(a), None) => x87::div(a, tento(to.dec)),
            (None, None) => tento(from.dec - to.dec),
        };
        let fa = f64::from_bits(from.unit.factor);
        let fb = f64::from_bits(to.unit.factor);

        let m = x87::mul(x, fa);
        let q = if m.is_infinite() && x.is_finite() {
            x87::mul(x87::div(x, fb), fa)
        } else {
            x87::div(m, fb)
        };
        x87::mul(q, scale)
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
            if from.chars().count() > 255 {
                return Err(XlError::VALUE);
            }
            let to = atp::req_text(a, 2)?;
            if to.chars().count() > 255 {
                return Err(XlError::VALUE);
            }
            let (a, b) = parse(&from, &to).ok_or(XlError::NA)?;
            convert_value(x, &a, &b).map(Value::num)
        })())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn conv(from: &str, to: &str) -> Option<R<f64>> {
        parse(from, to).map(|(a, b)| convert_value(1.0, &a, &b))
    }

    #[test]
    fn prefixes_and_second_reading() {
        assert!(matches!(conv("pc", "J"), Some(Ok(v)) if v > 0.0 && v < 1e-11));
        assert_eq!(conv("pc", "W"), Some(Err(XlError::NA)));
        assert_eq!(conv("pc", "pc"), Some(Ok(1.0)));
        assert_eq!(conv("dam", "m"), Some(Ok(10.0)));
        assert_eq!(conv("kibyte", "byte"), Some(Ok(1024.0)));
        assert!(conv("kin", "bit").is_none());

        let r = conv("Zm3", "zm3").unwrap().unwrap();
        assert_eq!(r.to_bits(), 1e126f64.to_bits() + 1);
    }
}
