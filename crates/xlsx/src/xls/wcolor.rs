use crate::xml::Node;

fn scheme_index(v: &str) -> Option<usize> {
    Some(match v {
        "lt1" | "bg1" => 0,
        "dk1" | "tx1" => 1,
        "lt2" | "bg2" => 2,
        "dk2" | "tx2" => 3,
        "accent1" => 4,
        "accent2" => 5,
        "accent3" => 6,
        "accent4" => 7,
        "accent5" => 8,
        "accent6" => 9,
        "hlink" => 10,
        "folHlink" => 11,
        _ => return None,
    })
}

fn to_linear(c: f64) -> f64 {
    if c <= 0.04045 {
        c / 12.92
    } else {
        ((c + 0.055) / 1.055).powf(2.4)
    }
}

fn from_linear(c: f64) -> f64 {
    if c <= 0.003_130_8 {
        c * 12.92
    } else {
        1.055 * c.powf(1.0 / 2.4) - 0.055
    }
}

fn to_hsl(r: f64, g: f64, b: f64) -> (f64, f64, f64) {
    let (mx, mn) = (r.max(g).max(b), r.min(g).min(b));
    let l = (mx + mn) / 2.0;
    let d = mx - mn;
    if d == 0.0 {
        return (0.0, 0.0, l);
    }
    let s = if l > 0.5 {
        d / (2.0 - mx - mn)
    } else {
        d / (mx + mn)
    };
    let h = if mx == r {
        (g - b) / d + if g < b { 6.0 } else { 0.0 }
    } else if mx == g {
        (b - r) / d + 2.0
    } else {
        (r - g) / d + 4.0
    };
    (h / 6.0, s, l)
}

fn from_hsl(h: f64, s: f64, l: f64) -> (f64, f64, f64) {
    if s == 0.0 {
        return (l, l, l);
    }
    let q = if l < 0.5 {
        l * (1.0 + s)
    } else {
        l + s - l * s
    };
    let p = 2.0 * l - q;
    let f = |mut t: f64| {
        t = t.rem_euclid(1.0);
        if t < 1.0 / 6.0 {
            p + (q - p) * 6.0 * t
        } else if t < 0.5 {
            q
        } else if t < 2.0 / 3.0 {
            p + (q - p) * (2.0 / 3.0 - t) * 6.0
        } else {
            p
        }
    };
    (f(h + 1.0 / 3.0), f(h), f(h - 1.0 / 3.0))
}

pub(crate) fn resolve(c: &Node, theme: &[u32]) -> Option<u32> {
    let base = match c.name.as_str() {
        "srgbClr" => u32::from_str_radix(c.attr("val")?, 16).ok()?,
        "schemeClr" => *theme.get(scheme_index(c.attr("val")?)?)?,
        "sysClr" => u32::from_str_radix(c.attr("lastClr")?, 16).ok()?,
        "prstClr" => match c.attr("val")? {
            "black" => 0,
            "white" => 0xFFFFFF,
            "red" => 0xFF0000,
            "green" => 0x008000,
            "blue" => 0x0000FF,
            "yellow" => 0xFFFF00,
            _ => return None,
        },
        _ => return None,
    };
    let mut rgb = [
        f64::from(base >> 16 & 0xFF) / 255.0,
        f64::from(base >> 8 & 0xFF) / 255.0,
        f64::from(base & 0xFF) / 255.0,
    ];
    for m in &c.kids {
        let v = m.attr("val").and_then(|v| v.parse::<f64>().ok());
        let Some(v) = v.map(|v| v / 100_000.0) else {
            continue;
        };
        match m.name.as_str() {
            "tint" => rgb = rgb.map(|c| from_linear(to_linear(c) * v + (1.0 - v))),
            "shade" => rgb = rgb.map(|c| from_linear(to_linear(c) * v)),
            "lumMod" | "lumOff" | "satMod" | "satOff" | "hueOff" => {
                let (mut h, mut s, mut l) = to_hsl(rgb[0], rgb[1], rgb[2]);
                match m.name.as_str() {
                    "lumMod" => l *= v,
                    "lumOff" => l += v,
                    "satMod" => s *= v,
                    "satOff" => s += v,
                    _ => h += v * 100_000.0 / 21_600_000.0,
                }
                let (r, g, b) = from_hsl(h, s.clamp(0.0, 1.0), l.clamp(0.0, 1.0));
                rgb = [r, g, b];
            }
            _ => {}
        }
    }
    let q = |c: f64| (c.clamp(0.0, 1.0) * 255.0).round() as u32;
    Some(q(rgb[0]) << 16 | q(rgb[1]) << 8 | q(rgb[2]))
}
