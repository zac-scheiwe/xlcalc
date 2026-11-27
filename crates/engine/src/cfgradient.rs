use crate::x87;

fn positions(n: usize) -> [f64; 3] {
    if n == 2 {
        [0.0, 1.0, 0.0]
    } else {
        [0.0, 0.5, 1.0]
    }
}

fn divisor_ok(d: f64) -> bool {
    let bits = d.to_bits();
    bits & 0xffff_0000_0000_0000 != 0 && bits & 0x7ff0_0000_0000_0000 != 0x7ff0_0000_0000_0000
}

fn rgb_interp(c1: u32, c2: u32, t: f64) -> u32 {
    let mut out = 0u32;
    for shift in [16u32, 8, 0] {
        let a = ((c1 >> shift) & 255) as i32;
        let b = ((c2 >> shift) & 255) as i32;
        let step = x87::mul(f64::from(b - a), t).trunc() as i32;
        out |= ((a + step) as u32 & 255) << shift;
    }
    out
}

pub fn scale_color(domain: &[f64], colors: &[u32], v: f64) -> u32 {
    let n = colors.len();
    let pos = positions(n);
    let mut d = domain.to_vec();
    d.sort_by(f64::total_cmp);
    let (lo, hi) = (d[0], d[n - 1]);

    let at = if v >= hi {
        pos[n - 1]
    } else if v <= lo {
        pos[0]
    } else {
        let k = if n == 3 && v >= d[1] { 1 } else { 0 };
        let den = x87::sub(d[k + 1], d[k]);
        if !divisor_ok(den) {
            pos[k]
        } else {
            let r = x87::div(x87::sub(v, d[k]), den);
            let step = x87::mul(x87::sub(pos[k + 1], pos[k]), r);
            if k == 0 {
                step
            } else {
                x87::add(step, pos[k])
            }
        }
    };

    let k = if at < pos[1] { 0 } else { 1 };
    let start = colors[k];
    let end = colors.get(k + 1).copied().unwrap_or(start);
    let den = x87::sub(pos[k + 1], pos[k]);
    if !divisor_ok(den) {
        return start;
    }
    rgb_interp(start, end, x87::div(x87::sub(at, pos[k]), den))
}

#[cfg(test)]
mod tests {
    use super::*;

    const RED: u32 = 0xF8696B;
    const YELLOW: u32 = 0xFFEB84;
    const GREEN: u32 = 0x63BE7B;

    #[test]
    fn ends_and_middle_take_their_colours() {
        let d = [1.0, 5.0, 9.0];
        let c = [RED, YELLOW, GREEN];
        assert_eq!(scale_color(&d, &c, 1.0), RED);
        assert_eq!(scale_color(&d, &c, 0.0), RED);
        assert_eq!(scale_color(&d, &c, 5.0), YELLOW);
        assert_eq!(scale_color(&d, &c, 9.0), GREEN);
        assert_eq!(scale_color(&d, &c, 12.0), GREEN);
    }

    #[test]
    fn thresholds_sort_by_value_and_colours_stay() {
        let c = [RED, YELLOW, GREEN];
        assert_eq!(
            scale_color(&[9.0, 5.0, 1.0], &c, 3.0),
            scale_color(&[1.0, 5.0, 9.0], &c, 3.0)
        );
    }

    #[test]
    fn two_colour_halfway_truncates_each_channel() {
        let got = scale_color(&[0.0, 10.0], &[0xFF7128, 0xFFEF9C], 5.0);
        assert_eq!(got, 0xFFB062);
    }

    #[test]
    fn equal_thresholds_leave_the_start_colour() {
        let c = [RED, GREEN];
        assert_eq!(scale_color(&[3.0, 3.0], &c, 3.0), GREEN);
        assert_eq!(scale_color(&[3.0, 3.0], &c, 2.0), RED);
    }
}
