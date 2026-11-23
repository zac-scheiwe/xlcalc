use xlcalc::drawing::{BagVal, Chart, Part};
use xlcalc::CellAddr;
use xlcalc_variant::Variant;

use crate::app::Application;
use crate::chart_layout::{cat_text_h, label_plan};
use crate::chartparts::{is_radar, is_xy, legend_text_emu};
use crate::charts::part_values;
use crate::error::OmError;
use crate::shapes::DrawRef;

pub(crate) const KEPT_BASE: &str = "Axis1.#bu";

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct DateAxis {
    pub base: Option<i32>,
    pub min: f64,
    pub max: f64,
    pub scale: Option<i32>,
    pub major: f64,

    pub labels: Vec<String>,

    pub bins: i64,

    pub start: i64,
}

fn days_from_civil(y: i64, m: i64, d: i64) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let mp = (m + 9) % 12;
    let doy = (153 * mp + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

fn civil_from_days(z: i64) -> (i64, i64, i64) {
    let z = z + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    (
        if m <= 2 {
            yoe + era * 400 + 1
        } else {
            yoe + era * 400
        },
        m,
        d,
    )
}

const EPOCH: i64 = -25_569;

fn ymd(serial: i64) -> (i64, i64, i64) {
    civil_from_days(serial + EPOCH)
}

fn serial(y: i64, m: i64, d: i64) -> i64 {
    days_from_civil(y, m, d) - EPOCH
}

fn bin_start(s: i64, u: i32) -> i64 {
    let (y, m, _) = ymd(s);
    match u {
        0 => s,
        1 => serial(y, m, 1),
        _ => serial(y, 1, 1),
    }
}

fn bins(a: i64, b: i64, u: i32) -> i64 {
    let ((y0, m0, _), (y1, m1, _)) = (ymd(a), ymd(b));
    match u {
        0 => b - a + 1,
        1 => (y1 * 12 + m1) - (y0 * 12 + m0) + 1,
        _ => y1 - y0 + 1,
    }
}

pub(crate) fn bin_number(lo: i64, u: i32, x: f64) -> i64 {
    let xi = x.floor() as i64;
    let ((y0, m0, _), (y1, m1, _)) = (ymd(lo), ymd(xi));
    1 + match u {
        0 => xi - lo,
        1 => (y1 * 12 + m1) - (y0 * 12 + m0),
        _ => y1 - y0,
    }
}

fn step_from(s: i64, u: i32, k: i64) -> i64 {
    let (y, m, d) = ymd(s);
    match u {
        0 => s + k,
        1 => {
            let t = y * 12 + (m - 1) + k;
            serial(t.div_euclid(12), t.rem_euclid(12) + 1, d)
        }
        _ => serial(y + k, m, d),
    }
}

fn auto_base(dates: &[i64], fixed: Option<i32>) -> i32 {
    let mut v = dates.to_vec();
    v.sort_unstable();
    let Some(gap) = v.windows(2).map(|w| w[1] - w[0]).min() else {
        return 0;
    };
    match (gap, fixed) {
        (g, _) if g < 28 => 0,
        (g, Some(0)) if g < 365 => 0,
        (g, _) if g < 365 => 1,
        (_, Some(f)) => f,
        _ => 2,
    }
}

fn time_capable(t: i32) -> bool {
    !is_xy(t) && !is_radar(t) && matches!(t, 51..=53 | 57..=59 | 4 | 63..=65 | 1 | 76 | 77)
}

fn bag_i(c: &Chart, k: &str) -> Option<i32> {
    match c.bag.get(k) {
        Some(BagVal::I(n)) => Some(*n),
        _ => None,
    }
}

impl Application<'_> {
    pub(crate) fn date_axis(&self, r: &DrawRef, c: &Chart) -> Result<Option<DateAxis>, OmError> {
        let ct = bag_i(c, "Axis1.CategoryType").unwrap_or(-4105);

        let at = crate::chartparts::axis_type(c);
        if !time_capable(at) || c.series.is_empty() || ct == 2 || crate::chartparts::kept_cat(c) {
            return Ok(None);
        }
        let wb = &self.book(r.book)?.wb;

        let s = c
            .series
            .iter()
            .find(|s| matches!(s.x, Part::Refs(_)) && !is_xy(s.chart_type))
            .or_else(|| c.series.iter().find(|s| !is_xy(s.chart_type)))
            .unwrap_or(&c.series[0]);
        let first = match &s.x {
            Part::Refs(v) => v.first().map(|(sh, rc)| CellAddr::new(*sh, rc.r0, rc.c0)),
            _ => None,
        };
        let vals = part_values(wb, &s.x);

        let nums: Option<Vec<f64>> = vals
            .iter()
            .filter(|v| !matches!(v, Variant::Empty))
            .map(|v| match v {
                Variant::R8(n) => Some(*n),
                _ => None,
            })
            .collect();
        let code = first.map(|a| wb.number_format(a).to_string());
        let dated = code
            .as_deref()
            .is_some_and(|f| xlcalc::NumberFormat::parse(f).is_ok_and(|f| f.is_date()));
        let nums = match nums {
            Some(n) if !n.is_empty() && (dated || ct == 3) && n.iter().all(|x| *x >= 61.0) => n,

            ref n if ct == 3 => {
                let count = part_values(wb, &s.values).len().max(vals.len()).max(1);
                let base = match c.bag.get("Axis1.BaseUnit") {
                    Some(BagVal::I(b)) => *b,
                    _ => 0,
                };
                return Ok(Some(DateAxis {
                    base: None,
                    min: 1.0,
                    max: count as f64,
                    scale: n.as_ref().map(|_| base),
                    major: 1.0,
                    labels: Vec::new(),
                    bins: count as i64,
                    start: 1,
                }));
            }
            _ => return Ok(None),
        };
        let dates: Vec<i64> = nums.iter().map(|x| x.floor() as i64).collect();
        let fixed_scale = bag_i(c, "Axis1.MajorUnitScale");
        let base = if matches!(c.bag.get("Axis1.BaseUnitIsAuto"), Some(BagVal::B(false))) {
            bag_i(c, "Axis1.BaseUnit").unwrap_or(0)
        } else {
            auto_base(&dates, fixed_scale)
        };

        let mut span = dates.clone();
        for x in c
            .series
            .iter()
            .filter(|s| s.axis_group == 1 && is_xy(s.chart_type))
        {
            let v = part_values(wb, &x.x);
            if v.is_empty() || v.iter().any(|v| matches!(v, Variant::Bstr(_))) {
                let n = if v.is_empty() {
                    part_values(wb, &x.values).len()
                } else {
                    v.len()
                };
                span.extend(1..=n as i64);
            } else {
                span.extend(v.iter().filter_map(|v| match v {
                    Variant::R8(n) => Some(n.floor() as i64),
                    _ => None,
                }));
            }
        }
        let lo = bin_start(*span.iter().min().unwrap_or(&0), base);
        let hi = bin_start(*span.iter().max().unwrap_or(&0), base);
        let nbins = bins(lo, hi, base);
        let ax = &c.axes[0];
        let (min, max) = (ax.min.unwrap_or(lo as f64), ax.max.unwrap_or(hi as f64));
        let bar = matches!(at, 57..=59);
        let area = matches!(at, 1 | 76 | 77);
        let len = match self.chart_geom_with(r, c, false)? {
            Some(g) => {
                if bar {
                    g.inside[3]
                } else if area {
                    g.plot[0] + g.plot[2] - g.inside[0]
                } else {
                    g.inside[2]
                }
            }
            None => 2_997_238,
        }
        .max(1);
        let fmt = |x: i64| -> String {
            code.as_deref()
                .and_then(|f| xlcalc::NumberFormat::parse(f).ok())
                .and_then(|f| {
                    f.format_number(x as f64, wb.locale(), xlcalc::DateSystem::D1900)
                        .ok()
                })
                .map_or_else(|| x.to_string(), |r| r.text)
        };

        let scale = match fixed_scale {
            Some(f) => f,
            None => {
                let size = if bar {
                    cat_text_h()
                } else {
                    legend_text_emu(&fmt(2_552_679)).max(1)
                };
                let n = (len / size).max(1);
                let span = if area { nbins - 1 } else { nbins };
                let per = span / n;
                let s = if per > 180 {
                    2
                } else if per > 14 {
                    1
                } else {
                    0
                };
                s.max(base)
            }
        };
        let count = bins(lo, hi, scale);
        let labels: Vec<String> = (0..count).map(|k| fmt(step_from(lo, scale, k))).collect();
        let major = match ax.major {
            Some(m) => m,
            None => {
                let step = if bar {
                    let fit = (len / cat_text_h()).max(1);
                    (count - 1) / fit + 1
                } else {
                    label_plan(&labels, len, 0, true).step
                };

                (if scale == 0 && step > 2 {
                    (step + 6) / 7 * 7
                } else {
                    step
                }) as f64
            }
        };
        Ok(Some(DateAxis {
            base: Some(base),
            min,
            max,
            scale: Some(scale),
            major,
            labels,
            bins: nbins,
            start: lo,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bin_number_months_and_days() {
        assert_eq!(bin_number(-29, 1, -0.5), 1);
        assert_eq!(bin_number(-29, 1, -29.5), 0);
        assert_eq!(bin_number(-29, 1, 46_174.0), 1519);
        assert_eq!(bin_number(46_082, 0, 46_082.0), 1);
        assert_eq!(bin_number(46_082, 0, 46_081.5), 0);
        assert_eq!(bin_number(46_082, 0, 46_085.5), 4);
        assert_eq!(
            bin_number(serial(2020, 1, 1), 2, serial(2026, 6, 30) as f64),
            7
        );
    }
}
