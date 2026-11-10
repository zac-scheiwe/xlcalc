use xlcalc::drawing::{Chart, DataLabels, ErrorBars, Series, SeriesFmt, Trendline};
use xlcalc::style::{nearest_color_index, PALETTE as XL_PALETTE};
use xlcalc_variant::Variant;

use crate::app::{unsupported, Application};
use crate::chartparts::{put_bool, put_num, range_err, Sub};
use crate::charts::part_values;
use crate::conv::Args;
use crate::error::OmError;
use crate::shapes::{DrawKind, DrawRef};
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Cls {
    Fill,

    Area,

    Line,
    Radar,

    Scatter,
}

fn cls(t: i32) -> Cls {
    match t {
        1 | 76..=78 | 15 | 87 => Cls::Area,
        4 | 63 | 64 | 65 | 66 | 67 | 72..=75 => Cls::Line,
        -4169 => Cls::Scatter,
        -4151 | 81 => Cls::Radar,
        _ => Cls::Fill,
    }
}

const PALETTE: [i32; 6] = [8544277, 3305961, 2386713, 13999631, 9644960, 3057486];
const MIXED: f32 = -2147483648.0;

fn no_such(m: &'static Member) -> OmError {
    let _ = m;
    range_err("This property is not valid for the current chart type.")
}

fn i4(n: i32) -> Variant {
    Variant::I4(n)
}

fn tri(b: bool) -> Variant {
    Variant::I4(if b { -1 } else { 0 })
}

fn border_weight(w: f32) -> i32 {
    if w <= 0.75 {
        1
    } else if w <= 1.5 {
        2
    } else if w <= 2.25 {
        -4138
    } else {
        4
    }
}

fn dash_of_style(ls: i32) -> Option<i32> {
    Some(match ls {
        1 => 1,
        -4115 => 7,
        -4118 => 10,
        4 => 8,
        5 => 9,
        _ => return None,
    })
}

fn style_of_dash(d: i32) -> i32 {
    match d {
        1 => 1,
        2 | 3 | 10 => -4118,
        4 | 7 => -4115,
        5 | 8 => 4,
        6 | 9 => 5,
        _ => -4105,
    }
}

fn marker_default(t: i32) -> (i32, i32) {
    if matches!(t, 65..=67 | 72 | 74 | -4169 | 81) {
        (-4105, 9)
    } else {
        (-4142, 5)
    }
}

fn is_visible_line(f: &SeriesFmt, c: Cls) -> bool {
    f.line_visible.unwrap_or(c != Cls::Scatter)
}
fn pal(k: usize) -> i32 {
    PALETTE[k % 6]
}

fn color_of_index(n: i32) -> i32 {
    match usize::try_from(n - 1).ok().and_then(|i| XL_PALETTE.get(i)) {
        Some(&rgb) => (((rgb & 0xFF) << 16) | (rgb & 0xFF00) | (rgb >> 16)) as i32,
        None => 0,
    }
}

fn index_of_color(c: i32) -> i32 {
    match c {
        -1 => 2,
        -2 => -4142,
        c => {
            let rgb = ((c & 0xFF) << 16) | (c & 0xFF00) | ((c >> 16) & 0xFF);
            i32::from(nearest_color_index(rgb as u32))
        }
    }
}

fn color_for_index_put(n: i32) -> Result<i32, OmError> {
    match n {
        -4105 => Ok(-1),
        -4142 | 0 => Ok(-2),
        1..=56 => Ok(color_of_index(n)),
        n if n > 56 => Ok(0),
        _ => Err(OmError::unable_to_set("ColorIndex", "Interior")),
    }
}

fn num(a: &Args) -> Result<f64, OmError> {
    put_num(a)
}

pub(crate) fn opt_num(a: &Args, i: usize) -> f64 {
    a.get(i)
        .and_then(|v| crate::controls::num_arg(v, a.lcid).ok())
        .unwrap_or(0.0)
}

fn smooth_default(t: i32) -> bool {
    matches!(t, 72 | 73)
}

impl Application<'_> {
    fn series_edit(
        &mut self,
        r: &DrawRef,
        k: usize,
        f: impl FnOnce(&mut Series, i32) -> Result<(), OmError>,
    ) -> Result<Variant, OmError> {
        self.chart_edit(r, |_, c| {
            let t = c.chart_type;
            let s = c.series.get_mut(k).ok_or_else(OmError::object_required)?;
            f(s, t)?;
            Ok(Variant::Empty)
        })
    }

    fn sub_of(&mut self, r: &DrawRef, s: Sub) -> Variant {
        self.alloc(crate::app::Object::Draw(DrawRef {
            kind: DrawKind::Sub(s),
            ..*r
        }))
    }

    pub(crate) fn series_member(
        &mut self,
        c: &Chart,
        r: &DrawRef,
        k: usize,
        m: &'static Member,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        let put = m.kind == InvokeKind::Put || m.kind == InvokeKind::PutRef;
        let ser = c.series.get(k)?;
        let t = ser.chart_type;
        let cl = cls(t);
        let f = &ser.fmt;
        let ku = k as u8;
        Some(match (m.name, put) {
            ("MarkerStyle", false) => Ok(i4(f.marker_style.unwrap_or(marker_default(t).0))),
            ("MarkerStyle", true) => (|| {
                let v = num(a)? as i32;
                let v = match v {
                    0 => -4142,
                    -4105 | -4142 | 8 | -4115 | 2 | -4118 | 9 | 1 | 5 | 3 | -4168 | -4147 => v,
                    _ => return Err(range_err("Parameter not valid")),
                };
                self.series_edit(r, k, |s, _| {
                    s.fmt.marker_style = Some(v);
                    Ok(())
                })
            })(),
            ("MarkerSize", false) => Ok(i4(f.marker_size.unwrap_or(marker_default(t).1))),
            ("MarkerSize", true) => (|| {
                let v = num(a)?.round_ties_even() as i32;
                if !(2..=72).contains(&v) {
                    return Err(OmError::exception(0x8000_4005_u32 as i32, None));
                }
                self.series_edit(r, k, |s, _| {
                    s.fmt.marker_size = Some(v);
                    Ok(())
                })
            })(),
            ("Smooth", false) => match cl {
                Cls::Fill | Cls::Radar => Err(no_such(m)),
                _ => Ok(Variant::Bool(f.smooth.unwrap_or(smooth_default(t)))),
            },
            ("Smooth", true) => (|| {
                if matches!(cl, Cls::Fill | Cls::Radar) {
                    return Err(no_such(m));
                }
                let v = put_bool(a)?;
                self.series_edit(r, k, |s, _| {
                    s.fmt.smooth = Some(v);
                    Ok(())
                })
            })(),
            ("MarkerBackgroundColor" | "MarkerForegroundColor", false) => match cl {
                Cls::Fill | Cls::Area => Err(no_such(m)),
                _ => {
                    let bg = m.name == "MarkerBackgroundColor";
                    let v = if bg { f.marker_bg } else { f.marker_fg };
                    Ok(i4(v.unwrap_or(
                        if !bg && !is_visible_line(f, cl) && cl != Cls::Scatter {
                            -2
                        } else {
                            -1
                        },
                    )))
                }
            },
            ("MarkerBackgroundColor" | "MarkerForegroundColor", true) => (|| {
                if matches!(cl, Cls::Fill | Cls::Area) {
                    return Err(no_such(m));
                }
                let v = num(a)? as i32;
                let bg = m.name == "MarkerBackgroundColor";
                self.series_edit(r, k, |s, _| {
                    if bg {
                        s.fmt.marker_bg = Some(v);
                    } else {
                        s.fmt.marker_fg = Some(v);
                    }
                    Ok(())
                })
            })(),
            ("MarkerBackgroundColorIndex" | "MarkerForegroundColorIndex", false) => match cl {
                Cls::Fill | Cls::Area => Err(no_such(m)),
                _ => {
                    let bg = m.name == "MarkerBackgroundColorIndex";
                    let v = if bg { f.marker_bg } else { f.marker_fg };
                    let dflt = if !bg && !is_visible_line(f, cl) && cl != Cls::Scatter {
                        -2
                    } else {
                        -1
                    };
                    Ok(i4(index_of_color(v.unwrap_or(dflt))))
                }
            },
            ("MarkerBackgroundColorIndex" | "MarkerForegroundColorIndex", true) => (|| {
                if matches!(cl, Cls::Fill | Cls::Area) {
                    return Err(no_such(m));
                }
                let v = color_for_index_put(num(a)? as i32)?;
                let bg = m.name == "MarkerBackgroundColorIndex";
                self.series_edit(r, k, |s, _| {
                    if bg {
                        s.fmt.marker_bg = Some(v);
                    } else {
                        s.fmt.marker_fg = Some(v);
                    }
                    Ok(())
                })
            })(),
            ("InvertIfNegative" | "Shadow", false) => Ok(Variant::Bool(false)),
            ("Explosion", false) => Ok(i4(0)),
            ("Format", false) => Ok(self.sub_of(r, Sub::SerFormat(ku))),
            ("Interior", false) => Ok(self.sub_of(r, Sub::SerInterior(ku))),
            ("Border", false) => Ok(self.sub_of(r, Sub::SerBorder(ku))),
            ("HasDataLabels", false) => Ok(Variant::Bool(f.labels.as_ref().is_some_and(shows))),
            ("HasDataLabels", true) => (|| {
                let v = put_bool(a)?;
                self.series_edit(r, k, |s, _| {
                    s.fmt.labels = if v {
                        Some(s.fmt.labels.take().unwrap_or_default())
                    } else {
                        None
                    };
                    Ok(())
                })
            })(),
            ("DataLabels", false) => {
                if f.labels.is_none() {
                    return Some(Err(no_labels_err(f.point_labels.is_empty())));
                }
                match a.get(0) {
                    Some(i) => (|| {
                        let n = crate::controls::num_arg(i, a.lcid)? as u16;
                        Ok(self.sub_of(r, Sub::SerLabel(ku, n)))
                    })(),
                    None => Ok(self.sub_of(r, Sub::SerLabels(ku))),
                }
            }
            ("Points", false) => match a.get(0) {
                Some(i) => (|| {
                    let n = crate::controls::num_arg(i, a.lcid)? as u16;
                    let count = part_values(&self.book(r.book)?.wb, &ser.values).len();
                    if n < 1 || n as usize > count {
                        return Err(crate::shapes::out_of_bounds());
                    }
                    Ok(self.sub_of(r, Sub::SerPoint(ku, n)))
                })(),
                None => Ok(self.sub_of(r, Sub::SerPoint(ku, 0))),
            },
            ("Trendlines", false) => match a.get(0) {
                Some(i) => (|| {
                    let n = crate::controls::num_arg(i, a.lcid)? as usize;
                    if n < 1 || n > f.trends.len() {
                        return Err(range_err("Parameter not valid"));
                    }
                    Ok(self.sub_of(r, Sub::Trend(ku, n as u8)))
                })(),
                None => Ok(self.sub_of(r, Sub::TrendColl(ku))),
            },
            ("HasErrorBars", false) => Ok(Variant::Bool(f.error_bars.is_some())),
            ("HasErrorBars", true) => (|| {
                let v = put_bool(a)?;
                self.series_edit(r, k, |s, _| {
                    s.fmt.error_bars = if v {
                        Some(s.fmt.error_bars.take().unwrap_or_default())
                    } else {
                        None
                    };
                    Ok(())
                })
            })(),
            ("ErrorBar", false) => (|| {
                let dir = a.i32(0)?.unwrap_or(0);
                let incl = a.i32(1)?.unwrap_or(0);
                let ty = a.i32(2)?.unwrap_or(0);
                let valid = matches!(dir, 1 | -4168)
                    && matches!(incl, 1..=3 | -4142)
                    && matches!(ty, 1 | 2 | 4 | -4155 | -4114);
                if !valid {
                    return Err(OmError::app_defined());
                }
                if ty == -4114 && matches!(incl, 1 | 3) {
                    return Err(OmError::type_mismatch());
                }
                if incl == -4142 {
                    return self.series_edit(r, k, |s, _| {
                        s.fmt.error_bars = None;
                        Ok(())
                    });
                }
                let bar_chart = matches!(t, 57..=62);
                let x_cats = !bar_chart && !matches!(t, -4169 | 72..=75 | 15 | 87);
                if let Some(e) = &f.error_bars {
                    if (bar_chart && dir == 1 && e.calls >= 2)
                        || (x_cats && dir == -4168 && e.x_calls >= 1)
                    {
                        return Err(range_err(
                            "A series may only have a single set of error bars.",
                        ));
                    }
                }
                self.series_edit(r, k, |s, _| {
                    let e = s.fmt.error_bars.get_or_insert_with(ErrorBars::default);
                    e.calls = e.calls.saturating_add(1);
                    if dir == -4168 {
                        e.x_calls = e.x_calls.saturating_add(1);
                    }
                    Ok(())
                })?;
                Ok(Variant::Empty)
            })(),
            ("ErrorBars", false) => {
                if f.error_bars.is_none() {
                    return Some(Err(OmError::exception(0x8000_4005_u32 as i32, None)));
                }
                Ok(self.sub_of(r, Sub::ErrBars(ku)))
            }
            _ => return None,
        })
    }

    pub(crate) fn serfmt_member(
        &mut self,
        r: &DrawRef,
        sub: Sub,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let put = m.kind == InvokeKind::Put || m.kind == InvokeKind::PutRef;
        let c = self.chart_of(r)?;
        let ku = sub.series().unwrap_or(0) as usize;
        let ser = c
            .series
            .get(ku)
            .cloned()
            .ok_or_else(OmError::object_required)?;
        let t = ser.chart_type;
        let cl = cls(t);
        let f = ser.fmt.clone();
        let edit = |app: &mut Self, f: &dyn Fn(&mut SeriesFmt)| {
            app.series_edit(r, ku, |s, _| {
                f(&mut s.fmt);
                Ok(())
            })
        };
        match sub {
            Sub::SerFormat(_) => match (m.name, put) {
                ("Fill", false) => Ok(self.sub_of(r, Sub::SerFill(ku as u8))),
                ("Line", false) => Ok(self.sub_of(r, Sub::SerLine(ku as u8))),
                _ => Err(unsupported(sub.interface_name(), m)),
            },
            Sub::SerFill(_) | Sub::SerLine(_) => {
                let line = matches!(sub, Sub::SerLine(_));
                match (m.name, put) {
                    ("ForeColor", false) => Ok(self.sub_of(r, Sub::SerFore(ku as u8, line))),
                    ("Visible", false) if !line && matches!(cl, Cls::Fill | Cls::Area) => {
                        Ok(tri(f.fill_visible != Some(false)))
                    }
                    ("Visible", false) if !line => {
                        Ok(tri(f.fill_visible.unwrap_or(f.interior.is_none())))
                    }
                    ("Visible", false) => Ok(tri(is_visible_line(&f, cl))),
                    ("Visible", true) => {
                        let v = put_bool(a)?;
                        if line {
                            let scatter = cl == Cls::Scatter;
                            edit(self, &|x| {
                                x.line_visible = Some(v);
                                if !v && x.marker_fg.is_none() {
                                    x.marker_fg = Some(-2);
                                }
                                if !v && scatter {
                                    x.line_rgb = Some(0);
                                }
                            })
                        } else if v {
                            edit(self, &|x| {
                                x.fill_visible = None;
                                if !matches!(cl, Cls::Fill | Cls::Area) {
                                    x.interior = None;
                                }
                            })
                        } else {
                            edit(self, &|x| x.fill_visible = Some(false))
                        }
                    }
                    ("Weight", false) if line => Ok(Variant::R4(match (f.line_weight, cl) {
                        (Some(w), Cls::Line | Cls::Radar) => w,
                        (Some(w), _) if f.line_rgb.is_some() || f.line_visible == Some(true) => w,
                        (_, Cls::Line | Cls::Radar) => 3.0,
                        _ => MIXED,
                    })),
                    ("Weight", true) if line => {
                        let w = num(a)?;
                        if w < 0.0 {
                            return Err(OmError::exception(
                                0x8007_0057_u32 as i32,
                                Some("The specified value is out of range."),
                            ));
                        }
                        edit(self, &|x| x.line_weight = Some(w as f32))
                    }
                    ("DashStyle", false) if line => Ok(i4(f.dash.unwrap_or(
                        if matches!(cl, Cls::Line | Cls::Radar) && f.line_visible != Some(false) {
                            1
                        } else {
                            -2
                        },
                    ))),
                    ("DashStyle", true) if line => {
                        let d = num(a)? as i32;
                        if !(1..=10).contains(&d) {
                            return Err(OmError::exception(
                                0x8007_0057_u32 as i32,
                                Some("The specified value is out of range."),
                            ));
                        }
                        edit(self, &|x| x.dash = Some(d))
                    }
                    _ => Err(unsupported(sub.interface_name(), m)),
                }
            }
            Sub::SerFore(_, line) => match (m.name, put) {
                ("RGB", false) => Ok(i4(if line {
                    f.line_rgb.unwrap_or(match cl {
                        Cls::Line | Cls::Radar => pal(ku),
                        Cls::Scatter => 16777215,
                        _ => 0,
                    })
                } else {
                    f.fill_rgb
                        .or(f.interior.filter(|c| *c >= 0 && cl != Cls::Scatter))
                        .filter(|_| matches!(cl, Cls::Fill | Cls::Area))
                        .unwrap_or(match cl {
                            Cls::Fill | Cls::Area => pal(ku),
                            _ => 0,
                        })
                })),
                ("RGB", true) => {
                    let v = num(a)? as i32;
                    if line {
                        edit(self, &|x| {
                            x.line_rgb = Some(v);
                            x.border_rgb = None;
                        })
                    } else if matches!(cl, Cls::Fill | Cls::Area) {
                        edit(self, &|x| {
                            x.fill_rgb = Some(v);
                            x.interior = Some(v);
                        })
                    } else {
                        edit(self, &|x| x.marker_bg = Some(v))
                    }
                }
                _ => Err(unsupported(sub.interface_name(), m)),
            },
            Sub::SerInterior(_) => {
                let fill_cls = matches!(cl, Cls::Fill | Cls::Area);
                let hidden = fill_cls && f.fill_visible == Some(false);
                let color = if hidden {
                    -2
                } else if fill_cls {
                    f.interior.or(f.fill_rgb).unwrap_or(pal(ku))
                } else {
                    f.interior.unwrap_or(16777215)
                };
                let auto = if fill_cls {
                    f.interior.is_none() && f.fill_rgb.is_none() && !hidden
                } else {
                    f.interior.is_none()
                };
                let set_color = |app: &mut Self, v: i32| {
                    edit(app, &|x| {
                        if fill_cls {
                            x.fill_rgb = Some(v);
                        }
                        x.interior = Some(v);
                        x.fill_visible = None;
                    })
                };
                match (m.name, put) {
                    ("Color", false) => Ok(i4(color)),
                    ("Color", true) => set_color(self, num(a)? as i32),
                    ("ColorIndex", false) => Ok(i4(if auto && !hidden {
                        -4105
                    } else {
                        index_of_color(color)
                    })),
                    ("ColorIndex", true) => match color_for_index_put(num(a)? as i32)? {
                        -1 => edit(self, &|x| {
                            x.interior = None;
                            x.fill_rgb = None;
                            x.fill_visible = None;
                        }),
                        -2 if fill_cls => edit(self, &|x| x.fill_visible = Some(false)),
                        -2 => edit(self, &|x| {
                            x.interior = Some(-2);
                            x.fill_visible = Some(false);
                        }),
                        v => set_color(self, v),
                    },
                    ("Pattern", false) => Ok(i4(if fill_cls {
                        if f.interior.is_some() || f.fill_rgb.is_some() || hidden {
                            1
                        } else {
                            -4105
                        }
                    } else if f.interior.is_some() || f.fill_visible == Some(false) {
                        -4142
                    } else {
                        -4105
                    })),
                    ("Pattern", true) => match num(a)? as i32 {
                        1 if fill_cls => set_color(self, color.max(0)),
                        -4142 if fill_cls => edit(self, &|x| {
                            x.interior = Some(color.max(0));
                            x.fill_rgb = Some(color.max(0));
                            x.fill_visible = Some(false);
                        }),
                        1 | -4142 => edit(self, &|x| x.interior = None),
                        _ => Err(OmError::exception(0x8007_0057_u32 as i32, None)),
                    },
                    _ => Err(unsupported(sub.interface_name(), m)),
                }
            }
            Sub::SerBorder(_) => match (m.name, put) {
                ("Color", false) => Ok(i4(f.border_rgb.unwrap_or(if f.line_rgb.is_some() {
                    -2
                } else if matches!(cl, Cls::Line | Cls::Radar) {
                    pal(ku)
                } else {
                    -2
                }))),
                ("Color", true) => {
                    let v = num(a)? as i32;
                    edit(self, &|x| {
                        x.line_rgb = Some(v);
                        x.border_rgb = Some(v);
                        x.line_visible = Some(true);
                    })
                }
                ("ColorIndex", false) => {
                    let none = f.line_visible == Some(false)
                        || f.border_style == Some(-4142)
                        || (f.line_visible.is_none()
                            && cl == Cls::Scatter
                            && f.border_style.is_none());
                    Ok(i4(if none {
                        -4142
                    } else {
                        match f.border_rgb {
                            Some(c) => index_of_color(c),
                            None => -4105,
                        }
                    }))
                }
                ("ColorIndex", true) => match color_for_index_put(num(a)? as i32)? {
                    -1 => edit(self, &|x| {
                        x.line_rgb = None;
                        x.border_rgb = None;
                        x.line_visible = Some(true);
                        if x.border_style == Some(-4142) {
                            x.border_style = None;
                        }
                    }),
                    -2 => edit(self, &|x| {
                        x.border_style = Some(-4142);
                        x.line_visible = Some(false);
                        x.dash = None;
                        if cl != Cls::Scatter && x.marker_fg.is_none() {
                            x.marker_fg = Some(-2);
                        }
                    }),
                    v => edit(self, &|x| {
                        x.line_rgb = Some(v);
                        x.border_rgb = Some(v);
                        x.line_visible = Some(true);
                    }),
                },
                ("Weight", false) => {
                    let w = match (f.line_weight, cl) {
                        (Some(w), _) => w,
                        (None, Cls::Line | Cls::Radar | Cls::Scatter) => 3.0,
                        _ => 1.0,
                    };
                    Ok(i4(border_weight(w)))
                }
                ("Weight", true) => {
                    let w = match num(a)? as i32 {
                        1 => 0.25,
                        2 => 1.0,
                        4 => 3.0,
                        -4138 | 3 => 2.0,
                        _ => return Err(OmError::unable_to_set("Weight", "Border")),
                    };
                    edit(self, &|x| {
                        x.line_weight = Some(w);
                        x.line_visible = Some(true);
                    })
                }
                ("LineStyle", false) => Ok(i4(
                    if f.line_visible == Some(false)
                        || f.border_style == Some(-4142)
                        || (f.line_visible.is_none()
                            && cl == Cls::Scatter
                            && f.border_style.is_none())
                    {
                        -4142
                    } else if let Some(d) = f.dash {
                        style_of_dash(d)
                    } else {
                        f.border_style.unwrap_or(-4105)
                    },
                )),
                ("LineStyle", true) => {
                    let v = num(a)? as i32;
                    match v {
                        -4142 | 0 => edit(self, &|x| {
                            x.border_style = Some(-4142);
                            x.line_visible = Some(false);
                            x.dash = None;
                            if cl != Cls::Scatter && x.marker_fg.is_none() {
                                x.marker_fg = Some(-2);
                            }
                        }),
                        -4105 => edit(self, &|x| {
                            x.border_style = Some(-4105);
                            x.line_visible = Some(true);
                            x.dash = None;
                        }),
                        1 | -4115 | -4118 | 4 | 5 => {
                            let d = dash_of_style(v);
                            edit(self, &|x| {
                                x.dash = d;
                                x.border_style = None;
                                x.line_visible = Some(true);
                            })
                        }
                        _ => Err(OmError::unable_to_set("LineStyle", "Border")),
                    }
                }
                _ => Err(unsupported(sub.interface_name(), m)),
            },
            Sub::SerLabels(_) | Sub::SerLabel(..) => self.labels_member(r, &c, sub, m, a),
            Sub::SerPoint(..) => match (m.name, put) {
                ("HasDataLabel", false) => {
                    let Sub::SerPoint(_, n) = sub else {
                        return Err(OmError::app_defined());
                    };
                    Ok(Variant::Bool(
                        f.labels.is_some() || f.point_labels.contains(&n),
                    ))
                }
                ("HasDataLabel", true) => {
                    let v = put_bool(a)?;
                    let Sub::SerPoint(_, n) = sub else {
                        return Err(OmError::app_defined());
                    };
                    edit(self, &|x| {
                        x.point_labels.retain(|p| *p != n);
                        if v {
                            x.point_labels.push(n);
                        }
                    })
                }
                ("DataLabel", false) => {
                    let Sub::SerPoint(_, n) = sub else {
                        return Err(OmError::app_defined());
                    };
                    Ok(self.sub_of(r, Sub::SerLabel(ku as u8, n)))
                }
                _ => Err(unsupported(sub.interface_name(), m)),
            },
            Sub::TrendColl(_) => self.trend_coll_member(r, &ser, ku, m, a),
            Sub::Trend(_, j) => self.trend_member(r, &ser, ku, j as usize, m, a),
            Sub::ErrBars(_) => match (m.name, put) {
                ("EndStyle", false) => Ok(i4(f.error_bars.map_or(1, |e| e.end_style))),
                ("EndStyle", true) => {
                    let v = num(a)? as i32;
                    if !matches!(v, 1 | 2) {
                        return Err(OmError::app_defined());
                    }
                    edit(self, &|x| {
                        if let Some(e) = &mut x.error_bars {
                            e.end_style = v;
                        }
                    })
                }
                ("Delete", false) => {
                    edit(self, &|x| x.error_bars = None)?;
                    Ok(Variant::Empty)
                }
                _ => Err(unsupported(sub.interface_name(), m)),
            },
            _ => Err(unsupported(sub.interface_name(), m)),
        }
    }

    pub(crate) fn apply_data_labels(&mut self, r: &DrawRef, a: &Args) -> Result<Variant, OmError> {
        let ty = a.i32(0)?.unwrap_or(2);
        self.chart_edit(r, |_, c| {
            for s in &mut c.series {
                s.fmt.labels = (ty != -4142).then(DataLabels::default);
            }
            Ok(Variant::Empty)
        })
    }

    fn labels_member(
        &mut self,
        r: &DrawRef,
        c: &Chart,
        sub: Sub,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let put = m.kind == InvokeKind::Put || m.kind == InvokeKind::PutRef;
        let ku = sub.series().unwrap_or(0) as usize;
        let ser = c
            .series
            .get(ku)
            .cloned()
            .ok_or_else(OmError::object_required)?;
        let t = ser.chart_type;
        let own = matches!(sub, Sub::SerLabel(_, n) if ser.fmt.point_labels.contains(&n));
        let Some(l) = ser
            .fmt
            .labels
            .clone()
            .or_else(|| own.then(DataLabels::default))
        else {
            return Err(no_labels_err(ser.fmt.point_labels.is_empty()));
        };
        let count = part_values(&self.book(r.book)?.wb, &ser.values).len();
        let flag = |name: &str| -> Option<bool> {
            Some(match name {
                "ShowValue" => l.show_value,
                "ShowSeriesName" => l.show_series,
                "ShowCategoryName" => l.show_category,
                "ShowPercentage" => l.show_percent,
                "ShowLegendKey" => l.show_legend_key,
                "ShowBubbleSize" => l.show_bubble,
                _ => return None,
            })
        };
        if let Some(v) = flag(m.name) {
            if !put {
                return Ok(Variant::Bool(v));
            }
            let nv = put_bool(a)?;
            let name = m.name;
            return self.series_edit(r, ku, |s, _| {
                if let Some(l) = &mut s.fmt.labels {
                    match name {
                        "ShowValue" => l.show_value = nv,
                        "ShowSeriesName" => l.show_series = nv,
                        "ShowCategoryName" => l.show_category = nv,
                        "ShowPercentage" => l.show_percent = nv,
                        "ShowLegendKey" => l.show_legend_key = nv,
                        _ => l.show_bubble = nv,
                    }
                }
                Ok(())
            });
        }
        match (m.name, put) {
            ("Position", false) => Ok(i4(l.position.unwrap_or(default_label_pos(t)))),
            ("Position", true) => {
                let v = num(a)? as i32;
                if !label_positions(t).contains(&v) {
                    return Err(OmError::exception(0x8000_4005_u32 as i32, None));
                }
                self.series_edit(r, ku, |s, _| {
                    if let Some(l) = &mut s.fmt.labels {
                        l.position = Some(v);
                    }
                    Ok(())
                })
            }
            ("NumberFormat", false) => Ok(Variant::Bstr(
                l.number_format.clone().unwrap_or_else(|| "General".into()),
            )),
            ("NumberFormat", true) => {
                let v = match a.put.as_ref() {
                    Some(Variant::Bstr(t)) => t.clone(),
                    _ => return Err(OmError::type_mismatch()),
                };
                self.series_edit(r, ku, |s, _| {
                    if let Some(l) = &mut s.fmt.labels {
                        l.number_format = Some(v);
                    }
                    Ok(())
                })
            }
            ("NumberFormatLinked", false) => Ok(Variant::Bool(l.number_format.is_none())),
            ("Count", false) => Ok(i4(count as i32)),
            ("Separator", false) => Ok(i4(1)),
            ("Delete", false) => {
                self.series_edit(r, ku, |s, _| {
                    s.fmt.labels = None;
                    Ok(())
                })?;
                Ok(Variant::Empty)
            }
            ("Text", false) => {
                if !shows(&l) {
                    return Err(OmError::exception(
                        0x8007_0057_u32 as i32,
                        Some("Parameter not valid"),
                    ));
                }
                let Sub::SerLabel(_, n) = sub else {
                    return Err(unsupported(sub.interface_name(), m));
                };
                let wb = &self.book(r.book)?.wb;
                let vals = part_values(wb, &ser.values);
                let cats = part_values(wb, &ser.x);
                let i = n.max(1) as usize - 1;
                let mut parts = Vec::new();
                if l.show_series {
                    parts.push(match &ser.name {
                        xlcalc::drawing::Part::None => format!("Series{}", ku + 1),
                        xlcalc::drawing::Part::Text(t) => t.clone(),
                        p => match part_values(wb, p).first() {
                            Some(Variant::Bstr(s)) => s.clone(),
                            _ => String::new(),
                        },
                    });
                }
                if l.show_category {
                    parts.push(match cats.get(i) {
                        Some(Variant::Bstr(s)) => s.clone(),
                        Some(Variant::R8(x)) => xlcalc::format_general(*x),
                        _ => (i + 1).to_string(),
                    });
                }
                if l.show_value {
                    parts.push(match vals.get(i) {
                        Some(Variant::R8(x)) => match &l.number_format {
                            Some(code) => render_us(code, *x),
                            None => xlcalc::format_general(*x),
                        },
                        _ => String::new(),
                    });
                }
                Ok(Variant::Bstr(parts.join(", ")))
            }
            _ => Err(unsupported(sub.interface_name(), m)),
        }
    }

    fn trend_coll_member(
        &mut self,
        r: &DrawRef,
        ser: &Series,
        ku: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match m.name {
            "Count" => Ok(i4(ser.fmt.trends.len() as i32)),
            "Add" => {
                if no_trend(ser.chart_type) {
                    return Err(OmError::exception(0x8000_4005_u32 as i32, None));
                }
                let kind = a.i32(0)?.unwrap_or(-4132);
                if !matches!(kind, -4132 | -4133 | 3 | 4 | 5 | 6) {
                    return Err(range_err("Parameter not valid"));
                }
                let order = a.i32(1)?.unwrap_or(2);
                if !(2..=6).contains(&order) {
                    return Err(range_err(
                        "Polynomial order must be an integer from 2 through 6.",
                    ));
                }
                let period = a.i32(2)?.unwrap_or(2);
                if crate::serfmt::opt_num(a, 4) > back_limit(self, r, ser) {
                    return Err(range_err(""));
                }

                let kind = if order > 2 { 3 } else { kind };
                let tl = Trendline {
                    kind,
                    order: if kind == 3 { order } else { 2 },
                    period: if kind == 6 { period } else { 2 },
                    forward: crate::serfmt::opt_num(a, 3),
                    backward: crate::serfmt::opt_num(a, 4),
                    name: match a.get(8) {
                        Some(Variant::Bstr(s)) => Some(s.clone()),
                        _ => None,
                    },
                    intercept: None,
                    display_equation: matches!(a.get(6), Some(Variant::Bool(true))),
                    display_r2: matches!(a.get(7), Some(Variant::Bool(true))),
                };
                self.series_edit(r, ku, |s, _| {
                    s.fmt.trends.push(tl);
                    Ok(())
                })?;
                let n = ser.fmt.trends.len() + 1;
                Ok(self.sub_of(r, Sub::Trend(ku as u8, n as u8)))
            }
            "Item" | "_Default" => match a.get(0) {
                Some(i) => {
                    let n = crate::controls::num_arg(i, a.lcid)? as usize;
                    if n < 1 || n > ser.fmt.trends.len() {
                        return Err(range_err("Parameter not valid"));
                    }
                    Ok(self.sub_of(r, Sub::Trend(ku as u8, n as u8)))
                }
                None => Err(OmError::app_defined()),
            },
            _ => Err(unsupported("Trendlines", m)),
        }
    }

    fn trend_member(
        &mut self,
        r: &DrawRef,
        ser: &Series,
        ku: usize,
        j: usize,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let put = m.kind == InvokeKind::Put || m.kind == InvokeKind::PutRef;
        let tl = ser
            .fmt
            .trends
            .get(j - 1)
            .cloned()
            .ok_or_else(OmError::object_required)?;
        let edit = |app: &mut Self, f: &dyn Fn(&mut Trendline)| {
            app.series_edit(r, ku, |s, _| {
                if let Some(t) = s.fmt.trends.get_mut(j - 1) {
                    f(t);
                }
                Ok(())
            })
        };
        let sname = match &ser.name {
            xlcalc::drawing::Part::None => format!("Series{}", ku + 1),
            xlcalc::drawing::Part::Text(t) => t.clone(),
            p => match part_values(&self.book(r.book)?.wb, p).first() {
                Some(Variant::Bstr(s)) => s.clone(),
                Some(Variant::R8(n)) => xlcalc::format_general(*n),
                _ => String::new(),
            },
        };
        match (m.name, put) {
            ("Type", false) => Ok(i4(tl.kind)),
            ("Type", true) => {
                let v = num(a)? as i32;
                if !matches!(v, -4132 | -4133 | 3 | 4 | 5 | 6) {
                    return Err(range_err("Parameter not valid"));
                }
                edit(self, &|t| t.kind = v)
            }
            ("Name", false) => Ok(Variant::Bstr(tl.name.clone().unwrap_or_else(|| {
                let base = match tl.kind {
                    -4132 => "Linear",
                    -4133 => "Log.",
                    3 => "Poly.",
                    4 => "Power",
                    5 => "Expon.",
                    _ => "2 per. Mov. Avg.",
                };
                if tl.kind == 6 {
                    format!("{} per. Mov. Avg. ({sname})", tl.period)
                } else {
                    format!("{base} ({sname})")
                }
            }))),
            ("Name", true) => {
                let v = match a.put.as_ref() {
                    Some(Variant::Bstr(t)) => t.clone(),
                    _ => return Err(OmError::type_mismatch()),
                };
                edit(self, &|t| t.name = Some(v.clone()))
            }
            ("NameIsAuto", false) => Ok(Variant::Bool(tl.name.is_none())),
            ("Order", false) => Ok(i4(tl.order)),
            ("Order", true) => {
                let v = num(a)? as i32;
                if tl.kind != 3 && (2..=6).contains(&v) {
                    return edit(self, &|_| {});
                }
                if !(2..=6).contains(&v) {
                    return Err(range_err("Parameter not valid"));
                }
                edit(self, &|t| t.order = v)
            }
            ("Period", false) => Ok(i4(tl.period)),
            ("Period", true) => {
                let v = num(a)? as i32;
                if tl.kind != 6 || !(2..=255).contains(&v) {
                    return Err(OmError::exception(0x800A_03EC_u32 as i32, None));
                }
                edit(self, &|t| t.period = v)
            }
            ("Forward", false) => Ok(Variant::R8(tl.forward)),
            ("Backward", false) => Ok(Variant::R8(tl.backward)),
            ("Forward", true) => {
                let v = num(a)?;
                edit(self, &|t| t.forward = v)
            }
            ("Backward", true) => {
                let v = num(a)?;
                if !(0.0..=back_limit(self, r, ser)).contains(&v) {
                    return Err(OmError::exception(0x8000_4005_u32 as i32, None));
                }
                edit(self, &|t| t.backward = v)
            }
            ("DisplayEquation", false) => Ok(Variant::Bool(tl.display_equation)),
            ("DisplayRSquared", false) => Ok(Variant::Bool(tl.display_r2)),
            ("DisplayEquation", true) => {
                let v = put_bool(a)?;
                edit(self, &|t| t.display_equation = v)
            }
            ("DisplayRSquared", true) => {
                let v = put_bool(a)?;
                edit(self, &|t| t.display_r2 = v)
            }
            ("InterceptIsAuto", false) => Ok(Variant::Bool(tl.intercept.is_none())),
            ("Intercept", false) => Ok(Variant::R8(tl.intercept.unwrap_or(0.0))),
            ("Delete", false) => {
                self.series_edit(r, ku, |s, _| {
                    if j - 1 < s.fmt.trends.len() {
                        s.fmt.trends.remove(j - 1);
                    }
                    Ok(())
                })?;
                Ok(Variant::Empty)
            }
            _ => Err(unsupported("Trendline", m)),
        }
    }
}

fn back_limit(app: &Application<'_>, r: &DrawRef, ser: &Series) -> f64 {
    if cls(ser.chart_type) != Cls::Scatter && !matches!(ser.chart_type, 72..=75) {
        return 0.5;
    }
    let Ok(b) = app.book(r.book) else { return 0.5 };
    part_values(&b.wb, &ser.x)
        .iter()
        .filter_map(|v| {
            if let Variant::R8(x) = v {
                Some(*x)
            } else {
                None
            }
        })
        .fold(f64::INFINITY, f64::min)
}

fn no_trend(t: i32) -> bool {
    is_pie_like(t)
        || matches!(t, 52 | 53 | 54..=56 | 58 | 59 | 60..=62 | 63 | 64 | 66 | 67 | 76..=78 | -4151 | 81 | 82 | -4100 | -4098 | -4101)
}

fn no_labels_err(none_at_all: bool) -> OmError {
    if none_at_all {
        OmError::exception(
            0x8000_4005_u32 as i32,
            Some("Unable to get the Count property of the DataLabels class."),
        )
    } else {
        OmError::exception(0x8007_0057_u32 as i32, Some("Parameter not valid"))
    }
}

fn shows(l: &DataLabels) -> bool {
    l.show_value || l.show_series || l.show_category || l.show_percent || l.show_bubble
}

fn is_pie_like(t: i32) -> bool {
    matches!(t, 5 | 68..=71 | 80 | -4102 | -4120)
}

fn default_label_pos(t: i32) -> i32 {
    match cls(t) {
        Cls::Fill if t == 52 || t == 53 || t == 58 || t == 59 => -4108,
        Cls::Fill if is_pie_like(t) => 5,
        Cls::Fill => 2,
        Cls::Area => -4108,
        Cls::Radar => 2,
        _ => -4152,
    }
}

fn label_positions(t: i32) -> &'static [i32] {
    match cls(t) {
        Cls::Fill if matches!(t, 52 | 53 | 58 | 59) => &[3, -4108, 4],
        Cls::Fill if is_pie_like(t) => &[2, 3, -4108, 5],
        Cls::Fill => &[2, 3, -4108, 4],
        Cls::Area => &[],
        Cls::Radar => &[],
        _ => &[-4131, -4152, -4108, 1, 0],
    }
}

fn render_us(code: &str, x: f64) -> String {
    xlcalc::NumberFormat::parse(code)
        .and_then(|f| f.format_number(x, &xlcalc::Locale::en_us(), xlcalc::DateSystem::D1900))
        .map_or_else(|_| xlcalc::format_general(x), |r| r.text)
}
