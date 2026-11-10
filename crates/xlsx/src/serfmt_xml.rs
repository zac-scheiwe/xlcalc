use crate::xml::Node;
use xlcalc::drawing::{DataLabels, ErrorBars, SeriesFmt, Trendline};

fn srgb(c: i32) -> String {
    format!(
        "<a:solidFill><a:srgbClr val=\"{:02X}{:02X}{:02X}\"/></a:solidFill>",
        c & 0xFF,
        (c >> 8) & 0xFF,
        (c >> 16) & 0xFF
    )
}

fn color_of(n: &Node) -> Option<i32> {
    let v = n.child("solidFill")?.child("srgbClr")?.attr("val")?;
    let rgb = u32::from_str_radix(v, 16).ok()?;
    Some((((rgb & 0xFF) << 16) | (rgb & 0xFF00) | (rgb >> 16)) as i32)
}

type Table = [(i32, &'static str)];

const DASHES: [(i32, &str); 10] = [
    (1, "solid"),
    (2, "sysDot"),
    (3, "dot"),
    (4, "dash"),
    (5, "dashDot"),
    (6, "sysDashDotDot"),
    (7, "lgDash"),
    (8, "lgDashDot"),
    (9, "lgDashDotDot"),
    (10, "sysDash"),
];

const SYMBOLS: [(i32, &str); 11] = [
    (-4142, "none"),
    (8, "circle"),
    (2, "diamond"),
    (1, "square"),
    (3, "triangle"),
    (-4168, "x"),
    (5, "star"),
    (-4118, "dot"),
    (-4115, "dash"),
    (9, "plus"),
    (-4147, "picture"),
];

const POSITIONS: [(i32, &str); 9] = [
    (-4108, "ctr"),
    (3, "inEnd"),
    (4, "inBase"),
    (2, "outEnd"),
    (0, "t"),
    (1, "b"),
    (-4131, "l"),
    (-4152, "r"),
    (5, "bestFit"),
];

const TRENDS: [(i32, &str); 6] = [
    (-4132, "linear"),
    (5, "exp"),
    (-4133, "log"),
    (6, "movingAvg"),
    (3, "poly"),
    (4, "power"),
];

fn name_of(table: &Table, v: i32) -> Option<&'static str> {
    table.iter().find(|(k, _)| *k == v).map(|(_, n)| *n)
}

fn code_of(table: &Table, name: &str) -> Option<i32> {
    table.iter().find(|(_, n)| *n == name).map(|(k, _)| *k)
}

fn filled(el: &str) -> bool {
    matches!(el, "barChart" | "areaChart" | "pieChart" | "doughnutChart")
}

fn sp_pr(el: &str, chart_type: i32, f: &SeriesFmt) -> String {
    let mut s = String::new();
    if filled(el) {
        if f.fill_visible == Some(false) {
            s.push_str("<a:noFill/>");
        } else if let Some(c) = f.interior.or(f.fill_rgb).filter(|c| *c >= 0) {
            s.push_str(&srgb(c));
        }
    }
    let hidden =
        f.line_visible == Some(false) || (chart_type == -4169 && f.line_visible != Some(true));
    if hidden {
        s.push_str("<a:ln w=\"19050\"><a:noFill/></a:ln>");
    } else if f.line_rgb.is_some() || f.line_weight.is_some() || f.dash.is_some() {
        s.push_str("<a:ln");
        if let Some(w) = f.line_weight {
            s.push_str(&format!(
                " w=\"{}\"",
                (f64::from(w) * 12700.0).round() as i64
            ));
        }
        s.push('>');
        if let Some(c) = f.line_rgb.filter(|c| *c >= 0) {
            s.push_str(&srgb(c));
        }
        if let Some(d) = f.dash.and_then(|d| name_of(&DASHES, d)) {
            s.push_str(&format!("<a:prstDash val=\"{d}\"/>"));
        }
        s.push_str("</a:ln>");
    }
    if s.is_empty() {
        s
    } else {
        format!("<c:spPr>{s}</c:spPr>")
    }
}

fn marker(f: &SeriesFmt) -> String {
    if f.marker_style.is_none()
        && f.marker_size.is_none()
        && f.marker_bg.is_none()
        && f.marker_fg.is_none()
    {
        return String::new();
    }
    let mut s = String::from("<c:marker>");
    if let Some(n) = f.marker_style.and_then(|v| name_of(&SYMBOLS, v)) {
        s.push_str(&format!("<c:symbol val=\"{n}\"/>"));
    }
    if let Some(z) = f.marker_size {
        s.push_str(&format!("<c:size val=\"{z}\"/>"));
    }
    let mut pr = String::new();
    match f.marker_bg {
        Some(-2) => pr.push_str("<a:noFill/>"),
        Some(c) if c >= 0 => pr.push_str(&srgb(c)),
        _ => {}
    }
    match f.marker_fg {
        Some(-2) => pr.push_str("<a:ln><a:noFill/></a:ln>"),
        Some(c) if c >= 0 => pr.push_str(&format!("<a:ln>{}</a:ln>", srgb(c))),
        _ => {}
    }
    if !pr.is_empty() {
        s.push_str(&format!("<c:spPr>{pr}</c:spPr>"));
    }
    s.push_str("</c:marker>");
    s
}

fn flags(d: &DataLabels) -> String {
    let b = u8::from;
    format!(
        "<c:showLegendKey val=\"{}\"/><c:showVal val=\"{}\"/><c:showCatName val=\"{}\"/><c:showSerName val=\"{}\"/><c:showPercent val=\"{}\"/><c:showBubbleSize val=\"{}\"/>",
        b(d.show_legend_key),
        b(d.show_value),
        b(d.show_category),
        b(d.show_series),
        b(d.show_percent),
        b(d.show_bubble)
    )
}

fn labels(f: &SeriesFmt) -> String {
    if f.labels.is_none() && f.point_labels.is_empty() {
        return String::new();
    }
    let mut s = String::from("<c:dLbls>");
    for p in &f.point_labels {
        s.push_str(&format!(
            "<c:dLbl><c:idx val=\"{}\"/>{}</c:dLbl>",
            p.saturating_sub(1),
            flags(&DataLabels::default())
        ));
    }
    match &f.labels {
        Some(d) => {
            if let Some(nf) = &d.number_format {
                s.push_str(&format!(
                    "<c:numFmt formatCode=\"{}\" sourceLinked=\"0\"/>",
                    crate::rawxml::escape_attr(nf)
                ));
            }
            if let Some(p) = d.position.and_then(|p| name_of(&POSITIONS, p)) {
                s.push_str(&format!("<c:dLblPos val=\"{p}\"/>"));
            }
            s.push_str(&flags(d));
        }
        None => s.push_str(&flags(&DataLabels {
            show_value: false,
            ..DataLabels::default()
        })),
    }
    s.push_str("</c:dLbls>");
    s
}

fn trends(f: &SeriesFmt) -> String {
    let num = crate::write::number_text;
    let mut s = String::new();
    for t in &f.trends {
        let Some(k) = name_of(&TRENDS, t.kind) else {
            continue;
        };
        s.push_str("<c:trendline>");
        if let Some(n) = &t.name {
            s.push_str(&format!(
                "<c:name>{}</c:name>",
                crate::rawxml::escape_text(n)
            ));
        }
        s.push_str(&format!("<c:trendlineType val=\"{k}\"/>"));
        if t.kind == 3 {
            s.push_str(&format!("<c:order val=\"{}\"/>", t.order));
        }
        if t.kind == 6 {
            s.push_str(&format!("<c:period val=\"{}\"/>", t.period));
        }
        if t.forward != 0.0 {
            s.push_str(&format!("<c:forward val=\"{}\"/>", num(t.forward)));
        }
        if t.backward != 0.0 {
            s.push_str(&format!("<c:backward val=\"{}\"/>", num(t.backward)));
        }
        if let Some(i) = t.intercept {
            s.push_str(&format!("<c:intercept val=\"{}\"/>", num(i)));
        }
        s.push_str(&format!(
            "<c:dispRSqr val=\"{}\"/><c:dispEq val=\"{}\"/></c:trendline>",
            u8::from(t.display_r2),
            u8::from(t.display_equation)
        ));
    }
    s
}

fn err_bars(el: &str, f: &SeriesFmt) -> String {
    let Some(e) = &f.error_bars else {
        return String::new();
    };
    let dir = if el == "scatterChart" || el == "bubbleChart" {
        "<c:errDir val=\"y\"/>"
    } else {
        ""
    };
    format!(
        "<c:errBars>{dir}<c:errBarType val=\"both\"/><c:errValType val=\"fixedVal\"/><c:noEndCap val=\"{}\"/><c:val val=\"1\"/></c:errBars>",
        u8::from(e.end_style == 2)
    )
}

pub(crate) fn series_pre(el: &str, chart_type: i32, f: &SeriesFmt) -> String {
    let mut s = sp_pr(el, chart_type, f);
    if matches!(el, "lineChart" | "scatterChart" | "radarChart") {
        s.push_str(&marker(f));
    }
    s.push_str(&labels(f));
    if !matches!(el, "pieChart" | "doughnutChart" | "radarChart") {
        s.push_str(&trends(f));
        s.push_str(&err_bars(el, f));
    }
    s
}

pub(crate) fn smooth(chart_type: i32, f: &SeriesFmt) -> String {
    let v = f.smooth.unwrap_or(matches!(chart_type, 72 | 73));
    format!("<c:smooth val=\"{}\"/>", u8::from(v))
}

fn val<'a>(n: &'a Node, name: &str) -> Option<&'a str> {
    n.child(name)?.attr("val")
}

fn flag(n: &Node, name: &str) -> Option<bool> {
    val(n, name).map(|v| matches!(v, "1" | "true"))
}

pub(crate) fn read_fmt(ser: &Node, el: &str, chart_type: i32) -> SeriesFmt {
    let mut f = SeriesFmt::default();
    if let Some(sp) = ser.child("spPr") {
        if filled(el) {
            if sp.child("noFill").is_some() {
                f.fill_visible = Some(false);
            } else if let Some(c) = color_of(sp) {
                f.fill_rgb = Some(c);
                f.interior = Some(c);
            }
        }
        if let Some(ln) = sp.child("ln") {
            if ln.child("noFill").is_some() {
                if chart_type != -4169 {
                    f.line_visible = Some(false);
                }
            } else {
                if let Some(c) = color_of(ln) {
                    f.line_rgb = Some(c);
                    f.border_rgb = Some(c);
                    f.line_visible = Some(true);
                }
                if let Some(w) = ln.attr("w").and_then(|w| w.parse::<f64>().ok()) {
                    f.line_weight = Some((w / 12700.0) as f32);
                }
                f.dash = val(ln, "prstDash").and_then(|d| code_of(&DASHES, d));
                if f.dash.is_some() {
                    f.line_visible = Some(true);
                }
            }
        }
    }
    if let Some(m) = ser.child("marker") {
        f.marker_style = val(m, "symbol").and_then(|s| code_of(&SYMBOLS, s));
        f.marker_size = val(m, "size").and_then(|v| v.parse().ok());
        if let Some(sp) = m.child("spPr") {
            f.marker_bg = if sp.child("noFill").is_some() {
                Some(-2)
            } else {
                color_of(sp)
            };
            if let Some(ln) = sp.child("ln") {
                f.marker_fg = if ln.child("noFill").is_some() {
                    Some(-2)
                } else {
                    color_of(ln)
                };
            }
        }
    }
    if let Some(sm) = val(ser, "smooth") {
        let v = matches!(sm, "1" | "true");
        if v != matches!(chart_type, 72 | 73) {
            f.smooth = Some(v);
        }
    }
    if let Some(d) = ser.child("dLbls") {
        for l in d.children("dLbl") {
            if let Some(i) = val(l, "idx").and_then(|v| v.parse::<u16>().ok()) {
                f.point_labels.push(i + 1);
            }
        }
        let dl = DataLabels {
            show_value: flag(d, "showVal").unwrap_or(false),
            show_series: flag(d, "showSerName").unwrap_or(false),
            show_category: flag(d, "showCatName").unwrap_or(false),
            show_percent: flag(d, "showPercent").unwrap_or(false),
            show_legend_key: flag(d, "showLegendKey").unwrap_or(false),
            show_bubble: flag(d, "showBubbleSize").unwrap_or(false),
            position: val(d, "dLblPos").and_then(|p| code_of(&POSITIONS, p)),
            number_format: d
                .child("numFmt")
                .and_then(|n| n.attr("formatCode"))
                .map(str::to_string),
        };
        if dl.show_value
            || dl.show_series
            || dl.show_category
            || dl.show_percent
            || dl.show_legend_key
            || dl.show_bubble
        {
            f.labels = Some(dl);
        }
    }
    for t in ser.children("trendline") {
        let Some(kind) = val(t, "trendlineType").and_then(|k| code_of(&TRENDS, k)) else {
            continue;
        };
        let num = |n: &str| val(t, n).and_then(|v| v.parse::<f64>().ok());
        f.trends.push(Trendline {
            kind,
            order: num("order").map_or(2, |v| v as i32),
            period: num("period").map_or(2, |v| v as i32),
            forward: num("forward").unwrap_or(0.0),
            backward: num("backward").unwrap_or(0.0),
            name: t.child("name").map(|n| n.text.clone()),
            intercept: num("intercept"),
            display_equation: flag(t, "dispEq").unwrap_or(false),
            display_r2: flag(t, "dispRSqr").unwrap_or(false),
        });
    }
    if let Some(e) = ser.child("errBars") {
        f.error_bars = Some(ErrorBars {
            end_style: if flag(e, "noEndCap") == Some(true) {
                2
            } else {
                1
            },
            ..ErrorBars::default()
        });
    }
    f
}
