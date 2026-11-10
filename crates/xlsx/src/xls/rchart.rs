use super::read::Fx;
use crate::xlsb::{esc, Dec};
use std::fmt::Write;

pub(super) type ChartRecs = Vec<(u16, Vec<u8>)>;

struct Ser {
    name_ref: Option<String>,
    name: String,
    named: bool,
    cat_ref: Option<String>,
    cat_text: bool,
    val_ref: Option<String>,
    fore: Option<u32>,
    line: Option<u32>,
    no_line: bool,
    marker: bool,

    fmt: u16,
    smooth: bool,

    imk: u16,
    msize: Option<u16>,

    lns: u16,
    we: i16,
    labels: u16,
}

struct Group {
    fmt: u16,
    axis_group: usize,
    kid: u16,
    kd: Vec<u8>,
}

#[derive(Default, Clone)]
struct Ax {
    seen: bool,
    deleted: bool,
    min: Option<f64>,
    max: Option<f64>,
    major: Option<f64>,
    minor: Option<f64>,
    reverse: bool,
    grid_major: bool,
    grid_minor: bool,
    title: Option<String>,

    cross_max: bool,

    value: bool,
}

fn f64_at(d: &[u8], i: usize) -> f64 {
    d.get(i..i + 8)
        .and_then(|b| b.try_into().ok())
        .map_or(0.0, f64::from_le_bytes)
}

fn legend_pos(d: &[u8]) -> u8 {
    let u = |i: usize| u32::from_le_bytes([d[i], d[i + 1], d[i + 2], d[i + 3]]);
    if d[16] != 7 {
        return d[16];
    }
    let (x, y, dx, dy) = (u(0), u(4), u(8), u(12));
    let vertical = d.get(18).is_some_and(|f| f & 0x10 != 0);
    if vertical {
        if x + dx / 2 < 2000 {
            4
        } else if y + dy / 2 < 1000 {
            1
        } else {
            3
        }
    } else if y + dy / 2 < 2000 {
        2
    } else {
        0
    }
}

fn formula(fx: &mut Fx, rgce: &[u8], cp: u16, idx: u32) -> Option<String> {
    match fx.text(rgce, &[], (0, 0), Some(idx), cp).ok()? {
        Dec::Text(t) => Some(t.trim_start_matches('=').to_string()),
        Dec::Exp { .. } => None,
    }
}

fn series_text(d: &[u8]) -> String {
    let (Some(&cch), Some(&flag)) = (d.get(2), d.get(3)) else {
        return String::new();
    };
    let body = d.get(4..).unwrap_or(&[]);
    if flag & 1 != 0 {
        let u: Vec<u16> = body
            .as_chunks::<2>()
            .0
            .iter()
            .take(usize::from(cch))
            .map(|c| u16::from_le_bytes(*c))
            .collect();
        String::from_utf16_lossy(&u)
    } else {
        body.iter()
            .take(usize::from(cch))
            .map(|&b| char::from(b))
            .collect()
    }
}

fn rgb(d: &[u8]) -> Option<u32> {
    let b = d.get(..3)?;
    Some(u32::from(b[0]) << 16 | u32::from(b[1]) << 8 | u32::from(b[2]))
}

fn fill_xml(c: Option<u32>, line: bool) -> String {
    match c {
        None => String::new(),
        Some(c) if line => format!(
            "<c:spPr><a:ln w=\"28575\"><a:solidFill><a:srgbClr val=\"{c:06X}\"/></a:solidFill></a:ln></c:spPr>"
        ),
        Some(c) => format!("<c:spPr><a:solidFill><a:srgbClr val=\"{c:06X}\"/></a:solidFill></c:spPr>"),
    }
}

fn ref_xml(kind: &str, f: &str) -> String {
    format!("<c:{kind}><c:f>{}</c:f></c:{kind}>", esc(f))
}

pub(super) fn to_xml(
    recs: &ChartRecs,
    fx: &mut Fx,
    cp: u16,
    idx: u32,
    what: &str,
    warnings: &mut Vec<String>,
) -> Option<String> {
    let mut depth = 0i32;
    let mut sers: Vec<Ser> = Vec::new();
    let mut in_ser = false;
    let mut ser_depth = 0;
    let mut title: Option<String> = None;
    let mut text_depth = -1i32;
    let mut text_flags = 0u16;
    let mut auto_title = false;
    let mut last_text = String::new();
    let mut legend: Option<u8> = None;
    let mut groups: Vec<Group> = Vec::new();
    let mut data_ix: Option<usize> = None;

    let mut ag_cur: Option<usize> = None;
    let mut ag_level = 0;
    let mut ax_cur: Option<usize> = None;
    let mut ax_level = 0;
    let mut alf: Option<u16> = None;
    let mut axes: [[Ax; 2]; 2] = Default::default();
    for (id, d) in recs {
        match *id {
            0x1033 => depth += 1,
            0x1034 => {
                depth -= 1;
                if in_ser && depth < ser_depth {
                    in_ser = false;
                }
                if ax_cur.is_some() && depth < ax_level {
                    ax_cur = None;
                }
                if ag_cur.is_some() && depth < ag_level {
                    ag_cur = None;
                }
            }
            0x1003 => {
                in_ser = true;
                ser_depth = depth + 1;
                sers.push(Ser {
                    name_ref: None,
                    name: String::new(),
                    named: false,
                    cat_ref: None,
                    cat_text: u16::from_le_bytes([d[0], d[1]]) == 3,
                    val_ref: None,
                    fore: None,
                    line: None,
                    no_line: false,
                    marker: false,
                    fmt: 0,
                    smooth: false,
                    imk: 0,
                    msize: None,
                    lns: 0,
                    we: 2,
                    labels: 0,
                });
            }
            0x1051 if in_ser && d.len() >= 8 => {
                let n = usize::from(u16::from_le_bytes([d[6], d[7]]));
                let f = (d[1] == 2)
                    .then(|| d.get(8..8 + n))
                    .flatten()
                    .and_then(|r| formula(fx, r, cp, idx));
                if let Some(s) = sers.last_mut() {
                    match d[0] {
                        0 => s.name_ref = f,
                        1 => s.val_ref = f,
                        2 => s.cat_ref = f,
                        _ => {}
                    }
                }
            }
            0x100D if in_ser => {
                if let Some(s) = sers.last_mut() {
                    s.named = true;
                    s.name = series_text(d);
                }
            }
            0x100D if depth == text_depth + 1 => last_text = series_text(d),
            0x1025 => {
                text_depth = depth;
                last_text.clear();
                text_flags = d
                    .get(24..26)
                    .map_or(0, |b| u16::from_le_bytes([b[0], b[1]]));
            }
            0x1027 if depth == text_depth + 1 && d.len() >= 2 => {
                let link = u16::from_le_bytes([d[0], d[1]]);
                if text_flags & 0x40 == 0 {
                    match (link, ag_cur) {
                        (1, None) => {
                            auto_title = text_flags & 0x10 != 0;
                            title = Some(std::mem::take(&mut last_text));
                        }
                        (2 | 3, Some(ag)) if !last_text.is_empty() => {
                            axes[ag][usize::from(link == 2)].title =
                                Some(std::mem::take(&mut last_text));
                        }
                        _ => {}
                    }
                }
            }
            0x1041 if d.len() >= 2 => {
                ag_cur = Some(usize::from(u16::from_le_bytes([d[0], d[1]]).min(1)));
                ag_level = depth + 1;
            }
            0x101D if ag_cur.is_some() && d.len() >= 2 => {
                ax_cur = Some(usize::from(u16::from_le_bytes([d[0], d[1]]).min(1)));
                ax_level = depth + 1;
                if let (Some(ag), Some(ax)) = (ag_cur, ax_cur) {
                    axes[ag][ax].seen = true;
                }
            }
            0x1020 if d.len() >= 8 => {
                if let (Some(ag), Some(ax)) = (ag_cur, ax_cur) {
                    let f = u16::from_le_bytes([d[6], d[7]]);
                    axes[ag][ax].reverse = f & 4 != 0;

                    axes[ag][1].cross_max = f & 2 != 0;
                }
            }
            0x101F if d.len() >= 42 => {
                if let (Some(ag), Some(ax)) = (ag_cur, ax_cur) {
                    let f = u16::from_le_bytes([d[40], d[41]]);
                    let a = &mut axes[ag][ax];
                    a.value = true;
                    let pick = |bit: u16, i: usize| (f & bit == 0).then(|| f64_at(d, i));
                    a.min = pick(1, 0);
                    a.max = pick(2, 8);
                    a.major = pick(4, 16);
                    a.minor = pick(8, 24);
                    a.reverse = f & 0x40 != 0;
                    if f & 0x80 != 0 && ax == 1 {
                        axes[ag][0].cross_max = true;
                    }
                }
            }
            0x1021 if ax_cur.is_some() && d.len() >= 2 => {
                alf = Some(u16::from_le_bytes([d[0], d[1]]));
            }
            0x1007 if ax_cur.is_some() && alf.is_some() && d.len() >= 6 => {
                let none = u16::from_le_bytes([d[4], d[5]]) == 5;
                if let (Some(ag), Some(ax)) = (ag_cur, ax_cur) {
                    let a = &mut axes[ag][ax];
                    match alf {
                        Some(0) => a.deleted = none,
                        Some(1) => a.grid_major = !none,
                        Some(2) => a.grid_minor = !none,
                        _ => {}
                    }
                }
                alf = None;
            }
            0x1006 if in_ser && d.len() >= 4 => {
                data_ix = Some(sers.len().saturating_sub(1));
            }
            0x100A if data_ix.is_some() && d.len() >= 14 => {
                let auto = u16::from_le_bytes([d[12], d[13]]) & 1 != 0;
                if let (Some(i), false) = (data_ix, auto) {
                    sers[i].fore = rgb(d);
                }
            }
            0x1007 if data_ix.is_some() && d.len() >= 8 => {
                if let Some(i) = data_ix {
                    let lns = u16::from_le_bytes([d[4], d[5]]);
                    if lns == 5 {
                        sers[i].no_line = true;
                    } else {
                        sers[i].line = rgb(d);
                        sers[i].lns = lns;
                        sers[i].we = i16::from_le_bytes([d[6], d[7]]);
                    }
                }
            }
            0x1009 if data_ix.is_some() && d.len() >= 12 => {
                if let Some(i) = data_ix {
                    let imk = u16::from_le_bytes([d[8], d[9]]);
                    let auto = u16::from_le_bytes([d[10], d[11]]) & 1 != 0;
                    sers[i].marker = imk != 0 && !auto;
                    sers[i].imk = imk;
                    if !auto && d.len() >= 20 {
                        let z = u32::from_le_bytes([d[16], d[17], d[18], d[19]]);
                        sers[i].msize = Some((z / 20).clamp(2, 72) as u16);
                    }
                }
            }
            0x105D if data_ix.is_some() && !d.is_empty() => {
                if let Some(i) = data_ix {
                    sers[i].smooth = d[0] & 1 != 0;
                }
            }
            0x100C if data_ix.is_some() && d.len() >= 2 => {
                if let Some(i) = data_ix {
                    sers[i].labels = u16::from_le_bytes([d[0], d[1]]);
                }
            }
            0x1045 => {
                if let (Some(i), true) = (data_ix, d.len() >= 2) {
                    sers[i].fmt = u16::from_le_bytes([d[0], d[1]]);
                }
                data_ix = None;
            }
            0x1014 if d.len() >= 20 => groups.push(Group {
                fmt: u16::from_le_bytes([d[18], d[19]]),
                axis_group: ag_cur.unwrap_or(0),
                kid: 0,
                kd: Vec::new(),
            }),
            0x1015 if d.len() >= 17 => legend = Some(legend_pos(d)),
            0x1017 | 0x1018 | 0x1019 | 0x101A | 0x101B | 0x103E => {
                if let Some(g) = groups.last_mut().filter(|g| g.kid == 0) {
                    g.kid = *id;
                    g.kd = d.clone();
                }
            }
            _ => {}
        }
    }
    groups.retain(|g| g.kid != 0);
    if groups.is_empty() || groups.iter().any(|g| g.kid == 0x103E) {
        warnings.push(format!("{what}: chart type not converted"));
        return None;
    }
    let mut x = String::from("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<c:chartSpace xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><c:chart>");
    if auto_title {
        x.push_str("<c:title><c:overlay val=\"0\"/></c:title><c:autoTitleDeleted val=\"0\"/>");
    } else if let Some(t) = &title {
        let _ = write!(
            x,
            "<c:title>{}<c:overlay val=\"0\"/></c:title><c:autoTitleDeleted val=\"0\"/>",
            rich(t)
        );
    } else {
        x.push_str("<c:autoTitleDeleted val=\"1\"/>");
    }
    x.push_str("<c:plotArea><c:layout/>");
    let first_scatter = groups[0].kid == 0x101B;
    let mut used_ag = [false; 2];
    let first_fmt = groups[0].fmt;
    for g in &groups {
        let flags = |at: usize| {
            g.kd.get(at..at + 2)
                .map_or(0, |b| u16::from_le_bytes([b[0], b[1]]))
        };
        let (tag, open, with_axes) = match g.kid {
            0x1017 => {
                let f = flags(4);
                let grouping = if f & 4 != 0 {
                    "percentStacked"
                } else if f & 2 != 0 {
                    "stacked"
                } else {
                    "clustered"
                };
                (
                    "barChart",
                    format!(
                        "<c:barDir val=\"{}\"/><c:grouping val=\"{grouping}\"/><c:varyColors val=\"0\"/>",
                        if f & 1 != 0 { "bar" } else { "col" }
                    ),
                    true,
                )
            }
            0x1018 | 0x101A => {
                let f = flags(0);
                let grouping = if f & 2 != 0 {
                    "percentStacked"
                } else if f & 1 != 0 {
                    "stacked"
                } else {
                    "standard"
                };
                (
                    if g.kid == 0x1018 {
                        "lineChart"
                    } else {
                        "areaChart"
                    },
                    format!("<c:grouping val=\"{grouping}\"/><c:varyColors val=\"0\"/>"),
                    true,
                )
            }
            0x1019 => {
                if flags(2) > 0 {
                    (
                        "doughnutChart",
                        "<c:varyColors val=\"1\"/>".to_string(),
                        false,
                    )
                } else {
                    ("pieChart", "<c:varyColors val=\"1\"/>".to_string(), false)
                }
            }
            _ => (
                "scatterChart",
                "<c:scatterStyle val=\"lineMarker\"/><c:varyColors val=\"0\"/>".to_string(),
                true,
            ),
        };
        let scatter = g.kid == 0x101B;
        let line_like = matches!(g.kid, 0x1018 | 0x101B);
        let _ = write!(x, "<c:{tag}>{open}");
        for (i, s) in sers.iter().enumerate() {
            let mine = if groups.iter().any(|h| h.fmt == s.fmt) {
                s.fmt == g.fmt
            } else {
                g.fmt == first_fmt
            };
            if !mine {
                continue;
            }
            let _ = write!(x, "<c:ser><c:idx val=\"{i}\"/><c:order val=\"{i}\"/>");
            if let Some(f) = &s.name_ref {
                let _ = write!(x, "<c:tx>{}</c:tx>", ref_xml("strRef", f));
            } else if s.named || !s.name.is_empty() {
                let _ = write!(x, "<c:tx><c:v>{}</c:v></c:tx>", esc(&s.name));
            }
            if line_like && s.no_line {
                x.push_str("<c:spPr><a:ln><a:noFill/></a:ln></c:spPr>");
            } else if line_like {
                x.push_str(&line_xml(s.line.or(s.fore), s.lns, s.we));
            } else {
                x.push_str(&fill_xml(s.fore, false));
            }
            if line_like {
                let sym = if s.marker { marker_name(s.imk) } else { "none" };
                match s.msize.filter(|_| s.marker) {
                    Some(z) => {
                        let _ = write!(
                            x,
                            "<c:marker><c:symbol val=\"{sym}\"/><c:size val=\"{z}\"/></c:marker>"
                        );
                    }
                    None => {
                        let _ = write!(x, "<c:marker><c:symbol val=\"{sym}\"/></c:marker>");
                    }
                }
            }
            if s.labels != 0 {
                let b = |bit: u16| u8::from(s.labels & bit != 0);
                let _ = write!(
                    x,
                    "<c:dLbls><c:showLegendKey val=\"0\"/><c:showVal val=\"{}\"/><c:showCatName val=\"{}\"/><c:showSerName val=\"{}\"/><c:showPercent val=\"{}\"/><c:showBubbleSize val=\"{}\"/></c:dLbls>",
                    b(1),
                    b(0x10),
                    b(0x40),
                    b(2),
                    b(0x20)
                );
            }
            if let Some(f) = &s.cat_ref {
                let k = if s.cat_text && !scatter {
                    "strRef"
                } else {
                    "numRef"
                };
                let t = if scatter { "xVal" } else { "cat" };
                let _ = write!(x, "<c:{t}>{}</c:{t}>", ref_xml(k, f));
            }
            if let Some(f) = &s.val_ref {
                let t = if scatter { "yVal" } else { "val" };
                let _ = write!(x, "<c:{t}>{}</c:{t}>", ref_xml("numRef", f));
            }
            if line_like && s.smooth {
                x.push_str("<c:smooth val=\"1\"/>");
            }
            x.push_str("</c:ser>");
        }
        if with_axes {
            let ag = g.axis_group.min(1);
            used_ag[ag] = true;
            let (c, v) = if ag == 0 { (111, 222) } else { (333, 444) };
            let _ = write!(x, "<c:axId val=\"{c}\"/><c:axId val=\"{v}\"/>");
        } else if tag == "doughnutChart" {
            x.push_str("<c:holeSize val=\"50\"/>");
        }
        let _ = write!(x, "</c:{tag}>");
    }
    for ag in 0..2 {
        if !used_ag[ag] {
            continue;
        }
        let (c, v) = if ag == 0 { (111, 222) } else { (333, 444) };
        let [ca, va] = &axes[ag];
        let cat_tag = if first_scatter && ag == 0 {
            "valAx"
        } else {
            "catAx"
        };
        let sec = ag == 1;
        let cat_max = sec && !ca.deleted;
        x.push_str(&axis_xml(
            cat_tag,
            c,
            v,
            ca,
            if cat_max { "t" } else { "b" },
            ca.grid_major,
            cat_max,
        ));
        x.push_str(&axis_xml(
            "valAx",
            v,
            c,
            va,
            if sec { "r" } else { "l" },
            va.grid_major,
            sec,
        ));
    }
    x.push_str("</c:plotArea>");
    if let Some(w) = legend {
        let pos = match w {
            0 => "b",
            1 => "tr",
            2 => "t",
            4 => "l",
            _ => "r",
        };
        let _ = write!(x, "<c:legend><c:legendPos val=\"{pos}\"/></c:legend>");
    }
    x.push_str("<c:plotVisOnly val=\"1\"/></c:chart></c:chartSpace>");
    Some(x)
}

fn axis_xml(
    tag: &str,
    id: u32,
    cross: u32,
    a: &Ax,
    pos: &str,
    grid: bool,
    crosses_max: bool,
) -> String {
    let orient = if a.reverse { "maxMin" } else { "minMax" };
    let mut o =
        format!("<c:{tag}><c:axId val=\"{id}\"/><c:scaling><c:orientation val=\"{orient}\"/>");
    if let Some(m) = a.max {
        let _ = write!(o, "<c:max val=\"{m}\"/>");
    }
    if let Some(m) = a.min {
        let _ = write!(o, "<c:min val=\"{m}\"/>");
    }
    let _ = write!(
        o,
        "</c:scaling><c:delete val=\"{}\"/><c:axPos val=\"{pos}\"/>",
        u8::from(a.deleted)
    );
    if grid {
        o.push_str("<c:majorGridlines/>");
    }
    if a.grid_minor {
        o.push_str("<c:minorGridlines/>");
    }
    if let Some(t) = &a.title {
        let _ = write!(o, "<c:title>{}<c:overlay val=\"0\"/></c:title>", rich(t));
    }
    let _ = write!(o, "<c:crossAx val=\"{cross}\"/>");
    if crosses_max {
        o.push_str("<c:crosses val=\"max\"/>");
    }
    if let Some(m) = a.major {
        let _ = write!(o, "<c:majorUnit val=\"{m}\"/>");
    }
    if let Some(m) = a.minor {
        let _ = write!(o, "<c:minorUnit val=\"{m}\"/>");
    }
    let _ = write!(o, "</c:{tag}>");
    o
}

fn rich(t: &str) -> String {
    format!(
        "<c:tx><c:rich><a:bodyPr/><a:p><a:r><a:t>{}</a:t></a:r></a:p></c:rich></c:tx>",
        esc(t)
    )
}

fn marker_name(imk: u16) -> &'static str {
    match imk {
        1 => "square",
        2 => "diamond",
        3 => "triangle",
        4 => "x",
        5 => "star",
        6 => "dot",
        7 => "dash",
        8 => "circle",
        9 => "plus",
        _ => "none",
    }
}

fn line_xml(c: Option<u32>, lns: u16, we: i16) -> String {
    let w = match we {
        -1 => 3175,
        0 => 9525,
        1 => 19050,
        _ => 28575,
    };
    let dash = match lns {
        1 => "<a:prstDash val=\"dash\"/>",
        2 => "<a:prstDash val=\"sysDot\"/>",
        3 => "<a:prstDash val=\"dashDot\"/>",
        4 => "<a:prstDash val=\"lgDashDotDot\"/>",
        _ => "",
    };
    match c {
        Some(c) => format!(
            "<c:spPr><a:ln w=\"{w}\"><a:solidFill><a:srgbClr val=\"{c:06X}\"/></a:solidFill>{dash}</a:ln></c:spPr>"
        ),
        None if !dash.is_empty() || we != 2 => {
            format!("<c:spPr><a:ln w=\"{w}\">{dash}</a:ln></c:spPr>")
        }
        None => String::new(),
    }
}
