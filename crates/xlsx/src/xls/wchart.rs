use crate::xml::Node;

#[derive(Clone, Copy, PartialEq, Debug)]
pub(super) enum Kind {
    Bar,
    Line,
    Area,
    Pie,
    Scatter,
}

pub(super) struct SeriesIn {
    pub name_ref: Option<String>,
    pub name: String,

    pub has_name: bool,
    pub cat_ref: Option<String>,
    pub cat_text: bool,
    pub val_ref: Option<String>,
    pub color: Option<u32>,
    pub no_line: bool,
    pub cat_n: usize,
    pub val_n: usize,

    pub group: usize,
    pub smooth: bool,

    pub marker: Option<(u16, u16)>,

    pub dash: Option<u16>,
    pub weight: Option<i16>,

    pub labels: u16,
}

pub(super) struct GroupIn {
    pub kind: Kind,
    pub horiz: bool,
    pub stacked: bool,
    pub percent: bool,
    pub markers: bool,
    pub donut: bool,
    pub secondary: bool,
}

#[derive(Default, Clone)]
pub(super) struct AxisIn {
    pub title: Option<String>,
    pub min: Option<f64>,
    pub max: Option<f64>,
    pub major: Option<f64>,
    pub minor: Option<f64>,
    pub reverse: bool,
    pub grid_major: bool,
    pub grid_minor: bool,
    pub deleted: bool,
}

pub(super) struct ChartIn {
    pub groups: Vec<GroupIn>,
    pub series: Vec<SeriesIn>,

    pub axes: [[AxisIn; 2]; 2],
    pub title: Option<String>,

    pub auto_title: bool,

    pub title_deleted: bool,

    pub legend: Option<u8>,
}

impl ChartIn {
    fn secondary(&self) -> bool {
        self.groups.iter().any(|g| g.secondary)
    }
}

fn texts(n: &Node, out: &mut String) {
    if n.name == "t" || n.name == "v" {
        out.push_str(&n.text);
    }
    for k in &n.kids {
        texts(k, out);
    }
}

fn ref_cells(f: &str) -> usize {
    let body = f.rsplit_once('!').map_or(f, |x| x.1).replace('$', "");
    let cell = |s: &str| -> Option<(u32, u32)> {
        let n = s.bytes().take_while(u8::is_ascii_alphabetic).count();
        let col = s[..n]
            .to_ascii_uppercase()
            .bytes()
            .fold(0u32, |c, b| c * 26 + u32::from(b - b'A' + 1));
        Some((s[n..].parse().ok()?, col))
    };
    let (a, b) = body.split_once(':').unwrap_or((&body, &body));
    match (cell(a), cell(b)) {
        (Some(p), Some(q)) => ((q.0.abs_diff(p.0) + 1) * (q.1.abs_diff(p.1) + 1)) as usize,
        _ => 0,
    }
}

fn ref_of(n: &Node) -> Option<(String, bool)> {
    for k in &n.kids {
        match k.name.as_str() {
            "strRef" | "numRef" => {
                let f = k.child("f")?.text.trim().to_string();
                return Some((f, k.name == "strRef"));
            }
            _ => {}
        }
    }
    None
}

fn kind_of(name: &str) -> Option<Kind> {
    Some(match name {
        "barChart" => Kind::Bar,
        "lineChart" => Kind::Line,
        "areaChart" => Kind::Area,
        "pieChart" | "doughnutChart" => Kind::Pie,
        "scatterChart" => Kind::Scatter,
        _ => return None,
    })
}

fn val_of<'a>(n: &'a Node, name: &str) -> Option<&'a str> {
    n.child(name)?.attr("val")
}

fn on(v: Option<&str>) -> bool {
    matches!(v, Some("1" | "true"))
}

fn axis_in(ax: &Node) -> AxisIn {
    let num = |parent: &Node, n: &str| val_of(parent, n).and_then(|v| v.parse::<f64>().ok());
    let sc = ax.child("scaling");
    let mut title = None;
    if let Some(t) = ax.child("title") {
        let mut s = String::new();
        texts(t, &mut s);
        if !s.is_empty() {
            title = Some(s);
        }
    }
    AxisIn {
        title,
        min: sc.and_then(|s| num(s, "min")),
        max: sc.and_then(|s| num(s, "max")),
        major: num(ax, "majorUnit"),
        minor: num(ax, "minorUnit"),
        reverse: sc.is_some_and(|s| val_of(s, "orientation") == Some("maxMin")),
        grid_major: ax.child("majorGridlines").is_some(),
        grid_minor: ax.child("minorGridlines").is_some(),
        deleted: on(val_of(ax, "delete")),
    }
}

fn marker_type(sym: &str) -> Option<u16> {
    Some(match sym {
        "none" => 0,
        "square" => 1,
        "diamond" => 2,
        "triangle" => 3,
        "x" => 4,
        "star" => 5,
        "dot" => 6,
        "dash" => 7,
        "circle" => 8,
        "plus" => 9,
        _ => return None,
    })
}

fn dash_type(d: &str) -> u16 {
    match d {
        "dash" | "lgDash" | "sysDash" => 1,
        "sysDot" => 2,
        "dashDot" | "lgDashDot" | "sysDashDot" => 3,
        "lgDashDotDot" | "sysDashDotDot" => 4,
        _ => 0,
    }
}

fn label_flags(s: &Node) -> u16 {
    let Some(d) = s.child("dLbls") else { return 0 };
    let f = |n: &str, bit: u16| if on(val_of(d, n)) { bit } else { 0 };
    f("showVal", 1)
        | f("showPercent", 2)
        | f("showCatName", 0x10)
        | f("showBubbleSize", 0x20)
        | f("showSerName", 0x40)
}

pub(super) fn parse(
    space: &Node,
    warnings: &mut Vec<String>,
    what: &str,
    theme: &[u32],
) -> Option<ChartIn> {
    let chart = space.child("chart")?;
    let plot = chart.child("plotArea")?;
    let els: Vec<&Node> = plot
        .kids
        .iter()
        .filter(|k| k.name.ends_with("Chart"))
        .collect();
    let first = els.first()?;
    if kind_of(&first.name).is_none() {
        warnings.push(format!("{what}: chart type {} not carried", first.name));
        return None;
    }
    let ids = |el: &Node| -> Vec<String> {
        el.children("axId")
            .filter_map(|a| a.attr("val").map(str::to_string))
            .collect()
    };
    let primary = ids(first);
    let mut groups: Vec<GroupIn> = Vec::new();
    let mut series: Vec<(f64, SeriesIn)> = Vec::new();
    let mut sec_ids: Vec<String> = Vec::new();
    for el in &els {
        let Some(kind) = kind_of(&el.name) else {
            warnings.push(format!("{what}: chart group {} not carried", el.name));
            continue;
        };
        if kind == Kind::Pie && !groups.is_empty()
            || groups.first().is_some_and(|g| g.kind == Kind::Pie)
        {
            warnings.push(format!(
                "{what}: a pie chart cannot share a chart, group {} not carried",
                el.name
            ));
            continue;
        }
        let eid = ids(el);
        let secondary = !eid.is_empty() && eid.first() != primary.first();
        if secondary && sec_ids.is_empty() {
            sec_ids = eid;
        }
        let grouping = val_of(el, "grouping").unwrap_or("");
        let gi = groups.len();
        groups.push(GroupIn {
            kind,
            horiz: val_of(el, "barDir") == Some("bar"),
            stacked: matches!(grouping, "stacked" | "percentStacked"),
            percent: grouping == "percentStacked",
            donut: el.name == "doughnutChart",
            secondary,
            markers: kind == Kind::Scatter
                || el
                    .children("ser")
                    .next()
                    .and_then(|s| s.child("marker")?.child("symbol"))
                    .is_none_or(|m| m.attr("val") != Some("none")),
        });
        for (si, s) in el.children("ser").enumerate() {
            let (cat_key, val_key) = if kind == Kind::Scatter {
                ("xVal", "yVal")
            } else {
                ("cat", "val")
            };
            let tx = s.child("tx").and_then(ref_of);
            let mut name = String::new();
            if let Some(t) = s.child("tx") {
                texts(t, &mut name);
            }
            let cat = s.child(cat_key).and_then(ref_of);
            let valr = s.child(val_key).and_then(ref_of);
            let color = s
                .child("spPr")
                .and_then(|p| {
                    p.child("solidFill")
                        .or_else(|| p.child("ln")?.child("solidFill"))
                })
                .and_then(|f| super::wcolor::resolve(f.kids.first()?, theme));
            let ln = s.child("spPr").and_then(|p| p.child("ln"));
            let marker = s.child("marker").and_then(|m| {
                let sym = val_of(m, "symbol").and_then(marker_type);
                let size = val_of(m, "size").and_then(|v| v.parse::<u16>().ok());
                match (sym, size) {
                    (None, None) => None,
                    (a, b) => Some((a.unwrap_or(2), b.map_or(100, |z| z.clamp(2, 72) * 20))),
                }
            });
            let order = val_of(s, "order")
                .and_then(|v| v.parse::<f64>().ok())
                .unwrap_or(si as f64);
            series.push((
                order,
                SeriesIn {
                    cat_n: cat.as_ref().map_or(0, |c| ref_cells(&c.0)),
                    val_n: valr.as_ref().map_or(0, |c| ref_cells(&c.0)),
                    name_ref: tx.map(|t| t.0),
                    name,
                    has_name: s.child("tx").is_some(),
                    cat_text: cat.as_ref().is_some_and(|c| c.1),
                    cat_ref: cat.map(|c| c.0),
                    val_ref: valr.map(|c| c.0),
                    color,
                    no_line: ln.is_some_and(|l| l.child("noFill").is_some()),
                    group: gi,
                    smooth: on(val_of(s, "smooth")),
                    marker,
                    dash: ln.and_then(|l| val_of(l, "prstDash")).map(dash_type),
                    weight: ln.and_then(|l| l.attr("w")?.parse::<i64>().ok()).map(|w| {
                        if w <= 3175 {
                            -1
                        } else if w <= 9525 {
                            0
                        } else if w <= 19050 {
                            1
                        } else {
                            2
                        }
                    }),
                    labels: label_flags(s),
                },
            ));
        }
    }
    series.sort_by(|a, b| a.0.total_cmp(&b.0));
    let series: Vec<SeriesIn> = series.into_iter().map(|s| s.1).collect();
    let nodes: Vec<&Node> = plot
        .kids
        .iter()
        .filter(|k| matches!(k.name.as_str(), "catAx" | "valAx" | "dateAx" | "serAx"))
        .collect();
    let find = |id: Option<&String>| -> AxisIn {
        id.and_then(|id| {
            nodes
                .iter()
                .find(|a| val_of(a, "axId") == Some(id.as_str()))
        })
        .map(|a| axis_in(a))
        .unwrap_or_default()
    };
    let axes = [
        [find(primary.first()), find(primary.get(1))],
        [find(sec_ids.first()), find(sec_ids.get(1))],
    ];
    let mut title = None;
    let mut auto_title = false;
    let mut title_deleted = false;
    if let Some(t) = chart.child("title") {
        let mut s = String::new();
        texts(t, &mut s);
        if s.is_empty() {
            auto_title = true;
        } else {
            title = Some(s);
        }
    } else if series.len() == 1 {
        let deleted = on(val_of(chart, "autoTitleDeleted"));
        auto_title = !deleted;
        title_deleted = deleted;
    }
    let legend = chart
        .child("legend")
        .map(|l| match val_of(l, "legendPos").unwrap_or("r") {
            "b" => 0,
            "tr" => 1,
            "t" => 2,
            "l" => 4,
            _ => 3,
        });
    Some(ChartIn {
        groups,
        series,
        axes,
        title,
        auto_title,
        title_deleted,
        legend,
    })
}

fn hex(s: &str) -> Vec<u8> {
    (0..s.len() / 2)
        .map(|i| u8::from_str_radix(&s[2 * i..2 * i + 2], 16).unwrap_or(0))
        .collect()
}

struct W(Vec<u8>);

impl W {
    fn rec(&mut self, id: u16, d: &[u8]) {
        self.0.extend(id.to_le_bytes());
        self.0.extend((d.len() as u16).to_le_bytes());
        self.0.extend(d);
    }
    fn h(&mut self, id: u16, s: &str) {
        self.rec(id, &hex(s));
    }
    fn begin(&mut self) {
        self.rec(0x1033, &[]);
    }
    fn end(&mut self) {
        self.rec(0x1034, &[]);
    }
}

fn text_rec(w: &mut W, id: u16, s: &str) {
    let wide = true;
    let mut d = Vec::new();
    d.extend(0u16.to_le_bytes());
    d.push(s.encode_utf16().count().min(255) as u8);
    d.push(u8::from(wide));
    for u in s.encode_utf16().take(255) {
        if wide {
            d.extend(u.to_le_bytes());
        } else {
            d.push(u as u8);
        }
    }
    w.rec(id, &d);
}

const PALETTE: [u32; 8] = [
    0x99_99FF, 0x99_3366, 0xFF_FFCC, 0xCC_FFFF, 0x66_0066, 0xFF_8080, 0x00_66CC, 0xCC_CCFF,
];

fn ai(w: &mut W, id: u8, rgce: Option<&[u8]>) {
    let mut d = vec![id, if rgce.is_some() { 2 } else { 1 }, 0, 0, 0, 0];
    let r = rgce.unwrap_or(&[]);
    d.extend((r.len() as u16).to_le_bytes());
    d.extend(r);
    w.rec(0x1051, &d);
}

const TICK_SHOWN: &str = "020003010000000000000000000000000000000000000000000008000000";
const TICK_VAL: &str = "020003010000000000000000000000000000000000000000200008000000";
const TICK_HIDDEN: &str = "020003010000000000000000000000000000000000000000230008000000";
const LINE_SHOWN: &str = "808080000000000004001700";
const LINE_HIDDEN: &str = "000000000500ffff08004d00";

fn axis_block(w: &mut W, which: u16, a: &AxisIn, value_scale: bool, cross_max: bool) {
    let mut d = vec![0u8; 18];
    d[..2].copy_from_slice(&which.to_le_bytes());
    w.rec(0x101D, &d);
    w.begin();
    if value_scale {
        let mut v = Vec::new();
        for x in [a.min, a.max, a.major, a.minor] {
            v.extend(x.unwrap_or(0.0).to_le_bytes());
        }
        v.extend(0.0f64.to_le_bytes());
        let mut fl = 0x0100u16;
        for (bit, x) in [(1, a.min), (2, a.max), (4, a.major), (8, a.minor)] {
            if x.is_none() {
                fl |= bit;
            }
        }
        fl |= 0x10;
        if cross_max {
            fl |= 0x80;
        }
        if a.reverse {
            fl |= 0x40;
        }
        v.extend(fl.to_le_bytes());
        w.rec(0x101F, &v);
        w.h(
            0x101E,
            if a.deleted {
                TICK_HIDDEN
            } else if which == 1 {
                TICK_VAL
            } else {
                TICK_SHOWN
            },
        );
    } else {
        let fl = 1u16 | if cross_max { 2 } else { 0 } | if a.reverse { 4 } else { 0 };
        let mut v = hex("010001000100");
        v.extend(fl.to_le_bytes());
        w.rec(0x1020, &v);
        w.h(0x1062, "00000000010000000100000000000000ef00");
        w.h(0x101E, if a.deleted { TICK_HIDDEN } else { TICK_SHOWN });
    }
    if !a.deleted {
        w.h(0x1026, "0000");
    }
    w.h(0x1021, "0000");
    w.h(0x1007, if a.deleted { LINE_HIDDEN } else { LINE_SHOWN });
    if value_scale {
        if a.grid_major {
            w.h(0x1021, "0100");
            w.h(0x1007, "808080000000000000001700");
        }
        if a.grid_minor {
            w.h(0x1021, "0200");
            w.h(0x1007, "c0c0c0000000000000001600");
        }
    }
    w.end();
}

fn axis_title(w: &mut W, link: u16, secondary: bool, text: &str) {
    let body = match (link, secondary) {
        (3, _) => "02020100000000004d070000950c00001a01000076010000000008000a000000",
        (_, false) => "020201000000000000000000d8050000e600000080010000000208000a005a00",
        (_, true) => "0202010000000000ba0e0000a4050000e6000000f3010000000208000a005a00",
    };
    w.h(0x1025, body);
    w.begin();
    w.h(0x104F, "02000200d1ffffff000000002c00000024000000");
    w.h(0x1026, "0000");
    w.h(0x1051, "0001000000000000");
    text_rec(w, 0x100D, text);
    w.h(0x1032, "00000300");
    w.begin();
    w.h(0x1007, "000000000500ffff08004d00");
    w.h(0x100A, "ffffff0000000000000000004e004d00");
    w.end();
    w.rec(0x1027, &[link as u8, (link >> 8) as u8, 0, 0, 0, 0]);
    w.end();
}

pub(super) fn substream(
    c: &ChartIn,
    enc: &mut dyn FnMut(&str) -> Option<Vec<u8>>,
    dgid: u32,
    size: (f64, f64),
) -> Vec<u8> {
    let mut w = W(Vec::new());
    w.h(0x809, "000620005a4fcd07c100020006080000");

    w.h(0x850, "500800000e0e040050085a08610861086a086b089d08a608");
    w.h(0x14, "");
    w.h(0x15, "");
    w.h(0x83, "0000");
    w.h(0x84, "0000");
    w.h(0x26, "666666666666e63f");
    w.h(0x27, "666666666666e63f");
    w.h(0x28, "000000000000e83f");
    w.h(0x29, "000000000000e83f");
    w.h(
        0xA1,
        "00001200010001000100040000000000333333333333d33f333333333333d33f0f00",
    );
    w.h(0x33, "0300");
    w.h(0x12, "0000");

    {
        let base = dgid * 1024;
        let mut dg = Vec::new();
        dg.extend(1u32.to_le_bytes());
        dg.extend(base.to_le_bytes());
        let mut sp = Vec::new();
        sp.extend(base.to_le_bytes());
        sp.extend(5u32.to_le_bytes());
        let mut pat = super::wdraw::escher(1, 0, 0xF009, &[0u8; 16]);
        pat.extend(super::wdraw::escher(2, 0, 0xF00A, &sp));
        let group = super::wdraw::escher(15, 0, 0xF004, &pat);
        let mut spgr = super::wdraw::escher(15, 0, 0xF003, &group);
        let mut body = super::wdraw::escher(0, dgid as u16, 0xF008, &dg);
        body.append(&mut spgr);
        w.rec(0xEC, &super::wdraw::escher(15, 0, 0xF002, &body));
    }
    w.h(0x1001, "0000");
    let mut ch = vec![0u8; 8];
    ch.extend((((size.0 * 65536.0) as u32) + 0x8000).to_le_bytes());
    ch.extend((((size.1 * 65536.0) as u32) + 0x8000).to_le_bytes());
    w.rec(0x1002, &ch);
    w.begin();
    w.h(0xA0, "01000100");
    w.h(0x1064, "0000010000000100");
    w.h(0x1032, "00000200");
    w.begin();
    w.h(0x1007, "808080000000000000001700");
    w.h(0x100A, "ffffff00000000000100000009004d00");
    w.end();
    for (i, s) in c.series.iter().enumerate() {
        let g = &c.groups[s.group];
        let line_like = matches!(g.kind, Kind::Line | Kind::Scatter);
        let col = s.color.unwrap_or(PALETTE[i % 8]);
        let rgb = [(col >> 16) as u8, (col >> 8) as u8, col as u8, 0];
        let mut d = Vec::new();
        d.extend((if s.cat_text { 3u16 } else { 1 }).to_le_bytes());
        d.extend(1u16.to_le_bytes());
        d.extend((s.cat_n as u16).to_le_bytes());
        d.extend((s.val_n as u16).to_le_bytes());
        d.extend(1u16.to_le_bytes());
        d.extend(0u16.to_le_bytes());
        w.rec(0x1003, &d);
        w.begin();
        let name_ptg = s.name_ref.as_deref().and_then(&mut *enc);
        ai(&mut w, 0, name_ptg.as_deref());
        if s.has_name {
            text_rec(&mut w, 0x100D, &s.name);
        }
        let cat_ptg = s.cat_ref.as_deref().and_then(&mut *enc);
        let val_ptg = s.val_ref.as_deref().and_then(&mut *enc);
        ai(&mut w, 1, val_ptg.as_deref());
        ai(&mut w, 2, cat_ptg.as_deref());
        ai(&mut w, 3, None);
        let mut d = vec![0xFF, 0xFF];
        d.extend((i as u16).to_le_bytes());
        d.extend((i as u16).to_le_bytes());
        d.extend(0u16.to_le_bytes());
        w.rec(0x1006, &d);
        w.begin();
        w.h(0x105F, "0000");
        let mut l = rgb.to_vec();
        if line_like {
            l.extend(s.dash.unwrap_or(0).to_le_bytes());
            l.extend(s.weight.unwrap_or(2).to_le_bytes());
            l.extend([0, 0, 0x4D, 0]);
            if s.no_line {
                l[4] = 5;
            }
        } else {
            l.extend(hex("0500ffff08004d00"));
            l[..3].copy_from_slice(&[0, 0, 0]);
        }
        w.rec(0x1007, &l);
        let mut a = rgb.to_vec();
        a.extend(hex("ffffff00"));
        a.extend(if line_like { 0u16 } else { 1 }.to_le_bytes());
        a.extend(hex("00004e004d00"));
        w.rec(0x100A, &a);
        w.h(0x100B, "0000");
        if s.smooth && line_like {
            w.h(0x105D, "0100");
        }
        let (imk, size, auto) = match s.marker {
            Some((t, z)) if line_like => (t, z, 0u16),
            _ => (u16::from(g.markers && line_like) * 2, 60, 0),
        };
        let mut m = rgb.to_vec();
        m.extend(rgb);
        m.extend(imk.to_le_bytes());
        m.extend(auto.to_le_bytes());
        m.extend(hex("4d004d00"));
        m.extend(u32::from(size).to_le_bytes());
        w.rec(0x1009, &m);
        if s.labels != 0 {
            w.rec(0x100C, &s.labels.to_le_bytes());
        }
        w.end();
        w.rec(0x1045, &(s.group as u16).to_le_bytes());
        w.end();
    }
    w.h(0x1044, "1b000000");
    for (id, flag) in [("0200", "30"), ("0300", "b1")] {
        w.h(0x1024, id);
        w.h(
            0x1025,
            &format!("0202010000000000cdffffffadffffff0000000000000000{flag}00080000000000"),
        );
        w.begin();
        w.h(0x104F, "0200020000000000000000000000000000000000");
        w.h(0x1026, "0000");
        w.h(0x1051, "0001000000000000");
        w.end();
    }
    let n_axis_groups = if c.secondary() { 2u16 } else { 1 };
    w.rec(0x1046, &n_axis_groups.to_le_bytes());
    let pie = c.groups.first().is_some_and(|g| g.kind == Kind::Pie);
    for ag in 0..usize::from(n_axis_groups) {
        let groups: Vec<usize> = (0..c.groups.len())
            .filter(|&i| c.groups[i].secondary == (ag == 1))
            .collect();
        if groups.is_empty() {
            continue;
        }
        let scatter = c.groups[groups[0]].kind == Kind::Scatter;
        let mut d = (ag as u16).to_le_bytes().to_vec();
        d.extend(hex("1301000016030000e60b0000c10a0000"));
        w.rec(0x1041, &d);
        w.begin();
        if ag == 0 {
            w.h(0x104F, "020002002100000059020000da0c00006e0d0000");
        } else {
            w.h(0x104F, "0200020000000000000000000000000000000000");
        }
        if !pie {
            let cm = ag == 1 && !c.axes[1][0].deleted;
            axis_block(&mut w, 0, &c.axes[ag][0], scatter, cm);
            axis_block(&mut w, 1, &c.axes[ag][1], true, cm);
            if let Some(t) = &c.axes[ag][0].title {
                axis_title(&mut w, 3, ag == 1, t);
            }
            if let Some(t) = &c.axes[ag][1].title {
                axis_title(&mut w, 2, ag == 1, t);
            }
        }
        if ag == 0 {
            w.rec(0x1035, &[]);
            w.h(0x1032, "00000300");
            w.begin();
            w.h(0x1007, "000000000500ffff08004d00");
            w.h(0x100A, "ffffff00000000000100000009004d00");
            w.end();
        }
        for &gi in &groups {
            chart_format(&mut w, c, gi, gi == 0, size);
        }
        w.end();
    }
    if c.title_deleted {
        w.h(
            0x1025,
            "0202010000000000beffffffa8ffffff00000000000000007000080000000000",
        );
        w.begin();
        w.h(0x104F, "0200020000000000000000000000000000000000");
        w.h(0x1051, "0001000000000000");
        w.h(0x1027, "010000000000");
        w.end();
    }
    if c.auto_title || c.title.is_some() {
        let auto = c.title.is_none();
        w.h(
            0x1025,
            if auto {
                "0202010000000000e60500002a000000c6030000b8020000100008000a000000"
            } else {
                "0202010000000000e60500002a000000c6030000b8020000000008000a000000"
            },
        );
        w.begin();
        w.h(0x104F, "02000200faffffffd6ffffff9700000043000000");
        w.h(0x1026, "0000");
        w.h(0x1051, "0001000000000000");
        if let Some(t) = &c.title {
            text_rec(&mut w, 0x100D, t);
        }
        w.h(0x1032, "00000300");
        w.begin();
        w.h(0x1007, "000000000500ffff08004d00");
        w.h(0x100A, "ffffff0000000000000000004e004d00");
        w.end();
        w.h(0x1027, "010000000000");
        w.end();
    }
    w.end();
    w.h(0x200, "0000000004000000000002000000");
    for k in [2u8, 1, 3] {
        w.rec(0x1065, &[k, 0]);
    }
    w.rec(0x0A, &[]);
    w.0
}

fn chart_format(w: &mut W, c: &ChartIn, gi: usize, with_legend: bool, size: (f64, f64)) {
    let g = &c.groups[gi];
    let varied = u16::from(g.kind == Kind::Pie);
    let mut cf = vec![0u8; 16];
    cf.extend(varied.to_le_bytes());
    cf.extend((gi as u16).to_le_bytes());
    w.rec(0x1014, &cf);
    w.begin();
    match g.kind {
        Kind::Bar => {
            let overlap: i16 = if g.stacked { 100 } else { 0 };
            let mut d = Vec::new();
            d.extend(overlap.to_le_bytes());
            d.extend(150u16.to_le_bytes());
            let fl = u16::from(g.horiz) | u16::from(g.stacked) << 1 | u16::from(g.percent) << 2;
            d.extend(fl.to_le_bytes());
            w.rec(0x1017, &d);
        }
        Kind::Line => {
            let fl = u16::from(g.stacked) | u16::from(g.percent) << 1;
            w.rec(0x1018, &fl.to_le_bytes());
        }
        Kind::Area => {
            let fl = u16::from(g.stacked) | u16::from(g.percent) << 1;
            w.rec(0x101A, &fl.to_le_bytes());
        }
        Kind::Pie => w.h(
            0x1019,
            if g.donut {
                "000032000000"
            } else {
                "000000000000"
            },
        ),
        Kind::Scatter => w.h(0x101B, "640001000000"),
    }
    w.h(0x1022, "00000000000000000f00");
    if gi > 0 {
        let mut d = (c.series.len() as u16).to_le_bytes().to_vec();
        for i in 1..=c.series.len() {
            d.extend((i as u16).to_le_bytes());
        }
        w.rec(0x1016, &d);
    }
    if let (true, Some(pos)) = (with_legend, c.legend) {
        legend(w, pos, size);
    }
    w.end();
}

fn legend(w: &mut W, pos: u8, size: (f64, f64)) {
    let (x, y, dx, dy, vert) = match pos {
        0 => (1318u32, 3564u32, 1357u32, 374u32, false),
        2 => (1318, 90, 1357, 374, false),
        4 => (26, 1610, 755, 748, true),
        1 => (3302, 90, 666, 748, true),
        _ => (3469, 1932, 499, 748, true),
    };

    let px = |pt: f64, units: u32| {
        (f64::from(units) * (2.0 * (pt + 0.5) - 16.0) / 4000.0).round() as u32
    };
    let (fw, fh) = (px(size.0, dx), px(size.1, dy));
    let mut d = Vec::new();
    for v in [x, y, dx, dy] {
        d.extend(v.to_le_bytes());
    }
    d.push(7);
    d.push(1);
    d.extend((if vert { 0x12u16 } else { 0x02 }).to_le_bytes());
    w.rec(0x1015, &d);
    w.begin();
    let mut p = hex("05000100");
    for v in [x, y, fw / 2, fh / 2] {
        p.extend(v.to_le_bytes());
    }
    w.rec(0x104F, &p);
    w.h(
        0x1025,
        "0202010000000000cdffffffadffffff00000000000000003000080000000000",
    );
    w.begin();
    w.h(0x104F, "0200020000000000000000000000000000000000");
    w.h(0x1026, "0000");
    w.h(0x1051, "0001000000000000");
    w.end();
    w.h(0x1032, "00000200");
    w.begin();
    w.h(0x1007, "000000000500ffff08004d00");
    w.h(0x100A, "ffffff0000000000000000004e004d00");

    w.h(0x852, "520800000500000000000000");
    w.h(0x852, "520800000900010000000000");
    w.h(0x852, "520800000700000000000000");
    w.h(0x8A4, "a408000000000000000000000000b626c15dac0300000000");
    w.h(0x853, "530800000700000000000000");
    w.end();
    let kind: u32 = match pos {
        0 => 0,
        2 => 4,
        4 => 8,
        1 => 2,
        _ => 6,
    };

    let sum = x ^ y ^ fw ^ fh;
    let mut l = hex("9d08000000000000000000");
    l.push(0);
    l.extend(sum.to_le_bytes());
    l.extend(kind.to_le_bytes());
    l.extend([0u8; 40]);
    w.rec(0x89D, &l);
    w.h(0x8A5, "a50800000000000000000000b68f628800000000");
    w.h(
        0x89E,
        "9e08000000000000000000000a00000000000f0002002f00000000000000",
    );
    w.h(0x853, "530800000900000000000000");
    w.end();
    w.h(0x853, "530800000500000000000000");
}
