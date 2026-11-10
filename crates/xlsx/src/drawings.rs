use crate::package::{dir_of, resolve, Package, Rel};
use crate::rawxml::{self, Tag};
use crate::xml::{self, Node};
use xlcalc::drawing::{
    Anchor, Chart, ChartAxis, Image, Part, Pos, Series, Shape, ShapeKind, ShapeProps, SheetDrawing,
};
use xlcalc::{CellAddr, Rect, SheetId, Value, Workbook};

pub(crate) const NS_XDR: &str =
    "http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing";
pub(crate) const NS_A: &str = "http://schemas.openxmlformats.org/drawingml/2006/main";
pub(crate) const NS_C: &str = "http://schemas.openxmlformats.org/drawingml/2006/chart";
pub(crate) const REL: &str = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/";
pub(crate) const CT_DRAWING: &str = "application/vnd.openxmlformats-officedocument.drawing+xml";
pub(crate) const CT_CHART: &str =
    "application/vnd.openxmlformats-officedocument.drawingml.chart+xml";

pub(crate) const PRESETS: &[(i32, &str)] = &[
    (1, "rect"),
    (2, "parallelogram"),
    (3, "trapezoid"),
    (4, "diamond"),
    (5, "roundRect"),
    (6, "octagon"),
    (7, "triangle"),
    (8, "rtTriangle"),
    (9, "ellipse"),
    (10, "hexagon"),
    (11, "plus"),
    (12, "pentagon"),
    (13, "can"),
    (14, "cube"),
    (15, "bevel"),
    (16, "foldedCorner"),
    (17, "smileyFace"),
    (18, "donut"),
    (19, "noSmoking"),
    (20, "blockArc"),
    (21, "heart"),
    (22, "lightningBolt"),
    (23, "sun"),
    (24, "moon"),
    (25, "arc"),
    (33, "rightArrow"),
    (34, "leftArrow"),
    (35, "upArrow"),
    (36, "downArrow"),
    (37, "leftRightArrow"),
    (38, "upDownArrow"),
    (39, "quadArrow"),
    (41, "bentArrow"),
    (45, "homePlate"),
    (52, "chevron"),
    (92, "star5"),
];

fn prst_type(name: &str) -> i32 {
    PRESETS.iter().find(|p| p.1 == name).map_or(1, |p| p.0)
}

pub(crate) fn type_prst(t: i32) -> &'static str {
    PRESETS.iter().find(|p| p.0 == t).map_or("rect", |p| p.1)
}

#[derive(Clone, Debug)]
pub(crate) struct Item {
    pub raw: String,

    pub shape: Option<Shape>,

    pub members: Vec<Shape>,

    pub chart: Option<(String, String)>,
}

#[derive(Clone, Debug)]
pub(crate) struct Parsed {
    pub root: Tag,
    pub items: Vec<Item>,

    pub rels: Vec<Rel>,
}

fn num(n: &Node, key: &str) -> i64 {
    n.attr(key).and_then(|v| v.parse().ok()).unwrap_or(0)
}

fn anchor_of(n: &Node) -> Anchor {
    let txt = |name: &str| -> i64 {
        n.child(name)
            .and_then(|c| c.text.trim().parse().ok())
            .unwrap_or(0)
    };
    Anchor {
        row: txt("row").max(0) as u32,
        col: txt("col").max(0) as u32,
        row_off: txt("rowOff"),
        col_off: txt("colOff"),
    }
}

fn find<'a>(n: &'a Node, name: &str) -> Option<&'a Node> {
    for k in &n.kids {
        if k.name == name {
            return Some(k);
        }
        if let Some(f) = find(k, name) {
            return Some(f);
        }
    }
    None
}

fn lock_aspect(obj: &Node) -> bool {
    ["picLocks", "spLocks", "graphicFrameLocks", "cxnSpLocks"]
        .iter()
        .filter_map(|k| find(obj, k))
        .any(|l| matches!(l.attr("noChangeAspect"), Some("1" | "true")))
}

fn read_image(pkg: &Package, dpart: &str, rels: &[Rel], obj: &Node) -> Option<Image> {
    let blip = find(obj, "blip")?;
    let rel_of = |key: &str| {
        blip.attr(key)
            .and_then(|rid| rels.iter().find(|r| r.id == rid))
    };
    let embed = rel_of("embed").and_then(|rel| {
        let part = resolve(dir_of(dpart), &rel.target);
        let bytes = pkg.part(&part)?.to_vec();
        let ext = part
            .rsplit_once('.')
            .map_or(String::new(), |(_, e)| e.to_lowercase());
        Some((ext, bytes))
    });
    let link = rel_of("link").filter(|r| r.external).map(|r| {
        let t = r.target.strip_prefix("file:///").unwrap_or(&r.target);
        t.to_string()
    });
    let embedded = embed.is_some();
    let (mut ext, mut bytes) = embed.unwrap_or_default();
    if let Some(path) = &link {
        if !embedded {
            bytes = std::fs::read(path).unwrap_or_default();
            ext = path
                .rsplit_once('.')
                .map_or(String::new(), |(_, e)| e.to_lowercase());
        }
    } else if !embedded {
        return None;
    }
    Some(Image {
        ext,
        bytes,
        linked: link.is_some(),
        source: link.unwrap_or_default(),
        embedded,
    })
}

fn texts(n: &Node, out: &mut String) {
    if n.name == "t" {
        out.push_str(&n.text);
    }
    for k in &n.kids {
        texts(k, out);
    }
}

fn body_text(obj: &Node) -> String {
    let Some(b) = obj.child("txBody") else {
        return String::new();
    };
    let paras: Vec<String> = b
        .children("p")
        .map(|p| {
            let mut s = String::new();
            for k in &p.kids {
                match k.name.as_str() {
                    "r" | "fld" => texts(k, &mut s),
                    "br" => s.push('\n'),
                    _ => {}
                }
            }
            s
        })
        .collect();
    paras.join("\n")
}

fn anchor_pos(wb: &Workbook, sheet: SheetId, n: &Node) -> Option<Pos> {
    Some(match n.name.as_str() {
        "twoCellAnchor" => {
            let from = anchor_of(n.child("from")?);
            let to = anchor_of(n.child("to")?);
            let both = Pos::Both { from, to };
            match n.attr("editAs") {
                Some("oneCell") => {
                    let (_, _, w, h) = wb.shape_rect_emu(sheet, &both);
                    Pos::One { from, w, h }
                }
                Some("absolute") => {
                    let (x, y, w, h) = wb.shape_rect_emu(sheet, &both);
                    Pos::Abs { x, y, w, h }
                }
                _ => both,
            }
        }
        "oneCellAnchor" => {
            let ext = n.child("ext")?;
            Pos::One {
                from: anchor_of(n.child("from")?),
                w: num(ext, "cx"),
                h: num(ext, "cy"),
            }
        }
        _ => {
            let p = n.child("pos")?;
            let ext = n.child("ext")?;
            Pos::Abs {
                x: num(p, "x"),
                y: num(p, "y"),
                w: num(ext, "cx"),
                h: num(ext, "cy"),
            }
        }
    })
}

fn is_object(n: &Node) -> bool {
    matches!(
        n.name.as_str(),
        "sp" | "pic" | "graphicFrame" | "cxnSp" | "grpSp"
    )
}

fn xfrm_of(obj: &Node) -> Option<[i64; 8]> {
    let x = obj
        .child("spPr")
        .or_else(|| obj.child("grpSpPr"))
        .and_then(|p| p.child("xfrm"))
        .or_else(|| obj.child("xfrm"))?;
    let (o, e) = (x.child("off")?, x.child("ext")?);
    let (ox, oy, cx, cy) = (num(o, "x"), num(o, "y"), num(e, "cx"), num(e, "cy"));
    let (co, ce) = (x.child("chOff"), x.child("chExt"));
    Some([
        ox,
        oy,
        cx,
        cy,
        co.map_or(ox, |c| num(c, "x")),
        co.map_or(oy, |c| num(c, "y")),
        ce.map_or(cx, |c| num(c, "cx")),
        ce.map_or(cy, |c| num(c, "cy")),
    ])
}

type ChartRef = Option<(String, String)>;

struct Ctx<'a> {
    sheet: SheetId,
    pkg: &'a Package,
    dpart: &'a str,
    rels: &'a [Rel],
}

fn parse_anchor(
    wb: &Workbook,
    sheet: SheetId,
    pkg: &Package,
    dpart: &str,
    rels: &[Rel],
    n: &Node,
) -> Option<(Shape, ChartRef, Vec<Shape>)> {
    let obj = n.kids.iter().find(|k| is_object(k)).or_else(|| {
        n.child("AlternateContent")?
            .child("Choice")?
            .kids
            .iter()
            .find(|k| is_object(k))
    })?;
    let pos = anchor_pos(wb, sheet, n)?;
    let locked = n
        .child("clientData")
        .is_none_or(|d| d.attr("fLocksWithSheet") != Some("0"));
    let mut members = Vec::new();
    let cx = Ctx {
        sheet,
        pkg,
        dpart,
        rels,
    };
    let (shape, chart) = build_shape(wb, &cx, obj, pos, locked, None, &mut members)?;
    Some((shape, chart, members))
}

fn build_shape(
    wb: &Workbook,
    cx: &Ctx,
    obj: &Node,
    pos: Pos,
    locked: bool,
    parent: Option<u32>,
    members: &mut Vec<Shape>,
) -> Option<(Shape, Option<(String, String)>)> {
    let (sheet, pkg, dpart, rels) = (cx.sheet, cx.pkg, cx.dpart, cx.rels);
    let nv = obj.kids.iter().find(|k| k.name.starts_with("nv"))?;
    let c = nv.child("cNvPr")?;
    let id = c.attr("id")?.parse::<u32>().ok()?;
    let mut chart_ref = None;
    let kind = match obj.name.as_str() {
        "sp" => {
            if nv
                .child("cNvSpPr")
                .is_some_and(|x| x.attr("txBox") == Some("1"))
            {
                ShapeKind::TextBox
            } else {
                let prst = obj
                    .child("spPr")
                    .and_then(|p| p.child("prstGeom"))
                    .and_then(|g| g.attr("prst"))
                    .unwrap_or("rect");
                ShapeKind::Auto(prst_type(prst))
            }
        }
        "cxnSp" => ShapeKind::Line,
        "pic" => ShapeKind::Picture,
        "grpSp" => {
            let mut ids = Vec::new();
            let [_, _, _, _, chx, chy, chw, chh] = xfrm_of(obj)?;
            let (gx, gy, gw, gh) = wb.shape_rect_emu(sheet, &pos);
            let map = |v: i64, ch: i64, chl: i64, g: i64, gl: i64| -> i64 {
                if chl == 0 {
                    g
                } else {
                    g + ((v - ch) as f64 * gl as f64 / chl as f64).round() as i64
                }
            };
            let scale = |l: i64, chl: i64, gl: i64| -> i64 {
                if chl == 0 {
                    l
                } else {
                    (l as f64 * gl as f64 / chl as f64).round() as i64
                }
            };
            for k in obj.kids.iter().filter(|k| is_object(k)) {
                let Some([x, y, w, h, ..]) = xfrm_of(k) else {
                    continue;
                };
                let r = (
                    map(x, chx, chw, gx, gw),
                    map(y, chy, chh, gy, gh),
                    scale(w, chw, gw),
                    scale(h, chh, gh),
                );
                let kpos = wb.pos_for(sheet, pos.placement(), r);
                let mut inner = Vec::new();
                if let Some((s, _)) = build_shape(wb, cx, k, kpos, true, Some(id), &mut inner) {
                    ids.push(s.id);
                    members.push(s);
                    members.extend(inner);
                }
            }
            ShapeKind::Group(ids)
        }
        _ if find(obj, "slicer").is_some() => ShapeKind::Slicer,
        _ if find(obj, "timeslicer").is_some() => ShapeKind::Timeline,
        _ => {
            let ch = find(obj, "chart")?;
            let rid = ch.attr("id")?.to_string();
            let rel = rels.iter().find(|r| r.id == rid)?;
            chart_ref = Some((resolve(dir_of(dpart), &rel.target), rid));
            ShapeKind::Chart
        }
    };
    let image = if obj.name == "pic" {
        read_image(pkg, dpart, rels, obj)
    } else {
        None
    };
    let sp_pr = obj.child("spPr").or_else(|| obj.child("grpSpPr"));
    let xfrm = sp_pr.and_then(|p| p.child("xfrm"));
    let rotation = xfrm.map_or(0.0, |x| num(x, "rot") as f64 / 60000.0);
    let flip = |k: &str| xfrm.is_some_and(|x| x.attr(k) == Some("1"));
    let chart = if chart_ref.is_some() {
        chart_ref.as_ref().and_then(|(p, _)| read_chart(wb, pkg, p))
    } else {
        None
    };
    Some((
        Shape {
            id,
            name: c.attr("name").unwrap_or("").to_string(),
            kind,
            pos,
            visible: c.attr("hidden") != Some("1"),
            text: body_text(obj),
            chart,
            flip_h: flip("flipH"),
            flip_v: flip("flipV"),
            rotation,
            locked,
            alt_text: c.attr("descr").unwrap_or("").to_string(),
            parent,
            lock_aspect: lock_aspect(obj),
            image,
            props: ShapeProps::default(),
        },
        chart_ref,
    ))
}

pub(crate) fn read_drawing(
    wb: &Workbook,
    sheet: SheetId,
    pkg: &Package,
    dpart: &str,
) -> Option<Parsed> {
    let bytes = pkg.part(dpart)?;
    let split = rawxml::split(bytes).ok()?;
    let tree = xml::parse(bytes).ok()?;
    let root = &tree;
    let rels = pkg.rels(dpart);
    let mut items = Vec::new();
    if root.kids.len() != split.kids.len() {
        for k in split.kids {
            items.push(Item {
                raw: k.raw,
                shape: None,
                members: Vec::new(),
                chart: None,
            });
        }
    } else {
        for (k, n) in split.kids.into_iter().zip(&root.kids) {
            let parsed = if matches!(
                n.name.as_str(),
                "twoCellAnchor" | "oneCellAnchor" | "absoluteAnchor"
            ) {
                parse_anchor(wb, sheet, pkg, dpart, &rels, n)
            } else {
                None
            };
            let (shape, chart, members) = match parsed {
                Some((s, c, m)) => (Some(s), c, m),
                None => (None, None, Vec::new()),
            };
            items.push(Item {
                raw: k.raw,
                shape,
                members,
                chart,
            });
        }
    }
    Some(Parsed {
        root: split.root,
        items,
        rels,
    })
}

pub(crate) fn drawing_part_of(pkg: &Package, sheet_part: &str) -> Option<(String, String)> {
    let rels = pkg.rels(sheet_part);
    let bytes = pkg.part(sheet_part)?;
    let split = rawxml::split(bytes).ok()?;
    let d = split.kid("drawing")?;
    let tag = Tag::parse(&d.raw).ok()?;
    let rid = tag
        .attrs
        .iter()
        .find(|(k, _)| k == "id" || k.ends_with(":id"))
        .map(|(_, v)| rawxml::unescape(v))?;
    let rel = rels.iter().find(|r| r.id == rid)?;
    Some((resolve(dir_of(sheet_part), &rel.target), rid))
}

pub(crate) fn read_sheet_drawing(
    wb: &Workbook,
    sheet: SheetId,
    pkg: &Package,
    sheet_part: &str,
) -> Option<SheetDrawing> {
    let (dpart, _) = drawing_part_of(pkg, sheet_part)?;
    let p = read_drawing(wb, sheet, pkg, &dpart)?;
    let mut shapes: Vec<Shape> = Vec::new();
    for i in p.items {
        if let Some(s) = i.shape {
            shapes.push(s);
            shapes.extend(i.members);
        }
    }
    Some(SheetDrawing {
        shapes,
        next_id: 0,
        ..SheetDrawing::default()
    })
}

fn unquote_sheet(s: &str) -> String {
    match s.strip_prefix('\'').and_then(|x| x.strip_suffix('\'')) {
        Some(q) => q.replace("''", "'"),
        None => s.to_string(),
    }
}

fn parse_cell_ref(s: &str) -> Option<(u32, u32)> {
    let s = s.replace('$', "");
    let letters = s.bytes().take_while(u8::is_ascii_alphabetic).count();
    if letters == 0 || letters == s.len() {
        return None;
    }
    let col = s[..letters]
        .to_ascii_uppercase()
        .bytes()
        .fold(0u32, |c, b| c * 26 + u32::from(b - b'A' + 1))
        - 1;
    let row: u32 = s[letters..].parse().ok()?;
    Some((row.checked_sub(1)?, col))
}

fn split_top(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut quote = false;
    for ch in s.chars() {
        match ch {
            '\'' => {
                quote = !quote;
                cur.push(ch);
            }
            ',' if !quote => out.push(std::mem::take(&mut cur)),
            _ => cur.push(ch),
        }
    }
    out.push(cur);
    out
}

pub(crate) fn parse_ref(wb: &Workbook, f: &str) -> Option<Part> {
    let f = f.trim();
    let inner = f
        .strip_prefix('(')
        .and_then(|x| x.strip_suffix(')'))
        .unwrap_or(f);
    let mut areas = Vec::new();
    for piece in split_top(inner) {
        let (sh, r) = piece.rsplit_once('!')?;
        let id = wb.sheet_by_name(&unquote_sheet(sh))?.id;
        let (a, b) = r.split_once(':').unwrap_or((r, r));
        let (r0, c0) = parse_cell_ref(a)?;
        let (r1, c1) = parse_cell_ref(b)?;
        areas.push((id, Rect::new(r0, c0, r1, c1)));
    }
    Some(Part::Refs(areas))
}

fn literal_part(n: &Node) -> Part {
    let pts: Vec<String> = n
        .children("pt")
        .map(|p| p.child("v").map(|v| v.text.clone()).unwrap_or_default())
        .collect();
    if n.name == "numLit" {
        Part::Text(format!("{{{}}}", pts.join(",")))
    } else {
        Part::Text(format!(
            "{{{}}}",
            pts.iter()
                .map(|p| format!("\"{p}\""))
                .collect::<Vec<_>>()
                .join(",")
        ))
    }
}

fn series_part(wb: &Workbook, n: &Node) -> Part {
    for k in &n.kids {
        match k.name.as_str() {
            "strRef" | "numRef" | "multiLvlStrRef" => {
                return k
                    .child("f")
                    .and_then(|f| parse_ref(wb, &f.text))
                    .unwrap_or(Part::Dead)
            }
            "numLit" | "strLit" => return literal_part(k),
            "v" => return Part::Text(k.text.clone()),
            _ => {}
        }
    }
    Part::None
}

fn rich_text(n: &Node) -> String {
    let mut paras = Vec::new();
    if let Some(rich) = find(n, "rich") {
        for p in rich.children("p") {
            let mut s = String::new();
            texts(p, &mut s);
            paras.push(s);
        }
    }
    paras.join("\n")
}

fn plot_type(el: &Node) -> i32 {
    let val = |name: &str| el.child(name).and_then(|c| c.attr("val")).unwrap_or("");
    let ser_noline = el.children("ser").next().is_some_and(|s| {
        s.child("spPr")
            .and_then(|p| p.child("ln"))
            .is_some_and(|l| l.child("noFill").is_some())
    });
    match el.name.as_str() {
        "barChart" => {
            let col = val("barDir") != "bar";
            let base = if col { 51 } else { 57 };
            base + match val("grouping") {
                "stacked" => 1,
                "percentStacked" => 2,
                _ => 0,
            }
        }
        "bar3DChart" => {
            let col = val("barDir") != "bar";
            match (col, val("grouping")) {
                (true, "stacked") => 55,
                (true, "percentStacked") => 56,
                (true, "standard") => -4100,
                (true, _) => 54,
                (false, "stacked") => 61,
                (false, "percentStacked") => 62,
                (false, _) => 60,
            }
        }
        "lineChart" => {
            let markers = el
                .children("ser")
                .next()
                .and_then(|s| s.child("marker"))
                .and_then(|m| m.child("symbol"))
                .is_none_or(|s| s.attr("val") != Some("none"));
            match (val("grouping"), markers) {
                ("stacked", false) => 63,
                ("stacked", true) => 64,
                ("percentStacked", false) => 65 + 1,
                ("percentStacked", true) => 67,
                (_, false) => 4,
                (_, true) => 65,
            }
        }
        "line3DChart" => -4101,
        "pieChart" => 5,
        "pie3DChart" => -4102,
        "ofPieChart" => {
            if val("ofPieType") == "bar" {
                71
            } else {
                68
            }
        }
        "doughnutChart" => -4120,
        "areaChart" => match val("grouping") {
            "stacked" => 76,
            "percentStacked" => 77,
            _ => 1,
        },
        "area3DChart" => -4098,
        "scatterChart" => match (val("scatterStyle"), ser_noline) {
            (_, true) => -4169,
            ("smoothMarker", _) => 72,
            ("lineMarker", _) => 74,
            _ => 75,
        },
        "radarChart" => match val("radarStyle") {
            "filled" => 82,
            "marker" => 81,
            _ => -4151,
        },
        "bubbleChart" => 15,
        "surfaceChart" | "surface3DChart" => 83,
        "stockChart" => 88,
        _ => 51,
    }
}

fn legend_code(v: &str) -> i32 {
    match v {
        "l" => -4131,
        "t" => -4160,
        "b" => -4107,
        "tr" => 2,
        _ => -4152,
    }
}

fn axis_of(ax: &Node, value_axis: bool) -> ChartAxis {
    let mut a = ChartAxis::new(false);
    a.grid_major = ax.child("majorGridlines").is_some();
    a.grid_minor = ax.child("minorGridlines").is_some();
    if ax.child("title").is_some() {
        a.has_title = true;
        a.title = rich_text(ax.child("title").unwrap_or(ax));
    }
    if let Some(s) = ax.child("scaling") {
        let v = |n: &str| {
            s.child(n)
                .and_then(|c| c.attr("val"))
                .and_then(|x| x.parse::<f64>().ok())
        };
        a.min = v("min");
        a.max = v("max");
        a.reverse = s
            .child("orientation")
            .is_some_and(|o| o.attr("val") == Some("maxMin"));
    }
    if value_axis {
        a.major = ax
            .child("majorUnit")
            .and_then(|c| c.attr("val"))
            .and_then(|x| x.parse().ok());
    }
    a
}

pub(crate) fn read_chart(wb: &Workbook, pkg: &Package, part: &str) -> Option<Chart> {
    let bytes = pkg.part(part)?;
    let tree = xml::parse(bytes).ok()?;
    let space = &tree;
    let chart_el = space.child("chart")?;
    let plot = chart_el.child("plotArea")?;
    let mut c = Chart::new();
    let mut first = true;
    for el in plot.kids.iter().filter(|k| k.name.ends_with("Chart")) {
        let t = plot_type(el);
        if first {
            c.chart_type = t;
            first = false;
        }
        for s in el.children("ser") {
            let tx = s.child("tx");
            let name = tx.map_or(Part::None, |t| series_part(wb, t));
            let (x, v) = if el.name == "scatterChart" || el.name == "bubbleChart" {
                (
                    s.child("xVal").map_or(Part::None, |n| series_part(wb, n)),
                    s.child("yVal").map_or(Part::None, |n| series_part(wb, n)),
                )
            } else {
                (
                    s.child("cat").map_or(Part::None, |n| series_part(wb, n)),
                    s.child("val").map_or(Part::None, |n| series_part(wb, n)),
                )
            };
            c.series.push(Series {
                name,
                x,
                values: v,
                chart_type: t,
                axis_group: 1,
                fmt: crate::serfmt_xml::read_fmt(s, &el.name, t),
            });
        }
    }
    if let Some(t) = chart_el.child("title") {
        c.has_title = true;
        if let Some(f) = find(t, "f") {
            c.title_ref = parse_ref(wb, &f.text);
            c.title = rich_text(t);
        } else {
            c.title = rich_text(t);
        }
    }
    if let Some(l) = chart_el.child("legend") {
        c.has_legend = true;
        c.legend_pos = legend_code(
            l.child("legendPos")
                .and_then(|p| p.attr("val"))
                .unwrap_or("r"),
        );
    } else {
        c.has_legend = false;
    }
    let axes: Vec<&Node> = plot
        .kids
        .iter()
        .filter(|k| matches!(k.name.as_str(), "catAx" | "valAx" | "dateAx" | "serAx"))
        .collect();
    if let Some(a) = axes.first() {
        c.axes[0] = axis_of(a, a.name == "valAx");
    }
    if let Some(a) = axes.get(1) {
        c.axes[1] = axis_of(a, true);
    } else if axes.is_empty() {
        c.axes = [ChartAxis::new(false), ChartAxis::new(true)];
    }
    c.plot_by = match c.series.first().map(|s| &s.values) {
        Some(Part::Refs(v)) if v.len() == 1 => {
            let r = v[0].1;
            if r.rows() == 1 && r.cols() > 1 {
                1
            } else {
                2
            }
        }
        _ => 2,
    };
    Some(c)
}

pub(crate) fn cell_cache(wb: &Workbook, s: SheetId, row: u32, col: u32) -> Option<(bool, String)> {
    match wb.value(CellAddr::new(s, row, col)) {
        Value::Number(n) => Some((true, crate::write::number_text(n))),
        Value::Bool(b) => Some((true, if b { "1" } else { "0" }.to_string())),
        Value::Text(t) => Some((false, t.to_string())),
        Value::Blank => None,
        e => Some((false, format!("{e:?}"))),
    }
}
