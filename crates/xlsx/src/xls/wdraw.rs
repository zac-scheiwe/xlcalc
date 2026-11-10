use super::write::Out;
use crate::xml::Node;

pub(super) fn escher(ver: u8, inst: u16, ty: u16, body: &[u8]) -> Vec<u8> {
    let mut v = Vec::new();
    v.extend((u16::from(ver) | (inst << 4)).to_le_bytes());
    v.extend(ty.to_le_bytes());
    v.extend((body.len() as u32).to_le_bytes());
    v.extend(body);
    v
}

pub(super) struct Item {
    pub sp: Vec<u8>,
    pub obj: Vec<u8>,
    pub text: Option<String>,

    pub tail: Vec<u8>,
}

impl Item {
    pub(super) fn declared(&self) -> u32 {
        self.sp.len() as u32 + if self.text.is_some() { 8 } else { 0 }
    }
}

pub(super) struct BlipOut {
    pub uid: [u8; 16],

    pub kind: u8,
    pub record: Vec<u8>,
    pub refs: u32,
}

pub(super) fn blip_index(
    store: &mut Vec<BlipOut>,
    ext: &str,
    bytes: &[u8],
    warnings: &mut Vec<String>,
) -> u32 {
    use std::hash::{Hash, Hasher};
    let mut uid = [0u8; 16];
    for (k, half) in uid.chunks_mut(8).enumerate() {
        let mut h = std::collections::hash_map::DefaultHasher::new();
        k.hash(&mut h);
        ext.hash(&mut h);
        bytes.hash(&mut h);
        half.copy_from_slice(&h.finish().to_le_bytes());
    }
    if let Some(i) = store.iter().position(|b| b.uid == uid) {
        store[i].refs += 1;
        return i as u32 + 1;
    }
    let (kind, ty, inst, data): (u8, u16, u16, &[u8]) = match ext {
        "png" => (6, 0xF01E, 0x6E0, bytes),
        "jpeg" | "jpg" => (5, 0xF01D, 0x46A, bytes),
        "tif" | "tiff" => (17, 0xF029, 0x6E4, bytes),

        "bmp" | "dib" if bytes.len() > 14 => (7, 0xF01F, 0x7A8, &bytes[14..]),
        _ => {
            warnings.push(format!("picture format {ext} not carried"));
            return 0;
        }
    };
    let mut body = uid.to_vec();
    body.push(0xFF);
    body.extend(data);
    store.push(BlipOut {
        uid,
        kind,
        record: escher(0, inst, ty, &body),
        refs: 1,
    });
    store.len() as u32
}

pub(super) fn sp_container(body: &[u8], text: bool) -> Vec<u8> {
    let mut v = Vec::new();
    v.extend(0x000Fu16.to_le_bytes());
    v.extend(0xF004u16.to_le_bytes());
    v.extend((body.len() as u32 + if text { 8 } else { 0 }).to_le_bytes());
    v.extend(body);
    v
}

pub(super) fn opt(props: &mut [(u16, u32)], complex: &[u8]) -> Vec<u8> {
    props.sort_by_key(|p| p.0 & 0x3FFF);
    let mut b = Vec::new();
    for (id, v) in props.iter() {
        b.extend(id.to_le_bytes());
        b.extend(v.to_le_bytes());
    }
    b.extend(complex);
    escher(3, props.len() as u16, 0xF00B, &b)
}

pub(super) fn emit(o: &mut Out, items: &[Item], dgid: u32) -> u32 {
    let base = dgid * 1024;
    let n = items.len() as u32;
    for (i, it) in items.iter().enumerate() {
        let mut d = Vec::new();
        if i == 0 {
            let mut dg = Vec::new();
            dg.extend((n + 1).to_le_bytes());
            dg.extend((base + n).to_le_bytes());
            let mut patriarch = Vec::new();
            patriarch.extend(escher(1, 0, 0xF009, &[0u8; 16]));
            let mut sp = Vec::new();
            sp.extend(base.to_le_bytes());
            sp.extend(5u32.to_le_bytes());
            patriarch.extend(escher(2, 0, 0xF00A, &sp));
            let group = escher(15, 0, 0xF004, &patriarch);
            let spgr_len = group.len() as u32 + items.iter().map(Item::declared).sum::<u32>();
            let mut spgr = vec![0x0F, 0, 0x03, 0xF0];
            spgr.extend(spgr_len.to_le_bytes());
            let mut dgc = vec![0x0F, 0, 0x02, 0xF0];
            dgc.extend((16 + 8 + spgr_len).to_le_bytes());
            d.extend(dgc);
            d.extend(escher(0, dgid as u16, 0xF008, &dg));
            d.extend(spgr);
            d.extend(group);
        }
        d.extend(&it.sp);
        o.rec(0xEC, &d);
        o.rec(0x5D, &it.obj);
        if let Some(text) = &it.text {
            o.rec(0xEC, &escher(0, 0, 0xF00D, &[]));
            txo(o, text);
        }
        o.b.extend(&it.tail);
    }
    n + 1
}

fn txo(o: &mut Out, text: &str) {
    let wide = !text.is_ascii();
    let units: Vec<u16> = text.encode_utf16().collect();
    let cch = units.len().min(0x7FFF);
    let mut t = Vec::new();
    t.extend(0x0212u16.to_le_bytes());
    t.extend([0u8; 8]);
    t.extend((cch as u16).to_le_bytes());
    t.extend(16u16.to_le_bytes());
    t.extend([0u8; 4]);
    o.rec(0x1B6, &t);
    let step = if wide { 3999 } else { 7999 };
    let mut at = 0;
    while at < cch {
        let end = (at + step).min(cch);
        let mut c = vec![u8::from(wide)];
        for &u in &units[at..end] {
            if wide {
                c.extend(u.to_le_bytes());
            } else {
                c.push(u as u8);
            }
        }
        o.rec(0x3C, &c);
        at = end;
    }
    if cch == 0 {
        o.rec(0x3C, &[0]);
    }
    let mut runs = Vec::new();
    for (ich, ifnt) in [(0u16, 0u16), (cch as u16, 0x7FFF)] {
        runs.extend(ich.to_le_bytes());
        runs.extend(ifnt.to_le_bytes());
        runs.extend([0u8; 4]);
    }
    o.rec(0x3C, &runs);
}

pub(super) const EMU_PX: f64 = 9525.0;

pub(super) struct Dims {
    cols: Vec<(u32, u32, f64)>,
    rows: Vec<(u32, f64)>,
    def_col: f64,
    def_row: f64,
}

pub(super) fn chars_px(w: f64) -> f64 {
    ((256.0 * w + (128.0f64 / 7.0).trunc()) / 256.0 * 7.0).trunc()
}

impl Dims {
    pub fn of(root: &Node) -> Dims {
        let f = |n: &Node, k: &str| n.attr(k).and_then(|v| v.parse::<f64>().ok());
        let fmt = root.child("sheetFormatPr");
        let def_col = fmt
            .and_then(|p| f(p, "defaultColWidth"))
            .or_else(|| fmt.map(|p| f(p, "baseColWidth").unwrap_or(8.0) + 5.0 / 7.0))
            .map_or(64.0, chars_px);
        let def_row = fmt
            .and_then(|p| f(p, "defaultRowHeight"))
            .map_or(20.0, |h| (h * 96.0 / 72.0).round());
        let mut cols = Vec::new();
        for c in root.child("cols").iter().flat_map(|c| c.children("col")) {
            if let (Some(a), Some(b), Some(w)) = (f(c, "min"), f(c, "max"), f(c, "width")) {
                let hidden = matches!(c.attr("hidden"), Some("1" | "true"));
                cols.push((
                    a as u32 - 1,
                    b as u32 - 1,
                    if hidden { 0.0 } else { chars_px(w) },
                ));
            }
        }
        let mut rows = Vec::new();
        for r in root
            .child("sheetData")
            .iter()
            .flat_map(|d| d.children("row"))
        {
            if let (Some(n), Some(h)) = (f(r, "r"), f(r, "ht")) {
                let hidden = matches!(r.attr("hidden"), Some("1" | "true"));
                rows.push((
                    n as u32 - 1,
                    if hidden {
                        0.0
                    } else {
                        (h * 96.0 / 72.0).round()
                    },
                ));
            }
        }
        Dims {
            cols,
            rows,
            def_col,
            def_row,
        }
    }
    pub fn col_px(&self, c: u32) -> f64 {
        self.cols
            .iter()
            .find(|k| k.0 <= c && c <= k.1)
            .map_or(self.def_col, |k| k.2)
    }
    pub fn row_px(&self, r: u32) -> f64 {
        self.rows
            .iter()
            .find(|k| k.0 == r)
            .map_or(self.def_row, |k| k.1)
    }
}

pub(super) type Corner = (u32, i64, u32, i64);

pub(super) fn anchor_words(d: &Dims, a: Corner, b: Corner) -> [u16; 8] {
    let enc = |c: Corner| -> [u16; 4] {
        let cw = d.col_px(c.0).max(1.0);
        let rh = d.row_px(c.2).max(1.0);
        let dx = (c.1 as f64 / EMU_PX / cw * 1024.0)
            .round()
            .clamp(0.0, 1023.0);
        let dy = (c.3 as f64 / EMU_PX / rh * 256.0).round().clamp(0.0, 255.0);
        [
            c.0.min(255) as u16,
            dx as u16,
            c.2.min(65535) as u16,
            dy as u16,
        ]
    };
    let (x, y) = (enc(a), enc(b));
    [x[0], x[1], x[2], x[3], y[0], y[1], y[2], y[3]]
}

pub(super) fn advance(d: &Dims, c: Corner, px: f64, py: f64) -> Corner {
    let (mut col, mut row) = (c.0, c.2);
    let mut x = c.1 as f64 / EMU_PX + px;
    let mut y = c.3 as f64 / EMU_PX + py;
    while x >= d.col_px(col).max(1.0) && col < 254 {
        x -= d.col_px(col).max(1.0);
        col += 1;
    }
    while y >= d.row_px(row).max(1.0) && row < 65534 {
        y -= d.row_px(row).max(1.0);
        row += 1;
    }
    (col, (x * EMU_PX) as i64, row, (y * EMU_PX) as i64)
}

pub(super) struct ShapeIn {
    pub name: String,

    pub spt: u16,

    pub ot: u16,
    pub anchor: [u16; 8],
    pub fill: Option<u32>,
    pub no_fill: bool,
    pub line: Option<u32>,
    pub no_line: bool,
    pub line_emu: Option<u32>,
    pub flip_h: bool,
    pub flip_v: bool,
    pub text: String,
    pub chart: Option<super::wchart::ChartIn>,

    pub size_pt: (f64, f64),

    pub pic: Option<Image>,

    pub group: bool,
    pub children: Vec<ShapeIn>,

    pub rect: [i64; 4],

    pub member: bool,

    pub preset: Option<String>,

    pub corners: Option<(Corner, Corner)>,
}

pub(super) fn flatten(shapes: &[ShapeIn]) -> Vec<&ShapeIn> {
    let mut v = Vec::new();
    for s in shapes {
        v.push(s);
        v.extend(flatten(&s.children));
    }
    v
}

pub(super) fn descendants(s: &ShapeIn) -> usize {
    flatten(&s.children).len()
}

pub(super) const SPT: &[(u16, &str)] = &[
    (1, "rect"),
    (2, "roundRect"),
    (3, "ellipse"),
    (4, "diamond"),
    (5, "triangle"),
    (6, "rtTriangle"),
    (7, "parallelogram"),
    (9, "hexagon"),
    (10, "octagon"),
    (11, "plus"),
    (13, "rightArrow"),
    (15, "homePlate"),
    (16, "cube"),
    (21, "plaque"),
    (22, "can"),
    (41, "callout1"),
    (42, "callout2"),
    (43, "callout3"),
    (44, "accentCallout1"),
    (45, "accentCallout2"),
    (46, "accentCallout3"),
    (47, "borderCallout1"),
    (48, "borderCallout2"),
    (49, "borderCallout3"),
    (50, "accentBorderCallout1"),
    (51, "accentBorderCallout2"),
    (52, "accentBorderCallout3"),
    (53, "ribbon"),
    (54, "ribbon2"),
    (55, "chevron"),
    (56, "pentagon"),
    (58, "star8"),
    (59, "star16"),
    (60, "star32"),
    (61, "wedgeRectCallout"),
    (62, "wedgeRoundRectCallout"),
    (63, "wedgeEllipseCallout"),
    (64, "wave"),
    (65, "foldedCorner"),
    (66, "leftArrow"),
    (67, "downArrow"),
    (68, "upArrow"),
    (69, "leftRightArrow"),
    (70, "upDownArrow"),
    (71, "irregularSeal1"),
    (72, "irregularSeal2"),
    (73, "lightningBolt"),
    (77, "leftArrowCallout"),
    (78, "rightArrowCallout"),
    (79, "upArrowCallout"),
    (80, "downArrowCallout"),
    (81, "leftRightArrowCallout"),
    (82, "upDownArrowCallout"),
    (84, "bevel"),
    (85, "leftBracket"),
    (86, "rightBracket"),
    (87, "leftBrace"),
    (88, "rightBrace"),
    (92, "star24"),
    (94, "notchedRightArrow"),
    (96, "smileyFace"),
    (97, "verticalScroll"),
    (98, "horizontalScroll"),
    (102, "curvedRightArrow"),
    (103, "curvedLeftArrow"),
    (104, "curvedUpArrow"),
    (105, "curvedDownArrow"),
    (106, "cloudCallout"),
    (107, "ellipseRibbon"),
    (108, "ellipseRibbon2"),
    (109, "flowChartProcess"),
    (110, "flowChartDecision"),
    (111, "flowChartInputOutput"),
    (112, "flowChartPredefinedProcess"),
    (113, "flowChartInternalStorage"),
    (114, "flowChartDocument"),
    (115, "flowChartMultidocument"),
    (116, "flowChartTerminator"),
    (117, "flowChartPreparation"),
    (118, "flowChartManualInput"),
    (119, "flowChartManualOperation"),
    (120, "flowChartConnector"),
    (121, "flowChartPunchedCard"),
    (122, "flowChartPunchedTape"),
    (123, "flowChartSummingJunction"),
    (124, "flowChartOr"),
    (125, "flowChartCollate"),
    (126, "flowChartSort"),
    (127, "flowChartExtract"),
    (128, "flowChartMerge"),
    (129, "flowChartOfflineStorage"),
    (130, "flowChartOnlineStorage"),
    (131, "flowChartMagneticTape"),
    (132, "flowChartMagneticDisk"),
    (133, "flowChartMagneticDrum"),
    (134, "flowChartDisplay"),
    (135, "flowChartDelay"),
    (176, "flowChartAlternateProcess"),
    (177, "flowChartOffpageConnector"),
    (183, "sun"),
    (184, "moon"),
    (185, "bracketPair"),
    (186, "bracePair"),
    (187, "star4"),
    (188, "doubleWave"),
    (189, "actionButtonBlank"),
    (190, "actionButtonHome"),
    (191, "actionButtonHelp"),
    (192, "actionButtonInformation"),
    (193, "actionButtonForwardNext"),
    (194, "actionButtonBackPrevious"),
    (195, "actionButtonEnd"),
    (196, "actionButtonBeginning"),
    (197, "actionButtonReturn"),
    (198, "actionButtonDocument"),
    (199, "actionButtonSound"),
    (200, "actionButtonMovie"),
];

fn colorref(n: &Node, theme: &[u32]) -> Option<u32> {
    let v = super::wcolor::resolve(n.child("solidFill")?.kids.first()?, theme)?;
    Some(((v & 0xFF) << 16) | (v & 0xFF00) | (v >> 16))
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
                    "br" => s.push('\r'),
                    _ => {}
                }
            }
            s
        })
        .collect();
    paras.join("\r")
}

fn corner(n: &Node) -> Option<Corner> {
    let g = |k: &str| -> Option<i64> { n.child(k)?.text.trim().parse().ok() };
    Some((
        g("col")? as u32,
        g("colOff").unwrap_or(0),
        g("row")? as u32,
        g("rowOff").unwrap_or(0),
    ))
}

fn corner_emu(d: &Dims, c: Corner) -> (i64, i64) {
    let mut x = 0.0;
    for k in 0..c.0 {
        x += d.col_px(k);
    }
    let mut y = 0.0;
    for k in 0..c.2 {
        y += d.row_px(k);
    }
    ((x * EMU_PX) as i64 + c.1, (y * EMU_PX) as i64 + c.3)
}

pub(super) type Image = (String, Vec<u8>);

pub(super) struct Source<'a> {
    pub dims: &'a Dims,
    pub sheet: &'a str,
    pub chart_of: &'a dyn Fn(&str) -> Option<Node>,

    pub image_of: &'a dyn Fn(&str) -> Option<Image>,
    pub theme: &'a [u32],
}

pub(super) fn read(drawing: &Node, src: &Source<'_>, warnings: &mut Vec<String>) -> Vec<ShapeIn> {
    let dims = src.dims;
    let sheet = src.sheet;
    let mut out = Vec::new();
    for anchor in &drawing.kids {
        let (from, to) = match anchor.name.as_str() {
            "twoCellAnchor" => (
                anchor.child("from").and_then(corner),
                anchor.child("to").and_then(corner),
            ),
            "oneCellAnchor" => {
                let f = anchor.child("from").and_then(corner);
                let e = anchor.child("ext");
                let g = |k: &str| {
                    e.and_then(|e| e.attr(k)?.parse::<f64>().ok())
                        .unwrap_or(0.0)
                };
                (
                    f,
                    f.map(|f| advance(dims, f, g("cx") / EMU_PX, g("cy") / EMU_PX)),
                )
            }
            "absoluteAnchor" => {
                let g = |n: &Node, k: &str| {
                    n.attr(k).and_then(|v| v.parse::<f64>().ok()).unwrap_or(0.0)
                };
                let (px, py) = anchor
                    .child("pos")
                    .map_or((0.0, 0.0), |p| (g(p, "x"), g(p, "y")));
                let (cx, cy) = anchor
                    .child("ext")
                    .map_or((0.0, 0.0), |e| (g(e, "cx"), g(e, "cy")));
                let f = advance(dims, (0, 0, 0, 0), px / EMU_PX, py / EMU_PX);
                (Some(f), Some(advance(dims, f, cx / EMU_PX, cy / EMU_PX)))
            }
            _ => (None, None),
        };
        let obj = anchor.kids.iter().find(|k| is_object(k));
        let Some(obj) = obj else { continue };
        let name = obj_name(obj);
        let (Some(from), Some(to)) = (from, to) else {
            warnings.push(format!(
                "sheet {sheet}: shape {name} has no cell anchor, not carried"
            ));
            continue;
        };
        let (a, b) = (corner_emu(dims, from), corner_emu(dims, to));
        let rect = [a.0, a.1, b.0, b.1];
        if let Some(mut s) = shape(obj, src, rect, false, warnings) {
            s.anchor = anchor_words(dims, from, to);
            s.corners = Some((from, to));
            s.size_pt = span_pt(dims, from, to);
            out.push(s);
        }
    }
    out
}

fn is_object(k: &Node) -> bool {
    matches!(
        k.name.as_str(),
        "sp" | "cxnSp" | "pic" | "graphicFrame" | "grpSp"
    )
}

fn obj_name(obj: &Node) -> String {
    obj.kids
        .iter()
        .find(|k| k.name.starts_with("nv"))
        .and_then(|nv| nv.child("cNvPr"))
        .and_then(|c| c.attr("name"))
        .unwrap_or("")
        .to_string()
}

fn xfrm_rects(x: &Node) -> Option<([i64; 4], [i64; 4])> {
    let g = |n: &Node, k: &str| n.attr(k).and_then(|v| v.parse::<i64>().ok()).unwrap_or(0);
    let (o, e) = (x.child("off")?, x.child("ext")?);
    let r = [
        g(o, "x"),
        g(o, "y"),
        g(o, "x") + g(e, "cx"),
        g(o, "y") + g(e, "cy"),
    ];
    let c = match (x.child("chOff"), x.child("chExt")) {
        (Some(co), Some(ce)) => [
            g(co, "x"),
            g(co, "y"),
            g(co, "x") + g(ce, "cx"),
            g(co, "y") + g(ce, "cy"),
        ],
        _ => r,
    };
    Some((r, c))
}

fn shape(
    obj: &Node,
    src: &Source<'_>,
    rect: [i64; 4],
    member: bool,
    warnings: &mut Vec<String>,
) -> Option<ShapeIn> {
    let sheet = src.sheet;
    let nv = obj.kids.iter().find(|k| k.name.starts_with("nv"));
    let name = obj_name(obj);
    let sp_pr = obj.child("spPr").or_else(|| obj.child("grpSpPr"));
    let prst = sp_pr
        .and_then(|p| p.child("prstGeom"))
        .and_then(|g| g.attr("prst"))
        .unwrap_or("rect");
    let xfrm = sp_pr.and_then(|p| p.child("xfrm"));
    let flip = |k: &str| xfrm.is_some_and(|x| x.attr(k) == Some("1"));
    let ln = sp_pr.and_then(|p| p.child("ln"));
    let mut chart = None;
    let mut pic = None;
    let mut children = Vec::new();
    let mut group = false;
    let mut preset = None;
    let (spt, ot) = match obj.name.as_str() {
        "cxnSp" => (20, 1),
        "sp" => {
            let tb = nv
                .and_then(|nv| nv.child("cNvSpPr"))
                .is_some_and(|x| x.attr("txBox") == Some("1"));
            if tb {
                (202, 6)
            } else {
                match SPT.iter().find(|p| p.1 == prst) {
                    Some(&(1, _)) => (1, 2),
                    Some(&(3, _)) => (3, 3),
                    Some(&(t, _)) => (t, 30),
                    None if !member && body_text(obj).is_empty() => {
                        preset = Some(prst.to_string());
                        (0, 30)
                    }
                    None => {
                        warnings.push(format!(
                            "sheet {sheet}: shape {name}: geometry {prst} with text written as a rectangle"
                        ));
                        (1, 2)
                    }
                }
            }
        }
        "pic" => {
            let rid = find_attr(obj, "blip", "embed");
            let Some(img) = rid.and_then(|r| (src.image_of)(r)) else {
                warnings.push(format!(
                    "sheet {sheet}: picture {name} has no image, not carried"
                ));
                return None;
            };
            if !matches!(
                img.0.as_str(),
                "png" | "jpeg" | "jpg" | "bmp" | "dib" | "tif" | "tiff"
            ) {
                warnings.push(format!(
                    "sheet {sheet}: picture {name}: image format {} not carried",
                    img.0
                ));
                return None;
            }
            pic = Some(img);
            (75, 8)
        }
        "grpSp" => {
            group = true;
            if let Some((_, c)) = xfrm.and_then(xfrm_rects) {
                let map = |v: i64, lo: i64, hi: i64, g0: i64, g1: i64| -> i64 {
                    if hi == lo {
                        g0
                    } else {
                        g0 + ((v - lo) as f64 * (g1 - g0) as f64 / (hi - lo) as f64).round() as i64
                    }
                };
                for k in obj.kids.iter().filter(|k| is_object(k)) {
                    let kx = k
                        .child("spPr")
                        .or_else(|| k.child("grpSpPr"))
                        .and_then(|p| p.child("xfrm"))
                        .and_then(xfrm_rects);
                    let Some((kr, _)) = kx else {
                        warnings.push(format!(
                            "sheet {sheet}: member {} of group {name} has no position, not carried",
                            obj_name(k)
                        ));
                        continue;
                    };
                    let kr = [
                        map(kr[0], c[0], c[2], rect[0], rect[2]),
                        map(kr[1], c[1], c[3], rect[1], rect[3]),
                        map(kr[2], c[0], c[2], rect[0], rect[2]),
                        map(kr[3], c[1], c[3], rect[1], rect[3]),
                    ];
                    if let Some(s) = shape(k, src, kr, true, warnings) {
                        children.push(s);
                    }
                }
            }
            (0, 0)
        }
        "graphicFrame" => {
            if member {
                warnings.push(format!(
                    "sheet {sheet}: chart {name} inside a group not carried"
                ));
                return None;
            }
            let parsed = find_chart(obj).and_then(src.chart_of).and_then(|n| {
                super::wchart::parse(&n, warnings, &format!("sheet {sheet}: {name}"), src.theme)
            });
            match parsed {
                Some(c) => {
                    chart = Some(c);
                    (201, 5)
                }
                None => {
                    warnings.push(format!("sheet {sheet}: chart {name} not carried"));
                    return None;
                }
            }
        }
        other => {
            warnings.push(format!("sheet {sheet}: {other} {name} not carried"));
            return None;
        }
    };
    Some(ShapeIn {
        name,
        spt,
        ot,
        anchor: [0; 8],
        fill: sp_pr.and_then(|p| colorref(p, src.theme)),
        no_fill: sp_pr.is_some_and(|p| p.child("noFill").is_some()),
        line: ln.and_then(|l| colorref(l, src.theme)),
        no_line: ln.is_some_and(|l| l.child("noFill").is_some()),
        line_emu: ln.and_then(|l| l.attr("w")?.parse().ok()),
        flip_h: flip("flipH"),
        flip_v: flip("flipV"),
        text: body_text(obj),
        chart,
        size_pt: (
            (rect[2] - rect[0]) as f64 / 12700.0,
            (rect[3] - rect[1]) as f64 / 12700.0,
        ),
        pic,
        group,
        children,
        rect,
        member,
        preset,
        corners: None,
    })
}

fn find_attr<'a>(n: &'a Node, tag: &str, attr: &str) -> Option<&'a str> {
    if n.name == tag {
        return n.attr(attr);
    }
    n.kids.iter().find_map(|k| find_attr(k, tag, attr))
}

fn find_chart(n: &Node) -> Option<&str> {
    if n.name == "chart" {
        return n.attr("id");
    }
    n.kids.iter().find_map(find_chart)
}

fn span_pt(d: &Dims, a: Corner, b: Corner) -> (f64, f64) {
    let mut x = (b.1 - a.1) as f64 / EMU_PX;
    for c in a.0..b.0 {
        x += d.col_px(c);
    }
    let mut y = (b.3 - a.3) as f64 / EMU_PX;
    for r in a.2..b.2 {
        y += d.row_px(r);
    }
    (x * 0.75, y * 0.75)
}

pub(super) fn item(
    dgid: u32,
    i: u32,
    s: &ShapeIn,
    enc: &mut dyn FnMut(&str) -> Option<Vec<u8>>,
    chart_dg: u32,
    pib: u32,
) -> Item {
    let spid = dgid * 1024 + 1 + i;
    let has_text = !s.text.is_empty() && s.ot != 1;
    let mut flags = if s.ot == 1 { 0x0B00u32 } else { 0x0A00 };
    if s.flip_h {
        flags |= 0x40;
    }
    if s.flip_v {
        flags |= 0x80;
    }
    if s.member {
        flags |= 2;
    }
    if s.group {
        flags = (flags & !0xA00) | 0x201;
    }
    let mut sp = Vec::new();
    sp.extend(spid.to_le_bytes());
    sp.extend(flags.to_le_bytes());
    let mut props: Vec<(u16, u32)> = Vec::new();
    if s.group {
        props.push((0x04, 0));
        props.push((0x7F, 0x0004_0004));
        props.push((0x3BF, 0x0002_0000));
    } else if s.pic.is_some() {
        props.push((0x7F, 0x01FB_0000));
        props.push((0x4104, pib));
        props.push((0x13F, 0x0006_0000));
        props.push((0x1BF, 0x0010_0000));
        props.push((0x1FF, 0x0008_0000));
        props.push((0x33F, 0x0018_0010));
        props.push((0x3BF, 0x0002_0000));
    } else if s.chart.is_some() {
        props.push((0x7F, 0x0104_0104));
        props.push((0x181, 0x0800_004E));
    }
    if !s.group && s.pic.is_none() {
        if has_text {
            props.push((0x80, 0x4100_0000 + spid * 0x100));
            props.push((0x8B, 2));
        }
        if s.chart.is_some() {
            props.push((0xBF, 0x0008_0008));
        } else if s.ot != 1 {
            props.push((0xBF, 0x001F_0018));
        }
        if let Some(c) = s.fill {
            props.push((0x181, c));
        }
        props.push((
            0x1BF,
            if s.no_fill || s.ot == 1 {
                0x0010_0000
            } else {
                0x0010_0010
            },
        ));
        if let Some(c) = s.line {
            props.push((0x1C0, c));
        }
        if let Some(w) = s.line_emu {
            props.push((0x1CB, w));
        }
        if s.chart.is_none() {
            props.push((
                0x1FF,
                if s.no_line {
                    0x0008_0000
                } else if s.ot == 1 {
                    0x0018_0018
                } else {
                    0x0008_0008
                },
            ));
        }
        props.push((0x3BF, 0x0002_0000));
    }
    let mut complex = Vec::new();
    if !s.name.is_empty() {
        for u in s.name.encode_utf16().chain([0]) {
            complex.extend(u.to_le_bytes());
        }
        props.push((0xC380, complex.len() as u32));
    }
    let mut anchor = vec![0u8, 0];
    for w in s.anchor {
        anchor.extend(w.to_le_bytes());
    }
    let mut body = Vec::new();
    if s.group {
        let mut bounds = Vec::new();
        for v in s.rect {
            bounds.extend((v as i32).to_le_bytes());
        }
        body.extend(escher(1, 0, 0xF009, &bounds));
    }
    body.extend(escher(2, if s.group { 0 } else { s.spt }, 0xF00A, &sp));
    let mut tertiary = None;
    if s.preset.is_some() {
        let flat: Vec<(u16, u32)> = props
            .iter()
            .copied()
            .filter(|p| p.0 & 0x8000 == 0)
            .collect();
        let sum = super::wblob::shape_checksum(0, &flat);
        tertiary = super::wblob::blob(s, spid, &sum);
    }
    body.extend(opt(&mut props, &complex));
    if let Some(z) = tertiary {
        let mut t = Vec::new();
        t.extend(0x1FFu16.to_le_bytes());
        t.extend(0x0040_0000u32.to_le_bytes());
        t.extend(0xC3A9u16.to_le_bytes());
        t.extend((z.len() as u32).to_le_bytes());
        t.extend(z);
        body.extend(escher(3, 2, 0xF122, &t));
    }
    if s.member {
        let mut child = Vec::new();
        for v in s.rect {
            child.extend((v as i32).to_le_bytes());
        }
        body.extend(escher(0, 0, 0xF00F, &child));
    } else {
        body.extend(escher(0, 0, 0xF010, &anchor));
    }
    body.extend(escher(0, 0, 0xF011, &[]));
    let mut obj = vec![0x15, 0, 0x12, 0];
    obj.extend((if s.group { 0 } else { s.ot }).to_le_bytes());
    obj.extend(((i + 1) as u16).to_le_bytes());
    obj.extend((if s.chart.is_some() { 0x4011u16 } else { 0x6011 }).to_le_bytes());
    obj.extend([0u8; 12]);
    if s.group {
        obj.extend([6, 0, 2, 0, 0, 0]);
    } else if s.pic.is_some() {
        obj.extend([7, 0, 2, 0, 0xFF, 0xFF, 8, 0, 2, 0, 0, 0]);
    }
    obj.extend([0u8; 4]);
    let tail = s
        .chart
        .as_ref()
        .map(|c| super::wchart::substream(c, enc, chart_dg, s.size_pt))
        .unwrap_or_default();
    let sp = if s.group {
        let mut v = vec![0x0F, 0, 0x03, 0xF0, 0, 0, 0, 0];
        v.extend(escher(15, 0, 0xF004, &body));
        v
    } else {
        sp_container(&body, has_text)
    };
    Item {
        sp,
        obj,
        text: has_text.then(|| s.text.clone()),
        tail,
    }
}
