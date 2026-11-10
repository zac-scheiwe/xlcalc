use super::ptg_out::put_chars;
use super::rstyles::DEFAULT_PALETTE;
use crate::xml::Node;

pub(crate) type Rec = (u16, Vec<u8>);

pub(crate) fn theme_colors(theme: Option<&Node>) -> Vec<u32> {
    let mut out = vec![
        0xFFFFFF, 0x000000, 0xE7E6E6, 0x44546A, 0x4472C4, 0xED7D31, 0xA5A5A5, 0xFFC000, 0x5B9BD5,
        0x70AD47, 0x0563C1, 0x954F72,
    ];
    let Some(scheme) = theme
        .and_then(|t| t.child("themeElements"))
        .and_then(|t| t.child("clrScheme"))
    else {
        return out;
    };
    let order = [
        "lt1", "dk1", "lt2", "dk2", "accent1", "accent2", "accent3", "accent4", "accent5",
        "accent6", "hlink", "folHlink",
    ];
    for (i, name) in order.iter().enumerate() {
        if let Some(c) = scheme.child(name).and_then(|n| n.kids.first()) {
            let v = c.attr("val").or_else(|| c.attr("lastClr"));
            if let Some(v) = v.and_then(|v| u32::from_str_radix(v, 16).ok()) {
                out[i] = v;
            }
        }
    }
    out
}

fn tint(rgb: u32, t: f64) -> u32 {
    if t == 0.0 {
        return rgb;
    }
    let (r, g, b) = (
        f64::from((rgb >> 16) & 0xFF) / 255.0,
        f64::from((rgb >> 8) & 0xFF) / 255.0,
        f64::from(rgb & 0xFF) / 255.0,
    );
    let (mx, mn) = (r.max(g).max(b), r.min(g).min(b));
    let l = (mx + mn) / 2.0;
    let d = mx - mn;
    let (mut h, s);
    if d == 0.0 {
        h = 0.0;
        s = 0.0;
    } else {
        s = if l > 0.5 {
            d / (2.0 - mx - mn)
        } else {
            d / (mx + mn)
        };
        h = if mx == r {
            (g - b) / d + if g < b { 6.0 } else { 0.0 }
        } else if mx == g {
            (b - r) / d + 2.0
        } else {
            (r - g) / d + 4.0
        };
        h /= 6.0;
    }
    let l2 = if t < 0.0 {
        l * (1.0 + t)
    } else {
        l * (1.0 - t) + t
    };
    let hue = |p: f64, q: f64, mut t: f64| {
        if t < 0.0 {
            t += 1.0;
        }
        if t > 1.0 {
            t -= 1.0;
        }
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
    let (r2, g2, b2) = if s == 0.0 {
        (l2, l2, l2)
    } else {
        let q = if l2 < 0.5 {
            l2 * (1.0 + s)
        } else {
            l2 + s - l2 * s
        };
        let p = 2.0 * l2 - q;
        (
            hue(p, q, h + 1.0 / 3.0),
            hue(p, q, h),
            hue(p, q, h - 1.0 / 3.0),
        )
    };
    let c = |x: f64| (x * 255.0).round().clamp(0.0, 255.0) as u32;
    c(r2) << 16 | c(g2) << 8 | c(b2)
}

pub(crate) struct Palette {
    pub colors: Vec<u32>,
    pub theme: Vec<u32>,
}

impl Palette {
    pub fn icv(&self, c: Option<&Node>, fallback: u16) -> u16 {
        let Some(c) = c else { return fallback };
        if c.attr("auto") == Some("1") {
            return fallback;
        }
        if let Some(i) = c.attr("indexed").and_then(|v| v.parse::<u16>().ok()) {
            return i;
        }
        let rgb = if let Some(t) = c.attr("theme").and_then(|v| v.parse::<usize>().ok()) {
            let base = self.theme.get(t).copied().unwrap_or(0);
            let tn: f64 = c.attr("tint").and_then(|v| v.parse().ok()).unwrap_or(0.0);
            tint(base, tn)
        } else if let Some(v) = c.attr("rgb").and_then(|v| u32::from_str_radix(v, 16).ok()) {
            v & 0xFF_FFFF
        } else {
            return fallback;
        };
        self.nearest(rgb)
    }

    pub fn rgb(&self, c: &Node) -> Option<u32> {
        if c.attr("auto") == Some("1") {
            return None;
        }
        if let Some(i) = c.attr("indexed").and_then(|v| v.parse::<usize>().ok()) {
            return match i {
                0..=7 => DEFAULT_PALETTE.get(i).copied(),
                8..=63 => self.colors.get(i - 8).copied(),
                _ => None,
            };
        }
        if let Some(t) = c.attr("theme").and_then(|v| v.parse::<usize>().ok()) {
            let base = self.theme.get(t).copied().unwrap_or(0);
            let tn: f64 = c.attr("tint").and_then(|v| v.parse().ok()).unwrap_or(0.0);
            return Some(tint(base, tn));
        }
        c.attr("rgb")
            .and_then(|v| u32::from_str_radix(v, 16).ok())
            .map(|v| v & 0xFF_FFFF)
    }

    fn nearest(&self, rgb: u32) -> u16 {
        let ch = |c: u32| [(c >> 16) & 0xFF, (c >> 8) & 0xFF, c & 0xFF];
        let is_grey = |c: u32| {
            let [r, g, b] = ch(c);
            r == g && g == b
        };
        let want = ch(rgb);
        let grey = is_grey(rgb);
        let mut best = (u32::MAX, 8u16);
        for (i, &c) in self.colors.iter().enumerate() {
            if grey && !is_grey(c) {
                continue;
            }
            let have = ch(c);
            let dist: u32 = (0..3).map(|k| want[k].abs_diff(have[k])).sum();
            if dist < best.0 {
                best = (dist, i as u16 + 8);
            }
        }
        best.1
    }
}

pub(crate) struct StyleOut {
    pub fonts: Vec<Rec>,
    pub formats: Vec<Rec>,
    pub xfs: Vec<Rec>,

    pub xfext: Vec<Rec>,
    pub styles: Vec<Rec>,

    pub dxfs: Vec<Rec>,

    pub table_styles: Vec<Rec>,

    pub xf_base: u32,
    pub palette: Palette,
}

fn flag(n: &Node, k: &str) -> bool {
    matches!(n.attr(k), Some("1" | "true"))
}

fn font_rec(f: &Node, pal: &Palette) -> Rec {
    let mut grbit = 0u16;
    let mut bold = false;
    let (mut uls, mut sss) = (0u8, 0u16);
    let mut height = 200u16;
    let mut icv = 0x7FFFu16;
    let mut name = "Arial".to_string();
    let (mut family, mut charset) = (0u8, 0u8);
    for k in &f.kids {
        match k.name.as_str() {
            "b" => bold = k.attr("val") != Some("0"),
            "i" => {
                if k.attr("val") != Some("0") {
                    grbit |= 2;
                }
            }
            "strike" => {
                if k.attr("val") != Some("0") {
                    grbit |= 8;
                }
            }
            "outline" => grbit |= 0x10,
            "shadow" => grbit |= 0x20,
            "condense" => grbit |= 0x40,
            "extend" => grbit |= 0x80,
            "u" => {
                uls = match k.attr("val") {
                    Some("double") => 2,
                    Some("singleAccounting") => 0x21,
                    Some("doubleAccounting") => 0x22,
                    _ => 1,
                }
            }
            "vertAlign" => {
                sss = match k.attr("val") {
                    Some("superscript") => 1,
                    Some("subscript") => 2,
                    _ => 0,
                }
            }
            "sz" => {
                height = (k
                    .attr("val")
                    .and_then(|v| v.parse::<f64>().ok())
                    .unwrap_or(10.0)
                    * 20.0)
                    .round() as u16
            }
            "color" => icv = pal.icv(Some(k), 0x7FFF),
            "name" => name = k.attr("val").unwrap_or("Arial").to_string(),
            "family" => family = k.attr("val").and_then(|v| v.parse().ok()).unwrap_or(0),
            "charset" => charset = k.attr("val").and_then(|v| v.parse().ok()).unwrap_or(0),
            _ => {}
        }
    }
    let mut d = Vec::new();
    d.extend(height.to_le_bytes());
    d.extend(grbit.to_le_bytes());
    d.extend(icv.to_le_bytes());
    d.extend((if bold { 700u16 } else { 400 }).to_le_bytes());
    d.extend(sss.to_le_bytes());
    d.extend([uls, family, charset, 0]);
    d.push(name.encode_utf16().count().min(255) as u8);
    put_chars(&mut d, &name);
    (0x31, d)
}

const BORDER_STYLES: [&str; 14] = [
    "none",
    "thin",
    "medium",
    "dashed",
    "dotted",
    "thick",
    "double",
    "hair",
    "mediumDashed",
    "dashDot",
    "mediumDashDot",
    "dashDotDot",
    "mediumDashDotDot",
    "slantDashDot",
];

const PATTERNS: [&str; 19] = [
    "none",
    "solid",
    "mediumGray",
    "darkGray",
    "lightGray",
    "darkHorizontal",
    "darkVertical",
    "darkDown",
    "darkUp",
    "darkGrid",
    "darkTrellis",
    "lightHorizontal",
    "lightVertical",
    "lightDown",
    "lightUp",
    "lightGrid",
    "lightTrellis",
    "gray125",
    "gray0625",
];

const H_ALIGN: [&str; 8] = [
    "general",
    "left",
    "center",
    "right",
    "fill",
    "justify",
    "centerContinuous",
    "distributed",
];

const V_ALIGN: [&str; 5] = ["top", "center", "bottom", "justify", "distributed"];

fn pos(list: &[&str], v: Option<&str>) -> Option<u32> {
    v.and_then(|v| list.iter().position(|x| *x == v))
        .map(|i| i as u32)
}

struct Border {
    b1: u32,
    b2: u32,
}

fn border(b: Option<&Node>, pal: &Palette) -> Border {
    let Some(b) = b else {
        return Border { b1: 0, b2: 0 };
    };
    let side = |n: &str| -> (u32, u32) {
        match b.child(n) {
            Some(s) => {
                let st = pos(&BORDER_STYLES, s.attr("style")).unwrap_or(0);
                let c = if st == 0 {
                    0
                } else {
                    u32::from(pal.icv(s.child("color"), 64))
                };
                (st, c)
            }
            None => (0, 0),
        }
    };
    let (l, r, t, bt, d) = (
        side("left"),
        side("right"),
        side("top"),
        side("bottom"),
        side("diagonal"),
    );
    let diag = u32::from(flag(b, "diagonalDown")) | (u32::from(flag(b, "diagonalUp")) << 1);
    let b1 =
        l.0 | (r.0 << 4) | (t.0 << 8) | (bt.0 << 12) | (l.1 << 16) | (r.1 << 23) | (diag << 30);
    let b2 = t.1 | (bt.1 << 7) | (d.1 << 14) | (d.0 << 21);
    Border { b1, b2 }
}

fn fill(f: Option<&Node>, pal: &Palette) -> (u32, u16) {
    if f.is_some_and(|f| f.child("gradientFill").is_some()) {
        return (1, 64 | (65 << 7));
    }
    let Some(p) = f.and_then(|f| f.child("patternFill")) else {
        return (0, 0x20C0);
    };
    let pat = pos(&PATTERNS, p.attr("patternType")).unwrap_or(0);
    if pat == 0 {
        return (0, 0x20C0);
    }
    let fg = pal.icv(p.child("fgColor"), 64);
    let bg = pal.icv(p.child("bgColor"), 65);
    (pat, fg | (bg << 7))
}

fn num(n: &Node, k: &str) -> Option<u32> {
    n.attr(k).and_then(|v| v.parse().ok())
}

fn full_color(c: &Node) -> Option<[u8; 16]> {
    let mut d = [0u8; 16];
    let tint: f64 = c.attr("tint").and_then(|v| v.parse().ok()).unwrap_or(0.0);
    let rgb = c.attr("rgb").and_then(|v| u32::from_str_radix(v, 16).ok());
    let (ty, val) = match c.attr("theme").and_then(|v| v.parse::<u32>().ok()) {
        Some(t) => (3u16, t.to_le_bytes()),
        None => {
            let v = rgb?;
            (2, [(v >> 16) as u8, (v >> 8) as u8, v as u8, 0xFF])
        }
    };
    d[..2].copy_from_slice(&ty.to_le_bytes());
    d[2..4].copy_from_slice(&((tint * 32767.0).round() as i16).to_le_bytes());
    d[4..8].copy_from_slice(&val);
    Some(d)
}

fn xf_ext_props(
    _xf: &Node,
    font: Option<&Node>,
    fill: Option<&Node>,
    border: Option<&Node>,
) -> Vec<(u16, Vec<u8>)> {
    let mut p: Vec<(u16, Vec<u8>)> = Vec::new();
    if let Some(d) = font.and_then(|f| f.child("color")).and_then(full_color) {
        p.push((0x0D, d.to_vec()));
    }
    match font
        .and_then(|f| f.child("scheme"))
        .and_then(|s| s.attr("val"))
    {
        Some("major") => p.push((0x0E, vec![1])),
        Some("minor") => p.push((0x0E, vec![2])),
        _ => {}
    }
    if let Some(pf) = fill.and_then(|f| f.child("patternFill")) {
        if pf.attr("patternType").is_some_and(|t| t != "none") {
            for (n, ty) in [("fgColor", 4u16), ("bgColor", 5)] {
                if let Some(d) = pf.child(n).and_then(full_color) {
                    p.push((ty, d.to_vec()));
                }
            }
        }
    }
    if let Some(b) = border {
        for (n, ty) in [
            ("top", 7u16),
            ("bottom", 8),
            ("left", 9),
            ("right", 10),
            ("diagonal", 11),
        ] {
            if let Some(s) = b
                .child(n)
                .filter(|s| s.attr("style").is_some_and(|v| v != "none"))
            {
                if let Some(d) = s.child("color").and_then(full_color) {
                    p.push((ty, d.to_vec()));
                }
            }
        }
    }
    p
}

fn xf_ext_rec(ixfe: u16, props: &[(u16, Vec<u8>)]) -> Vec<u8> {
    let mut d = Vec::new();
    d.extend(0x087Du16.to_le_bytes());
    d.extend([0u8; 10]);
    d.extend(0u16.to_le_bytes());
    d.extend(ixfe.to_le_bytes());
    d.extend(0u16.to_le_bytes());
    d.extend((props.len() as u16).to_le_bytes());
    for (ty, data) in props {
        d.extend(ty.to_le_bytes());
        d.extend(((data.len() + 4) as u16).to_le_bytes());
        d.extend(data);
    }
    d
}

pub(crate) fn convert(styles: &Node, theme: Option<&Node>) -> StyleOut {
    let pal = Palette {
        colors: DEFAULT_PALETTE.to_vec(),
        theme: theme_colors(theme),
    };

    let mut fonts: Vec<Rec> = Vec::new();
    if let Some(fs) = styles.child("fonts") {
        for f in fs.children("font") {
            fonts.push(font_rec(f, &pal));
        }
    }
    if fonts.is_empty() {
        fonts.push(font_rec(&Node::default(), &pal));
    }
    while fonts.len() < 4 {
        fonts.push(fonts[0].clone());
    }

    let mut formats: Vec<Rec> = Vec::new();
    if let Some(nf) = styles.child("numFmts") {
        for f in nf.children("numFmt") {
            let id = num(f, "numFmtId").unwrap_or(164) as u16;
            let code = f.attr("formatCode").unwrap_or("General");
            let mut d = Vec::new();
            d.extend(id.to_le_bytes());
            d.extend((code.encode_utf16().count() as u16).to_le_bytes());
            put_chars(&mut d, code);
            formats.push((0x41E, d));
        }
    }
    let fills: Vec<&Node> = styles
        .child("fills")
        .map(|f| f.children("fill").collect())
        .unwrap_or_default();
    let borders: Vec<&Node> = styles
        .child("borders")
        .map(|f| f.children("border").collect())
        .unwrap_or_default();
    let style_xfs: Vec<&Node> = styles
        .child("cellStyleXfs")
        .map(|f| f.children("xf").collect())
        .unwrap_or_default();
    let cell_xfs: Vec<&Node> = styles
        .child("cellXfs")
        .map(|f| f.children("xf").collect())
        .unwrap_or_default();

    let xf_rec = |x: &Node, is_style: bool, parent: u32| -> Rec {
        let font = num(x, "fontId").unwrap_or(0);
        let font = if font >= 4 { font + 1 } else { font };
        let fmt = num(x, "numFmtId").unwrap_or(0) as u16;
        let al = x.child("alignment");
        let pr = x.child("protection");
        let locked = pr.is_none_or(|p| p.attr("locked") != Some("0"));
        let hidden = pr.is_some_and(|p| flag(p, "hidden"));
        let mut flags = u16::from(locked) | (u16::from(hidden) << 1);
        if is_style {
            flags |= 4;
        }
        if flag(x, "quotePrefix") {
            flags |= 8;
        }
        flags |= (parent as u16) << 4;
        let h = al
            .and_then(|a| pos(&H_ALIGN, a.attr("horizontal")))
            .unwrap_or(0);
        let v = al
            .and_then(|a| pos(&V_ALIGN, a.attr("vertical")))
            .unwrap_or(2);
        let wrap = al.is_some_and(|a| flag(a, "wrapText"));
        let rot = al.and_then(|a| num(a, "textRotation")).unwrap_or(0) as u8;
        let indent = al.and_then(|a| num(a, "indent")).unwrap_or(0) as u8 & 0xF;
        let shrink = al.is_some_and(|a| flag(a, "shrinkToFit"));
        let ro = al.and_then(|a| num(a, "readingOrder")).unwrap_or(0) as u8 & 3;
        let b = border(
            num(x, "borderId").and_then(|i| borders.get(i as usize).copied()),
            &pal,
        );
        let (pat, colors) = fill(
            num(x, "fillId").and_then(|i| fills.get(i as usize).copied()),
            &pal,
        );
        let mut d = Vec::new();
        d.extend((font as u16).to_le_bytes());
        d.extend(fmt.to_le_bytes());
        d.extend(flags.to_le_bytes());
        d.push((h as u8) | (u8::from(wrap) << 3) | ((v as u8) << 4));
        d.push(rot);
        d.push(indent | (u8::from(shrink) << 4) | (ro << 6));

        d.push(if is_style { 0 } else { 0xFC });
        d.extend(b.b1.to_le_bytes());
        d.extend((b.b2 | (pat << 26)).to_le_bytes());
        d.extend(colors.to_le_bytes());
        (0xE0, d)
    };

    let n_style = style_xfs.len().max(1);

    let xf_base = n_style.max(15);
    let mut xfs: Vec<Rec> = Vec::new();
    let mut ext_of: Vec<Option<&Node>> = Vec::new();
    for i in 0..xf_base {
        let x = style_xfs.get(i).or(style_xfs.first()).copied();
        xfs.push(xf_rec(x.unwrap_or(&Node::default()), true, 0xFFF));
        ext_of.push(x);
    }
    for x in &cell_xfs {
        let parent = num(x, "xfId").unwrap_or(0);
        xfs.push(xf_rec(x, false, parent));
        ext_of.push(Some(x));
    }
    if cell_xfs.is_empty() {
        xfs.push(xf_rec(&Node::default(), false, 0));
        ext_of.push(None);
    }
    let font_nodes: Vec<&Node> = styles
        .child("fonts")
        .map(|f| f.children("font").collect())
        .unwrap_or_default();
    let mut xfext: Vec<Rec> = Vec::new();
    for (i, x) in ext_of.iter().enumerate() {
        let props = x.map(|x| {
            xf_ext_props(
                x,
                num(x, "fontId").and_then(|i| font_nodes.get(i as usize).copied()),
                num(x, "fillId").and_then(|i| fills.get(i as usize).copied()),
                num(x, "borderId").and_then(|i| borders.get(i as usize).copied()),
            )
        });
        let props = props.unwrap_or_default();

        if i >= xf_base && !props.is_empty() {
            xfs[i].1[17] |= 2;
        }
        xfext.push((0x87D, xf_ext_rec(i as u16, &props)));
    }
    let mut crc = vec![0u8; 12];
    crc[..2].copy_from_slice(&0x087Cu16.to_le_bytes());
    crc.extend(0u16.to_le_bytes());
    crc.extend((xfs.len() as u16).to_le_bytes());
    crc.extend(xf_crc(&xfs).to_le_bytes());
    xfext.insert(0, (0x87C, crc));

    let mut st: Vec<Rec> = Vec::new();
    if let Some(cs) = styles.child("cellStyles") {
        for s in cs.children("cellStyle") {
            let ixfe = num(s, "xfId").unwrap_or(0) as u16;
            let mut d = Vec::new();
            match num(s, "builtinId") {
                Some(b) if b < 10 => {
                    d.extend((ixfe | 0x8000).to_le_bytes());
                    d.push(b as u8);
                    d.push(0xFF);
                }
                _ => {
                    let name = s.attr("name").unwrap_or("");
                    d.extend(ixfe.to_le_bytes());
                    d.extend((name.encode_utf16().count() as u16).to_le_bytes());
                    put_chars(&mut d, name);
                }
            }
            st.push((0x293, d));
        }
    }
    let (dxfs, table_styles) = super::tstyle::convert(styles, &pal);
    StyleOut {
        fonts,
        formats,
        xfs,
        xfext,
        styles: st,
        dxfs,
        table_styles,
        xf_base: xf_base as u32,
        palette: pal,
    }
}

fn xf_crc(xfs: &[Rec]) -> u32 {
    let mut c = 0u32;
    for (_, data) in xfs {
        for &b in data {
            c ^= u32::from(b) << 24;
            for _ in 0..8 {
                c = if c & 0x8000_0000 != 0 {
                    (c << 1) ^ 0xAF
                } else {
                    c << 1
                };
            }
        }
    }
    c
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn xf_crc_matches_excel() {
        let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/../../conformance/xls");
        let mut n = 0;
        for e in std::fs::read_dir(dir).unwrap() {
            let p = e.unwrap().path();
            if p.extension().and_then(|x| x.to_str()) != Some("xls") {
                continue;
            }
            let cfb = xlcalc_vbaproject::cfb::Cfb::new(std::fs::read(&p).unwrap()).unwrap();
            let wb = cfb.read_stream("Workbook").unwrap();
            let (mut xfs, mut want): (Vec<Rec>, Option<u32>) = (Vec::new(), None);
            let mut i = 0;
            while i + 4 <= wb.len() {
                let id = u16::from_le_bytes([wb[i], wb[i + 1]]);
                let len = usize::from(u16::from_le_bytes([wb[i + 2], wb[i + 3]]));
                let data = wb[i + 4..i + 4 + len].to_vec();
                match id {
                    0xE0 => xfs.push((id, data)),
                    0x87C => want = Some(u32::from_le_bytes(data[16..20].try_into().unwrap())),
                    _ => {}
                }
                i += 4 + len;
            }
            assert_eq!(want, Some(xf_crc(&xfs)), "{}", p.display());
            n += 1;
        }
        assert!(n >= 10);
    }

    #[test]
    fn nearest_colour_matches_excel() {
        let pal = Palette {
            colors: DEFAULT_PALETTE.to_vec(),
            theme: theme_colors(None),
        };
        let text = std::fs::read_to_string(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../conformance/xls/palette.tsv"
        ))
        .unwrap();
        let mut n = 0;
        for line in text.lines() {
            let f: Vec<&str> = line.split('\t').collect();
            let rgb = u32::from_str_radix(f[0], 16).unwrap();

            let want = f[1].parse::<u16>().unwrap() + 7;
            let got = pal.nearest(rgb);
            assert!(
                got == want
                    || pal.colors[usize::from(got) - 8] == pal.colors[usize::from(want) - 8],
                "{}: Excel {want}, here {got}",
                f[0]
            );
            n += 1;
        }
        assert!(n >= 400);
    }
}
