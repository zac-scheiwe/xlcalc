use super::cur::Cur;
use std::fmt::Write as _;

pub(crate) const DEFAULT_PALETTE: [u32; 56] = [
    0x000000, 0xFFFFFF, 0xFF0000, 0x00FF00, 0x0000FF, 0xFFFF00, 0xFF00FF, 0x00FFFF, 0x800000,
    0x008000, 0x000080, 0x808000, 0x800080, 0x008080, 0xC0C0C0, 0x808080, 0x9999FF, 0x993366,
    0xFFFFCC, 0xCCFFFF, 0x660066, 0xFF8080, 0x0066CC, 0xCCCCFF, 0x000080, 0xFF00FF, 0xFFFF00,
    0x00FFFF, 0x800080, 0x800000, 0x008080, 0x0000FF, 0x00CCFF, 0xCCFFFF, 0xCCFFCC, 0xFFFF99,
    0x99CCFF, 0xFF99CC, 0xCC99FF, 0xFFCC99, 0x3366FF, 0x33CCCC, 0x99CC00, 0xFFCC00, 0xFF9900,
    0xFF6600, 0x666699, 0x969696, 0x003366, 0x339966, 0x003300, 0x333300, 0x993300, 0x993366,
    0x333399, 0x333333,
];

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

const BUILTIN_STYLES: [&str; 10] = [
    "Normal",
    "RowLevel_1",
    "ColLevel_1",
    "Comma",
    "Currency",
    "Percent",
    "Comma [0]",
    "Currency [0]",
    "Hyperlink",
    "Followed Hyperlink",
];

pub(crate) struct Font {
    pub xml: String,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct ExtColor {
    ty: u16,
    tint: i16,
    val: u32,
}

impl ExtColor {
    fn xml(self, tag: &str) -> Option<String> {
        let tint = if self.tint == 0 {
            String::new()
        } else {
            format!(" tint=\"{}\"", f64::from(self.tint) / 32767.0)
        };
        match self.ty {
            1 => Some(format!("<{tag} indexed=\"{}\"/>", self.val)),
            2 => {
                let b = self.val.to_le_bytes();
                Some(format!(
                    "<{tag} rgb=\"{:02X}{:02X}{:02X}{:02X}\"{tint}/>",
                    b[3], b[0], b[1], b[2]
                ))
            }
            3 => Some(format!("<{tag} theme=\"{}\"{tint}/>", self.val)),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Default)]
pub(crate) struct XfExt {
    fg: Option<ExtColor>,
    bg: Option<ExtColor>,

    border: [Option<ExtColor>; 5],
    text: Option<ExtColor>,

    scheme: u8,
}

pub(crate) struct Xf {
    pub font: u16,
    pub fmt: u16,
    pub flags: u16,
    pub align: u8,
    pub rot: u8,
    pub indent: u8,
    pub border1: u32,
    pub border2: u32,
    pub fill: u16,
}

impl Xf {
    pub fn is_style(&self) -> bool {
        self.flags & 4 != 0
    }
    pub fn parent(&self) -> usize {
        usize::from(self.flags >> 4)
    }
}

#[derive(Default)]
pub(crate) struct Styles {
    pub fonts: Vec<Font>,
    pub formats: Vec<(u16, String)>,
    pub xfs: Vec<Xf>,

    pub styles: Vec<(u16, Option<u8>, String)>,
    pub palette: Option<Vec<u32>>,

    pub xfext: Vec<XfExt>,

    pub dxfs: Vec<String>,

    pub table_default: Option<(String, String)>,

    pub table_styles: Vec<super::tstyle::TableStyle>,
}

fn esc(s: &str) -> String {
    super::super::xlsb::esc(s)
}

fn color_xml(tag: &str, icv: u16) -> String {
    match icv {
        0x7FFF => format!("<{tag} auto=\"1\"/>"),
        i => format!("<{tag} indexed=\"{i}\"/>"),
    }
}

impl Styles {
    pub fn font(&mut self, d: &[u8], cp: u16) -> Result<(), String> {
        let mut c = Cur::new(d);
        let height = c.u16()?;
        let grbit = c.u16()?;
        let icv = c.u16()?;
        let bls = c.u16()?;
        let sss = c.u16()?;
        let uls = c.u8()?;
        let family = c.u8()?;
        let charset = c.u8()?;
        c.u8()?;
        let name = c.short_str(cp)?;
        let mut x = String::from("<font>");
        if bls >= 700 {
            x.push_str("<b/>");
        }
        if grbit & 2 != 0 {
            x.push_str("<i/>");
        }
        if grbit & 8 != 0 {
            x.push_str("<strike/>");
        }
        if grbit & 0x10 != 0 {
            x.push_str("<outline/>");
        }
        if grbit & 0x20 != 0 {
            x.push_str("<shadow/>");
        }
        match uls {
            1 => x.push_str("<u/>"),
            2 => x.push_str("<u val=\"double\"/>"),
            0x21 => x.push_str("<u val=\"singleAccounting\"/>"),
            0x22 => x.push_str("<u val=\"doubleAccounting\"/>"),
            _ => {}
        }
        match sss {
            1 => x.push_str("<vertAlign val=\"superscript\"/>"),
            2 => x.push_str("<vertAlign val=\"subscript\"/>"),
            _ => {}
        }
        let _ = write!(x, "<sz val=\"{}\"/>", f64::from(height) / 20.0);
        if icv != 0x7FFF {
            x.push_str(&color_xml("color", icv));
        }
        let _ = write!(x, "<name val=\"{}\"/>", esc(&name));
        if family != 0 {
            let _ = write!(x, "<family val=\"{family}\"/>");
        }
        if charset != 0 {
            let _ = write!(x, "<charset val=\"{charset}\"/>");
        }
        x.push_str("</font>");
        self.fonts.push(Font { xml: x });
        Ok(())
    }

    pub fn xf(&mut self, d: &[u8]) -> Result<(), String> {
        let mut c = Cur::new(d);
        let font = c.u16()?;
        let fmt = c.u16()?;
        let flags = c.u16()?;
        let align = c.u8()?;
        let rot = c.u8()?;
        let indent = c.u8()?;
        c.u8()?;
        let border1 = c.u32()?;
        let border2 = c.u32()?;
        let fill = c.u16()?;
        self.xfs.push(Xf {
            font,
            fmt,
            flags,
            align,
            rot,
            indent,
            border1,
            border2,
            fill,
        });
        Ok(())
    }

    pub fn style(&mut self, d: &[u8], cp: u16) -> Result<(), String> {
        let mut c = Cur::new(d);
        let ixfe = c.u16()?;
        if ixfe & 0x8000 != 0 {
            let id = c.u8()?;
            self.styles.push((ixfe & 0xFFF, Some(id), String::new()));
        } else {
            let n = usize::from(c.u16()?);
            let name = c.xl_chars(n, cp)?;
            self.styles.push((ixfe & 0xFFF, None, name));
        }
        Ok(())
    }

    pub fn palette(&mut self, d: &[u8]) -> Result<(), String> {
        let mut c = Cur::new(d);
        let n = usize::from(c.u16()?);
        let mut v = Vec::new();
        for _ in 0..n {
            let t = c.take(4)?;
            v.push(u32::from(t[0]) << 16 | u32::from(t[1]) << 8 | u32::from(t[2]));
        }
        self.palette = Some(v);
        Ok(())
    }

    pub fn xf_ext(&mut self, d: &[u8]) -> Result<(), String> {
        let mut c = Cur::new(d);
        c.take(14)?;
        let ixfe = usize::from(c.u16()?);
        c.u16()?;
        let n = c.u16()?;
        let mut x = XfExt::default();
        for _ in 0..n {
            let ty = c.u16()?;
            let cb = usize::from(c.u16()?);
            let data = c.take(cb.saturating_sub(4))?;
            let mut body = Cur::new(data);
            let mut color = || -> Result<ExtColor, String> {
                Ok(ExtColor {
                    ty: body.u16()?,
                    tint: body.i16()?,
                    val: body.u32()?,
                })
            };
            match ty {
                4 => x.fg = Some(color()?),
                5 => x.bg = Some(color()?),
                7..=11 => x.border[usize::from(ty) - 7] = Some(color()?),
                0x0D => x.text = Some(color()?),
                0x0E => x.scheme = data.first().copied().unwrap_or(0),
                _ => {}
            }
        }
        if self.xfext.len() <= ixfe {
            self.xfext.resize(ixfe + 1, XfExt::default());
        }
        self.xfext[ixfe] = x;
        Ok(())
    }

    pub fn font_id(&self, i: u16) -> usize {
        usize::from(if i > 4 { i - 1 } else { i })
    }

    pub fn rpr(&self, ifnt: u16) -> String {
        match self.fonts.get(self.font_id(ifnt)) {
            Some(f) => f
                .xml
                .replacen("<font>", "<rPr>", 1)
                .replacen("</font>", "</rPr>", 1)
                .replace("<name val", "<rFont val"),
            None => String::new(),
        }
    }

    fn style_pos(&self, i: usize) -> usize {
        if i >= self.xfs.len() {
            return 0;
        }
        self.xfs[..i].iter().filter(|x| x.is_style()).count()
    }

    fn ext_font(&self, x: &Xf, ext: &XfExt, fonts: &mut Vec<String>) -> usize {
        let id = self.font_id(x.font);
        let Some(base) = fonts.get(id).cloned() else {
            return id;
        };
        let mut xml = base;
        if let Some(c) = ext.text.and_then(|c| c.xml("color")) {
            if let Some(s) = xml.find("<color ") {
                if let Some(e) = xml[s..].find("/>") {
                    xml.replace_range(s..s + e + 2, &c);
                }
            } else if let Some(s) = xml.find("<name ") {
                xml.insert_str(s, &c);
            }
        }
        if ext.scheme != 0 && !xml.contains("<scheme ") {
            let v = if ext.scheme == 1 { "major" } else { "minor" };
            xml = xml.replace("</font>", &format!("<scheme val=\"{v}\"/></font>"));
        }
        if xml == fonts[id] {
            return id;
        }
        match fonts.iter().position(|f| *f == xml) {
            Some(p) => p,
            None => {
                fonts.push(xml);
                fonts.len() - 1
            }
        }
    }

    fn xf_xml(
        &self,
        x: &Xf,
        style_side: bool,
        (ext, font_id): (&XfExt, usize),
        borders: &mut Vec<String>,
        fills: &mut Vec<String>,
    ) -> String {
        let border = {
            let b1 = x.border1;
            let b2 = x.border2;

            let side = |tag: &str, which: usize, dg: u32, icv: u32| -> String {
                if dg == 0 {
                    format!("<{tag}/>")
                } else {
                    let st = BORDER_STYLES.get(dg as usize).copied().unwrap_or("thin");
                    let c = ext.border[which]
                        .and_then(|c| c.xml("color"))
                        .unwrap_or_else(|| color_xml("color", icv as u16));
                    format!("<{tag} style=\"{st}\">{c}</{tag}>")
                }
            };
            let diag = (b1 >> 30) & 3;
            let mut s = String::from("<border");
            if diag & 1 != 0 {
                s.push_str(" diagonalDown=\"1\"");
            }
            if diag & 2 != 0 {
                s.push_str(" diagonalUp=\"1\"");
            }
            s.push('>');
            s.push_str(&side("left", 2, b1 & 0xF, (b1 >> 16) & 0x7F));
            s.push_str(&side("right", 3, (b1 >> 4) & 0xF, (b1 >> 23) & 0x7F));
            s.push_str(&side("top", 0, (b1 >> 8) & 0xF, b2 & 0x7F));
            s.push_str(&side("bottom", 1, (b1 >> 12) & 0xF, (b2 >> 7) & 0x7F));
            s.push_str(&side(
                "diagonal",
                4,
                if diag != 0 { (b2 >> 21) & 0xF } else { 0 },
                (b2 >> 14) & 0x7F,
            ));
            s.push_str("</border>");
            s
        };
        let pat = (x.border2 >> 26) & 0x3F;
        let fill = if pat == 0 {
            "<fill><patternFill patternType=\"none\"/></fill>".to_string()
        } else {
            let fg = x.fill & 0x7F;
            let bg = (x.fill >> 7) & 0x7F;
            format!(
                "<fill><patternFill patternType=\"{}\">{}{}</patternFill></fill>",
                PATTERNS.get(pat as usize).copied().unwrap_or("solid"),
                ext.fg
                    .and_then(|c| c.xml("fgColor"))
                    .unwrap_or_else(|| color_xml("fgColor", fg)),
                ext.bg
                    .and_then(|c| c.xml("bgColor"))
                    .unwrap_or_else(|| color_xml("bgColor", bg))
            )
        };
        let bid = index_of(borders, border);
        let fid = index_of(fills, fill);
        let mut s = format!(
            "<xf numFmtId=\"{}\" fontId=\"{}\" fillId=\"{fid}\" borderId=\"{bid}\"",
            x.fmt, font_id
        );
        if !style_side {
            let _ = write!(s, " xfId=\"{}\"", self.style_pos(x.parent()));
        }
        if x.flags & 8 != 0 {
            s.push_str(" quotePrefix=\"1\"");
        }

        s.push_str(" applyNumberFormat=\"1\" applyFont=\"1\" applyFill=\"1\" applyBorder=\"1\" applyAlignment=\"1\" applyProtection=\"1\">");
        let h = usize::from(x.align & 7);
        let v = usize::from((x.align >> 4) & 7);
        s.push_str("<alignment");
        if h != 0 {
            let _ = write!(
                s,
                " horizontal=\"{}\"",
                H_ALIGN.get(h).copied().unwrap_or("general")
            );
        }
        if v != 2 {
            let _ = write!(
                s,
                " vertical=\"{}\"",
                V_ALIGN.get(v).copied().unwrap_or("bottom")
            );
        }
        if x.rot != 0 {
            let _ = write!(s, " textRotation=\"{}\"", x.rot);
        }
        if x.align & 8 != 0 {
            s.push_str(" wrapText=\"1\"");
        }
        if x.indent & 0xF != 0 {
            let _ = write!(s, " indent=\"{}\"", x.indent & 0xF);
        }
        if x.indent & 0x10 != 0 {
            s.push_str(" shrinkToFit=\"1\"");
        }
        if x.indent >> 6 != 0 {
            let _ = write!(s, " readingOrder=\"{}\"", x.indent >> 6);
        }
        s.push_str("/>");
        let _ = write!(
            s,
            "<protection locked=\"{}\" hidden=\"{}\"/>",
            u8::from(x.flags & 1 != 0),
            u8::from(x.flags & 2 != 0)
        );
        s.push_str("</xf>");
        s
    }

    pub fn to_xml(&self) -> String {
        let mut o = String::from(
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">",
        );
        if !self.formats.is_empty() {
            let _ = write!(o, "<numFmts count=\"{}\">", self.formats.len());
            for (id, code) in &self.formats {
                let _ = write!(
                    o,
                    "<numFmt numFmtId=\"{id}\" formatCode=\"{}\"/>",
                    esc(code)
                );
            }
            o.push_str("</numFmts>");
        }
        let mut fonts: Vec<String> = self.fonts.iter().map(|f| f.xml.clone()).collect();
        if fonts.is_empty() {
            fonts.push("<font><sz val=\"10\"/><name val=\"Arial\"/></font>".into());
        }
        let mut borders =
            vec!["<border><left/><right/><top/><bottom/><diagonal/></border>".to_string()];
        let mut fills = vec![
            "<fill><patternFill patternType=\"none\"/></fill>".to_string(),
            "<fill><patternFill patternType=\"gray125\"/></fill>".to_string(),
        ];
        let mut style_xfs = String::new();
        let mut cell_xfs = String::new();
        let mut n_style = 0;
        for (i, x) in self.xfs.iter().enumerate() {
            let ext = self.xfext.get(i).copied().unwrap_or_default();
            let fid = self.ext_font(x, &ext, &mut fonts);
            if x.is_style() {
                style_xfs.push_str(&self.xf_xml(x, true, (&ext, fid), &mut borders, &mut fills));
                n_style += 1;
            }
            cell_xfs.push_str(&self.xf_xml(x, false, (&ext, fid), &mut borders, &mut fills));
        }
        let _ = write!(o, "<fonts count=\"{}\">", fonts.len());
        for f in &fonts {
            o.push_str(f);
        }
        o.push_str("</fonts>");
        if self.xfs.is_empty() {
            style_xfs.push_str("<xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\"/>");
            cell_xfs.push_str(
                "<xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\"/>",
            );
            n_style = 1;
        }
        let _ = write!(
            o,
            "<fills count=\"{}\">{}</fills>",
            fills.len(),
            fills.concat()
        );
        let _ = write!(
            o,
            "<borders count=\"{}\">{}</borders>",
            borders.len(),
            borders.concat()
        );
        let _ = write!(
            o,
            "<cellStyleXfs count=\"{n_style}\">{style_xfs}</cellStyleXfs>"
        );
        let _ = write!(
            o,
            "<cellXfs count=\"{}\">{cell_xfs}</cellXfs>",
            self.xfs.len().max(1)
        );
        o.push_str("<cellStyles count=\"");
        let mut cs = String::new();
        let mut n = 0;
        for (ixfe, builtin, name) in &self.styles {
            let id = self.style_pos(usize::from(*ixfe));
            match builtin {
                Some(b) => {
                    let nm = BUILTIN_STYLES
                        .get(usize::from(*b))
                        .copied()
                        .unwrap_or("Normal");
                    let _ = write!(
                        cs,
                        "<cellStyle name=\"{nm}\" xfId=\"{id}\" builtinId=\"{b}\"/>"
                    );
                }
                None => {
                    let _ = write!(cs, "<cellStyle name=\"{}\" xfId=\"{id}\"/>", esc(name));
                }
            }
            n += 1;
        }
        let _ = write!(o, "{n}\">{cs}</cellStyles>");
        let _ = write!(
            o,
            "<dxfs count=\"{}\">{}</dxfs>",
            self.dxfs.len(),
            self.dxfs.concat()
        );
        o.push_str(&self.table_styles_xml());
        if let Some(p) = &self.palette {
            let mut all: Vec<u32> = DEFAULT_PALETTE[..8].to_vec();
            all.extend(p);
            o.push_str("<colors><indexedColors>");
            for c in all {
                let _ = write!(o, "<rgbColor rgb=\"FF{c:06X}\"/>");
            }
            o.push_str("</indexedColors></colors>");
        }
        o.push_str("</styleSheet>");
        o
    }
}

fn index_of(list: &mut Vec<String>, s: String) -> usize {
    match list.iter().position(|x| *x == s) {
        Some(i) => i,
        None => {
            list.push(s);
            list.len() - 1
        }
    }
}
