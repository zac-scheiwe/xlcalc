use super::wdraw::{Dims, EMU_PX, SPT};
use crate::xlsb::esc;
use std::fmt::Write;

#[derive(Default, Clone)]
pub(super) struct Pending {
    pub spt: u16,
    pub flags: u32,
    pub anchor: Option<[u16; 8]>,
    pub fill: Option<u32>,
    pub filled: Option<bool>,
    pub line: Option<u32>,
    pub line_on: Option<bool>,
    pub line_emu: Option<u32>,
    pub name: String,

    pub pib: u32,

    pub child: Option<[i32; 4]>,

    pub bounds: Option<[i32; 4]>,

    pub level: usize,

    pub preset: Option<String>,

    pub metro_name: Option<String>,
}

#[derive(Default)]
pub(super) struct EscState {
    pos: u64,
    open: Vec<u64>,

    bounds: Option<[i32; 4]>,
}

pub(super) struct Shape {
    pub p: Pending,
    pub ot: u16,
    pub id: u16,
    pub text: String,

    pub chart: Option<String>,
}

fn rgb(v: u32) -> Option<u32> {
    (v >> 24 == 0).then_some(v & 0xFF_FFFF)
}

pub(super) fn scan(d: &[u8], out: &mut Vec<Pending>, st: &mut EscState) {
    let mut p = 0;
    let base = st.pos;
    st.pos += d.len() as u64;
    while p + 8 <= d.len() {
        let at = base + p as u64;
        st.open.retain(|&e| e > at);
        let ver = d[p] & 15;
        let inst = (u16::from_le_bytes([d[p], d[p + 1]]) >> 4) as usize;
        let ty = u16::from_le_bytes([d[p + 2], d[p + 3]]);
        let len = u32::from_le_bytes([d[p + 4], d[p + 5], d[p + 6], d[p + 7]]) as usize;
        if ver == 15 {
            if ty == 0xF003 {
                st.open.push(at + 8 + len as u64);
            }
            p += 8;
            continue;
        }
        let end = (p + 8 + len).min(d.len());
        let b = &d[p + 8..end];
        match ty {
            0xF00A if b.len() >= 8 => {
                let flags = u32::from_le_bytes([b[4], b[5], b[6], b[7]]);
                let bounds = st.bounds.take();
                if flags & 4 == 0 {
                    out.push(Pending {
                        spt: inst as u16,
                        flags,
                        bounds,
                        level: st.open.len().saturating_sub(usize::from(flags & 1 != 0)),
                        ..Pending::default()
                    });
                }
            }
            0xF00B | 0xF122 => {
                if let Some(cur) = out.last_mut() {
                    opt(b, inst, cur);
                }
            }
            0xF00F | 0xF009 if b.len() >= 16 => {
                let mut a = [0i32; 4];
                for (i, v) in a.iter_mut().enumerate() {
                    *v = i32::from_le_bytes([b[4 * i], b[4 * i + 1], b[4 * i + 2], b[4 * i + 3]]);
                }
                if ty == 0xF009 {
                    st.bounds = Some(a);
                } else if let Some(cur) = out.last_mut() {
                    cur.child = Some(a);
                }
            }
            0xF010 if b.len() >= 18 => {
                if let Some(cur) = out.last_mut() {
                    let mut a = [0u16; 8];
                    for (i, v) in a.iter_mut().enumerate() {
                        *v = u16::from_le_bytes([b[2 + 2 * i], b[3 + 2 * i]]);
                    }
                    cur.anchor = Some(a);
                }
            }
            _ => {}
        }
        p = end;
    }
}

fn opt(b: &[u8], n: usize, cur: &mut Pending) {
    let mut complex = n * 6;
    for k in 0..n {
        let Some(e) = b.get(k * 6..k * 6 + 6) else {
            return;
        };
        let id = u16::from_le_bytes([e[0], e[1]]);
        let v = u32::from_le_bytes([e[2], e[3], e[4], e[5]]);
        let pid = id & 0x3FFF;
        if id & 0x8000 != 0 {
            let data = b.get(complex..complex + v as usize).unwrap_or(&[]);
            complex += v as usize;
            if data.starts_with(b"PK") {
                (cur.preset, cur.metro_name) = metro_info(data);
            }
            if pid == 0x380 {
                let u: Vec<u16> = data
                    .as_chunks::<2>()
                    .0
                    .iter()
                    .map(|c| u16::from_le_bytes(*c))
                    .take_while(|&c| c != 0)
                    .collect();
                cur.name = String::from_utf16_lossy(&u);
            }
            continue;
        }
        match pid {
            0x104 => cur.pib = v,
            0x181 => cur.fill = rgb(v),
            0x1BF if v >> 16 & 0x10 != 0 => cur.filled = Some(v & 0x10 != 0),
            0x1C0 => cur.line = rgb(v),
            0x1CB => cur.line_emu = Some(v),
            0x1FF if v >> 16 & 8 != 0 => cur.line_on = Some(v & 8 != 0),
            _ => {}
        }
    }
}

fn metro_info(zip: &[u8]) -> (Option<String>, Option<String>) {
    let attr = |x: &str, tag: &str, name: &str| -> Option<String> {
        let rest = &x[x.find(tag)?..];
        let key = format!("{name}=\"");
        let p = rest.find(&key)? + key.len();
        let end = rest[p..].find('"')?;
        Some(rest[p..p + end].to_string())
    };
    let read = || -> Option<String> {
        let mut z = zip::ZipArchive::new(std::io::Cursor::new(zip)).ok()?;
        let mut f = z.by_name("drs/shapexml.xml").ok()?;
        let mut x = String::new();
        std::io::Read::read_to_string(&mut f, &mut x).ok()?;
        Some(x)
    };
    let Some(x) = read() else {
        return (None, None);
    };
    (
        attr(&x, "prstGeom", "prst"),
        attr(&x, "cNvPr", "name").map(|n| {
            n.replace("&lt;", "<")
                .replace("&gt;", ">")
                .replace("&quot;", "\"")
                .replace("&apos;", "'")
                .replace("&amp;", "&")
        }),
    )
}

fn hex(c: u32) -> String {
    format!(
        "{:02X}{:02X}{:02X}",
        c & 0xFF,
        c >> 8 & 0xFF,
        c >> 16 & 0xFF
    )
}

pub(super) fn obj_head(d: &[u8]) -> Option<(u16, u16)> {
    if d.len() < 8 || d[0] != 0x15 || d[1] != 0 {
        return None;
    }
    Some((
        u16::from_le_bytes([d[4], d[5]]),
        u16::from_le_bytes([d[6], d[7]]),
    ))
}

fn corner(x: &mut String, tag: &str, d: &Dims, col: u16, dx: u16, row: u16, dy: u16) {
    let ox = (f64::from(dx) / 1024.0 * d.col_px(u32::from(col)) * EMU_PX).round() as i64;
    let oy = (f64::from(dy) / 256.0 * d.row_px(u32::from(row)) * EMU_PX).round() as i64;
    let _ = write!(
        x,
        "<xdr:{tag}><xdr:col>{col}</xdr:col><xdr:colOff>{ox}</xdr:colOff><xdr:row>{row}</xdr:row><xdr:rowOff>{oy}</xdr:rowOff></xdr:{tag}>"
    );
}

#[derive(Clone)]
pub(super) struct Blip {
    pub ext: &'static str,
    pub bytes: Vec<u8>,
}

type EscRec<'a> = (u16, u16, &'a [u8]);

fn escher_recs(d: &[u8]) -> Vec<EscRec<'_>> {
    let mut v = Vec::new();
    let mut p = 0;
    while p + 8 <= d.len() {
        let inst = u16::from_le_bytes([d[p], d[p + 1]]) >> 4;
        let ty = u16::from_le_bytes([d[p + 2], d[p + 3]]);
        let len = u32::from_le_bytes([d[p + 4], d[p + 5], d[p + 6], d[p + 7]]) as usize;
        let end = (p + 8 + len).min(d.len());
        v.push((inst, ty, &d[p + 8..end]));
        p = end;
    }
    v
}

pub(super) fn blips(d: &[u8]) -> Vec<Option<Blip>> {
    let mut out = Vec::new();
    for (_, ty, body) in escher_recs(d) {
        if ty != 0xF000 {
            continue;
        }
        for (_, ty, store) in escher_recs(body) {
            if ty != 0xF001 {
                continue;
            }
            for (inst, ty, b) in escher_recs(store) {
                out.push(match ty {
                    0xF007 if b.len() > 36 => {
                        let at = 36 + usize::from(b[33]);
                        b.get(at..).and_then(|r| {
                            let (i, t, body) = *escher_recs(r).first()?;
                            blip(i, t, body)
                        })
                    }
                    0xF01A..=0xF02F => blip(inst, ty, b),
                    _ => None,
                });
            }
        }
    }
    out
}

fn blip(inst: u16, ty: u16, b: &[u8]) -> Option<Blip> {
    let two = matches!(
        inst,
        0x46B | 0x6E1 | 0x6E3 | 0x6E5 | 0x7A9 | 0x3D5 | 0x217 | 0x543
    );
    let at = 16 * (1 + usize::from(two));
    match ty {
        0xF01D | 0xF01E | 0xF029 => Some(Blip {
            ext: match ty {
                0xF01D => "jpeg",
                0xF01E => "png",
                _ => "tiff",
            },
            bytes: b.get(at + 1..)?.to_vec(),
        }),
        0xF01F => {
            let dib = b.get(at + 1..)?;
            let u32at = |o: usize| -> Option<u32> {
                Some(u32::from_le_bytes(dib.get(o..o + 4)?.try_into().ok()?))
            };
            let hdr = u32at(0)? as usize;
            let bits = u16::from_le_bytes(dib.get(14..16)?.try_into().ok()?);
            let used = u32at(32)? as usize;
            let pal = if bits <= 8 {
                4 * if used == 0 { 1usize << bits } else { used }
            } else {
                4 * used
            };
            let masks = if hdr == 40 && u32at(16)? == 3 { 12 } else { 0 };
            let mut v = b"BM".to_vec();
            v.extend((dib.len() as u32 + 14).to_le_bytes());
            v.extend([0u8; 4]);
            v.extend(((14 + hdr + pal + masks) as u32).to_le_bytes());
            v.extend(dib);
            Some(Blip {
                ext: "bmp",
                bytes: v,
            })
        }
        _ => None,
    }
}

fn abs_emu(d: &Dims, col: u16, dx: u16, row: u16, dy: u16) -> (i64, i64) {
    let mut x = 0.0;
    for c in 0..u32::from(col) {
        x += d.col_px(c);
    }
    x += f64::from(dx) / 1024.0 * d.col_px(u32::from(col));
    let mut y = 0.0;
    for r in 0..u32::from(row) {
        y += d.row_px(r);
    }
    y += f64::from(dy) / 256.0 * d.row_px(u32::from(row));
    ((x * EMU_PX).round() as i64, (y * EMU_PX).round() as i64)
}

fn tree_of(shapes: &[Shape], idx: &[usize]) -> Vec<(usize, Vec<usize>)> {
    let mut out = Vec::new();
    let mut at = 0;
    while at < idx.len() {
        let i = idx[at];
        let lv = shapes[i].p.level;
        let mut kids = Vec::new();
        let mut j = at + 1;
        if shapes[i].p.flags & 1 != 0 {
            while j < idx.len() && shapes[idx[j]].p.level > lv {
                kids.push(idx[j]);
                j += 1;
            }
        }
        out.push((i, kids));
        at = j;
    }
    out
}

type Part = (String, Vec<u8>);

struct Env<'a> {
    shapes: &'a [Shape],
    dims: &'a Dims,
    sheet: &'a str,
    n: usize,
    blips: &'a [Option<Blip>],

    extra: Vec<Part>,
    rels: String,
    nrel: usize,
    charts: usize,
}

const REL_NS: &str = "http://schemas.openxmlformats.org/officeDocument/2006/relationships";

impl Env<'_> {
    fn rel(&mut self, kind: &str, target: &str) -> String {
        self.nrel += 1;
        let id = format!("rId{}", self.nrel);
        let _ = write!(
            self.rels,
            "<Relationship Id=\"{id}\" Type=\"{REL_NS}/{kind}\" Target=\"{target}\"/>"
        );
        id
    }

    fn object(
        &mut self,
        i: usize,
        kids: &[usize],
        parent: Option<([i32; 4], [i64; 4])>,
        warnings: &mut Vec<String>,
    ) -> Option<String> {
        let top = parent.is_none();
        let s = &self.shapes[i];
        let sheet = self.sheet;
        let is_group = s.p.flags & 1 != 0;
        let line = s.ot == 1;
        if s.ot == 9 && s.p.preset.is_none() {
            warnings.push(format!(
                "{sheet}: freeform {} read as a rectangle (its outline is not converted)",
                s.id
            ));
        }
        if !is_group
            && !matches!(s.ot, 1 | 2 | 3 | 6 | 8 | 9 | 30)
            && !(s.ot == 5 && s.chart.is_some())
        {
            warnings.push(format!(
                "{sheet}: drawing object {} (type {:#x}) not converted",
                s.id, s.ot
            ));
            return None;
        }
        let name = if s.p.name.is_empty() {
            s.p.metro_name
                .clone()
                .unwrap_or_else(|| format!("Shape {}", s.id))
        } else {
            s.p.name.clone()
        };
        let [x0, y0, x1, y1] = match (parent, s.p.child, s.p.anchor) {
            (Some((bd, r)), Some(c), _) => {
                let map = |v: i32, lo: i32, hi: i32, g0: i64, g1: i64| -> i64 {
                    if hi == lo {
                        g0
                    } else {
                        g0 + ((f64::from(v) - f64::from(lo)) * (g1 - g0) as f64
                            / (f64::from(hi) - f64::from(lo)))
                        .round() as i64
                    }
                };
                [
                    map(c[0], bd[0], bd[2], r[0], r[2]),
                    map(c[1], bd[1], bd[3], r[1], r[3]),
                    map(c[2], bd[0], bd[2], r[0], r[2]),
                    map(c[3], bd[1], bd[3], r[1], r[3]),
                ]
            }
            (_, _, Some(a)) => {
                let (x0, y0) = abs_emu(self.dims, a[0], a[1], a[2], a[3]);
                let (x1, y1) = abs_emu(self.dims, a[4], a[5], a[6], a[7]);
                [x0, y0, x1, y1]
            }
            _ => return None,
        };
        let (off, ext) = if top {
            ((0, 0), (0, 0))
        } else {
            ((x0, y0), (x1 - x0, y1 - y0))
        };
        let flips = format!(
            "{}{}",
            if s.p.flags & 0x40 != 0 {
                " flipH=\"1\""
            } else {
                ""
            },
            if s.p.flags & 0x80 != 0 {
                " flipV=\"1\""
            } else {
                ""
            }
        );
        let id = s.id + 1;
        if is_group {
            let mut inner = String::new();
            let frame = (
                s.p.bounds
                    .unwrap_or([x0 as i32, y0 as i32, x1 as i32, y1 as i32]),
                [x0, y0, x1, y1],
            );
            for (k, sub) in tree_of(self.shapes, kids) {
                if let Some(o) = self.object(k, &sub, Some(frame), warnings) {
                    inner.push_str(&o);
                }
            }
            let (cx, cy) = (x1 - x0, y1 - y0);
            return Some(format!(
                "<xdr:grpSp><xdr:nvGrpSpPr><xdr:cNvPr id=\"{id}\" name=\"{}\"/><xdr:cNvGrpSpPr/></xdr:nvGrpSpPr><xdr:grpSpPr><a:xfrm><a:off x=\"{x0}\" y=\"{y0}\"/><a:ext cx=\"{cx}\" cy=\"{cy}\"/><a:chOff x=\"{x0}\" y=\"{y0}\"/><a:chExt cx=\"{cx}\" cy=\"{cy}\"/></a:xfrm></xdr:grpSpPr>{inner}</xdr:grpSp>",
                esc(&name)
            ));
        }
        let xfrm = format!(
            "<a:xfrm{flips}><a:off x=\"{}\" y=\"{}\"/><a:ext cx=\"{}\" cy=\"{}\"/></a:xfrm>",
            off.0, off.1, ext.0, ext.1
        );
        if s.ot == 8 {
            let Some(b) = self
                .blips
                .get((s.p.pib as usize).wrapping_sub(1))
                .and_then(|b| b.as_ref())
            else {
                warnings.push(format!("{sheet}: picture {name} has no image, not carried"));
                return None;
            };
            let file = format!("image{}.{}", s.p.pib, b.ext);
            let part = format!("xl/media/{file}");
            if !self.extra.iter().any(|p| p.0 == part) {
                self.extra.push((part, b.bytes.clone()));
            }
            let rid = self.rel("image", &format!("../media/{file}"));
            return Some(format!(
                "<xdr:pic><xdr:nvPicPr><xdr:cNvPr id=\"{id}\" name=\"{}\"/><xdr:cNvPicPr/></xdr:nvPicPr><xdr:blipFill><a:blip xmlns:r=\"{REL_NS}\" r:embed=\"{rid}\"/><a:stretch><a:fillRect/></a:stretch></xdr:blipFill><xdr:spPr>{xfrm}<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom></xdr:spPr></xdr:pic>",
                esc(&name)
            ));
        }
        let prst = if line {
            "line"
        } else if s.ot == 6 {
            "rect"
        } else {
            s.p.preset
                .as_deref()
                .unwrap_or_else(|| SPT.iter().find(|p| p.0 == s.p.spt).map_or("rect", |p| p.1))
        };
        let mut sp_pr = format!("{xfrm}<a:prstGeom prst=\"{prst}\"><a:avLst/></a:prstGeom>");
        if s.p.filled == Some(false) {
            sp_pr.push_str("<a:noFill/>");
        } else if let Some(c) = s.p.fill {
            let _ = write!(
                sp_pr,
                "<a:solidFill><a:srgbClr val=\"{}\"/></a:solidFill>",
                hex(c)
            );
        }
        if s.p.line_on == Some(false) {
            sp_pr.push_str("<a:ln><a:noFill/></a:ln>");
        } else if s.p.line.is_some() || s.p.line_emu.is_some() {
            let w =
                s.p.line_emu
                    .map_or(String::new(), |w| format!(" w=\"{w}\""));
            let c = s.p.line.map_or(String::new(), |c| {
                format!("<a:solidFill><a:srgbClr val=\"{}\"/></a:solidFill>", hex(c))
            });
            let _ = write!(sp_pr, "<a:ln{w}>{c}</a:ln>");
        }
        if let (5, Some(cx)) = (s.ot, &s.chart) {
            self.charts += 1;
            let file = format!("chart{}_{}.xml", self.n, self.charts);
            self.extra
                .push((format!("xl/charts/{file}"), cx.clone().into_bytes()));
            let rid = self.rel("chart", &format!("../charts/{file}"));
            Some(format!(
                "<xdr:graphicFrame macro=\"\"><xdr:nvGraphicFramePr><xdr:cNvPr id=\"{id}\" name=\"{}\"/><xdr:cNvGraphicFramePr/></xdr:nvGraphicFramePr><xdr:xfrm><a:off x=\"{}\" y=\"{}\"/><a:ext cx=\"{}\" cy=\"{}\"/></xdr:xfrm><a:graphic><a:graphicData uri=\"http://schemas.openxmlformats.org/drawingml/2006/chart\"><c:chart xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:r=\"{REL_NS}\" r:id=\"{rid}\"/></a:graphicData></a:graphic></xdr:graphicFrame>",
                esc(&name),
                off.0,
                off.1,
                ext.0,
                ext.1
            ))
        } else if line {
            Some(format!(
                "<xdr:cxnSp macro=\"\"><xdr:nvCxnSpPr><xdr:cNvPr id=\"{id}\" name=\"{}\"/><xdr:cNvCxnSpPr/></xdr:nvCxnSpPr><xdr:spPr>{sp_pr}</xdr:spPr></xdr:cxnSp>",
                esc(&name)
            ))
        } else {
            let tb = if s.ot == 6 { " txBox=\"1\"" } else { "" };
            let mut body = String::new();
            if !s.text.is_empty() {
                body.push_str("<xdr:txBody><a:bodyPr/><a:lstStyle/>");
                for para in s.text.split('\n') {
                    let _ = write!(body, "<a:p><a:r><a:t>{}</a:t></a:r></a:p>", esc(para));
                }
                body.push_str("</xdr:txBody>");
            }
            Some(format!(
                "<xdr:sp macro=\"\" textlink=\"\"><xdr:nvSpPr><xdr:cNvPr id=\"{id}\" name=\"{}\"/><xdr:cNvSpPr{tb}/></xdr:nvSpPr><xdr:spPr>{sp_pr}</xdr:spPr>{body}</xdr:sp>",
                esc(&name)
            ))
        }
    }
}

pub(super) fn drawing_xml(
    shapes: &[Shape],
    dims: &Dims,
    sheet: &str,
    blips: &[Option<Blip>],
    warnings: &mut Vec<String>,
    n: usize,
) -> Option<(String, Vec<Part>)> {
    let mut env = Env {
        shapes,
        dims,
        sheet,
        n,
        blips,
        extra: Vec::new(),
        rels: String::new(),
        nrel: 0,
        charts: 0,
    };
    let mut x = String::from("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<xdr:wsDr xmlns:xdr=\"http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\">");
    let mut any = false;
    let all: Vec<usize> = (0..shapes.len()).collect();
    for (i, kids) in tree_of(shapes, &all) {
        let Some(a) = shapes[i].p.anchor else {
            continue;
        };
        let Some(obj) = env.object(i, &kids, None, warnings) else {
            continue;
        };
        any = true;
        x.push_str("<xdr:twoCellAnchor>");
        corner(&mut x, "from", dims, a[0], a[1], a[2], a[3]);
        corner(&mut x, "to", dims, a[4], a[5], a[6], a[7]);
        x.push_str(&obj);
        x.push_str("<xdr:clientData/></xdr:twoCellAnchor>");
    }
    x.push_str("</xdr:wsDr>");
    let Env {
        mut extra, rels, ..
    } = env;
    if !extra.is_empty() {
        extra.push((
            format!("xl/drawings/_rels/drawing{n}.xml.rels"),
            format!("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">{rels}</Relationships>").into_bytes(),
        ));
    }
    any.then_some((x, extra))
}
