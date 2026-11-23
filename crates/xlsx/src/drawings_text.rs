use crate::rawxml::{escape_attr, escape_text};
use crate::xml::Node;
use xlcalc::drawing::Shape;
use xlcalc::drawing_text::{CharFmt, FontColor, TextFrame, UNDERLINES};

const VERT: [&str; 6] = [
    "horz",
    "vert270",
    "vert",
    "eaVert",
    "mongolianVert",
    "wordArtVert",
];
const ALIGN: [&str; 7] = ["l", "ctr", "r", "just", "dist", "thaiDist", "justLow"];
const DEFAULT_MARGINS: [f32; 4] = [7.2, 3.6, 7.2, 3.6];

fn emu(pt: f32) -> i64 {
    (f64::from(pt) * 12700.0).round() as i64
}

fn rgb_to_bgr(v: &str) -> Option<u32> {
    let n = u32::from_str_radix(v, 16).ok()?;
    Some(((n & 0xFF) << 16) | (n & 0xFF00) | ((n >> 16) & 0xFF))
}

fn bgr_to_hex(c: u32) -> String {
    format!(
        "{:02X}{:02X}{:02X}",
        c & 0xFF,
        (c >> 8) & 0xFF,
        (c >> 16) & 0xFF
    )
}

fn read_fmt(rpr: Option<&Node>) -> CharFmt {
    let mut f = CharFmt::default();
    let Some(r) = rpr else {
        return f;
    };
    let pct = |k: &str| -> Option<f32> { r.attr(k)?.parse::<f32>().ok().map(|v| v / 100.0) };
    if let Some(s) = pct("sz") {
        f.size = s;
    }
    f.bold = matches!(r.attr("b"), Some("1" | "true"));
    f.italic = matches!(r.attr("i"), Some("1" | "true"));
    if let Some(u) = r.attr("u") {
        f.underline = UNDERLINES
            .iter()
            .position(|n| *n == u)
            .map_or(0, |i| i as u8);
    }
    f.strike = match r.attr("strike") {
        Some("sngStrike") => 1,
        Some("dblStrike") => 2,
        _ => 0,
    };
    if let Some(b) = r.attr("baseline").and_then(|v| v.parse::<f32>().ok()) {
        f.baseline = b / 100_000.0;
    }
    f.caps = match r.attr("cap") {
        Some("all") => 1,
        Some("small") => 2,
        _ => 0,
    };
    f.spacing = pct("spc").unwrap_or(0.0);
    f.kerning = pct("kern").unwrap_or(0.0);
    if let Some(fill) = r.child("solidFill") {
        if let Some(c) = fill
            .child("srgbClr")
            .and_then(|c| c.attr("val"))
            .and_then(rgb_to_bgr)
        {
            f.color = FontColor::Rgb(c);
        }
    } else if r.child("noFill").is_some() {
        f.color = FontColor::NoFill;
    }
    if let Some(t) = r.child("latin").and_then(|l| l.attr("typeface")) {
        if !t.starts_with('+') {
            f.name = Some(t.to_string());
        }
    }
    f
}

fn utf16_len(s: &str) -> usize {
    s.encode_utf16().count()
}

pub(crate) fn read_frame(obj: &Node, text: &str) -> TextFrame {
    let mut tf = TextFrame::default();
    let Some(body) = obj.child("txBody") else {
        return tf;
    };
    if let Some(bp) = body.child("bodyPr") {
        let over = |k: &str| match bp.attr(k) {
            Some("clip") => 1,
            Some("ellipsis") => 2,
            _ => 0,
        };
        tf.v_over = over("vertOverflow");
        tf.h_over = over("horzOverflow").min(1);
        tf.orient = bp
            .attr("vert")
            .and_then(|v| VERT.iter().position(|n| *n == v))
            .map_or(1, |i| i as u8 + 1);
        tf.wrap = bp.attr("wrap") != Some("none");
        let ins = |k: &str, d: f32| {
            bp.attr(k)
                .and_then(|v| v.parse::<f64>().ok())
                .map_or(d, |v| (v / 12700.0) as f32)
        };
        tf.margins = [
            ins("lIns", DEFAULT_MARGINS[0]),
            ins("tIns", DEFAULT_MARGINS[1]),
            ins("rIns", DEFAULT_MARGINS[2]),
            ins("bIns", DEFAULT_MARGINS[3]),
        ];
        tf.anchor = match bp.attr("anchor") {
            Some("ctr") => 3,
            Some("b") => 4,
            _ => 1,
        };
        tf.anchor_ctr = matches!(bp.attr("anchorCtr"), Some("1" | "true"));
        tf.upright = matches!(bp.attr("upright"), Some("1" | "true"));
        tf.autosize = if bp.child("spAutoFit").is_some() {
            1
        } else if bp.child("normAutofit").is_some() {
            2
        } else {
            0
        };
    }
    let paras: Vec<&Node> = body.children("p").collect();
    if let Some(p) = paras.first() {
        if let Some(pp) = p.child("pPr") {
            tf.align = pp
                .attr("algn")
                .and_then(|a| ALIGN.iter().position(|n| *n == a))
                .map_or(1, |i| i as u8 + 1);
            tf.reading = match pp.attr("rtl") {
                Some("1" | "true") => 2,
                Some("0" | "false") => 1,
                _ => 0,
            };
        }
    }
    let mut chars: Vec<CharFmt> = Vec::new();
    for (pi, p) in paras.iter().enumerate() {
        if pi > 0 {
            let last = chars.last().cloned().unwrap_or_default();
            chars.push(last);
        }
        for k in &p.kids {
            match k.name.as_str() {
                "r" | "fld" => {
                    let f = read_fmt(k.child("rPr"));
                    let mut s = String::new();
                    collect_t(k, &mut s);
                    chars.extend(std::iter::repeat_n(f, utf16_len(&s)));
                }
                "br" => chars.push(read_fmt(k.child("rPr"))),
                _ => {}
            }
        }
    }
    if text.is_empty() {
        if let Some(e) = paras.first().and_then(|p| p.child("endParaRPr")) {
            tf.base = read_fmt(Some(e));
        }
    } else {
        tf.chars = chars;
        tf.fit(utf16_len(text));
        tf.chars.truncate(utf16_len(text));
    }
    tf
}

fn collect_t(n: &Node, out: &mut String) {
    if n.name == "t" {
        out.push_str(&n.text);
    }
    for k in &n.kids {
        collect_t(k, out);
    }
}

fn rpr_attrs(f: &CharFmt) -> String {
    let mut a = format!(
        " lang=\"en-US\" sz=\"{}\"",
        (f64::from(f.size) * 100.0).round() as i64
    );
    if f.bold {
        a.push_str(" b=\"1\"");
    }
    if f.italic {
        a.push_str(" i=\"1\"");
    }
    if f.underline > 0 {
        let u = UNDERLINES.get(usize::from(f.underline)).unwrap_or(&"sng");
        a.push_str(&format!(" u=\"{u}\""));
    }
    match f.strike {
        1 => a.push_str(" strike=\"sngStrike\""),
        2 => a.push_str(" strike=\"dblStrike\""),
        _ => {}
    }
    if f.kerning != 0.0 {
        a.push_str(&format!(
            " kern=\"{}\"",
            (f64::from(f.kerning) * 100.0).round() as i64
        ));
    }
    match f.caps {
        1 => a.push_str(" cap=\"all\""),
        2 => a.push_str(" cap=\"small\""),
        _ => {}
    }
    if f.spacing != 0.0 {
        a.push_str(&format!(
            " spc=\"{}\"",
            (f64::from(f.spacing) * 100.0).round() as i64
        ));
    }
    if f.baseline != 0.0 {
        a.push_str(&format!(
            " baseline=\"{}\"",
            (f64::from(f.baseline) * 100_000.0).round() as i64
        ));
    }
    a
}

fn rpr_children(f: &CharFmt) -> String {
    let mut c = String::new();
    match f.color {
        FontColor::Rgb(v) => c.push_str(&format!(
            "<a:solidFill><a:srgbClr val=\"{}\"/></a:solidFill>",
            bgr_to_hex(v)
        )),
        FontColor::NoFill => c.push_str("<a:noFill/>"),
        FontColor::Default => {}
    }
    if let Some(n) = &f.name {
        c.push_str(&format!("<a:latin typeface=\"{}\"/>", escape_attr(n)));
    }
    c
}

fn rpr(tag: &str, f: &CharFmt) -> String {
    let kids = rpr_children(f);
    if kids.is_empty() {
        format!("<a:{tag}{}/>", rpr_attrs(f))
    } else {
        format!("<a:{tag}{}>{kids}</a:{tag}>", rpr_attrs(f))
    }
}

fn body_pr(tf: &TextFrame) -> String {
    let mut a = String::from(" vertOverflow=\"");
    a.push_str(["overflow", "clip", "ellipsis"][usize::from(tf.v_over.min(2))]);
    a.push_str("\" horzOverflow=\"");
    a.push_str(if tf.h_over == 0 { "overflow" } else { "clip" });
    a.push('"');
    if tf.orient > 1 {
        a.push_str(&format!(
            " vert=\"{}\"",
            VERT[usize::from(tf.orient.min(6)) - 1]
        ));
    }
    if !tf.wrap {
        a.push_str(" wrap=\"none\"");
    }
    for (i, k) in ["lIns", "tIns", "rIns", "bIns"].iter().enumerate() {
        if tf.margins[i] != DEFAULT_MARGINS[i] {
            a.push_str(&format!(" {k}=\"{}\"", emu(tf.margins[i])));
        }
    }
    a.push_str(" rtlCol=\"0\" anchor=\"");
    a.push_str(match tf.anchor {
        3 => "ctr",
        4 => "b",
        _ => "t",
    });
    a.push('"');
    if tf.anchor_ctr {
        a.push_str(" anchorCtr=\"1\"");
    }
    if tf.upright {
        a.push_str(" upright=\"1\"");
    }
    match tf.autosize {
        1 => format!("<a:bodyPr{a}><a:spAutoFit/></a:bodyPr>"),
        2 => format!("<a:bodyPr{a}><a:normAutofit/></a:bodyPr>"),
        _ => format!("<a:bodyPr{a}/>"),
    }
}

pub(crate) fn tx_body(s: &Shape) -> String {
    let tf = &s.props.tf;
    let mut p = String::from("<a:p>");
    let align = tf.align.clamp(1, 7);
    if align != 1 || tf.reading != 0 {
        let rtl = match tf.reading {
            1 => " rtl=\"0\"",
            2 => " rtl=\"1\"",
            _ => "",
        };
        p.push_str(&format!(
            "<a:pPr algn=\"{}\"{rtl}/>",
            ALIGN[usize::from(align) - 1]
        ));
    }
    if s.text.is_empty() {
        p.push_str(&rpr("endParaRPr", &tf.base));
    } else {
        let units: Vec<u16> = s.text.encode_utf16().collect();
        let mut i = 0;
        while i < units.len() {
            let f = tf.fmt_at(i);
            let mut j = i + 1;
            while j < units.len() && tf.fmt_at(j) == f {
                j += 1;
            }
            let run = String::from_utf16_lossy(&units[i..j]);
            p.push_str(&format!(
                "<a:r>{}<a:t>{}</a:t></a:r>",
                rpr("rPr", f),
                escape_text(&run)
            ));
            i = j;
        }
    }
    p.push_str("</a:p>");
    format!("<xdr:txBody>{}<a:lstStyle/>{p}</xdr:txBody>", body_pr(tf))
}
