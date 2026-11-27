use crate::ftext;
use crate::xml::Node;
use xlcalc::rich::{FontExtra, PhoneticRun};

const KINDS: [&str; 4] = [
    "halfwidthKatakana",
    "fullwidthKatakana",
    "Hiragana",
    "noConversion",
];

const ALIGNS: [&str; 4] = ["noControl", "left", "center", "distributed"];

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct PhRef {
    pub runs: Vec<PhoneticRun>,
    pub font_id: u32,
    pub kind: u8,
    pub align: u8,
}

impl PhRef {
    pub fn xml(&self) -> String {
        let mut s = String::new();
        for r in &self.runs {
            s.push_str(&format!(
                "<rPh sb=\"{}\" eb=\"{}\">{}</rPh>",
                r.from,
                r.to,
                crate::write::string_t(&r.text)
            ));
        }
        s.push_str(&format!("<phoneticPr fontId=\"{}\"", self.font_id));
        if self.kind != 1 {
            s.push_str(&format!(" type=\"{}\"", KINDS[usize::from(self.kind & 3)]));
        }
        if self.align != 1 {
            let a = ALIGNS[usize::from(self.align & 3)];
            s.push_str(&format!(" alignment=\"{a}\""));
        }
        s.push_str("/>");
        s
    }

    pub fn binary(&self, len: usize) -> (Vec<u16>, Vec<[u16; 3]>, u16) {
        let mut text = Vec::new();
        let mut runs = Vec::new();
        for r in &self.runs {
            let at = text.len() as u16;
            runs.push([at, r.from as u16, r.to.saturating_sub(r.from) as u16]);
            text.extend(xlcalc::wide::units(&r.text));
        }
        if matches!(&self.runs[..], [r] if r.from == 0 && r.to as usize == len) {
            runs.clear();
        }
        let flags = 0x30 | u16::from(self.align & 3) << 2 | u16::from(self.kind & 3);
        (text, runs, flags)
    }

    pub fn from_binary(
        text: &[u16],
        runs: &[[u16; 3]],
        len: usize,
        font_id: u32,
        flags: u16,
    ) -> PhRef {
        let whole = [[0, 0, len as u16]];
        let runs = if runs.is_empty() && !text.is_empty() {
            &whole[..]
        } else {
            runs
        };
        let runs = runs
            .iter()
            .enumerate()
            .map(|(i, &[at, from, len])| {
                let end = runs.get(i + 1).map_or(text.len(), |r| usize::from(r[0]));
                let end = end.min(text.len());
                let at = usize::from(at).min(end);
                PhoneticRun {
                    text: xlcalc::wide::from_units(&text[at..end]).into(),
                    from: u32::from(from),
                    to: u32::from(from) + u32::from(len),
                }
            })
            .collect();
        PhRef {
            runs,
            font_id,
            kind: (flags & 3) as u8,
            align: (flags >> 2 & 3) as u8,
        }
    }
}

pub(crate) fn phonetic_of(si: &Node) -> Option<PhRef> {
    let pr = si.child("phoneticPr");
    let runs: Vec<PhoneticRun> = si
        .children("rPh")
        .map(|r| {
            let pos = |k: &str| r.attr(k).and_then(|v| v.parse().ok()).unwrap_or(0);
            let text = r.child("t").map_or("", |t| t.text.as_str());
            PhoneticRun {
                text: ftext::unescape_xstring(text).into(),
                from: pos("sb"),
                to: pos("eb"),
            }
        })
        .collect();
    if pr.is_none() && runs.is_empty() {
        return None;
    }
    let attr = |k: &str| pr.and_then(|p| p.attr(k));
    let code = |k: &str, names: &[&str]| {
        attr(k)
            .and_then(|v| names.iter().position(|n| *n == v))
            .map_or(1, |i| i as u8)
    };
    Some(PhRef {
        runs,
        font_id: attr("fontId").and_then(|v| v.parse().ok()).unwrap_or(0),
        kind: code("type", &KINDS),
        align: code("alignment", &ALIGNS),
    })
}

pub(crate) fn binary_runs(si: &Node) -> Vec<(u16, Option<&Node>)> {
    let mut at = 0usize;
    let mut runs = Vec::new();
    for (i, r) in si.children("r").enumerate() {
        let rpr = r.child("rPr");
        if (i > 0 || rpr.is_some()) && at <= usize::from(u16::MAX) {
            runs.push((at as u16, rpr));
        }
        let text = r.child("t").map_or("", |t| t.text.as_str());
        at += xlcalc::wide::units(&ftext::unescape_xstring(text)).count();
    }
    runs
}

pub(crate) fn as_font(rpr: &Node) -> Node {
    let mut f = rpr.clone();
    f.name = "font".into();
    for k in &mut f.kids {
        if k.name == "rFont" {
            k.name = "name".into();
        }
    }
    f
}

pub(crate) fn font_extra(f: &Node) -> FontExtra {
    let on = |k: &str| {
        f.child(k)
            .is_some_and(|n| !matches!(n.attr("val"), Some("0" | "false")))
    };
    FontExtra {
        charset: f
            .child("charset")
            .and_then(|n| n.attr("val"))
            .and_then(|v| v.parse().ok()),
        condense: on("condense"),
        extend: on("extend"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn phonetics_go_through_the_binary_form() {
        let si = crate::xml::parse(
            "<si><t>東京都庁</t><rPh sb=\"0\" eb=\"2\"><t>トウキョウ</t></rPh>\
             <rPh sb=\"2\" eb=\"4\"><t>トチョウ</t></rPh>\
             <phoneticPr fontId=\"1\" type=\"Hiragana\" alignment=\"distributed\"/></si>"
                .as_bytes(),
        )
        .unwrap();
        let ph = phonetic_of(&si).unwrap();
        let (text, runs, flags) = ph.binary(4);

        assert_eq!(runs, [[0, 0, 2], [5, 2, 2]]);
        assert_eq!(flags, 0x3E);
        assert_eq!(PhRef::from_binary(&text, &runs, 4, 1, flags), ph);
        let xml = format!("<si>{}</si>", ph.xml());
        let again = crate::xml::parse(xml.as_bytes()).unwrap();
        assert_eq!(phonetic_of(&again).unwrap(), ph);

        let one = PhRef {
            runs: vec![PhoneticRun {
                text: "トウキョウ".into(),
                from: 0,
                to: 4,
            }],
            ..ph
        };
        let (text, runs, flags) = one.binary(4);
        assert!(runs.is_empty());
        assert_eq!(PhRef::from_binary(&text, &runs, 4, 1, flags), one);
        assert_eq!(one.binary(5).1, [[0, 0, 4]]);
    }
}
