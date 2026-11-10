use super::Rec;
use crate::xlsb::esc;
use std::fmt::Write;

pub(super) struct Note {
    pub row: u32,
    pub col: u32,
    pub visible: bool,
    pub id: u16,
    pub author: String,
}

#[derive(Default, Clone)]
pub(super) struct NoteObj {
    pub id: u16,
    pub anchor: Option<[u16; 8]>,
    pub text: Option<String>,
}

pub(super) fn note(d: &[u8]) -> Option<Note> {
    if d.len() < 10 {
        return None;
    }
    let u = |i: usize| u32::from(u16::from_le_bytes([d[i], d[i + 1]]));
    let cch = u(8) as usize;
    let wide = d.get(10).copied()? & 1 != 0;
    let body = d.get(11..)?;
    let author = if wide {
        let u16s: Vec<u16> = body
            .as_chunks::<2>()
            .0
            .iter()
            .take(cch)
            .map(|c| u16::from_le_bytes(*c))
            .collect();
        String::from_utf16_lossy(&u16s)
    } else {
        body.iter().take(cch).map(|&b| char::from(b)).collect()
    };
    Some(Note {
        row: u(0),
        col: u(2),
        visible: u(4) & 2 != 0,
        id: u(6) as u16,
        author,
    })
}

pub(super) fn obj_note_id(d: &[u8]) -> Option<u16> {
    if d.len() < 8 || d[0] != 0x15 || d[1] != 0 {
        return None;
    }
    let ot = u16::from_le_bytes([d[4], d[5]]);
    (ot == 0x19).then(|| u16::from_le_bytes([d[6], d[7]]))
}

pub(super) fn txo_text(r: &Rec<'_>) -> String {
    let cch =
        r.d.get(10..12)
            .map_or(0, |b| usize::from(u16::from_le_bytes([b[0], b[1]])));
    let mut s = String::new();
    let mut n = 0;
    for c in &r.cont {
        if n >= cch {
            break;
        }
        let Some((&flag, body)) = c.split_first() else {
            continue;
        };
        if flag & 1 != 0 {
            let u16s: Vec<u16> = body
                .as_chunks::<2>()
                .0
                .iter()
                .take(cch - n)
                .map(|c| u16::from_le_bytes(*c))
                .collect();
            n += u16s.len();
            s.push_str(&String::from_utf16_lossy(&u16s));
        } else {
            let t: String = body.iter().take(cch - n).map(|&b| char::from(b)).collect();
            n += t.chars().count();
            s.push_str(&t);
        }
    }
    s.replace('\r', "\n")
}

fn cell_ref(row: u32, col: u32) -> String {
    let mut c = col;
    let mut name = String::new();
    loop {
        name.insert(0, char::from(b'A' + (c % 26) as u8));
        if c < 26 {
            break;
        }
        c = c / 26 - 1;
    }
    format!("{name}{}", row + 1)
}

pub(super) fn parts(
    notes: &[(Note, NoteObj)],
    n: u32,
    dims: &super::wdraw::Dims,
) -> (String, String) {
    let mut authors: Vec<&str> = Vec::new();
    let mut list = String::new();
    let mut vml = format!(
        "<xml xmlns:v=\"urn:schemas-microsoft-com:vml\" xmlns:o=\"urn:schemas-microsoft-com:office:office\" xmlns:x=\"urn:schemas-microsoft-com:office:excel\"><o:shapelayout v:ext=\"edit\"><o:idmap v:ext=\"edit\" data=\"{n}\"/></o:shapelayout><v:shapetype id=\"_x0000_t202\" coordsize=\"21600,21600\" o:spt=\"202\" path=\"m,l,21600r21600,l21600,xe\"><v:stroke joinstyle=\"miter\"/><v:path gradientshapeok=\"t\" o:connecttype=\"rect\"/></v:shapetype>"
    );
    for (i, (nt, obj)) in notes.iter().enumerate() {
        let ai = authors
            .iter()
            .position(|a| *a == nt.author)
            .unwrap_or_else(|| {
                authors.push(&nt.author);
                authors.len() - 1
            });
        let text = obj.text.clone().unwrap_or_default();
        let _ = write!(
            list,
            "<comment ref=\"{}\" authorId=\"{ai}\"><text><t xml:space=\"preserve\">{}</t></text></comment>",
            cell_ref(nt.row, nt.col),
            esc(&text)
        );
        let a = obj.anchor.unwrap_or([
            nt.col as u16 + 1,
            15,
            nt.row.saturating_sub(1) as u16,
            10,
            nt.col as u16 + 3,
            15,
            nt.row as u16 + 3,
            16,
        ]);
        let raw = a;

        let px =
            |c: u16, d: u16| (f64::from(d) / 1024.0 * dims.col_px(u32::from(c))).round() as u32;
        let py = |r: u16, d: u16| (f64::from(d) / 256.0 * dims.row_px(u32::from(r))).round() as u32;
        let a = [
            u32::from(a[0]),
            px(a[0], a[1]),
            u32::from(a[2]),
            py(a[2], a[3]),
            u32::from(a[4]),
            px(a[4], a[5]),
            u32::from(a[6]),
            py(a[6], a[7]),
        ];
        let _ = write!(
            vml,
            "<v:shape id=\"_x0000_s{}\" type=\"#_x0000_t202\" alt=\"xlanchor:{}\" style=\"position:absolute;margin-left:80pt;margin-top:2pt;width:108pt;height:59pt;z-index:{};visibility:{}\" fillcolor=\"#ffffe1\" o:insetmode=\"auto\"><v:fill color2=\"#ffffe1\"/><v:shadow on=\"t\" color=\"black\" obscured=\"t\"/><v:path o:connecttype=\"none\"/><v:textbox style=\"mso-direction-alt:auto\"><div style=\"text-align:left\"></div></v:textbox><x:ClientData ObjectType=\"Note\"><x:MoveWithCells/><x:SizeWithCells/><x:Anchor>{}, {}, {}, {}, {}, {}, {}, {}</x:Anchor><x:AutoFill>False</x:AutoFill><x:Row>{}</x:Row><x:Column>{}</x:Column>{}</x:ClientData></v:shape>",
            n * 1024 + 1 + i as u32,
            raw.iter().map(u16::to_string).collect::<Vec<_>>().join(","),
            i + 1,
            if nt.visible { "visible" } else { "hidden" },
            a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
            nt.row,
            nt.col,
            if nt.visible { "<x:Visible/>" } else { "" }
        );
    }
    vml.push_str("</xml>");
    let mut x = String::from(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<comments xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><authors>",
    );
    for a in authors {
        let _ = write!(x, "<author>{}</author>", esc(a));
    }
    let _ = write!(x, "</authors><commentList>{list}</commentList></comments>");
    (x, vml)
}
