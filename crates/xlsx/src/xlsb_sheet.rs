use crate::xlsb::{cell_name, esc, rect_name};
use std::fmt::Write as _;

#[derive(Default)]
pub(crate) struct SheetXml {
    ws_prop: Option<u16>,
    tab_color: Option<[u8; 8]>,
    view: Option<(u16, u16)>,
    pane: Option<([f64; 2], u32, u32, u32, u8)>,
    sels: Vec<String>,
    merges: Vec<String>,
    protection: String,
    tail: String,
    brk_row: Option<Vec<(u32, u32)>>,
    brk_col: Option<Vec<(u32, u32)>>,
    brk_is_row: bool,
}

fn u16_at(d: &[u8], at: usize) -> Option<u16> {
    Some(u16::from_le_bytes(d.get(at..at + 2)?.try_into().ok()?))
}

fn u32_at(d: &[u8], at: usize) -> Option<u32> {
    Some(u32::from_le_bytes(d.get(at..at + 4)?.try_into().ok()?))
}

fn f64_at(d: &[u8], at: usize) -> Option<f64> {
    Some(f64::from_le_bytes(d.get(at..at + 8)?.try_into().ok()?))
}

const PANES: [&str; 4] = ["bottomRight", "topRight", "bottomLeft", "topLeft"];

impl SheetXml {
    pub(crate) fn ws_prop(&mut self, d: &[u8]) {
        self.ws_prop = u16_at(d, 0);

        self.tab_color = d
            .get(3..11)
            .and_then(|c| <[u8; 8]>::try_from(c).ok())
            .filter(|c| c[0] >> 1 != 0);
    }

    pub(crate) fn sheet_pr(&self, code_name: Option<&str>) -> String {
        let f = self.ws_prop.unwrap_or(0x04C9);
        let mut inner = String::new();
        let mut outline = String::new();
        if f & 0x40 == 0 {
            outline.push_str(" summaryBelow=\"0\"");
        }
        if f & 0x80 == 0 {
            outline.push_str(" summaryRight=\"0\"");
        }
        if !outline.is_empty() {
            let _ = write!(inner, "<outlinePr{outline}/>");
        }
        if f & 0x100 != 0 {
            inner.push_str("<pageSetUpPr fitToPage=\"1\"/>");
        }
        if let Some(c) = self.tab_color {
            inner.insert_str(0, &crate::xlsb_styles::color_xml("tabColor", c));
        }
        let code = code_name.map_or(String::new(), |c| format!(" codeName=\"{}\"", esc(c)));
        if inner.is_empty() && code.is_empty() {
            String::new()
        } else if inner.is_empty() {
            format!("<sheetPr{code}/>")
        } else {
            format!("<sheetPr{code}>{inner}</sheetPr>")
        }
    }

    pub(crate) fn view(&mut self, d: &[u8]) {
        if let (Some(f), Some(z)) = (u16_at(d, 0), u16_at(d, 22)) {
            self.view = Some((f, z));
        }
    }

    pub(crate) fn pane(&mut self, d: &[u8]) {
        if let (Some(x), Some(y), Some(r), Some(c), Some(p)) = (
            f64_at(d, 0),
            f64_at(d, 8),
            u32_at(d, 16),
            u32_at(d, 20),
            u32_at(d, 24),
        ) {
            self.pane = Some(([x, y], r, c, p, d.get(28).copied().unwrap_or(0)));
        }
    }

    pub(crate) fn select(&mut self, d: &[u8]) {
        let (Some(pnn), Some(r), Some(c)) = (u32_at(d, 0), u32_at(d, 4), u32_at(d, 8)) else {
            return;
        };
        let rng = match (u32_at(d, 20), u32_at(d, 24), u32_at(d, 28), u32_at(d, 32)) {
            (Some(r0), Some(r1), Some(c0), Some(c1)) => rect_name(r0, c0, r1, c1),
            _ => cell_name(r, c),
        };
        let pane = PANES.get(pnn as usize).copied().unwrap_or("topLeft");
        let mut s = String::from("<selection");
        if pnn != 3 {
            let _ = write!(s, " pane=\"{pane}\"");
        }
        let _ = write!(s, " activeCell=\"{}\" sqref=\"{rng}\"/>", cell_name(r, c));
        self.sels.push(s);
    }

    pub(crate) fn views(&self) -> String {
        let (f, zoom) = self.view.unwrap_or((0x039C, 100));
        let mut a = String::new();
        if f & 0x02 != 0 {
            a.push_str(" showFormulas=\"1\"");
        }
        if f & 0x04 == 0 {
            a.push_str(" showGridLines=\"0\"");
        }
        if f & 0x08 == 0 {
            a.push_str(" showRowColHeaders=\"0\"");
        }
        if f & 0x10 == 0 {
            a.push_str(" showZeros=\"0\"");
        }
        if f & 0x20 != 0 {
            a.push_str(" rightToLeft=\"1\"");
        }
        if f & 0x40 != 0 {
            a.push_str(" tabSelected=\"1\"");
        }
        if zoom != 100 && zoom != 0 {
            let _ = write!(a, " zoomScale=\"{zoom}\" zoomScaleNormal=\"{zoom}\"");
        }
        let mut inner = String::new();
        if let Some(([x, y], r, c, p, fl)) = self.pane {
            inner.push_str("<pane");
            if x > 0.0 {
                let _ = write!(inner, " xSplit=\"{x}\"");
            }
            if y > 0.0 {
                let _ = write!(inner, " ySplit=\"{y}\"");
            }
            let _ = write!(inner, " topLeftCell=\"{}\"", cell_name(r, c));
            let _ = write!(
                inner,
                " activePane=\"{}\"",
                PANES.get(p as usize).copied().unwrap_or("topLeft")
            );
            let state = match fl & 3 {
                0 => "split",
                1 => "frozenSplit",
                _ => "frozen",
            };
            if state != "split" {
                let _ = write!(inner, " state=\"{state}\"");
            }
            inner.push_str("/>");
        }
        for s in &self.sels {
            inner.push_str(s);
        }
        let sel_only = self.pane.is_none()
            && self.sels.len() == 1
            && self.sels[0].contains("activeCell=\"A1\" sqref=\"A1\"");
        let inner = if sel_only { String::new() } else { inner };
        if inner.is_empty() {
            format!("<sheetViews><sheetView{a} workbookViewId=\"0\"/></sheetViews>")
        } else {
            format!(
                "<sheetViews><sheetView{a} workbookViewId=\"0\">{inner}</sheetView></sheetViews>"
            )
        }
    }

    pub(crate) fn merge(&mut self, d: &[u8]) {
        if let (Some(r0), Some(r1), Some(c0), Some(c1)) =
            (u32_at(d, 0), u32_at(d, 4), u32_at(d, 8), u32_at(d, 12))
        {
            self.merges.push(rect_name(r0, c0, r1, c1));
        }
    }

    pub(crate) fn merges_xml(&self) -> String {
        if self.merges.is_empty() {
            return String::new();
        }
        let mut s = format!("<mergeCells count=\"{}\">", self.merges.len());
        for m in &self.merges {
            let _ = write!(s, "<mergeCell ref=\"{m}\"/>");
        }
        s.push_str("</mergeCells>");
        s
    }

    pub(crate) fn protection(&mut self, d: &[u8]) {
        let Some(pwd) = u16_at(d, 0) else { return };
        let w = |i: usize| u32_at(d, 2 + 4 * i).unwrap_or(0) != 0;
        let mut s = String::from("<sheetProtection");
        if pwd != 0 {
            let _ = write!(s, " password=\"{pwd:04X}\"");
        }
        if w(0) {
            s.push_str(" sheet=\"1\"");
        }
        for (i, (k, locked_by_default)) in PROT.iter().enumerate() {
            let locked = !w(i + 1);
            if locked != *locked_by_default {
                let _ = write!(s, " {k}=\"{}\"", u8::from(locked));
            }
        }
        s.push_str("/>");
        self.protection = s;
    }

    pub(crate) fn protection_xml(&self) -> &str {
        &self.protection
    }

    pub(crate) fn print_options(&mut self, d: &[u8]) {
        let f = u16_at(d, 0).unwrap_or(0);
        let mut s = String::new();
        for (bit, k) in [
            (0x04, "headings"),
            (0x08, "gridLines"),
            (0x01, "horizontalCentered"),
            (0x02, "verticalCentered"),
        ] {
            if f & bit != 0 {
                let _ = write!(s, " {k}=\"1\"");
            }
        }
        if !s.is_empty() {
            self.tail.insert_str(0, &format!("<printOptions{s}/>"));
        }
    }

    pub(crate) fn margins(&mut self, d: &[u8]) {
        let v: Vec<f64> = (0..6).filter_map(|i| f64_at(d, 8 * i)).collect();
        if let [l, r, t, b, h, f] = v[..] {
            let _ = write!(
                self.tail,
                "<pageMargins left=\"{l}\" right=\"{r}\" top=\"{t}\" bottom=\"{b}\" header=\"{h}\" footer=\"{f}\"/>"
            );
        }
    }

    pub(crate) fn page_setup(&mut self, d: &[u8]) {
        let g = |i: usize| u32_at(d, 4 * i).unwrap_or(0);
        let Some(fl) = u16_at(d, 32) else { return };
        let mut s = String::from("<pageSetup");
        if g(0) != 1 {
            let _ = write!(s, " paperSize=\"{}\"", g(0));
        }
        if g(1) != 100 {
            let _ = write!(s, " scale=\"{}\"", g(1));
        }
        if fl & 0x80 != 0 {
            let _ = write!(s, " firstPageNumber=\"{}\" useFirstPageNumber=\"1\"", g(5));
        }
        if g(6) != 1 {
            let _ = write!(s, " fitToWidth=\"{}\"", g(6));
        }
        if g(7) != 1 {
            let _ = write!(s, " fitToHeight=\"{}\"", g(7));
        }
        if fl & 1 != 0 {
            s.push_str(" pageOrder=\"overThenDown\"");
        }
        if fl & 0x40 == 0 {
            let _ = write!(
                s,
                " orientation=\"{}\"",
                if fl & 2 != 0 { "landscape" } else { "portrait" }
            );
        }
        if fl & 0x08 != 0 {
            s.push_str(" blackAndWhite=\"1\"");
        }
        if fl & 0x10 != 0 {
            s.push_str(" draft=\"1\"");
        }
        if fl & 0x20 != 0 {
            s.push_str(" cellComments=\"asDisplayed\"");
        }
        if g(2) != 600 && g(2) != 0 {
            let _ = write!(s, " horizontalDpi=\"{}\"", g(2));
        }
        if g(3) != 600 && g(3) != 0 {
            let _ = write!(s, " verticalDpi=\"{}\"", g(3));
        }
        if g(4) != 1 {
            let _ = write!(s, " copies=\"{}\"", g(4));
        }
        s.push_str("/>");
        self.tail.push_str(&s);
    }

    pub(crate) fn header_footer(&mut self, d: &[u8]) {
        let Some(fl) = u16_at(d, 0) else { return };
        let mut at = 2;
        let mut texts: Vec<String> = Vec::new();
        for _ in 0..6 {
            let Some(n) = u32_at(d, at) else { break };
            at += 4;
            if n == 0xFFFF_FFFF {
                texts.push(String::new());
                continue;
            }
            let end = at + 2 * n as usize;
            let Some(b) = d.get(at..end) else { break };
            let u: Vec<u16> = b
                .as_chunks::<2>()
                .0
                .iter()
                .map(|c| u16::from_le_bytes(*c))
                .collect();
            texts.push(String::from_utf16_lossy(&u));
            at = end;
        }
        let mut a = String::new();
        if fl & 1 != 0 {
            a.push_str(" differentOddEven=\"1\"");
        }
        if fl & 2 != 0 {
            a.push_str(" differentFirst=\"1\"");
        }
        if fl & 4 == 0 {
            a.push_str(" scaleWithDoc=\"0\"");
        }
        if fl & 8 == 0 {
            a.push_str(" alignWithMargins=\"0\"");
        }
        let mut inner = String::new();
        for (t, k) in texts.iter().zip([
            "oddHeader",
            "oddFooter",
            "evenHeader",
            "evenFooter",
            "firstHeader",
            "firstFooter",
        ]) {
            if !t.is_empty() {
                let _ = write!(inner, "<{k}>{}</{k}>", esc(t));
            }
        }
        if !inner.is_empty() || !a.is_empty() {
            let _ = write!(self.tail, "<headerFooter{a}>{inner}</headerFooter>");
        }
    }

    pub(crate) fn breaks_begin(&mut self, rows: bool) {
        self.brk_is_row = rows;
        if rows {
            self.brk_row = Some(Vec::new());
        } else {
            self.brk_col = Some(Vec::new());
        }
    }

    pub(crate) fn brk(&mut self, d: &[u8]) {
        let (Some(id), Some(max)) = (u32_at(d, 0), u32_at(d, 8)) else {
            return;
        };
        let list = if self.brk_is_row {
            &mut self.brk_row
        } else {
            &mut self.brk_col
        };
        if let Some(l) = list {
            l.push((id, max));
        }
    }

    pub(crate) fn tail_xml(&self) -> String {
        const ORDER: [&str; 4] = [
            "<printOptions",
            "<pageMargins",
            "<pageSetup",
            "<headerFooter",
        ];
        let mut starts: Vec<usize> = ORDER
            .iter()
            .flat_map(|t| self.tail.match_indices(t).map(|(at, _)| at))
            .collect();
        starts.sort_unstable();
        let mut by_rank: Vec<(usize, &str)> = starts
            .iter()
            .enumerate()
            .map(|(i, &at)| {
                let end = starts.get(i + 1).copied().unwrap_or(self.tail.len());
                let piece = &self.tail[at..end];
                let rank = ORDER.iter().position(|t| piece.starts_with(t)).unwrap_or(0);
                (rank, piece)
            })
            .collect();
        by_rank.sort_by_key(|(rank, _)| *rank);
        let mut s = String::new();
        s.push_str(&self.tail[..starts.first().copied().unwrap_or(self.tail.len())]);
        for (_, p) in &by_rank {
            s.push_str(p);
        }
        for (tag, list) in [("rowBreaks", &self.brk_row), ("colBreaks", &self.brk_col)] {
            if let Some(l) = list.as_ref().filter(|l| !l.is_empty()) {
                let _ = write!(s, "<{tag} count=\"{0}\" manualBreakCount=\"{0}\">", l.len());
                for (id, max) in l {
                    let _ = write!(s, "<brk id=\"{id}\" max=\"{max}\" man=\"1\"/>");
                }
                let _ = write!(s, "</{tag}>");
            }
        }
        s
    }
}

pub(crate) const PROT: [(&str, bool); 15] = [
    ("objects", false),
    ("scenarios", false),
    ("formatCells", true),
    ("formatColumns", true),
    ("formatRows", true),
    ("insertColumns", true),
    ("insertRows", true),
    ("insertHyperlinks", true),
    ("deleteColumns", true),
    ("deleteRows", true),
    ("selectLockedCells", false),
    ("sort", true),
    ("autoFilter", true),
    ("pivotTables", true),
    ("selectUnlockedCells", false),
];
