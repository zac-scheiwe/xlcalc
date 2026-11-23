#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum FontColor {
    #[default]
    Default,

    Rgb(u32),

    NoFill,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CharFmt {
    pub name: Option<String>,
    pub size: f32,
    pub bold: bool,
    pub italic: bool,

    pub underline: u8,

    pub strike: u8,

    pub baseline: f32,

    pub caps: u8,
    pub spacing: f32,
    pub kerning: f32,
    pub color: FontColor,
}

impl Default for CharFmt {
    fn default() -> CharFmt {
        CharFmt {
            name: None,
            size: 11.0,
            bold: false,
            italic: false,
            underline: 0,
            strike: 0,
            baseline: 0.0,
            caps: 0,
            spacing: 0.0,
            kerning: 0.0,
            color: FontColor::Default,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TextFrame {
    pub align: u8,

    pub anchor: u8,

    pub anchor_ctr: bool,

    pub margins: [f32; 4],

    pub autosize: u8,
    pub wrap: bool,

    pub orient: u8,

    pub h_over: u8,

    pub v_over: u8,

    pub reading: u8,

    pub upright: bool,

    pub chars: Vec<CharFmt>,

    pub base: CharFmt,
}

impl Default for TextFrame {
    fn default() -> TextFrame {
        TextFrame {
            align: 1,
            anchor: 1,
            anchor_ctr: false,
            margins: [7.2, 3.6, 7.2, 3.6],
            autosize: 0,
            wrap: true,
            orient: 1,
            h_over: 1,
            v_over: 1,
            reading: 0,
            upright: false,
            chars: Vec::new(),
            base: CharFmt::default(),
        }
    }
}

impl TextFrame {
    pub fn fmt_at(&self, i: usize) -> &CharFmt {
        self.chars.get(i).unwrap_or(&self.base)
    }

    pub fn fit(&mut self, len: usize) {
        let last = self.chars.last().unwrap_or(&self.base).clone();
        self.chars.resize(len, last);
    }

    pub fn splice(&mut self, old_len: usize, start: usize, len: usize, new_len: usize) {
        self.fit(old_len);
        let start = start.min(old_len);
        let len = len.min(old_len - start);
        let replaced: Vec<CharFmt> = self.chars[start..start + len].to_vec();
        let fill = replaced
            .last()
            .or_else(|| start.checked_sub(1).and_then(|i| self.chars.get(i)))
            .unwrap_or(&self.base)
            .clone();
        let new: Vec<CharFmt> = (0..new_len)
            .map(|i| replaced.get(i).unwrap_or(&fill).clone())
            .collect();
        if new_len == 0 && old_len == len {
            if let Some(f) = replaced.first() {
                self.base = f.clone();
            }
        }
        self.chars.splice(start..start + len, new);
    }

    pub fn modify(&mut self, text_len: usize, start: usize, len: usize, f: impl Fn(&mut CharFmt)) {
        if text_len == 0 {
            f(&mut self.base);
            return;
        }
        self.fit(text_len);
        let end = (start + len).min(text_len);
        for c in &mut self.chars[start.min(end)..end] {
            f(c);
        }
    }

    pub fn range(&self, text_len: usize, start: usize, len: usize) -> Vec<&CharFmt> {
        if text_len == 0 {
            return vec![&self.base];
        }
        let end = (start + len).min(text_len);
        (start.min(end)..end).map(|i| self.fmt_at(i)).collect()
    }

    pub fn clear_text(&mut self) {
        self.chars.clear();
        self.base = CharFmt::default();
    }
}

pub const UNDERLINES: [&str; 18] = [
    "none",
    "words",
    "sng",
    "dbl",
    "heavy",
    "dotted",
    "dottedHeavy",
    "dash",
    "dashHeavy",
    "dashLong",
    "dashLongHeavy",
    "dotDash",
    "dotDashHeavy",
    "dotDotDash",
    "dotDotDashHeavy",
    "wavy",
    "wavyHeavy",
    "wavyDbl",
];

#[cfg(test)]
mod tests {
    use super::*;

    fn bold() -> CharFmt {
        CharFmt {
            bold: true,
            ..CharFmt::default()
        }
    }

    #[test]
    fn splice_keeps_positions() {
        let mut t = TextFrame {
            chars: vec![CharFmt::default(), bold(), bold()],
            ..TextFrame::default()
        };

        t.splice(3, 1, 1, 3);
        assert_eq!(t.chars.len(), 5);
        assert!(t.chars[1].bold && t.chars[2].bold && t.chars[3].bold && t.chars[4].bold);
        assert!(!t.chars[0].bold);
    }

    #[test]
    fn modify_empty_frame_sets_base() {
        let mut t = TextFrame::default();
        t.modify(0, 0, 0, |c| c.bold = true);
        assert!(t.base.bold);
        t.splice(0, 0, 0, 2);
        assert!(t.chars[0].bold && t.chars[1].bold);
    }
}
