use std::fmt;

pub const MAX_ROWS: u32 = 1_048_576;

pub const MAX_COLS: u32 = 16_384;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct SheetId(pub u32);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct CellAddr {
    pub sheet: SheetId,

    pub row: u32,

    pub col: u32,
}

impl CellAddr {
    pub fn new(sheet: SheetId, row: u32, col: u32) -> CellAddr {
        CellAddr { sheet, row, col }
    }

    pub fn a1(&self) -> String {
        format!("{}{}", col_to_letters(self.col), self.row + 1)
    }

    pub fn rc_key(&self) -> u64 {
        ((self.row as u64) << 14) | self.col as u64
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Rect {
    pub r0: u32,

    pub c0: u32,

    pub r1: u32,

    pub c1: u32,
}

impl Rect {
    pub fn new(r0: u32, c0: u32, r1: u32, c1: u32) -> Rect {
        Rect {
            r0: r0.min(r1),
            c0: c0.min(c1),
            r1: r0.max(r1),
            c1: c0.max(c1),
        }
    }

    pub fn cell(r: u32, c: u32) -> Rect {
        Rect {
            r0: r,
            c0: c,
            r1: r,
            c1: c,
        }
    }

    pub fn rows(&self) -> u32 {
        self.r1 - self.r0 + 1
    }

    pub fn cols(&self) -> u32 {
        self.c1 - self.c0 + 1
    }

    pub fn cell_count(&self) -> u64 {
        self.rows() as u64 * self.cols() as u64
    }

    pub fn is_cell(&self) -> bool {
        self.r0 == self.r1 && self.c0 == self.c1
    }

    pub fn contains(&self, r: u32, c: u32) -> bool {
        r >= self.r0 && r <= self.r1 && c >= self.c0 && c <= self.c1
    }

    pub fn intersects(&self, o: &Rect) -> bool {
        self.r0 <= o.r1 && o.r0 <= self.r1 && self.c0 <= o.c1 && o.c0 <= self.c1
    }

    pub fn intersection(&self, o: &Rect) -> Option<Rect> {
        if self.intersects(o) {
            Some(Rect {
                r0: self.r0.max(o.r0),
                c0: self.c0.max(o.c0),
                r1: self.r1.min(o.r1),
                c1: self.c1.min(o.c1),
            })
        } else {
            None
        }
    }

    pub fn union_bounds(&self, o: &Rect) -> Rect {
        Rect {
            r0: self.r0.min(o.r0),
            c0: self.c0.min(o.c0),
            r1: self.r1.max(o.r1),
            c1: self.c1.max(o.c1),
        }
    }

    pub fn is_full_columns(&self) -> bool {
        self.r0 == 0 && self.r1 == MAX_ROWS - 1
    }

    pub fn is_full_rows(&self) -> bool {
        self.c0 == 0 && self.c1 == MAX_COLS - 1
    }

    pub fn a1(&self) -> String {
        if self.is_full_columns() && !self.is_full_rows() {
            format!("{}:{}", col_to_letters(self.c0), col_to_letters(self.c1))
        } else if self.is_full_rows() && !self.is_full_columns() {
            format!("{}:{}", self.r0 + 1, self.r1 + 1)
        } else if self.is_cell() {
            format!("{}{}", col_to_letters(self.c0), self.r0 + 1)
        } else {
            format!(
                "{}{}:{}{}",
                col_to_letters(self.c0),
                self.r0 + 1,
                col_to_letters(self.c1),
                self.r1 + 1
            )
        }
    }
}

impl fmt::Display for Rect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.a1())
    }
}

pub fn col_to_letters(mut col: u32) -> String {
    let mut buf = [0u8; 4];
    let mut i = buf.len();
    loop {
        i -= 1;
        buf[i] = b'A' + (col % 26) as u8;
        if col < 26 {
            break;
        }
        col = col / 26 - 1;
    }
    String::from_utf8_lossy(&buf[i..]).into_owned()
}

pub fn letters_to_col(s: &str) -> Option<u32> {
    if s.is_empty() || s.len() > 3 {
        return None;
    }
    let mut n: u32 = 0;
    for b in s.bytes() {
        let d = match b {
            b'A'..=b'Z' => b - b'A',
            b'a'..=b'z' => b - b'a',
            _ => return None,
        } as u32;
        n = n * 26 + d + 1;
    }
    let col = n - 1;
    (col < MAX_COLS).then_some(col)
}

pub fn parse_a1_cell(s: &str) -> Option<(u32, u32, bool, bool)> {
    let b = s.as_bytes();
    let mut i = 0;
    let col_abs = b.first() == Some(&b'$');
    if col_abs {
        i += 1;
    }
    let ls = i;
    while i < b.len() && b[i].is_ascii_alphabetic() {
        i += 1;
    }
    let col = letters_to_col(&s[ls..i])?;
    let row_abs = b.get(i) == Some(&b'$');
    if row_abs {
        i += 1;
    }
    let ds = i;
    while i < b.len() && b[i].is_ascii_digit() {
        i += 1;
    }

    let nz = ds + s[ds..i].bytes().take_while(|&c| c == b'0').count();
    if i != b.len() || ds == i || nz == i || i - nz > 7 {
        return None;
    }
    let row: u32 = s[nz..i].parse().ok()?;
    (row >= 1 && row <= MAX_ROWS).then_some((row - 1, col, row_abs, col_abs))
}

pub fn parse_a1_rect(s: &str) -> Option<Rect> {
    if let Some((a, b)) = s.split_once(':') {
        if let (Some(x), Some(y)) = (parse_a1_cell(a), parse_a1_cell(b)) {
            return Some(Rect::new(x.0, x.1, y.0, y.1));
        }
        let strip = |t: &str| t.trim_start_matches('$').to_string();
        if let (Some(c0), Some(c1)) = (letters_to_col(&strip(a)), letters_to_col(&strip(b))) {
            return Some(Rect::new(0, c0, MAX_ROWS - 1, c1));
        }
        let (ra, rb) = (strip(a), strip(b));
        if let (Ok(r0), Ok(r1)) = (ra.parse::<u32>(), rb.parse::<u32>()) {
            if (1..=MAX_ROWS).contains(&r0) && (1..=MAX_ROWS).contains(&r1) {
                return Some(Rect::new(r0 - 1, 0, r1 - 1, MAX_COLS - 1));
            }
        }
        None
    } else {
        parse_a1_cell(s).map(|(r, c, _, _)| Rect::cell(r, c))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn columns_round_trip() {
        for (c, s) in [
            (0, "A"),
            (25, "Z"),
            (26, "AA"),
            (701, "ZZ"),
            (702, "AAA"),
            (16383, "XFD"),
        ] {
            assert_eq!(col_to_letters(c), s);
            assert_eq!(letters_to_col(s), Some(c));
        }
        assert_eq!(letters_to_col("XFE"), None);
    }

    #[test]
    fn cells_and_rects() {
        assert_eq!(parse_a1_cell("$B$3"), Some((2, 1, true, true)));
        assert_eq!(parse_a1_cell("A0"), None);
        assert_eq!(parse_a1_cell("A1048577"), None);
        assert_eq!(parse_a1_rect("A:B").unwrap().a1(), "A:B");
        assert_eq!(parse_a1_rect("3:2").unwrap().a1(), "2:3");
        assert_eq!(parse_a1_rect("C5:A1").unwrap().a1(), "A1:C5");
    }
}
