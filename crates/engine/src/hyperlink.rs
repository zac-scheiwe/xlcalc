use crate::structure::{Axis, Shift};
use crate::{Rect, SheetId, Workbook};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Hyperlink {
    pub id: u32,
    pub rect: Rect,

    pub address: String,
    pub sub_address: String,

    pub screen_tip: Option<String>,

    pub name: Option<String>,
}

impl Hyperlink {
    pub fn display_name(&self) -> Option<String> {
        if let Some(n) = &self.name {
            return Some(n.clone());
        }
        match (self.address.is_empty(), self.sub_address.is_empty()) {
            (true, true) => None,
            (false, true) => Some(self.address.clone()),
            (true, false) => Some(self.sub_address.clone()),
            (false, false) => Some(format!("{} - {}", self.address, self.sub_address)),
        }
    }
}

pub fn normalize_address(raw: &str) -> (String, bool) {
    let t = raw.trim();
    if let Some(rest) = t.strip_prefix('#') {
        return (rest.to_string(), true);
    }
    let lower = t.to_ascii_lowercase();
    if lower.starts_with("file:///") {
        let path = percent_decode(&t[8..]).replace('/', "\\");
        return (path, false);
    }
    for scheme in ["http://", "https://", "ftp://"] {
        if lower.starts_with(scheme) {
            let rest = &t[scheme.len()..];
            let end = rest.find(['/', '?', '#']).unwrap_or(rest.len());
            let (host, tail) = rest.split_at(end);
            let tail = if tail.starts_with('/') {
                tail.to_string()
            } else {
                format!("/{tail}")
            };
            return (
                format!("{scheme}{}{tail}", host.to_ascii_lowercase()),
                false,
            );
        }
    }
    if lower.starts_with("mailto:") {
        return (t.to_string(), false);
    }
    (percent_decode(t), false)
}

pub fn split_fragment(address: &str) -> (String, Option<String>) {
    if address.to_ascii_lowercase().starts_with("mailto:") {
        return (address.to_string(), None);
    }
    match address.split_once('#') {
        Some((a, f)) => (a.to_string(), Some(f.to_string())),
        None => (address.to_string(), None),
    }
}

fn percent_decode(s: &str) -> String {
    let b = s.as_bytes();
    let mut out: Vec<u8> = Vec::with_capacity(b.len());
    let mut i = 0;
    while i < b.len() {
        if b[i] == b'%' && i + 2 < b.len() {
            if let Ok(v) = u8::from_str_radix(&s[i + 1..i + 3], 16) {
                out.push(v);
                i += 3;
                continue;
            }
        }
        out.push(b[i]);
        i += 1;
    }
    String::from_utf8_lossy(&out).into_owned()
}

impl Workbook {
    pub fn hyperlinks(&self, sheet: SheetId) -> &[Hyperlink] {
        self.hyperlinks.get(&sheet).map_or(&[], Vec::as_slice)
    }

    pub fn hyperlink_add(&mut self, sheet: SheetId, mut h: Hyperlink) -> u32 {
        self.hl_next += 1;
        h.id = self.hl_next;
        let id = h.id;

        if let Some(old) = self
            .hyperlinks
            .get_mut(&sheet)
            .and_then(|v| v.iter_mut().find(|o| o.rect == h.rect))
        {
            *old = h;
            return id;
        }
        self.hl_clear(sheet, &h.rect);
        self.hyperlinks.entry(sheet).or_default().push(h);
        id
    }

    pub fn hyperlink_mut(&mut self, sheet: SheetId, id: u32) -> Option<&mut Hyperlink> {
        self.hyperlinks
            .get_mut(&sheet)
            .and_then(|v| v.iter_mut().find(|h| h.id == id))
    }

    pub fn hyperlink_remove(&mut self, sheet: SheetId, id: u32) -> bool {
        let Some(v) = self.hyperlinks.get_mut(&sheet) else {
            return false;
        };
        let n = v.len();
        v.retain(|h| h.id != id);
        v.len() != n
    }

    pub fn hl_clear(&mut self, sheet: SheetId, r: &Rect) {
        if let Some(v) = self.hyperlinks.get_mut(&sheet) {
            v.retain(|h| h.rect.intersection(r).is_none());
        }
    }

    pub fn hl_paste(&mut self, ss: SheetId, src: Rect, ds: SheetId, pasted: Rect, cut: bool) {
        let inside: Vec<Hyperlink> = self
            .hyperlinks(ss)
            .iter()
            .filter(|h| src.intersection(&h.rect).is_some_and(|i| i == h.rect))
            .cloned()
            .collect();
        if cut {
            let ids: Vec<u32> = inside.iter().map(|h| h.id).collect();
            if let Some(v) = self.hyperlinks.get_mut(&ss) {
                v.retain(|h| !ids.contains(&h.id));
            }
        }
        self.hl_clear(ds, &pasted);
        let (rows, cols) = (src.rows(), src.cols());
        let mut r = pasted.r0;
        while r + rows - 1 <= pasted.r1 {
            let mut c = pasted.c0;
            while c + cols - 1 <= pasted.c1 {
                for h in &inside {
                    let mut n = h.clone();
                    n.rect = Rect::new(
                        h.rect.r0 - src.r0 + r,
                        h.rect.c0 - src.c0 + c,
                        h.rect.r1 - src.r0 + r,
                        h.rect.c1 - src.c0 + c,
                    );
                    if !cut {
                        self.hl_next += 1;
                        n.id = self.hl_next;
                    }
                    self.hyperlinks.entry(ds).or_default().push(n);
                }
                c += cols;
            }
            r += rows;
        }
    }

    pub(crate) fn hl_shift(&mut self, s: &Shift) {
        let rows = s.axis == Axis::Rows;
        let Some(v) = self.hyperlinks.get_mut(&s.sheet) else {
            return;
        };
        v.retain_mut(|h| {
            if let Some((p0, p1)) = s.limit {
                let (a, b) = if rows {
                    (h.rect.c0, h.rect.c1)
                } else {
                    (h.rect.r0, h.rect.r1)
                };
                if a < p0 || b > p1 {
                    return true;
                }
            }
            let (a, b) = if rows {
                (h.rect.r0, h.rect.r1)
            } else {
                (h.rect.c0, h.rect.c1)
            };
            let Some((na, nb)) = s.map_span(a, b) else {
                return false;
            };
            if rows {
                h.rect.r0 = na;
                h.rect.r1 = nb;
            } else {
                h.rect.c0 = na;
                h.rect.c1 = nb;
            }
            true
        });
    }

    pub(crate) fn hl_drop_sheet(&mut self, id: SheetId) {
        self.hyperlinks.remove(&id);
    }

    pub(crate) fn hl_copy_sheet(&mut self, src: SheetId, new: SheetId) {
        if let Some(mut v) = self.hyperlinks.get(&src).cloned() {
            for h in &mut v {
                self.hl_next += 1;
                h.id = self.hl_next;
            }
            self.hyperlinks.insert(new, v);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::normalize_address as n;

    #[test]
    fn addresses_follow_excel() {
        assert_eq!(n("http://x.org"), ("http://x.org/".into(), false));
        assert_eq!(n("HTTP://X.ORG"), ("http://x.org/".into(), false));
        assert_eq!(n("http://x.org?q=1"), ("http://x.org/?q=1".into(), false));
        assert_eq!(
            n("http://example.com/a"),
            ("http://example.com/a".into(), false)
        );
        assert_eq!(n("file:///C:/a.txt"), ("C:\\a.txt".into(), false));
        assert_eq!(n("a%20b.txt"), ("a b.txt".into(), false));
        assert_eq!(n("  http://sp/p  "), ("http://sp/p".into(), false));
        assert_eq!(n("#Sheet2!A1"), ("Sheet2!A1".into(), true));
        assert_eq!(n("x.org"), ("x.org".into(), false));
        assert_eq!(
            super::split_fragment("https://x.org/p#top"),
            ("https://x.org/p".into(), Some("top".into()))
        );
        assert_eq!(super::split_fragment("mailto:a@b.c#x").1, None);
    }
}
