use super::parse::{Body, DateTok, Item};
use super::NumberFormat;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum VOp {
    Date,

    Time,

    General,

    DollarFirst,

    CurrencyTag,

    Header(u8),

    Sci,

    Frac,

    Fill,
}

#[derive(Clone, Debug)]
pub(crate) struct Fm {
    sections: Arc<Vec<Vec<VOp>>>,

    custom: bool,

    pub(crate) ty: u8,

    pub(crate) has_date: bool,

    pub(crate) has_time: bool,
}

impl Fm {
    fn general() -> Fm {
        Fm {
            sections: Arc::new(Vec::new()),
            custom: false,
            ty: 0,
            has_date: false,
            has_time: false,
        }
    }

    pub(crate) fn of(code: &str, custom: bool) -> Fm {
        if code.eq_ignore_ascii_case("general") {
            return Fm::general();
        }
        let Ok(nf) = NumberFormat::parse(code) else {
            return Fm::general();
        };
        let raw = split_raw(code);
        let mut sections: Vec<Vec<VOp>> = Vec::new();
        let (mut has_date, mut has_time) = (false, false);
        for (i, s) in nf.sections.iter().enumerate() {
            let rawsec = raw.get(i).map(String::as_str).unwrap_or("");
            let (ops, d, t) = section_ops(&s.body, rawsec, i == 1);
            has_date |= d;
            has_time |= t;
            sections.push(ops);

            if matches!(s.body, Body::Text(_)) {
                break;
            }
        }
        let mut fm = Fm {
            sections: Arc::new(sections),

            custom: custom || code == "@",
            ty: 0,
            has_date,
            has_time,
        };
        fm.ty = fm.val_type(0, true);
        fm
    }

    fn val_type(&self, hint: u8, mode1: bool) -> u8 {
        let mut pending = 0u8;
        let mut prev: Option<usize> = None;
        let mut fill = false;
        for (si, sec) in self.sections.iter().enumerate() {
            if sec.is_empty() {
                continue;
            }

            if prev.is_some_and(|p| p != si) {
                if fill && matches!(pending, 1 | 2) {
                    return if mode1 { 10 } else { u8::from(hint == 10) };
                }
                if mode1 {
                    if pending != 0 {
                        return pending;
                    }
                } else if pending == hint {
                    return 1;
                }
                fill = false;
            }
            prev = Some(si);
            for op in sec {
                let (t, decisive) = match op {
                    VOp::Date => (3, true),
                    VOp::Time => (4, true),
                    VOp::General => (8, true),
                    VOp::DollarFirst => (2, true),
                    VOp::CurrencyTag => (2, false),
                    VOp::Header(t) => (*t, false),
                    VOp::Sci => (7, false),
                    VOp::Frac => (6, false),
                    VOp::Fill => {
                        fill = true;
                        continue;
                    }
                };
                pending = t;
                if decisive && (mode1 || hint == t) {
                    return if mode1 { t } else { 1 };
                }
            }
        }
        if mode1 {
            if pending == 0 && self.custom {
                9
            } else {
                pending
            }
        } else {
            u8::from(pending == hint)
        }
    }

    fn can_be(&self, hint: u8) -> bool {
        self.val_type(hint, false) != 0
    }
}

pub(crate) fn join(stored: &mut Fm, new: &Fm) -> bool {
    let (ts, tn) = (stored.ty, new.ty);
    if ts == tn {
        return true;
    }
    if ts != 0 {
        return tn == 0 || new.can_be(ts) || stored.can_be(tn);
    }
    *stored = new.clone();
    true
}

pub(crate) fn compatible(a: &Fm, b: &Fm) -> bool {
    let mut s = a.clone();
    join(&mut s, b)
}

fn section_ops(body: &Body, raw: &str, negative: bool) -> (Vec<VOp>, bool, bool) {
    let mut ops = Vec::new();
    let (mut date, mut time) = (false, false);
    let tag = currency_tag(raw);
    match body {
        Body::Number(l) => {
            if l.n_int + l.n_frac > 0 {
                ops.push(VOp::Header(if l.scale == 2 { 5 } else { 1 }));
                if dollar_first(raw, negative) {
                    ops.push(VOp::DollarFirst);
                }
            }
            fills(&l.items, &mut ops);
            if tag {
                ops.push(VOp::CurrencyTag);
            }
        }
        Body::Sci(l) => {
            if l.n_int + l.n_frac > 0 {
                ops.push(VOp::Sci);
            }
            fills(&l.items, &mut ops);
            if tag {
                ops.push(VOp::CurrencyTag);
            }
        }
        Body::Frac(l) => {
            ops.push(VOp::Frac);
            fills(&l.items, &mut ops);
            if tag {
                ops.push(VOp::CurrencyTag);
            }
        }
        Body::General(_) => ops.push(VOp::General),

        Body::Date(_) if !has_field_letter(raw) => {
            if tag {
                ops.push(VOp::CurrencyTag);
            }
        }
        Body::Date(l) => {
            if tag {
                ops.push(VOp::CurrencyTag);
            }
            for it in &l.items {
                match it {
                    Item::Date(t) => match t {
                        DateTok::Year(_)
                        | DateTok::Month(_)
                        | DateTok::Day(_)
                        | DateTok::EYear(_)
                        | DateTok::Buddhist(_) => {
                            date = true;
                            ops.push(VOp::Date);
                        }
                        DateTok::Hour(_)
                        | DateTok::Hour12(_)
                        | DateTok::Minute(_)
                        | DateTok::Second(_)
                        | DateTok::SubSec(_)
                        | DateTok::ElapsedH(_)
                        | DateTok::ElapsedM(_)
                        | DateTok::ElapsedS(_)
                        | DateTok::LeftH(_)
                        | DateTok::LeftM(_)
                        | DateTok::LeftS(_) => {
                            time = true;
                            ops.push(VOp::Time);
                        }
                        DateTok::General(_) => ops.push(VOp::General),
                        DateTok::Era(_) | DateTok::AmPm | DateTok::AmPmCjk => {}
                    },
                    Item::Fill(_) => ops.push(VOp::Fill),
                    Item::General => ops.push(VOp::General),
                    _ => {}
                }
            }
        }
        Body::Literal(items) | Body::Text(items) => {
            fills(items, &mut ops);
        }
    }
    (ops, date, time)
}

fn fills(items: &[Item], ops: &mut Vec<VOp>) {
    for it in items {
        match it {
            Item::Fill(_) => ops.push(VOp::Fill),
            Item::General => ops.push(VOp::General),
            _ => {}
        }
    }
}

fn split_raw(code: &str) -> Vec<String> {
    let mut out = vec![String::new()];
    let mut it = code.chars();
    while let Some(c) = it.next() {
        match c {
            ';' => out.push(String::new()),
            '"' | '[' => {
                let end = if c == '"' { '"' } else { ']' };
                let cur = out.last_mut().unwrap();
                cur.push(c);
                for d in it.by_ref() {
                    cur.push(d);
                    if d == end {
                        break;
                    }
                }
            }
            '\\' | '_' | '*' => {
                let cur = out.last_mut().unwrap();
                cur.push(c);
                if let Some(d) = it.next() {
                    cur.push(d);
                }
            }
            c => out.last_mut().unwrap().push(c),
        }
    }
    out
}

fn has_field_letter(raw: &str) -> bool {
    let mut it = raw.chars();
    while let Some(c) = it.next() {
        match c {
            '"' => {
                for d in it.by_ref() {
                    if d == '"' {
                        break;
                    }
                }
            }
            '[' => {
                let mut inner = String::new();
                for d in it.by_ref() {
                    if d == ']' {
                        break;
                    }
                    inner.push(d);
                }
                let t = inner.to_ascii_lowercase();
                if !t.is_empty() && t.chars().all(|x| matches!(x, 'h' | 'm' | 's')) {
                    return true;
                }
            }
            '\\' | '_' | '*' => {
                it.next();
            }
            c if "dmyhsegbDMYHSEGB".contains(c) => return true,
            _ => {}
        }
    }
    false
}

fn currency_tag(raw: &str) -> bool {
    let mut rest = raw;
    while let Some(at) = rest.find("[$") {
        let inner = &rest[at + 2..];
        let end = inner.find(']').unwrap_or(inner.len());
        let text = inner[..end].split('-').next().unwrap_or("");
        if !text.is_empty() {
            return true;
        }
        rest = &inner[end..];
    }
    false
}

fn dollar_first(raw: &str, negative: bool) -> bool {
    let mut s = raw;
    while let Some(r) = s.strip_prefix('[') {
        let Some(end) = r.find(']') else { return false };
        if r.starts_with('$') {
            return false;
        }
        s = &r[end + 1..];
    }

    let s = if negative {
        s.strip_prefix('-').unwrap_or(s)
    } else {
        s
    };
    let s = s.strip_prefix('(').unwrap_or(s);
    s.starts_with('$') || s.starts_with("\"US$\"")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ty(code: &str) -> u8 {
        Fm::of(code, true).ty
    }

    #[test]
    fn types() {
        assert_eq!(ty("0.00"), 1);
        assert_eq!(ty("0%"), 5);
        assert_eq!(ty("0.0E+0"), 7);
        assert_eq!(ty("# ?/?"), 6);
        assert_eq!(ty("d/mm/yy"), 3);
        assert_eq!(ty("hh:mm"), 4);
        assert_eq!(ty("hh:mm d/mm"), 4);
        assert_eq!(ty("[$-409]General"), 8);
        assert_eq!(ty("$#,##0"), 2);
        assert_eq!(ty("#,##0$"), 1);
        assert_eq!(ty("#,##0;[Red]-$#,##0"), 1);
        assert!(Fm::of("$#,##0.00;[Red]-$#,##0.00", false).ty == 2);
        assert_eq!(ty("_($* #,##0_);_($* (#,##0);_($* \"-\"_);_(@_)"), 10);
        assert_eq!(ty("General"), 0);
        assert_eq!(ty("@"), 9);
        assert_eq!(Fm::of("@", false).ty, 9);
    }

    #[test]
    fn flags() {
        let f = Fm::of("0%;[Color3]mmm-yy", true);
        assert!(f.has_date && !f.has_time);
        let f = Fm::of("0.00$;hh:mm /p;@\" units\"", true);
        assert!(f.has_time && !f.has_date);
        let f = Fm::of("0;@;hh:mm", true);
        assert!(!f.has_time);
    }

    #[test]
    fn joining() {
        let general = Fm::general();
        let pct = Fm::of("0%", true);
        let num = Fm::of("0.00", true);
        let mut g = general.clone();
        assert!(join(&mut g, &pct));
        assert_eq!(g.ty, 5);
        assert!(!join(&mut g, &Fm::of("hh:mm", true)));
        assert!(join(&mut g, &general));
        let mut n = num.clone();
        assert!(!join(&mut n, &pct));
    }
}
