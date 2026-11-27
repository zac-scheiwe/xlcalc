use xlcalc::{ElapsedUnit, StrictDate};

const SS: &str = "http://schemas.openxmlformats.org/";
const PS: &str = "http://purl.oclc.org/ooxml/";

const NS: &[(&str, &str)] = &[
    ("spreadsheetml/2006/main", "spreadsheetml/main"),
    ("wordprocessingml/2006/main", "wordprocessingml/main"),
    ("presentationml/2006/main", "presentationml/main"),
    ("schemaLibrary/2006/main", "schemaLibrary/main"),
    (
        "officeDocument/2006/relationships",
        "officeDocument/relationships",
    ),
    (
        "officeDocument/2006/extended-properties",
        "officeDocument/extendedProperties",
    ),
    (
        "officeDocument/2006/custom-properties",
        "officeDocument/customProperties",
    ),
    (
        "officeDocument/2006/docPropsVTypes",
        "officeDocument/docPropsVTypes",
    ),
    ("officeDocument/2006/math", "officeDocument/math"),
    (
        "officeDocument/2006/bibliography",
        "officeDocument/bibliography",
    ),
    ("officeDocument/2006/customXml", "officeDocument/customXml"),
    ("officeDocument/2006/oleObject", "officeDocument/oleObject"),
    ("drawingml/2006/main", "drawingml/main"),
    ("drawingml/2006/chart", "drawingml/chart"),
    ("drawingml/2006/chartDrawing", "drawingml/chartDrawing"),
    ("drawingml/2006/diagram", "drawingml/diagram"),
    ("drawingml/2006/lockedCanvas", "drawingml/lockedCanvas"),
    ("drawingml/2006/picture", "drawingml/picture"),
    (
        "drawingml/2006/spreadsheetDrawing",
        "drawingml/spreadsheetDrawing",
    ),
    ("drawingml/2006/table", "drawingml/table"),
    (
        "drawingml/2006/wordprocessingDrawing",
        "drawingml/wordprocessingDrawing",
    ),
];

const KINDS: &[&str] = &[
    "aFChunk",
    "attachedTemplate",
    "audio",
    "calcChain",
    "chart",
    "chartsheet",
    "chartUserShapes",
    "commentAuthors",
    "comments",
    "connections",
    "control",
    "customProperty",
    "customXml",
    "customXmlProps",
    "diagramColors",
    "diagramData",
    "diagramLayout",
    "diagramQuickStyle",
    "dialogsheet",
    "drawing",
    "endnotes",
    "externalLink",
    "externalLinkPath",
    "font",
    "fontTable",
    "footer",
    "footnotes",
    "frame",
    "glossaryDocument",
    "handoutMaster",
    "header",
    "htmlPubSaveAs",
    "hyperlink",
    "image",
    "mailMergeHeaderSource",
    "mailMergeRecipientData",
    "mailMergeSource",
    "notesMaster",
    "notesSlide",
    "numbering",
    "officeDocument",
    "oleObject",
    "package",
    "pivotCacheDefinition",
    "pivotCacheRecords",
    "pivotTable",
    "presProps",
    "printerSettings",
    "queryTable",
    "revisionHeaders",
    "revisionLog",
    "richPivotRecords",
    "settings",
    "sharedStrings",
    "sheetMetadata",
    "slide",
    "slideLayout",
    "slideMaster",
    "slideUpdateInfo",
    "slideUpdateUrl",
    "styles",
    "subDocument",
    "table",
    "tableSingleCells",
    "tableStyles",
    "tags",
    "theme",
    "themeOverride",
    "transform",
    "usernames",
    "video",
    "viewProps",
    "volatileDependencies",
    "webSettings",
    "worksheet",
    "xmlMaps",
];

const RENAMED_KINDS: &[(&str, &str)] = &[
    ("extended-properties", "extendedProperties"),
    ("custom-properties", "customProperties"),
];

const REL_T: &str = "officeDocument/2006/relationships/";
const REL_S: &str = "officeDocument/relationships/";
const THUMB_T: &str =
    "http://schemas.openxmlformats.org/package/2006/relationships/metadata/thumbnail";
const THUMB_S: &str = "http://purl.oclc.org/ooxml/officeDocument/relationships/metadata/thumbnail";

fn ns_to_strict(u: &str) -> Option<String> {
    let rest = u.strip_prefix(SS)?;
    NS.iter()
        .find(|(t, _)| *t == rest)
        .map(|(_, s)| format!("{PS}{s}"))
}

fn ns_to_transitional(u: &str) -> Option<String> {
    let rest = u.strip_prefix(PS)?;
    NS.iter()
        .find(|(_, s)| *s == rest)
        .map(|(t, _)| format!("{SS}{t}"))
}

fn rel_to_strict(u: &str) -> Option<String> {
    if u == THUMB_T {
        return Some(THUMB_S.into());
    }
    let kind = u.strip_prefix(SS)?.strip_prefix(REL_T)?;
    if let Some((_, s)) = RENAMED_KINDS.iter().find(|(t, _)| *t == kind) {
        return Some(format!("{PS}{REL_S}{s}"));
    }
    KINDS.contains(&kind).then(|| format!("{PS}{REL_S}{kind}"))
}

fn rel_to_transitional(u: &str) -> Option<String> {
    if u == THUMB_S {
        return Some(THUMB_T.into());
    }
    let kind = u.strip_prefix(PS)?.strip_prefix(REL_S)?;
    if let Some((t, _)) = RENAMED_KINDS.iter().find(|(_, s)| *s == kind) {
        return Some(format!("{SS}{REL_T}{t}"));
    }
    KINDS.contains(&kind).then(|| format!("{SS}{REL_T}{kind}"))
}

fn rewrite_tags(
    xml: &str,
    attr: &dyn Fn(&str, &str) -> Option<String>,
    elem: &dyn Fn(&str) -> Option<&'static str>,
) -> String {
    let b = xml.as_bytes();
    let mut out = String::with_capacity(xml.len() + 64);
    let mut i = 0;
    while i < b.len() {
        let Some(lt) = xml[i..].find('<') else {
            out.push_str(&xml[i..]);
            break;
        };
        out.push_str(&xml[i..i + lt]);
        i += lt;
        let rest = &xml[i..];

        let skip_to = if rest.starts_with("<!--") {
            Some("-->")
        } else if rest.starts_with("<![CDATA[") {
            Some("]]>")
        } else if rest.starts_with("<?") {
            Some("?>")
        } else {
            None
        };
        if let Some(end) = skip_to {
            let n = rest.find(end).map_or(rest.len(), |p| p + end.len());
            out.push_str(&rest[..n]);
            i += n;
            continue;
        }

        let mut q = 0u8;
        let mut end = rest.len();
        for (k, &c) in rest.as_bytes().iter().enumerate().skip(1) {
            match (q, c) {
                (0, b'"') | (0, b'\'') => q = c,
                (x, y) if x != 0 && x == y => q = 0,
                (0, b'>') => {
                    end = k + 1;
                    break;
                }
                _ => {}
            }
        }
        out.push_str(&rewrite_tag(&rest[..end], attr, elem));
        i += end;
    }
    out
}

fn rewrite_tag(
    tag: &str,
    attr: &dyn Fn(&str, &str) -> Option<String>,
    elem: &dyn Fn(&str) -> Option<&'static str>,
) -> String {
    let b = tag.as_bytes();
    let mut out = String::with_capacity(tag.len() + 32);

    let mut i = 1;
    if b.get(i) == Some(&b'/') {
        i += 1;
    }
    let name_end = tag[i..]
        .find(|c: char| c.is_whitespace() || c == '/' || c == '>')
        .map_or(tag.len(), |p| i + p);
    let name = &tag[i..name_end];
    out.push_str(&tag[..i]);
    out.push_str(elem(name).unwrap_or(name));
    let mut j = name_end;
    while j < tag.len() {
        let Some(eq) = tag[j..].find('=') else {
            out.push_str(&tag[j..]);
            return out;
        };
        let eq = j + eq;
        let after = &tag[eq + 1..];
        let Some(qc) = after.chars().next().filter(|c| *c == '"' || *c == '\'') else {
            out.push_str(&tag[j..=eq]);
            j = eq + 1;
            continue;
        };
        let vstart = eq + 2;
        let Some(vlen) = tag[vstart..].find(qc) else {
            out.push_str(&tag[j..]);
            return out;
        };
        let vend = vstart + vlen;
        let before = &tag[j..eq];
        let aname = before.trim();
        let value = &tag[vstart..vend];
        out.push_str(&tag[j..vstart]);
        match attr(aname, value) {
            Some(v) => out.push_str(&v),
            None => out.push_str(value),
        }
        out.push(qc);
        j = vend + 1;
    }
    out
}

fn is_styles(name: &str, ty: Option<&str>) -> bool {
    name.eq_ignore_ascii_case("xl/styles.xml")
        || ty.is_some_and(|t| t.ends_with("spreadsheetml.styles+xml"))
}

fn is_sheet(ty: Option<&str>) -> bool {
    ty.is_some_and(|t| t.ends_with("spreadsheetml.worksheet+xml"))
}

const MC: &str = "http://schemas.openxmlformats.org/markup-compatibility/2006";
const VML: &str = "urn:schemas-microsoft-com:vml";
const CHOICE: &str = "<mc:Choice Requires=\"v\"><legacyDrawing ";

fn wrap_legacy_drawing(xml: &str) -> String {
    let Some(at) = xml.find("<legacyDrawing ") else {
        return xml.to_string();
    };
    let Some(len) = xml[at..].find("/>") else {
        return xml.to_string();
    };
    let tag = &xml[at..at + len + 2];
    let mut s = String::with_capacity(xml.len() + 160);
    s.push_str(&xml[..at]);
    s.push_str(&format!(
        "<mc:AlternateContent xmlns:mc=\"{MC}\"><mc:Choice Requires=\"v\">{tag}</mc:Choice></mc:AlternateContent>"
    ));
    s.push_str(&xml[at + tag.len()..]);

    if !s.contains("xmlns:v=") {
        if let Some(p) = s.find("<worksheet ") {
            s.insert_str(p + "<worksheet".len(), &format!(" xmlns:v=\"{VML}\""));
        }
    }
    s
}

fn unwrap_legacy_drawing(xml: &str) -> String {
    let Some(at) = xml.find(CHOICE) else {
        return xml.to_string();
    };
    let Some(start) = xml[..at].rfind("<mc:AlternateContent") else {
        return xml.to_string();
    };
    let Some(close) = xml[at..].find("</mc:AlternateContent>") else {
        return xml.to_string();
    };
    let inner = &xml[at + "<mc:Choice Requires=\"v\">".len()..at + close];
    let Some(inner) = inner.strip_suffix("</mc:Choice>") else {
        return xml.to_string();
    };
    format!(
        "{}{}{}",
        &xml[..start],
        inner,
        &xml[at + close + "</mc:AlternateContent>".len()..]
    )
}

fn border_to_strict(n: &str) -> Option<&'static str> {
    match n {
        "left" => Some("start"),
        "right" => Some("end"),
        _ => None,
    }
}

fn border_to_transitional(n: &str) -> Option<&'static str> {
    match n {
        "start" => Some("left"),
        "end" => Some("right"),
        _ => None,
    }
}

fn convert(name: &str, ty: Option<&str>, bytes: &[u8], to_strict: bool) -> Option<Vec<u8>> {
    let lower = name.to_ascii_lowercase();
    let rels = lower.ends_with(".rels");
    if !rels && !lower.ends_with(".xml") {
        return None;
    }
    let xml = std::str::from_utf8(bytes).ok()?;
    let attr = |an: &str, v: &str| -> Option<String> {
        if an == "xmlns" || an.starts_with("xmlns:") {
            return if to_strict {
                ns_to_strict(v)
            } else {
                ns_to_transitional(v)
            };
        }
        if rels && an == "Type" {
            return if to_strict {
                rel_to_strict(v)
            } else {
                rel_to_transitional(v)
            };
        }
        None
    };
    let styles = is_styles(name, ty);
    let elem: &dyn Fn(&str) -> Option<&'static str> = match (styles, to_strict) {
        (true, true) => &border_to_strict,
        (true, false) => &border_to_transitional,
        _ => &|_| None,
    };
    let mut s = rewrite_tags(xml, &attr, elem);
    if is_sheet(ty) {
        s = if to_strict {
            wrap_legacy_drawing(&s)
        } else {
            unwrap_legacy_drawing(&s)
        };
    }
    Some(s.into_bytes())
}

pub(crate) fn to_strict(name: &str, ty: Option<&str>, bytes: &[u8]) -> Vec<u8> {
    convert(name, ty, bytes, true).unwrap_or_else(|| bytes.to_vec())
}

pub(crate) fn to_transitional(name: &str, ty: Option<&str>, bytes: &[u8]) -> Vec<u8> {
    convert(name, ty, bytes, false).unwrap_or_else(|| bytes.to_vec())
}

pub(crate) fn is_strict_root_rels(bytes: &[u8]) -> bool {
    std::str::from_utf8(bytes).is_ok_and(|s| {
        s.contains("\"http://purl.oclc.org/ooxml/officeDocument/relationships/officeDocument\"")
    })
}

fn civil(days: i64) -> (i64, u32, u32) {
    let z = days + 719_468 - 25_569;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let m = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    let y = yoe + era * 400 + i64::from(m <= 2);
    (y, m, d)
}

fn digits17(g: f64) -> Option<(u128, i32)> {
    if g == 0.0 {
        return None;
    }
    let s = format!("{g:.16e}");
    let (mant, exp) = s.split_once('e')?;
    let exp: i32 = exp.parse().ok()?;
    let mut d: u128 = mant.replace('.', "").parse().ok()?;
    let mut e = exp - 16;
    while d.is_multiple_of(10) {
        d /= 10;
        e += 1;
    }
    Some((d, e))
}

fn frac_seconds(x: f64) -> String {
    let y = x * 128.0;
    let g = y - y.trunc();
    let Some((d, e)) = digits17(g) else {
        return "000".into();
    };
    let p = (d * 675).to_string();
    let scale = (-e) as usize;
    let frac = if p.len() >= scale {
        p[p.len() - scale..].to_string()
    } else {
        format!("{}{p}", "0".repeat(scale - p.len()))
    };
    if frac.is_empty() {
        "000".into()
    } else {
        frac
    }
}

fn day_seconds(f: f64) -> u32 {
    if f == 0.0 {
        return 0;
    }
    let bits = f.to_bits();
    let exp = ((bits >> 52) & 0x7ff) as i32;
    let m = if exp == 0 {
        bits & ((1 << 52) - 1)
    } else {
        (bits & ((1 << 52) - 1)) | (1 << 52)
    };
    let e = if exp == 0 { -1074 } else { exp - 1075 };

    let shift = (-e) as u32;
    if shift >= 128 {
        return 0;
    }
    ((u128::from(m) * 86_400) >> shift) as u32
}

pub(crate) fn iso_cell(x: f64, fmt: StrictDate, d1904: bool) -> Option<String> {
    if x < 0.0 || x.is_nan() {
        return None;
    }
    let fix = x.trunc();
    let limit = 2_958_465.0 - if d1904 { 1462.0 } else { 0.0 };
    if fix > limit {
        return None;
    }
    if let StrictDate::Elapsed(unit) = fmt {
        return duration(x, unit, d1904);
    }
    let int = x == fix;

    let date = int || fix >= 1.0;
    let time = !int;
    let mut s = String::new();
    if date {
        let days = fix as i64 + if d1904 { 1462 } else { 0 };
        let (y, m, d) = civil(days);
        s.push_str(&format!("{y:04}-{m:02}-{d:02}"));
    }
    if time {
        let secs = day_seconds(x - fix);
        if date {
            s.push('T');
        }
        s.push_str(&format!(
            "{:02}:{:02}:{:02}.{}",
            secs / 3600,
            secs / 60 % 60,
            secs % 60,
            frac_seconds(x)
        ));
    }
    Some(s)
}

fn duration(x: f64, unit: ElapsedUnit, d1904: bool) -> Option<String> {
    const HALF_MS: f64 = 5.787037037037248e-9;
    let (hours, minutes) = match unit {
        ElapsedUnit::Hours => (true, true),
        ElapsedUnit::Minutes => (false, true),
        ElapsedUnit::Seconds => (false, false),
    };
    let mut v = x + HALF_MS;
    if d1904 {
        if v.trunc() > 2_957_003.0 {
            return None;
        }
        v += 1462.0;
    } else if v.trunc() > 2_958_465.0 {
        return None;
    }
    let mut s = String::from("PT");
    v *= 24.0;
    if hours {
        let h = v.trunc();
        v -= h;
        s.push_str(&format!("{h}H"));
    }
    v *= 60.0;
    if minutes && (!hours || v != 0.0) {
        let m = v.trunc();
        v -= m;
        s.push_str(&format!("{m}M"));
    }
    v *= 60.0;
    if minutes && v == 0.0 {
        return Some(s);
    }
    let sec = v.trunc();
    v -= sec;
    s.push_str(&sec.to_string());
    if v != 0.0 {
        s.push('.');
        for _ in 0..3 {
            v *= 10.0;
            let d = v.trunc();
            v -= d;
            s.push_str(&d.to_string());
        }
    }
    s.push('S');
    Some(s)
}

fn div_to_f64(num: u128, den: u128) -> f64 {
    if num == 0 {
        return 0.0;
    }

    let mut n = num;
    let mut shift = 0i32;
    while n / den < (1u128 << 64) && n < (1u128 << 126) {
        n <<= 1;
        shift += 1;
    }
    let q = n / den;
    let sticky = !n.is_multiple_of(den);
    let drop = (128 - q.leading_zeros()).saturating_sub(53);
    let mut top = q >> drop;
    if drop > 0 {
        let low = q & ((1u128 << drop) - 1);
        let half = 1u128 << (drop - 1);
        if low > half || (low == half && (sticky || top & 1 == 1)) {
            top += 1;
        }
    }
    (top as f64) * 2f64.powi(drop as i32 - shift)
}

fn day_fraction(secs: u128, frac: u128, k: u32) -> f64 {
    let num = secs * 10u128.pow(k) + frac;
    div_to_f64(num, 675 * 5u128.pow(k)) * 2f64.powi(-(k as i32 + 7))
}

pub(crate) fn from_iso(t: &str, d1904: bool, compat: bool) -> Option<f64> {
    if let Some(d) = t.strip_prefix("PT") {
        return from_duration(d, d1904);
    }
    let (d, tm) = match t.split_once('T') {
        Some((d, tm)) => (Some(d), Some(tm)),
        None if t.contains(':') => (None, Some(t)),
        None => (Some(t), None),
    };
    let mut days: i64 = 0;
    if let Some(d) = d {
        let neg = d.starts_with('-');
        let mut it = d.trim_start_matches('-').split('-');
        let y: i64 = it.next()?.parse().ok()?;
        let m: i64 = it.next()?.parse().ok()?;
        let dd: i64 = it.next()?.parse().ok()?;
        let y = if neg { -y } else { y };

        let (yy, mm) = if m <= 2 { (y - 1, m + 9) } else { (y, m - 3) };
        let era = yy.div_euclid(400);
        let yoe = yy - era * 400;
        let doy = (153 * mm + 2) / 5 + dd - 1;
        let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
        let unix = era * 146_097 + doe - 719_468;
        days = unix + 25_569;
        if d1904 {
            days -= 1462;
        } else if compat && (1..61).contains(&days) {
            days -= 1;
        }
    }
    let Some(tm) = tm else {
        return Some(days as f64);
    };
    let tm = tm.trim_end_matches('Z');
    let mut it = tm.split(':');
    let h: u128 = it.next()?.parse().ok()?;
    let mi: u128 = it.next().map_or(Some(0), |x| x.parse().ok())?;
    let se = it.next().unwrap_or("0");
    let (si, fr) = se.split_once('.').unwrap_or((se, ""));
    let si: u128 = si.parse().ok()?;
    let fr = &fr[..fr.len().min(19)];
    let k = fr.len() as u32;
    let frac: u128 = if fr.is_empty() { 0 } else { fr.parse().ok()? };

    Some(days as f64 + day_fraction(h * 3600 + mi * 60 + si, frac, k))
}

fn from_duration(t: &str, d1904: bool) -> Option<f64> {
    let mut secs: u128 = 0;
    let mut frac: u128 = 0;
    let mut k: u32 = 0;
    let mut num = String::new();
    for c in t.chars() {
        match c {
            '0'..='9' | '.' => num.push(c),
            'H' | 'M' | 'S' => {
                let (w, f) = num.split_once('.').unwrap_or((&num, ""));
                let w: u128 = w.parse().ok()?;
                let mult = match c {
                    'H' => 3600,
                    'M' => 60,
                    _ => 1,
                };
                secs += w * mult;
                if !f.is_empty() {
                    let f = &f[..f.len().min(19)];
                    k = f.len() as u32;
                    frac = f.parse().ok()?;

                    if c != 'S' {
                        return None;
                    }
                }
                num.clear();
            }
            _ => return None,
        }
    }
    let v = day_fraction(secs, frac, k);
    Some(if d1904 { v - 1462.0 } else { v })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dates_as_excel_wrote_them() {
        let p = StrictDate::Plain;
        let h = StrictDate::Elapsed(ElapsedUnit::Hours);
        let m = StrictDate::Elapsed(ElapsedUnit::Minutes);
        let s = StrictDate::Elapsed(ElapsedUnit::Seconds);
        let cases: &[(f64, StrictDate, Option<&str>)] = &[
            (0.0, p, Some("1899-12-30")),
            (45000.5, p, Some("2023-03-15T12:00:00.000")),
            (0.5, p, Some("12:00:00.000")),
            (1.5, h, Some("PT36H0M0.000S")),
            (0.123456789, p, Some("02:57:46.66656959999977050")),
            (
                45000.00001,
                p,
                Some("2023-03-15T00:00:00.8640002924948930625"),
            ),
            (2958466.0, p, None),
            (2958465.0, p, Some("9999-12-31")),
            (90.0, m, Some("PT129600M0.000S")),
            (1.5, s, Some("PT129600.000S")),
            (0.3333333333333333, p, Some("07:59:59.9999999999984025")),
            (0.00001157, p, Some("00:00:00.9996479999999999325")),
            (-1.0, p, None),
            (1.25, p, Some("1899-12-31T06:00:00.000")),
            (61.0, p, Some("1900-03-01")),
            (0.75, h, Some("PT18H0M0.000S")),
            (1.0, h, Some("PT24H0M0.000S")),
        ];
        for (x, f, want) in cases {
            assert_eq!(iso_cell(*x, *f, false).as_deref(), *want, "{x}");
        }
    }

    #[test]
    fn iso_round_trips() {
        for x in [
            0.0,
            1.0,
            59.0,
            60.0,
            61.0,
            45000.5,
            0.123456789,
            45000.00001,
            0.5,
        ] {
            let t = iso_cell(x, StrictDate::Plain, false).unwrap();
            let back = from_iso(&t, false, false).unwrap();
            assert_eq!(back.to_bits(), x.to_bits(), "{x} {t}");
        }
        let t = iso_cell(1.5, StrictDate::Elapsed(ElapsedUnit::Hours), false).unwrap();
        assert_eq!(from_iso(&t, false, false), Some(1.5));
    }

    #[test]
    fn names_swap_both_ways() {
        let t = "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"w.xml\"/><Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/vmlDrawing\" Target=\"v.vml\"/><Relationship Id=\"rId3\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties\" Target=\"a.xml\"/></Relationships>";
        let s = to_strict("xl/_rels/workbook.xml.rels", None, t.as_bytes());
        let s = String::from_utf8(s).unwrap();
        assert!(s.contains(
            "Type=\"http://purl.oclc.org/ooxml/officeDocument/relationships/worksheet\""
        ));
        assert!(s.contains("Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/vmlDrawing\""));
        assert!(s.contains("ooxml/officeDocument/relationships/extendedProperties"));
        assert!(
            s.contains("xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"")
        );
        let back = to_transitional("xl/_rels/workbook.xml.rels", None, s.as_bytes());
        assert_eq!(String::from_utf8(back).unwrap(), t);
    }

    #[test]
    fn borders_and_legacy_drawing() {
        let st = "<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><borders count=\"1\"><border><left style=\"thin\"><color indexed=\"64\"/></left><right/><top/></border></borders></styleSheet>";
        let s = String::from_utf8(to_strict("xl/styles.xml", None, st.as_bytes())).unwrap();
        assert!(s.contains("<border><start style=\"thin\"><color indexed=\"64\"/></start><end/><top/></border>"), "{s}");
        assert!(s.contains("xmlns=\"http://purl.oclc.org/ooxml/spreadsheetml/main\""));
        assert_eq!(
            String::from_utf8(to_transitional("xl/styles.xml", None, s.as_bytes())).unwrap(),
            st
        );
        let ws = "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><sheetData/><legacyDrawing r:id=\"rId2\"/></worksheet>";
        let ty = Some("application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml");
        let s =
            String::from_utf8(to_strict("xl/worksheets/sheet1.xml", ty, ws.as_bytes())).unwrap();
        assert!(
            s.contains("<mc:Choice Requires=\"v\"><legacyDrawing r:id=\"rId2\"/></mc:Choice>"),
            "{s}"
        );
        assert!(s.contains("xmlns:v=\"urn:schemas-microsoft-com:vml\""));
        let back = String::from_utf8(to_transitional(
            "xl/worksheets/sheet1.xml",
            ty,
            s.as_bytes(),
        ))
        .unwrap();
        assert!(
            back.contains("<sheetData/><legacyDrawing r:id=\"rId2\"/></worksheet>"),
            "{back}"
        );
    }
}
