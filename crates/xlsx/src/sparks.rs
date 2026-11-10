use crate::rawxml::{escape_attr, escape_text};
use crate::xml::Node;
use xlcalc::sparkline::{SparkGroup, Sparkline};
use xlcalc::{col_to_letters, parse_a1_rect, Rect, SheetId, Workbook};

pub(crate) const URI: &str = "{05C60535-1F16-4fd2-B633-F4F36F0B64E0}";

const POINT_ATTR: [&str; 6] = ["negative", "markers", "high", "low", "first", "last"];

const POINT_COLOR: [&str; 6] = [
    "colorNegative",
    "colorMarkers",
    "colorHigh",
    "colorLow",
    "colorFirst",
    "colorLast",
];

pub(crate) fn find(sheet: &Node) -> Option<Node> {
    sheet
        .child("extLst")?
        .children("ext")
        .find(|e| e.attr("uri").is_some_and(|u| u.eq_ignore_ascii_case(URI)))?
        .child("sparklineGroups")
        .cloned()
}

fn flag(n: &Node, k: &str) -> bool {
    matches!(n.attr(k), Some("1" | "true"))
}

fn bgr(n: &Node, k: &str) -> Option<i32> {
    let v = n.child(k)?.attr("rgb")?;
    let v = u32::from_str_radix(v, 16).ok()?;
    let (r, g, b) = ((v >> 16) & 255, (v >> 8) & 255, v & 255);
    Some((b << 16 | g << 8 | r) as i32)
}

fn cell_rect(t: &str) -> Option<Rect> {
    let t = t.replace('$', "").to_uppercase();
    if t.contains(':') {
        parse_a1_rect(&t)
    } else {
        parse_a1_rect(&format!("{t}:{t}"))
    }
}

pub(crate) fn read(wb: &mut Workbook, sheet: SheetId, node: &Node) {
    for gn in node.children("sparklineGroup") {
        let kind = match gn.attr("type") {
            Some("column") => 2,
            Some("stacked") => 3,
            _ => 1,
        };
        let id = wb.next_spark_id();
        let mut g = SparkGroup::new(id, kind, false);
        if let Some(w) = gn.attr("lineWeight").and_then(|v| v.parse().ok()) {
            g.line_weight = w;
        }
        g.blanks = match gn.attr("displayEmptyCellsAs") {
            Some("gap") => 1,
            Some("span") => 3,
            _ => 2,
        };
        g.hidden = flag(gn, "displayHidden");
        g.right_to_left = flag(gn, "rightToLeft");
        g.axis_visible = flag(gn, "displayXAxis");
        let scale = |k: &str| match gn.attr(k) {
            Some("group") => 1,
            Some("custom") => 3,
            _ => 2,
        };
        g.min_type = scale("minAxisType");
        g.max_type = scale("maxAxisType");
        g.min_value = gn
            .attr("manualMin")
            .and_then(|v| v.parse().ok())
            .unwrap_or(0.0);
        g.max_value = gn
            .attr("manualMax")
            .and_then(|v| v.parse().ok())
            .unwrap_or(0.0);
        if let Some(c) = bgr(gn, "colorSeries") {
            g.series_color = c;
        }
        if let Some(c) = bgr(gn, "colorAxis") {
            g.axis_color = c;
        }
        for (k, pt) in g.points.iter_mut().enumerate() {
            pt.visible = flag(gn, POINT_ATTR[k]);
            if let Some(c) = bgr(gn, POINT_COLOR[k]) {
                pt.color = c;
            }
        }
        if let Some(sl) = gn.child("sparklines") {
            for s in sl.children("sparkline") {
                let (Some(f), Some(q)) = (s.child("f"), s.child("sqref")) else {
                    continue;
                };
                let f = f.text.trim();
                let (name, cells) = match f.rsplit_once('!') {
                    Some((n, c)) => (Some(n), c),
                    None => (None, f),
                };
                let src_sheet = match name {
                    None => sheet,
                    Some(n) => {
                        let n = n
                            .strip_prefix('\'')
                            .and_then(|x| x.strip_suffix('\''))
                            .map_or_else(|| n.to_string(), |x| x.replace("''", "'"));
                        match wb.sheet_by_name(&n) {
                            Some(s) => s.id,
                            None => continue,
                        }
                    }
                };
                if cells.contains('$') {
                    g.abs = true;
                }
                let (Some(src), Some(at)) = (cell_rect(cells), cell_rect(q.text.trim())) else {
                    continue;
                };
                g.lines.push(Sparkline {
                    row: at.r0,
                    col: at.c0,
                    src_sheet,
                    src,
                });
            }
        }
        if !g.lines.is_empty() {
            wb.spark_groups_mut(sheet).push(g);
        }
    }
}

fn argb(c: i32) -> String {
    let c = c as u32;
    format!(
        "FF{:02X}{:02X}{:02X}",
        c & 255,
        (c >> 8) & 255,
        (c >> 16) & 255
    )
}

fn quote(name: &str) -> String {
    let plain = !name.is_empty()
        && name.chars().all(|c| c.is_alphanumeric() || c == '_')
        && !name.chars().next().is_some_and(|c| c.is_ascii_digit());
    if plain {
        name.to_string()
    } else {
        format!("'{}'", name.replace('\'', "''"))
    }
}

fn a1(r: &Rect, abs: bool) -> String {
    let cell = |row: u32, col: u32| {
        let c = col_to_letters(col);
        if abs {
            format!("${c}${}", row + 1)
        } else {
            format!("{c}{}", row + 1)
        }
    };
    if r.r0 == r.r1 && r.c0 == r.c1 {
        cell(r.r0, r.c0)
    } else {
        format!("{}:{}", cell(r.r0, r.c0), cell(r.r1, r.c1))
    }
}

pub(crate) fn write(wb: &Workbook, sheet: SheetId) -> Option<String> {
    let groups = wb.spark_groups(sheet);
    if groups.is_empty() {
        return None;
    }
    let mut o = format!(
        "<ext uri=\"{URI}\" xmlns:x14=\"http://schemas.microsoft.com/office/spreadsheetml/2009/9/main\"><x14:sparklineGroups xmlns:xm=\"http://schemas.microsoft.com/office/excel/2006/main\">"
    );
    for g in groups {
        o.push_str("<x14:sparklineGroup");
        match g.kind {
            2 => o.push_str(" type=\"column\""),
            3 => o.push_str(" type=\"stacked\""),
            _ => {}
        }
        o.push_str(&format!(" lineWeight=\"{}\"", g.line_weight));
        match g.blanks {
            1 => o.push_str(" displayEmptyCellsAs=\"gap\""),
            3 => o.push_str(" displayEmptyCellsAs=\"span\""),
            _ => {}
        }
        for (k, on) in [
            ("displayHidden", g.hidden),
            ("rightToLeft", g.right_to_left),
            ("displayXAxis", g.axis_visible),
        ] {
            if on {
                o.push_str(&format!(" {k}=\"1\""));
            }
        }
        for (k, t, v) in [
            ("minAxisType", g.min_type, g.min_value),
            ("maxAxisType", g.max_type, g.max_value),
        ] {
            match t {
                1 => o.push_str(&format!(" {k}=\"group\"")),
                3 => {
                    let m = if k == "minAxisType" {
                        "manualMin"
                    } else {
                        "manualMax"
                    };
                    o.push_str(&format!(" {k}=\"custom\" {m}=\"{v}\""));
                }
                _ => {}
            }
        }
        for (k, pt) in g.points.iter().enumerate() {
            if pt.visible {
                o.push_str(&format!(" {}=\"1\"", POINT_ATTR[k]));
            }
        }
        o.push('>');
        o.push_str(&format!(
            "<x14:colorSeries rgb=\"{}\"/>",
            argb(g.series_color)
        ));

        for k in [0, usize::MAX, 1, 4, 5, 2, 3] {
            if k == usize::MAX {
                o.push_str(&format!("<x14:colorAxis rgb=\"{}\"/>", argb(g.axis_color)));
            } else {
                o.push_str(&format!(
                    "<x14:{} rgb=\"{}\"/>",
                    POINT_COLOR[k],
                    argb(g.points[k].color)
                ));
            }
        }
        o.push_str("<x14:sparklines>");
        for l in &g.lines {
            let prefix = if l.src_sheet == sheet {
                wb.sheet(sheet)
            } else {
                wb.sheet(l.src_sheet)
            }
            .map(|s| format!("{}!", quote(&s.name)))
            .unwrap_or_default();
            o.push_str(&format!(
                "<x14:sparkline><xm:f>{}</xm:f><xm:sqref>{}</xm:sqref></x14:sparkline>",
                escape_text(&format!("{prefix}{}", a1(&l.src, g.abs))),
                escape_attr(&a1(&Rect::cell(l.row, l.col), false))
            ));
        }
        o.push_str("</x14:sparklines></x14:sparklineGroup>");
    }
    o.push_str("</x14:sparklineGroups></ext>");
    Some(o)
}

pub(crate) fn merge_uris(old: Option<&str>, drop: &[&str], add: Vec<String>) -> Option<String> {
    let mut inner = String::new();
    let mut head = "<extLst>".to_string();
    let mut tail = "</extLst>".to_string();
    if let Some(old) = old {
        if let Some(open_end) = old.find('>') {
            head = old[..=open_end].to_string();
            let close = old.rfind("</").unwrap_or(old.len());
            tail = old[close..].to_string();
            inner = old[open_end + 1..close].to_string();
        }

        for uri in drop {
            while let Some(at) = inner.find(&uri.to_lowercase()).or_else(|| inner.find(uri)) {
                let start = inner[..at].rfind("<ext").unwrap_or(0);
                let end = inner[at..]
                    .find("</ext>")
                    .map_or(inner.len(), |e| at + e + "</ext>".len());
                inner.replace_range(start..end, "");
            }
        }
    }
    for e in add {
        inner.push_str(&e);
    }
    (!inner.trim().is_empty()).then(|| format!("{head}{inner}{tail}"))
}
