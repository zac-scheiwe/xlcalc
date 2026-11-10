use crate::drawings::{
    cell_cache, drawing_part_of, read_drawing, type_prst, NS_A, NS_C, NS_XDR, REL,
};
use crate::package::{dir_of, relative, Package, Rel};
use crate::rawxml::{self, escape_attr, escape_text, Tag};
use std::collections::BTreeSet;
use xlcalc::drawing::{Anchor, Chart, ChartAxis, Part, Pos, Shape, ShapeKind};
use xlcalc::{Rect, SheetId, Workbook};

fn abs_cell(r: u32, c: u32) -> String {
    format!("${}${}", xlcalc::col_to_letters(c), r + 1)
}

fn quote_sheet(name: &str) -> String {
    let plain = !name.is_empty()
        && name.chars().all(|c| c.is_alphanumeric() || c == '_')
        && !name.chars().next().is_some_and(|c| c.is_ascii_digit());
    if plain {
        name.to_string()
    } else {
        format!("'{}'", name.replace('\'', "''"))
    }
}

fn areas_text(wb: &Workbook, areas: &[(SheetId, Rect)]) -> String {
    let one = |(s, r): &(SheetId, Rect)| {
        let name = wb.sheet(*s).map_or(String::new(), |x| x.name.clone());
        let cells = if r.r0 == r.r1 && r.c0 == r.c1 {
            abs_cell(r.r0, r.c0)
        } else {
            format!("{}:{}", abs_cell(r.r0, r.c0), abs_cell(r.r1, r.c1))
        };
        format!("{}!{cells}", quote_sheet(&name))
    };
    let v: Vec<String> = areas.iter().map(one).collect();
    if v.len() == 1 {
        v[0].clone()
    } else {
        format!("({})", v.join(","))
    }
}

fn cache(wb: &Workbook, areas: &[(SheetId, Rect)], numeric: bool) -> String {
    let mut pts = Vec::new();
    for (s, r) in areas {
        for row in r.r0..=r.r1 {
            for col in r.c0..=r.c1 {
                pts.push(cell_cache(wb, *s, row, col));
            }
        }
    }
    let tag = if numeric { "numCache" } else { "strCache" };
    let mut out = format!("<c:{tag}>");
    if numeric {
        out.push_str("<c:formatCode>General</c:formatCode>");
    }
    out.push_str(&format!("<c:ptCount val=\"{}\"/>", pts.len()));
    for (i, p) in pts.iter().enumerate() {
        if let Some((_, t)) = p {
            out.push_str(&format!(
                "<c:pt idx=\"{i}\"><c:v>{}</c:v></c:pt>",
                escape_text(t)
            ));
        }
    }
    out.push_str(&format!("</c:{tag}>"));
    out
}

fn all_numbers(wb: &Workbook, areas: &[(SheetId, Rect)]) -> bool {
    let mut any = false;
    for (s, r) in areas {
        for row in r.r0..=r.r1 {
            for col in r.c0..=r.c1 {
                match cell_cache(wb, *s, row, col) {
                    Some((false, _)) => return false,
                    Some(_) => any = true,
                    None => {}
                }
            }
        }
    }
    any
}

fn part_xml(wb: &Workbook, tag: &str, p: &Part, force_num: bool) -> String {
    match p {
        Part::None | Part::Dead => String::new(),
        Part::External(f) => {
            let r = if tag == "tx" || !force_num {
                "strRef"
            } else {
                "numRef"
            };
            format!(
                "<c:{tag}><c:{r}><c:f>{}</c:f></c:{r}></c:{tag}>",
                escape_text(f)
            )
        }
        Part::Refs(a) => {
            let num = force_num || (tag != "tx" && all_numbers(wb, a));
            let r = if num { "numRef" } else { "strRef" };
            format!(
                "<c:{tag}><c:{r}><c:f>{}</c:f>{}</c:{r}></c:{tag}>",
                escape_text(&areas_text(wb, a)),
                cache(wb, a, num)
            )
        }
        Part::Text(t) => {
            if let Some(inner) = t.strip_prefix('{').and_then(|x| x.strip_suffix('}')) {
                let items: Vec<&str> = inner.split(',').collect();
                let num = force_num || items.iter().all(|i| i.trim().parse::<f64>().is_ok());
                let lit = if num { "numLit" } else { "strLit" };
                let mut s = format!("<c:{tag}><c:{lit}>");
                if num {
                    s.push_str("<c:formatCode>General</c:formatCode>");
                }
                s.push_str(&format!("<c:ptCount val=\"{}\"/>", items.len()));
                for (i, it) in items.iter().enumerate() {
                    s.push_str(&format!(
                        "<c:pt idx=\"{i}\"><c:v>{}</c:v></c:pt>",
                        escape_text(it.trim().trim_matches('"'))
                    ));
                }
                s.push_str(&format!("</c:{lit}></c:{tag}>"));
                s
            } else {
                format!("<c:{tag}><c:v>{}</c:v></c:{tag}>", escape_text(t))
            }
        }
    }
}

fn rich(text: &str, size: Option<u32>) -> String {
    let sz = size.map_or(String::new(), |s| format!(" sz=\"{s}\""));
    let paras: String = text
        .split('\n')
        .map(|l| {
            format!(
                "<a:p><a:pPr><a:defRPr{sz}/></a:pPr><a:r><a:rPr lang=\"en-US\"{sz}/><a:t>{}</a:t></a:r></a:p>",
                escape_text(l)
            )
        })
        .collect();
    format!("<c:tx><c:rich><a:bodyPr/><a:lstStyle/>{paras}</c:rich></c:tx>")
}

fn legend_pos(code: i32) -> &'static str {
    match code {
        -4131 => "l",
        -4160 => "t",
        -4107 => "b",
        2 => "tr",
        _ => "r",
    }
}

fn type_parts(t: i32) -> (&'static str, &'static str, &'static str) {
    const G: [&str; 3] = ["clustered", "stacked", "percentStacked"];
    const A: [&str; 3] = ["standard", "stacked", "percentStacked"];
    match t {
        51..=53 => ("barChart", "col", G[(t - 51) as usize]),
        57..=59 => ("barChart", "bar", G[(t - 57) as usize]),
        4 | 65 => ("lineChart", "", "standard"),
        63 | 64 => ("lineChart", "", "stacked"),
        66 | 67 => ("lineChart", "", "percentStacked"),
        5 | -4102 => ("pieChart", "", ""),
        -4120 => ("doughnutChart", "", ""),
        1 => ("areaChart", "", A[0]),
        76 | 77 => ("areaChart", "", A[(t - 75) as usize]),
        -4169 | 72..=75 => ("scatterChart", "", ""),
        -4151 | 81 | 82 => ("radarChart", "", ""),
        15 => ("bubbleChart", "", ""),
        _ => ("barChart", "col", "clustered"),
    }
}

fn num(v: f64) -> String {
    crate::write::number_text(v)
}

fn axis_xml(a: &ChartAxis, kind: &str, id: u32, cross: u32, pos: &str, between: bool) -> String {
    let orient = if a.reverse { "maxMin" } else { "minMax" };
    let mut s =
        format!("<c:{kind}><c:axId val=\"{id}\"/><c:scaling><c:orientation val=\"{orient}\"/>");
    if let Some(v) = a.max {
        s.push_str(&format!("<c:max val=\"{}\"/>", num(v)));
    }
    if let Some(v) = a.min {
        s.push_str(&format!("<c:min val=\"{}\"/>", num(v)));
    }
    s.push_str(&format!(
        "</c:scaling><c:delete val=\"0\"/><c:axPos val=\"{pos}\"/>"
    ));
    if a.grid_major {
        s.push_str("<c:majorGridlines/>");
    }
    if a.grid_minor {
        s.push_str("<c:minorGridlines/>");
    }
    if a.has_title {
        s.push_str(&format!(
            "<c:title>{}<c:overlay val=\"0\"/></c:title>",
            rich(&a.title, None)
        ));
    }
    s.push_str("<c:numFmt formatCode=\"General\" sourceLinked=\"1\"/><c:majorTickMark val=\"out\"/><c:minorTickMark val=\"none\"/><c:tickLblPos val=\"nextTo\"/>");
    s.push_str(&format!(
        "<c:crossAx val=\"{cross}\"/><c:crosses val=\"autoZero\"/>"
    ));
    if kind == "catAx" {
        s.push_str("<c:auto val=\"1\"/><c:lblAlgn val=\"ctr\"/><c:lblOffset val=\"100\"/><c:noMultiLvlLbl val=\"0\"/>");
    } else {
        let cb = if between { "between" } else { "midCat" };
        s.push_str(&format!("<c:crossBetween val=\"{cb}\"/>"));
        if let Some(m) = a.major {
            s.push_str(&format!("<c:majorUnit val=\"{}\"/>", num(m)));
        }
    }
    s.push_str(&format!("</c:{kind}>"));
    s
}

pub(crate) fn chart_xml(wb: &Workbook, c: &Chart) -> String {
    let (el, dir, grouping) = type_parts(c.chart_type);
    let scatter = el == "scatterChart" || el == "bubbleChart";
    let no_axes = matches!(el, "pieChart" | "doughnutChart");
    let mut s = String::from(rawxml::DECL);
    s.push_str(&format!(
        "<c:chartSpace xmlns:c=\"{NS_C}\" xmlns:a=\"{NS_A}\" xmlns:r=\"{}\"><c:roundedCorners val=\"0\"/><c:chart>",
        &REL[..REL.len() - 1]
    ));
    if c.has_title {
        let body = match &c.title_ref {
            Some(Part::Refs(a)) => format!(
                "<c:tx><c:strRef><c:f>{}</c:f>{}</c:strRef></c:tx>",
                escape_text(&areas_text(wb, a)),
                cache(wb, a, false)
            ),
            _ => rich(&c.title, Some(1400)),
        };
        s.push_str(&format!("<c:title>{body}<c:overlay val=\"0\"/></c:title>"));
    }
    s.push_str(&format!(
        "<c:autoTitleDeleted val=\"{}\"/><c:plotArea><c:layout/><c:{el}>",
        u8::from(!c.has_title)
    ));
    match el {
        "barChart" => s.push_str(&format!(
            "<c:barDir val=\"{dir}\"/><c:grouping val=\"{grouping}\"/><c:varyColors val=\"0\"/>"
        )),
        "lineChart" | "areaChart" => s.push_str(&format!(
            "<c:grouping val=\"{grouping}\"/><c:varyColors val=\"0\"/>"
        )),
        "pieChart" | "doughnutChart" => s.push_str("<c:varyColors val=\"1\"/>"),
        "scatterChart" => {
            let style = if matches!(c.chart_type, 72 | 73) {
                "smoothMarker"
            } else {
                "lineMarker"
            };
            s.push_str(&format!(
                "<c:scatterStyle val=\"{style}\"/><c:varyColors val=\"0\"/>"
            ));
        }
        "radarChart" => s.push_str("<c:radarStyle val=\"marker\"/><c:varyColors val=\"0\"/>"),
        _ => s.push_str("<c:varyColors val=\"0\"/>"),
    }
    for (i, ser) in c.series.iter().enumerate() {
        s.push_str(&format!(
            "<c:ser><c:idx val=\"{i}\"/><c:order val=\"{i}\"/>"
        ));
        s.push_str(&part_xml(wb, "tx", &ser.name, false));
        s.push_str(&crate::serfmt_xml::series_pre(el, c.chart_type, &ser.fmt));
        if scatter {
            s.push_str(&part_xml(wb, "xVal", &ser.x, true));
            s.push_str(&part_xml(wb, "yVal", &ser.values, true));
        } else {
            s.push_str(&part_xml(wb, "cat", &ser.x, false));
            s.push_str(&part_xml(wb, "val", &ser.values, true));
        }
        if el == "lineChart" || el == "scatterChart" {
            s.push_str(&crate::serfmt_xml::smooth(c.chart_type, &ser.fmt));
        }
        s.push_str("</c:ser>");
    }
    match el {
        "barChart" => s.push_str("<c:gapWidth val=\"150\"/>"),
        "doughnutChart" => s.push_str("<c:firstSliceAng val=\"0\"/><c:holeSize val=\"50\"/>"),
        "pieChart" => s.push_str("<c:firstSliceAng val=\"0\"/>"),
        _ => {}
    }
    if !no_axes {
        s.push_str("<c:axId val=\"111111111\"/><c:axId val=\"222222222\"/>");
    }
    s.push_str(&format!("</c:{el}>"));
    if !no_axes {
        if scatter {
            s.push_str(&axis_xml(
                &c.axes[0],
                "valAx",
                111_111_111,
                222_222_222,
                "b",
                false,
            ));
        } else {
            let p = if dir == "bar" { "l" } else { "b" };
            s.push_str(&axis_xml(
                &c.axes[0],
                "catAx",
                111_111_111,
                222_222_222,
                p,
                true,
            ));
        }
        let p = if dir == "bar" { "b" } else { "l" };
        s.push_str(&axis_xml(
            &c.axes[1],
            "valAx",
            222_222_222,
            111_111_111,
            p,
            !scatter,
        ));
    }
    s.push_str("</c:plotArea>");
    if c.has_legend {
        s.push_str(&format!(
            "<c:legend><c:legendPos val=\"{}\"/><c:overlay val=\"0\"/></c:legend>",
            legend_pos(c.legend_pos)
        ));
    }
    s.push_str("<c:plotVisOnly val=\"1\"/><c:dispBlanksAs val=\"gap\"/></c:chart></c:chartSpace>");
    s
}

fn anchor_xml(a: &Anchor) -> String {
    format!(
        "<xdr:col>{}</xdr:col><xdr:colOff>{}</xdr:colOff><xdr:row>{}</xdr:row><xdr:rowOff>{}</xdr:rowOff>",
        a.col, a.col_off, a.row, a.row_off
    )
}

fn wrap_anchor(wb: &Workbook, sheet: SheetId, pos: &Pos, body: &str) -> String {
    if wb
        .sheet(sheet)
        .is_some_and(|s| s.kind == xlcalc::SheetKind::Chart)
    {
        let (x, y, w, h) = wb.shape_rect_emu(sheet, pos);
        return format!(
            "<xdr:absoluteAnchor><xdr:pos x=\"{x}\" y=\"{y}\"/><xdr:ext cx=\"{w}\" cy=\"{h}\"/>{body}<xdr:clientData/></xdr:absoluteAnchor>"
        );
    }
    let (edit, from, to) = match pos {
        Pos::Both { from, to } => ("", *from, *to),
        Pos::One { from, w, h } => {
            let (x, y) = wb.anchor_xy(sheet, from);
            ("oneCell", *from, wb.xy_anchor(sheet, x + w, y + h))
        }
        Pos::Abs { x, y, w, h } => (
            "absolute",
            wb.xy_anchor(sheet, *x, *y),
            wb.xy_anchor(sheet, x + w, y + h),
        ),
    };
    let attr = if edit.is_empty() {
        String::new()
    } else {
        format!(" editAs=\"{edit}\"")
    };
    format!(
        "<xdr:twoCellAnchor{attr}><xdr:from>{}</xdr:from><xdr:to>{}</xdr:to>{body}<xdr:clientData/></xdr:twoCellAnchor>",
        anchor_xml(&from),
        anchor_xml(&to)
    )
}

fn xfrm(wb: &Workbook, sheet: SheetId, s: &Shape) -> String {
    let (x, y, w, h) = wb.shape_rect_emu(sheet, &s.pos);
    let mut a = String::from("<a:xfrm");
    if s.rotation != 0.0 {
        a.push_str(&format!(
            " rot=\"{}\"",
            (s.rotation * 60000.0).round() as i64
        ));
    }
    if s.flip_h {
        a.push_str(" flipH=\"1\"");
    }
    if s.flip_v {
        a.push_str(" flipV=\"1\"");
    }
    a.push_str(&format!(
        "><a:off x=\"{x}\" y=\"{y}\"/><a:ext cx=\"{w}\" cy=\"{h}\"/></a:xfrm>"
    ));
    a
}

fn c_nv_pr(s: &Shape) -> String {
    let mut t = format!(
        "<xdr:cNvPr id=\"{}\" name=\"{}\"",
        s.id,
        escape_attr(&s.name)
    );
    if !s.alt_text.is_empty() {
        t.push_str(&format!(" descr=\"{}\"", escape_attr(&s.alt_text)));
    }
    if !s.visible {
        t.push_str(" hidden=\"1\"");
    }
    t.push_str("/>");
    t
}

fn tx_body(s: &Shape) -> String {
    let paras: String = if s.text.is_empty() {
        "<a:p><a:endParaRPr lang=\"en-US\"/></a:p>".to_string()
    } else {
        s.text
            .split('\n')
            .map(|l| {
                format!(
                    "<a:p><a:r><a:rPr lang=\"en-US\"/><a:t>{}</a:t></a:r></a:p>",
                    escape_text(l)
                )
            })
            .collect()
    };
    format!("<xdr:txBody><a:bodyPr/><a:lstStyle/>{paras}</xdr:txBody>")
}

fn group_body(wb: &Workbook, sheet: SheetId, s: &Shape, kids: &[u32]) -> String {
    let (x, y, w, h) = wb.shape_rect_emu(sheet, &s.pos);
    let inner: String = kids
        .iter()
        .filter_map(|k| wb.drawing(sheet)?.get(*k))
        .filter_map(|c| shape_body(wb, sheet, c, None, None))
        .collect();
    format!(
        "<xdr:grpSp><xdr:nvGrpSpPr>{}<xdr:cNvGrpSpPr/></xdr:nvGrpSpPr><xdr:grpSpPr><a:xfrm><a:off x=\"{x}\" y=\"{y}\"/><a:ext cx=\"{w}\" cy=\"{h}\"/><a:chOff x=\"{x}\" y=\"{y}\"/><a:chExt cx=\"{w}\" cy=\"{h}\"/></a:xfrm></xdr:grpSpPr>{inner}</xdr:grpSp>",
        c_nv_pr(s)
    )
}

fn shape_xml(
    wb: &Workbook,
    sheet: SheetId,
    s: &Shape,
    rid: Option<&str>,
    link: Option<&str>,
) -> Option<String> {
    let body = shape_body(wb, sheet, s, rid, link)?;
    Some(wrap_anchor(wb, sheet, &s.pos, &body))
}

fn shape_body(
    wb: &Workbook,
    sheet: SheetId,
    s: &Shape,
    rid: Option<&str>,
    link: Option<&str>,
) -> Option<String> {
    let body = match &s.kind {
        ShapeKind::Slicer => {
            let (x, y, w, h) = wb.shape_rect_emu(sheet, &s.pos);
            let cnv = c_nv_pr(s);
            crate::slicers_xml::frame(&s.name, &cnv, x, y, w, h)
        }
        ShapeKind::Timeline => {
            let (x, y, w, h) = wb.shape_rect_emu(sheet, &s.pos);
            let cnv = c_nv_pr(s);
            crate::slicers_xml::timeline_frame(&s.name, &cnv, x, y, w, h)
        }
        ShapeKind::Group(kids) => group_body(wb, sheet, s, kids),
        ShapeKind::Auto(t) => format!(
            "<xdr:sp macro=\"\" textlink=\"\"><xdr:nvSpPr>{}<xdr:cNvSpPr/></xdr:nvSpPr><xdr:spPr>{}<a:prstGeom prst=\"{}\"><a:avLst/></a:prstGeom><a:solidFill><a:schemeClr val=\"accent1\"/></a:solidFill><a:ln w=\"12700\"><a:solidFill><a:schemeClr val=\"accent1\"><a:shade val=\"50000\"/></a:schemeClr></a:solidFill></a:ln></xdr:spPr>{}</xdr:sp>",
            c_nv_pr(s),
            xfrm(wb, sheet, s),
            type_prst(*t),
            tx_body(s)
        ),
        ShapeKind::TextBox => format!(
            "<xdr:sp macro=\"\" textlink=\"\"><xdr:nvSpPr>{}<xdr:cNvSpPr txBox=\"1\"/></xdr:nvSpPr><xdr:spPr>{}<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom><a:noFill/></xdr:spPr>{}</xdr:sp>",
            c_nv_pr(s),
            xfrm(wb, sheet, s),
            tx_body(s)
        ),
        ShapeKind::Line => format!(
            "<xdr:cxnSp macro=\"\"><xdr:nvCxnSpPr>{}<xdr:cNvCxnSpPr/></xdr:nvCxnSpPr><xdr:spPr>{}<a:prstGeom prst=\"line\"><a:avLst/></a:prstGeom><a:ln w=\"9525\"><a:solidFill><a:schemeClr val=\"tx1\"/></a:solidFill></a:ln></xdr:spPr></xdr:cxnSp>",
            c_nv_pr(s),
            xfrm(wb, sheet, s)
        ),
        ShapeKind::Chart => {
            let rid = rid?;
            let (x, y, w, h) = wb.shape_rect_emu(sheet, &s.pos);
            format!(
                "<xdr:graphicFrame macro=\"\"><xdr:nvGraphicFramePr>{}<xdr:cNvGraphicFramePr/></xdr:nvGraphicFramePr><xdr:xfrm><a:off x=\"{x}\" y=\"{y}\"/><a:ext cx=\"{w}\" cy=\"{h}\"/></xdr:xfrm><a:graphic><a:graphicData uri=\"{NS_C}\"><c:chart xmlns:c=\"{NS_C}\" xmlns:r=\"{}\" r:id=\"{rid}\"/></a:graphicData></a:graphic></xdr:graphicFrame>",
                c_nv_pr(s),
                &REL[..REL.len() - 1]
            )
        }
        ShapeKind::Picture => {
            if rid.is_none() && link.is_none() {
                return None;
            }
            let mut blip = String::new();
            if let Some(r) = rid {
                blip.push_str(&format!(" r:embed=\"{r}\""));
            }
            if let Some(r) = link {
                blip.push_str(&format!(" r:link=\"{r}\""));
            }
            let lock = if s.lock_aspect {
                "<a:picLocks noChangeAspect=\"1\"/>"
            } else {
                "<a:picLocks/>"
            };
            format!(
                "<xdr:pic><xdr:nvPicPr>{}<xdr:cNvPicPr>{lock}</xdr:cNvPicPr></xdr:nvPicPr><xdr:blipFill><a:blip xmlns:r=\"{}\"{blip}/><a:stretch><a:fillRect/></a:stretch></xdr:blipFill><xdr:spPr>{}<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom></xdr:spPr></xdr:pic>",
                c_nv_pr(s),
                &REL[..REL.len() - 1],
                xfrm(wb, sheet, s)
            )
        }
    };
    Some(body)
}

fn patch_item(
    wb: &Workbook,
    sheet: SheetId,
    raw: &str,
    old: &Shape,
    new: &Shape,
) -> Option<String> {
    let mut same = new.clone();
    same.pos = old.pos;
    same.name = old.name.clone();
    same.alt_text = old.alt_text.clone();
    same.visible = old.visible;
    same.text = old.text.clone();
    same.flip_h = old.flip_h;
    same.flip_v = old.flip_v;
    same.rotation = old.rotation;
    same.chart = old.chart.clone();
    if same != *old {
        return None;
    }
    let anchor = [
        "<xdr:twoCellAnchor",
        "<xdr:oneCellAnchor",
        "<xdr:absoluteAnchor",
    ]
    .iter()
    .any(|a| raw.starts_with(a));
    if !anchor
        || matches!(
            old.kind,
            ShapeKind::Group(_) | ShapeKind::Slicer | ShapeKind::Timeline
        )
    {
        return None;
    }
    let first = [
        "<xdr:sp ",
        "<xdr:sp>",
        "<xdr:pic>",
        "<xdr:pic ",
        "<xdr:cxnSp ",
        "<xdr:cxnSp>",
        "<xdr:graphicFrame",
    ]
    .iter()
    .filter_map(|m| raw.find(m))
    .min()?;
    let tail = raw.rfind("<xdr:clientData")?;
    if tail < first {
        return None;
    }
    let mut body = raw[first..tail].to_string();

    let at = body.find("<xdr:cNvPr ")?;
    let end = at + body[at..].find('>')? + 1;
    let mut tag = Tag::parse(&body[at..end]).ok()?;
    tag.set("name", Some(&new.name));
    tag.set(
        "descr",
        (!new.alt_text.is_empty()).then_some(new.alt_text.as_str()),
    );
    tag.set("hidden", (!new.visible).then_some("1"));
    body.replace_range(at..end, &tag.write());

    let (open, close, gen) = if matches!(old.kind, ShapeKind::Chart) {
        ("<xdr:xfrm", "</xdr:xfrm>", None)
    } else {
        ("<a:xfrm", "</a:xfrm>", Some(xfrm(wb, sheet, new)))
    };
    if let Some(gen) = gen {
        let xs = body.find(open)?;
        let xe = xs + body[xs..].find(close)? + close.len();
        body.replace_range(xs..xe, &gen);
    } else if new.pos != old.pos {
        let xs = body.find(open)?;
        let xe = xs + body[xs..].find(close)? + close.len();
        let (x, y, w, h) = wb.shape_rect_emu(sheet, &new.pos);
        body.replace_range(
            xs..xe,
            &format!(
                "<xdr:xfrm><a:off x=\"{x}\" y=\"{y}\"/><a:ext cx=\"{w}\" cy=\"{h}\"/></xdr:xfrm>"
            ),
        );
    }
    if new.text != old.text {
        let ts = body.find("<xdr:txBody>")?;
        let te = ts + body[ts..].find("</xdr:txBody>")? + "</xdr:txBody>".len();
        body.replace_range(ts..te, &tx_body(new));
    }
    let wrapped = wrap_anchor(wb, sheet, &new.pos, "\u{1}");
    let (head, foot) = wrapped.split_once('\u{1}')?;
    let _ = foot;
    Some(format!("{head}{body}{}", &raw[tail..]))
}

fn same_members(
    wb: &Workbook,
    sheet: SheetId,
    sh: &Shape,
    model: &[Shape],
    item: &crate::drawings::Item,
) -> bool {
    if !matches!(sh.kind, ShapeKind::Group(_)) {
        return true;
    }
    let Some(d) = wb.drawing(sheet) else {
        return item.members.is_empty();
    };
    let ids = d.block(sh.id);
    let mine: Vec<&Shape> = ids[1..]
        .iter()
        .filter_map(|i| model.iter().find(|s| s.id == *i))
        .collect();
    mine.len() == item.members.len() && mine.iter().zip(&item.members).all(|(a, b)| *a == b)
}

pub(crate) struct Built {
    pub part: String,
    pub xml: String,
    pub rels: Vec<Rel>,

    pub charts: Vec<(String, String)>,

    pub media: Vec<(String, Vec<u8>)>,

    pub rel_id: String,
    pub new: bool,
}

pub(crate) struct Namer<'a> {
    pub part: &'a mut dyn FnMut(&str) -> String,
    pub rel: &'a mut dyn FnMut() -> String,
    pub sheet_rel: &'a mut dyn FnMut() -> String,
}

pub(crate) fn plan(
    wb: &Workbook,
    sheet: SheetId,
    pkg: Option<&Package>,
    sheet_part: &str,
    namer: &mut Namer,
    warnings: &mut Vec<String>,
) -> Option<Option<Built>> {
    let model: Vec<Shape> = wb.drawing(sheet).map_or(Vec::new(), |d| d.shapes.clone());
    let src = pkg.and_then(|p| {
        drawing_part_of(p, sheet_part)
            .and_then(|(dp, rid)| read_drawing(wb, sheet, p, &dp).map(|parsed| (dp, rid, parsed)))
    });
    let src_shapes: Vec<&Shape> = src.as_ref().map_or(Vec::new(), |(_, _, p)| {
        p.items
            .iter()
            .filter(|i| i.shape.is_some())
            .flat_map(|i| i.shape.iter().chain(i.members.iter()))
            .collect()
    });
    let same =
        src_shapes.len() == model.len() && src_shapes.iter().zip(&model).all(|(a, b)| *a == b);
    if same {
        return None;
    }
    let opaque_only = src
        .as_ref()
        .is_none_or(|(_, _, p)| p.items.iter().all(|i| i.shape.is_none()));
    if model.is_empty() && opaque_only {
        return Some(None);
    }
    let (part, rel_id, new, mut rels, root, src_items) = match src {
        Some((dp, rid, p)) => (dp, rid, false, p.rels, Some(p.root), p.items),
        None => (
            (namer.part)("xl/drawings/drawing#.xml"),
            (namer.sheet_rel)(),
            true,
            Vec::new(),
            None,
            Vec::new(),
        ),
    };
    let mut root = root.unwrap_or_else(|| Tag::new("xdr:wsDr"));
    for (k, v) in [("xmlns:xdr", NS_XDR), ("xmlns:a", NS_A)] {
        if root.get(k).is_none() {
            root.set(k, Some(v));
        }
    }
    let mut out_items: Vec<String> = Vec::new();
    let mut charts: Vec<(String, String)> = Vec::new();
    let mut media: Vec<(String, Vec<u8>)> = Vec::new();
    let mut used_src: BTreeSet<usize> = BTreeSet::new();
    for sh in model.iter().filter(|s| s.parent.is_none()) {
        let found = src_items
            .iter()
            .enumerate()
            .find(|(_, i)| i.shape.as_ref().is_some_and(|s| s.id == sh.id));
        if let Some((ix, item)) = found {
            used_src.insert(ix);
            let same = same_members(wb, sheet, sh, &model, item);
            if item.shape.as_ref() == Some(sh) && same {
                out_items.push(item.raw.clone());
                continue;
            }
            if matches!(sh.kind, ShapeKind::Group(_)) && !same {
                let rich = wb.drawing(sheet).is_some_and(|d| {
                    d.block(sh.id).iter().any(|i| {
                        d.get(*i).is_some_and(|s| {
                            matches!(s.kind, ShapeKind::Picture | ShapeKind::Chart)
                        })
                    })
                });
                if rich {
                    warnings.push(format!(
                        "group {}: pictures and charts inside are kept as read",
                        sh.name
                    ));
                    out_items.push(item.raw.clone());
                } else if let Some(x) = shape_xml(wb, sheet, sh, None, None) {
                    out_items.push(x);
                } else {
                    out_items.push(item.raw.clone());
                }
                continue;
            }
        }
        if let Some((_, item)) = found {
            if let Some(patched) = item
                .shape
                .as_ref()
                .and_then(|o| patch_item(wb, sheet, &item.raw, o, sh))
            {
                if sh.kind == ShapeKind::Chart
                    && item.shape.as_ref().and_then(|o| o.chart.as_ref()) != sh.chart.as_ref()
                {
                    if let (Some(chart), Some((cp, _))) = (sh.chart.as_ref(), item.chart.clone()) {
                        charts.push((cp, chart_xml(wb, chart)));
                    }
                }
                out_items.push(patched);
                continue;
            }
        }
        let mut rid = None;
        if sh.kind == ShapeKind::Chart {
            let Some(chart) = sh.chart.as_ref() else {
                continue;
            };
            let (cpart, id) = match found.and_then(|(_, i)| i.chart.clone()) {
                Some((cp, id)) => (cp, id),
                None => {
                    let cp = (namer.part)("xl/charts/chart#.xml");
                    let id = (namer.rel)();
                    rels.push(Rel {
                        id: id.clone(),
                        ty: format!("{REL}chart"),
                        target: relative(dir_of(&part), &cp),
                        external: false,
                    });
                    (cp, id)
                }
            };
            let unchanged = found
                .and_then(|(_, i)| i.shape.as_ref())
                .is_some_and(|s| s.chart.as_ref() == Some(chart));
            if !unchanged {
                charts.push((cpart, chart_xml(wb, chart)));
            }
            rid = Some(id);
        }
        let mut link_rid = None;
        if let (ShapeKind::Picture, Some(img)) = (&sh.kind, &sh.image) {
            if img.embedded || !img.linked {
                let mp = (namer.part)(&format!("xl/media/image#.{}", img.ext));
                let id = (namer.rel)();
                rels.push(Rel {
                    id: id.clone(),
                    ty: format!("{REL}image"),
                    target: relative(dir_of(&part), &mp),
                    external: false,
                });
                media.push((mp, img.bytes.clone()));
                rid = Some(id);
            }
            if img.linked {
                let id = (namer.rel)();
                rels.push(Rel {
                    id: id.clone(),
                    ty: format!("{REL}image"),
                    target: format!("file:///{}", img.source),
                    external: true,
                });
                link_rid = Some(id);
            }
        }
        match shape_xml(wb, sheet, sh, rid.as_deref(), link_rid.as_deref()) {
            Some(x) => out_items.push(x),
            None => match found {
                Some((_, item)) => out_items.push(item.raw.clone()),
                None => warnings.push(format!("shape {}: not written", sh.name)),
            },
        }
    }
    for (ix, item) in src_items.iter().enumerate() {
        if item.shape.is_none() && !used_src.contains(&ix) {
            out_items.push(item.raw.clone());
        }
    }
    root.empty = false;
    let mut xml = String::from(rawxml::DECL);
    xml.push_str(&root.open());
    for i in out_items {
        xml.push_str(&i);
    }
    xml.push_str(&format!("</{}>", root.name));
    Some(Some(Built {
        part,
        xml,
        rels,
        charts,
        media,
        rel_id,
        new,
    }))
}
