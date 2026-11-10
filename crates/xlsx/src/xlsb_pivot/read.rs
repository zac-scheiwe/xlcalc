use super::formula;
use super::*;
use crate::write::number_text;
use crate::xlsb::{error_name, esc, recs, rect_name, Rd};
use std::fmt::Write as _;

#[derive(Default)]
struct Items {
    xml: String,
    labels: Vec<String>,
}

impl Items {
    fn push(&mut self, xml: String, label: String) {
        self.xml.push_str(&xml);
        self.labels.push(label);
    }
}

fn date_text(rd: &mut Rd<'_>) -> Result<String, String> {
    let y = rd.u16()?;
    let m = rd.u16()?;
    let d = rd.u8()?;
    let h = rd.u8()?;
    let mi = rd.u8()?;
    let s = rd.u8()?;
    Ok(format!("{y:04}-{m:02}-{d:02}T{h:02}:{mi:02}:{s:02}"))
}

fn value_item(kind: u32, rd: &mut Rd<'_>) -> Result<(String, String), String> {
    Ok(match kind {
        I_MISSING => ("<m/>".to_string(), String::new()),
        I_NUM => {
            let v = number_text(rd.f64()?);
            (format!("<n v=\"{v}\"/>"), v)
        }
        I_BOOL => {
            let b = rd.u8()? != 0;
            (
                format!("<b v=\"{}\"/>", u8::from(b)),
                if b { "TRUE" } else { "FALSE" }.to_string(),
            )
        }
        I_ERR => {
            let e = error_name(rd.u8()?).unwrap_or("#N/A").to_string();
            (format!("<e v=\"{}\"/>", esc(&e)), e)
        }
        I_STR => {
            let s = rd.wstr()?;
            (format!("<s v=\"{}\"/>", esc(&s)), s)
        }
        I_DATE => {
            let s = date_text(rd)?;
            (format!("<d v=\"{s}\"/>"), s)
        }
        I_CALC => {
            let s = rd.wstr()?;
            (format!("<s v=\"{}\" f=\"1\"/>", esc(&s)), s)
        }
        _ => return Err(format!("pivot item {kind}")),
    })
}

fn array_items(rd: &mut Rd<'_>, into: &mut Items) -> Result<(), String> {
    let ty = rd.u16()?;
    let n = rd.u32()?;
    let kind = match ty {
        1 => I_NUM,
        2 => I_STR,
        0x10 => I_ERR,
        0x20 => I_DATE,
        _ => return Err(format!("pivot item array {ty}")),
    };
    for _ in 0..n {
        let (x, l) = value_item(kind, rd)?;
        into.push(x, l);
    }
    Ok(())
}

#[derive(Default)]
struct Fld {
    name: String,
    caption: Option<String>,
    num_fmt: u32,
    flags: u16,
    shared_attrs: String,
    shared: Items,
    shared_count: Option<u32>,
    has_shared: bool,
    group: String,
    rgce: Option<Vec<u8>>,
    names: Vec<usize>,
    group_items: Items,
    group_open: Option<(i32, i32)>,
    range: String,
    discrete: Option<Vec<u32>>,
}

struct Calc {
    rgce: Vec<u8>,
    field: usize,
    x: usize,
    names: Vec<(usize, usize)>,
}

fn shared_attrs(flags: u16, minmax: Option<(f64, f64)>) -> String {
    let mut a = String::new();
    if flags & 1 == 0 {
        a.push_str(" containsSemiMixedTypes=\"0\"");
    }
    if flags & 2 == 0 {
        a.push_str(" containsNonDate=\"0\"");
    }
    if flags & 4 != 0 {
        a.push_str(" containsDate=\"1\"");
    }
    if flags & 8 == 0 {
        a.push_str(" containsString=\"0\"");
    }
    if flags & 0x10 != 0 {
        a.push_str(" containsBlank=\"1\"");
    }
    if flags & 0x20 != 0 {
        a.push_str(" containsMixedTypes=\"1\"");
    }
    if flags & 0x40 != 0 {
        a.push_str(" containsNumber=\"1\"");
    }
    if flags & 0x80 != 0 {
        a.push_str(" containsInteger=\"1\"");
    }
    if flags & 0x200 != 0 {
        a.push_str(" longText=\"1\"");
    }
    if let Some((lo, hi)) = minmax {
        if flags & 4 != 0 && flags & 0x40 == 0 {
            let _ = write!(
                a,
                " minDate=\"{}\" maxDate=\"{}\"",
                crate::slicers_xml::iso(lo),
                crate::slicers_xml::iso(hi)
            );
        } else {
            let _ = write!(
                a,
                " minValue=\"{}\" maxValue=\"{}\"",
                number_text(lo),
                number_text(hi)
            );
        }
    }
    a
}

#[derive(PartialEq, Clone, Copy)]
enum Dest {
    None,
    Shared,
    Group,
}

pub(crate) fn cache_def(
    bin: &[u8],
    rid: &str,
    warnings: &mut Vec<String>,
) -> Result<String, String> {
    let mut head = String::new();
    let mut source = String::new();
    let mut fields: Vec<Fld> = Vec::new();
    let mut calcs: Vec<Calc> = Vec::new();
    let mut into = Dest::None;
    let mut in_calc: Option<Calc> = None;
    let mut area_ref = false;
    let mut tuple_field: Option<(i32, Option<(usize, usize)>)> = None;
    for (id, d) in recs(bin) {
        let mut rd = Rd::new(d);
        match id {
            R_DEF => {
                let refreshed = rd.u8()?;
                let min = rd.u8()?;
                let created = rd.u8()?;
                let f1 = rd.u8()?;
                rd.u32()?;
                let date = rd.f64()?;
                let f2 = rd.u8()?;
                let count = rd.u32()?;
                let by = if f2 & 1 != 0 {
                    rd.wstr()?
                } else {
                    String::new()
                };
                let _ = write!(
                    head,
                    " refreshedBy=\"{}\" refreshedDate=\"{}\" createdVersion=\"{created}\" refreshedVersion=\"{refreshed}\" minRefreshableVersion=\"{min}\" recordCount=\"{count}\"",
                    esc(&by),
                    number_text(date)
                );
                if f1 & 4 != 0 {
                    head.push_str(" refreshOnLoad=\"1\"");
                }
                if f1 & 1 == 0 {
                    head.push_str(" saveData=\"0\"");
                }
            }
            R_SRC => {
                let ty = rd.u32()?;
                if ty != 0 {
                    warnings.push("pivot cache: source is not a worksheet range".into());
                }
            }
            R_SHEETSRC => {
                let def = rd.u8()?;
                rd.u8()?;
                let fl = rd.u8()?;
                let sheet = if fl & 2 != 0 {
                    rd.wstr()?
                } else {
                    String::new()
                };
                if fl & 1 != 0 {
                    rd.wstr()?;
                }
                if def == 0 {
                    let r0 = rd.u32()?;
                    let r1 = rd.u32()?;
                    let c0 = rd.u32()?;
                    let c1 = rd.u32()?;
                    source = format!(
                        "<cacheSource type=\"worksheet\"><worksheetSource ref=\"{}\" sheet=\"{}\"/></cacheSource>",
                        rect_name(r0, c0, r1, c1),
                        esc(&sheet)
                    );
                } else {
                    let name = rd.wstr()?;
                    source = format!(
                        "<cacheSource type=\"worksheet\"><worksheetSource name=\"{}\"/></cacheSource>",
                        esc(&name)
                    );
                }
            }
            R_FIELD => {
                let flags = rd.u16()?;
                let num_fmt = rd.u32()?;
                rd.u16()?;
                rd.u32()?;
                rd.u32()?;
                let mapping = rd.u32()?;
                let name = rd.wstr()?;
                let caption = if flags & 8 != 0 {
                    Some(rd.wstr()?)
                } else {
                    None
                };
                let mut f = Fld {
                    name,
                    caption,
                    num_fmt,
                    flags,
                    ..Fld::default()
                };
                if flags & 0x100 != 0 {
                    let cce = rd.u32()? as usize;
                    f.rgce = Some(rd.take(cce)?.to_vec());
                    let cb = rd.u32()? as usize;
                    rd.take(cb)?;
                }
                let _ = mapping;
                fields.push(f);
                into = Dest::None;
            }
            R_SHARED => {
                let flags = rd.u16()?;
                let count = rd.u32()?;
                let mm = if flags & 0x100 != 0 {
                    Some((rd.f64()?, rd.f64()?))
                } else {
                    None
                };
                if let Some(f) = fields.last_mut() {
                    f.shared_attrs = shared_attrs(flags, mm);
                    f.shared_count = Some(count);
                    f.has_shared = true;
                }
                into = Dest::Shared;
            }
            R_SHARED_END | R_GITEMS_END => into = Dest::None,
            I_MISSING | I_NUM | I_BOOL | I_ERR | I_STR | I_DATE | I_CALC => {
                if let Some(f) = fields.last_mut() {
                    let (x, l) = value_item(id, &mut rd)?;
                    match into {
                        Dest::Shared => f.shared.push(x, l),
                        Dest::Group => f.group_items.push(x, l),
                        Dest::None => {}
                    }
                }
            }
            R_ARRAY => {
                if let Some(f) = fields.last_mut() {
                    match into {
                        Dest::Shared => array_items(&mut rd, &mut f.shared)?,
                        Dest::Group => array_items(&mut rd, &mut f.group_items)?,
                        Dest::None => {}
                    }
                }
            }
            R_GROUP => {
                let par = rd.u32()? as i32;
                let base = rd.u32()? as i32;
                if let Some(f) = fields.last_mut() {
                    f.group_open = Some((par, base));
                }
            }
            R_RANGEPR => {
                let by = rd.u8()?;
                let fl = rd.u8()?;
                let lo = rd.f64()?;
                let hi = rd.f64()?;
                let step = rd.f64()?;
                let mut a = String::new();
                if fl & 1 == 0 {
                    a.push_str(" autoStart=\"0\"");
                }
                if fl & 2 == 0 {
                    a.push_str(" autoEnd=\"0\"");
                }
                let by_name = [
                    "", "seconds", "minutes", "hours", "days", "months", "quarters", "years",
                ];
                if by > 0 {
                    let _ = write!(
                        a,
                        " groupBy=\"{}\"",
                        by_name.get(usize::from(by)).unwrap_or(&"")
                    );
                    let _ = write!(
                        a,
                        " startDate=\"{}\" endDate=\"{}\"",
                        crate::slicers_xml::iso(lo),
                        crate::slicers_xml::iso(hi)
                    );
                    if step != 1.0 {
                        let _ = write!(a, " groupInterval=\"{}\"", number_text(step));
                    }
                } else {
                    let _ = write!(
                        a,
                        " startNum=\"{}\" endNum=\"{}\" groupInterval=\"{}\"",
                        number_text(lo),
                        number_text(hi),
                        number_text(step)
                    );
                }
                if let Some(f) = fields.last_mut() {
                    f.range = format!("<rangePr{a}/>");
                }
            }
            R_DISCRETE => {
                if let Some(f) = fields.last_mut() {
                    f.discrete = Some(Vec::new());
                }
            }
            R_DISCRETE_X => {
                let v = rd.u32()?;
                if let Some(f) = fields.last_mut() {
                    if let Some(x) = f.discrete.as_mut() {
                        x.push(v);
                    }
                }
            }
            R_GITEMS => into = Dest::Group,
            R_GROUP_END => {
                if let Some(f) = fields.last_mut() {
                    if let Some((par, base)) = f.group_open.take() {
                        let mut g = String::from("<fieldGroup");
                        if par >= 0 {
                            let _ = write!(g, " par=\"{par}\"");
                        }
                        if base >= 0 {
                            let _ = write!(g, " base=\"{base}\"");
                        }
                        g.push('>');
                        g.push_str(&f.range);
                        if let Some(dp) = &f.discrete {
                            let _ = write!(g, "<discretePr count=\"{}\">", dp.len());
                            for v in dp {
                                let _ = write!(g, "<x v=\"{v}\"/>");
                            }
                            g.push_str("</discretePr>");
                        }
                        if !f.group_items.labels.is_empty() {
                            let _ = write!(
                                g,
                                "<groupItems count=\"{}\">{}</groupItems>",
                                f.group_items.labels.len(),
                                f.group_items.xml
                            );
                        }
                        g.push_str("</fieldGroup>");
                        f.group = g;
                    }
                }
            }
            R_TUPLE => {
                let fld = rd.u32()? as i32;
                tuple_field = Some((fld, None));
            }
            R_TMEMBER => {
                rd.u8()?;
                let f = rd.u32()? as usize;
                let it = rd.u32()? as usize;
                if let Some(t) = tuple_field.as_mut() {
                    t.1 = Some((f, it));
                }
            }
            R_TUPLE_END => {
                if let Some((fld, m)) = tuple_field.take() {
                    if let Some(c) = in_calc.as_mut() {
                        if let Some(m) = m {
                            c.names.push(m);
                        }
                    } else if let Some(f) = fields.last_mut() {
                        if fld >= 0 {
                            f.names.push(fld as usize);
                        }
                    }
                }
            }
            R_CALCMEM => {
                let _ = rd.u32()?;
                let cce = rd.u32()? as usize;
                in_calc = Some(Calc {
                    rgce: rd.take(cce)?.to_vec(),
                    field: 0,
                    x: 0,
                    names: Vec::new(),
                });
            }
            R_AREA => area_ref = true,
            R_REF => {
                let f = rd.u32()? as usize;
                if let Some(c) = in_calc.as_mut() {
                    c.field = f;
                }
            }
            R_REF_X => {
                let x = rd.u32()? as usize;
                if area_ref {
                    if let Some(c) = in_calc.as_mut() {
                        c.x = x;
                    }
                }
            }
            R_CALCMEM_END => {
                if let Some(c) = in_calc.take() {
                    calcs.push(c);
                }
                area_ref = false;
            }
            _ => {}
        }
    }

    let names: Vec<String> = fields.iter().map(|f| f.name.clone()).collect();
    let mut cf = String::new();
    for f in &fields {
        let mut a = format!(" name=\"{}\" numFmtId=\"{}\"", esc(&f.name), f.num_fmt);
        if let Some(c) = &f.caption {
            let _ = write!(a, " caption=\"{}\"", esc(c));
        }
        if let Some(rgce) = &f.rgce {
            let text = formula::decode(rgce, &|i| {
                f.names
                    .get(i)
                    .and_then(|&n| names.get(n))
                    .map(|n| formula::field_text(n))
            });
            match text {
                Ok(t) => {
                    let _ = write!(a, " formula=\"{}\"", esc(&t));
                }
                Err(e) => warnings.push(format!("pivot calculated field {}: {e}", f.name)),
            }
        }
        if f.flags & 4 == 0 {
            a.push_str(" databaseField=\"0\"");
        }
        let mut body = String::new();
        if f.has_shared {
            let n = f.shared.labels.len();
            let _ = f.shared_count;
            if n == 0 {
                let _ = write!(body, "<sharedItems{}/>", f.shared_attrs);
            } else {
                let _ = write!(
                    body,
                    "<sharedItems{} count=\"{n}\">{}</sharedItems>",
                    f.shared_attrs, f.shared.xml
                );
            }
        }
        body.push_str(&f.group);
        if body.is_empty() {
            let _ = write!(cf, "<cacheField{a}/>");
        } else {
            let _ = write!(cf, "<cacheField{a}>{body}</cacheField>");
        }
    }

    let mut ci = String::new();
    for c in &calcs {
        let label = |(f, it): (usize, usize)| {
            fields
                .get(f)
                .and_then(|x| x.shared.labels.get(it))
                .map(|l| formula::item_text(&fields[f].name, l))
        };
        match formula::decode(&c.rgce, &|i| c.names.get(i).and_then(|&m| label(m))) {
            Ok(t) => {
                let _ = write!(
                    ci,
                    "<calculatedItem formula=\"{}\"><pivotArea cacheIndex=\"1\" outline=\"0\" fieldPosition=\"0\"><references count=\"1\"><reference field=\"{}\" count=\"1\"><x v=\"{}\"/></reference></references></pivotArea></calculatedItem>",
                    esc(&t),
                    c.field,
                    c.x
                );
            }
            Err(e) => warnings.push(format!("pivot calculated item: {e}")),
        }
    }
    let ci = if ci.is_empty() {
        String::new()
    } else {
        format!(
            "<calculatedItems count=\"{}\">{ci}</calculatedItems>",
            calcs.len()
        )
    };
    Ok(format!(
        "<pivotCacheDefinition xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" r:id=\"{rid}\"{head}>{source}<cacheFields count=\"{}\">{cf}</cacheFields>{ci}</pivotCacheDefinition>",
        fields.len()
    ))
}

pub(crate) fn table(bin: &[u8]) -> Result<(String, i32), String> {
    let mut head = String::new();
    let mut cache_id = 0i32;
    let mut location = String::new();
    let mut fields = String::new();
    let mut nfields = 0usize;
    let mut row_fields = String::new();
    let mut col_fields = String::new();
    let mut pages = String::new();
    let mut npages = 0;
    let mut datas = String::new();
    let mut ndatas = 0;
    let mut style = String::new();

    let mut cur: Option<(String, String, String)> = None;
    let mut nitems = 0;
    let mut autosort = false;
    let mut sort_x = 0u32;
    let mut axis_count = 0;
    for (id, d) in recs(bin) {
        let mut rd = Rd::new(d);
        match id {
            T_DEF => {
                let f1 = rd.u32()?;
                let f2 = rd.u32()?;
                let f3 = rd.u32()?;
                let data_axis = rd.u8()?;
                rd.u8()?;
                let upd = rd.u8()?;
                let min = rd.u8()?;
                rd.u32()?;
                rd.u16()?;
                rd.u16()?;
                rd.u32()?;
                cache_id = rd.u32()? as i32;
                let name = rd.wstr()?;
                let _ = write!(head, " name=\"{}\" cacheId=\"{cache_id}\"", esc(&name));
                if data_axis == 1 {
                    head.push_str(" dataOnRows=\"1\"");
                }
                let _ = write!(
                    head,
                    " updatedVersion=\"{upd}\" minRefreshableVersion=\"{min}\" indent=\"{}\"",
                    (f1 >> 24) & 0x7f
                );
                if f2 & 0x100 != 0 {
                    head.push_str(" useAutoFormatting=\"1\"");
                }
                if f2 & 0x2000 == 0 {
                    head.push_str(" rowGrandTotals=\"0\"");
                }
                if f2 & 0x4000 == 0 {
                    head.push_str(" colGrandTotals=\"0\"");
                }
                if f2 & 0x20000 != 0 {
                    head.push_str(" itemPrintTitles=\"1\"");
                }
                if f3 & 8 == 0 {
                    head.push_str(" compact=\"0\"");
                }
                if f2 & 0x10000 != 0 {
                    head.push_str(" outline=\"1\"");
                }
                if f3 & 2 != 0 {
                    head.push_str(" outlineData=\"1\"");
                }
                if f3 & 1 == 0 {
                    head.push_str(" compactData=\"0\"");
                }
                let mut caption = |bit: bool, attr: &str, rd: &mut Rd<'_>| -> Result<(), String> {
                    if bit {
                        let s = rd.wstr()?;
                        let _ = write!(head, " {attr}=\"{}\"", esc(&s));
                    }
                    Ok(())
                };
                caption(f2 & 0x80000 != 0, "dataCaption", &mut rd)?;
                caption(f2 & 0x100000 != 0, "grandTotalCaption", &mut rd)?;
                caption(f3 & 0x40 == 0, "errorCaption", &mut rd)?;
                caption(f3 & 0x80 == 0, "missingCaption", &mut rd)?;
                caption(f2 & 0x200000 != 0, "pageStyle", &mut rd)?;
                caption(f2 & 0x400000 != 0, "pivotTableStyle", &mut rd)?;
                caption(f2 & 0x800000 != 0, "vacatedStyle", &mut rd)?;
                caption(f2 & 0x4000_0000 != 0, "tag", &mut rd)?;
                caption(f3 & 0x800 != 0, "colHeaderCaption", &mut rd)?;
                caption(f3 & 0x400 != 0, "rowHeaderCaption", &mut rd)?;
            }
            T_LOC => {
                let r0 = rd.u32()?;
                let r1 = rd.u32()?;
                let c0 = rd.u32()?;
                let c1 = rd.u32()?;
                let fh = rd.u32()?.wrapping_sub(r0);
                let fd = rd.u32()?.wrapping_sub(r0);
                let fc = rd.u32()?.wrapping_sub(c0);
                let rp = rd.u32()?;
                let cp = rd.u32()?;
                let _ = write!(
                    location,
                    "<location ref=\"{}\" firstHeaderRow=\"{fh}\" firstDataRow=\"{fd}\" firstDataCol=\"{fc}\"",
                    rect_name(r0, c0, r1, c1)
                );
                if rp > 0 {
                    let _ = write!(location, " rowPageCount=\"{rp}\"");
                }
                if cp > 0 {
                    let _ = write!(location, " colPageCount=\"{cp}\"");
                }
                location.push_str("/>");
            }
            T_FIELD => {
                let f1 = rd.u32()?;
                let num = rd.u32()?;
                let f2 = rd.u32()?;
                rd.u32()?;
                rd.u32()?;
                let mut a = String::new();
                if f1 & 0x2000_0000 != 0 {
                    let _ = write!(a, " name=\"{}\"", esc(&rd.wstr()?));
                }
                match f1 & 7 {
                    1 => a.push_str(" axis=\"axisRow\""),
                    2 => a.push_str(" axis=\"axisCol\""),
                    4 => a.push_str(" axis=\"axisPage\""),
                    3 => a.push_str(" axis=\"axisValues\""),
                    _ => {}
                }
                if f1 & 8 != 0 {
                    a.push_str(" dataField=\"1\"");
                }
                if f1 & 0x1000_0000 == 0 {
                    a.push_str(" compact=\"0\"");
                }
                if f2 & 0x20 != 0 {
                    a.push_str(" showAll=\"1\"");
                } else {
                    a.push_str(" showAll=\"0\"");
                }
                if f1 & 0x100 == 0 {
                    a.push_str(" defaultSubtotal=\"0\"");
                }
                for (i, n) in SUBTOTAL_ATTRS.iter().enumerate() {
                    if f1 & (0x200 << i) != 0 {
                        let _ = write!(a, " {n}=\"1\"");
                    }
                }
                if f2 & 1 == 0 {
                    a.push_str(" dragToRow=\"0\"");
                }
                if f2 & 2 == 0 {
                    a.push_str(" dragToCol=\"0\"");
                }
                if f2 & 4 == 0 {
                    a.push_str(" dragToPage=\"0\"");
                }
                if f2 & 0x80 != 0 {
                    a.push_str(" insertBlankRow=\"1\"");
                }
                if f2 & 0x1000 != 0 {
                    a.push_str(if f2 & 0x2000 != 0 {
                        " sortType=\"ascending\""
                    } else {
                        " sortType=\"descending\""
                    });
                }
                if f2 & 0x80000 != 0 {
                    a.push_str(" multipleItemSelectionAllowed=\"1\"");
                }
                if num > 0 {
                    let _ = write!(a, " numFmtId=\"{num}\"");
                }
                cur = Some((a, String::new(), String::new()));
                nitems = 0;
                nfields += 1;
            }
            T_ITEM => {
                let ty = rd.u8()?;
                let fl = rd.u16()?;
                let x = rd.u32()? as i32;
                let mut a = String::new();
                if fl & 0x10 != 0 {
                    let _ = write!(a, " n=\"{}\"", esc(&rd.wstr()?));
                }
                if fl & 4 != 0 {
                    a.push_str(" f=\"1\"");
                }
                if fl & 1 != 0 {
                    a.push_str(" h=\"1\"");
                }
                if fl & 2 != 0 {
                    a.push_str(" sd=\"0\"");
                }
                if ty != 0 {
                    let _ = write!(
                        a,
                        " t=\"{}\"",
                        ITEM_TYPES.get(usize::from(ty)).unwrap_or(&"data")
                    );
                } else {
                    let _ = write!(a, " x=\"{x}\"");
                }
                if let Some(c) = cur.as_mut() {
                    let _ = write!(c.1, "<item{a}/>");
                    nitems += 1;
                }
            }
            T_ITEMS_END => {}
            T_SORT => autosort = true,
            R_REF_X if autosort => sort_x = rd.u32()?,
            T_SORT_END => {
                if let Some(c) = cur.as_mut() {
                    let _ = write!(
                        c.2,
                        "<autoSortScope><pivotArea dataOnly=\"0\" outline=\"0\" fieldPosition=\"0\"><references count=\"1\"><reference field=\"4294967294\" count=\"1\" selected=\"0\"><x v=\"{sort_x}\"/></reference></references></pivotArea></autoSortScope>"
                    );
                }
                autosort = false;
            }
            T_FILL => {
                rd.u32()?;
                if rd.u32()? == 1 {
                    if let Some(c) = cur.as_mut() {
                        c.0.push_str(" fillDownLabels=\"1\"");
                    }
                }
            }
            T_FIELD_END => {
                if let Some((a, items, tail)) = cur.take() {
                    if nitems == 0 && tail.is_empty() {
                        let _ = write!(fields, "<pivotField{a}/>");
                    } else {
                        let _ = write!(
                            fields,
                            "<pivotField{a}><items count=\"{nitems}\">{items}</items>{tail}</pivotField>"
                        );
                    }
                }
            }
            T_ROWFIELDS | T_COLFIELDS => {
                let n = rd.u32()?;
                let mut s = format!(
                    "<{} count=\"{n}\">",
                    if id == T_ROWFIELDS {
                        "rowFields"
                    } else {
                        "colFields"
                    }
                );
                for _ in 0..n {
                    let _ = write!(s, "<field x=\"{}\"/>", rd.u32()? as i32);
                }
                let _ = write!(
                    s,
                    "</{}>",
                    if id == T_ROWFIELDS {
                        "rowFields"
                    } else {
                        "colFields"
                    }
                );
                if id == T_ROWFIELDS {
                    row_fields = s;
                } else {
                    col_fields = s;
                }
                axis_count += 1;
            }
            T_PAGE => {
                let f = rd.u32()?;
                let item = rd.u32()?;
                let hier = rd.u32()? as i32;
                let fl = rd.u8()?;
                let mut s = format!("<pageField fld=\"{f}\"");
                if item != PAGE_ALL {
                    let _ = write!(s, " item=\"{item}\"");
                }
                let _ = write!(s, " hier=\"{hier}\"");
                if fl & 1 != 0 {
                    let _ = write!(s, " name=\"{}\"", esc(&rd.wstr()?));
                }
                s.push_str("/>");
                pages.push_str(&s);
                npages += 1;
            }
            T_DATA => {
                let fld = rd.u32()?;
                let sub = rd.u32()?;
                let show = rd.u32()?;
                let bf = rd.u32()?;
                let bi = rd.u32()?;
                let nf = rd.u32()?;
                let has = rd.u8()?;
                let mut s = String::from("<dataField");
                if has == 1 {
                    let _ = write!(s, " name=\"{}\"", esc(&rd.wstr()?));
                }
                let _ = write!(s, " fld=\"{fld}\"");
                if sub > 0 {
                    let _ = write!(
                        s,
                        " subtotal=\"{}\"",
                        SUBTOTALS.get(sub as usize).unwrap_or(&"sum")
                    );
                }
                if show > 0 {
                    let _ = write!(
                        s,
                        " showDataAs=\"{}\"",
                        SHOW_AS.get(show as usize).unwrap_or(&"normal")
                    );
                }
                let _ = write!(s, " baseField=\"{}\" baseItem=\"{bi}\"", bf as i32);
                if nf > 0 {
                    let _ = write!(s, " numFmtId=\"{nf}\"");
                }
                s.push_str("/>");
                datas.push_str(&s);
                ndatas += 1;
            }
            T_STYLE => {
                let fl = rd.u16()?;
                let name = rd.wstr()?;
                let b = |m: u16| u8::from(fl & m != 0);
                style = format!(
                    "<pivotTableStyleInfo name=\"{}\" showRowHeaders=\"{}\" showColHeaders=\"{}\" showRowStripes=\"{}\" showColStripes=\"{}\" showLastColumn=\"{}\"/>",
                    esc(&name),
                    b(0x10),
                    b(0x20),
                    b(4),
                    b(8),
                    b(2)
                );
            }
            _ => {}
        }
    }
    let _ = axis_count;
    let pf = if npages > 0 {
        format!("<pageFields count=\"{npages}\">{pages}</pageFields>")
    } else {
        String::new()
    };
    let df = if ndatas > 0 {
        format!("<dataFields count=\"{ndatas}\">{datas}</dataFields>")
    } else {
        String::new()
    };
    Ok((
        format!(
            "<pivotTableDefinition xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"{head}>{location}<pivotFields count=\"{nfields}\">{fields}</pivotFields>{row_fields}{col_fields}{pf}{df}{style}</pivotTableDefinition>"
        ),
        cache_id,
    ))
}
