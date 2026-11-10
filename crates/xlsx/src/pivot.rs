use crate::rawxml::{escape_attr, escape_text};
use crate::write::number_text;
use crate::xml::Node;
use xlcalc::pivot::{
    display_order, map_idents, render, Ax, AxItem, DataField, DateUnit, Form, Func, Group, Orient,
    PivotCache, PivotTable, PtField, Sort,
};
use xlcalc::{Rect, SheetId, Value, Workbook};

pub(crate) struct PivotDef {
    pub sheet: usize,
    pub table: Node,
    pub cache: Node,
    pub cache_part: String,
}

const CUSTOM: [(&str, &str); 11] = [
    ("sum", "sumSubtotal"),
    ("countA", "countASubtotal"),
    ("avg", "avgSubtotal"),
    ("max", "maxSubtotal"),
    ("min", "minSubtotal"),
    ("product", "productSubtotal"),
    ("count", "countSubtotal"),
    ("stdDev", "stdDevSubtotal"),
    ("stdDevP", "stdDevPSubtotal"),
    ("var", "varSubtotal"),
    ("varP", "varPSubtotal"),
];

const SHOW: [(i32, &str); 10] = [
    (2, "difference"),
    (3, "percent"),
    (4, "percentDiff"),
    (5, "runTotal"),
    (6, "percentOfRow"),
    (7, "percentOfCol"),
    (8, "percentOfTotal"),
    (9, "index"),
    (10, "percentOfParentRow"),
    (11, "percentOfParentCol"),
];

fn func_of(s: Option<&str>) -> Func {
    Func::from_code(match s {
        Some("count") => -4112,
        Some("average") => -4106,
        Some("max") => -4136,
        Some("min") => -4139,
        Some("product") => -4149,
        Some("countNums") => -4113,
        Some("stdDev") => -4155,
        Some("stdDevp") => -4156,
        Some("var") => -4164,
        Some("varp") => -4165,
        _ => -4157,
    })
    .unwrap_or(Func::Sum)
}

fn func_name(f: Func) -> Option<&'static str> {
    match f {
        Func::Sum => None,
        Func::Count => Some("count"),
        Func::Average => Some("average"),
        Func::Max => Some("max"),
        Func::Min => Some("min"),
        Func::Product => Some("product"),
        Func::CountNums => Some("countNums"),
        Func::StdDev => Some("stdDev"),
        Func::StdDevP => Some("stdDevp"),
        Func::Var => Some("var"),
        Func::VarP => Some("varp"),
    }
}

fn shared_value(n: &Node) -> Option<Value> {
    let v = n.attr("v");
    Some(match n.name.as_str() {
        "s" => Value::text(v.unwrap_or("")),
        "n" => Value::Number(v?.parse().ok()?),
        "d" => Value::Number(crate::slicers_xml::serial(v?)?),
        "b" => Value::Bool(matches!(v, Some("1") | Some("true"))),
        "m" => Value::Blank,
        _ => return None,
    })
}

fn rect_of(s: &str) -> Option<Rect> {
    crate::parse_rect(s)
}

fn named_source(wb: &Workbook, name: &str) -> Option<(String, Rect)> {
    if let Some(t) = wb.table_by_name(name) {
        return Some((wb.sheet(t.sheet)?.name.to_string(), t.rect));
    }
    let def = wb.names().get(&name.to_uppercase(), None)?;
    let text = def.refers_to.trim_start_matches('=').replace('$', "");
    let (sheet, r) = text.rsplit_once('!')?;
    Some((sheet.trim_matches('\'').replace("''", "'"), rect_of(r)?))
}

pub(crate) fn adopt(
    wb: &mut Workbook,
    ids: &[SheetId],
    defs: &[PivotDef],
    warnings: &mut Vec<String>,
) {
    let mut caches: Vec<(String, usize)> = Vec::new();
    for d in defs {
        let name = d.table.attr("name").unwrap_or("PivotTable");
        let Some(&sheet) = ids.get(d.sheet) else {
            continue;
        };
        let ci = match caches.iter().find(|c| c.0 == d.cache_part) {
            Some(c) => c.1,
            None => {
                let Some(ws) = d.cache.child("cacheSource").and_then(|s| {
                    (s.attr("type") == Some("worksheet"))
                        .then(|| s.child("worksheetSource"))
                        .flatten()
                }) else {
                    warnings.push(format!(
                        "pivot table {name}: source is not a worksheet range"
                    ));
                    continue;
                };
                let named = ws.attr("name").and_then(|n| named_source(wb, n));
                let (Some(r), Some(sn)) = (
                    named
                        .as_ref()
                        .map(|n| n.1)
                        .or_else(|| ws.attr("ref").and_then(rect_of)),
                    named
                        .as_ref()
                        .map(|n| n.0.as_str())
                        .or_else(|| ws.attr("sheet")),
                ) else {
                    warnings.push(format!(
                        "pivot table {name}: source is not a worksheet range"
                    ));
                    continue;
                };
                let Some(src_sheet) = wb
                    .sheets()
                    .find(|s| s.name.eq_ignore_ascii_case(sn))
                    .map(|s| s.id)
                else {
                    warnings.push(format!("pivot table {name}: source sheet {sn} not found"));
                    continue;
                };
                let source = format!("{sn}!R{}C{}:R{}C{}", r.r0 + 1, r.c0 + 1, r.r1 + 1, r.c1 + 1);
                let Some(mut cache) = PivotCache::read(wb, &source, src_sheet, r) else {
                    warnings.push(format!("pivot table {name}: source range unreadable"));
                    continue;
                };
                if let Some(cfs) = d.cache.child("cacheFields") {
                    for cf in cfs.children("cacheField") {
                        if let (Some(n), Some(fm)) = (cf.attr("name"), cf.attr("formula")) {
                            cache.add_calc(n, fm);
                        }
                    }
                }
                wb.pivots_mut().caches.push(cache);
                let i = wb.pivots().caches.len() - 1;
                caches.push((d.cache_part.clone(), i));
                i
            }
        };

        let cfs: Vec<&Node> = d
            .cache
            .child("cacheFields")
            .map(|f| f.children("cacheField").collect())
            .unwrap_or_default();
        let kind = |cf: &Node| {
            if cf.attr("formula").is_some() {
                1
            } else if cf.attr("databaseField") == Some("0") {
                2
            } else {
                0
            }
        };
        let mut order: Vec<usize> = Vec::new();
        for k in 0..3 {
            order.extend((0..cfs.len()).filter(|&i| kind(cfs[i]) == k));
        }
        let mut fmap = vec![usize::MAX; cfs.len()];
        for (e, &i) in order.iter().enumerate() {
            fmap[i] = e;
        }
        let nfields = wb.pivots().caches[ci].fields.len();
        if order.iter().filter(|&&i| kind(cfs[i]) < 2).count() != nfields {
            warnings.push(format!(
                "pivot table {name}: cache fields do not match the source"
            ));
            continue;
        }
        let ntot = order.len();
        let Some(loc) = d
            .table
            .child("location")
            .and_then(|l| l.attr("ref"))
            .and_then(rect_of)
        else {
            continue;
        };
        let mut t = PivotTable::new(name, ci, nfields, sheet, loc.r0, loc.c0);
        let shared: Vec<Vec<Value>> = order
            .iter()
            .map(|&i| {
                cfs[i]
                    .child("sharedItems")
                    .map(|s| s.kids.iter().filter_map(shared_value).collect())
                    .unwrap_or_default()
            })
            .collect();

        let mut glabels: Vec<Option<Vec<String>>> = vec![None; ntot];
        let mut grouping_ok = true;
        for (e, &i) in order.iter().enumerate() {
            let Some(fg) = cfs[i].child("fieldGroup") else {
                continue;
            };
            let Some(gi) = fg.child("groupItems") else {
                continue;
            };
            let labels: Vec<String> = gi
                .kids
                .iter()
                .filter_map(shared_value)
                .map(|v| value_label(&v))
                .collect();
            let base = fg
                .attr("base")
                .and_then(|b| b.parse::<usize>().ok())
                .and_then(|b| fmap.get(b).copied())
                .filter(|&b| b < nfields);
            let num = |rp: &Node, k: &str| rp.attr(k).and_then(|v| v.parse::<f64>().ok());
            let date = |rp: &Node, k: &str| {
                rp.attr(k)
                    .and_then(crate::slicers_xml::serial)
                    .map(f64::floor)
            };
            if e >= nfields {
                t.fields.push(PtField::default());
                let group = match (base, fg.child("rangePr"), fg.child("discretePr")) {
                    (Some(src), Some(rp), _) => {
                        let unit = match rp.attr("groupBy") {
                            Some("days") => Some(DateUnit::Days(
                                num(rp, "groupInterval").map_or(1, |v| v.max(1.0) as u32),
                            )),
                            Some("months") => Some(DateUnit::Month),
                            Some("quarters") => Some(DateUnit::Quarter),
                            Some("years") => Some(DateUnit::Year),
                            _ => None,
                        };
                        unit.map(|unit| Group::Derived {
                            src,
                            start: (rp.attr("autoStart") == Some("0"))
                                .then(|| date(rp, "startDate"))
                                .flatten(),
                            end: (rp.attr("autoEnd") == Some("0"))
                                .then(|| date(rp, "endDate"))
                                .flatten(),
                            unit,
                        })
                    }
                    (Some(src), None, Some(dp)) => {
                        let disc: Vec<usize> = dp
                            .children("x")
                            .filter_map(|x| x.attr("v")?.parse().ok())
                            .collect();
                        let base_labels: Vec<String> =
                            shared[src].iter().map(value_label).collect();
                        let mut groups = Vec::new();
                        for (j, lab) in labels.iter().enumerate() {
                            let members: Vec<String> = disc
                                .iter()
                                .enumerate()
                                .filter(|(_, &g)| g == j)
                                .filter_map(|(b, _)| base_labels.get(b).cloned())
                                .collect();
                            if members.len() > 1 || members.first().is_some_and(|m| m != lab) {
                                groups.push((lab.clone(), members));
                            }
                        }
                        Some(Group::Manual { src, groups })
                    }
                    _ => None,
                };
                match group {
                    Some(g) => t.fields[e].group = Some(g),
                    None => grouping_ok = false,
                }
            } else if let Some(rp) = fg.child("rangePr").filter(|r| r.attr("startNum").is_some()) {
                t.fields[e].group = Some(Group::Num {
                    start: (rp.attr("autoStart") == Some("0"))
                        .then(|| num(rp, "startNum"))
                        .flatten(),
                    end: (rp.attr("autoEnd") == Some("0"))
                        .then(|| num(rp, "endNum"))
                        .flatten(),
                    by: num(rp, "groupInterval").filter(|b| *b > 0.0).unwrap_or(1.0),
                });
            }
            glabels[e] = Some(labels);
        }
        if !grouping_ok {
            warnings.push(format!("pivot table {name}: grouping not supported"));
            continue;
        }
        let pf: Vec<&Node> = d
            .table
            .child("pivotFields")
            .map(|p| p.children("pivotField").collect())
            .unwrap_or_default();
        let pf: Vec<&Node> = order.iter().filter_map(|&i| pf.get(i).copied()).collect();

        let mut order_cache = wb.pivots().caches[ci].clone();
        for (f, node) in pf.iter().enumerate().take(nfields) {
            if matches!(t.fields[f].group, Some(Group::Num { .. })) {
                continue;
            }
            let mut items = Vec::new();
            if let Some(its) = node.child("items") {
                for it in its.children("item") {
                    if it.attr("t").is_some() || it.attr("f") == Some("1") {
                        continue;
                    }
                    let Some(x) = it.attr("x").and_then(|x| x.parse::<usize>().ok()) else {
                        continue;
                    };
                    if let Some(v) = shared.get(f).and_then(|s| s.get(x)) {
                        items.push(xlcalc::pivot::Item {
                            value: v.clone(),
                            label: String::new(),
                        });
                    }
                }
            }
            if node.attr("sortType") == Some("descending") && node.child("autoSortScope").is_none()
            {
                items.reverse();
            }
            order_cache.fields[f].items = items;
        }
        let cache = &mut wb.pivots_mut().caches[ci];
        let mut fresh = cache.clone();
        fresh.keep_item_order(&order_cache, &[]);
        *cache = fresh;
        let cache = wb.pivots().caches[ci].clone();

        if let Some(cis) = d.cache.child("calculatedItems") {
            for ci_ in cis.children("calculatedItem") {
                let Some(r) = ci_
                    .child("pivotArea")
                    .and_then(|a| a.child("references"))
                    .and_then(|r| r.child("reference"))
                else {
                    continue;
                };
                let (Some(fld), Some(x)) = (
                    r.attr("field").and_then(|f| f.parse::<usize>().ok()),
                    r.child("x")
                        .and_then(|x| x.attr("v")?.parse::<usize>().ok()),
                ) else {
                    continue;
                };
                let Some(e) = fmap.get(fld).copied().filter(|&e| e < nfields) else {
                    continue;
                };

                if t.fields
                    .iter()
                    .any(|f| matches!(&f.group, Some(Group::Manual { src, .. }) if *src == e))
                {
                    warnings.push(format!(
                        "pivot table {name}: a calculated item of a field with a selection group is not kept"
                    ));
                    continue;
                }
                if let (Some(v), Some(fm)) = (shared[e].get(x), ci_.attr("formula")) {
                    t.fields[e].calc_items.push(xlcalc::pivot::CalcItem {
                        name: value_label(v),
                        formula: item_formula_in(fm),
                    });
                }
            }
        }
        let gcache = cache.grouped(&t);

        let resolve = |f: usize, x: usize| -> Option<usize> {
            if let Some(l) = &glabels[f] {
                let lab = l.get(x)?;
                gcache
                    .fields
                    .get(f)?
                    .items
                    .iter()
                    .position(|i| &i.label == lab)
            } else {
                let v = shared.get(f)?.get(x)?;
                gcache
                    .fields
                    .get(f)?
                    .items
                    .iter()
                    .position(|i| xlcalc::pivot::item_cmp(&i.value, v).is_eq())
            }
        };
        for (f, node) in pf.iter().enumerate().take(ntot) {
            t.fields[f].orient = match node.attr("axis") {
                Some("axisRow") => Orient::Row,
                Some("axisCol") => Orient::Column,
                Some("axisPage") => Orient::Page,
                _ => Orient::Hidden,
            };

            let custom = node
                .attrs
                .iter()
                .any(|a| a.0.ends_with("Subtotal") && a.0 != "defaultSubtotal" && a.1 == "1");
            t.fields[f].subtotals = node.attr("defaultSubtotal") != Some("0")
                && (!custom
                    || node.child("items").is_some_and(|i| {
                        i.children("item").any(|x| x.attr("t") == Some("default"))
                    }));
            if let Some(nm) = node.attr("name") {
                if gcache.fields.get(f).is_some_and(|g| nm != g.name) {
                    t.fields[f].caption = Some(nm.to_string());
                }
            }
            if let Some(its) = node.child("items") {
                for it in its.children("item") {
                    let Some(x) = it.attr("x").and_then(|x| x.parse::<usize>().ok()) else {
                        continue;
                    };
                    if it.attr("f") == Some("1") {
                        continue;
                    }
                    let Some(p) = resolve(f, x) else {
                        continue;
                    };
                    if let Some(n) = it.attr("n") {
                        t.fields[f].item_caps.push((p as u32, n.to_string()));
                    }
                    if it.attr("h") == Some("1") {
                        t.fields[f].hidden.push(p);
                    }
                }
            }
            t.fields[f].blank_line = node.attr("insertBlankRow") == Some("1");
            t.fields[f].multi_page = node.attr("multipleItemSelectionAllowed") == Some("1");
            t.fields[f].repeat = node.attr("fillDownLabels") == Some("1");
            for (i, (_, attr)) in CUSTOM.iter().enumerate() {
                if node.attr(attr) == Some("1") {
                    t.fields[f].custom.push(i as u8 + 2);
                }
            }
            let desc = node.attr("sortType") == Some("descending");
            if matches!(node.attr("sortType"), Some("ascending" | "descending")) {
                let data = node
                    .child("autoSortScope")
                    .and_then(|a| {
                        a.child("pivotArea")?
                            .child("references")?
                            .child("reference")?
                            .child("x")
                    })
                    .and_then(|x| x.attr("v")?.parse::<usize>().ok());
                t.fields[f].sort = match data {
                    Some(d) => Sort::Data(desc, d),
                    None => Sort::Name(desc),
                };
            }
        }
        let field_of = |x: i64| -> Option<usize> {
            usize::try_from(x)
                .ok()
                .and_then(|x| fmap.get(x).copied())
                .filter(|&e| e < ntot)
        };
        let axis = |name: &str| -> Vec<Ax> {
            d.table
                .child(name)
                .map(|n| {
                    n.children("field")
                        .filter_map(|f| f.attr("x")?.parse::<i64>().ok())
                        .filter_map(|x| {
                            if x == -2 {
                                Some(Ax::Values)
                            } else {
                                field_of(x).map(Ax::Field)
                            }
                        })
                        .collect()
                })
                .unwrap_or_default()
        };
        t.rows = axis("rowFields");
        t.cols = axis("colFields");
        if let Some(p) = d.table.child("pageFields") {
            t.pages = p
                .children("pageField")
                .filter_map(|f| field_of(f.attr("fld")?.parse().ok()?))
                .collect();
            for pfn in p.children("pageField") {
                let (Some(fld), Some(item)) = (
                    pfn.attr("fld")
                        .and_then(|f| f.parse::<i64>().ok())
                        .and_then(field_of),
                    pfn.attr("item").and_then(|f| f.parse::<u32>().ok()),
                ) else {
                    continue;
                };
                if (item as usize) < gcache.fields[fld].items.len() {
                    t.fields[fld].page_item = Some(item);
                }
            }
        }
        if let Some(df) = d.table.child("dataFields") {
            for x in df.children("dataField") {
                let Some(src) = x
                    .attr("fld")
                    .and_then(|f| f.parse::<i64>().ok())
                    .and_then(field_of)
                    .filter(|&s| s < nfields)
                else {
                    continue;
                };
                let base_field = x
                    .attr("baseField")
                    .and_then(|f| f.parse::<i64>().ok())
                    .and_then(field_of);
                t.data.push(DataField {
                    src,
                    caption: x.attr("name").unwrap_or("").to_string(),
                    func: func_of(x.attr("subtotal")),
                    numfmt: None,
                    show: {
                        let name = x.attr("showDataAs");
                        let kind = if name == Some("percentOfParent") {
                            12
                        } else {
                            name.and_then(|n| SHOW.iter().find(|s| s.1 == n))
                                .map_or(-4143, |s| s.0)
                        };
                        xlcalc::pivot::ShowAs {
                            kind,
                            base_field: (kind != -4143).then_some(base_field).flatten(),
                            base_item: (kind != -4143)
                                .then(|| x.attr("baseItem")?.parse::<usize>().ok())
                                .flatten()
                                .zip(base_field)
                                .and_then(|(p, bf)| display_order(&t, &gcache, bf).get(p).copied()),
                        }
                    },
                });
            }
        }
        t.form = if d.table.attr("compact") != Some("0") {
            Form::Compact
        } else if d.table.attr("outline") == Some("1") {
            Form::Outline
        } else {
            Form::Tabular
        };
        t.row_caption = d.table.attr("rowHeaderCaption").map(str::to_string);
        t.col_caption = d.table.attr("colHeaderCaption").map(str::to_string);
        t.row_grand = d.table.attr("rowGrandTotals") != Some("0");
        t.col_grand = d.table.attr("colGrandTotals") != Some("0");
        t.opts.display_error = matches!(d.table.attr("showError"), Some("1" | "true"));
        t.opts.error_string = d.table.attr("errorCaption").unwrap_or("").to_string();
        t.opts.display_null = !matches!(d.table.attr("showMissing"), Some("0" | "false"));
        t.opts.null_string = d.table.attr("missingCaption").unwrap_or("").to_string();
        if let Some(si) = d.table.child("pivotTableStyleInfo") {
            t.style = si.attr("name").unwrap_or("").to_string();
            let flag = |k: &str, dflt: bool| si.attr(k).map_or(dflt, |v| v == "1" || v == "true");
            t.style_flags = [
                flag("showColHeaders", false),
                flag("showRowHeaders", false),
                flag("showColStripes", false),
                flag("showRowStripes", false),
                flag("showLastColumn", false),
            ];
        }
        wb.pivots_mut().tables.push(t);
        let ti = wb.pivots().tables.len() - 1;
        wb.pivot_place(ti);
    }
}

fn value_xml(v: &Value) -> String {
    match v {
        Value::Number(n) => format!("<n v=\"{}\"/>", number_text(*n)),
        Value::Bool(b) => format!("<b v=\"{}\"/>", u8::from(*b)),
        Value::Error(e) => format!("<e v=\"{}\"/>", escape_attr(e.as_str())),
        Value::Blank => "<m/>".to_string(),
        Value::Text(t) => format!("<s v=\"{}\"/>", escape_attr(t.as_ref())),
        _ => "<m/>".to_string(),
    }
}

fn cache_parts(
    wb: &Workbook,
    c: &PivotCache,
    on_axis: &[bool],
    dates: &[bool],
    calc: &[Vec<String>],
    groups: &[String],
) -> (String, String) {
    let mut fields = String::new();

    let inline: Vec<bool> = (0..c.fields.len())
        .map(|f| !on_axis[f] && !dates[f])
        .collect();
    for (f, cf) in c.fields.iter().enumerate() {
        if let Some(fm) = &cf.formula {
            fields.push_str(&format!(
                "<cacheField name=\"{}\" numFmtId=\"0\" formula=\"{}\" databaseField=\"0\"/>",
                escape_attr(&cf.name),
                escape_attr(fm)
            ));
            continue;
        }
        let vals: Vec<&Value> = c.records.iter().map(|r| &r[f]).collect();
        let nums: Vec<f64> = vals
            .iter()
            .filter_map(|v| {
                if let Value::Number(n) = v {
                    Some(*n)
                } else {
                    None
                }
            })
            .collect();
        let all_num = !vals.is_empty() && nums.len() == vals.len();
        let any_blank = vals.iter().any(|v| matches!(v, Value::Blank));
        let any_text = vals.iter().any(|v| matches!(v, Value::Text(_)));
        let other = vals
            .iter()
            .any(|v| matches!(v, Value::Bool(_) | Value::Error(_)));
        let any_num = !nums.is_empty();
        let mut attrs = String::new();
        if dates[f] && all_num {
            let mn = nums.iter().cloned().fold(f64::INFINITY, f64::min);
            let mx = nums.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
            attrs.push_str(&format!(
                " containsSemiMixedTypes=\"0\" containsNonDate=\"0\" containsDate=\"1\" containsString=\"0\" minDate=\"{}\" maxDate=\"{}\"",
                crate::slicers_xml::iso(mn),
                crate::slicers_xml::iso(mx + 1.0)
            ));
        } else if all_num || (any_num && !any_text && !other) {
            if !any_blank {
                attrs.push_str(" containsSemiMixedTypes=\"0\"");
            }
            attrs.push_str(" containsString=\"0\"");
            if any_blank {
                attrs.push_str(" containsBlank=\"1\"");
            }
            attrs.push_str(" containsNumber=\"1\"");
            if nums.iter().all(|n| n.fract() == 0.0 && n.abs() < 1e15) {
                attrs.push_str(" containsInteger=\"1\"");
            }
            let mn = nums.iter().cloned().fold(f64::INFINITY, f64::min);
            let mx = nums.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
            attrs.push_str(&format!(
                " minValue=\"{}\" maxValue=\"{}\"",
                number_text(mn),
                number_text(mx)
            ));
        } else if any_num || other {
            attrs.push_str(" containsMixedTypes=\"1\"");
            if any_blank {
                attrs.push_str(" containsBlank=\"1\"");
            }
            if any_num {
                attrs.push_str(" containsNumber=\"1\"");

                if nums.iter().all(|n| n.fract() == 0.0 && n.abs() < 1e15) {
                    attrs.push_str(" containsInteger=\"1\"");
                }
                let mn = nums.iter().cloned().fold(f64::INFINITY, f64::min);
                let mx = nums.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
                attrs.push_str(&format!(
                    " minValue=\"{}\" maxValue=\"{}\"",
                    number_text(mn),
                    number_text(mx)
                ));
            }
        } else if any_blank {
            attrs.push_str(" containsBlank=\"1\"");
        }
        let body = if inline[f] && !other && (all_num || (!any_blank && !any_num)) {
            String::new()
        } else {
            cf.items
                .iter()
                .map(|i| match i.value {
                    Value::Number(n) if dates[f] => {
                        format!("<d v=\"{}\"/>", crate::slicers_xml::iso(n))
                    }
                    _ => value_xml(&i.value),
                })
                .collect::<String>()
        };
        let body = format!(
            "{body}{}",
            calc[f]
                .iter()
                .map(|n| format!("<s v=\"{}\" f=\"1\"/>", escape_attr(n)))
                .collect::<String>()
        );
        let shared = if body.is_empty() {
            format!("<sharedItems{attrs}/>")
        } else {
            format!(
                "<sharedItems{attrs} count=\"{}\">{body}</sharedItems>",
                cf.items.len() + calc[f].len()
            )
        };
        fields.push_str(&format!(
            "<cacheField name=\"{}\" numFmtId=\"{}\">{shared}{}</cacheField>",
            escape_attr(&cf.name),
            if dates[f] {
                14
            } else {
                crate::styles::builtin_id(wb.number_format(xlcalc::CellAddr::new(
                    c.sheet,
                    c.rect.r0 + 1,
                    c.rect.c0 + f as u32,
                )))
            },
            groups[f]
        ));
    }
    let mut recs = String::new();
    for (ri, r) in c.records.iter().enumerate() {
        recs.push_str("<r>");
        for (f, v) in r.iter().enumerate() {
            if c.fields[f].formula.is_some() {
                continue;
            }
            let vals_inline = {
                let vs: Vec<&Value> = c.records.iter().map(|r| &r[f]).collect();
                let all_num = !vs.is_empty() && vs.iter().all(|v| matches!(v, Value::Number(_)));
                let plain_text = vs.iter().all(|v| matches!(v, Value::Text(_)));
                inline[f] && (all_num || plain_text)
            };
            if vals_inline {
                recs.push_str(&value_xml(v));
            } else {
                recs.push_str(&format!("<x v=\"{}\"/>", c.idx[ri][f]));
            }
        }
        recs.push_str("</r>");
    }
    (fields, recs)
}

fn x_xml(v: u32) -> String {
    if v == 0 {
        "<x/>".to_string()
    } else {
        format!("<x v=\"{v}\"/>")
    }
}

fn items_xml(tag: &str, items: &[AxItem], empty_one: bool) -> String {
    let body: String = if items.is_empty() && empty_one {
        "<i/>".to_string()
    } else {
        items
            .iter()
            .map(|it| {
                let mut a = String::new();
                match it.kind {
                    1 => a.push_str(" t=\"default\""),
                    2 => a.push_str(" t=\"grand\""),
                    k @ 12..=22 => a.push_str(&format!(" t=\"{}\"", CUSTOM[k as usize - 12].0)),
                    _ => {}
                }
                if it.r > 0 {
                    a.push_str(&format!(" r=\"{}\"", it.r));
                }
                if let Some(i) = it.i {
                    a.push_str(&format!(" i=\"{i}\""));
                }
                format!(
                    "<i{a}>{}</i>",
                    it.xs.iter().map(|&x| x_xml(x)).collect::<String>()
                )
            })
            .collect()
    };
    let n = if items.is_empty() && empty_one {
        1
    } else {
        items.len()
    };
    format!("<{tag} count=\"{n}\">{body}</{tag}>")
}

fn item_ref(field: &str, name: &str) -> String {
    let plain = name
        .chars()
        .next()
        .is_some_and(|c| c.is_alphabetic() || c == '_')
        && name.chars().all(|c| c.is_alphanumeric() || c == '_');
    if plain {
        format!("{field}[{name}]")
    } else {
        format!("{field}['{name}']")
    }
}

fn item_formula_out(field: &str, formula: &str) -> String {
    map_idents(formula, |n| Some(item_ref(field, n))).unwrap_or_else(|| formula.to_string())
}

pub(crate) fn calc_owner(wb: &Workbook, ci: usize, f: usize) -> Option<&PivotTable> {
    wb.pivots()
        .tables
        .iter()
        .find(|t| t.cache == ci && t.fields.get(f).is_some_and(|p| !p.calc_items.is_empty()))
}

pub(crate) fn group_table(wb: &Workbook, ci: usize) -> Option<&PivotTable> {
    wb.pivots()
        .tables
        .iter()
        .find(|t| t.cache == ci && t.fields.iter().any(|f| f.group.is_some()))
}

fn group_items_xml(items: &[xlcalc::pivot::Item]) -> String {
    format!(
        "<groupItems count=\"{}\">{}</groupItems>",
        items.len(),
        items
            .iter()
            .map(|i| value_xml(&i.value))
            .collect::<String>()
    )
}

fn num_values(c: &PivotCache, f: usize) -> Vec<f64> {
    c.fields[f]
        .items
        .iter()
        .filter_map(|i| match i.value {
            Value::Number(n) => Some(n),
            _ => None,
        })
        .collect()
}

fn auto_attrs(start: bool, end: bool) -> String {
    format!(
        "{}{}",
        if start { " autoStart=\"0\"" } else { "" },
        if end { " autoEnd=\"0\"" } else { "" }
    )
}

fn num_group_xml(
    c: &PivotCache,
    gc: &PivotCache,
    f: usize,
    g: (Option<f64>, Option<f64>, f64),
) -> String {
    let nums = num_values(c, f);
    let lo =
        g.0.unwrap_or_else(|| nums.iter().cloned().fold(f64::INFINITY, f64::min));
    let hi =
        g.1.unwrap_or_else(|| nums.iter().cloned().fold(f64::NEG_INFINITY, f64::max));
    format!(
        "<fieldGroup base=\"{f}\"><rangePr{} startNum=\"{}\" endNum=\"{}\" groupInterval=\"{}\"/>{}</fieldGroup>",
        auto_attrs(g.0.is_some(), g.1.is_some()),
        number_text(lo),
        number_text(hi),
        number_text(g.2),
        group_items_xml(&gc.fields[f].items)
    )
}

fn date_group_field(
    c: &PivotCache,
    gc: &PivotCache,
    idx: usize,
    src: usize,
    (start, end, unit): (Option<f64>, Option<f64>, DateUnit),
) -> String {
    let nums = num_values(c, src);
    let lo = start.unwrap_or_else(|| nums.iter().cloned().fold(f64::INFINITY, f64::min));
    let hi = end.unwrap_or_else(|| nums.iter().cloned().fold(f64::NEG_INFINITY, f64::max) + 1.0);
    let (by, step) = match unit {
        DateUnit::Days(n) => ("days", (n > 1).then(|| format!(" groupInterval=\"{n}\""))),
        DateUnit::Month => ("months", None),
        DateUnit::Quarter => ("quarters", None),
        DateUnit::Year => ("years", None),
    };
    format!(
        "<cacheField name=\"{}\" numFmtId=\"0\" databaseField=\"0\"><fieldGroup base=\"{src}\"><rangePr{} groupBy=\"{by}\" startDate=\"{}\" endDate=\"{}\"{}/>{}</fieldGroup></cacheField>",
        escape_attr(&gc.fields[idx].name),
        auto_attrs(start.is_some(), end.is_some()),
        crate::slicers_xml::iso(lo),
        crate::slicers_xml::iso(hi),
        step.unwrap_or_default(),
        group_items_xml(&gc.fields[idx].items)
    )
}

fn manual_group_field(
    c: &PivotCache,
    gc: &PivotCache,
    idx: usize,
    src: usize,
    groups: &[(String, Vec<String>)],
) -> String {
    let items = &gc.fields[idx].items;
    let map: String = c.fields[src]
        .items
        .iter()
        .map(|b| {
            let label = groups
                .iter()
                .find(|(_, m)| m.iter().any(|l| l.to_lowercase() == b.label.to_lowercase()))
                .map_or(&b.label, |g| &g.0);
            let k = items.iter().position(|i| &i.label == label).unwrap_or(0);
            format!("<x v=\"{k}\"/>")
        })
        .collect();
    format!(
        "<cacheField name=\"{}\" numFmtId=\"0\" databaseField=\"0\"><fieldGroup base=\"{src}\"><discretePr count=\"{}\">{map}</discretePr>{}</fieldGroup></cacheField>",
        escape_attr(&gc.fields[idx].name),
        c.fields[src].items.len(),
        group_items_xml(items)
    )
}

pub(crate) fn cache_xml(wb: &Workbook, ci: usize, records_rid: &str) -> (String, String) {
    let pv = wb.pivots();
    let c = &pv.caches[ci];
    let mut on_axis = vec![false; c.fields.len()];
    for t in pv.tables.iter().filter(|t| t.cache == ci) {
        for (f, pf) in t.fields.iter().enumerate().take(on_axis.len()) {
            if matches!(pf.orient, Orient::Row | Orient::Column | Orient::Page) {
                on_axis[f] = true;
            }
        }
    }

    for sc in wb.slicer_caches().iter().filter(|s| s.pivot) {
        let on = pv
            .tables
            .iter()
            .any(|t| t.cache == ci && t.name.eq_ignore_ascii_case(&sc.table));
        if let Some(f) = c
            .fields
            .iter()
            .position(|f| f.name.eq_ignore_ascii_case(&sc.field))
            .filter(|_| on)
        {
            on_axis[f] = true;
        }
    }
    let sliced = wb.slicer_caches().iter().any(|s| {
        s.pivot
            && pv
                .tables
                .iter()
                .any(|t| t.cache == ci && t.name.eq_ignore_ascii_case(&s.table))
    });
    let ext = if sliced {
        format!(
            "<extLst><ext uri=\"{{725AE2AE-9491-48be-B2B4-4EB974FC3084}}\" xmlns:x14=\"http://schemas.microsoft.com/office/spreadsheetml/2009/9/main\"><x14:pivotCacheDefinition pivotCacheId=\"{}\"/></ext></extLst>",
            ci + 1
        )
    } else {
        String::new()
    };

    let mut dates: Vec<bool> = c
        .fields
        .iter()
        .map(|f| {
            wb.slicer_caches().iter().any(|s| {
                s.timeline.is_some()
                    && s.field.eq_ignore_ascii_case(&f.name)
                    && pv
                        .tables
                        .iter()
                        .any(|t| t.cache == ci && t.name.eq_ignore_ascii_case(&s.table))
            })
        })
        .collect();

    let timeline = dates.iter().any(|d| *d);
    let n_src = c.fields.len();
    let mut calc: Vec<Vec<String>> = vec![Vec::new(); n_src];
    let mut groups: Vec<String> = vec![String::new(); n_src];
    let mut derived = String::new();
    let mut calc_xml = String::new();
    for f in 0..n_src {
        if let Some(t) = calc_owner(wb, ci, f) {
            on_axis[f] = true;
            let fname = &c.fields[f].name;
            for (j, ci_) in t.fields[f].calc_items.iter().enumerate() {
                calc[f].push(ci_.name.clone());
                calc_xml.push_str(&format!(
                    "<calculatedItem formula=\"{}\"><pivotArea cacheIndex=\"1\" outline=\"0\" fieldPosition=\"0\"><references count=\"1\"><reference field=\"{f}\" count=\"1\"><x v=\"{}\"/></reference></references></pivotArea></calculatedItem>",
                    escape_attr(&item_formula_out(fname, &ci_.formula)),
                    c.fields[f].items.len() + j
                ));
            }
        }
    }
    let mut derived_n = 0;
    if let Some(t) = group_table(wb, ci) {
        let gc = c.grouped(t);
        let mut par: Vec<Option<usize>> = vec![None; n_src];
        for (f, pf) in t.fields.iter().enumerate() {
            match &pf.group {
                Some(Group::Num { start, end, by }) if f < n_src => {
                    on_axis[f] = true;
                    groups[f] = num_group_xml(c, &gc, f, (*start, *end, *by));
                }
                Some(Group::Derived {
                    src,
                    start,
                    end,
                    unit,
                }) if f >= n_src => {
                    on_axis[*src] = true;
                    dates[*src] = true;
                    par[*src] = Some(f);
                    derived.push_str(&date_group_field(c, &gc, f, *src, (*start, *end, *unit)));
                    derived_n += 1;
                }
                Some(Group::Manual { src, groups: g }) if f >= n_src => {
                    on_axis[*src] = true;
                    par[*src] = Some(f);
                    derived.push_str(&manual_group_field(c, &gc, f, *src, g));
                    derived_n += 1;
                }
                _ => {}
            }
        }
        for (src, p) in par.iter().enumerate() {
            if let Some(p) = p {
                groups[src] = format!("<fieldGroup par=\"{p}\"/>");
            }
        }
    }
    let (mut fields, recs) = cache_parts(wb, c, &on_axis, &dates, &calc, &groups);
    fields.push_str(&derived);
    let calc_xml = if calc_xml.is_empty() {
        String::new()
    } else {
        format!(
            "<calculatedItems count=\"{}\">{calc_xml}</calculatedItems>",
            calc.iter().map(Vec::len).sum::<usize>()
        )
    };
    let (sheet_name, rect) = (
        wb.sheet(c.sheet)
            .map(|s| s.name.clone())
            .unwrap_or_default(),
        c.rect,
    );

    let refresh = if timeline { "" } else { " refreshOnLoad=\"1\"" };
    let def = format!(
        "<pivotCacheDefinition xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" r:id=\"{records_rid}\"{refresh} refreshedBy=\"xlcalc\" refreshedDate=\"0\" createdVersion=\"8\" refreshedVersion=\"8\" minRefreshableVersion=\"3\" recordCount=\"{}\"><cacheSource type=\"worksheet\"><worksheetSource ref=\"{}\" sheet=\"{}\"/></cacheSource><cacheFields count=\"{}\">{fields}</cacheFields>{calc_xml}{ext}</pivotCacheDefinition>",
        c.records.len(),
        crate::write::rect_text(&rect),
        escape_attr(&sheet_name),
        n_src + derived_n,
    );
    let records = format!(
        "<pivotCacheRecords xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" count=\"{}\">{recs}</pivotCacheRecords>",
        c.records.len()
    );
    (def, records)
}

pub(crate) fn table_xml(wb: &Workbook, ti: usize, cache_id: u32) -> String {
    let pv = wb.pivots();
    let t = &pv.tables[ti];
    let r = render(t, &pv.caches[t.cache]);
    let grouped = pv.caches[t.cache].grouped(t);
    let c = &grouped;
    let n_src = pv.caches[t.cache].fields.len();
    let extra = group_table(wb, t.cache).map_or(0, |g| g.fields.len().saturating_sub(n_src));
    let t1 = t
        .ranges
        .table1
        .unwrap_or(Rect::new(t.row0, t.col0, t.row0, t.col0));
    let (outline, compact) = match t.form {
        Form::Compact => (" outline=\"1\" outlineData=\"1\"", ""),
        Form::Outline => (
            " compact=\"0\" outline=\"1\" outlineData=\"1\" compactData=\"0\"",
            " compact=\"0\"",
        ),
        Form::Tabular => (
            " compact=\"0\" compactData=\"0\"",
            " compact=\"0\" outline=\"0\"",
        ),
    };
    let first_header = u32::from(r.real_cols || (t.cols.is_empty() && t.data.len() <= 1));
    let page_attr = if t.pages.is_empty() {
        String::new()
    } else {
        format!(" rowPageCount=\"{}\" colPageCount=\"1\"", t.pages.len())
    };
    let mut s = format!(
        "<pivotTableDefinition xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" name=\"{}\"{}{} cacheId=\"{cache_id}\" applyNumberFormats=\"0\" applyBorderFormats=\"0\" applyFontFormats=\"0\" applyPatternFormats=\"0\" applyAlignmentFormats=\"0\" applyWidthHeightFormats=\"1\" dataCaption=\"Values\" updatedVersion=\"8\" minRefreshableVersion=\"3\" useAutoFormatting=\"1\" itemPrintTitles=\"1\" createdVersion=\"8\" indent=\"0\"{outline}{}{}{} multipleFieldFilters=\"0\">",
        escape_attr(&t.name),
        t.row_caption
            .as_ref()
            .map_or(String::new(), |x| format!(" rowHeaderCaption=\"{}\"", escape_attr(x))),
        t.col_caption
            .as_ref()
            .map_or(String::new(), |x| format!(" colHeaderCaption=\"{}\"", escape_attr(x))),
        if t.row_grand { "" } else { " rowGrandTotals=\"0\"" },
        if t.col_grand { "" } else { " colGrandTotals=\"0\"" },
        option_attrs(&t.opts),
    );
    s.push_str(&format!(
        "<location ref=\"{}\" firstHeaderRow=\"{first_header}\" firstDataRow=\"{}\" firstDataCol=\"{}\"{page_attr}/>",
        crate::write::rect_text(&t1),
        r.hdr_rows.max(1),
        r.left_cols
    ));
    s.push_str(&format!(
        "<pivotFields count=\"{}\">",
        if t.fields.len() > n_src {
            t.fields.len()
        } else {
            n_src + extra
        }
    ));
    let group_srcs: Vec<usize> = group_table(wb, t.cache).map_or_else(Vec::new, |g| {
        g.fields
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match &x.group {
                Some(Group::Derived { src, .. } | Group::Manual { src, .. }) => Some(*src),
                Some(Group::Num { .. }) => Some(i),
                None => None,
            })
            .collect()
    });
    let is_data = |f: usize| t.data.iter().any(|d| d.src == f);
    for (f, pf) in t.fields.iter().enumerate() {
        let axis = match pf.orient {
            Orient::Row => " axis=\"axisRow\"",
            Orient::Column => " axis=\"axisCol\"",
            Orient::Page => " axis=\"axisPage\"",
            _ => "",
        };
        let on_axis = !axis.is_empty();
        let data = if is_data(f) { " dataField=\"1\"" } else { "" };
        let sub = if pf.subtotals {
            ""
        } else {
            " defaultSubtotal=\"0\""
        };
        let mut items = String::new();
        let mut extra = String::new();
        if pf.blank_line {
            extra.push_str(" insertBlankRow=\"1\"");
        }
        for &ix in &pf.custom {
            extra.push_str(&format!(" {}=\"1\"", CUSTOM[ix as usize - 2].1));
        }
        match pf.sort {
            Sort::Manual => {}
            Sort::Name(desc) | Sort::Data(desc, _) => {
                extra.push_str(if desc {
                    " sortType=\"descending\""
                } else {
                    " sortType=\"ascending\""
                });
            }
        }
        let mut tail = String::new();
        if let Sort::Data(_, d) = pf.sort {
            tail = format!("<autoSortScope><pivotArea dataOnly=\"0\" outline=\"0\" fieldPosition=\"0\"><references count=\"1\"><reference field=\"4294967294\" count=\"1\" selected=\"0\"><x v=\"{d}\"/></reference></references></pivotArea></autoSortScope>");
        }
        if pf.repeat {
            extra.push_str(" fillDownLabels=\"1\"");
        }
        if pf.multi_page {
            extra.push_str(" multipleItemSelectionAllowed=\"1\"");
        }
        let grouped_src = group_srcs.contains(&f);
        let owns_calc = calc_owner(wb, t.cache, f).is_some_and(|o| std::ptr::eq(o, t));
        if on_axis || grouped_src || owns_calc {
            let mut order = display_order(t, c, f);
            if !owns_calc {
                order.retain(|o| (*o as usize) < c.fields[f].items.len());
            }
            let mut n = order.len();
            for o in &order {
                let calc = if (*o as usize) >= c.fields[f].items.len() {
                    " f=\"1\""
                } else {
                    ""
                };
                let h = if pf.hidden.contains(&(*o as usize)) {
                    " h=\"1\""
                } else {
                    ""
                };
                let nm = match pf.item_caps.iter().find(|x| x.0 == *o) {
                    Some((_, cap)) => format!(" n=\"{}\"", escape_attr(cap)),
                    None => String::new(),
                };
                items.push_str(&format!("<item{nm}{calc}{h} x=\"{o}\"/>"));
            }
            if pf.subtotals {
                items.push_str("<item t=\"default\"/>");
                n += 1;
            }
            for &ix in &pf.custom {
                items.push_str(&format!("<item t=\"{}\"/>", CUSTOM[ix as usize - 2].0));
                n += 1;
            }
            items = format!("<items count=\"{n}\">{items}</items>");
        }
        if c.fields[f].formula.is_some() {
            s.push_str(&format!("<pivotField{data} dragToRow=\"0\" dragToCol=\"0\" dragToPage=\"0\" showAll=\"0\" defaultSubtotal=\"0\"/>"));
            continue;
        }
        let cap = match &pf.caption {
            Some(x) => format!(" name=\"{}\"", escape_attr(x)),
            None => String::new(),
        };
        let head = format!("<pivotField{cap}{axis}{data}{compact} showAll=\"0\"{sub}{extra}");
        if items.is_empty() {
            s.push_str(&format!("{head}/>"));
        } else {
            s.push_str(&format!("{head}>{items}{tail}</pivotField>"));
        }
    }
    for _ in t.fields.len()..n_src + extra {
        s.push_str("<pivotField showAll=\"0\"/>");
    }
    s.push_str("</pivotFields>");
    let axis_fields = |tag: &str, ax: &[Ax]| -> String {
        if ax.is_empty() {
            return String::new();
        }
        let body: String = ax
            .iter()
            .map(|a| match a {
                Ax::Field(f) => format!("<field x=\"{f}\"/>"),
                Ax::Values => "<field x=\"-2\"/>".to_string(),
            })
            .collect();
        format!("<{tag} count=\"{}\">{body}</{tag}>", ax.len())
    };
    let (rax, cax) = t.axes();
    s.push_str(&axis_fields("rowFields", &rax));
    s.push_str(&items_xml("rowItems", &r.row_items, true));
    s.push_str(&axis_fields("colFields", &cax));
    s.push_str(&items_xml("colItems", &r.col_items, true));
    if !t.pages.is_empty() {
        s.push_str(&format!("<pageFields count=\"{}\">", t.pages.len()));
        for f in &t.pages {
            let item = t.fields[*f]
                .page_item
                .and_then(|p| display_order(t, c, *f).iter().position(|&o| o == p))
                .map(|p| format!(" item=\"{p}\""))
                .unwrap_or_default();
            s.push_str(&format!("<pageField fld=\"{f}\"{item} hier=\"-1\"/>"));
        }
        s.push_str("</pageFields>");
    }
    if !t.data.is_empty() {
        s.push_str(&format!("<dataFields count=\"{}\">", t.data.len()));
        for d in &t.data {
            let sub = func_name(d.func)
                .map(|n| format!(" subtotal=\"{n}\""))
                .unwrap_or_default();
            let mut show = String::new();
            if d.show.kind == 12 {
                show = " showDataAs=\"percentOfParent\"".to_string();
            } else if let Some(n) = SHOW.iter().find(|x| x.0 == d.show.kind).map(|x| x.1) {
                show = format!(" showDataAs=\"{n}\"");
            }
            let bf = d.show.base_field.unwrap_or(0);
            let bi = d
                .show
                .base_item
                .and_then(|p| display_order(t, c, bf).iter().position(|&o| o == p))
                .unwrap_or(0);
            s.push_str(&format!(
                "<dataField name=\"{}\" fld=\"{}\"{sub}{show} baseField=\"{bf}\" baseItem=\"{bi}\"/>",
                escape_attr(&d.caption),
                d.src
            ));
        }
        s.push_str("</dataFields>");
    }
    let fl = |i: usize| u8::from(t.style_flags[i]);
    s.push_str(&format!(
        "<pivotTableStyleInfo name=\"{}\" showRowHeaders=\"{}\" showColHeaders=\"{}\" showRowStripes=\"{}\" showColStripes=\"{}\" showLastColumn=\"{}\"/></pivotTableDefinition>",
        escape_attr(&t.style),
        fl(1),
        fl(0),
        fl(3),
        fl(2),
        fl(4)
    ));

    let _ = escape_text;
    s
}

fn value_label(v: &Value) -> String {
    match v {
        Value::Number(n) => xlcalc::pivot::number_label(*n),
        Value::Text(t) => t.to_string(),
        Value::Bool(b) => if *b { "TRUE" } else { "FALSE" }.to_string(),
        _ => String::new(),
    }
}

fn item_formula_in(formula: &str) -> String {
    let ch: Vec<char> = formula.chars().collect();
    let mut out = String::new();
    let mut i = 0;
    while i < ch.len() {
        if ch[i] != '[' {
            out.push(ch[i]);
            i += 1;
            continue;
        }

        let mut k = out.len();
        if out.ends_with('\'') {
            k -= 1;
            while k > 0 && !out[..k].ends_with('\'') {
                k -= out[..k].chars().next_back().map_or(1, char::len_utf8);
            }
            k = k.saturating_sub(1);
        } else {
            while let Some(c) = out[..k].chars().next_back() {
                if c.is_alphanumeric() || c == '_' || c == '.' {
                    k -= c.len_utf8();
                } else {
                    break;
                }
            }
        }
        out.truncate(k);
        let mut j = i + 1;
        let mut name = String::new();
        let mut quoted = false;
        while j < ch.len() && (ch[j] != ']' || quoted) {
            if ch[j] == '\'' {
                quoted = !quoted;
            } else {
                name.push(ch[j]);
            }
            j += 1;
        }
        let plain = name
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
            && name.chars().all(|c| c.is_alphanumeric() || c == '_');
        if plain {
            out.push_str(&name);
        } else {
            out.push_str(&format!("'{name}'"));
        }
        i = j + 1;
    }
    out
}

fn option_attrs(o: &xlcalc::pivot::Opts) -> String {
    let mut s = String::new();
    if o.display_error {
        s.push_str(" showError=\"1\"");
    }
    if !o.error_string.is_empty() {
        s.push_str(&format!(
            " errorCaption=\"{}\"",
            escape_attr(&o.error_string)
        ));
    }
    if !o.display_null {
        s.push_str(" showMissing=\"0\"");
    }
    if !o.null_string.is_empty() {
        s.push_str(&format!(
            " missingCaption=\"{}\"",
            escape_attr(&o.null_string)
        ));
    }
    s
}
