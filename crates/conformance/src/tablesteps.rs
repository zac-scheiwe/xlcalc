use serde_json::Value as J;
use xlcalc::{parse_range_ref, SheetId, Workbook};
use xlcalc_om::{flags, OmError};
use xlcalc_variant::{HResult, ObjectId, Variant};

use super::omsteps::{self, OmSession};
use super::{oos_what, Outcome, Tally};

fn message(e: &OmError) -> String {
    e.detail.clone().unwrap_or_else(|| e.to_string())
}

fn obj(v: Variant) -> Result<ObjectId, OmError> {
    match v {
        Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) => Ok(id),
        _ => Err(OmError::object_required()),
    }
}

fn text(v: Variant) -> String {
    match v {
        Variant::Bstr(s) => s,
        _ => String::new(),
    }
}

fn count(s: &mut OmSession, coll: ObjectId) -> Result<i32, OmError> {
    Ok(match s.get(coll, "Count")? {
        Variant::I4(n) => n,
        _ => 0,
    })
}

fn item(s: &mut OmSession, coll: ObjectId, index: Variant) -> Result<ObjectId, OmError> {
    obj(s.call(coll, "Item", flags::METHOD | flags::GET, &[index])?)
}

fn get_obj(s: &mut OmSession, target: ObjectId, member: &str) -> Result<ObjectId, OmError> {
    obj(s.get(target, member)?)
}

fn worksheet(s: &mut OmSession, name: &str) -> Result<ObjectId, OmError> {
    let app = s.app_handle();
    s.object(app, "Worksheets", &[Variant::Bstr(name.to_string())])
}

pub(crate) fn find_table(
    s: &mut OmSession,
    name: &str,
    sheet: Option<&str>,
) -> Result<ObjectId, OmError> {
    let key = Variant::Bstr(name.to_string());
    if let Some(sh) = sheet {
        let ws = worksheet(s, sh)?;
        let los = get_obj(s, ws, "ListObjects")?;
        return item(s, los, key);
    }
    let app = s.app_handle();
    let sheets = s.object(app, "Worksheets", &[])?;
    for i in 1..=count(s, sheets)? {
        let ws = item(s, sheets, Variant::I4(i))?;
        let los = get_obj(s, ws, "ListObjects")?;
        if let Ok(lo) = item(s, los, key.clone()) {
            return Ok(lo);
        }
    }
    Err(OmError::app_defined().with_detail("no such table"))
}

fn range_of(wb: &Workbook, r: &str, default: SheetId) -> Option<(String, String)> {
    let (sheet, rect) = parse_range_ref(wb, r, default)?;
    Some((wb.sheet(sheet)?.name.clone(), rect.a1()))
}

fn list_one(s: &mut OmSession, sheet: &str, lo: ObjectId, rich: bool) -> Result<String, OmError> {
    let name = text(s.get(lo, "Name")?);
    let rng = get_obj(s, lo, "Range")?;
    let range = s.address(rng)?;
    let mut line = format!("{name} {sheet} {range}");
    if rich {
        let headers = s.get(lo, "ShowHeaders")? == Variant::Bool(true);
        let totals = s.get(lo, "ShowTotals")? == Variant::Bool(true);
        let cols = get_obj(s, lo, "ListColumns")?;
        let mut names = Vec::new();
        for k in 1..=count(s, cols)? {
            let c = item(s, cols, Variant::I4(k))?;
            names.push(text(s.get(c, "Name")?));
        }
        line.push_str(&format!(
            " h={} t={} cols={}",
            u8::from(headers),
            u8::from(totals),
            names.join("|")
        ));
    }
    Ok(line)
}

fn list_tables(s: &mut OmSession, rich: bool) -> Result<Vec<String>, OmError> {
    let app = s.app_handle();
    let sheets = s.object(app, "Worksheets", &[])?;
    let mut out = Vec::new();
    for i in 1..=count(s, sheets)? {
        let ws = item(s, sheets, Variant::I4(i))?;
        let sheet = text(s.get(ws, "Name")?);
        let los = get_obj(s, ws, "ListObjects")?;
        for j in 1..=count(s, los)? {
            let lo = item(s, los, Variant::I4(j))?;
            out.push(list_one(s, &sheet, lo, rich)?);
        }
    }
    Ok(out)
}

fn sort_table(s: &mut OmSession, lo: ObjectId, step: &J) -> Result<(), OmError> {
    let sort = get_obj(s, lo, "Sort")?;
    let fields = get_obj(s, sort, "SortFields")?;
    s.method(fields, "Clear")?;
    let cols = get_obj(s, lo, "ListColumns")?;
    for k in step["keys"].as_array().unwrap() {
        let col = item(s, cols, Variant::I4(k["key"].as_i64().unwrap() as i32))?;
        let body = get_obj(s, col, "DataBodyRange")?;
        let order = if k["order"].as_str() == Some("desc") {
            2
        } else {
            1
        };
        let args = [
            Variant::Dispatch(Some(body)),
            Variant::I4(0),
            Variant::I4(order),
        ];
        s.call(fields, "Add", flags::METHOD, &args)?;
    }
    s.put(sort, "Header", Variant::I4(1))?;
    let case = step["matchcase"].as_bool().unwrap_or(false);
    s.put(sort, "MatchCase", Variant::Bool(case))?;
    s.method(sort, "Apply")?;
    Ok(())
}

fn recorded(rec: &J) -> Vec<String> {
    let mut want: Vec<String> = rec["tables"]
        .as_array()
        .map(|a| {
            a.iter()
                .map(|x| {
                    let extra = match (x["headers"].as_bool(), x["totals"].as_bool()) {
                        (Some(h), Some(t)) => {
                            let cols: Vec<&str> = x["columns"]
                                .as_array()
                                .map(|a| a.iter().filter_map(|c| c.as_str()).collect())
                                .unwrap_or_default();
                            format!(
                                " h={} t={} cols={}",
                                u8::from(h),
                                u8::from(t),
                                cols.join("|")
                            )
                        }
                        _ => String::new(),
                    };
                    format!(
                        "{} {} {}{}",
                        x["name"].as_str().unwrap(),
                        x["sheet"].as_str().unwrap(),
                        x["range"].as_str().unwrap(),
                        extra
                    )
                })
                .collect()
        })
        .unwrap_or_default();
    want.sort();
    want
}

fn add_table(
    s: &mut OmSession,
    (sh, a1): &(String, String),
    name: &str,
    totals: bool,
) -> Result<(), OmError> {
    let ws = worksheet(s, sh)?;
    let los = get_obj(s, ws, "ListObjects")?;
    let rng = Variant::Dispatch(Some(s.range(sh, a1)?));
    let missing = Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0);
    let args = [Variant::I4(1), rng, missing, Variant::I4(1)];
    let lo = obj(s.call(los, "Add", flags::METHOD, &args)?)?;

    s.put(lo, "Name", Variant::Bstr(name.to_string()))?;
    if totals {
        let _ = s.put(lo, "ShowTotals", Variant::Bool(true));
    }
    Ok(())
}

pub struct Ctx<'a> {
    pub op: &'a str,
    pub step: &'a J,
    pub rec: &'a J,
    pub n: usize,
    pub default_sheet: SheetId,
}

pub fn run(om: &mut Option<OmSession>, wb: &mut Workbook, cx: &Ctx, tally: &mut Tally) -> bool {
    if !matches!(
        cx.op,
        "table"
            | "table_totals"
            | "table_unlist"
            | "table_delete"
            | "table_resize"
            | "table_rename"
            | "table_headers"
            | "table_sort"
            | "tables"
    ) {
        return false;
    }
    let (op, step, rec, n) = (cx.op, cx.step, cx.rec, cx.n);
    let excel_err = !rec["error"].is_null();
    let name = step["name"].as_str().unwrap_or("");
    let sheet = step["sheet"].as_str();
    let session = om.get_or_insert_with(|| OmSession::new(wb));
    match op {
        "tables" => {
            let rich = rec["tables"]
                .as_array()
                .is_some_and(|a| a.iter().any(|x| x["headers"].is_boolean()));
            let mut got = session
                .with(wb, |s| list_tables(s, rich))
                .unwrap_or_default();
            got.sort();
            let want = recorded(rec);
            tally.add(
                if got == want {
                    Outcome::Exact
                } else {
                    Outcome::Wrong
                },
                || format!("step {n}: tables expected {want:?}, got {got:?}"),
            );
        }
        "table" => {
            let at = range_of(wb, step["range"].as_str().unwrap(), cx.default_sheet).unwrap();
            let totals = step["totals"].as_bool().unwrap_or(false);
            let r = session.with(wb, |s| add_table(s, &at, name, totals));
            if let Err(e) = r {
                let msg = message(&e);
                if let Some(w) = oos_what(&msg) {
                    tally.pending_oos = Some(w);
                } else if excel_err {
                    tally.add(Outcome::Exact, String::new);
                } else {
                    tally.add(Outcome::Wrong, || format!("step {n}: table failed: {msg}"));
                }
            }
        }
        "table_sort" => {
            let r = session.with(wb, |s| {
                let lo = find_table(s, name, sheet).map_err(|_| "no such table".to_string())?;
                sort_table(s, lo, step).map_err(|e| message(&e))
            });
            omsteps::verdict(tally, op, rec, n, r.err());
        }
        _ => {
            let to = step["to"].as_str().unwrap_or("");
            let value = step["value"].as_bool().unwrap_or(false);
            let resize = (op == "table_resize")
                .then(|| range_of(wb, step["range"].as_str().unwrap(), cx.default_sheet))
                .flatten();
            let lookup_sheet = if op == "table_totals" { sheet } else { None };
            let r = session.with(wb, |s| {
                let lo = find_table(s, name, lookup_sheet)?;
                match op {
                    "table_totals" => s.put(lo, "ShowTotals", Variant::Bool(value)),
                    "table_headers" => s.put(lo, "ShowHeaders", Variant::Bool(value)),
                    "table_rename" => s.put(lo, "Name", Variant::Bstr(to.to_string())),
                    "table_unlist" => s.method(lo, "Unlist").map(|_| ()),
                    "table_delete" => s.method(lo, "Delete").map(|_| ()),
                    _ => {
                        let (sh, a1) = resize.clone().ok_or_else(OmError::app_defined)?;
                        let rng = Variant::Dispatch(Some(s.range(&sh, &a1)?));
                        s.call(lo, "Resize", flags::METHOD, &[rng]).map(|_| ())
                    }
                }
            });
            let err = r.err().map(|e| message(&e));
            if let Some(w) = err.as_deref().and_then(oos_what) {
                tally.pending_oos = Some(w);
            } else if err.is_some() != excel_err {
                tally.add(Outcome::Wrong, || {
                    format!(
                        "step {n}: {op} — Excel error {:?}, engine {err:?}",
                        rec["error"]
                    )
                });
            }
        }
    }
    true
}
