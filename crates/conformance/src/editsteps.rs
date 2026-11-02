use serde_json::Value as J;
use xlcalc::{parse_cell_ref, Workbook};

use super::{Outcome, Tally};

pub fn run(wb: &mut Workbook, op: &str, step: &J, rec: &J, n: usize, tally: &mut Tally) -> bool {
    let excel_err = !rec["error"].is_null();
    let mut verdict = |tally: &mut Tally, err: Option<String>| {
        if let Some(w) = err.as_deref().and_then(super::oos_what) {
            tally.pending_oos = Some(w);
            return;
        }
        let ok = err.is_some() == excel_err;
        tally.add(if ok { Outcome::Exact } else { Outcome::Wrong }, || {
            format!(
                "step {n}: {op} — Excel error {:?}, engine {err:?}",
                rec["error"]
            )
        });
    };
    match op {
        "group_rows" | "ungroup_rows" | "outline_show" => {
            let Some(sid) = step["sheet"]
                .as_str()
                .and_then(|n| wb.sheet_by_name(n.trim_matches('\'')))
                .map(|s| s.id)
            else {
                verdict(tally, Some("no such sheet".into()));
                return true;
            };
            let at = step["at"].as_u64().unwrap_or(1) as u32 - 1;
            let count = step["count"].as_u64().unwrap_or(1) as u32;
            let r = match op {
                "group_rows" => wb.group_rows(sid, at, count, true).map(|_| ()),
                "ungroup_rows" => wb.group_rows(sid, at, count, false).map(|_| ()),
                _ => {
                    wb.outline_show(sid, step["level"].as_u64().unwrap_or(1) as u32);
                    Ok(())
                }
            };
            verdict(tally, r.err().map(|e| e.to_string()));
        }
        _ => {
            let _ = (parse_cell_ref, &mut verdict);
            return false;
        }
    }
    true
}
