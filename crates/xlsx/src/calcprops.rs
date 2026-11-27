use crate::rawxml::{self, Tag};
use crate::xml::{self, Node};
use xlcalc::parser::ast::{Expr, RefKind};

pub(crate) const ENGINE_CALC_ID: u32 = 191_029;

const OLDEST_KEPT_ID: u32 = 191_028;

pub(crate) const FEATURES: [&str; 7] = [
    "microsoft.com:RD",
    "microsoft.com:Single",
    "microsoft.com:FV",
    "microsoft.com:CNMTM",
    "microsoft.com:LET_WF",
    "microsoft.com:LAMBDA_WF",
    "microsoft.com:ARRAYTEXT_WF",
];

const REQUIRED_FEATURES: usize = 4;

pub(crate) const URI_FEATURES: &str = "{B58B0392-4F1F-4190-BB64-5DF3571DCE5F}";

const EDITED: u32 = 7;

const RUP_BUILD: &str = "30625";

pub(crate) fn features(book: &Node) -> impl Iterator<Item = &str> {
    book.child("extLst")
        .into_iter()
        .flat_map(|l| l.children("ext"))
        .filter_map(|e| e.child("calcFeatures"))
        .flat_map(|f| f.children("feature"))
        .filter_map(|f| f.attr("name"))
}

fn excel_version(book: &Node) -> Option<&Node> {
    book.child("fileVersion")
        .filter(|v| v.attr("appName") == Some("xl"))
}

fn number(node: Option<&Node>, key: &str) -> u32 {
    node.and_then(|n| n.attr(key))
        .and_then(|s| s.parse().ok())
        .unwrap_or(0)
}

fn retained_id(book: &Node) -> u32 {
    let version = excel_version(book);
    let last = number(version, "lastEdited");
    if last == 0 || (last == 1 && number(version, "rupBuild") < 1916) {
        0
    } else {
        number(book.child("calcPr"), "calcId")
    }
}

fn asks_full_calc(book: &Node, key: &str) -> bool {
    let calc = book.child("calcPr");
    matches!(calc.and_then(|c| c.attr(key)), Some("1" | "true"))
}

pub(crate) fn load_decision(book: &Node, from_xls: bool) -> (bool, u32) {
    if from_xls {
        let id = number(book.child("calcPr"), "calcId");
        let kept = id >= ENGINE_CALC_ID && !asks_full_calc(book, "fullCalcOnLoad");
        (kept, id)
    } else {
        (!needs_calc(book), retained_id(book))
    }
}

pub(crate) fn needs_calc(book: &Node) -> bool {
    let id = retained_id(book);
    (id & !1) < OLDEST_KEPT_ID
        || FEATURES[..REQUIRED_FEATURES]
            .iter()
            .any(|&f| !features(book).any(|g| g == f))
        || asks_full_calc(book, "fullCalcOnLoad")
        || asks_full_calc(book, "forceFullCalc")
}

pub(crate) fn file_version(source: &Node, raw: Option<&str>) -> Result<String, String> {
    let mut tag = match raw {
        Some(r) => rawxml::split(r.as_bytes())?.root,
        None => Tag::new("fileVersion"),
    };
    let lowest = match number(excel_version(source), "lowestEdited") {
        n @ 1..EDITED => n,
        _ => EDITED,
    };
    tag.set("appName", Some("xl"))
        .set("lastEdited", Some(&EDITED.to_string()))
        .set("lowestEdited", Some(&lowest.to_string()))
        .set("rupBuild", Some(RUP_BUILD));
    Ok(tag.empty())
}

pub(crate) fn features_ext(source: &Node) -> String {
    let others = features(source).filter(|f| !FEATURES.contains(f));
    let list: String = FEATURES
        .into_iter()
        .chain(others)
        .map(|f| format!("<xcalcf:feature name=\"{}\"/>", crate::xlsb::esc(f)))
        .collect();
    format!(
        "<ext uri=\"{URI_FEATURES}\" \
         xmlns:xcalcf=\"http://schemas.microsoft.com/office/spreadsheetml/2018/calcfeatures\">\
         <xcalcf:calcFeatures>{list}</xcalcf:calcFeatures></ext>"
    )
}

pub(crate) fn compat_full_calc(names: &str) -> Result<bool, String> {
    let root = xml::parse(names.as_bytes())?;
    let asks = root
        .children("definedName")
        .filter_map(|n| xlcalc::parser::parse(&n.text).ok())
        .any(|e| changes_with_grid(&e));
    Ok(asks)
}

fn changes_with_grid(e: &Expr) -> bool {
    match e {
        Expr::Ref { kind, .. } => match *kind {
            RefKind::Cell { .. } => false,
            RefKind::Cols { .. } | RefKind::Rows { .. } => true,
            RefKind::Area { r0, c0, r1, c1 } => {
                let whole = |a: u32, b: u32, size: u32| {
                    let (a, b) = (a.min(b), a.max(b));
                    b - a >= size || (a == 0 && b == size - 1)
                };
                whole(r0.index, r1.index, 0x1_0000) || whole(c0.index, c1.index, 0x100)
            }
        },
        Expr::Func { name, args } => {
            name.eq_ignore_ascii_case("ADDRESS") || args.iter().any(changes_with_grid)
        }
        Expr::Call { callee, args } => {
            changes_with_grid(callee) || args.iter().any(changes_with_grid)
        }
        Expr::Unary { expr, .. }
        | Expr::Percent(expr)
        | Expr::Spill(expr)
        | Expr::Field { expr, .. }
        | Expr::Paren(expr)
        | Expr::Ws { expr, .. } => changes_with_grid(expr),
        Expr::Binary { left, right, .. } => changes_with_grid(left) || changes_with_grid(right),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn book(version: &str, calc: &str, features: &[&str]) -> Node {
        let list: String = features
            .iter()
            .map(|f| format!("<feature name=\"microsoft.com:{f}\"/>"))
            .collect();
        let text = format!(
            "<workbook>{version}<calcPr {calc}/><extLst><ext><calcFeatures>{list}</calcFeatures>\
             </ext></extLst></workbook>"
        );
        xml::parse(text.as_bytes()).unwrap()
    }

    #[test]
    fn load_decision() {
        let xl =
            "<fileVersion appName=\"xl\" lastEdited=\"7\" lowestEdited=\"7\" rupBuild=\"30527\"/>";
        let four = ["RD", "Single", "FV", "CNMTM"];
        let now = "calcId=\"191029\"";
        assert!(!needs_calc(&book(xl, now, &four)));
        assert!(!needs_calc(&book(xl, "calcId=\"191028\"", &four)));
        assert!(!needs_calc(&book(xl, "calcId=\"999999\"", &four)));
        assert!(!needs_calc(&book(
            "<fileVersion appName=\"xl\" lastEdited=\"4\"/>",
            now,
            &four
        )));
        assert!(needs_calc(&book(xl, "calcId=\"191027\"", &four)));
        assert!(needs_calc(&book(xl, "", &four)));
        assert!(needs_calc(&book(xl, now, &[])));
        assert!(needs_calc(&book(xl, now, &four[..3])));
        assert!(needs_calc(&book("", now, &four)));
        assert!(needs_calc(&book(
            "<fileVersion appName=\"other\" lastEdited=\"7\"/>",
            now,
            &four
        )));
        assert!(needs_calc(&book(
            "<fileVersion appName=\"xl\"/>",
            now,
            &four
        )));
        for asked in ["fullCalcOnLoad", "forceFullCalc"] {
            let calc = format!("{now} {asked}=\"1\"");
            assert!(needs_calc(&book(xl, &calc, &four)));
        }
    }

    #[test]
    fn written_file_version() {
        let none = book("", "", &[]);
        assert_eq!(
            file_version(&none, None).unwrap(),
            "<fileVersion appName=\"xl\" lastEdited=\"7\" lowestEdited=\"7\" rupBuild=\"30625\"/>"
        );
        let raw = "<fileVersion appName=\"xl\" lastEdited=\"4\" lowestEdited=\"4\"/>";
        let old = book(raw, "", &[]);
        assert!(file_version(&old, Some(raw))
            .unwrap()
            .contains("lastEdited=\"7\" lowestEdited=\"4\""));
    }

    #[test]
    fn names_that_change_with_the_grid() {
        let asks = |f: &str| {
            let xml =
                format!("<definedNames><definedName name=\"n\">{f}</definedName></definedNames>");
            compat_full_calc(&xml).unwrap()
        };
        for f in [
            "ADDRESS(1,1)",
            "IF(TRUE,1,ADDRESS(1,1))",
            "Sheet1!$C:$C",
            "Sheet1!$3:$3",
            "Sheet1!$C$1:$C$65536",
            "SUM(Sheet1:Sheet2!$C:$C)",
        ] {
            assert!(asks(f), "{f}");
        }
        for f in [
            "Sheet1!$A$1",
            "Sheet1!$C$1:$C$65535",
            "SUM(Sheet1!$A$1:$B$2)",
        ] {
            assert!(!asks(f), "{f}");
        }
    }
}
