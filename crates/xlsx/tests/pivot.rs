use xlcalc::pivot::{DataField, Form, Func, Orient, PivotCache, PivotTable};
use xlcalc::{CellAddr, Value, Workbook};
use xlcalc_xlsx::{check_package, import_bytes, save, xlsx_to_xlsb, SaveOptions};

fn deliver(xlsx: Vec<u8>, xlsb: bool) -> Vec<u8> {
    if !xlsb {
        return xlsx;
    }
    let (bytes, warnings) = xlsx_to_xlsb(&xlsx).unwrap();
    assert!(warnings.is_empty(), "{warnings:?}");
    bytes
}

fn build(form: Form, two_data: bool) -> Workbook {
    let mut wb = Workbook::new();
    let s = wb.active_sheet();
    let rows = [
        ("Region", "Product", "Qtr", 0.0),
        ("East", "Apple", "Q1", 10.0),
        ("East", "Pear", "Q2", 5.0),
        ("West", "Apple", "Q1", 20.0),
        ("West", "Pear", "Q2", 7.0),
        ("North", "Plum", "Q1", 11.0),
        ("East", "Apple", "Q2", 3.0),
    ];
    for (r, row) in rows.iter().enumerate() {
        let r = r as u32;
        wb.set_value(CellAddr::new(s, r, 0), Value::text(row.0));
        wb.set_value(CellAddr::new(s, r, 1), Value::text(row.1));
        wb.set_value(CellAddr::new(s, r, 2), Value::text(row.2));
        wb.set_value(
            CellAddr::new(s, r, 3),
            if r == 0 {
                Value::text("Amount")
            } else {
                Value::Number(row.3)
            },
        );
    }
    let rect = xlcalc::Rect::new(0, 0, 6, 3);
    let cache = PivotCache::read(&wb, "Sheet1!R1C1:R7C4", s, rect).unwrap();
    wb.pivots_mut().caches.push(cache);
    let mut t = PivotTable::new("PT1", 0, 4, s, 4, 6);
    t.set_orient(0, Orient::Row);
    t.set_orient(1, Orient::Row);
    t.set_orient(2, Orient::Column);
    t.data.push(DataField {
        src: 3,
        caption: "Sum of Amount".into(),
        func: Func::Sum,
        numfmt: None,
        show: Default::default(),
    });
    if two_data {
        t.data.push(DataField {
            src: 3,
            caption: "Count of Amount".into(),
            func: Func::Count,
            numfmt: None,
            show: Default::default(),
        });
    }
    t.form = form;
    wb.pivots_mut().tables.push(t);
    wb.pivot_update(0).unwrap();
    wb
}

#[test]
fn round_trip() {
    for (form, two, xlsb) in [
        (Form::Compact, false, false),
        (Form::Outline, true, false),
        (Form::Tabular, false, false),
        (Form::Compact, false, true),
        (Form::Outline, true, true),
        (Form::Tabular, false, true),
    ] {
        let wb = build(form, two);
        let saved = save(&wb, None, &SaveOptions::default()).unwrap();
        assert!(saved.warnings.is_empty(), "{:?}", saved.warnings);
        let problems = check_package(&saved.bytes);
        assert!(problems.is_empty(), "{problems:#?}");
        let bytes = deliver(saved.bytes, xlsb);
        if let Ok(p) = std::env::var("PIVOT_OUT") {
            let ext = if xlsb { "xlsb" } else { "xlsx" };
            std::fs::write(format!("{p}_{form:?}_{two}.{ext}"), &bytes).unwrap();
        }
        let back = import_bytes(bytes).unwrap();
        assert!(back.warnings.is_empty(), "{:?}", back.warnings);
        let (a, b) = (&wb.pivots().tables[0], &back.workbook.pivots().tables[0]);
        assert_eq!(a.name, b.name);
        assert_eq!(a.axes(), b.axes());
        assert_eq!(a.form, b.form);
        assert_eq!(a.data.len(), b.data.len());
        assert_eq!(a.ranges, b.ranges, "{form:?}");
        assert_eq!(a.written, b.written);
    }
}

#[test]
fn settings_round_trip() {
    for xlsb in [false, true] {
        settings_case(xlsb);
    }
}

fn settings_case(xlsb: bool) {
    use xlcalc::pivot::Sort;
    let mut wb = build(Form::Outline, false);
    {
        let t = &mut wb.pivots_mut().tables[0];
        t.fields[0].sort = Sort::Name(true);
        t.fields[0].blank_line = true;
        t.fields[0].subtotals = false;
        t.fields[0].custom = vec![2, 4];
        t.fields[0].hidden.push(1);
        t.fields[1].sort = Sort::Data(true, 0);
        t.data[0].show.kind = 3;
        t.data[0].show.base_field = Some(0);
        t.data[0].show.base_item = Some(2);
        t.fields[0].caption = Some("Zone".to_string());
        t.fields[0].item_caps.push((0, "Zed".to_string()));
        t.style = "PivotStyleMedium9".to_string();
        t.style_flags[3] = true;
        t.row_caption = Some("Areas".to_string());
        t.fields[1].repeat = true;
    }
    wb.pivot_update(0).unwrap();
    let saved = save(&wb, None, &SaveOptions::default()).unwrap();
    let problems = check_package(&saved.bytes);
    assert!(problems.is_empty(), "{problems:#?}");
    let bytes = deliver(saved.bytes, xlsb);
    if let Ok(p) = std::env::var("PIVOT_OUT") {
        let ext = if xlsb { "xlsb" } else { "xlsx" };
        std::fs::write(format!("{p}_settings.{ext}"), &bytes).unwrap();
    }
    let back = import_bytes(bytes).unwrap();
    let (a, b) = (&wb.pivots().tables[0], &back.workbook.pivots().tables[0]);
    for f in 0..2 {
        assert_eq!(a.fields[f].sort, b.fields[f].sort);
        assert_eq!(a.fields[f].custom, b.fields[f].custom);
        assert_eq!(a.fields[f].blank_line, b.fields[f].blank_line);
        assert_eq!(a.fields[f].subtotals, b.fields[f].subtotals);
        assert_eq!(a.fields[f].hidden, b.fields[f].hidden);
    }
    assert_eq!(a.fields[0].caption, b.fields[0].caption);
    assert_eq!(a.fields[0].item_caps, b.fields[0].item_caps);
    assert_eq!(a.style, b.style);
    assert_eq!(a.style_flags, b.style_flags);
    assert_eq!(a.row_caption, b.row_caption);
    assert_eq!(a.fields[1].repeat, b.fields[1].repeat);
    assert_eq!(a.data[0].show, b.data[0].show);
    assert_eq!(a.ranges, b.ranges);
}

fn grouped_book(kind: &str) -> Workbook {
    use xlcalc::pivot::{CalcItem, DateUnit, Group};
    let mut wb = Workbook::new();
    let s = wb.active_sheet();
    let heads = ["Date", "Units", "Region", "Amount"];
    for (c, h) in heads.iter().enumerate() {
        wb.set_value(CellAddr::new(s, 0, c as u32), Value::text(*h));
    }
    let dates = [
        44941.0, 44972.0, 45000.0, 45031.0, 45062.0, 45292.0, 45323.0, 45352.0,
    ];
    let units = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 12.0, 15.0];
    let regions = [
        "East", "West", "East", "North", "West", "East", "North", "West",
    ];
    for i in 0..8 {
        let r = i as u32 + 1;
        wb.set_value(CellAddr::new(s, r, 0), Value::Number(dates[i]));
        wb.set_value(CellAddr::new(s, r, 1), Value::Number(units[i]));
        wb.set_value(CellAddr::new(s, r, 2), Value::text(regions[i]));
        wb.set_value(CellAddr::new(s, r, 3), Value::Number(10.0 + i as f64));
    }
    let rect = xlcalc::Rect::new(0, 0, 8, 3);
    let cache = PivotCache::read(&wb, "Sheet1!R1C1:R9C4", s, rect).unwrap();
    wb.pivots_mut().caches.push(cache);
    let mut t = PivotTable::new("PT1", 0, 4, s, 3, 6);
    t.data.push(DataField {
        src: 3,
        caption: "Sum of Amount".into(),
        func: Func::Sum,
        numfmt: None,
        show: Default::default(),
    });
    match kind {
        "calc" => {
            t.set_orient(2, Orient::Row);
            t.fields[2].calc_items.push(CalcItem {
                name: "Mix".into(),
                formula: "East+West*2".into(),
            });
        }
        "num" => {
            t.set_orient(1, Orient::Row);
            t.fields[1].group = Some(Group::Num {
                start: Some(1.0),
                end: Some(15.0),
                by: 5.0,
            });
        }
        "date" => {
            t.set_orient(0, Orient::Row);
            t.add_date_levels(0, &[DateUnit::Month, DateUnit::Year], None, None);
        }
        _ => {
            t.set_orient(2, Orient::Row);
            t.add_manual_group(2, "Group1", vec!["East".into(), "West".into()]);
        }
    }
    wb.pivots_mut().tables.push(t);
    wb.pivot_update(0).unwrap();
    wb
}

#[test]
fn grouping_round_trip() {
    for (kind, xlsb) in [
        ("calc", false),
        ("num", false),
        ("date", false),
        ("sel", false),
        ("calc", true),
        ("num", true),
        ("date", true),
        ("sel", true),
    ] {
        let wb = grouped_book(kind);
        let saved = save(&wb, None, &SaveOptions::default()).unwrap();
        assert!(saved.warnings.is_empty(), "{kind}: {:?}", saved.warnings);
        let problems = check_package(&saved.bytes);
        assert!(problems.is_empty(), "{kind}: {problems:#?}");
        let bytes = deliver(saved.bytes, xlsb);
        if let Ok(p) = std::env::var("PIVOT_OUT") {
            let ext = if xlsb { "xlsb" } else { "xlsx" };
            std::fs::write(format!("{p}_{kind}.{ext}"), &bytes).unwrap();
        }
        let back = import_bytes(bytes).unwrap();
        assert!(back.warnings.is_empty(), "{kind}: {:?}", back.warnings);
        let (a, b) = (&wb.pivots().tables[0], &back.workbook.pivots().tables[0]);
        assert_eq!(a.fields.len(), b.fields.len(), "{kind}");
        for f in 0..a.fields.len() {
            assert_eq!(a.fields[f].group, b.fields[f].group, "{kind} field {f}");
            assert_eq!(a.fields[f].calc_items.len(), b.fields[f].calc_items.len());
        }
        assert_eq!(a.axes(), b.axes(), "{kind}");
        assert_eq!(a.ranges, b.ranges, "{kind}");
        let r = xlcalc::pivot::render(a, &wb.pivots().caches[0]);
        let r2 = xlcalc::pivot::render(b, &back.workbook.pivots().caches[0]);
        assert_eq!(format!("{r:?}"), format!("{r2:?}"), "{kind}");
    }
}
