use xlcalc::pivot::{DataField, Form, Func, Orient, PivotCache, PivotTable};
use xlcalc::{CellAddr, Rect, Value, Workbook};
use xlcalc_xlsx::{check_package, import_bytes, save, SaveOptions};

fn build() -> Workbook {
    let mut wb = Workbook::new();
    let s = wb.active_sheet();
    let rows = ["k", "a", "b", "a", "c", "b"];
    for (r, k) in rows.iter().enumerate() {
        let r = r as u32;
        wb.set_value(CellAddr::new(s, r, 0), Value::text(*k));
        wb.set_value(
            CellAddr::new(s, r, 1),
            if r == 0 {
                Value::text("v")
            } else {
                Value::Number(r as f64)
            },
        );
    }
    wb.add_table("T1", s, Rect::new(0, 0, 5, 1)).unwrap();
    let c = wb.slicer_cache_add("T1", "k", false).unwrap();
    let i = wb.slicer_add(c, s, Some("S1"), Some("Cap"), [10.0, 300.0, 100.0, 150.0]);
    assert_eq!(i, Ok(0));
    wb.slicer_cache_mut(c).unwrap().slicers[0].columns = 2;
    wb.slicer_select(c, "b", false).unwrap();

    let s2 = wb.add_sheet("Sheet2").unwrap();
    let cache = PivotCache::read(&wb, "Sheet1!R1C1:R6C2", s, Rect::new(0, 0, 5, 1)).unwrap();
    wb.pivots_mut().caches.push(cache);
    let mut t = PivotTable::new("PT1", 0, 2, s2, 2, 0);
    t.set_orient(0, Orient::Row);
    t.data.push(DataField {
        src: 1,
        caption: "Sum of v".into(),
        func: Func::Sum,
        numfmt: None,
        show: Default::default(),
    });
    t.form = Form::Compact;
    wb.pivots_mut().tables.push(t);
    wb.pivot_update(0).unwrap();
    let c2 = wb.slicer_cache_add("PT1", "k", true).unwrap();
    wb.slicer_add(c2, s2, Some("S2"), None, [10.0, 300.0, 100.0, 150.0])
        .unwrap();
    wb.slicer_select(c2, "a", false).unwrap();

    let s3 = wb.add_sheet("Dates").unwrap();
    for (r, d) in [44927.0, 44958.0, 45000.0, 45200.0].iter().enumerate() {
        let r = r as u32 + 1;
        wb.set_value(CellAddr::new(s3, r, 0), Value::Number(*d));
        wb.set_value(CellAddr::new(s3, r, 1), Value::Number(f64::from(r)));
    }
    wb.set_value(CellAddr::new(s3, 0, 0), Value::text("d"));
    wb.set_value(CellAddr::new(s3, 0, 1), Value::text("v"));
    let cache = PivotCache::read(&wb, "Dates!R1C1:R5C2", s3, Rect::new(0, 0, 4, 1)).unwrap();
    wb.pivots_mut().caches.push(cache);
    for (n, row) in [("PT2", 2), ("PT3", 12)] {
        let mut t = PivotTable::new(n, 1, 2, s3, row, 3);
        t.set_orient(0, Orient::Row);
        t.data.push(DataField {
            src: 1,
            caption: "Sum of v".into(),
            func: Func::Sum,
            numfmt: None,
            show: Default::default(),
        });
        t.form = Form::Compact;
        wb.pivots_mut().tables.push(t);
        let ti = wb.pivots().tables.len() - 1;
        wb.pivot_update(ti).unwrap();
    }
    let c3 = wb.timeline_cache_add("PT2", "d").unwrap();
    wb.slicer_pivot_add(c3, "PT3").unwrap();
    wb.slicer_add(c3, s3, Some("TL1"), None, [10.0, 300.0, 262.5, 108.0])
        .unwrap();
    wb.slicer_cache_mut(c3).unwrap().slicers[0].tv.level = 1;
    wb.timeline_set(c3, Some(44950.0), Some(45100.0));
    wb
}

#[test]
fn round_trip() {
    let wb = build();
    let saved = save(&wb, None, &SaveOptions::default()).unwrap();
    assert!(saved.warnings.is_empty(), "{:?}", saved.warnings);
    let problems = check_package(&saved.bytes);
    assert!(problems.is_empty(), "{problems:#?}");
    if let Ok(p) = std::env::var("SLICER_OUT") {
        std::fs::write(p, &saved.bytes).unwrap();
    }
    let back = import_bytes(saved.bytes).unwrap();
    assert!(back.warnings.is_empty(), "{:?}", back.warnings);
    let mut a = wb.slicer_caches().to_vec();
    let mut b = back.workbook.slicer_caches().to_vec();
    a.sort_by(|x, y| x.name.cmp(&y.name));
    b.sort_by(|x, y| x.name.cmp(&y.name));
    assert_eq!(a.len(), 3);
    assert_eq!(b.len(), 3);
    for (x, y) in a.iter().zip(&b) {
        assert_eq!(
            (&x.name, &x.table, x.pivot, &x.field),
            (&y.name, &y.table, y.pivot, &y.field)
        );
        assert_eq!(x.off, y.off);
        assert_eq!(x.timeline, y.timeline);
        assert_eq!(x.pivots, y.pivots);
        assert_eq!(x.slicers.len(), y.slicers.len());
        for (p, q) in x.slicers.iter().zip(&y.slicers) {
            assert_eq!(
                (&p.name, &p.caption, p.columns),
                (&q.name, &q.caption, q.columns)
            );
            assert_eq!(p.style, q.style);
            assert_eq!(p.tv, q.tv);
            assert!((p.row_h - q.row_h).abs() < 1e-3);
        }
    }

    let s = back.workbook.sheets().next().unwrap().id;
    let sh = back.workbook.drawing(s).unwrap();
    assert!(sh.shapes.iter().any(|x| x.name == "S1"));
    let d = back.workbook.sheets().nth(2).unwrap().id;
    let sh = back.workbook.drawing(d).unwrap();
    assert!(sh
        .shapes
        .iter()
        .any(|x| x.name == "TL1" && x.kind == xlcalc::drawing::ShapeKind::Timeline));
}
