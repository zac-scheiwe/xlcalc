use xlcalc::Workbook;
use xlcalc_variant::{HResult, ObjectId, Variant};

use crate::app::{flags, Application, Call, DISPID_VALUE};
use crate::error::{DISP_E_BADPARAMCOUNT, DISP_E_PARAMNOTFOUND, DISP_E_UNKNOWNNAME};

fn app() -> Application<'static> {
    let mut wb = Workbook::empty();
    wb.add_sheet("Sheet1").unwrap();
    wb.add_sheet("Sheet2").unwrap();
    let mut app = Application::new();
    app.add_workbook(wb);
    app
}

fn obj(v: Variant) -> ObjectId {
    match v {
        Variant::Dispatch(Some(id)) => id,
        other => panic!("not an object: {other:?}"),
    }
}

fn get(app: &mut Application, h: ObjectId, member: &str, args: &[Variant]) -> Variant {
    app.invoke(h, member, Call::VB, args, &[]).unwrap()
}

fn range(app: &mut Application, a1: &str) -> ObjectId {
    let root = app.handle();
    obj(get(app, root, "Range", &[Variant::Bstr(a1.into())]))
}

fn put(app: &mut Application, h: ObjectId, member: &str, v: Variant) {
    app.invoke(h, member, Call::PUT, &[v], &[]).unwrap();
}

#[test]
fn arguments_bind_by_name_and_position() {
    let mut a = app();
    let r = range(&mut a, "Sheet1!B2:D5");
    let named = a
        .invoke(
            r,
            "Offset",
            Call::VB,
            &[],
            &[("ColumnOffset", Variant::I4(1))],
        )
        .unwrap();
    let text = get(&mut a, obj(named), "Address", &[]);
    assert_eq!(text, Variant::Bstr("$C$2:$E$5".into()));

    let missing = Variant::Error(HResult::DISP_E_PARAMNOTFOUND.0);
    let moved = a
        .invoke(r, "Offset", Call::VB, &[missing, Variant::I4(2)], &[])
        .unwrap();
    assert_eq!(
        get(&mut a, obj(moved), "Address", &[]),
        Variant::Bstr("$D$2:$F$5".into())
    );
}

#[test]
fn a_named_argument_given_twice_is_refused() {
    let mut a = app();
    let r = range(&mut a, "Sheet1!A1");
    let err = a
        .invoke(
            r,
            "Address",
            Call::VB,
            &[Variant::Bool(false)],
            &[("RowAbsolute", Variant::Bool(true))],
        )
        .unwrap_err();
    assert_eq!(err.hresult, DISP_E_PARAMNOTFOUND);
    assert_eq!(err.arg_err, Some(0));
}

#[test]
fn names_are_found_without_regard_to_case() {
    let mut a = app();
    let r = range(&mut a, "Sheet1!A1");
    let ids = a.get_ids_of_names(r, "vAlUe2", &[]).unwrap();
    assert_eq!(ids.len(), 1);
    let err = a.get_ids_of_names(r, "Nope", &[]).unwrap_err();
    assert_eq!(err.hresult, DISP_E_UNKNOWNNAME);
    assert_eq!(err.ids, vec![-1]);
    let err = a.get_ids_of_names(r, "Address", &["Bogus"]).unwrap_err();
    assert_eq!(err.hresult.0 as u32, 0x800A_03EC);
    assert_eq!(err.ids, vec![0xec, -1]);
}

#[test]
fn a_property_with_no_parameters_hands_extra_arguments_to_the_default_member() {
    let mut a = app();
    let root = a.handle();
    let sheet = obj(get(
        &mut a,
        root,
        "Worksheets",
        &[Variant::Bstr("sheet2".into())],
    ));
    assert_eq!(
        get(&mut a, sheet, "Name", &[]),
        Variant::Bstr("Sheet2".into())
    );
    let cell = obj(get(
        &mut a,
        sheet,
        "Cells",
        &[Variant::I4(2), Variant::I4(3)],
    ));
    assert_eq!(
        get(&mut a, cell, "Address", &[]),
        Variant::Bstr("$C$2".into())
    );

    let r = range(&mut a, "Sheet1!A1");
    let err = a
        .invoke(
            r,
            "Offset",
            Call::VB,
            &[Variant::I4(1), Variant::I4(1), Variant::I4(1)],
            &[],
        )
        .unwrap_err();
    assert_eq!(err.hresult, DISP_E_BADPARAMCOUNT);
}

#[test]
fn the_default_member_reads_the_value() {
    let mut a = app();
    let r = range(&mut a, "Sheet1!A1");
    put(&mut a, r, "Value2", Variant::R8(2.5));
    let v = a
        .invoke_dispid(r, DISPID_VALUE, Call::GET, &[], &[])
        .unwrap();
    assert_eq!(v, Variant::R8(2.5));
}

#[test]
fn dispatch_flags_select_the_member() {
    let mut a = app();
    let r = range(&mut a, "Sheet1!A1");
    let method_only = Call {
        flags: flags::METHOD,
        lcid: 0x0409,
    };
    let err = a
        .invoke(
            r,
            "Offset",
            method_only,
            &[Variant::I4(1), Variant::I4(1)],
            &[],
        )
        .unwrap_err();
    assert_eq!(err.hresult.0 as u32, 0x8002_0003);

    let err = a
        .invoke(r, "Row", Call::PUT, &[Variant::I4(5)], &[])
        .unwrap_err();
    assert_eq!(err.hresult, DISP_E_BADPARAMCOUNT);
}

#[test]
fn values_come_back_as_the_variants_excel_gives() {
    let mut a = app();
    let r = range(&mut a, "Sheet1!A1:A3");
    let a1 = range(&mut a, "Sheet1!A1");
    put(&mut a, a1, "Value2", Variant::R8(44000.0));
    put(
        &mut a,
        a1,
        "NumberFormat",
        Variant::Bstr("yyyy-mm-dd".into()),
    );
    assert_eq!(get(&mut a, a1, "Value", &[]), Variant::Date(44000.0));
    assert_eq!(get(&mut a, a1, "Value2", &[]), Variant::R8(44000.0));
    assert_eq!(
        get(&mut a, a1, "Text", &[]),
        Variant::Bstr("2020-06-18".into())
    );
    let a2 = range(&mut a, "Sheet1!A2");
    put(&mut a, a2, "Formula", Variant::Bstr("=NA()".into()));
    assert_eq!(
        get(&mut a, a2, "Value2", &[]),
        Variant::Error(0x800A_07FA_u32 as i32)
    );
    let Variant::Array(arr) = get(&mut a, r, "Value2", &[]) else {
        panic!("a block reads as an array");
    };
    assert_eq!(arr.bounds, vec![(1, 3), (1, 1)]);
    assert_eq!(arr.data[2], Variant::Empty);
}

#[test]
fn text_is_read_with_the_locale_of_the_call() {
    let mut a = app();
    let us = range(&mut a, "Sheet1!A1");
    let nz = range(&mut a, "Sheet1!A2");
    let text = Variant::Bstr("1/2/2020".into());
    a.invoke(us, "Value2", Call::PUT, std::slice::from_ref(&text), &[])
        .unwrap();
    let call = Call {
        flags: flags::PUT,
        lcid: 5129,
    };
    a.invoke(nz, "Value2", call, &[text], &[]).unwrap();
    assert_eq!(get(&mut a, us, "Value2", &[]), Variant::R8(43832.0));
    assert_eq!(get(&mut a, nz, "Value2", &[]), Variant::R8(43862.0));

    let german = Call {
        flags: flags::GET,
        lcid: 1031,
    };
    let err = a.invoke(us, "Value2", german, &[], &[]).unwrap_err();
    assert_eq!(err.scode, 0x8002_8018_u32 as i32);
}

#[test]
fn a_released_handle_is_refused() {
    let mut a = app();
    let r = range(&mut a, "Sheet1!A1");
    a.release(r);
    let err = a.invoke(r, "Address", Call::VB, &[], &[]).unwrap_err();
    assert_eq!(err.hresult.0 as u32, 0x8002_0003);
    assert!(a.type_name(r).is_none());

    let again = range(&mut a, "Sheet1!A2");
    assert_ne!(again, r);
    assert!(a.invoke(r, "Address", Call::VB, &[], &[]).is_err());
}

#[test]
fn collections_enumerate() {
    let mut a = app();
    let root = a.handle();
    let sheets = obj(get(&mut a, root, "Worksheets", &[]));
    let Variant::Unknown(Some(e)) = get(&mut a, sheets, "_NewEnum", &[]) else {
        panic!("_NewEnum gives an enumerator");
    };
    let first = a.enum_next(e, 1);
    assert_eq!(first.len(), 1);
    let rest = a.enum_next(e, 5);
    assert_eq!(rest.len(), 1);
    assert!(a.enum_next(e, 1).is_empty());
    a.enum_reset(e);
    assert_eq!(a.enum_next(e, 5).len(), 2);
    let name = get(&mut a, obj(first[0].clone()), "Name", &[]);
    assert_eq!(name, Variant::Bstr("Sheet1".into()));
}

#[test]
fn names_add_read_and_delete() {
    let mut a = app();
    let root = a.handle();
    let names = obj(get(&mut a, root, "Names", &[]));
    let n = a
        .invoke(
            names,
            "Add",
            Call::VB,
            &[],
            &[
                ("Name", Variant::Bstr("total".into())),
                ("RefersTo", Variant::Bstr("=Sheet1!$B$2:$C$3".into())),
            ],
        )
        .unwrap();
    let n = obj(n);
    assert_eq!(get(&mut a, n, "Name", &[]), Variant::Bstr("total".into()));
    assert_eq!(
        get(&mut a, n, "RefersTo", &[]),
        Variant::Bstr("=Sheet1!$B$2:$C$3".into())
    );
    assert_eq!(
        get(&mut a, n, "RefersToR1C1", &[]),
        Variant::Bstr("=Sheet1!R2C2:R3C3".into())
    );
    let r = obj(get(&mut a, n, "RefersToRange", &[]));
    assert_eq!(get(&mut a, r, "Count", &[]), Variant::I4(4));
    assert_eq!(get(&mut a, names, "Count", &[]), Variant::I4(1));
    get(&mut a, n, "Delete", &[]);
    assert_eq!(get(&mut a, names, "Count", &[]), Variant::I4(0));
    let err = a
        .invoke(
            names,
            "Item",
            Call::VB,
            &[Variant::Bstr("total".into())],
            &[],
        )
        .unwrap_err();
    assert_eq!(err.number, 1004);
}

#[test]
fn r1c1_formulas_convert_to_a1() {
    use crate::range::r1c1_to_a1;
    assert_eq!(r1c1_to_a1("=RC[-1]+1", 0, 13), "=M1+1");
    assert_eq!(r1c1_to_a1("=SUM(R[-1]C[-1],R2C2)", 5, 5), "=SUM(E5,$B$2)");
    assert_eq!(
        r1c1_to_a1("=ROUND(R1C1,0)&\"RC\"", 3, 3),
        "=ROUND($A$1,0)&\"RC\""
    );
}

#[test]
fn added_workbook_takes_puts() {
    let mut a = Application::new();
    let root = a.handle();
    let books = obj(get(&mut a, root, "Workbooks", &[]));
    let wb = obj(a.invoke(books, "Add", Call::VB, &[], &[]).unwrap());
    let sheets = obj(get(&mut a, wb, "Worksheets", &[]));
    let ws = obj(get(&mut a, sheets, "Item", &[Variant::I4(1)]));
    let r = obj(get(&mut a, ws, "Range", &[Variant::Bstr("A1".into())]));
    a.invoke(
        r,
        "Value2",
        Call { flags: 4, lcid: 0 },
        &[Variant::I4(2)],
        &[],
    )
    .unwrap();
}

#[test]
fn save_as_and_open_go_through_the_file_system() {
    let mut a = app();
    let fs = crate::files::MemFs::new();
    fs.add_folder(r"C:\out");
    a.set_file_system(Box::new(fs));
    let root = a.handle();
    let r = range(&mut a, "Sheet1!A1");
    put(&mut a, r, "Value2", Variant::R8(4.0));
    let book = obj(get(&mut a, root, "ActiveWorkbook", &[]));

    let bad = a.invoke(
        book,
        "SaveAs",
        Call::VB,
        &[Variant::Bstr(r"C:\out\x.xlsm".into()), Variant::I4(51)],
        &[],
    );
    assert!(bad.is_err());
    assert!(a
        .invoke(
            book,
            "SaveAs",
            Call::VB,
            &[Variant::Bstr(r"C:\nope\x.xlsx".into())],
            &[],
        )
        .is_err());
    get(&mut a, book, "SaveAs", &[Variant::Bstr(r"C:\out\x".into())]);
    assert_eq!(
        get(&mut a, book, "Name", &[]),
        Variant::Bstr("x.xlsx".into())
    );
    assert_eq!(
        get(&mut a, book, "Path", &[]),
        Variant::Bstr(r"C:\out".into())
    );
    assert_eq!(get(&mut a, book, "FileFormat", &[]), Variant::R8(51.0));
    assert_eq!(get(&mut a, book, "Saved", &[]), Variant::Bool(true));
    get(
        &mut a,
        book,
        "SaveCopyAs",
        &[Variant::Bstr(r"C:\out\copy.xlsx".into())],
    );
    get(&mut a, book, "Close", &[]);
    let books = obj(get(&mut a, root, "Workbooks", &[]));
    assert_eq!(get(&mut a, books, "Count", &[]), Variant::I4(0));
    let copy = obj(get(
        &mut a,
        books,
        "Open",
        &[Variant::Bstr(r"C:\out\copy.xlsx".into())],
    ));
    assert_eq!(
        get(&mut a, copy, "Name", &[]),
        Variant::Bstr("copy.xlsx".into())
    );
    let r = range(&mut a, "Sheet1!A1");
    assert_eq!(get(&mut a, r, "Value2", &[]), Variant::R8(4.0));

    get(
        &mut a,
        copy,
        "SaveAs",
        &[Variant::Bstr(r"C:\out\t.csv".into()), Variant::I4(6)],
    );
    assert_eq!(get(&mut a, copy, "FileFormat", &[]), Variant::R8(6.0));
    let sheet = obj(get(&mut a, copy, "ActiveSheet", &[]));
    assert_eq!(get(&mut a, sheet, "Name", &[]), Variant::Bstr("t".into()));
}

#[test]
fn new_workbooks_skip_reserved_numbers() {
    let mut a = Application::new();
    a.reserve_book_numbers(&[1, 4]);
    let root = a.handle();
    let books = obj(get(&mut a, root, "Workbooks", &[]));
    let mut names = Vec::new();
    for _ in 0..3 {
        let b = obj(get(&mut a, books, "Add", &[]));
        names.push(get(&mut a, b, "Name", &[]));
    }
    assert_eq!(
        names,
        ["Book2", "Book3", "Book5"].map(|n| Variant::Bstr(n.into()))
    );
}

#[test]
fn form_controls_follow_their_linked_cell() {
    let mut a = app();
    let root = a.handle();
    let ws = obj(get(&mut a, root, "Worksheets", &[Variant::I4(1)]));
    let boxes = obj(get(&mut a, ws, "CheckBoxes", &[]));
    let add = [10.0, 10.0, 60.0, 20.0].map(Variant::R8);
    let cb = obj(get(&mut a, boxes, "Add", &add));
    assert_eq!(
        get(&mut a, cb, "Name", &[]),
        Variant::Bstr("Check Box 1".into())
    );
    put(&mut a, cb, "LinkedCell", Variant::Bstr("e1".into()));
    assert_eq!(
        get(&mut a, cb, "LinkedCell", &[]),
        Variant::Bstr("E1".into())
    );

    assert_eq!(get(&mut a, cb, "Value", &[]), Variant::I4(-4146));
    put(&mut a, cb, "Value", Variant::I4(1));
    assert_eq!(get(&mut a, cb, "Value", &[]), Variant::R8(1.0));
    let e1 = range(&mut a, "Sheet1!E1");
    assert_eq!(get(&mut a, e1, "Value", &[]), Variant::Bool(true));

    put(&mut a, e1, "Value", Variant::I4(0));
    assert_eq!(get(&mut a, cb, "Value", &[]), Variant::I4(-4146));
    put(&mut a, e1, "Value", Variant::Bstr("txt".into()));
    assert_eq!(get(&mut a, cb, "Value", &[]), Variant::I4(-4146));

    let book = a.active_workbook().unwrap();
    let sheet = a.engine(book).unwrap().sheet_at(0).id;
    assert_eq!(a.click_control(book, sheet, "Check Box 1").unwrap(), None);
    assert_eq!(get(&mut a, cb, "Value", &[]), Variant::R8(1.0));
    assert_eq!(get(&mut a, e1, "Value", &[]), Variant::Bool(true));
    let code = "Public Function Fn()\r\n  Worksheets(1).Range(\"F1\").Value = Application.Caller\r\n  Fn = 7\r\nEnd Function\r\n";
    let module = xlcalc_vba::SourceModule {
        name: "Module1".into(),
        kind: xlcalc_vba::ModuleKind::Standard,
        source: code.into(),
    };
    assert!(a.add_vba_module(book, module).unwrap().is_empty());
    put(&mut a, cb, "OnAction", Variant::Bstr("Fn".into()));
    let r = a.click_control(book, sheet, "Check Box 1").unwrap();
    assert_eq!(r, Some(Variant::I2(7)));
    assert_eq!(get(&mut a, e1, "Value", &[]), Variant::Bool(false));
    let f1 = range(&mut a, "Sheet1!F1");
    assert_eq!(
        get(&mut a, f1, "Value", &[]),
        Variant::Bstr("Check Box 1".into())
    );

    put(&mut a, cb, "OnAction", Variant::Bstr("NoSuch".into()));
    assert!(a.click_control(book, sheet, "Check Box 1").is_err());
}

#[test]
fn the_default_host_refuses_dialogs_and_queues_on_time() {
    let mut a = app();
    let book = a.active_workbook().unwrap();
    let code = "Public Function Ask()\r\n  Ask = MsgBox(\"Go?\", vbYesNo, \"T\")\r\nEnd Function\r\nPublic Sub Later()\r\n  Worksheets(1).Range(\"A1\").Value = 5\r\nEnd Sub\r\nPublic Sub Plan()\r\n  Application.OnTime 1, \"Later\"\r\n  DoEvents\r\n  Debug.Print \"x\"; 1\r\nEnd Sub\r\n";
    let module = xlcalc_vba::SourceModule {
        name: "Module1".into(),
        kind: xlcalc_vba::ModuleKind::Standard,
        source: code.into(),
    };
    assert!(a.add_vba_module(book, module).unwrap().is_empty());
    let e = a.run("Ask", &[]).unwrap_err();
    assert!(
        e.to_string().contains("not supported by xlcalc: MsgBox"),
        "{e}"
    );
    a.run("Plan", &[]).unwrap();
    assert_eq!(a.on_time_queue().len(), 1);
    assert_eq!(a.on_time_queue()[0].procedure, "Later");

    assert!(a.run_on_time(0.5).is_empty());
    let done = a.run_on_time(2.0);
    assert_eq!(done.len(), 1, "{done:?}");
    assert!(done[0].is_ok(), "{done:?}");
    assert!(a.on_time_queue().is_empty());
    let a1 = range(&mut a, "Sheet1!A1");
    assert_eq!(get(&mut a, a1, "Value", &[]), Variant::R8(5.0));

    struct Yes(std::sync::Arc<std::sync::Mutex<Vec<String>>>);
    impl crate::Host for Yes {
        fn msg_box(
            &mut self,
            prompt: &str,
            buttons: i32,
            title: &str,
        ) -> Result<i32, crate::OmError> {
            self.0
                .lock()
                .unwrap()
                .push(format!("{prompt}|{buttons}|{title}"));
            Ok(6)
        }
        fn debug_print(&mut self, line: &str) {
            self.0.lock().unwrap().push(line.to_string());
        }
    }
    let log = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
    a.set_host(Box::new(Yes(log.clone())));
    assert_eq!(a.run("Ask", &[]).unwrap(), Variant::I2(6));
    a.run("Plan", &[]).unwrap();
    assert_eq!(*log.lock().unwrap(), ["Go?|4|T", "x 1 "]);
}
