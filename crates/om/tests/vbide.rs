use xlcalc::Workbook;
use xlcalc_om::{flags, Application, Call, OmError};
use xlcalc_variant::{ObjectId, Variant};

fn app() -> Application<'static> {
    let mut app = Application::new();
    app.add_workbook(Workbook::new());
    app
}

fn call(
    app: &mut Application,
    h: ObjectId,
    name: &str,
    args: &[Variant],
) -> Result<Variant, OmError> {
    let c = Call {
        flags: flags::METHOD | flags::GET,
        lcid: 0x0409,
    };
    app.invoke(h, name, c, args, &[])
}

fn obj(app: &mut Application, h: ObjectId, name: &str, args: &[Variant]) -> ObjectId {
    match call(app, h, name, args) {
        Ok(Variant::Dispatch(Some(id))) => id,
        other => panic!("{name}: {other:?}"),
    }
}

fn text(app: &mut Application, cm: ObjectId) -> String {
    let n = match call(app, cm, "CountOfLines", &[]).unwrap() {
        Variant::I4(n) => n,
        other => panic!("{other:?}"),
    };
    if n == 0 {
        return String::new();
    }
    match call(app, cm, "Lines", &[Variant::I4(1), Variant::I4(n)]).unwrap() {
        Variant::Bstr(s) => s,
        other => panic!("{other:?}"),
    }
}

fn comps(app: &mut Application) -> ObjectId {
    let root = app.handle();
    let wb = obj(app, root, "ActiveWorkbook", &[]);
    let p = obj(app, wb, "VBProject", &[]);
    obj(app, p, "VBComponents", &[])
}

fn names(app: &mut Application, cs: ObjectId) -> Vec<String> {
    let n = call(app, cs, "Count", &[]).unwrap();
    let Variant::I4(n) = n else { panic!() };
    (1..=n)
        .map(|i| {
            let c = obj(app, cs, "Item", &[Variant::I4(i)]);
            match call(app, c, "Name", &[]).unwrap() {
                Variant::Bstr(s) => s,
                other => panic!("{other:?}"),
            }
        })
        .collect()
}

#[test]
fn a_new_workbook_has_document_modules() {
    let mut app = app();
    let cs = comps(&mut app);
    assert_eq!(names(&mut app, cs), ["ThisWorkbook", "Sheet1"]);
    let c = obj(&mut app, cs, "Item", &[Variant::Bstr("sheet1".into())]);
    assert_eq!(call(&mut app, c, "Type", &[]).unwrap(), Variant::I4(100));
}

#[test]
fn added_modules_are_named_and_edited_like_the_vbe_does() {
    let mut app = app();
    let cs = comps(&mut app);
    let m = obj(&mut app, cs, "Add", &[Variant::I4(1)]);
    assert_eq!(
        call(&mut app, m, "Name", &[]).unwrap(),
        Variant::Bstr("Module1".into())
    );
    let cm = obj(&mut app, m, "CodeModule", &[]);
    let add = |app: &mut Application, s: &str| {
        call(app, cm, "AddFromString", &[Variant::Bstr(s.into())]).unwrap()
    };
    add(
        &mut app,
        "Public Function Triple(x As Double) As Double\r\n    Triple = x * 3\r\nEnd Function",
    );
    add(&mut app, "Option Explicit");
    assert_eq!(
        text(&mut app, cm),
        "Option Explicit\r\nPublic Function Triple(x As Double) As Double\r\n    Triple = x * 3\r\nEnd Function"
    );
    assert_eq!(
        call(&mut app, cm, "CountOfDeclarationLines", &[]).unwrap(),
        Variant::I4(1)
    );

    let root = app.handle();
    let r = call(
        &mut app,
        root,
        "Run",
        &[Variant::Bstr("Triple".into()), Variant::R8(4.0)],
    );
    assert_eq!(r.unwrap(), Variant::R8(12.0));

    let past = call(&mut app, cm, "Lines", &[Variant::I4(4), Variant::I4(9)]).unwrap();
    assert_eq!(past, Variant::Bstr("End Function\r\n".into()));
    call(
        &mut app,
        cm,
        "ReplaceLine",
        &[Variant::I4(1), Variant::Bstr("Option Compare Text".into())],
    )
    .unwrap();
    call(&mut app, cm, "DeleteLines", &[Variant::I4(1)]).unwrap();
    assert!(text(&mut app, cm).starts_with("Public Function"));
    let m2 = obj(&mut app, cs, "Add", &[Variant::I4(2)]);
    assert_eq!(
        call(&mut app, m2, "Name", &[]).unwrap(),
        Variant::Bstr("Class1".into())
    );

    app.invoke(m, "Name", Call::PUT, &[Variant::Bstr("Mod_A".into())], &[])
        .unwrap();
    assert_eq!(
        names(&mut app, cs),
        ["ThisWorkbook", "Sheet1", "Mod_A", "Class1"]
    );
    assert!(call(&mut app, cm, "CountOfLines", &[]).is_ok());
}

#[test]
fn refusals_carry_the_vbe_error_numbers() {
    let mut app = app();
    let cs = comps(&mut app);
    let number = |r: Result<Variant, OmError>| r.unwrap_err().number;
    let bad = |r: Result<Variant, OmError>| r.unwrap_err().number == 9;
    assert!(bad(call(&mut app, cs, "Item", &[Variant::I4(0)])));
    assert!(bad(call(
        &mut app,
        cs,
        "Item",
        &[Variant::Bstr("Nope".into())]
    )));
    assert_eq!(number(call(&mut app, cs, "Add", &[Variant::I4(7)])), 440);
    let m = obj(&mut app, cs, "Add", &[Variant::I4(1)]);
    let rename = |app: &mut Application, n: &str| {
        app.invoke(m, "Name", Call::PUT, &[Variant::Bstr(n.into())], &[])
    };
    assert_eq!(number(rename(&mut app, "bad name")), 50132);
    assert_eq!(number(rename(&mut app, "Sheet1")), 32813);
    assert!(rename(&mut app, &"y".repeat(31)).is_ok());
    let cm = obj(&mut app, m, "CodeModule", &[]);
    assert_eq!(
        number(call(
            &mut app,
            cm,
            "Lines",
            &[Variant::I4(0), Variant::I4(1)]
        )),
        0x8007_0057_u32 as i32
    );
    assert_eq!(
        number(call(&mut app, cm, "DeleteLines", &[Variant::I4(1)])),
        0x8007_0057_u32 as i32
    );
    let tw = obj(&mut app, cs, "Item", &[Variant::I4(1)]);
    let r = call(&mut app, cs, "Remove", &[Variant::Dispatch(Some(tw))]);
    assert!(r.is_err());
}
