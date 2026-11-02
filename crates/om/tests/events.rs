use std::sync::{Arc, Mutex};

use xlcalc::Workbook;
use xlcalc_om::{Application, Call, Event, EventSink, EventSource, SinkFailed};
use xlcalc_variant::Variant;

#[derive(Default)]
struct Recorder(Mutex<Vec<String>>);

impl EventSink for Recorder {
    fn deliver(&self, app: &mut Application<'_>, event: &mut Event<'_>) -> Result<(), SinkFailed> {
        let first = match event.args.first() {
            Some(Variant::Dispatch(Some(id))) => {
                match app.invoke(*id, "Name", Call::GET, &[], &[]) {
                    Ok(Variant::Bstr(s)) => s,
                    _ => "?".into(),
                }
            }
            _ => String::new(),
        };
        self.0
            .lock()
            .unwrap()
            .push(format!("{} {first}", event.member.name));
        Ok(())
    }
}

fn put(app: &mut Application, a1: &str, formula: &str) {
    let h = app.handle();
    let r = match app.invoke(h, "Range", Call::VB, &[Variant::Bstr(a1.into())], &[]) {
        Ok(Variant::Dispatch(Some(id))) => id,
        other => panic!("Range({a1}): {other:?}"),
    };
    app.invoke(
        r,
        "Formula",
        Call::PUT,
        &[Variant::Bstr(formula.into())],
        &[],
    )
    .unwrap();
    app.release(r);
}

#[test]
fn advised_application_sink() {
    let mut app = Application::new();
    app.add_workbook(Workbook::new());
    let rec = Arc::new(Recorder::default());
    let cookie = app.advise(EventSource::Application, rec.clone());
    put(&mut app, "A1", "1");
    put(&mut app, "A2", "=A1*2");
    assert!(rec
        .0
        .lock()
        .unwrap()
        .contains(&"SheetChange Sheet1".to_string()));
    assert!(rec
        .0
        .lock()
        .unwrap()
        .contains(&"SheetCalculate Sheet1".to_string()));
    assert!(app.unadvise(cookie));
    rec.0.lock().unwrap().clear();
    put(&mut app, "A1", "3");
    assert!(rec.0.lock().unwrap().is_empty());
}

#[test]
fn document_module_handler() {
    let mut app = Application::new();
    let book = app.add_workbook(Workbook::new());
    let src = "Private Sub Worksheet_Change(ByVal Target As Range)\r\n\
               If Target.Address = \"$A$1\" Then Range(\"B1\").Value = Target.Value * 10\r\n\
               End Sub\r\n";
    let (module, errors) = app.add_document_module(book, "Sheet1", src).unwrap();
    assert_eq!(module, "Sheet1");
    assert!(errors.is_empty(), "{errors:?}");
    put(&mut app, "A1", "4");
    let wb = app.engine(book).unwrap();
    let s = wb.sheet_at(0).id;
    let b1 = xlcalc::parse_cell_ref(wb, "B1", s).unwrap();
    assert_eq!(format!("{:?}", wb.value(b1)), "Number(40.0)");
    assert!(app.take_handler_errors().is_empty());
}
