use xlcalc_variant::Variant;

use crate::app::Application;
use crate::conv::Args;
use crate::error::OmError;
use crate::events::HandlerError;
use crate::typelib::Member;

#[derive(Clone, Debug, PartialEq)]
pub struct OnTimeRequest {
    pub earliest: f64,

    pub procedure: String,

    pub latest: Option<f64>,
}

pub trait Host: Send {
    fn msg_box(&mut self, prompt: &str, buttons: i32, title: &str) -> Result<i32, OmError> {
        let _ = (prompt, buttons, title);
        Err(OmError::unsupported("MsgBox"))
    }

    fn input_box(
        &mut self,
        prompt: &str,
        title: &str,
        default: &str,
    ) -> Result<Option<String>, OmError> {
        let _ = (prompt, title, default);
        Err(OmError::unsupported("InputBox"))
    }

    fn get_open_filename(
        &mut self,
        filter: &str,
        title: &str,
        multi: bool,
    ) -> Result<Option<Vec<String>>, OmError> {
        let _ = (filter, title, multi);
        Err(OmError::unsupported("GetOpenFilename"))
    }

    fn on_time(&mut self, request: &OnTimeRequest, schedule: bool) {
        let _ = (request, schedule);
    }

    fn debug_print(&mut self, line: &str) {
        let _ = line;
    }

    fn beep(&mut self) {}

    fn run_open_macros(&self) -> bool {
        false
    }

    fn unhandled_error(&mut self, error: &HandlerError) {
        let _ = error;
    }
}

pub struct NoDialogs;

impl Host for NoDialogs {}

pub(crate) fn debug_line(items: &[xlcalc_vba::DebugItem]) -> String {
    use xlcalc_vba::DebugItem;
    let mut out = String::new();
    for it in items {
        match it {
            DebugItem::Value(v) => out.push_str(&debug_text(v)),
            DebugItem::Zone => {
                let col = out.chars().count();
                let next = (col / 14 + 1) * 14;
                out.extend(std::iter::repeat_n(' ', next - col));
            }
            DebugItem::Join => {}
        }
    }
    out
}

fn debug_text(v: &Variant) -> String {
    use xlcalc_variant::{change_type, Locale, VarType};
    let text = match change_type(v, VarType::Bstr, Locale::EN_US, 0) {
        Ok(Variant::Bstr(s)) => s,
        _ => match v {
            Variant::Null => "Null".to_string(),
            Variant::Error(sc) => format!("Error {}", sc & 0xffff),
            _ => String::new(),
        },
    };
    match v {
        Variant::I2(_)
        | Variant::I4(_)
        | Variant::I8(_)
        | Variant::Ui1(_)
        | Variant::R4(_)
        | Variant::R8(_)
        | Variant::Cy(_)
        | Variant::Decimal(_) => {
            let sign = if text.starts_with('-') { "" } else { " " };
            format!("{sign}{text} ")
        }
        _ => text,
    }
}

impl Application<'_> {
    pub fn set_host(&mut self, host: Box<dyn Host>) {
        self.host = host;
    }

    pub fn on_time_queue(&self) -> &[OnTimeRequest] {
        &self.on_time
    }

    pub fn run_on_time(&mut self, now: f64) -> Vec<Result<Variant, OmError>> {
        let (due, wait): (Vec<OnTimeRequest>, Vec<OnTimeRequest>) =
            std::mem::take(&mut self.on_time)
                .into_iter()
                .partition(|r| r.earliest <= now);
        self.on_time = wait;
        due.into_iter()
            .filter(|r| r.latest.is_none_or(|l| now <= l))
            .map(|r| self.run(&r.procedure, &[]))
            .collect()
    }

    pub fn run(&mut self, name: &str, args: &[Variant]) -> Result<Variant, OmError> {
        let mut values = vec![Some(Variant::Bstr(name.to_string()))];
        values.extend(args.iter().cloned().map(Some));
        let a = Args {
            values,
            put: None,
            lcid: 0x0409,
        };
        let r = self.app_run(&a);
        self.flush_events();
        r
    }

    pub(crate) fn vba_msg_box(&mut self, args: &[Variant]) -> Result<Variant, OmError> {
        let prompt = text_arg(args, 0);
        let buttons = int_arg(args, 1);
        let title = text_arg(args, 2);
        let pressed = self.host.msg_box(&prompt, buttons, &title)?;
        Ok(Variant::I2(pressed as i16))
    }

    pub(crate) fn vba_input_box(&mut self, args: &[Variant]) -> Result<Variant, OmError> {
        let (prompt, title, default) = (text_arg(args, 0), text_arg(args, 1), text_arg(args, 2));
        let text = self.host.input_box(&prompt, &title, &default)?;
        Ok(Variant::Bstr(text.unwrap_or_default()))
    }

    pub(crate) fn vba_beep(&mut self) {
        self.host.beep();
    }

    pub(crate) fn vba_debug_print(&mut self, line: &str) {
        self.host.debug_print(line);
    }

    pub(crate) fn host_member(
        &mut self,
        m: &'static Member,
        a: &Args,
    ) -> Option<Result<Variant, OmError>> {
        Some(match m.name {
            "InputBox" => (|| {
                let prompt = a.string(0)?.unwrap_or_default();
                let title = a.string(1)?.unwrap_or_else(|| "Input".to_string());
                let default = a.string(2)?.unwrap_or_default();
                let ty = a.i32(7)?.unwrap_or(2);
                Ok(match self.host.input_box(&prompt, &title, &default)? {
                    None => Variant::Bool(false),
                    Some(t) if ty & 1 != 0 => t
                        .trim()
                        .parse::<f64>()
                        .map(Variant::R8)
                        .unwrap_or(Variant::Bstr(t)),
                    Some(t) => Variant::Bstr(t),
                })
            })(),

            "GetOpenFilename" => (|| {
                let filter = a.string(0)?.unwrap_or_default();
                let title = a.string(2)?.unwrap_or_default();
                let multi = a.bool(4)?.unwrap_or(false);
                Ok(match self.host.get_open_filename(&filter, &title, multi)? {
                    None => Variant::Bool(false),
                    Some(files) if multi => Variant::Array(Box::new(xlcalc_variant::SafeArray {
                        elem: xlcalc_variant::VarType::Variant,
                        bounds: vec![(1, files.len() as u32)],
                        data: files.into_iter().map(Variant::Bstr).collect(),
                    })),
                    Some(files) => Variant::Bstr(files.into_iter().next().unwrap_or_default()),
                })
            })(),

            "OnTime" => (|| {
                let earliest = a.f64(0)?.ok_or_else(OmError::type_mismatch)?;
                let procedure = a.string(1)?.ok_or_else(OmError::type_mismatch)?;
                let latest = a.f64(2)?;
                let schedule = a.bool(3)?.unwrap_or(true);
                let request = OnTimeRequest {
                    earliest,
                    procedure,
                    latest,
                };
                self.host.on_time(&request, schedule);
                if schedule {
                    self.on_time.push(request);
                    Ok(Variant::Empty)
                } else {
                    let at = self.on_time.iter().position(|r| {
                        r.earliest == request.earliest
                            && r.procedure.eq_ignore_ascii_case(&request.procedure)
                    });
                    match at {
                        Some(i) => {
                            self.on_time.remove(i);
                            Ok(Variant::Empty)
                        }
                        None => Err(OmError::app_defined()),
                    }
                }
            })(),
            "Dialogs" => Err(OmError::unsupported("Application.Dialogs")),
            _ => return None,
        })
    }
}

fn text_arg(args: &[Variant], i: usize) -> String {
    use xlcalc_variant::{change_type, Locale, VarType};
    match args.get(i) {
        None | Some(Variant::Error(_)) => String::new(),
        Some(v) => match change_type(v, VarType::Bstr, Locale::EN_US, 0) {
            Ok(Variant::Bstr(s)) => s,
            _ => String::new(),
        },
    }
}

fn int_arg(args: &[Variant], i: usize) -> i32 {
    use xlcalc_variant::{change_type, Locale, VarType};
    match args.get(i) {
        None | Some(Variant::Error(_)) => 0,
        Some(v) => match change_type(v, VarType::I4, Locale::EN_US, 0) {
            Ok(Variant::I4(n)) => n,
            _ => 0,
        },
    }
}
