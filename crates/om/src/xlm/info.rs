use xlcalc::xlm::Operand;
use xlcalc::{CellAddr, Value, XlError};
use xlcalc_variant::Variant;

use super::{first_value, variant_value, xlm_formula_a1, AppEnv};
use crate::app::Call;
use xlcalc::xlm::XlmError;

const TEXT_CAP: usize = 255;

const FORMULA_CAP: usize = TEXT_CAP - 1;

impl AppEnv<'_, '_> {
    pub(super) fn get_workspace(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let na = Value::Error(XlError::NA);
        let b = |x: bool| Ok(Value::Bool(x));
        let n = |x: f64| Ok(Value::Number(x));
        let text = |s: &str| Ok(Value::Text(s.into()));

        let ty = args
            .first()
            .and_then(|op| self.number_arg(op))
            .map_or(0, |x| x.trunc() as i32);

        if ty < 1 || (100..200).contains(&ty) {
            return Ok(Value::Error(XlError::VALUE));
        }
        match ty {
            1 => text("Windows (64-bit) NT 10.00"),
            2 => text("16.0"),
            3 => {
                if self.option("FixedDecimal")? == Variant::Bool(true) {
                    self.option_value("FixedDecimalPlaces")
                } else {
                    n(0.0)
                }
            }
            4 => b(self.option("ReferenceStyle")? == Variant::I4(-4150)),
            8 => b(self.option("IgnoreRemoteRequests")? == Variant::Bool(false)),

            9 => match self.option_value("TransitionMenuKey")? {
                Value::Text(t) if t.is_empty() => Ok(na),
                v => Ok(v),
            },

            10 => n(0.0),
            15 => n(match self.option("WindowState")? {
                Variant::I4(-4140) => 2.0,
                Variant::I4(-4137) => 3.0,
                _ => 1.0,
            }),

            16 | 17 | 49 | 53 => Ok(na),

            18 => b(true),
            20 => self.grouped_sheets(),
            22 => self.option_value("DDEAppReturnCode"),

            24 => match self.option_value("AltStartupPath")? {
                Value::Text(t) if t.is_empty() => Ok(na),
                v => Ok(v),
            },

            25 | 31 | 45 | 59 => b(false),
            28 => n(1.0),
            63 => self.option_value("MailSystem"),
            69 | 72 => b(true),
            10001 => n(0.0),
            5..=7 | 23 | 26 | 27 | 32 | 36 | 40 | 46 | 48 | 50 | 54..=58 | 64..=67 | 70 => {
                let member = match ty {
                    5 => "DisplayScrollBars",
                    6 => "DisplayStatusBar",
                    7 => "DisplayFormulaBar",
                    23 => "StartupPath",
                    26 => "UserName",
                    27 => "OrganizationName",
                    32 => "Path",
                    36 => "CellDragAndDrop",
                    40 => "ScreenUpdating",
                    46 => "MoveAfterReturn",
                    48 => "LibraryPath",
                    50 => "DisplayFullScreen",
                    54 => "EditDirectlyInCell",
                    55 => "AlertBeforeOverwriting",
                    56 => "StandardFont",
                    57 => "StandardFontSize",
                    58 => "DisplayRecentFiles",
                    64 => "AskToUpdateLinks",
                    65 => "CopyObjectsWithCells",
                    66 => "SheetsInNewWorkbook",
                    67 => "DefaultFilePath",
                    _ => "PromptForSummaryInfo",
                };
                self.option_value(member)
            }

            11..=14
            | 19
            | 21
            | 29
            | 30
            | 33..=35
            | 37..=39
            | 41..=44
            | 51
            | 52
            | 60..=62
            | 68
            | 71
            | 73
            | 202
            | 204..=206
            | 10000
            | 10002 => Err(XlmError::Unsupported(format!("GET.WORKSPACE({ty})"))),

            _ => Ok(Value::Error(XlError::VALUE)),
        }
    }

    fn number_arg(&self, op: &Operand) -> Option<f64> {
        match first_value(self.wbr(), op) {
            Value::Number(x) => Some(x),
            Value::Bool(x) => Some(f64::from(u8::from(x))),
            Value::Blank => Some(0.0),
            Value::Text(t) => self.wbr().number_in_text(&t, self.wbr().locale()),
            _ => None,
        }
    }

    fn option(&mut self, member: &str) -> Result<Variant, XlmError> {
        let root = self.app.handle();
        self.inv(root, member, Call::GET, &[], &[])
    }

    fn option_value(&mut self, member: &str) -> Result<Value, XlmError> {
        let v = self.option(member)?;
        Ok(variant_value(&v).unwrap_or(Value::Error(XlError::NA)))
    }

    fn grouped_sheets(&mut self) -> Result<Value, XlmError> {
        let na = Value::Error(XlError::NA);
        let window = Self::dispatch(self.option("ActiveWindow")?)?;
        let sheets = Self::dispatch(self.inv(window, "SelectedSheets", Call::GET, &[], &[])?)?;
        let Variant::I4(count) = self.inv(sheets, "Count", Call::GET, &[], &[])? else {
            return Ok(na);
        };
        if count < 2 {
            return Ok(na);
        }
        let book = self.active_book_obj()?;
        let book_name = self.get_text(book, "Name").unwrap_or_default();

        let mut names = Vec::new();
        for i in 1..=count {
            let key = [Variant::I4(i)];
            let s = Self::dispatch(self.inv(sheets, "Item", Call::GET, &key, &[])?)?;
            names.push(self.get_text(s, "Name").unwrap_or_default());
        }
        let active = Self::dispatch(self.inv(window, "ActiveSheet", Call::GET, &[], &[])?)?;
        let active = self.get_text(active, "Name").unwrap_or_default();

        let mut ordered = Vec::with_capacity(names.len());
        if let Some(at) = names.iter().position(|n| *n == active) {
            ordered.push(names.remove(at));
        }
        ordered.extend(names.into_iter().rev());
        let n = ordered.len() as u32;
        let out = ordered
            .iter()
            .map(|name| Value::Text(format!("[{book_name}]{name}").into()))
            .collect::<Vec<_>>();
        Ok(xlcalc::Array::new(1, n, out).into_value())
    }

    fn info_cell(&mut self, op: Option<&Operand>) -> Option<CellAddr> {
        let sheet = self.wbr().active_sheet();
        let (row, col) = self.wbr().active_cell_of(sheet);
        let at = match op {
            None | Some(Operand::Missing) => CellAddr::new(sheet, row, col),
            Some(Operand::Ref(r)) => {
                let a = r.areas().first()?;
                CellAddr::new(a.sheet, a.rect.r0, a.rect.c0)
            }
            Some(op) => {
                let Value::Text(t) = first_value(self.wbr(), op) else {
                    return None;
                };
                let a1 = xlm_formula_a1(&format!("={t}"), row, col)?;
                let wb = self.wbr();
                xlcalc::parse_cell_ref(wb, &a1.trim_start_matches('=').replace('$', ""), sheet)?
            }
        };

        let kind = self.wbr().sheet(at.sheet)?.kind;
        (kind == xlcalc::SheetKind::Worksheet || kind.is_macro()).then_some(at)
    }

    pub(super) fn get_formula(&mut self, args: &[Operand]) -> Value {
        let value = Value::Error(XlError::VALUE);
        if matches!(args.first(), None | Some(Operand::Missing)) {
            return value;
        }
        let Some(at) = self.info_cell(args.first()) else {
            return value;
        };
        let wb = self.wbr();

        let source = wb.spill_anchor(at).unwrap_or(at);
        let formula = wb.formula_text_r1c1(source);
        let cell = wb.value(at);
        if (formula.is_some() || !matches!(cell, Value::Blank))
            && wb.cell_style(at).formula_hidden
            && self.app.contents_protected(self.book, at.sheet)
        {
            return Value::Error(XlError::NA);
        }
        let cap = if formula.is_some() {
            FORMULA_CAP
        } else {
            TEXT_CAP
        };

        let text = formula.unwrap_or_else(|| match cell {
            Value::Blank => String::new(),
            Value::Number(n) => xlcalc::format_literal(n),
            Value::Text(t) => t.to_string(),
            Value::Bool(b) => if b { "TRUE" } else { "FALSE" }.into(),
            Value::Error(e) => e.to_string(),
            Value::Array(_) | Value::Lambda(_) => "#VALUE!".into(),
        });
        if text.encode_utf16().count() > cap {
            return value;
        }
        Value::Text(text.as_str().into())
    }

    pub(super) fn get_note(&mut self, args: &[Operand]) -> Value {
        let value = Value::Error(XlError::VALUE);
        let Some(at) = self.info_cell(args.first()) else {
            return value;
        };
        let note: Vec<u16> = self
            .wbr()
            .note_at(at.sheet, at.row, at.col)
            .map_or_else(Vec::new, |n| n.text.encode_utf16().collect());

        let int = |env: &Self, i: usize, min: f64, default: usize| match args.get(i) {
            None | Some(Operand::Missing) => Some(default),
            Some(op) => {
                let x = env.number_arg(op)?.trunc();
                (x >= min && x <= f64::from(i32::MAX)).then_some(x as usize)
            }
        };
        let Some(start) = int(self, 1, 1.0, 1) else {
            return value;
        };
        let Some(count) = int(self, 2, 0.0, note.len()) else {
            return value;
        };
        let from = (start - 1).min(note.len());
        let take = count.min(note.len() - from).min(TEXT_CAP);
        Value::Text(String::from_utf16_lossy(&note[from..from + take]).into())
    }
}
