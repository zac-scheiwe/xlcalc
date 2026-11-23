use xlcalc::xlm::{Operand, XlmError};
use xlcalc::{Value, XlError};
use xlcalc_variant::{ObjectId, Variant};

use super::fmt::A;
use super::AppEnv;
use crate::app::Call;

const VALUE: Result<Value, XlmError> = Ok(Value::Error(XlError::VALUE));
const TRUE: Result<Value, XlmError> = Ok(Value::Bool(true));

impl AppEnv<'_, '_> {
    pub(super) fn active_window(&mut self) -> Result<ObjectId, XlmError> {
        let root = self.app.handle();
        Self::dispatch(self.inv(root, "ActiveWindow", Call::GET, &[], &[])?)
    }

    fn active_sheet_obj(&mut self) -> Result<ObjectId, XlmError> {
        let root = self.app.handle();
        Self::dispatch(self.inv(root, "ActiveSheet", Call::GET, &[], &[])?)
    }

    fn win_put(&mut self, member: &str, v: Variant) -> Result<(), XlmError> {
        let w = self.active_window()?;
        self.inv(w, member, Call::PUT, &[v], &[])?;
        Ok(())
    }

    pub(super) fn window_command(
        &mut self,
        name: &str,
        args: &[Operand],
    ) -> Result<Value, XlmError> {
        match name {
            "ZOOM" => self.zoom(args),
            "FREEZE.PANES" => self.freeze_panes(args),
            "DISPLAY" => self.display(args),
            "WINDOW.TITLE" => {
                let title = match self.a(args, 0) {
                    A::Miss => None,
                    A::Text(t) => Some(t),
                    A::Num(n) => Some(format!("{n}")),
                    A::Bool(b) => Some(if b { "TRUE" } else { "FALSE" }.to_string()),
                    A::Err => return VALUE,
                };
                if let Some((b, n)) = self.app.current_window() {
                    if let Ok(bk) = self.app.book_mut(b) {
                        match title {
                            Some(t) => bk.window.captions.insert(n, t),
                            None => bk.window.captions.remove(&n),
                        };
                    }
                }
                TRUE
            }
            "WINDOW.MAXIMIZE" | "WINDOW.MINIMIZE" | "WINDOW.RESTORE" => {
                if !matches!(self.a(args, 0), A::Miss | A::Text(_)) {
                    return VALUE;
                }
                if let Some((b, _)) = self.app.current_window() {
                    let _ = match name {
                        "WINDOW.MAXIMIZE" => self.app.set_window_state(b, -4137),
                        "WINDOW.MINIMIZE" => self.app.set_window_state(b, -4140),
                        _ => self.app.restore_window(b),
                    };
                }
                TRUE
            }
            "SHOW.ACTIVE.CELL" => TRUE,
            "NEW.WINDOW" => {
                let w = self.active_window()?;
                self.inv(w, "NewWindow", Call::VB, &[], &[])?;
                TRUE
            }
            "ARRANGE.ALL" => match self.a(args, 0) {
                A::Miss | A::Bool(true) | A::Num(1.0..=4.0) => TRUE,
                _ => VALUE,
            },
            "ACTIVATE.NEXT" | "ACTIVATE.PREV" => {
                self.app.rotate_window(name == "ACTIVATE.NEXT");
                TRUE
            }
            other => Err(XlmError::Unsupported(other.to_string())),
        }
    }

    fn zoom(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let n = match self.a(args, 0) {
            A::Miss | A::Bool(true) => {
                if let Some((b, _)) = self.app.current_window() {
                    let _ = self.app.zoom_to_selection(b);
                }
                return TRUE;
            }
            A::Bool(false) => 100.0,
            A::Num(n) => n,

            A::Text(t) => match t.trim().parse::<f64>() {
                Ok(n) => n,
                Err(_) => return VALUE,
            },
            A::Err => return VALUE,
        };
        if !(10.0..401.0).contains(&n) {
            return VALUE;
        }
        self.win_put("Zoom", Variant::R8(n.trunc()))?;
        TRUE
    }

    fn freeze_panes(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let on = match self.a(args, 0) {
            A::Miss => true,
            a => match a.flag() {
                Some(b) => b,
                None => return VALUE,
            },
        };
        let split = |a: A| match a {
            A::Num(n) if n >= 0.0 => Some(n.trunc() as u32),
            _ => None,
        };
        let (cols, rows) = (split(self.a(args, 1)), split(self.a(args, 2)));
        let w = self.app.current_window();
        if on && (cols.is_some() || rows.is_some()) {
            if let Some((b, _)) = w {
                let _ = self.app.freeze_at(b, rows.unwrap_or(0), cols.unwrap_or(0));
            }
        } else if !on {
            if let Some((b, _)) = w {
                let _ = self.app.freeze_at(b, 0, 0);
            }
        } else {
            self.win_put("FreezePanes", Variant::Bool(true))?;
        }
        TRUE
    }

    fn display(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let mut flags: [Option<bool>; 4] = [None; 4];
        for (i, f) in flags.iter_mut().enumerate() {
            match self.a(args, i) {
                A::Miss => {}
                a => match a.flag() {
                    Some(b) => *f = Some(b),
                    None => return VALUE,
                },
            }
        }
        let color = match self.a(args, 4) {
            A::Miss => None,
            A::Bool(b) => Some(i32::from(b)),
            A::Num(n) if (0.0..57.0).contains(&n) => Some(n.trunc() as i32),
            _ => return VALUE,
        };
        let mut tail: [Option<bool>; 2] = [None; 2];
        for (i, f) in tail.iter_mut().enumerate() {
            match self.a(args, 6 + i) {
                A::Miss => {}
                a => match a.flag() {
                    Some(b) => *f = Some(b),
                    None => return VALUE,
                },
            }
        }
        match self.a(args, 8) {
            A::Miss => {}

            A::Bool(true) => {}
            A::Num(n) if (1.0..4.0).contains(&n) => {}
            _ => return VALUE,
        }
        let names = [
            "DisplayFormulas",
            "DisplayGridlines",
            "DisplayHeadings",
            "DisplayZeros",
        ];
        for (name, f) in names.iter().zip(flags) {
            if let Some(b) = f {
                self.win_put(name, Variant::Bool(b))?;
            }
        }
        if let Some(c) = color {
            if c == 0 {
                if let Some((b, _)) = self.app.current_window() {
                    let _ = self.app.set_gridline_auto(b);
                }
            } else {
                self.win_put("GridlineColorIndex", Variant::I4(c))?;
            }
        }
        if let Some(b) = tail[0] {
            self.win_put("DisplayOutline", Variant::Bool(b))?;
        }
        if let Some(b) = tail[1] {
            let s = self.active_sheet_obj()?;
            self.inv(s, "DisplayPageBreaks", Call::PUT, &[Variant::Bool(b)], &[])?;
        }
        TRUE
    }

    fn scroll_bars_off(&mut self) -> bool {
        let root = self.app.handle();
        matches!(
            self.inv(root, "DisplayScrollBars", Call::GET, &[], &[]),
            Ok(Variant::Bool(false))
        )
    }

    pub(super) fn get_window(&mut self, args: &[Operand]) -> Result<Value, XlmError> {
        let ty = match self.a(args, 0) {
            A::Num(n) => n.trunc() as i32,
            _ => return VALUE,
        };
        let w = self.active_window()?;
        let flag = |s: &mut Self, m: &str| {
            let v = s.inv(w, m, Call::GET, &[], &[]);
            Ok(Value::Bool(matches!(v, Ok(Variant::Bool(true)))))
        };
        let num = |s: &mut Self, m: &str| {
            let v = s.inv(w, m, Call::GET, &[], &[]);
            Ok(match v {
                Ok(Variant::R8(x)) => Value::Number(x),
                Ok(Variant::I4(x)) => Value::Number(f64::from(x)),
                _ => Value::Error(XlError::NA),
            })
        };
        match ty {
            1 | 30 => {
                let root = self.app.handle();
                let book = self.active_book_obj()?;
                let b = self.get_text(book, "Name").unwrap_or_default();
                let s = Self::dispatch(self.inv(root, "ActiveSheet", Call::GET, &[], &[])?)?;
                let sn = self.get_text(s, "Name").unwrap_or_default();
                Ok(Value::Text(format!("[{b}]{sn}").into()))
            }
            2 | 3 => Ok(Value::Number(1.0)),
            8 => flag(self, "DisplayFormulas"),
            9 => flag(self, "DisplayGridlines"),
            10 => flag(self, "DisplayHeadings"),
            11 => flag(self, "DisplayZeros"),
            12 => {
                let v = self.inv(w, "GridlineColorIndex", Call::GET, &[], &[])?;
                Ok(Value::Number(match v {
                    Variant::R8(x) => x,
                    _ => 0.0,
                }))
            }

            13 | 14
                if matches!(
                    self.inv(w, "FreezePanes", Call::GET, &[], &[]),
                    Ok(Variant::Bool(true))
                ) =>
            {
                Ok(Value::Number(1.0))
            }
            13 => num(self, "ScrollColumn"),
            14 => num(self, "ScrollRow"),
            17..=19 => {
                let Some((b, _)) = self.app.current_window() else {
                    return Ok(Value::Error(XlError::NA));
                };
                let (pane, col, row) = self
                    .app
                    .pane_flags(b)
                    .map_err(|_| XlmError::Failed("pane".into()))?;
                Ok(match ty {
                    17 => Value::Number(pane),
                    18 => Value::Bool(col),
                    _ => Value::Bool(row),
                })
            }
            24 => {
                let frozen = matches!(
                    self.inv(w, "FreezePanes", Call::GET, &[], &[]),
                    Ok(Variant::Bool(true))
                );
                Ok(Value::Bool(frozen))
            }
            20 | 23 => {
                let st = match self.inv(w, "WindowState", Call::GET, &[], &[]) {
                    Ok(Variant::I4(x)) => x,
                    _ => -4143,
                };
                Ok(if ty == 20 {
                    Value::Bool(st == -4137)
                } else {
                    Value::Number(match st {
                        -4140 => 2.0,
                        -4137 => 3.0,
                        _ => 1.0,
                    })
                })
            }
            21 => Ok(Value::Bool(false)),
            22 => flag(self, "DisplayOutline"),
            25 => num(self, "Zoom"),
            26 | 27 | 29 if self.scroll_bars_off() => Ok(Value::Bool(false)),
            26 => flag(self, "DisplayHorizontalScrollBar"),
            27 => flag(self, "DisplayVerticalScrollBar"),
            29 => {
                let v = self.inv(w, "DisplayWorkbookTabs", Call::GET, &[], &[]);
                Ok(Value::Bool(!matches!(v, Ok(Variant::Bool(false)))))
            }
            28 => num(self, "TabRatio"),
            31 => {
                let v = self.inv(w, "Caption", Call::GET, &[], &[])?;
                Ok(match v {
                    Variant::Bstr(s) => Value::Text(s.to_string().into()),
                    _ => Value::Error(XlError::NA),
                })
            }
            32..=1000 => VALUE,
            _ => Ok(Value::Error(XlError::NA)),
        }
    }
}
