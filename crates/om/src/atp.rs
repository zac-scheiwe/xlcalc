use xlcalc::atp::{Arg, AtpError};
use xlcalc_variant::Variant;

use crate::app::Application;
use crate::conv::Args;
use crate::error::OmError;

impl Application<'_> {
    pub(crate) fn atp_run(&mut self, proc: &str, a: &Args) -> Option<Result<Variant, OmError>> {
        if !self.addin_installed("Analysis ToolPak - VBA") {
            return None;
        }
        let mut args = Vec::new();
        for v in a.values.iter().skip(1) {
            args.push(match v {
                None | Some(Variant::Error(_)) | Some(Variant::Empty) => Arg::Missing,
                Some(Variant::Bool(b)) => Arg::Bool(*b),
                Some(Variant::Bstr(s)) => Arg::Text(s.clone()),
                Some(Variant::I2(n)) => Arg::Num(f64::from(*n)),
                Some(Variant::I4(n)) => Arg::Num(f64::from(*n)),
                Some(Variant::R4(n)) => Arg::Num(f64::from(*n)),
                Some(Variant::R8(n)) => Arg::Num(*n),
                Some(d @ Variant::Dispatch(_)) => match self.range_arg(Some(d)) {
                    Ok((sheet, rect)) => Arg::Range(sheet, rect),
                    Err(e) => return Some(Err(e)),
                },
                Some(_) => Arg::Missing,
            });
        }
        let book = self.active?;
        let run = {
            let wb = &self.book(book).ok()?.wb;
            wb.atp_run(proc, &args)?
        };
        Some(match run {
            Ok(run) => {
                let wb = &mut self.book_mut(book).ok()?.wb;
                wb.atp_apply(run);
                Ok(Variant::Empty)
            }

            Err(AtpError::Message(_)) => Ok(Variant::Empty),
        })
    }
}
