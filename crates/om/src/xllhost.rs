#![cfg(windows)]

use xlcalc::{Arg, CellAddr, FunctionHost, Value};
use xlcalc_variant::Variant;
use xlcalc_vba::MacroLookup;
use xlcalc_xll::XllHost;

use crate::app::{Application, BookId};
use crate::conv::Args;
use crate::error::OmError;

impl Application<'_> {
    pub(crate) fn register_xll(&mut self, a: &Args) -> Result<Variant, OmError> {
        let path = a.string(0)?.ok_or_else(OmError::app_defined)?;
        Ok(Variant::Bool(self.load_xll(&path)))
    }

    pub(crate) fn load_xll(&mut self, path: &str) -> bool {
        let Ok(host) = XllHost::load(path) else {
            return false;
        };
        if !self
            .xlls
            .iter()
            .any(|x| x.path().eq_ignore_ascii_case(host.path()))
        {
            self.xlls.push(host);
            self.refresh_function_hosts(true);
        }
        true
    }

    pub(crate) fn run_xll_function(
        &mut self,
        book: BookId,
        name: &str,
        a: &Args,
    ) -> Result<Option<Variant>, OmError> {
        let b = self.book(book)?;
        if let Some(h) = &b.vba.host {
            if matches!(h.project().lookup_macro(name), MacroLookup::Found(_)) {
                return Ok(None);
            }
        }
        let upper = name.to_uppercase();
        if !self.xlls.iter().any(|x| x.signature(&upper).is_some()) {
            return Ok(None);
        }
        let mut texts = Vec::new();
        for v in &a.values[1..] {
            texts.push(match v {
                None => String::new(),
                Some(v) => self.formula_arg(book, v)?,
            });
        }
        while texts.last().is_some_and(String::is_empty) {
            texts.pop();
        }
        let wb = &self.book(book)?.wb;
        let at = CellAddr::new(wb.active_sheet(), 0, 0);
        let r = wb.cfv_eval(&format!("{upper}({})", texts.join(",")), at, at);
        Ok(Some(crate::vba::value_variant(&r)))
    }

    fn formula_arg(&self, book: BookId, v: &Variant) -> Result<String, OmError> {
        Ok(match self.engine_arg(v)? {
            Arg::Missing => String::new(),
            Arg::Value(Value::Number(n)) => xlcalc::format_general(n),
            Arg::Value(Value::Text(t)) => format!("\"{}\"", t.replace('"', "\"\"")),
            Arg::Value(Value::Bool(b)) => if b { "TRUE" } else { "FALSE" }.to_string(),
            Arg::Value(Value::Blank) => String::new(),
            Arg::Ref(areas) | Arg::XRef(areas) => {
                let wb = &self.book(book)?.wb;
                let parts: Vec<String> = areas
                    .iter()
                    .map(|a| {
                        let r = a.rect;
                        let name = wb
                            .sheets()
                            .find(|s| s.id == a.sheet)
                            .map_or("", |s| s.name.as_str());
                        format!(
                            "'{}'!{}{}:{}{}",
                            name.replace('\'', "''"),
                            xlcalc::col_to_letters(r.c0),
                            r.r0 + 1,
                            xlcalc::col_to_letters(r.c1),
                            r.r1 + 1
                        )
                    })
                    .collect();
                format!("({})", parts.join(","))
            }
            Arg::Value(_) => return Err(OmError::type_mismatch()),
        })
    }
}

pub(crate) struct Unloaded(pub(crate) XllHost);

impl FunctionHost for Unloaded {
    fn signature(&self, name: &str) -> Option<xlcalc::Signature> {
        self.0.signature(name)
    }

    fn call(
        &self,
        _name: &str,
        _args: &[Arg],
        _cx: &mut xlcalc::CallContext<'_>,
    ) -> Result<Value, xlcalc::RuntimeError> {
        Err(xlcalc::RuntimeError)
    }
}
