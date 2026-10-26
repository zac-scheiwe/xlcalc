use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::{self, RecalcReport};
use crate::number::format_general;
use crate::numfmt::NumberFormat;
use crate::value::Value;
use crate::workbook::{CalcMode, Workbook};

impl Workbook {
    pub fn display_text(&self, addr: CellAddr) -> String {
        let v = self.value(addr);
        match &v {
            Value::Blank => return String::new(),
            Value::Error(e) => return e.to_string(),
            _ => {}
        }
        let code = self.number_format(addr);
        if code.eq_ignore_ascii_case("general") {
            return match &v {
                Value::Number(n) => format_general(*n),
                Value::Text(s) => s.to_string(),
                Value::Bool(b) => if *b { "TRUE" } else { "FALSE" }.into(),
                _ => "#VALUE!".into(),
            };
        }
        match NumberFormat::parse(code) {
            Ok(f) => match f.format(&v, &self.locale, self.settings.date_system) {
                Ok(r) => r.text,
                Err(_) => "#####".into(),
            },
            Err(_) => match &v {
                Value::Number(n) => format_general(*n),
                Value::Text(s) => s.to_string(),
                _ => String::new(),
            },
        }
    }

    pub fn used_range(&self, sheet: SheetId) -> Option<Rect> {
        let sh = self.sheet(sheet)?;
        match (sh.cells.bounds(), sh.spills.bounds()) {
            (Some(a), Some(b)) => Some(a.union_bounds(&b)),
            (Some(a), None) | (None, Some(a)) => Some(a),
            (None, None) => None,
        }
    }

    pub fn addr(&self, sheet: SheetId, a1: &str) -> Option<CellAddr> {
        crate::parse_cell_ref(self, &a1.replace('$', ""), sheet)
    }

    pub fn is_semiautomatic(&self) -> bool {
        self.settings.mode == CalcMode::Automatic && self.settings.except_tables
    }

    pub fn delete_name_active(
        &mut self,
        name: &str,
        scope: Option<SheetId>,
    ) -> Option<RecalcReport> {
        let key = name.to_uppercase();
        let active = self.active_sheet();
        let first = self.sheets.first().map(|s| s.id);
        let scope = match scope {
            None if first.is_some_and(|f| self.names.get(&key, Some(f)).is_some()) => first,
            None if self.names.get(&key, None).is_some()
                && self.names.get(&key, Some(active)).is_some() =>
            {
                Some(active)
            }
            s => s,
        };
        self.delete_name(name, scope)
    }

    pub fn delete_name(&mut self, name: &str, scope: Option<SheetId>) -> Option<RecalcReport> {
        let key = name.to_uppercase();
        self.names.delete(&key, scope)?;
        calc::name_changed(self, &key);
        Some(self.after_edit(vec![]))
    }
}
