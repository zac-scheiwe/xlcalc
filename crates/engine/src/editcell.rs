use crate::address::CellAddr;
use crate::datetime::{split_serial, ymd_from_serial};
use crate::locale::Locale;
use crate::number::format_literal;
use crate::numfmt::{EditClass, NumberFormat};
use crate::value::Value;
use crate::workbook::Workbook;

fn short_date(loc: &Locale, y: i64, m: u32, d: u32) -> String {
    let pat: Vec<char> = loc.short_date_format.chars().collect();
    let mut out = String::new();
    let mut i = 0;
    while i < pat.len() {
        let c = pat[i];
        let mut n = 1;
        while i + n < pat.len() && pat[i + n] == c {
            n += 1;
        }
        let v = match c {
            'd' => Some(d as i64),
            'm' => Some(m as i64),
            'y' => Some(y),
            _ => None,
        };
        match (v, n) {
            (Some(v), 1) => out.push_str(&v.to_string()),
            (Some(v), 2) => out.push_str(&format!("{v:02}")),
            (Some(v), _) => out.push_str(&format!("{v:04}")),
            (None, _) => out.extend(std::iter::repeat_n(c, n)),
        }
        i += n;
    }
    out
}

impl Workbook {
    pub(crate) fn number_edit_text(&self, fmt: &str, x: f64) -> String {
        let class = NumberFormat::parse(fmt).map_or(EditClass::Plain, |f| f.edit_class());
        match class {
            EditClass::Plain => format_literal(x),
            EditClass::Percent => format!("{}%", format_literal(x * 100.0)),
            EditClass::Date { calendar } => {
                let ds = self.settings.date_system;
                let max = ds.max_serial();
                if x < 0.0 || x >= max + 1.0 {
                    return format_literal(x);
                }
                let days = x.floor();
                let frac = x - days;
                let (_, h, mi, s, _) = split_serial(frac);
                let time = format!(
                    "{}:{mi:02}:{s:02} {}",
                    if h % 12 == 0 { 12 } else { h % 12 },
                    if h < 12 { "am" } else { "pm" }
                );
                if !calendar && x < 1.0 {
                    return time;
                }
                let (y, m, d) = ymd_from_serial(ds, days as i64);
                let date = short_date(&self.locale, y, m, d);
                if frac == 0.0 && calendar {
                    date
                } else {
                    format!("{date}  {time}")
                }
            }
        }
    }

    pub(crate) fn const_edit_text(&self, addr: CellAddr, v: &Value) -> Option<String> {
        Some(match v {
            Value::Number(x) => self.number_edit_text(self.number_format(addr), *x),
            Value::Text(t) => t.to_string(),
            Value::Bool(b) => if *b { "TRUE" } else { "FALSE" }.to_string(),
            Value::Error(e) => e.to_string(),
            _ => return None,
        })
    }
}
