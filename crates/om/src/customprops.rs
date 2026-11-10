use std::cmp::Ordering;

use xlcalc::SheetId;
use xlcalc_variant::{change_type, VarType, Variant};

use crate::app::{Application, BookId, Object};
use crate::conv::{variant_locale, Args};
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

#[derive(Clone, Debug)]
pub(crate) struct CustomProp {
    pub id: u32,
    pub name: String,
    pub value: String,
}

fn range_error() -> OmError {
    OmError::exception(0x800A_03EC_u32 as i32, None)
}

fn bad_index() -> OmError {
    OmError::exception(0x8002_000B_u32 as i32, None)
}

fn out_of_range() -> OmError {
    OmError::exception(
        0x8007_0057_u32 as i32,
        Some("Value does not fall within the expected range."),
    )
}

fn fold(c: char) -> char {
    match c {
        'à'..='å' | 'À'..='Å' => 'a',
        'è'..='ë' | 'È'..='Ë' => 'e',
        'ì'..='ï' | 'Ì'..='Ï' => 'i',
        'ò'..='ö' | 'Ò'..='Ö' => 'o',
        'ù'..='ü' | 'Ù'..='Ü' => 'u',
        'ç' | 'Ç' => 'c',
        'ñ' | 'Ñ' => 'n',
        _ => c.to_lowercase().next().unwrap_or(c),
    }
}

fn key(name: &str) -> Vec<(u8, char)> {
    name.chars()
        .map(|c| {
            let l = fold(c);
            let class = if l.is_ascii_digit() {
                1
            } else if l.is_alphabetic() {
                2
            } else {
                0
            };
            (class, l)
        })
        .collect()
}

fn order(a: &str, b: &str) -> Ordering {
    key(a).cmp(&key(b))
}

fn same(a: &str, b: &str) -> bool {
    a.to_lowercase() == b.to_lowercase()
}

fn value_text(v: &Variant, lcid: u32) -> Result<String, OmError> {
    match v {
        Variant::Bstr(s) => Ok(s.clone()),
        Variant::Empty | Variant::Null => Ok(String::new()),
        Variant::Bool(b) => Ok(if *b { "-1" } else { "0" }.to_string()),
        v => match change_type(v, VarType::Bstr, variant_locale(lcid), 0) {
            Ok(Variant::Bstr(s)) => Ok(s),
            _ => Err(OmError::type_mismatch()),
        },
    }
}

impl Application<'_> {
    fn custom_props(&self, b: BookId, s: SheetId) -> Vec<CustomProp> {
        self.custom_props.get(&(b, s)).cloned().unwrap_or_default()
    }

    fn edit_custom(&mut self, b: BookId, s: SheetId, f: impl FnOnce(&mut Vec<CustomProp>)) {
        let list = self.custom_props.entry((b, s)).or_default();
        f(list);
        list.sort_by(|x, y| order(&x.name, &y.name));
    }

    pub(crate) fn custom_properties_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let list = self.custom_props(b, s);
        match m.name {
            "Count" => Ok(Variant::I4(list.len() as i32)),
            "Item" | "_Default" => {
                let n = match a.get(0) {
                    Some(Variant::Bstr(t)) => t
                        .trim()
                        .parse::<f64>()
                        .map_err(|_| OmError::exception(0x8002_0005_u32 as i32, None))?,
                    Some(v) => crate::controls::num_arg(v, a.lcid)?,
                    None => return Err(OmError::app_defined()),
                };
                let i = n.round_ties_even();
                if i < 1.0 {
                    return Err(range_error());
                }
                match list.get(i as usize - 1) {
                    Some(p) => Ok(self.alloc(Object::CustomProperty(b, s, p.id))),
                    None => Err(bad_index()),
                }
            }
            "_NewEnum" => {
                let items = list
                    .iter()
                    .map(|p| self.alloc(Object::CustomProperty(b, s, p.id)))
                    .collect();
                Ok(self.alloc_enumerator(items))
            }
            "Add" => {
                let name = a.string(0)?.unwrap_or_default();
                let value = match a.get(1) {
                    Some(v) => value_text(v, a.lcid)?,
                    None => String::new(),
                };
                if name.encode_utf16().count() > 255 {
                    return Err(OmError::exception(0x8007_000E_u32 as i32, None));
                }
                if list.iter().any(|p| same(&p.name, &name)) {
                    return Err(bad_index());
                }
                self.next_custom += 1;
                let id = self.next_custom;
                self.edit_custom(b, s, |l| l.push(CustomProp { id, name, value }));
                Ok(self.alloc(Object::CustomProperty(b, s, id)))
            }
            "Parent" => Ok(self.alloc(Object::Worksheet(b, s))),
            "Application" => Ok(Variant::Dispatch(Some(self.handle()))),
            "Creator" => Ok(Variant::I4(0x5843_454C)),
            _ => Err(OmError::app_defined()),
        }
    }

    pub(crate) fn custom_property_member(
        &mut self,
        b: BookId,
        s: SheetId,
        id: u32,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let list = self.custom_props(b, s);
        let found = list.iter().find(|p| p.id == id).cloned();
        let put = m.kind == InvokeKind::Put;
        match (m.name, put) {
            ("Name", false) => Ok(Variant::Bstr(found.map(|p| p.name).unwrap_or_default())),
            ("Value" | "_Default", false) => {
                Ok(Variant::Bstr(found.map(|p| p.value).unwrap_or_default()))
            }
            ("Parent", _) => Ok(self.alloc(Object::Worksheet(b, s))),
            ("Application", _) => Ok(Variant::Dispatch(Some(self.handle()))),
            ("Creator", _) => Ok(Variant::I4(0x5843_454C)),
            ("Delete", _) => {
                if found.is_none() {
                    return Err(OmError::exception(0x800A_01A8_u32 as i32, None));
                }
                self.edit_custom(b, s, |l| l.retain(|x| x.id != id));
                Ok(Variant::Empty)
            }
            ("Name", true) => {
                if found.is_none() {
                    return Err(OmError::object_required());
                }
                let name = crate::controls::put_text(a)?;
                if name.encode_utf16().count() > 255 {
                    return Err(OmError::exception(0x8007_000E_u32 as i32, None));
                }
                if list.iter().any(|x| same(&x.name, &name)) {
                    return Err(out_of_range());
                }
                self.edit_custom(b, s, |l| {
                    if let Some(x) = l.iter_mut().find(|x| x.id == id) {
                        x.name = name;
                    }
                });
                Ok(Variant::Empty)
            }
            ("Value", true) => {
                if found.is_none() {
                    return Err(OmError::object_required());
                }
                let v = match a.put.as_ref() {
                    None | Some(Variant::Empty | Variant::Null) => return Err(range_error()),
                    Some(v) => value_text(v, a.lcid)?,
                };
                self.edit_custom(b, s, |l| {
                    if let Some(x) = l.iter_mut().find(|x| x.id == id) {
                        x.value = v;
                    }
                });
                Ok(Variant::Empty)
            }
            _ => Err(OmError::app_defined()),
        }
    }
}
