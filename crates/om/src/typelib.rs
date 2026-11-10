use std::cmp::Ordering;

pub use crate::members::{ENUMERATIONS, INTERFACES};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Guid(pub u128);

impl Guid {
    pub const fn parse(text: &str) -> Guid {
        let b = text.as_bytes();
        assert!(b.len() == 36, "a GUID has 36 characters");
        let mut v = 0u128;
        let mut i = 0;
        while i < b.len() {
            if i == 8 || i == 13 || i == 18 || i == 23 {
                assert!(
                    b[i] == b'-',
                    "a GUID has dashes after 8, 12, 16 and 20 digits"
                );
            } else {
                let d = match b[i] {
                    c @ b'0'..=b'9' => c - b'0',
                    c @ b'a'..=b'f' => c - b'a' + 10,
                    c @ b'A'..=b'F' => c - b'A' + 10,
                    _ => panic!("a GUID digit is hexadecimal"),
                };
                v = (v << 4) | d as u128;
            }
            i += 1;
        }
        Guid(v)
    }
}

#[derive(Debug)]
pub struct Interface {
    pub name: &'static str,
    pub iid: Guid,

    pub dual: bool,

    pub hidden: bool,

    pub members: &'static [Member],
}

#[derive(Debug)]
pub struct Member {
    pub name: &'static str,
    pub dispid: i32,
    pub kind: InvokeKind,

    pub ret: Ty,

    pub params: &'static [Param],

    pub hidden: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum InvokeKind {
    Func = 1,
    Get = 2,
    Put = 4,
    PutRef = 8,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Param {
    pub name: &'static str,
    pub ty: Ty,
    pub opt: Opt,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Opt {
    Required,

    Optional,

    Default(DefaultValue),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum DefaultValue {
    I2(i16),
    I4(i32),
    R4(f32),
    Bool(bool),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Ty {
    Void,
    Variant,
    Bool,
    I4,
    Int,
    R4,
    R8,
    Bstr,
    Date,
    Dispatch,
    Unknown,

    VariantArray,

    Enum(&'static str),

    Object(&'static str),

    ByRef(&'static Ty),
}

#[derive(Debug)]
pub struct Enumeration {
    pub name: &'static str,

    pub constants: &'static [(&'static str, i32)],
}

fn cmp_ignore_case(a: &str, b: &str) -> Ordering {
    let a = a.bytes().map(|c| c.to_ascii_lowercase());
    a.cmp(b.bytes().map(|c| c.to_ascii_lowercase()))
}

pub fn interface(name: &str) -> Option<&'static Interface> {
    [
        INTERFACES,
        crate::vbide_members::INTERFACES,
        crate::office_members::INTERFACES,
    ]
    .into_iter()
    .find_map(|table| {
        let i = table
            .binary_search_by(|t| cmp_ignore_case(t.name, name))
            .ok()?;
        Some(&table[i])
    })
}

pub fn enumeration(name: &str) -> Option<&'static Enumeration> {
    [ENUMERATIONS, crate::vbide_members::ENUMERATIONS]
        .into_iter()
        .find_map(|table| {
            let i = table
                .binary_search_by(|e| cmp_ignore_case(e.name, name))
                .ok()?;
            Some(&table[i])
        })
}

impl Interface {
    pub fn named(&self, name: &str) -> &'static [Member] {
        let members = self.members;
        let lo = members.partition_point(|m| cmp_ignore_case(m.name, name) == Ordering::Less);
        let n = members[lo..].partition_point(|m| cmp_ignore_case(m.name, name) == Ordering::Equal);
        &members[lo..lo + n]
    }

    pub fn member(&self, dispid: i32, kind: InvokeKind) -> Option<&'static Member> {
        self.members
            .iter()
            .find(|m| m.dispid == dispid && m.kind == kind)
    }
}

pub(crate) const fn m(
    name: &'static str,
    dispid: i32,
    kind: InvokeKind,
    ret: Ty,
    params: &'static [Param],
) -> Member {
    Member {
        name,
        dispid,
        kind,
        ret,
        params,
        hidden: false,
    }
}

pub(crate) const fn h(
    name: &'static str,
    dispid: i32,
    kind: InvokeKind,
    ret: Ty,
    params: &'static [Param],
) -> Member {
    Member {
        hidden: true,
        ..m(name, dispid, kind, ret, params)
    }
}

pub(crate) const fn p(name: &'static str, ty: Ty) -> Param {
    Param {
        name,
        ty,
        opt: Opt::Required,
    }
}

pub(crate) const fn o(name: &'static str, ty: Ty) -> Param {
    Param {
        name,
        ty,
        opt: Opt::Optional,
    }
}

pub(crate) const fn d(name: &'static str, ty: Ty, default: DefaultValue) -> Param {
    Param {
        name,
        ty,
        opt: Opt::Default(default),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dispid(iface: &str, name: &str) -> i32 {
        let members = interface(iface).unwrap().named(name);
        assert!(!members.is_empty(), "{iface}.{name}");
        assert!(members.iter().all(|m| m.dispid == members[0].dispid));
        members[0].dispid
    }

    #[test]
    fn totals() {
        assert_eq!(INTERFACES.len(), 388);
        assert_eq!(INTERFACES.iter().filter(|t| t.dual).count(), 31);
        let members: usize = INTERFACES.iter().map(|t| t.members.len()).sum();
        assert_eq!(members, 10_753);
        let params = || {
            INTERFACES
                .iter()
                .flat_map(|t| t.members)
                .flat_map(|m| m.params)
        };
        assert_eq!(params().count(), 8_902);
        assert_eq!(params().filter(|p| p.opt != Opt::Required).count(), 4_266);
        let defaults = params().filter(|p| matches!(p.opt, Opt::Default(_)));
        assert_eq!(defaults.count(), 187);
        assert_eq!(ENUMERATIONS.len(), 283);
        let constants: usize = ENUMERATIONS.iter().map(|e| e.constants.len()).sum();
        assert_eq!(constants, 2_333);
    }

    #[test]
    fn sorted_for_lookup() {
        let sorted = |a: &str, b: &str| cmp_ignore_case(a, b) == Ordering::Less;
        assert!(INTERFACES.windows(2).all(|w| sorted(w[0].name, w[1].name)));
        assert!(ENUMERATIONS
            .windows(2)
            .all(|w| sorted(w[0].name, w[1].name)));
        for t in INTERFACES {
            assert!(t.members.windows(2).all(|w| {
                let by_name = cmp_ignore_case(w[0].name, w[1].name);
                by_name == Ordering::Less || (by_name == Ordering::Equal && w[0].kind < w[1].kind)
            }));
        }
    }

    #[test]
    fn spot_checks() {
        assert_eq!(dispid("Range", "Value"), 0x6);
        assert_eq!(dispid("Range", "Value2"), 0x56C);
        assert_eq!(dispid("Range", "Formula"), 0x105);
        assert_eq!(dispid("Range", "Formula2"), 0x62C);
        assert_eq!(dispid("Range", "Text"), 0x8A);
        assert_eq!(dispid("Range", "_Default"), 0x0);
        assert_eq!(dispid("Range", "Item"), 0xAA);
        assert_eq!(dispid("_Application", "Workbooks"), 0x23C);
        assert_eq!(dispid("Workbooks", "Open"), 0x783);
        assert_eq!(dispid("_Application", "Calculate"), 0x117);
        assert_eq!(dispid("range", "VALUE"), 0x6);

        let range = interface("Range").unwrap();
        assert_eq!(
            range.iid,
            Guid::parse("00020846-0000-0000-C000-000000000046")
        );
        assert!(!range.dual);
        assert!(interface("_Application").unwrap().dual);
        assert!(range.named("Valu").is_empty());
        assert!(interface("Rnage").is_none());

        let value = range.named("Value");
        assert_eq!(value.len(), 2);
        assert_eq!(value[0].kind, InvokeKind::Get);
        assert_eq!(
            value[0].params,
            [o("RangeValueDataType", Ty::Variant)].as_slice()
        );
        assert_eq!(
            value[1].params,
            [o("RangeValueDataType", Ty::Variant), p("", Ty::Variant)].as_slice()
        );
        assert_eq!(range.member(0x6, InvokeKind::Put).unwrap().name, "Value");
        assert!(range.member(0x6, InvokeKind::Func).is_none());

        let sort = &range.named("Sort")[0];
        assert_eq!(sort.params.len(), 16);
        assert_eq!(
            sort.params[7],
            d("Header", Ty::Enum("XlYesNoGuess"), DefaultValue::I4(2))
        );
        let new_enum = &range.named("_NewEnum")[0];
        assert_eq!((new_enum.dispid, new_enum.ret), (-4, Ty::Unknown));
        assert_eq!(range.named("Cells")[0].ret, Ty::Object("Range"));

        let order = enumeration("XlSortOrder").unwrap();
        assert_eq!(order.constants, [("xlAscending", 1), ("xlDescending", 2)]);
        assert!(enumeration("Constants")
            .unwrap()
            .constants
            .contains(&("xlAll", -4104)));
    }
}
