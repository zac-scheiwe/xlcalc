use crate::legacy_ftab::Ftab;
use crate::legacy_single::ftab;
use std::sync::OnceLock;

pub(crate) const MASK_OPERATOR: u32 = 7;

pub(crate) const MASK_RANGE: u32 = 8;

pub(crate) const MASK_REF_ARRAY: u32 = 0x48;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Class {
    Ref,

    Val,

    Arr,
}

#[derive(Clone, Copy)]
pub(crate) enum Kind {
    Call(Option<&'static Ftab>),

    RefToken { array_const: bool },

    Other,
}

pub(crate) fn ftab_of(id: crate::functions::FuncId) -> Option<&'static Ftab> {
    static BY_ID: OnceLock<Vec<Option<&'static Ftab>>> = OnceLock::new();
    BY_ID.get_or_init(|| {
        (0..crate::functions::count())
            .map(|i| ftab(crate::functions::def(i as crate::functions::FuncId).name))
            .collect()
    })[id as usize]
}

pub(crate) fn arg_mask(f: Option<&Ftab>, i: usize) -> u32 {
    match f {
        Some(f) if !f.masks.is_empty() => {
            let i = if f.ifunc == 255 { i + 1 } else { i };
            f.masks[i.min(f.masks.len() - 1)]
        }
        _ => MASK_OPERATOR,
    }
}

fn keeps_ref(kind: Kind, mask: u32) -> bool {
    mask & 8 != 0
        && match kind {
            Kind::Call(f) => f.is_some_and(|f| f.flags & 8 != 0),

            Kind::RefToken { array_const } => !array_const,
            Kind::Other => false,
        }
}

pub(crate) fn walks(kind: Kind, mask: u32) -> bool {
    mask & 0x40 != 0 && !keeps_ref(kind, mask)
}

fn allowed(kind: Kind, mask: u32, array_entry: bool) -> bool {
    array_entry
        || mask & 7 == 0
        || match kind {
            Kind::Call(Some(f)) => {
                if f.ifunc < 0x25b {
                    f.flags == 0x40
                } else {
                    f.flags & 0x40 != 0
                }
            }
            Kind::RefToken { array_const } => array_const,
            _ => false,
        }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Anc {
    pub call: bool,

    pub marked0: bool,

    pub walks: bool,

    pub mask: u32,
}

fn skipped_in_walk(anc: &[Anc], q: usize, j: usize) -> bool {
    anc[q].marked0 || (q + 1..j).any(|k| anc[k].call && marked_before(anc, q, k))
}

fn marked_before(anc: &[Anc], q: usize, k: usize) -> bool {
    anc[k].marked0
        || anc[k].walks
        || (q + 1..k).any(|r| anc[r].walks && !skipped_in_walk(anc, r, k))
}

pub(crate) fn classify(kind: Kind, mask: u32, anc: &[Anc], array_entry: bool) -> Class {
    if keeps_ref(kind, mask) {
        return Class::Ref;
    }
    if mask & 0x40 != 0 && allowed(kind, mask, array_entry) {
        return Class::Arr;
    }
    let reached = (0..anc.len()).rev().any(|q| {
        anc[q].walks
            && allowed(kind, anc[q].mask, array_entry)
            && !skipped_in_walk(anc, q, anc.len())
    });
    if reached {
        Class::Arr
    } else {
        Class::Val
    }
}

pub(crate) fn ftab_of_name(name: &str) -> Option<&'static Ftab> {
    ftab(name)
}

pub(crate) fn ftab_call() -> Option<&'static Ftab> {
    static REC: OnceLock<Option<&'static Ftab>> = OnceLock::new();
    *REC.get_or_init(|| crate::legacy_ftab::FTAB.iter().find(|f| f.ifunc == 255))
}

pub(crate) fn callee_mask() -> u32 {
    ftab_call().map_or(MASK_OPERATOR, |f| f.masks[0])
}

impl Anc {
    pub(crate) fn of(kind: Kind, mask: u32) -> Anc {
        Anc {
            call: matches!(kind, Kind::Call(_)),
            marked0: match kind {
                Kind::Call(Some(f)) => f.flags & 0x40 != 0,
                Kind::Call(None) => true,
                _ => false,
            },
            walks: walks(kind, mask),
            mask,
        }
    }
}
