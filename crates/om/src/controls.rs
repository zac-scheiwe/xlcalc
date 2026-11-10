use std::collections::HashMap;

use xlcalc::{CellAddr, Command, SheetId, Value, Workbook, XlError};
use xlcalc_variant::{change_type, VarType, Variant};
use xlcalc_xlsx::{Control, ControlKind};

use crate::addr::parse_range_text;
use crate::app::{Application, BookId, Object};
use crate::conv::Args;
use crate::error::OmError;
use crate::typelib::{InvokeKind, Member};

pub(crate) type BookControls = HashMap<SheetId, SheetControls>;

#[derive(Default)]
pub(crate) struct SheetControls {
    pub list: Vec<Ctl>,

    pub counter: u32,
    next_uid: u32,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Seen {
    Blank,
    Number(u64),
    Bool(bool),
    Text(String),
    Error(String),
}

impl Seen {
    fn of(v: &Value) -> Seen {
        match v {
            Value::Blank => Seen::Blank,
            Value::Number(n) => Seen::Number(n.to_bits()),
            Value::Bool(b) => Seen::Bool(*b),
            Value::Error(e) => Seen::Error(format!("{e:?}")),
            other => Seen::Text(format!("{other:?}")),
        }
    }
}

pub(crate) struct Ctl {
    pub uid: u32,
    pub c: Control,

    pub seen: Option<Seen>,
    pub visible: bool,

    pub multi: i32,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Coll {
    Shapes,
    Kind(ControlKind),
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct CtlRef {
    pub book: BookId,
    pub sheet: SheetId,
    pub uid: u32,
    pub kind: ControlKind,
    pub shape: bool,
}

impl Coll {
    pub(crate) fn interface_name(self) -> &'static str {
        match self {
            Coll::Shapes => "Shapes",
            Coll::Kind(k) => match k {
                ControlKind::Button => "Buttons",
                ControlKind::CheckBox => "CheckBoxes",
                ControlKind::Drop => "DropDowns",
                ControlKind::List => "ListBoxes",
                ControlKind::Spin => "Spinners",
                ControlKind::Scroll => "ScrollBars",
                ControlKind::Radio => "OptionButtons",
                ControlKind::GBox => "GroupBoxes",
                ControlKind::Label => "Labels",
            },
        }
    }
}

fn class_name(k: ControlKind) -> &'static str {
    match k {
        ControlKind::Button => "Button",
        ControlKind::CheckBox => "CheckBox",
        ControlKind::Drop => "DropDown",
        ControlKind::List => "ListBox",
        ControlKind::Spin => "Spinner",
        ControlKind::Scroll => "ScrollBar",
        ControlKind::Radio => "OptionButton",
        ControlKind::GBox => "GroupBox",
        ControlKind::Label => "Label",
    }
}

fn base_name(k: ControlKind) -> &'static str {
    match k {
        ControlKind::Button => "Button",
        ControlKind::CheckBox => "Check Box",
        ControlKind::Drop => "Drop Down",
        ControlKind::List => "List Box",
        ControlKind::Spin => "Spinner",
        ControlKind::Scroll => "Scroll Bar",
        ControlKind::Radio => "Option Button",
        ControlKind::GBox => "Group Box",
        ControlKind::Label => "Label",
    }
}

impl CtlRef {
    pub(crate) fn interface_name(&self) -> &'static str {
        if self.shape {
            "Shape"
        } else {
            class_name(self.kind)
        }
    }
}

fn form_control_type(k: ControlKind) -> i32 {
    match k {
        ControlKind::Button => 0,
        ControlKind::CheckBox => 1,
        ControlKind::Drop => 2,
        ControlKind::GBox => 4,
        ControlKind::Label => 5,
        ControlKind::List => 6,
        ControlKind::Radio => 7,
        ControlKind::Scroll => 8,
        ControlKind::Spin => 9,
    }
}

fn kind_of_form_control(t: i32) -> Option<ControlKind> {
    Some(match t {
        0 => ControlKind::Button,
        1 => ControlKind::CheckBox,
        2 => ControlKind::Drop,
        4 => ControlKind::GBox,
        5 => ControlKind::Label,
        6 => ControlKind::List,
        7 => ControlKind::Radio,
        8 => ControlKind::Scroll,
        9 => ControlKind::Spin,
        _ => return None,
    })
}

fn kind_of_collection(name: &str) -> Option<ControlKind> {
    Some(match name {
        "Buttons" => ControlKind::Button,
        "CheckBoxes" => ControlKind::CheckBox,
        "DropDowns" => ControlKind::Drop,
        "ListBoxes" => ControlKind::List,
        "Spinners" => ControlKind::Spin,
        "ScrollBars" => ControlKind::Scroll,
        "OptionButtons" => ControlKind::Radio,
        "GroupBoxes" => ControlKind::GBox,
        "Labels" => ControlKind::Label,
        _ => return None,
    })
}

pub(crate) fn is_collection_member(name: &str) -> bool {
    name == "Shapes" || kind_of_collection(name).is_some()
}

fn snap(v: f64) -> f64 {
    (v * 2.0).round() / 2.0
}

fn normalise_link(wb: &Workbook, sheet: SheetId, text: &str) -> Option<String> {
    let t = text.trim();
    let (sid, _) = parse_range_text(wb, sheet, t)?;
    match t.rfind('!') {
        Some(i) if sid != sheet => {
            let name = wb.sheet(sid)?.name.replace('\'', "''");
            Some(format!("'{name}'!{}", t[i + 1..].to_uppercase()))
        }
        Some(_) => Some(t.to_string()),
        None => Some(t.to_uppercase()),
    }
}

fn link_addr(wb: &Workbook, sheet: SheetId, text: &str) -> Option<CellAddr> {
    if text.is_empty() {
        return None;
    }
    let (sid, r) = parse_range_text(wb, sheet, text)?;
    Some(CellAddr::new(sid, r.r0, r.c0))
}

fn list_count(wb: &Workbook, sheet: SheetId, text: &str) -> i32 {
    match parse_range_text(wb, sheet, text) {
        Some((_, r)) if !text.is_empty() => {
            ((r.r1 - r.r0 + 1) as i64 * (r.c1 - r.c0 + 1) as i64).min(i32::MAX as i64) as i32
        }
        _ => 0,
    }
}

fn clamp_int(v: f64, lo: i32, hi: i32) -> i32 {
    let n = if v.is_nan() { 0.0 } else { v.trunc() };
    let n = n.clamp(-2e9, 2e9) as i32;
    n.max(lo).min(hi)
}

fn group_of(list: &[Ctl], idx: usize) -> Vec<usize> {
    let container = |i: usize| {
        let c = &list[i].c;
        let (x, y) = (c.left + c.width / 2.0, c.top + c.height / 2.0);
        list.iter().position(|g| {
            g.c.kind == ControlKind::GBox
                && x >= g.c.left
                && x <= g.c.left + g.c.width
                && y >= g.c.top
                && y <= g.c.top + g.c.height
        })
    };
    let mine = container(idx);
    (0..list.len())
        .filter(|&i| list[i].c.kind == ControlKind::Radio && container(i) == mine)
        .collect()
}

fn linked_group(list: &[Ctl], idx: usize) -> Vec<usize> {
    if list[idx].c.kind == ControlKind::Radio {
        group_of(list, idx)
    } else {
        vec![idx]
    }
}

fn effective_link(list: &[Ctl], idx: usize) -> String {
    if list[idx].c.kind == ControlKind::Radio {
        if let Some(l) = group_of(list, idx)
            .into_iter()
            .map(|i| list[i].c.fmla_link.clone())
            .find(|l| !l.is_empty())
        {
            return l;
        }
    }
    list[idx].c.fmla_link.clone()
}

fn cell_value(list: &[Ctl], idx: usize) -> Value {
    let c = &list[idx].c;
    match c.kind {
        ControlKind::CheckBox => match c.checked {
            0 => Value::Bool(false),
            1 => Value::Bool(true),
            _ => Value::Error(XlError::NA),
        },
        ControlKind::Radio => Value::Number(group_value(list, idx) as f64),
        ControlKind::Spin | ControlKind::Scroll => Value::Number(c.val as f64),
        _ => Value::Number(c.sel as f64),
    }
}

fn group_value(list: &[Ctl], idx: usize) -> usize {
    group_of(list, idx)
        .iter()
        .position(|&i| list[i].c.checked == 1)
        .map_or(0, |p| p + 1)
}

fn write_link(wb: &mut Workbook, sheet: SheetId, list: &mut [Ctl], idx: usize, v: Value) {
    let Some(addr) = link_addr(wb, sheet, &effective_link(list, idx)) else {
        return;
    };
    wb.command(Command::Control, |wb| wb.set_value(addr, v));
    let seen = Seen::of(&wb.value(addr));
    for i in linked_group(list, idx) {
        list[i].seen = Some(seen.clone());
    }
}

fn resync_other_groups(wb: &mut Workbook, sheet: SheetId, list: &mut [Ctl], idx: usize) {
    let mine = group_of(list, idx);
    let mut done = mine.clone();
    for k in 0..list.len() {
        if list[k].c.kind != ControlKind::Radio || done.contains(&k) {
            continue;
        }
        let group = group_of(list, k);
        done.extend(&group);
        let Some(addr) = link_addr(wb, sheet, &effective_link(list, k)) else {
            continue;
        };
        let want = group_value(list, k) as f64;
        match wb.value(addr) {
            Value::Blank => {}
            Value::Number(x) if x == want => {}
            _ => write_link(wb, sheet, list, k, Value::Number(want)),
        }
    }
}

fn sync(wb: &Workbook, sheet: SheetId, list: &mut [Ctl]) {
    for idx in 0..list.len() {
        let kind = list[idx].c.kind;
        if matches!(
            kind,
            ControlKind::Button | ControlKind::GBox | ControlKind::Label
        ) {
            continue;
        }
        let link = effective_link(list, idx);
        let Some(addr) = link_addr(wb, sheet, &link) else {
            continue;
        };
        let v = wb.value(addr);
        let now = Seen::of(&v);
        match &list[idx].seen {
            None => {
                list[idx].seen = Some(now);
                continue;
            }
            Some(s) if *s == now => continue,
            Some(_) => {}
        }
        apply_cell(wb, sheet, list, idx, &v);
        for i in linked_group(list, idx) {
            list[i].seen = Some(now.clone());
        }
    }
}

fn apply_cell(wb: &Workbook, sheet: SheetId, list: &mut [Ctl], idx: usize, v: &Value) {
    let kind = list[idx].c.kind;
    match kind {
        ControlKind::CheckBox => {
            let state = match v {
                Value::Blank | Value::Bool(false) => 0,
                Value::Bool(true) => 1,
                Value::Number(n) => i32::from(*n != 0.0),
                Value::Error(XlError::NA) => 2,
                _ => return,
            };
            list[idx].c.checked = state;
        }
        ControlKind::Radio => {
            let n = match v {
                Value::Number(n) => n.trunc() as i64,
                Value::Bool(true) => 1,
                Value::Blank | Value::Bool(false) => 0,
                _ => return,
            };
            for (k, i) in group_of(list, idx).into_iter().enumerate() {
                list[i].c.checked = i32::from(n == k as i64 + 1);
            }
        }
        ControlKind::Spin | ControlKind::Scroll => {
            let n = match v {
                Value::Number(n) => *n,
                Value::Blank => 0.0,
                _ => return,
            };
            let c = &mut list[idx].c;
            c.val = clamp_int(n, c.min, c.max);
        }
        _ => {
            let n = match v {
                Value::Number(n) => *n,
                Value::Blank => 0.0,
                _ => return,
            };
            let count = list_count(wb, sheet, &list[idx].c.fmla_range);
            list[idx].c.sel = clamp_int(n, 0, count);
        }
    }
}

pub(crate) fn num_arg(v: &Variant, lcid: u32) -> Result<f64, OmError> {
    if let Variant::Bool(b) = v {
        return Ok(if *b { 1.0 } else { 0.0 });
    }
    match change_type(v, VarType::R8, crate::conv::variant_locale(lcid), 0) {
        Ok(Variant::R8(x)) => Ok(x),
        _ => Err(OmError::type_mismatch()),
    }
}

pub(crate) fn put_num(a: &Args) -> Result<f64, OmError> {
    num_arg(a.put.as_ref().unwrap_or(&Variant::Empty), a.lcid)
}

pub(crate) fn arg_num(a: &Args, i: usize) -> Result<f64, OmError> {
    match a.get(i) {
        Some(v) => num_arg(v, a.lcid),
        None => Err(OmError::app_defined()),
    }
}

pub(crate) fn put_text(a: &Args) -> Result<String, OmError> {
    Ok(match a.put.as_ref() {
        Some(Variant::Bstr(s)) => s.clone(),
        Some(v) => match change_type(v, VarType::Bstr, crate::conv::variant_locale(a.lcid), 0) {
            Ok(Variant::Bstr(s)) => s,
            _ => return Err(OmError::type_mismatch()),
        },
        None => String::new(),
    })
}

fn r8(x: f64) -> Variant {
    Variant::R8(x)
}

impl Application<'_> {
    fn with_control<R>(
        &mut self,
        r: &CtlRef,
        f: impl FnOnce(&mut Workbook, &mut Vec<Ctl>, usize) -> Result<R, OmError>,
    ) -> Result<R, OmError> {
        let bk = self.book_mut(r.book)?;
        let sc = bk.controls.entry(r.sheet).or_default();
        let wb: &mut Workbook = &mut bk.wb;
        if wb.sheet(r.sheet).is_none() {
            return Err(OmError::object_required());
        }
        sync(wb, r.sheet, &mut sc.list);
        let idx = sc
            .list
            .iter()
            .position(|c| c.uid == r.uid)
            .ok_or_else(OmError::object_required)?;
        f(wb, &mut sc.list, idx)
    }

    fn ctl_ref(&self, b: BookId, s: SheetId, uid: u32, kind: ControlKind, shape: bool) -> Object {
        Object::Control(CtlRef {
            book: b,
            sheet: s,
            uid,
            kind,
            shape,
        })
    }

    fn add_control(
        &mut self,
        b: BookId,
        s: SheetId,
        kind: ControlKind,
        rect: [f64; 4],
        shape: bool,
    ) -> Result<Variant, OmError> {
        let bk = self.book_mut(b)?;
        if bk.wb.sheet(s).is_none() {
            return Err(OmError::object_required());
        }
        let sc = bk.controls.entry(s).or_default();
        sc.counter += 1;
        sc.next_uid += 1;
        let uid = sc.next_uid;
        let name = format!("{} {}", base_name(kind), sc.counter);
        let rect = rect.map(snap);
        let mut c = Control::new(kind, &name, rect);

        sc.list.push(Ctl {
            uid,
            c: c.clone(),
            seen: None,
            visible: true,
            multi: -4142,
        });
        if kind == ControlKind::Radio {
            let idx = sc.list.len() - 1;
            c.fmla_link = effective_link(&sc.list, idx);
            sc.list[idx].c.fmla_link = c.fmla_link;
        }
        Ok(self.alloc(self.ctl_ref(b, s, uid, kind, shape)))
    }

    pub(crate) fn sheet_controls_member(
        &mut self,
        b: BookId,
        s: SheetId,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let coll = match kind_of_collection(m.name) {
            Some(k) => Coll::Kind(k),
            None => Coll::Shapes,
        };
        match a.get(0) {
            Some(i) if m.kind == InvokeKind::Func => {
                let msg = format!(
                    "Unable to get the {} property of the Worksheet class",
                    m.name
                );
                self.control_item(b, s, coll, i, a.lcid)
                    .map_err(|_| OmError::app_defined_with(&msg))
            }
            _ => Ok(self.alloc(Object::Controls(b, s, coll))),
        }
    }

    pub(crate) fn collection_items(
        &mut self,
        b: BookId,
        s: SheetId,
        coll: Coll,
    ) -> Result<Vec<(u32, ControlKind, String)>, OmError> {
        let bk = self.book_mut(b)?;
        if bk.wb.sheet(s).is_none() {
            return Err(OmError::object_required());
        }
        let sc = bk.controls.entry(s).or_default();
        Ok(sc
            .list
            .iter()
            .filter(|c| match coll {
                Coll::Shapes => true,
                Coll::Kind(k) => c.c.kind == k,
            })
            .map(|c| (c.uid, c.c.kind, c.c.name.clone()))
            .collect())
    }

    fn control_item(
        &mut self,
        b: BookId,
        s: SheetId,
        coll: Coll,
        index: &Variant,
        lcid: u32,
    ) -> Result<Variant, OmError> {
        let items = self.collection_items(b, s, coll)?;
        let found = match index {
            Variant::Bstr(name) => items.iter().find(|i| i.2.eq_ignore_ascii_case(name)),
            v => {
                let n = num_arg(v, lcid)?.trunc();
                if n >= 1.0 {
                    items.get(n as usize - 1)
                } else {
                    None
                }
            }
        };
        let (uid, kind, _) = found.ok_or_else(OmError::app_defined)?.clone();
        let shape = coll == Coll::Shapes;
        Ok(self.alloc(self.ctl_ref(b, s, uid, kind, shape)))
    }

    pub(crate) fn controls_member(
        &mut self,
        b: BookId,
        s: SheetId,
        coll: Coll,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match (m.name, m.kind) {
            ("Count", _) => Ok(Variant::I4(self.collection_items(b, s, coll)?.len() as i32)),
            ("Item" | "_Default", _) => match a.get(0) {
                Some(i) => self.control_item(b, s, coll, i, a.lcid),
                None => Err(OmError::app_defined()),
            },
            ("Add", _) => {
                let Coll::Kind(kind) = coll else {
                    return Err(crate::app::unsupported("Shapes", m));
                };
                let rect = [
                    arg_num(a, 0)?,
                    arg_num(a, 1)?,
                    arg_num(a, 2)?,
                    arg_num(a, 3)?,
                ];
                self.add_control(b, s, kind, rect, false)
            }
            ("AddFormControl", _) => {
                let kind = kind_of_form_control(a.i32(0)?.unwrap_or(-1))
                    .ok_or_else(OmError::app_defined)?;
                let rect = [
                    arg_num(a, 1)?,
                    arg_num(a, 2)?,
                    arg_num(a, 3)?,
                    arg_num(a, 4)?,
                ];
                self.add_control(b, s, kind, rect, true)
            }
            ("_NewEnum", _) => {
                let shape = coll == Coll::Shapes;
                let items = self.collection_items(b, s, coll)?;
                let handles = items
                    .into_iter()
                    .map(|(uid, kind, _)| self.alloc(self.ctl_ref(b, s, uid, kind, shape)))
                    .collect();
                Ok(self.alloc(Object::Enumerator(handles, 0)))
            }
            _ => Err(crate::app::unsupported(coll.interface_name(), m)),
        }
    }

    pub(crate) fn control_member(
        &mut self,
        r: &CtlRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        if r.shape {
            match m.name {
                "Type" => return self.with_control(r, |_, _, _| Ok(Variant::I4(8))),
                "FormControlType" => {
                    return self
                        .with_control(r, |_, _, _| Ok(Variant::I4(form_control_type(r.kind))))
                }
                "ControlFormat" => {
                    self.with_control(r, |_, _, _| Ok(()))?;
                    return Ok(self.alloc(Object::ControlFormat(*r)));
                }
                _ => {}
            }
        }
        self.control_prop(r, m, a)
    }

    pub(crate) fn control_prop(
        &mut self,
        r: &CtlRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        let kind = r.kind;
        let class = class_name(kind);
        let put = m.kind == InvokeKind::Put || m.kind == InvokeKind::PutRef;
        let unsup = || crate::app::unsupported(if r.shape { "Shape" } else { class }, m);

        let pos = |x: f64| {
            if r.shape {
                Variant::R4(x as f32)
            } else {
                r8(x)
            }
        };
        let bar = matches!(kind, ControlKind::Spin | ControlKind::Scroll);
        let list_kind = matches!(kind, ControlKind::Drop | ControlKind::List);
        let sheet = r.sheet;
        match (m.name, put) {
            ("Name", false) => {
                self.with_control(r, |_, l, i| Ok(Variant::Bstr(l[i].c.name.clone())))
            }
            ("Name", true) => {
                let v = put_text(a)?;
                self.with_control(r, |_, l, i| {
                    l[i].c.name = v;
                    Ok(Variant::Empty)
                })
            }
            ("Caption" | "Text", false) => {
                self.with_control(r, |_, l, i| Ok(Variant::Bstr(l[i].c.caption.clone())))
            }
            ("Caption" | "Text", true) => {
                let v = put_text(a)?;
                self.with_control(r, |_, l, i| {
                    l[i].c.caption = v;
                    Ok(Variant::Empty)
                })
            }
            ("OnAction", false) => {
                self.with_control(r, |_, l, i| Ok(Variant::Bstr(l[i].c.on_action.clone())))
            }
            ("OnAction", true) => {
                let v = put_text(a)?;
                self.with_control(r, |_, l, i| {
                    l[i].c.on_action = v;
                    Ok(Variant::Empty)
                })
            }
            ("Left" | "Top" | "Width" | "Height", false) => {
                let name = m.name;
                self.with_control(r, |_, l, i| {
                    let c = &l[i].c;
                    Ok(pos(match name {
                        "Left" => c.left,
                        "Top" => c.top,
                        "Width" => c.width,
                        _ => c.height,
                    }))
                })
            }
            ("Left" | "Top" | "Width" | "Height", true) => {
                let v = snap(put_num(a)?);
                let name = m.name;
                self.with_control(r, |_, l, i| {
                    let c = &mut l[i].c;
                    match name {
                        "Left" => c.left = v,
                        "Top" => c.top = v,
                        "Width" => c.width = v,
                        _ => c.height = v,
                    }
                    Ok(Variant::Empty)
                })
            }
            ("Placement", false) => {
                self.with_control(r, |_, l, i| Ok(Variant::I4(l[i].c.placement)))
            }
            ("Placement", true) => {
                let v = put_num(a)? as i32;
                self.with_control(r, |_, l, i| {
                    if !(1..=3).contains(&v) {
                        return Err(OmError::unable_to_set("Placement", class));
                    }
                    l[i].c.placement = v;
                    Ok(Variant::Empty)
                })
            }
            ("Locked", false) => self.with_control(r, |_, l, i| Ok(Variant::Bool(l[i].c.locked))),
            ("Locked", true) => {
                let v = put_num(a)? != 0.0;
                self.with_control(r, |_, l, i| {
                    l[i].c.locked = v;
                    Ok(Variant::Empty)
                })
            }
            ("Enabled", false) => self.with_control(r, |_, l, i| Ok(Variant::Bool(l[i].c.enabled))),
            ("Enabled", true) => {
                let v = put_num(a)? != 0.0;
                self.with_control(r, |_, l, i| {
                    l[i].c.enabled = v;
                    Ok(Variant::Empty)
                })
            }
            ("PrintObject", false) => {
                self.with_control(r, |_, l, i| Ok(Variant::Bool(l[i].c.print_object)))
            }
            ("PrintObject", true) => {
                let v = put_num(a)? != 0.0;
                self.with_control(r, |_, l, i| {
                    l[i].c.print_object = v;
                    Ok(Variant::Empty)
                })
            }
            ("Visible", false) => {
                let shape = r.shape;
                self.with_control(r, |_, l, i| {
                    Ok(match (shape, l[i].visible) {
                        (true, v) => Variant::I4(if v { -1 } else { 0 }),
                        (false, v) => Variant::Bool(v),
                    })
                })
            }
            ("Visible", true) => {
                let v = put_num(a)? != 0.0;
                self.with_control(r, |_, l, i| {
                    l[i].visible = v;
                    Ok(Variant::Empty)
                })
            }
            ("LinkedCell", false) => {
                self.with_control(r, |_, l, i| Ok(Variant::Bstr(effective_link(l, i))))
            }
            ("LinkedCell", true) => {
                let text = put_text(a)?;
                self.with_control(r, |wb, l, i| {
                    let norm = normalise_link(wb, sheet, &text);
                    for k in linked_group(l, i) {
                        l[k].c.fmla_link = norm.clone().unwrap_or_default();
                        l[k].seen = None;
                    }
                    if let Some(addr) = norm.and_then(|n| link_addr(wb, sheet, &n)) {
                        let blank = matches!(wb.value(addr), Value::Blank);
                        if blank {
                            let seen = Seen::of(&Value::Blank);
                            for k in linked_group(l, i) {
                                l[k].seen = Some(seen.clone());
                            }
                        } else {
                            let v = cell_value(l, i);
                            write_link(wb, sheet, l, i, v);
                        }
                    }
                    if l[i].c.kind == ControlKind::Radio {
                        resync_other_groups(wb, sheet, l, i);
                    }
                    Ok(Variant::Empty)
                })
            }
            ("Value" | "_Default", false) => self.with_control(r, |_, l, i| {
                let c = &l[i].c;
                Ok(match kind {
                    ControlKind::CheckBox | ControlKind::Radio => match c.checked {
                        0 => Variant::I4(-4146),
                        1 => r8(1.0),
                        _ => r8(2.0),
                    },
                    ControlKind::Spin | ControlKind::Scroll => r8(c.val as f64),
                    ControlKind::Drop | ControlKind::List => r8(c.sel as f64),
                    _ => return Err(OmError::app_defined()),
                })
            }),
            ("Value" | "_Default", true) => {
                let n = put_num(a)?;
                self.with_control(r, |wb, l, i| {
                    let bad = || OmError::unable_to_set("Value", class);
                    match kind {
                        ControlKind::CheckBox | ControlKind::Radio => {
                            let new = if n == 1.0 {
                                1
                            } else if n == -4146.0 || n == 0.0 {
                                0
                            } else if n == 2.0 && kind == ControlKind::CheckBox {
                                2
                            } else {
                                return Err(bad());
                            };
                            if kind == ControlKind::Radio {
                                let before = group_value(l, i);
                                if new == 1 {
                                    for k in group_of(l, i) {
                                        l[k].c.checked = 0;
                                    }
                                }
                                l[i].c.checked = new;
                                let after = group_value(l, i);
                                if before != after {
                                    write_link(wb, sheet, l, i, Value::Number(after as f64));
                                }
                            } else if l[i].c.checked != new {
                                l[i].c.checked = new;
                                let v = cell_value(l, i);
                                write_link(wb, sheet, l, i, v);
                            }
                        }
                        ControlKind::Spin | ControlKind::Scroll => {
                            let n = n.trunc();
                            if !(0.0..=30000.0).contains(&n) {
                                return Err(OmError {
                                    error_info: true,
                                    info_description: Some(
                                        "Scroll value must be between 0 and 30000.".to_string(),
                                    ),
                                    ..bad()
                                });
                            }
                            let c = &mut l[i].c;
                            c.val = clamp_int(n, c.min, c.max);
                            let v = cell_value(l, i);
                            write_link(wb, sheet, l, i, v);
                        }
                        ControlKind::Drop | ControlKind::List => {
                            let n = n.trunc();
                            let count = list_count(wb, sheet, &l[i].c.fmla_range);
                            if n < 0.0 || n > count as f64 {
                                return Err(bad());
                            }
                            if l[i].c.sel != n as i32 {
                                l[i].c.sel = n as i32;
                                let v = cell_value(l, i);
                                write_link(wb, sheet, l, i, v);
                            }
                        }
                        _ => return Err(OmError::app_defined()),
                    }
                    Ok(Variant::Empty)
                })
            }
            ("Min" | "Max" | "SmallChange" | "LargeChange", false) if bar => {
                let name = m.name;
                self.with_control(r, |_, l, i| {
                    let c = &l[i].c;
                    Ok(r8(match name {
                        "Min" => c.min,
                        "Max" => c.max,
                        "SmallChange" => c.inc,
                        _ => c.page,
                    } as f64))
                })
            }
            ("Min" | "Max" | "SmallChange" | "LargeChange", true) if bar => {
                let n = put_num(a)?.trunc();
                let name = m.name;
                self.with_control(r, |wb, l, i| {
                    if !(0.0..=30000.0).contains(&n) {
                        return Err(OmError::unable_to_set(name, class));
                    }
                    let n = n as i32;
                    let c = &mut l[i].c;
                    match name {
                        "Min" => c.min = n,
                        "Max" => c.max = n,
                        "SmallChange" => c.inc = n,
                        _ => c.page = n,
                    }
                    if matches!(name, "Min" | "Max") {
                        c.val = clamp_int(c.val as f64, c.min, c.max);
                    }

                    let v = cell_value(l, i);
                    write_link(wb, sheet, l, i, v);
                    Ok(Variant::Empty)
                })
            }
            ("ListFillRange", false) if list_kind => {
                self.with_control(r, |_, l, i| Ok(Variant::Bstr(l[i].c.fmla_range.clone())))
            }
            ("ListFillRange", true) if list_kind => {
                let text = put_text(a)?;
                self.with_control(r, |wb, l, i| {
                    l[i].c.fmla_range = text;

                    let count = list_count(wb, sheet, &l[i].c.fmla_range);
                    let from_cell = link_addr(wb, sheet, &effective_link(l, i))
                        .map(|addr| wb.value(addr))
                        .and_then(|v| match v {
                            Value::Number(n) => Some(n),
                            Value::Blank => Some(0.0),
                            _ => None,
                        });
                    let cur = from_cell.unwrap_or(l[i].c.sel as f64);
                    l[i].c.sel = clamp_int(cur, 0, count);
                    Ok(Variant::Empty)
                })
            }
            ("ListIndex", false) if list_kind => {
                self.with_control(r, |_, l, i| Ok(Variant::I4(l[i].c.sel)))
            }
            ("ListCount", false) if list_kind => self.with_control(r, |wb, l, i| {
                Ok(Variant::I4(list_count(wb, sheet, &l[i].c.fmla_range)))
            }),
            ("DropDownLines", false) if kind == ControlKind::Drop => {
                self.with_control(r, |_, l, i| Ok(r8(l[i].c.drop_lines as f64)))
            }
            ("DropDownLines", true) if kind == ControlKind::Drop => {
                let n = put_num(a)?.trunc().clamp(-2e9, 2e9) as i32;
                self.with_control(r, |_, l, i| {
                    l[i].c.drop_lines = n;
                    Ok(Variant::Empty)
                })
            }
            ("MultiSelect", false) if kind == ControlKind::List => {
                self.with_control(r, |_, l, i| Ok(Variant::I4(l[i].multi)))
            }
            ("MultiSelect", true) if kind == ControlKind::List => {
                let n = put_num(a)?.trunc().clamp(-2e9, 2e9) as i32;
                self.with_control(r, |_, l, i| {
                    if n == 0 {
                        return Err(OmError::unable_to_set("MultiSelect", class));
                    }
                    l[i].multi = n;
                    Ok(Variant::Empty)
                })
            }
            _ => Err(unsup()),
        }
    }

    pub(crate) fn control_format_member(
        &mut self,
        r: &CtlRef,
        m: &'static Member,
        a: &Args,
    ) -> Result<Variant, OmError> {
        self.control_prop(r, m, a)
    }

    pub fn click_control(
        &mut self,
        book: BookId,
        sheet: SheetId,
        name: &str,
    ) -> Result<Option<Variant>, OmError> {
        let Some((action, caller)) = self.click(book, sheet, name)? else {
            return Ok(None);
        };
        self.flush_events();
        let prev = self.action_caller.replace(caller);
        let r = self.run(&action, &[]);
        self.action_caller = prev;
        r.map(Some)
    }

    fn click(
        &mut self,
        book: BookId,
        sheet: SheetId,
        name: &str,
    ) -> Result<Option<(String, String)>, OmError> {
        let bk = self.book_mut(book)?;
        let sc = bk.controls.entry(sheet).or_default();
        let wb: &mut Workbook = &mut bk.wb;
        sync(wb, sheet, &mut sc.list);
        let l = &mut sc.list;
        let i = l
            .iter()
            .position(|c| c.c.name.eq_ignore_ascii_case(name))
            .ok_or_else(OmError::app_defined)?;
        if !l[i].c.enabled {
            return Ok(None);
        }
        match l[i].c.kind {
            ControlKind::CheckBox => {
                l[i].c.checked = i32::from(l[i].c.checked != 1);
                let v = cell_value(l, i);
                write_link(wb, sheet, l, i, v);
            }
            ControlKind::Radio => {
                for k in group_of(l, i) {
                    l[k].c.checked = 0;
                }
                l[i].c.checked = 1;
                let v = cell_value(l, i);
                write_link(wb, sheet, l, i, v);
            }
            _ => {}
        }
        let action = l[i].c.on_action.clone();
        Ok((!action.is_empty()).then(|| (action, l[i].c.name.clone())))
    }

    pub(crate) fn controls_for_save(&mut self, b: BookId) -> Vec<(SheetId, Vec<Control>)> {
        let Ok(bk) = self.book_mut(b) else {
            return Vec::new();
        };
        let wb: &Workbook = &bk.wb;
        let mut out = Vec::new();
        for (sheet, sc) in bk.controls.iter_mut() {
            if wb.sheet(*sheet).is_none() {
                continue;
            }
            sync(wb, *sheet, &mut sc.list);
            out.push((*sheet, sc.list.iter().map(|c| c.c.clone()).collect()));
        }
        out
    }
}

pub(crate) fn from_file(list: Vec<Control>) -> SheetControls {
    let mut sc = SheetControls::default();
    for c in list {
        sc.next_uid += 1;
        let tail = c
            .name
            .rsplit(' ')
            .next()
            .and_then(|n| n.parse::<u32>().ok())
            .unwrap_or(0);
        sc.counter = sc.counter.max(tail);
        sc.list.push(Ctl {
            uid: sc.next_uid,
            c,
            seen: None,
            visible: true,
            multi: -4142,
        });
    }
    sc
}
