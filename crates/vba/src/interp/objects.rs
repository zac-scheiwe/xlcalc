use super::{Cell, Exec, Loc};
use crate::host::InvokeKind;
use crate::ir::{NamedArg, ParamInfo, ProcId, COLLECTION_CLASS};
use crate::types::VType;
use crate::VbaError;
use std::collections::HashMap;
use xlcalc_variant::{ObjectId, Variant};

const TAG: u64 = 1 << 63;
const KEY_BITS: u32 = 40;
const KEY_MASK: u64 = (1 << KEY_BITS) - 1;

pub fn is_instance(id: ObjectId) -> bool {
    id.0 & TAG != 0
}

pub(crate) fn instance_of(id: ObjectId) -> Option<(u64, usize)> {
    is_instance(id).then_some((id.0 & KEY_MASK, ((id.0 & !TAG) >> KEY_BITS) as usize))
}

fn make_id(key: u64, iface: usize) -> ObjectId {
    ObjectId(TAG | ((iface as u64) << KEY_BITS) | key)
}

pub(crate) fn identity(v: &Variant) -> Result<Option<u64>, VbaError> {
    match v {
        Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) => {
            Ok(Some(instance_of(*id).map_or(id.0, |(k, _)| TAG | k)))
        }
        Variant::Dispatch(None) | Variant::Unknown(None) => Ok(None),
        _ => Err(VbaError::new(424)),
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Instance {
    pub class: usize,
    pub fields: Vec<Cell>,

    pub seq: Vec<u64>,
    pub terminating: bool,

    pub keys: Vec<Option<String>>,

    pub log: Vec<(bool, usize)>,
}

pub(crate) struct Item {
    pub name: Option<String>,
    pub val: Variant,
    pub loc: Option<Loc>,
}

impl Item {
    pub fn value(val: Variant) -> Item {
        Item {
            name: None,
            val,
            loc: None,
        }
    }
}

fn count(v: &Variant, counts: &mut HashMap<u64, usize>) {
    match v {
        Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) => {
            if let Some((k, _)) = instance_of(*id) {
                *counts.entry(k).or_default() += 1;
            }
        }
        Variant::Array(a) => {
            for x in &a.data {
                count(x, counts);
            }
        }
        _ => {}
    }
}

fn count_cell(c: &Cell, counts: &mut HashMap<u64, usize>) {
    if let Cell::Own(s) = c {
        count(&s.val, counts);
    }
}

pub(crate) fn holds_instance(v: &Variant) -> bool {
    match v {
        Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) => is_instance(*id),
        Variant::Array(a) => a.data.iter().any(holds_instance),
        _ => false,
    }
}

pub(crate) struct Connection {
    pub seq: u64,
    pub handler: ProcId,
    pub me: Option<ObjectId>,
}

impl Exec<'_> {
    pub(super) fn class_of_type(&self, ty: &VType) -> Option<usize> {
        match ty {
            VType::Object(Some(c)) => self
                .code
                .class_by_name
                .get(&c.to_ascii_lowercase())
                .copied(),
            _ => None,
        }
    }

    pub(super) fn early(&self, ty: &VType) -> bool {
        self.class_of_type(ty).is_some()
    }

    pub(super) fn class_of_key(&self, key: u64) -> Result<usize, VbaError> {
        self.st
            .instances
            .get(&key)
            .map(|x| x.class)
            .ok_or_else(|| VbaError::new(91))
    }

    pub(super) fn instance_class_name(&self, key: u64) -> Result<String, VbaError> {
        let c = self.class_of_key(key)?;
        if c == COLLECTION_CLASS {
            return Ok("Collection".to_string());
        }
        Ok(self.code.modules[c].name.clone())
    }

    fn is_a(&self, c: usize, target: usize) -> bool {
        c == target
            || self
                .code
                .class(c)
                .is_some_and(|ci| ci.implements.contains(&target))
    }

    pub(super) fn type_of(&mut self, id: ObjectId, class: &str) -> Result<bool, VbaError> {
        if class.eq_ignore_ascii_case("object") {
            return Ok(true);
        }
        if let Some((key, _)) = instance_of(id) {
            let c = self.class_of_key(key)?;
            return Ok(self
                .code
                .class_by_name
                .get(&class.to_ascii_lowercase())
                .is_some_and(|&t| self.is_a(c, t)));
        }
        let name = self.lend(|h| Ok(h.type_name(id)))?;
        Ok(name.eq_ignore_ascii_case(class))
    }

    pub(super) fn coerce_obj(&self, ty: &VType, v: Variant) -> Result<Variant, VbaError> {
        let Some(target) = self.class_of_type(ty) else {
            return Ok(v);
        };
        match v {
            Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) => {
                let Some((key, _)) = instance_of(id) else {
                    return Err(VbaError::new(13));
                };
                let c = self.class_of_key(key)?;
                if c == target {
                    Ok(Variant::Dispatch(Some(make_id(key, 0))))
                } else if self.is_a(c, target) {
                    Ok(Variant::Dispatch(Some(make_id(key, target + 1))))
                } else {
                    Err(VbaError::new(13))
                }
            }
            v => Ok(v),
        }
    }

    pub(super) fn pin_all(&mut self, depth: usize, v: &Variant) {
        if let Variant::Dispatch(Some(id)) | Variant::Unknown(Some(id)) = v {
            if is_instance(*id) {
                self.st.pins.push((depth, *id));
            }
        }
    }

    pub(super) fn boundary(&mut self) {
        let depth = self.st.frames.len();
        let before = self.st.pins.len();
        self.st.pins.retain(|&(d, _)| d < depth);
        if self.st.pins.len() != before {
            self.st.dirty = true;
        }
        if self.st.dirty {
            self.collect();
        }
    }

    pub(super) fn release_frame(&mut self, base: usize, depth: usize) {
        let before = self.st.pins.len();
        self.st.pins.retain(|&(d, _)| d < depth);
        if self.st.pins.len() != before {
            self.st.dirty = true;
            self.collect();
        }
        for i in base..self.st.stack.len() {
            let held = matches!(&self.st.stack[i], Cell::Own(s) if holds_instance(&s.val));
            self.st.stack[i] = Cell::Own(super::Slot::value(VType::Variant, Variant::Empty));
            if held {
                self.st.dirty = true;
                self.collect();
            }
        }
        self.st.stack.truncate(base);
    }

    fn counts(&self) -> HashMap<u64, usize> {
        let mut counts = HashMap::new();
        let st = &self.st;
        for c in st.globals.iter().chain(&st.stack) {
            count_cell(c, &mut counts);
        }
        for x in st.instances.values() {
            for c in &x.fields {
                count_cell(c, &mut counts);
            }
        }
        for id in st
            .frames
            .iter()
            .flatten()
            .chain(st.pins.iter().map(|(_, id)| id))
        {
            count(&Variant::Dispatch(Some(*id)), &mut counts);
        }
        counts
    }

    pub(super) fn collect(&mut self) {
        if self.st.collecting || self.st.ending {
            return;
        }
        self.st.collecting = true;
        loop {
            self.st.dirty = false;
            let counts = self.counts();
            let dead = self
                .st
                .instances
                .iter()
                .find(|(k, _)| !counts.contains_key(k))
                .map(|(k, x)| (*k, x.class, x.terminating));
            let Some((key, class, terminating)) = dead else {
                break;
            };
            let term = self.code.class(class).and_then(|c| c.term);
            match term {
                Some(t) if !terminating => {
                    if let Some(x) = self.st.instances.get_mut(&key) {
                        x.terminating = true;
                    }

                    let _ = self.run(t, Vec::new(), Some(make_id(key, 0)), None);
                    if self.st.ending {
                        break;
                    }

                    if self.counts().contains_key(&key) {
                        continue;
                    }
                    self.st.instances.remove(&key);
                }
                _ => {
                    self.st.instances.remove(&key);
                }
            }
        }
        self.st.collecting = false;
    }

    pub(super) fn new_instance(&mut self, class: usize) -> Result<ObjectId, VbaError> {
        let code = self.code;
        if class == COLLECTION_CLASS {
            return Ok(self.new_collection());
        }
        let ci = code.class(class).ok_or_else(|| VbaError::new(429))?;
        let mut fields = Vec::new();
        for g in &ci.fields {
            fields.push(Cell::Own(self.alloc_info(g)?));
        }
        let key = self.st.next_key;
        self.st.next_key += 1;
        let n = fields.len();
        self.st.instances.insert(
            key,
            Instance {
                class,
                fields,
                seq: vec![0; n],
                terminating: false,
                keys: Vec::new(),
                log: Vec::new(),
            },
        );
        let id = make_id(key, 0);
        let depth = self.st.frames.len();
        self.st.pins.push((depth, id));
        if let Some(init) = ci.init {
            self.run(init, Vec::new(), Some(id), None)?;
        }
        Ok(id)
    }

    pub(super) fn connect(&mut self, loc: &Loc, _v: &Variant) {
        match loc {
            Loc::Global(i) if self.code.globals[*i].with_events.is_some() => {
                let s = self.st.next_seq;
                self.st.next_seq += 1;
                self.st.seq.insert(*i, s);
            }
            Loc::Field(k, i) => {
                let Some(class) = self.st.instances.get(k).map(|x| x.class) else {
                    return;
                };
                let we = self
                    .code
                    .class(class)
                    .is_some_and(|c| c.fields[*i].with_events.is_some());
                if we {
                    let s = self.st.next_seq;
                    self.st.next_seq += 1;
                    if let Some(x) = self.st.instances.get_mut(k) {
                        x.seq[*i] = s;
                    }
                }
            }
            _ => {}
        }
    }

    pub(super) fn items(&mut self, args: &[NamedArg]) -> Result<Vec<Item>, VbaError> {
        let mut out = Vec::new();
        for a in args {
            let val = self.eval_spilled(&a.value)?;
            let loc = match &a.place {
                Some(p) => self.place_loc(p)?,
                None => None,
            };
            out.push(Item {
                name: a.name.clone(),
                val,
                loc,
            });
        }
        Ok(out)
    }

    fn bind_items(
        &mut self,
        params: &[ParamInfo],
        items: Vec<Item>,
    ) -> Result<Vec<Cell>, VbaError> {
        let fixed = params.iter().filter(|q| !q.param_array).count();
        let mut slots: Vec<Option<Item>> = (0..fixed).map(|_| None).collect();
        let mut rest = Vec::new();
        let mut k = 0;
        for it in items {
            match &it.name {
                Some(n) => {
                    let i = params[..fixed]
                        .iter()
                        .position(|q| q.name.eq_ignore_ascii_case(n))
                        .ok_or_else(|| VbaError::new(448))?;
                    slots[i] = Some(it);
                }
                None if k < fixed => {
                    slots[k] = Some(it);
                    k += 1;
                }
                None if fixed < params.len() => rest.push(it.val),
                None => return Err(VbaError::new(450)),
            }
        }
        let mut cells = Vec::new();
        for (q, it) in params
            .iter()
            .zip(slots.into_iter().chain(std::iter::repeat_with(|| None)))
        {
            if q.param_array {
                cells.push(Self::rest_array(std::mem::take(&mut rest)));
                continue;
            }
            cells.push(match it {
                Some(Item { val, .. }) if val == super::MISSING && q.optional => {
                    self.missing_param(q)?
                }
                Some(Item { loc: Some(l), .. }) if !q.by_val && !q.array => Cell::Ref(l),
                Some(Item { val, .. }) => {
                    let from = VType::of_value(&val);
                    self.param_value(q, super::Val::V(val), &from)?
                }
                None if q.optional => self.missing_param(q)?,
                None => return Err(VbaError::new(449)),
            });
        }
        Ok(cells)
    }

    pub(super) fn call_member(
        &mut self,
        id: ObjectId,
        name: &str,
        kind: InvokeKind,
        mut items: Vec<Item>,
        early: bool,
    ) -> Result<Variant, VbaError> {
        let code = self.code;
        let (key, iface) = instance_of(id).ok_or_else(|| VbaError::new(51))?;
        let class = self.class_of_key(key)?;
        if class == COLLECTION_CLASS {
            return self.collection_member(key, name, kind, items);
        }
        let shown = if iface == 0 { class } else { iface - 1 };
        let si = code.class(shown).ok_or_else(|| VbaError::new(438))?;
        let mut name = name.to_ascii_lowercase();
        if name.is_empty() {
            name = si.default.clone().ok_or_else(|| VbaError::new(438))?;
        }
        let ci = code.class(class).ok_or_else(|| VbaError::new(438))?;
        let member = if iface == 0 {
            ci.members.get(&name)
        } else {
            if !si.members.contains_key(&name) {
                return Err(VbaError::new(438));
            }
            let iname = format!("{}_{name}", code.modules[shown].name.to_ascii_lowercase());
            ci.members.get(&iname)
        };
        let member = member.ok_or_else(|| VbaError::new(438))?;
        let visible = |p: ProcId| {
            let q = &code.procs[p];
            iface != 0 || if q.friend { early } else { q.public }
        };
        let me = Some(make_id(key, 0));
        if let Some(f) = member.field {
            if iface == 0 && !ci.fields[f].public {
                return Err(VbaError::new(438));
            }
            let loc = Loc::Field(key, f);
            return match kind {
                InvokeKind::Get => {
                    let v = self.load(&loc)?;
                    if items.is_empty() {
                        return Ok(v);
                    }
                    let mut pos = Vec::new();
                    for it in items {
                        pos.push(it.val);
                    }
                    match v {
                        Variant::Array(a) => {
                            let mut ns = Vec::new();
                            for x in &pos {
                                ns.push(self.cv.to_i64(&super::Val::V(x.clone()))?);
                            }
                            let k = super::flat(&a.bounds, &ns)?;
                            Ok(a.data[k].clone())
                        }

                        v if !matches!(v, Variant::Dispatch(_) | Variant::Unknown(_)) => {
                            Err(VbaError::new(451))
                        }
                        v => {
                            let o = Self::object_id(&v)?;
                            self.invoke(o, "", InvokeKind::Get, pos, Vec::new())
                        }
                    }
                }
                InvokeKind::Put | InvokeKind::PutRef => {
                    let v = items.pop().map_or(Variant::Empty, |i| i.val);
                    if !items.is_empty() {
                        return Err(VbaError::new(450));
                    }
                    if kind == InvokeKind::Put {
                        let from = VType::of_value(&v);
                        self.let_loc(&loc, super::Val::V(v), &from)?;
                    } else {
                        self.set_loc(&loc, v)?;
                    }
                    Ok(Variant::Empty)
                }
            };
        }
        let proc = match kind {
            InvokeKind::Get => member.get,
            InvokeKind::Put => member.let_,
            InvokeKind::PutRef => member.set,
        };
        let Some(p) = proc.filter(|&p| visible(p)) else {
            if kind == InvokeKind::Put && member.get.is_some_and(visible) {
                if let Some(g) = member.get {
                    if code.procs[g].kind != crate::ast::ProcKind::PropertyGet {
                        return Err(VbaError::new(if items.is_empty() { 446 } else { 450 }));
                    }
                }
                return Err(VbaError::new(451));
            }
            return Err(VbaError::new(438));
        };

        if kind == InvokeKind::Get
            && code.procs[p].kind == crate::ast::ProcKind::PropertyGet
            && items.len() > code.procs[p].params.len()
            && !code.procs[p].params.iter().any(|q| q.param_array)
        {
            return Err(VbaError::new(451));
        }
        let cells = self.bind_items(&code.procs[p].params, items)?;
        self.run(p, cells, me, None)
    }

    pub(super) fn connections(
        &self,
        source: Option<u64>,
        host: Option<ObjectId>,
        event: &str,
    ) -> Vec<Connection> {
        let code = self.code;
        let same = |v: &Variant| match v {
            Variant::Dispatch(Some(id)) => match (instance_of(*id), source, host) {
                (Some((k, _)), Some(s), _) => k == s,
                (None, _, Some(h)) => *id == h,
                _ => false,
            },
            _ => false,
        };
        let handler = |module: usize, var: &str| {
            code.proc_names
                .get(&(module, format!("{var}_{event}").to_ascii_lowercase()))
                .copied()
        };
        let mut out = Vec::new();
        for (i, g) in code.globals.iter().enumerate() {
            let Some(m) = g.with_events else { continue };
            let Cell::Own(s) = &self.st.globals[i] else {
                continue;
            };
            if !same(&s.val) {
                continue;
            }
            if let Some(h) = handler(m, &g.name) {
                out.push(Connection {
                    seq: self.st.seq.get(&i).copied().unwrap_or(0),
                    handler: h,
                    me: None,
                });
            }
        }
        for (&k, x) in &self.st.instances {
            let Some(ci) = code.class(x.class) else {
                continue;
            };
            for (i, f) in ci.fields.iter().enumerate() {
                if f.with_events.is_none() {
                    continue;
                }
                let Some(Cell::Own(s)) = x.fields.get(i) else {
                    continue;
                };
                if !same(&s.val) {
                    continue;
                }
                if let Some(h) = handler(x.class, &f.name) {
                    out.push(Connection {
                        seq: x.seq[i],
                        handler: h,
                        me: Some(make_id(k, 0)),
                    });
                }
            }
        }
        out.sort_by_key(|c| c.seq);
        out
    }

    pub(super) fn raise_event(
        &mut self,
        event: usize,
        args: &[crate::ir::CArg],
    ) -> Result<(), VbaError> {
        let code = self.code;
        let Some(key) = self.me.and_then(instance_of).map(|(k, _)| k) else {
            return Ok(());
        };
        let Some(ev) = code.class(self.module).and_then(|c| c.events.get(event)) else {
            return Ok(());
        };
        let cells = self.bind_args(&ev.params, args)?;
        for c in self.connections(Some(key), None, &ev.name) {
            let mut mine = cells.clone();
            mine.truncate(code.procs[c.handler].params.len());
            if mine.len() != code.procs[c.handler].params.len() {
                return Err(VbaError::new(450));
            }
            self.run(c.handler, mine, c.me, None)?;
        }
        Ok(())
    }
}

impl Exec<'_> {
    fn new_collection(&mut self) -> ObjectId {
        let key = self.st.next_key;
        self.st.next_key += 1;
        self.st.instances.insert(
            key,
            Instance {
                class: COLLECTION_CLASS,
                fields: Vec::new(),
                seq: Vec::new(),
                terminating: false,
                keys: Vec::new(),
                log: Vec::new(),
            },
        );
        let id = make_id(key, 0);
        let depth = self.st.frames.len();
        self.st.pins.push((depth, id));
        id
    }

    pub(super) fn collection_log(&self, id: ObjectId) -> Vec<(bool, usize)> {
        instance_of(id)
            .and_then(|(key, _)| self.st.instances.get(&key))
            .map(|x| x.log.clone())
            .unwrap_or_default()
    }

    pub(super) fn collection_items(&self, id: ObjectId) -> Option<Vec<Variant>> {
        let (key, _) = instance_of(id)?;
        let x = self.st.instances.get(&key)?;
        (x.class == COLLECTION_CLASS).then(|| {
            x.fields
                .iter()
                .map(|c| match c {
                    Cell::Own(s) => s.val.clone(),
                    Cell::Ref(_) => Variant::Empty,
                })
                .collect()
        })
    }

    fn collection_pos(&self, key: u64, index: &Variant, remove: bool) -> Result<usize, VbaError> {
        let x = &self.st.instances[&key];
        if let Variant::Bstr(s) = index {
            return x
                .keys
                .iter()
                .position(|k| k.as_ref().is_some_and(|k| self.same_key(k, s)))
                .ok_or_else(|| VbaError::new(5));
        }
        let n = self.cv.to_i64(&super::Val::V(index.clone()))?;
        usize::try_from(n - 1)
            .ok()
            .filter(|&i| i < x.fields.len())
            .ok_or_else(|| VbaError::new(if remove && x.fields.is_empty() { 5 } else { 9 }))
    }

    fn same_key(&self, a: &str, b: &str) -> bool {
        matches!(
            super::ops::var_cmp(
                &Variant::Bstr(a.to_string()),
                &Variant::Bstr(b.to_string()),
                true,
                self.cv.locale
            ),
            Ok(Some(std::cmp::Ordering::Equal))
        )
    }

    fn collection_member(
        &mut self,
        key: u64,
        name: &str,
        kind: InvokeKind,
        items: Vec<Item>,
    ) -> Result<Variant, VbaError> {
        if kind != InvokeKind::Get {
            return Err(VbaError::new(438));
        }

        let implicit = name.is_empty();
        let name = if implicit { "item" } else { name }.to_ascii_lowercase();
        let order: &[&str] = match name.as_str() {
            "add" => &["item", "key", "before", "after"],
            "count" => &[],
            "item" | "remove" => &["index"],
            _ => return Err(VbaError::new(438)),
        };
        let mut args: Vec<Option<Variant>> = vec![None; order.len()];
        let mut next = 0;
        for it in items {
            let slot = match &it.name {
                Some(n) => order.iter().position(|o| o.eq_ignore_ascii_case(n)),
                None => {
                    next += 1;
                    Some(next - 1).filter(|&i| i < order.len())
                }
            };
            let slot =
                slot.ok_or_else(|| VbaError::new(if it.name.is_some() { 448 } else { 450 }))?;
            if it.val != super::MISSING {
                args[slot] = Some(it.val);
            }
        }
        match name.as_str() {
            "count" => {
                let n = self.st.instances[&key].fields.len();
                Ok(Variant::I4(n as i32))
            }
            "item" | "remove" => {
                let index = args[0]
                    .take()
                    .ok_or_else(|| VbaError::new(if implicit { 450 } else { 449 }))?;
                let at = self.collection_pos(key, &index, name == "remove")?;
                if name == "item" {
                    return match &self.st.instances[&key].fields[at] {
                        Cell::Own(s) => Ok(s.val.clone()),
                        Cell::Ref(_) => Ok(Variant::Empty),
                    };
                }
                if let Some(x) = self.st.instances.get_mut(&key) {
                    x.fields.remove(at);
                    x.keys.remove(at);
                    x.log.push((true, at));
                }
                self.st.dirty = true;
                Ok(Variant::Empty)
            }
            _ => {
                let item = args[0].take().ok_or_else(|| VbaError::new(449))?.canon();
                let ckey = match args[1].take() {
                    Some(Variant::Bstr(s)) => Some(s),
                    Some(_) => return Err(VbaError::new(13)),
                    None => None,
                };
                if let Some(k) = &ckey {
                    let taken = self.st.instances[&key]
                        .keys
                        .iter()
                        .any(|o| o.as_ref().is_some_and(|o| self.same_key(o, k)));
                    if taken {
                        return Err(VbaError::new(457));
                    }
                }
                let at = match (args[2].take(), args[3].take()) {
                    (Some(_), Some(_)) => return Err(VbaError::new(5)),

                    (Some(_), None) | (None, Some(_))
                        if self.st.instances[&key].fields.is_empty() =>
                    {
                        return Err(VbaError::new(5))
                    }
                    (Some(b), None) => self.collection_pos(key, &b, false)?,
                    (None, Some(a)) => self.collection_pos(key, &a, false)? + 1,
                    (None, None) => self.st.instances[&key].fields.len(),
                };
                let cell = Cell::Own(super::Slot::value(VType::Variant, item));
                if let Some(x) = self.st.instances.get_mut(&key) {
                    x.fields.insert(at, cell);
                    x.keys.insert(at, ckey);
                    x.log.push((false, at));
                }
                Ok(Variant::Empty)
            }
        }
    }
}
