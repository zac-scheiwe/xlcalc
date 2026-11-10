use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::address::{CellAddr, SheetId};
use crate::formula::Formula;
use crate::value::Text;

#[derive(Debug, Clone)]
pub struct NameDef {
    pub name: String,

    pub scope: Option<SheetId>,

    pub refers_to: String,

    pub(crate) formula: Arc<Formula>,
}

impl NameDef {
    pub fn host(&self) -> CellAddr {
        CellAddr::new(self.scope.unwrap_or(SheetId(0)), 0, 0)
    }
}

#[derive(Default, Debug)]
pub struct Names {
    map: FxHashMap<(Option<SheetId>, Text), NameDef>,

    placeholders: FxHashMap<Text, Text>,

    deleted_scoped: rustc_hash::FxHashSet<(SheetId, Text)>,

    global_bound: FxHashMap<(SheetId, Text), Vec<Text>>,

    copy_stale: FxHashMap<(SheetId, Text), Vec<Text>>,

    hidden: std::collections::BTreeMap<String, String>,

    invisible: rustc_hash::FxHashSet<(Option<SheetId>, Text)>,

    macro_types: FxHashMap<(Option<SheetId>, Text), i32>,
}

impl Names {
    pub fn macro_type(&self, key: &str, scope: Option<SheetId>) -> Option<i32> {
        self.macro_types.get(&(scope, key.into())).copied()
    }

    pub fn set_macro_type(&mut self, key: &str, scope: Option<SheetId>, t: Option<i32>) {
        match t {
            Some(t) => {
                self.macro_types.insert((scope, key.into()), t);
            }
            None => {
                self.macro_types.remove(&(scope, key.into()));
            }
        }
    }

    pub fn get(&self, key: &str, scope: Option<SheetId>) -> Option<&NameDef> {
        self.map.get(&(scope, Text::from(key)))
    }

    pub fn lookup(&self, key: &str, sheet: SheetId) -> Option<&NameDef> {
        self.get(key, Some(sheet)).or_else(|| self.get(key, None))
    }

    pub fn placeholder(&self, key: &str) -> Option<&Text> {
        self.placeholders.get(key)
    }

    pub fn note_placeholder(&mut self, key: &str, display: &Text) {
        self.placeholders
            .entry(Text::from(key))
            .or_insert_with(|| display.clone());
    }

    pub fn insert(&mut self, def: NameDef) -> Option<NameDef> {
        let key = (def.scope, Text::from(def.name.to_uppercase()));
        self.invisible.remove(&key);
        if let Some(s) = def.scope {
            self.deleted_scoped.remove(&(s, key.1.clone()));
            let bound = self.unbound_locals(&def, s);
            self.global_bound.insert((s, key.1.clone()), bound);
        }
        self.map.insert(key, def)
    }

    fn unbound_locals(&self, def: &NameDef, scope: SheetId) -> Vec<Text> {
        let mut out = Vec::new();
        crate::formula::walk(&def.formula.root, &mut |n| {
            if let crate::formula::Node::Name {
                key, sheet: None, ..
            } = n
            {
                if self.get(key, Some(scope)).is_none() && !out.contains(key) {
                    out.push(key.clone());
                }
            }
        });
        out
    }

    pub(crate) fn rebind(&mut self, scope: SheetId) {
        let keys: Vec<Text> = self
            .map
            .keys()
            .filter(|(s, _)| *s == Some(scope))
            .map(|(_, k)| k.clone())
            .collect();
        for k in keys {
            let bound = self.unbound_locals(&self.map[&(Some(scope), k.clone())], scope);
            self.global_bound.insert((scope, k), bound);
        }
    }

    pub(crate) fn bound_global(&self, scope: SheetId, name: &str, key: &str) -> bool {
        self.global_bound
            .get(&(scope, Text::from(name)))
            .is_some_and(|v| v.iter().any(|k| &**k == key))
    }

    pub(crate) fn mark_copy_stale(&mut self, scope: SheetId, name: Text, keys: Vec<Text>) {
        if keys.is_empty() {
            self.copy_stale.remove(&(scope, name));
        } else {
            self.copy_stale.insert((scope, name), keys);
        }
    }

    pub(crate) fn is_copy_stale(&self, scope: SheetId, name: &str, key: &str) -> bool {
        self.copy_stale
            .get(&(scope, Text::from(name)))
            .is_some_and(|v| v.iter().any(|k| &**k == key))
    }

    pub fn remove(&mut self, key: &str, scope: Option<SheetId>) -> Option<NameDef> {
        self.invisible.remove(&(scope, Text::from(key)));
        if let Some(s) = scope {
            self.global_bound.remove(&(s, Text::from(key)));
            self.copy_stale.remove(&(s, Text::from(key)));
        }
        self.map.remove(&(scope, Text::from(key)))
    }

    pub fn delete(&mut self, key: &str, scope: Option<SheetId>) -> Option<NameDef> {
        let d = self.remove(key, scope)?;
        if let Some(s) = scope {
            self.deleted_scoped.insert((s, Text::from(key)));
        }
        Some(d)
    }

    pub fn was_deleted(&self, key: &str, sheet: SheetId) -> bool {
        self.deleted_scoped.contains(&(sheet, Text::from(key)))
    }

    pub fn iter(&self) -> impl Iterator<Item = &NameDef> {
        self.map.values()
    }

    pub fn is_visible(&self, key: &str, scope: Option<SheetId>) -> bool {
        !self.invisible.contains(&(scope, Text::from(key)))
    }

    pub fn set_visible(&mut self, key: &str, scope: Option<SheetId>, visible: bool) {
        let k = (scope, Text::from(key));
        if visible {
            self.invisible.remove(&k);
        } else if self.map.contains_key(&k) {
            self.invisible.insert(k);
        }
    }

    pub(crate) fn note_hidden(&mut self, name: &str) {
        self.hidden
            .entry(name.to_uppercase())
            .or_insert_with(|| name.to_string());
    }

    pub fn hidden(&self) -> impl Iterator<Item = &str> {
        self.hidden.values().map(String::as_str)
    }

    pub fn hidden_get(&self, key: &str) -> Option<&str> {
        self.hidden.get(key).map(String::as_str)
    }

    pub fn delete_hidden(&mut self, key: &str) -> bool {
        self.hidden.remove(key).is_some()
    }
}
