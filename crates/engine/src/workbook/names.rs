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
}

impl Names {
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

    pub fn remove(&mut self, key: &str, scope: Option<SheetId>) -> Option<NameDef> {
        if let Some(s) = scope {
            self.global_bound.remove(&(s, Text::from(key)));
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
}
