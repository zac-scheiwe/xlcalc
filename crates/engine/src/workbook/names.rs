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

    pub formula: Arc<Formula>,
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
        self.map.insert(key, def)
    }

    pub fn remove(&mut self, key: &str, scope: Option<SheetId>) -> Option<NameDef> {
        self.map.remove(&(scope, Text::from(key)))
    }

    pub fn iter(&self) -> impl Iterator<Item = &NameDef> {
        self.map.values()
    }
}
