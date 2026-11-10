use std::sync::Arc;

use super::sheetops::{remap_sheet, DEAD};
use super::{ext, Command, EditError, Entry, NameDef, Sheet, SheetState, Workbook};
use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::RecalcReport;
use crate::formula::{self, Formula, Node, SheetRef};
use crate::formula_print::print_node;
use crate::text;

impl Workbook {
    pub fn import_sheet(
        &mut self,
        src: &Workbook,
        id: SheetId,
        src_book: &str,
        before: Option<SheetId>,
    ) -> Result<SheetId, EditError> {
        self.command(Command::CopySheet, |wb| {
            wb.import_sheet_run(src, id, src_book, before)
        })
    }

    pub fn external_sheet(&self, book: &str, name: &str) -> SheetId {
        self.ext_resolve(book, name).unwrap_or(DEAD)
    }

    fn import_sheet_run(
        &mut self,
        src: &Workbook,
        sid: SheetId,
        src_book: &str,
        before: Option<SheetId>,
    ) -> Result<SheetId, EditError> {
        let from = src.sheet(sid).ok_or(EditError::NoSuchSheet)?;
        let at = match before {
            Some(b) => self.sheet_position(b).ok_or(EditError::NoSuchSheet)?,
            None => self.sheets.len(),
        };
        let name = if self.sheet_by_name(&from.name).is_none() {
            from.name.clone()
        } else {
            self.copy_name(&from.name)?
        };
        let kind = from.kind;
        let new = SheetId(self.next_sheet);
        self.next_sheet += 1;
        self.session.active = new;
        if let Some(sel) = src.session.selection.get(&sid).cloned() {
            self.session.selection.insert(new, sel);
        }
        self.sheets.insert(
            at,
            Sheet {
                id: new,
                name,
                state: SheetState::Visible,
                kind,
                cells: Default::default(),
                spills: Default::default(),
                dirty: Default::default(),
            },
        );

        let this = &*self;
        let mut redirect = |sr: &SheetRef| match sr {
            SheetRef::Sheet(s) if *s == sid => Some(SheetRef::Sheet(new)),
            SheetRef::Sheet(s) if s.0 >= ext::EXT_BASE => {
                let (link, sheet) = src.ext_sheet_info(*s)?;
                Some(SheetRef::Sheet(this.external_sheet(&link.path, &sheet)))
            }
            SheetRef::Sheet(s) => {
                let name = src.sheet(*s).map(|x| x.name.clone());
                Some(SheetRef::Sheet(
                    name.map_or(DEAD, |n| this.external_sheet(src_book, &n)),
                ))
            }
            SheetRef::Span(..) => Some(SheetRef::Sheet(DEAD)),
            _ => None,
        };

        let mut table_names: Vec<(String, String)> = Vec::new();
        let mut tables = Vec::new();
        for t in src.tables.iter().filter(|t| t.sheet == sid) {
            let nn = if self.table_by_name(&t.name).is_none() {
                t.name.clone()
            } else {
                self.copy_table_name(&t.name)
            };
            table_names.push((t.name.clone(), nn.clone()));
            let mut copy = t.clone();
            copy.rename_in_snapshots(&t.name, &nn);
            copy.name = nn;
            copy.sheet = new;
            tables.push(copy);
        }
        let mut for_copy = |n: &Node| match n {
            Node::StructRef {
                table,
                implicit,
                spec,
            } => table_names
                .iter()
                .find(|(o, _)| text::eq_ordinal_ci(o, table))
                .map(|(_, nn)| Node::StructRef {
                    table: nn.as_str().into(),
                    implicit: *implicit,
                    spec: spec.clone(),
                }),
            n => remap_sheet(n, &mut redirect),
        };

        let entries: Vec<(u32, u32, Entry)> = match from.cells.bounds() {
            Some(b) => from
                .cells
                .iter_rect(Rect::new(b.r0, b.c0, b.r1, b.c1))
                .map(|(r, c, e)| (r, c, e.clone()))
                .collect(),
            None => Vec::new(),
        };
        let mut mapped = Vec::new();
        for (r, c, e) in entries {
            match e {
                Entry::Const(v) => mapped.push((r, c, Some(v), None)),
                Entry::Formula(fid) => {
                    let f = src.formula(fid);
                    let root = formula::map_node(&f.formula.root, &mut for_copy);
                    mapped.push((
                        r,
                        c,
                        None,
                        Some((root, f.formula.volatile, f.value.clone(), f.array)),
                    ));
                }
            }
        }

        let mut names = Vec::new();
        for d in src.names.iter().filter(|d| d.scope == Some(sid)) {
            names.push((d.clone(), formula::map_node(&d.formula.root, &mut for_copy)));
        }

        let key_of = |d: &NameDef| crate::value::Text::from(d.name.to_uppercase());
        let local_keys: Vec<crate::value::Text> = src
            .names
            .iter()
            .filter(|d| d.scope == Some(sid))
            .map(key_of)
            .collect();
        let mut wanted: Vec<crate::value::Text> = Vec::new();
        let mut used: Vec<crate::value::Text> = Vec::new();
        let note = |n: &Node, used: &mut Vec<crate::value::Text>| match n {
            Node::Name {
                key, sheet: None, ..
            }
            | Node::NamedCall { key, .. }
                if !used.contains(key) =>
            {
                used.push(key.clone());
            }
            _ => {}
        };
        if let Some(b) = from.cells.bounds() {
            for (_, _, e) in from.cells.iter_rect(Rect::new(b.r0, b.c0, b.r1, b.c1)) {
                if let Entry::Formula(f) = e {
                    formula::walk(&src.formula(*f).formula.root, &mut |n| note(n, &mut used));
                }
            }
        }
        for d in src.names.iter().filter(|d| d.scope == Some(sid)) {
            formula::walk(&d.formula.root, &mut |n| note(n, &mut used));
        }
        loop {
            let mut grew = false;
            for d in src.names.iter().filter(|d| d.scope.is_none()) {
                let k = key_of(d);
                if wanted.contains(&k) || local_keys.contains(&k) {
                    continue;
                }
                let mut hit = used.contains(&k);
                formula::walk(&d.formula.root, &mut |n| match n {
                    Node::Ref(SheetRef::Sheet(x), _) | Node::DeadRef(SheetRef::Sheet(x)) => {
                        hit |= *x == sid;
                    }
                    Node::Name {
                        key, sheet: None, ..
                    } => hit |= wanted.contains(key),
                    _ => {}
                });
                if hit {
                    wanted.push(k);

                    formula::walk(&d.formula.root, &mut |n| note(n, &mut used));
                    grew = true;
                }
            }
            if !grew {
                break;
            }
        }
        let mut globals = Vec::new();
        for d in src.names.iter().filter(|d| d.scope.is_none()) {
            if wanted.contains(&key_of(d)) {
                let here = self.names.get(&key_of(d), None).is_some();
                let root = formula::map_node(&d.formula.root, &mut for_copy);
                let text = print_node(self, &root, CellAddr::new(new, 0, 0));
                globals.push((d.clone(), root, here, text));
            }
        }

        let own: Vec<crate::value::Text> = local_keys
            .iter()
            .cloned()
            .chain(globals.iter().filter(|g| g.2).map(|g| key_of(&g.0)))
            .collect();
        for g in &mut globals {
            g.1 = formula::map_node(&g.1, &mut |n| match n {
                Node::Name {
                    key,
                    display,
                    sheet: None,
                } if own.contains(key) => Some(Node::Name {
                    key: key.clone(),
                    display: display.clone(),
                    sheet: Some(SheetRef::Sheet(new)),
                }),
                _ => None,
            });
        }
        let mut copied = Vec::new();
        for (r, c, v, f) in mapped {
            let addr = CellAddr::new(new, r, c);
            if let Some(v) = v {
                self.sheet_mut(new)
                    .unwrap()
                    .cells
                    .set(r, c, Entry::Const(v));
            }
            if let Some((root, volatile, cached, array)) = f {
                let text = print_node(self, &root, addr);
                let nf = self.alloc_formula(addr, Arc::new(Formula { root, volatile }), text);
                self.note_hidden_names(nf);
                self.formula_mut(nf).value = cached;
                self.formula_mut(nf).array = array;
                self.arrays_seen |= array.is_some_and(|(r, c)| r > 1 || c > 1);
                self.sheet_mut(new)
                    .unwrap()
                    .cells
                    .set(r, c, Entry::Formula(nf));
                copied.push(nf);
            }
        }
        for (d, root) in names {
            let host = CellAddr::new(new, 0, 0);
            self.note_hidden_in(&root, None);
            self.names.insert(NameDef {
                name: d.name.clone(),
                scope: Some(new),
                refers_to: print_node(self, &root, host),
                formula: Arc::new(Formula {
                    root,
                    volatile: d.formula.volatile,
                }),
            });
        }
        for (d, root, here, text) in globals {
            let scope = here.then_some(new);
            self.note_hidden_in(&root, None);
            self.names.insert(NameDef {
                name: d.name.clone(),
                scope,
                refers_to: text,
                formula: Arc::new(Formula {
                    root,
                    volatile: d.formula.volatile,
                }),
            });
        }
        self.names.rebind(new);
        self.tables_made += tables.len();
        self.tables.extend(tables);
        self.cellfmt.import_sheet(&src.cellfmt, sid, new);
        self.import_merges(src, sid, new);
        if let Some(v) = src.views.get(&sid).cloned() {
            self.views.insert(new, v);
        }
        if let Some(c) = src.cfv.get(&sid).cloned() {
            self.cfv.insert(new, c);
        }
        for (k, t) in src.apostrophe.iter().filter(|(k, _)| k.sheet == sid) {
            self.apostrophe
                .insert(CellAddr::new(new, k.row, k.col), t.clone());
        }
        self.reserve_array_blocks();
        let auto = self.settings.mode == crate::CalcMode::Automatic;
        self.silently(|wb| wb.relink_and_recalc());
        if auto {
            self.dirty_all(&copied);
        }
        self.recalc_point();
        Ok(new)
    }

    pub fn leave_sheet(&mut self, id: SheetId, to: SheetId) -> Result<RecalcReport, EditError> {
        self.delete_sheet_moved(id, to)
    }
}
