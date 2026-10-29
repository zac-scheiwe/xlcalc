use std::sync::Arc;

use crate::address::{CellAddr, Rect, SheetId};
use crate::calc::RecalcReport;
use crate::formula::{self, Formula, Node, SheetRef};
use crate::formula_print::print_node;
use crate::text;
use crate::workbook::{tables::Table, EditError, Entry, FormulaId, NameDef, Sheet, Workbook};

const DEAD: SheetId = SheetId(0);

pub const MAX_SHEET_NAME: usize = 31;

fn remap_sheet(n: &Node, f: &mut dyn FnMut(&SheetRef) -> Option<SheetRef>) -> Option<Node> {
    match n {
        Node::Ref(sr, c) => f(sr).map(|s| Node::Ref(s, *c)),
        Node::Name {
            key,
            display,
            sheet: Some(sr),
        } => f(sr).map(|s| Node::Name {
            key: key.clone(),
            display: display.clone(),
            sheet: Some(s),
        }),
        Node::DeadRef(sr) => f(sr).map(Node::DeadRef),
        _ => None,
    }
}

impl Workbook {
    pub(crate) fn names_follow(
        &mut self,
        src_sheet: SheetId,
        src: Rect,
        dst_sheet: SheetId,
        translate: bool,
    ) {
        if src_sheet == dst_sheet || self.names.iter().next().is_none() {
            return;
        }
        let mut keys: Vec<crate::value::Text> = Vec::new();
        let fids: Vec<FormulaId> = match self.sheet(src_sheet) {
            Some(sh) => sh
                .cells
                .iter_rect(src)
                .filter_map(|(_, _, e)| match e {
                    Entry::Formula(f) => Some(*f),
                    _ => None,
                })
                .collect(),
            None => return,
        };
        for fid in fids {
            formula::walk(&self.formula(fid).formula.root, &mut |n| match n {
                Node::Name {
                    key, sheet: None, ..
                }
                | Node::NamedCall { key, .. }
                    if !keys.contains(key) =>
                {
                    keys.push(key.clone());
                }
                _ => {}
            });
        }
        for key in keys {
            let Some(def) = self.names.get(&key, Some(src_sheet)).cloned() else {
                continue;
            };
            if self.names.get(&key, Some(dst_sheet)).is_some()
                || self.names.get(&key, None).is_some()
            {
                continue;
            }
            let global = self.names.placeholder(&key).is_some();
            let scope = if global { None } else { Some(dst_sheet) };
            let root = if translate {
                let mut redirect = |sr: &SheetRef| match sr {
                    SheetRef::Sheet(s) if *s == src_sheet => Some(SheetRef::Sheet(dst_sheet)),
                    _ => None,
                };
                formula::map_node(&def.formula.root, &mut |n| remap_sheet(n, &mut redirect))
            } else {
                def.formula.root.clone()
            };
            let mut copy = NameDef {
                scope,
                formula: Arc::new(Formula {
                    root,
                    volatile: def.formula.volatile,
                }),
                ..def
            };
            copy.refers_to = print_node(self, &copy.formula.root, copy.host());
            self.names.insert(copy);
            crate::calc::name_changed(self, &key);
        }
    }

    pub fn check_sheet_name(&self, name: &str, except: Option<SheetId>) -> Result<(), EditError> {
        let bad = name.is_empty()
            || text::utf16_len(name) > MAX_SHEET_NAME
            || name.chars().any(|c| ":\\/?*[]".contains(c))
            || name.starts_with('\'')
            || name.ends_with('\'')
            || text::eq_sheet_name(name, "History");
        if bad {
            return Err(EditError::BadName(name.to_string()));
        }
        if self
            .sheets
            .iter()
            .any(|s| Some(s.id) != except && text::eq_sheet_name(&s.name, name))
        {
            return Err(EditError::DuplicateSheet(name.to_string()));
        }
        Ok(())
    }

    pub fn name_refers_to(&self, def: &NameDef) -> String {
        let (r, c) = self.active_cell();
        let host = CellAddr::new(def.scope.unwrap_or(SheetId(0)), r, c);
        print_node(self, &def.formula.root, host)
    }

    fn positions(&self) -> Vec<SheetId> {
        self.sheets.iter().map(|s| s.id).collect()
    }

    fn reprint_all(&mut self) {
        let ids: Vec<FormulaId> = self.formula_ids().collect();
        let mut any_changed = false;
        for fid in ids.iter().copied() {
            let text = {
                let f = self.formula(fid);
                print_node(self, &f.formula.root, f.addr)
            };
            any_changed |= self.formula(fid).text != text;
            self.formula_mut(fid).text = text;
        }

        if any_changed {
            for fid in ids {
                let mut has = false;
                formula::walk(&self.formula(fid).formula.root, &mut |n| {
                    if let Node::Func { id, .. } = n {
                        has |= crate::functions::def(*id).name == "FORMULATEXT";
                    }
                });
                if has {
                    crate::calc::mark_dirty(self, fid);
                }
            }
        }
        self.reprint_names();
    }

    fn reprint_names(&mut self) {
        let defs: Vec<NameDef> = self.names.iter().cloned().collect();
        for mut def in defs {
            let host = def.host();
            def.refers_to = print_node(self, &def.formula.root, host);
            self.names.insert(def);
        }
    }

    fn rewrite_all(&mut self, f: &mut dyn FnMut(&Node) -> Option<Node>) -> RewritePlan {
        let mut plan = RewritePlan::default();
        let ids: Vec<FormulaId> = self.formula_ids().collect();
        for fid in ids {
            let cell = self.formula(fid);
            let mut changed = false;
            let root = formula::map_node(&cell.formula.root, &mut |n| {
                let r = f(n);
                changed |= r.is_some();
                r
            });
            if changed {
                plan.formulas.push((fid, root, cell.formula.volatile));
            }
        }
        let defs: Vec<NameDef> = self.names.iter().cloned().collect();
        for def in defs {
            let mut changed = false;
            let root = formula::map_node(&def.formula.root, &mut |n| {
                let r = f(n);
                changed |= r.is_some();
                r
            });
            if changed {
                plan.names.push((def, root));
            }
        }
        plan
    }

    fn apply_plan(&mut self, plan: RewritePlan) {
        for (fid, root, volatile) in plan.formulas {
            let host = self.formula(fid).addr;
            let text = print_node(self, &root, host);
            let c = self.formula_mut(fid);
            c.formula = Arc::new(Formula { root, volatile });
            c.text = text;
        }
        for (mut def, root) in plan.names {
            let host = def.host();
            def.refers_to = print_node(self, &root, host);
            def.formula = Arc::new(Formula {
                root,
                volatile: def.formula.volatile,
            });
            self.names.insert(def);
        }
    }

    pub(crate) fn normalize_spans(&self, f: Formula) -> Formula {
        let mut fix = |sr: &SheetRef| match sr {
            SheetRef::Span(a, b) => match (self.sheet_position(*a), self.sheet_position(*b)) {
                (Some(x), Some(y)) if x > y => Some(SheetRef::Span(*b, *a)),
                _ => None,
            },
            _ => None,
        };
        let mut changed = false;
        let root = formula::map_node(&f.root, &mut |n| {
            let r = remap_sheet(n, &mut fix);
            changed |= r.is_some();
            r
        });
        if changed {
            Formula {
                root,
                volatile: f.volatile,
            }
        } else {
            f
        }
    }

    pub fn rename_sheet(&mut self, id: SheetId, new: &str) -> Result<RecalcReport, EditError> {
        let pos = self.sheet_position(id).ok_or(EditError::NoSuchSheet)?;

        let new = new.split(' ').next().unwrap_or("");
        self.check_sheet_name(new, Some(id))?;
        self.sheets[pos].name = new.to_string();
        self.reprint_all();

        if let Some(cell) = crate::functions::lookup("CELL") {
            let hits: Vec<FormulaId> = self
                .formula_ids()
                .filter(|&f| {
                    let mut hit = false;
                    formula::walk(&self.formula(f).formula.root, &mut |n| {
                        hit |= matches!(n, Node::Func { id, .. } if *id == cell);
                    });
                    hit
                })
                .collect();
            for f in hits {
                crate::calc::mark_dirty(self, f);
            }
        }
        Ok(self.after_edit(vec![]))
    }

    fn release_stale_copies(&mut self) {
        for fid in std::mem::take(&mut self.calc.stale_copy) {
            if self.formulas.get(fid as usize).is_some_and(|f| f.is_some()) {
                crate::calc::mark_dirty(self, fid);
            }
        }
    }

    pub fn delete_sheet(&mut self, id: SheetId) -> Result<RecalcReport, EditError> {
        self.release_stale_copies();
        let pos = self.sheet_position(id).ok_or(EditError::NoSuchSheet)?;
        if self.sheets.len() <= 1 {
            return Err(EditError::Compile(
                "a workbook must contain at least one visible worksheet".into(),
            ));
        }
        if self.session.active == id {
            let next = self
                .sheets
                .get(pos + 1)
                .or_else(|| self.sheets.get(pos.wrapping_sub(1)));
            if let Some(s) = next {
                self.session.active = s.id;
            }
        }
        let order = self.positions();
        let removed_tables: Vec<String> = self
            .tables
            .iter()
            .filter(|t| t.sheet == id)
            .map(|t| t.name.clone())
            .collect();
        let mut give_up = |sr: &SheetRef| span_without(sr, &order, id);
        let plan = self.rewrite_all(&mut |n| match n {
            Node::StructRef { table, .. }
                if removed_tables.iter().any(|r| text::eq_ordinal_ci(r, table)) =>
            {
                Some(Node::DeadRef(SheetRef::Current))
            }
            n => remap_sheet(n, &mut give_up),
        });

        let doomed: Vec<FormulaId> = self
            .formula_ids()
            .filter(|&f| self.formula(f).addr.sheet == id)
            .collect();
        for fid in doomed {
            self.free_formula(fid);
        }
        let plan = RewritePlan {
            formulas: plan
                .formulas
                .into_iter()
                .filter(|(f, ..)| self.formulas[*f as usize].is_some())
                .collect(),
            names: plan
                .names
                .into_iter()
                .filter(|(d, _)| d.scope != Some(id))
                .collect(),
        };
        self.tables.retain(|t| t.sheet != id);
        self.drop_sheet_data_tables(id);
        let scoped: Vec<String> = self
            .names
            .iter()
            .filter(|d| d.scope == Some(id))
            .map(|d| d.name.to_uppercase())
            .collect();
        for key in scoped {
            self.names.remove(&key, Some(id));
        }
        self.sheets.remove(pos);
        if self.calc.circular_ref.is_some_and(|a| a.sheet == id) {
            self.calc.circular_ref = None;
        }
        self.apply_plan(plan);
        self.reprint_names();
        Ok(self.relink_and_recalc())
    }

    pub fn move_sheet(
        &mut self,
        id: SheetId,
        before: Option<SheetId>,
    ) -> Result<RecalcReport, EditError> {
        let from = self.sheet_position(id).ok_or(EditError::NoSuchSheet)?;
        self.session.active = id;

        let next = self.sheets.get(from + 1).map(|s| s.id);
        if before == Some(id) || before == next {
            return Ok(RecalcReport::default());
        }
        self.release_stale_copies();
        let old = self.positions();
        let mut new = old.clone();
        new.remove(from);
        let to = match before {
            Some(b) => new
                .iter()
                .position(|&s| s == b)
                .ok_or(EditError::NoSuchSheet)?,
            None => new.len(),
        };
        new.insert(to, id);
        let mut cross = |sr: &SheetRef| span_after_move(sr, &old, &new, id);
        let plan = self.rewrite_all(&mut |n| remap_sheet(n, &mut cross));
        let sheet = self.sheets.remove(from);
        let to = match before {
            Some(b) => self
                .sheets
                .iter()
                .position(|s| s.id == b)
                .unwrap_or(self.sheets.len()),
            None => self.sheets.len(),
        };
        self.sheets.insert(to, sheet);
        self.apply_plan(plan);
        Ok(self.relink_and_recalc())
    }

    fn copy_name(&self, src: &str) -> Result<String, EditError> {
        let inner = src.strip_suffix(')').and_then(|s| {
            s.rfind('(')
                .map(|i| (&s[..i], s[i + 1..].trim_matches(' ')))
        });
        let mut parsed = None;
        if let Some((b, n)) = inner {
            let neg = n.strip_prefix('-');
            let digits = neg.unwrap_or(n);
            let ok = !digits.is_empty()
                && digits
                    .chars()
                    .all(|c| c.is_ascii_digit() || ('\u{660}'..='\u{669}').contains(&c));
            if ok && neg.is_some() {
                return Err(EditError::BadName(src.to_string()));
            }
            if ok {
                let v = digits.chars().try_fold(0u64, |a, c| {
                    let d = if c.is_ascii_digit() {
                        c as u32 - 48
                    } else {
                        c as u32 - 0x660
                    };
                    a.checked_mul(10)?.checked_add(d as u64)
                });
                parsed = v.filter(|v| *v <= 32767).map(|v| (b, v + 1));
            }
        }
        let (base, start, sep) = match parsed {
            Some((b, n)) => {
                let t = b.trim_end_matches(' ');
                (t, n, if t.len() == b.len() { "" } else { " " })
            }
            None => (src, 2, " "),
        };
        for k in start.. {
            let suffix = format!("{sep}({k})");
            let room = MAX_SHEET_NAME.saturating_sub(suffix.len());
            let mut cut = text::utf16_slice(base, 0, room);

            if cut.ends_with('\u{fffd}') && !base.contains('\u{fffd}') {
                cut.pop();
            }
            let name = format!("{cut}{suffix}");
            if self.sheet_by_name(&name).is_none() {
                return Ok(name);
            }
        }
        unreachable!()
    }

    pub(crate) fn copy_table_name(&self, base: &str) -> String {
        let mut n = self.tables_made + 2;
        loop {
            let name = if base.ends_with(|c: char| c.is_ascii_digit()) {
                format!("{base}{n}")
            } else {
                format!("{base}_{n}")
            };
            if self.table_by_name(&name).is_none() {
                return name;
            }
            n += 1;
        }
    }

    pub fn copy_sheet(
        &mut self,
        src: SheetId,
        before: Option<SheetId>,
    ) -> Result<SheetId, EditError> {
        let src_pos = self.sheet_position(src).ok_or(EditError::NoSuchSheet)?;
        self.release_stale_copies();
        let has_tables = self.tables.iter().any(|t| t.sheet == src);
        let at = match before {
            Some(b) => self.sheet_position(b).ok_or(EditError::NoSuchSheet)?,
            None => self.sheets.len(),
        };
        let name = self.copy_name(&self.sheets[src_pos].name.clone())?;
        let new = SheetId(self.next_sheet);
        self.next_sheet += 1;
        self.session.active = new;
        if let Some(&sel) = self.session.selection.get(&src) {
            self.session.selection.insert(new, sel);
        }

        let mut table_names: Vec<(String, String)> = Vec::new();
        let order = self.positions();
        let tables: Vec<Table> = self
            .tables
            .iter()
            .filter(|t| t.sheet == src)
            .cloned()
            .collect();
        for t in &tables {
            let nn = self.copy_table_name(&t.name);
            table_names.push((t.name.clone(), nn.clone()));
            let mut copy = t.clone();
            copy.name = nn;
            copy.sheet = new;
            self.tables_made += 1;
            self.tables.push(copy);
        }

        self.copy_sheet_data_tables(src, new);
        let mut redirect = |sr: &SheetRef| match sr {
            SheetRef::Sheet(s) if *s == src => Some(SheetRef::Sheet(new)),
            SheetRef::Span(a, b) if *a == src || *b == src => Some(SheetRef::Sheet(DEAD)),
            _ => None,
        };
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
        self.sheets.insert(
            at,
            Sheet {
                id: new,
                name,
                cells: Default::default(),
                spills: Default::default(),
                dirty: Default::default(),
            },
        );
        self.fmt_copy_sheet(src, new);

        let mut stale: Vec<(CellAddr, CellAddr)> = Vec::new();
        let entries: Vec<(u32, u32, Entry)> = {
            let sh = self.sheet(src).unwrap();
            match sh.cells.bounds() {
                Some(b) => sh
                    .cells
                    .iter_rect(Rect::new(b.r0, b.c0, b.r1, b.c1))
                    .map(|(r, c, e)| (r, c, e.clone()))
                    .collect(),
                None => Vec::new(),
            }
        };
        for (r, c, e) in entries {
            let addr = CellAddr::new(new, r, c);
            match e {
                Entry::Const(v) => {
                    self.sheet_mut(new)
                        .unwrap()
                        .cells
                        .set(r, c, Entry::Const(v));
                }
                Entry::Formula(fid) => {
                    let (root, volatile, cached, array) = {
                        let f = self.formula(fid);
                        let mut broken = false;
                        formula::walk(&f.formula.root, &mut |n| {
                            if let Node::Ref(SheetRef::Span(a, b), _) = n {
                                broken |= *a == src || *b == src;
                            }
                        });
                        if broken && has_tables {
                            stale.push((f.addr, addr));
                        }

                        let mut src_local = false;
                        formula::walk(&f.formula.root, &mut |n| {
                            if let Node::Name {
                                sheet: Some(SheetRef::Sheet(s)),
                                ..
                            } = n
                            {
                                src_local |= *s == src;
                            }
                        });

                        let keep_names = src_local
                            && !matches!(
                                f.formula.root,
                                Node::Unary(crate::parser::ast::UnaryOp::ImplicitQuiet, _)
                            )
                            && {
                                let text = print_node(self, &f.formula.root, f.addr);
                                let body = text.trim_start_matches('=');
                                let special = ["@", "[", "#REF!"].iter().any(|s| body.contains(s));
                                !special || crate::legacy::text_may_array(self, body, src)
                            };
                        (
                            formula::map_node(&f.formula.root, &mut |n: &Node| match n {
                                Node::Name {
                                    sheet: Some(SheetRef::Sheet(s)),
                                    ..
                                } if keep_names
                                    && *s == src
                                    && matches!(n, Node::Name { key, .. }
                                        if self.names.get(key, Some(src)).is_some()) =>
                                {
                                    Some(n.clone())
                                }
                                n => for_copy(n),
                            }),
                            f.formula.volatile,
                            f.value.clone(),
                            f.array,
                        )
                    };
                    let text = print_node(self, &root, addr);
                    let nf = self.alloc_formula(addr, Arc::new(Formula { root, volatile }), text);

                    self.formula_mut(nf).value = cached;
                    self.formula_mut(nf).array = array;
                    self.arrays_seen |= array.is_some_and(|(r, c)| r > 1 || c > 1);
                    self.sheet_mut(new)
                        .unwrap()
                        .cells
                        .set(r, c, Entry::Formula(nf));
                }
            }
        }
        self.copy_merges(src, new);
        self.reserve_array_blocks();

        let in_span = |a: SheetId, b: SheetId| {
            let pa = order.iter().position(|&s| s == a);
            let pb = order.iter().position(|&s| s == b);
            let ps = order.iter().position(|&s| s == src);
            match (pa, pb, ps) {
                (Some(x), Some(y), Some(p)) => x.min(y) <= p && p <= x.max(y),
                _ => false,
            }
        };
        let defs: Vec<NameDef> = self.names.iter().cloned().collect();
        let key_of = |d: &NameDef| crate::value::Text::from(d.name.to_uppercase());
        let local_keys: Vec<crate::value::Text> = defs
            .iter()
            .filter(|d| d.scope == Some(src))
            .map(key_of)
            .collect();

        let mut copied_globals: Vec<crate::value::Text> = Vec::new();
        loop {
            let mut grew = false;
            for def in defs.iter().filter(|d| d.scope.is_none()) {
                let k = key_of(def);
                if local_keys.contains(&k) || copied_globals.contains(&k) {
                    continue;
                }
                let mut hit = false;
                formula::walk(&def.formula.root, &mut |n| {
                    let sr = match n {
                        Node::Ref(sr, _) | Node::DeadRef(sr) => Some(sr),
                        Node::Name {
                            sheet: Some(sr), ..
                        } => Some(sr),
                        Node::Name {
                            key, sheet: None, ..
                        } => {
                            hit |=
                                copied_globals.contains(key) || self.names.get(key, None).is_none();
                            None
                        }
                        _ => None,
                    };
                    hit |= match sr {
                        Some(SheetRef::Sheet(s)) => *s == src,
                        Some(SheetRef::Span(a, b)) => in_span(*a, *b),
                        _ => false,
                    };
                });
                if hit {
                    copied_globals.push(k);
                    grew = true;
                }
            }
            if !grew {
                break;
            }
        }
        let gets_copy =
            |k: &crate::value::Text| local_keys.contains(k) || copied_globals.contains(k);

        let mut known: Vec<crate::value::Text> = Vec::new();
        let mut as_is: Vec<crate::value::Text> = Vec::new();
        let mut todo: Vec<crate::value::Text> = Vec::new();
        {
            let sh = self.sheet(src).unwrap();
            let mut fids: Vec<FormulaId> = Vec::new();
            if let Some(b) = sh.cells.bounds() {
                for (_, _, e) in sh.cells.iter_rect(Rect::new(b.r0, b.c0, b.r1, b.c1)) {
                    if let Entry::Formula(f) = e {
                        fids.push(*f);
                    }
                }
            }
            for fid in fids {
                formula::walk(&self.formula(fid).formula.root, &mut |n| match n {
                    Node::Name { key, sheet, .. } => {
                        if sheet.is_none()
                            || matches!(sheet, Some(SheetRef::Sheet(s)) if *s == src)
                                && !todo.contains(key)
                        {
                            todo.push(key.clone());
                        }
                    }
                    Node::NamedCall { key, .. } if !todo.contains(key) => todo.push(key.clone()),
                    _ => {}
                });
            }
        }
        while let Some(k) = todo.pop() {
            if !known.contains(&k) {
                known.push(k.clone());
            }

            if !gets_copy(&k) || as_is.contains(&k) {
                continue;
            }
            as_is.push(k.clone());
            let def = self
                .names
                .get(&k, Some(src))
                .or_else(|| self.names.get(&k, None));
            if let Some(def) = def {
                formula::walk(&def.formula.root, &mut |n| match n {
                    Node::Name { key, sheet, .. } => {
                        if !known.contains(key) {
                            known.push(key.clone());
                        }

                        let follow = copied_globals.contains(key)
                            || local_keys.contains(key) && self.names.get(key, None).is_some();
                        if sheet.is_none() && follow && !as_is.contains(key) {
                            todo.push(key.clone());
                        }
                    }
                    Node::NamedCall { key, .. } if !known.contains(key) => known.push(key.clone()),
                    _ => {}
                });
            }
        }
        let mut from_global = Vec::new();
        for def in defs.clone() {
            let k = key_of(&def);
            let wanted = match def.scope {
                Some(s) => s == src,
                None => copied_globals.contains(&k),
            };
            if !wanted {
                continue;
            }
            if def.scope.is_none() {
                from_global.push(def.name.clone());
            }

            let mut foreign = false;
            formula::walk(&def.formula.root, &mut |n| {
                if let Node::Name {
                    key,
                    sheet: Some(SheetRef::Sheet(s)),
                    ..
                } = n
                {
                    foreign |= *s != src && self.names.get(key, Some(*s)).is_some();
                }
            });
            let root = if foreign {
                Node::Lit(crate::value::Value::Error(crate::value::XlError::NA))
            } else {
                formula::map_node(&def.formula.root, &mut for_copy)
            };

            let root = if as_is.contains(&k) {
                root
            } else {
                formula::map_node(&root, &mut |n| match n {
                    Node::Name {
                        key,
                        display,
                        sheet: None,
                    } => {
                        let sheet = if gets_copy(key) {
                            SheetRef::Sheet(new)
                        } else if self.names.get(key, None).is_some() && !known.contains(key) {
                            SheetRef::Sheet(DEAD)
                        } else {
                            return None;
                        };
                        Some(Node::Name {
                            key: key.clone(),
                            display: display.clone(),
                            sheet: Some(sheet),
                        })
                    }
                    _ => None,
                })
            };
            let host = CellAddr::new(new, 0, 0);
            self.names.insert(NameDef {
                name: def.name.clone(),
                scope: Some(new),
                refers_to: print_node(self, &root, host),
                formula: Arc::new(Formula {
                    root,
                    volatile: def.formula.volatile,
                }),
            });
        }

        self.names.rebind(new);

        let ids: Vec<_> = self
            .formula_ids()
            .filter(|&f| self.formula(f).addr.sheet == new)
            .collect();
        for fid in ids {
            let (host, formula) = {
                let c = self.formula(fid);
                (c.addr, c.formula.clone())
            };
            let mut changed = false;
            let root = formula::map_node(&formula.root, &mut |n| match n {
                Node::Name {
                    key,
                    display,
                    sheet: Some(SheetRef::Sheet(gone)),
                } if (self.sheet(*gone).is_none()
                    || self.names.get(key, Some(*gone)).is_none())
                    && self
                        .names
                        .get(key, Some(new))
                        .is_some_and(|d| from_global.contains(&d.name)) =>
                {
                    changed = true;
                    Some(Node::Name {
                        key: key.clone(),
                        display: display.clone(),
                        sheet: Some(SheetRef::Sheet(new)),
                    })
                }
                _ => None,
            });
            if changed {
                let text = print_node(self, &root, host);
                let c = self.formula_mut(fid);
                c.formula = Arc::new(Formula {
                    root,
                    volatile: formula.volatile,
                });
                c.text = text;
            }
        }
        self.relink_and_recalc();
        for (from, to) in stale {
            if let (Some(a), Some(b)) = (self.formula_at(from), self.formula_at(to)) {
                let v = self.formula(a).value.clone();
                self.formula_mut(b).value = v;
                self.set_dirty(b, false);
                self.calc.stale_copy.push(b);
            }
        }
        Ok(new)
    }
}

#[derive(Default)]
struct RewritePlan {
    formulas: Vec<(FormulaId, Node, bool)>,
    names: Vec<(NameDef, Node)>,
}

fn span_ends(sr: &SheetRef, order: &[SheetId]) -> Option<(usize, usize)> {
    match sr {
        SheetRef::Span(a, b) => {
            let pa = order.iter().position(|s| s == a)?;
            let pb = order.iter().position(|s| s == b)?;
            Some((pa.min(pb), pa.max(pb)))
        }
        _ => None,
    }
}

fn span_without(sr: &SheetRef, order: &[SheetId], gone: SheetId) -> Option<SheetRef> {
    let (lo, hi) = span_ends(sr, order)?;
    let (slo, shi) = (order[lo], order[hi]);
    let (a, b) = if gone == slo {
        (order[lo + 1], shi)
    } else if gone == shi {
        (slo, order[hi - 1])
    } else {
        return None;
    };
    Some(if a == b {
        SheetRef::Sheet(a)
    } else {
        SheetRef::Span(a, b)
    })
}

fn span_after_move(
    sr: &SheetRef,
    old: &[SheetId],
    new: &[SheetId],
    moved: SheetId,
) -> Option<SheetRef> {
    let (lo, hi) = span_ends(sr, old)?;
    let (slo, shi) = (old[lo], old[hi]);
    if moved != slo && moved != shi {
        return None;
    }
    let (nlo, nhi) = (
        new.iter().position(|&s| s == slo)?,
        new.iter().position(|&s| s == shi)?,
    );
    if nlo < nhi {
        return None;
    }
    let (a, b) = if moved == slo {
        (old[lo + 1], shi)
    } else {
        (slo, old[hi - 1])
    };
    Some(if a == b {
        SheetRef::Sheet(a)
    } else {
        SheetRef::Span(a, b)
    })
}
