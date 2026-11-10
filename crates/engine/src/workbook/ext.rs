use std::sync::Mutex;

use super::{CellStore, DirtyIndex, Entry, Sheet, SheetKind, SheetState, SpillIndex, Workbook};
use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::calc::{self, RecalcReport};
use crate::formula::{self, Node, SheetRef};
use crate::value::Value;
use crate::workbook::notice::Command;

pub const EXT_BASE: u32 = 0x4000_0000;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ExtLink {
    pub path: String,

    pub open: bool,

    pub broken: bool,
}

impl ExtLink {
    pub fn name(&self) -> &str {
        split_path(&self.path).1
    }

    pub fn dir(&self) -> &str {
        split_path(&self.path).0
    }
}

pub fn split_path(p: &str) -> (&str, &str) {
    match p.rfind(['\\', '/']) {
        Some(i) => (&p[..=i], &p[i + 1..]),
        None => ("", p),
    }
}

#[derive(Default)]
pub(crate) struct ExtState {
    pub links: Vec<ExtLink>,

    pub sheets: Vec<(usize, String)>,

    pub names: Vec<(usize, ExtName)>,

    pub open: Vec<OpenBook>,

    pub tabs: Vec<(usize, Vec<String>)>,

    pub docs: Vec<String>,
    pub windows: Vec<String>,
}

#[derive(Clone, Debug, Default)]
pub struct OpenBook {
    pub path: String,
    pub sheets: Vec<String>,
}

impl ExtState {
    fn open_book(&self, key: &str) -> Option<&OpenBook> {
        let (dir, name) = split_path(key);
        self.open.iter().find(|o| {
            let (od, on) = split_path(&o.path);
            on.eq_ignore_ascii_case(name)
                && (dir.is_empty() || od.is_empty() || o.path.eq_ignore_ascii_case(key))
        })
    }

    fn link_for(&mut self, key: &str) -> usize {
        let (dir, name) = split_path(key);
        let found = self.links.iter().position(|l| {
            !l.broken
                && if dir.is_empty() || l.dir().is_empty() {
                    l.name().eq_ignore_ascii_case(name)
                } else {
                    l.path.eq_ignore_ascii_case(key)
                }
        });
        if let Some(i) = found {
            if self.links[i].dir().is_empty() && !dir.is_empty() {
                self.links[i].path = key.to_string();
            }
            return i;
        }
        self.links.push(ExtLink {
            path: key.to_string(),
            ..Default::default()
        });
        self.links.len() - 1
    }

    fn sheet_for(&mut self, link: usize, sheet: &str) -> usize {
        match self
            .sheets
            .iter()
            .position(|(l, n)| *l == link && crate::text::eq_sheet_name(n, sheet))
        {
            Some(i) => i,
            None => {
                self.sheets.push((link, sheet.to_string()));
                self.sheets.len() - 1
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ExtSheetData {
    pub name: String,
    pub cells: Vec<(u32, u32, Value)>,
}

pub(crate) fn new_ghost(id: SheetId, name: &str) -> Sheet {
    Sheet {
        id,
        name: name.to_string(),
        state: SheetState::Hidden,
        kind: SheetKind::Worksheet,
        cells: CellStore::default(),
        spills: SpillIndex::default(),
        dirty: DirtyIndex::default(),
    }
}

pub(crate) fn plain_part(s: &str) -> bool {
    s.chars()
        .all(|c| c.is_alphanumeric() || matches!(c, '_' | '.' | '[' | ']'))
}

impl Workbook {
    fn ext_lock(&self) -> std::sync::MutexGuard<'_, ExtState> {
        self.ext.lock().unwrap_or_else(|e| e.into_inner())
    }

    pub(crate) fn ext_resolve(&self, book: &str, sheet: &str) -> Result<SheetId, String> {
        let mut st = self.ext_lock();
        let link = st.link_for(book);
        let mut absent = false;
        if let Some(ob) = st.open_book(book).cloned() {
            st.links[link].open = true;
            if st.links[link].dir().is_empty() {
                st.links[link].path = ob.path.clone();
            }
            absent = !sheet.is_empty()
                && !ob
                    .sheets
                    .iter()
                    .any(|s| crate::text::eq_sheet_name(s, sheet));
        }
        if absent {
            return Err(format!("no sheet {sheet} in {book}"));
        }
        let idx = st.sheet_for(link, sheet);
        Ok(SheetId(EXT_BASE + idx as u32))
    }

    pub fn ext_set_open_books(&self, books: Vec<OpenBook>) {
        self.ext_lock().open = books;
    }

    pub fn set_session_books(&self, docs: Vec<String>, windows: Vec<String>) {
        let mut st = self.ext_lock();
        st.docs = docs;
        st.windows = windows;
    }

    pub fn session_books(&self) -> (Vec<String>, Vec<String>) {
        let st = self.ext_lock();
        if st.docs.is_empty() {
            let own = vec![self.book_name().to_string()];
            return (own.clone(), own);
        }
        (st.docs.clone(), st.windows.clone())
    }

    pub(crate) fn ext_touch_book(&self, book: &str) -> bool {
        let mut st = self.ext_lock();
        let known = st.open_book(book).cloned();
        let link = st
            .links
            .iter()
            .position(|l| !l.broken && l.name().eq_ignore_ascii_case(split_path(book).1));
        match (known, link) {
            (Some(ob), link) => {
                let li = link.unwrap_or_else(|| st.link_for(book));
                st.links[li].open = true;
                if st.links[li].dir().is_empty() {
                    st.links[li].path = ob.path;
                }
                true
            }
            (None, Some(_)) => true,
            (None, None) => false,
        }
    }

    pub fn ext_sheet_info(&self, id: SheetId) -> Option<(ExtLink, String)> {
        let st = self.ext_lock();
        let (l, n) = st.sheets.get(id.0.checked_sub(EXT_BASE)? as usize)?;
        Some((st.links[*l].clone(), n.clone()))
    }

    pub(crate) fn ext_prefix(&self, id: SheetId) -> Option<String> {
        let (l, sheet) = self.ext_sheet_info(id)?;
        let book = l.name();
        if l.open || l.dir().is_empty() {
            let body = format!("[{book}]{sheet}");
            Some(if plain_part(&body) {
                format!("{body}!")
            } else {
                format!("'{}'!", body.replace('\'', "''"))
            })
        } else {
            let body = format!("{}[{book}]{sheet}", l.dir());
            Some(format!("'{}'!", body.replace('\'', "''")))
        }
    }

    pub fn ext_links(&self) -> Vec<ExtLink> {
        self.ext_lock().links.clone()
    }

    pub fn ext_link(&mut self, key: &str) -> usize {
        self.ext_lock().link_for(key)
    }

    pub fn ext_used_links(&self) -> Vec<usize> {
        let mut used = std::collections::BTreeSet::new();
        for f in self.formulas.iter().flatten() {
            formula::walk(&f.formula.root, &mut |n| match n {
                Node::Ref(s, _) | Node::DeadRef(s) => self.ext_note(s, &mut used),
                Node::Name { sheet: Some(s), .. } => self.ext_note(s, &mut used),
                _ => {}
            });
        }
        let st = self.ext_lock();
        used.into_iter().filter(|&l| !st.links[l].broken).collect()
    }

    fn ext_note(&self, r: &SheetRef, used: &mut std::collections::BTreeSet<usize>) {
        if let SheetRef::Sheet(id) = r {
            if let Some(l) = self.ext_link_of(*id) {
                used.insert(l);
            }
        }
    }

    fn ext_ref_sheet(&self, r: &SheetRef) -> bool {
        let mut used = std::collections::BTreeSet::new();
        self.ext_note(r, &mut used);
        !used.is_empty()
    }

    pub fn ext_set_link(&mut self, link: usize, path: &str, open: bool) -> bool {
        let mut st = self.ext_lock();
        let Some(l) = st.links.get_mut(link) else {
            return false;
        };
        let changed = l.path != path || l.open != open;
        l.path = path.to_string();
        l.open = open;
        changed
    }

    pub fn ext_data(&self, link: usize) -> Vec<ExtSheetData> {
        let st = self.ext_lock();
        let mut out = Vec::new();
        for (i, (l, name)) in st.sheets.iter().enumerate() {
            if *l != link || name.is_empty() || name == CONST_SHEET {
                continue;
            }
            let id = SheetId(EXT_BASE + i as u32);
            let Some(sh) = self.ext_sheets.iter().find(|s| s.id == id) else {
                continue;
            };
            out.push(ExtSheetData {
                name: sh.name.clone(),
                cells: sheet_cells(sh),
            });
        }
        out
    }

    pub fn ext_data_read(&self, link: usize) -> Vec<ExtSheetData> {
        let names = self.ext_names(link);
        let st = self.ext_lock();
        let mut out = Vec::new();
        for (i, (l, name)) in st.sheets.iter().enumerate() {
            if *l != link || name.is_empty() || name == CONST_SHEET {
                continue;
            }
            let id = SheetId(EXT_BASE + i as u32);
            let Some(sh) = self.ext_sheets.iter().find(|s| s.id == id) else {
                continue;
            };
            let mut rects = self.graph.read_rects(id);
            rects.extend(
                names
                    .iter()
                    .filter(|n| n.sheet.eq_ignore_ascii_case(&sh.name))
                    .map(|n| n.rect),
            );
            let cells = sheet_cells(sh)
                .into_iter()
                .filter(|(r, c, _)| {
                    rects
                        .iter()
                        .any(|x| *r >= x.r0 && *r <= x.r1 && *c >= x.c0 && *c <= x.c1)
                })
                .collect();
            out.push(ExtSheetData {
                name: sh.name.clone(),
                cells,
            });
        }
        out
    }

    pub fn ext_set_data(&mut self, link: usize, data: Vec<ExtSheetData>) -> RecalcReport {
        let mut changed: Vec<SheetId> = Vec::new();
        let ids: Vec<SheetId> = {
            let mut st = self.ext_lock();
            let ids: Vec<usize> = data.iter().map(|d| st.sheet_for(link, &d.name)).collect();

            for (i, (l, n)) in st.sheets.iter().enumerate() {
                if *l == link && n != CONST_SHEET && !ids.contains(&i) {
                    changed.push(SheetId(EXT_BASE + i as u32));
                }
            }
            ids.into_iter()
                .map(|i| SheetId(EXT_BASE + i as u32))
                .collect()
        };
        {
            let mut st = self.ext_lock();
            st.tabs.retain(|(l, _)| *l != link);
            st.tabs
                .push((link, data.iter().map(|d| d.name.clone()).collect()));
        }
        let had = self.ext_sheets.len();
        let gone = changed.clone();
        self.ext_sheets.retain(|s| !gone.contains(&s.id));
        changed.retain(|g| gone.contains(g) && had > 0);
        for (id, d) in ids.iter().zip(&data) {
            let mut new = new_ghost(*id, &d.name);
            for (r, c, v) in &d.cells {
                if !matches!(v, Value::Blank) {
                    new.cells.set(*r, *c, Entry::Const(v.clone()));
                }
            }
            let same = self
                .ext_sheets
                .iter()
                .find(|s| s.id == *id)
                .is_some_and(|old| {
                    old.name == new.name
                        && format!("{:?}", sheet_cells(old)) == format!("{:?}", sheet_cells(&new))
                });
            if same {
                continue;
            }
            self.ext_sheets.retain(|s| s.id != *id);
            self.ext_sheets.push(new);
            changed.push(*id);
        }
        if changed.is_empty() {
            return RecalcReport::default();
        }
        self.ext_dirty(changed)
    }

    pub fn ext_reopened(&mut self) -> RecalcReport {
        self.ext_dirty_with(Vec::new(), true)
    }

    fn ext_dirty(&mut self, changed: Vec<SheetId>) -> RecalcReport {
        self.ext_dirty_with(changed, false)
    }

    fn ext_dirty_with(&mut self, changed: Vec<SheetId>, all_refs: bool) -> RecalcReport {
        let auto = self.settings.mode == super::CalcMode::Automatic;
        let mark = self.notices().len();
        let report = self.command(Command::Calculate, |wb| {
            let all = Rect::new(0, 0, MAX_ROWS - 1, MAX_COLS - 1);
            for id in changed {
                calc::rect_changed(wb, id, all);
            }

            let ids: Vec<_> = wb.formula_ids().collect();
            for fid in ids {
                let mut named = false;
                formula::walk(&wb.formula(fid).formula.root, &mut |n| match n {
                    Node::Name { sheet: Some(s), .. } => named |= wb.ext_name_link(s).is_some(),
                    Node::Ref(s, _) if all_refs => named |= wb.ext_ref_sheet(s),
                    _ => {}
                });
                if named {
                    calc::mark_dirty(wb, fid);
                }
            }
            if auto {
                wb.recalc_run()
            } else {
                RecalcReport::default()
            }
        });

        if !auto {
            self.notices_mut().truncate(mark);
        }
        report
    }

    pub fn ext_break(&mut self, link: usize) -> RecalcReport {
        let mut hit: Vec<CellAddr> = Vec::new();
        for f in self.formulas.iter().flatten() {
            let mut used = std::collections::BTreeSet::new();
            formula::walk(&f.formula.root, &mut |n| match n {
                Node::Ref(s, _) | Node::DeadRef(s) => self.ext_note(s, &mut used),
                Node::Name { sheet: Some(s), .. } => self.ext_note(s, &mut used),
                _ => {}
            });
            if used.contains(&link) {
                hit.push(f.addr);
            }
        }
        if let Some(l) = self.ext_lock().links.get_mut(link) {
            l.broken = true;
        }
        let mut report = RecalcReport::default();
        for addr in hit {
            let v = match self.value(addr) {
                Value::Array(a) => a.data[0].clone(),
                v => v,
            };
            let r = self.set_value(addr, v);
            report.changed.extend(r.changed);
        }
        report
    }

    pub(crate) fn ext_span(&self, a: SheetId, b: SheetId) -> Vec<SheetId> {
        let st = self.ext_lock();
        let at = |id: SheetId| st.sheets.get(id.0.checked_sub(EXT_BASE)? as usize);
        let (Some((la, na)), Some((lb, nb))) = (at(a), at(b)) else {
            return vec![];
        };
        let Some((_, tabs)) = st.tabs.iter().find(|(l, _)| l == la) else {
            return vec![];
        };
        let pos = |n: &String| tabs.iter().position(|t| crate::text::eq_sheet_name(t, n));
        let (Some(p), Some(q)) = (pos(na), pos(nb)) else {
            return vec![];
        };
        if la != lb {
            return vec![];
        }
        (p.min(q)..=p.max(q))
            .filter_map(|i| {
                st.sheets
                    .iter()
                    .position(|(l, n)| l == la && crate::text::eq_sheet_name(n, &tabs[i]))
            })
            .map(|i| SheetId(EXT_BASE + i as u32))
            .collect()
    }

    pub(crate) fn ext_span_prefix(&self, a: SheetId, b: SheetId) -> Option<String> {
        let ids = self.ext_span(a, b);
        let (first, last) = (*ids.first()?, *ids.last()?);
        let p = self.ext_prefix(first)?;
        let (_, last_name) = self.ext_sheet_info(last)?;
        let (head, quoted) = match p.strip_suffix("'!") {
            Some(h) => (h.to_string(), true),
            None => (p.strip_suffix('!')?.to_string(), false),
        };
        Some(if quoted {
            format!("{head}:{}'!", last_name.replace('\x27', "''"))
        } else {
            format!("{head}:{last_name}!")
        })
    }

    pub(crate) fn ext_link_of(&self, id: SheetId) -> Option<usize> {
        let st = self.ext_lock();
        st.sheets
            .get(id.0.checked_sub(EXT_BASE)? as usize)
            .map(|(l, _)| *l)
    }
}

fn sheet_cells(s: &Sheet) -> Vec<(u32, u32, Value)> {
    let mut v = Vec::new();
    if let Some(b) = s.cells.bounds() {
        s.cells.for_each_in(b, |r, c, e| {
            if let Entry::Const(x) = e {
                v.push((r, c, x.clone()));
            }
        });
    }
    v
}

pub(crate) type ExtCell = Mutex<ExtState>;

impl Workbook {
    pub(crate) fn ext_const_closed(&self, id: SheetId) -> bool {
        self.ext_sheet_info(id)
            .is_some_and(|(l, n)| !l.open && n == CONST_SHEET)
    }

    pub(crate) fn ext_closed(&self, id: SheetId) -> bool {
        self.ext_sheet_info(id).is_some_and(|(l, _)| !l.open)
    }
}

impl Workbook {
    pub fn ext_file_links(&self) -> Vec<(usize, ExtLink)> {
        self.ext_links()
            .into_iter()
            .enumerate()
            .filter(|(_, l)| !l.broken)
            .collect()
    }

    pub(crate) fn ext_file_indexes(&self, text: &str) -> String {
        let mut out = text.to_string();
        for (n, (_, l)) in self.ext_file_links().iter().enumerate() {
            let idx = format!("[{}]", n + 1);
            if !l.dir().is_empty() {
                let dir = l.dir().replace('\'', "''");
                out = out.replace(&format!("{dir}[{}]", l.name()), &idx);
                out = out.replace(&format!("[{}]", l.path), &idx);
            }
            out = out.replace(&format!("[{}]", l.name()), &idx);
        }

        for (n, (_, l)) in self.ext_file_links().iter().enumerate() {
            let idx = format!("[{}]!", n + 1);
            let quoted = format!("'{}'!", l.path.replace('\x27', "''"));
            out = out.replace(&quoted, &idx);
            let plain = format!("{}!", l.name());
            let mut res = String::with_capacity(out.len());
            let mut rest = out.as_str();
            while let Some(i) = rest.find(&plain) {
                let prev = rest[..i].chars().next_back();
                let ok = !prev
                    .is_some_and(|p| p.is_alphanumeric() || matches!(p, '_' | '.' | '\x27' | ']'));
                res.push_str(&rest[..i]);
                res.push_str(if ok { &idx } else { &plain });
                rest = &rest[i + plain.len()..];
            }
            res.push_str(rest);
            out = res;
        }

        let mut res = String::with_capacity(out.len());
        let mut rest = out.as_str();
        while let Some(i) = rest.find("'[") {
            res.push_str(&rest[..i]);
            let tail = &rest[i + 1..];
            match tail.find("'!") {
                Some(j) if plain_part(&tail[..j]) && tail[..j].matches('[').count() == 1 => {
                    res.push_str(&tail[..j]);
                    res.push('!');
                    rest = &tail[j + 2..];
                }
                _ => {
                    res.push('\'');
                    rest = tail;
                }
            }
        }
        res.push_str(rest);
        res
    }
}

impl Workbook {
    pub fn sheet_names(&self) -> Vec<String> {
        self.sheets
            .iter()
            .filter(|s| s.kind == SheetKind::Worksheet)
            .map(|s| s.name.clone())
            .collect()
    }

    pub fn has_ext_links(&self) -> bool {
        !self.ext_lock().links.is_empty()
    }

    pub fn values_snapshot(&self) -> Vec<ExtSheetData> {
        self.sheets
            .iter()
            .filter(|s| s.kind == SheetKind::Worksheet)
            .map(|s| {
                let mut cells = Vec::new();
                if let Some(b) = s.cells.bounds() {
                    for r in b.r0..=b.r1 {
                        for c in b.c0..=b.c1 {
                            match self.value(CellAddr::new(s.id, r, c)) {
                                Value::Blank => {}
                                v => cells.push((r, c, v)),
                            }
                        }
                    }
                }
                ExtSheetData {
                    name: s.name.clone(),
                    cells,
                }
            })
            .collect()
    }
}

impl Workbook {
    pub(crate) fn ext_find_open(&self, text: &str) -> Option<SheetId> {
        let (dir, rest) = text.split_once('[')?;
        let (book, sheet) = rest.split_once(']')?;
        let key = format!("{dir}{book}");
        let (kdir, kname) = split_path(&key);
        let st = self.ext_lock();
        let link = st.links.iter().position(|l| {
            l.open
                && !l.broken
                && if kdir.is_empty() || l.dir().is_empty() {
                    l.name().eq_ignore_ascii_case(kname)
                } else {
                    l.path.eq_ignore_ascii_case(&key)
                }
        })?;
        let idx = st
            .sheets
            .iter()
            .position(|(l, n)| *l == link && crate::text::eq_sheet_name(n, sheet))?;
        let id = SheetId(EXT_BASE + idx as u32);
        self.ext_sheets.iter().any(|s| s.id == id).then_some(id)
    }
}

#[derive(Clone, Debug)]
pub struct ExtName {
    pub name: String,
    pub sheet: String,
    pub rect: Rect,

    pub value: Option<Value>,
}

impl PartialEq for ExtName {
    fn eq(&self, o: &ExtName) -> bool {
        self.name == o.name
            && self.sheet == o.sheet
            && self.rect == o.rect
            && format!("{:?}", self.value) == format!("{:?}", o.value)
    }
}

const CONST_SHEET: &str = "\u{1}";

impl ExtName {
    pub fn parse(name: &str, refers_to: &str) -> Option<ExtName> {
        let body = refers_to.strip_prefix('=').unwrap_or(refers_to);
        let none = || ExtName {
            name: name.to_string(),
            sheet: String::new(),
            rect: Rect::new(0, 0, 0, 0),
            value: None,
        };
        let constant = if let Some(t) = body.strip_prefix('"').and_then(|t| t.strip_suffix('"')) {
            Some(Value::from(t.replace("\"\"", "\"").as_str()))
        } else if body.eq_ignore_ascii_case("TRUE") {
            Some(Value::Bool(true))
        } else if body.eq_ignore_ascii_case("FALSE") {
            Some(Value::Bool(false))
        } else {
            body.parse::<f64>().ok().map(Value::Number)
        };
        if constant.is_some() {
            return Some(ExtName {
                value: constant,
                ..none()
            });
        }
        let Some((sheet, cells)) = body.rsplit_once('!') else {
            return Some(none());
        };
        let sheet = match sheet.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')) {
            Some(q) => q.replace("''", "'"),
            None => sheet.to_string(),
        };
        let Some(rect) = crate::address::parse_a1_rect(&cells.replace('$', "")) else {
            return Some(none());
        };
        Some(ExtName {
            name: name.to_string(),
            sheet,
            rect,
            value: None,
        })
    }

    pub fn refers_to(&self) -> String {
        if self.sheet.is_empty() {
            return "#REF!".into();
        }
        let col = crate::address::col_to_letters;
        let r = self.rect;
        let cells = if r.is_cell() {
            format!("${}${}", col(r.c0), r.r0 + 1)
        } else {
            format!("${}${}:${}${}", col(r.c0), r.r0 + 1, col(r.c1), r.r1 + 1)
        };
        let sheet = if plain_part(&self.sheet) {
            self.sheet.clone()
        } else {
            format!("'{}'", self.sheet.replace('\'', "''"))
        };
        format!("={sheet}!{cells}")
    }
}

impl Workbook {
    pub fn names_snapshot(&self) -> Vec<ExtName> {
        self.names
            .iter()
            .filter(|d| d.scope.is_none())
            .filter_map(|d| ExtName::parse(&d.name, &d.refers_to))
            .collect()
    }

    pub fn ext_names(&self, link: usize) -> Vec<ExtName> {
        self.ext_lock()
            .names
            .iter()
            .filter(|(l, _)| *l == link)
            .map(|(_, n)| n.clone())
            .collect()
    }

    pub fn ext_set_names(&mut self, link: usize, mut names: Vec<ExtName>) -> bool {
        let mut consts = Vec::new();
        for (row, n) in names.iter_mut().filter(|n| n.value.is_some()).enumerate() {
            n.rect = Rect::new(row as u32, 0, row as u32, 0);
            consts.push((row as u32, 0, n.value.clone().unwrap_or(Value::Blank)));
        }
        let id = {
            let mut st = self.ext_lock();
            let old: Vec<&ExtName> = st
                .names
                .iter()
                .filter(|(l, _)| *l == link)
                .map(|(_, n)| n)
                .collect();
            if old.len() == names.len() && old.iter().zip(&names).all(|(a, b)| *a == b) {
                return false;
            }
            st.names.retain(|(l, _)| *l != link);
            st.names.extend(names.into_iter().map(|n| (link, n)));
            SheetId(EXT_BASE + st.sheet_for(link, CONST_SHEET) as u32)
        };
        let mut ghost = new_ghost(id, CONST_SHEET);
        for (r, c, v) in consts {
            ghost.cells.set(r, c, Entry::Const(v));
        }
        self.ext_sheets.retain(|s| s.id != id);
        self.ext_sheets.push(ghost);
        if self.settings.mode == super::CalcMode::Automatic {
            self.full_recalc();
        }
        true
    }

    fn ext_find_link(&self, key: &str) -> Option<usize> {
        let (dir, name) = split_path(key);
        self.ext_lock().links.iter().position(|l| {
            !l.broken
                && if dir.is_empty() || l.dir().is_empty() {
                    l.name().eq_ignore_ascii_case(name)
                } else {
                    l.path.eq_ignore_ascii_case(key)
                }
        })
    }

    fn ext_name_link(&self, sh: &SheetRef) -> Option<usize> {
        match sh {
            SheetRef::Sheet(id) if id.0 >= EXT_BASE => self.ext_link_of(*id),
            SheetRef::Missing(n) => self.ext_find_link(n),
            _ => None,
        }
    }

    pub(crate) fn ext_name_lookup(
        &self,
        sh: &SheetRef,
        key: &str,
    ) -> Option<Result<(SheetId, Rect), ()>> {
        let link = self.ext_name_link(sh)?;
        let mut st = self.ext_lock();
        let found = st
            .names
            .iter()
            .find(|(l, n)| *l == link && n.name.eq_ignore_ascii_case(key))
            .map(|(_, n)| (n.sheet.clone(), n.rect, n.value.is_some()));
        Some(match found {
            Some((sheet, rect, true)) if sheet.is_empty() => {
                let idx = st.sheet_for(link, CONST_SHEET);
                Ok((SheetId(EXT_BASE + idx as u32), rect))
            }
            Some((sheet, _, _)) if sheet.is_empty() => Err(()),
            Some((sheet, rect, _)) => {
                let idx = st.sheet_for(link, &sheet);
                Ok((SheetId(EXT_BASE + idx as u32), rect))
            }
            None => Err(()),
        })
    }

    pub(crate) fn ext_name_prefix(&self, sh: &SheetRef) -> Option<String> {
        let link = self.ext_name_link(sh)?;
        let l = self.ext_lock().links.get(link)?.clone();
        Some(if l.open || l.dir().is_empty() {
            if plain_part(l.name()) {
                format!("{}!", l.name())
            } else {
                format!("'{}'!", l.name().replace('\'', "''"))
            }
        } else {
            format!("'{}'!", l.path.replace('\'', "''"))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn data(cells: &[(u32, u32, f64)]) -> Vec<ExtSheetData> {
        vec![ExtSheetData {
            name: "Data".into(),
            cells: cells
                .iter()
                .map(|&(r, c, v)| (r, c, Value::Number(v)))
                .collect(),
        }]
    }

    #[test]
    fn open_and_closed_links() {
        let mut wb = Workbook::new();
        let s = wb.active_sheet();
        let a = |r| CellAddr::new(s, r, 0);
        let l = wb.ext_link("src.xlsx");
        wb.ext_set_link(l, r"C:\d\src.xlsx", true);
        wb.ext_set_data(l, data(&[(0, 0, 5.0), (1, 0, 7.0)]));
        wb.set_formula(a(0), "=[src.xlsx]Data!A1+1").unwrap();
        wb.set_formula(a(1), "=SUMIF([src.xlsx]Data!A1:A2,\">5\")")
            .unwrap();
        assert_eq!(
            format!("{:?}", wb.value(a(0))),
            format!("{:?}", Value::Number(6.0))
        );
        assert_eq!(
            format!("{:?}", wb.value(a(1))),
            format!("{:?}", Value::Number(7.0))
        );
        assert_eq!(wb.formula_text(a(0)).unwrap(), "=[src.xlsx]Data!A1+1");
        wb.ext_set_data(l, data(&[(0, 0, 9.0)]));
        assert_eq!(
            format!("{:?}", wb.value(a(0))),
            format!("{:?}", Value::Number(10.0))
        );
        wb.ext_set_link(l, r"C:\d\src.xlsx", false);
        assert_eq!(
            wb.formula_text(a(0)).unwrap(),
            r"='C:\d\[src.xlsx]Data'!A1+1"
        );
        wb.set_formula(a(2), r"='C:\d\[src.xlsx]Data'!A1").unwrap();
        assert_eq!(
            format!("{:?}", wb.value(a(2))),
            format!("{:?}", Value::Number(9.0))
        );
        wb.set_formula(a(3), "=[nosuch.xlsx]Data!A1").unwrap();
        assert_eq!(
            format!("{:?}", wb.value(a(3))),
            format!("{:?}", Value::Error(crate::value::XlError::REF))
        );
        wb.ext_break(l);
        assert_eq!(wb.formula_text(a(0)), None);
    }
}
