use crate::app::{Application, BookId, BookWb};
use crate::conv::Args;
use crate::error::OmError;
use xlcalc::{ExtLink, ExtName, ExtSheetData, SheetId};
use xlcalc_variant::{SafeArray, VarType, Variant};

fn same_path(a: &str, b: &str) -> bool {
    a.len() == b.len()
        && a.chars().zip(b.chars()).all(|(x, y)| {
            x.eq_ignore_ascii_case(&y) || (matches!(x, '/' | '\\') && matches!(y, '/' | '\\'))
        })
}

fn unable(method: &str) -> OmError {
    OmError::app_defined_with(&format!(
        "Unable to get the {method} property of the Workbook class"
    ))
}

impl Application<'_> {
    pub(crate) fn link_source(&self, l: &ExtLink, except: usize) -> Option<usize> {
        self.books.iter().enumerate().position(|(i, b)| {
            i != except
                && if l.dir().is_empty() {
                    b.name.eq_ignore_ascii_case(l.name())
                } else {
                    same_path(&b.path, &l.path)
                }
        })
    }

    pub(crate) fn follow_source_shift(
        &mut self,
        book: BookId,
        sheet: SheetId,
        rows: bool,
        at: u32,
        count: u32,
        insert: bool,
    ) {
        let bi = self.book_index(book);
        let Some(name) = self.books[bi].wb.sheet(sheet).map(|s| s.name.to_string()) else {
            return;
        };
        self.for_dependents(bi, &mut |w, li| {
            w.ext_source_shift(li, &name, rows, at, count, insert)
                .is_some()
        });
    }

    pub(crate) fn follow_source_rename(&mut self, book: BookId, name: bool, old: &str, new: &str) {
        let bi = self.book_index(book);
        self.for_dependents(bi, &mut |w, li| {
            if name {
                w.ext_source_renamed_name(li, old, new)
            } else {
                w.ext_source_renamed_sheet(li, old, new)
            }
        });
    }

    pub(crate) fn follow_source_sheet_deleted(&mut self, book: BookId, name: &str) {
        let bi = self.book_index(book);
        self.for_dependents(bi, &mut |w, li| w.ext_source_deleted_sheet(li, name));
    }

    fn for_dependents(
        &mut self,
        bi: usize,
        f: &mut dyn FnMut(&mut xlcalc::Workbook, usize) -> bool,
    ) {
        for i in 0..self.books.len() {
            if i == bi {
                continue;
            }
            let links = match &self.books[i].wb {
                BookWb::Own(w) => w.ext_links(),
                _ => continue,
            };
            for (li, l) in links.iter().enumerate() {
                if l.broken || self.link_source(l, i) != Some(bi) {
                    continue;
                }
                if let BookWb::Own(w) = &mut self.books[i].wb {
                    if f(w, li) {
                        self.books[i].saved = false;
                    }
                }
            }
        }
    }

    pub(crate) fn prelink_text(&mut self, b: BookId, text: &str) {
        if !text.starts_with('=') {
            return;
        }
        let bi = self.book_index(b);
        let mut any = false;
        let mut rest = text;
        while let Some(i) = rest.find('[') {
            let Some(j) = rest[i..].find(']') else {
                break;
            };
            let name = &rest[i + 1..i + j];

            let dir = rest[..i]
                .rfind('\'')
                .map(|q| &rest[q + 1..i])
                .filter(|d| d.ends_with(['\\', '/']))
                .unwrap_or("")
                .to_string();
            rest = &rest[i + j + 1..];
            let open = self
                .books
                .iter()
                .enumerate()
                .any(|(k, x)| k != bi && x.name.eq_ignore_ascii_case(name));
            if !name.is_empty() && !open && !dir.is_empty() {
                let key = format!("{dir}{name}");
                if self.fs.exists(&key) {
                    if let BookWb::Own(w) = &mut self.books[bi].wb {
                        let li = w.ext_link(&key);
                        let marks = w.notices().len();
                        if self.read_link_file(b, li, true) {
                            if let BookWb::Own(w) = &mut self.books[bi].wb {
                                w.notices_mut().truncate(marks);
                            }
                        }
                    }
                }
                continue;
            }
            if name.is_empty() || !open {
                continue;
            }
            if let BookWb::Own(w) = &mut self.books[bi].wb {
                w.ext_link(name);
                any = true;
            }
        }

        for key in quoted_book_paths(text) {
            let file = crate::files::file_name(&key);
            let open = self
                .books
                .iter()
                .enumerate()
                .any(|(k, x)| k != bi && x.name.eq_ignore_ascii_case(&file));
            if open || !self.fs.exists(&key) {
                continue;
            }
            if let BookWb::Own(w) = &mut self.books[bi].wb {
                let li = w.ext_link(&key);
                let marks = w.notices().len();
                if self.read_link_file(b, li, true) {
                    if let BookWb::Own(w) = &mut self.books[bi].wb {
                        w.notices_mut().truncate(marks);
                    }
                }
            }
        }
        if any {
            let marks: Vec<usize> = self
                .books
                .iter()
                .map(|x| match &x.wb {
                    BookWb::Own(w) => w.notices().len(),
                    _ => 0,
                })
                .collect();
            self.sync_links();
            for (x, m) in self.books.iter_mut().zip(marks) {
                if let BookWb::Own(w) = &mut x.wb {
                    w.notices_mut().truncate(m);
                }
            }
        }
    }

    pub(crate) fn book_links(&self, i: usize) -> Option<Vec<usize>> {
        let BookWb::Own(w) = &self.books.get(i)?.wb else {
            return None;
        };
        if !w.has_ext_links() {
            return None;
        }
        Some(
            w.ext_links()
                .iter()
                .filter_map(|l| self.link_source(l, i))
                .collect(),
        )
    }

    pub(crate) fn book_index(&self, b: BookId) -> usize {
        self.books.iter().position(|x| x.id == b).unwrap_or(0)
    }

    pub(crate) fn sync_links(&mut self) -> bool {
        let mut changed = false;
        self.refresh_chains();
        let open: Vec<(usize, xlcalc::OpenBook)> = self
            .books
            .iter()
            .enumerate()
            .map(|(i, b)| {
                let sheets = b.wb.sheet_names();
                let path = if b.path.is_empty() {
                    b.name.clone()
                } else {
                    b.path.clone()
                };
                (i, xlcalc::OpenBook { path, sheets })
            })
            .collect();
        let docs: Vec<String> = self
            .books
            .iter()
            .filter(|b| !b.is_addin)
            .map(|b| b.name.clone())
            .collect();

        let windows: Vec<String> = self
            .window_list()
            .into_iter()
            .filter_map(|w| self.window_caption(w).ok())
            .collect();
        for (i, b) in self.books.iter().enumerate() {
            if let BookWb::Own(w) = &b.wb {
                w.set_session_books(docs.clone(), windows.clone());
            }
            let others = open
                .iter()
                .filter(|(j, _)| *j != i)
                .map(|(_, o)| o.clone())
                .collect();
            if let BookWb::Own(w) = &b.wb {
                w.ext_set_open_books(others);
            }
        }
        if !self.books.iter().any(|b| b.wb.has_ext_links()) {
            return false;
        }
        let mut snaps: Vec<Option<(Vec<ExtSheetData>, Vec<ExtName>)>> =
            vec![None; self.books.len()];
        for bi in 0..self.books.len() {
            if !self.books[bi].wb.has_ext_links() {
                continue;
            }
            let used = self.books[bi].wb.ext_used_links();
            for (li, l) in self.books[bi].wb.ext_links().into_iter().enumerate() {
                if l.broken {
                    continue;
                }
                let update = match self.link_source(&l, bi) {
                    Some(si) => {
                        let path = if self.books[si].path.is_empty() {
                            self.books[si].name.clone()
                        } else {
                            self.books[si].path.clone()
                        };
                        let data = snaps[si]
                            .get_or_insert_with(|| {
                                let wb = &self.books[si].wb;
                                (wb.values_snapshot(), wb.names_snapshot())
                            })
                            .clone();
                        Some((path, true, Some(data)))
                    }
                    None if l.open => Some((l.path.clone(), false, None)),
                    None => None,
                };
                let Some((path, open, data)) = update else {
                    continue;
                };
                if let BookWb::Own(w) = &mut self.books[bi].wb {
                    let was_open = w.ext_links().get(li).is_some_and(|l| l.open);
                    let mut touched = w.ext_set_link(li, &path, open);

                    if was_open && !open {
                        w.ext_set_fresh(li, false);
                    }
                    if let Some(d) = data {
                        touched |= !w.ext_set_data(li, d.0).changed.is_empty();
                        touched |= w.ext_set_names(li, d.1);
                    }

                    if open && !was_open {
                        w.ext_reopened();
                    }
                    changed |= touched;

                    if touched && open && used.contains(&li) {
                        self.books[bi].saved = false;
                    }
                }
            }
        }
        changed
    }

    pub(crate) fn resolve_file_links(
        &mut self,
        b: BookId,
        dir: &str,
        links: &[xlcalc_xlsx::FileLink],
    ) {
        for fl in links {
            let joined = if dir.is_empty() || fl.rooted {
                fl.target.clone()
            } else {
                format!("{dir}{}", fl.target)
            };
            let exists = self.fs.exists(&joined);
            let path = if fl.missing {
                fl.target.clone()
            } else if exists || fl.absolute.is_none() {
                joined
            } else {
                fl.absolute.clone().unwrap_or_default()
            };
            if let Ok(bk) = self.book_mut(b) {
                let li = bk.wb.ext_link(&fl.key());
                bk.wb.ext_set_link(li, &path, false);
            }
        }
    }

    pub(crate) fn update_link_from_file(&mut self, b: BookId, li: usize) -> bool {
        self.read_link_file(b, li, false)
    }

    fn read_link_file(&mut self, b: BookId, li: usize, quiet: bool) -> bool {
        let Ok(bk) = self.book(b) else {
            return false;
        };
        let Some(l) = bk.wb.ext_links().get(li).cloned() else {
            return false;
        };
        if l.dir().is_empty() || !self.fs.exists(&l.path) {
            return false;
        }
        let Ok(bytes) = self.fs.read(&l.path) else {
            return false;
        };
        let Ok(imp) = xlcalc_xlsx::import_bytes(bytes) else {
            return false;
        };
        let data = imp.workbook.values_snapshot();

        let names = imp.workbook.names_snapshot();
        if let Ok(bk) = self.book_mut(b) {
            if quiet {
                bk.wb.ext_set_data_quiet(li, data);
                bk.wb.ext_set_names_quiet(li, names);
            } else {
                bk.wb.ext_set_data(li, data);
                bk.wb.ext_set_names(li, names);
            }
            bk.wb.ext_set_fresh(li, true);
            bk.saved = false;
        }
        true
    }

    pub(crate) fn update_links_on_open(&mut self, b: BookId, update: Option<i32>) {
        let update = match update {
            Some(n) => n != 0,
            None => !self.ask_to_update_links,
        };
        let Ok(bk) = self.book(b) else {
            return;
        };
        let links = bk.wb.ext_links();
        let used = bk.wb.ext_used_links();
        let bi = self.book_index(b);
        let mut linked = false;
        for (li, l) in links.iter().enumerate() {
            let open = self.link_source(l, bi).is_some();
            linked |= !l.broken && used.contains(&li);
            if update && !l.broken && !open {
                self.update_link_from_file(b, li);
            }
        }
        self.sync_links();

        if linked {
            for (i, other) in self.books.iter_mut().enumerate() {
                if i == bi {
                    continue;
                }
                if let BookWb::Own(w) = &mut other.wb {
                    if w.settings().mode == xlcalc::CalcMode::Automatic {
                        w.recalc();
                    }
                }
            }
        }
    }

    fn named_link(&self, b: BookId, name: &str) -> Option<usize> {
        let bk = self.book(b).ok()?;
        let links = bk.wb.ext_links();
        bk.wb.ext_used_links().into_iter().find(|&i| {
            let l = &links[i];
            same_path(&l.path, name)
                || (!name.contains(['\\', '/']) && l.name().eq_ignore_ascii_case(name))
        })
    }

    pub(crate) fn link_member(
        &mut self,
        b: BookId,
        name: &str,
        a: &Args,
    ) -> Result<Variant, OmError> {
        match name {
            "LinkSources" => {
                let excel_links = a.i32(0)?.is_none_or(|t| t == 1);

                let wb = &self.book(b)?.wb;
                let links = wb.ext_links();
                let paths: Vec<Variant> = wb
                    .ext_link_order()
                    .into_iter()
                    .filter(|_| excel_links)
                    .map(|i| Variant::Bstr(links[i].path.clone()))
                    .collect();
                let mut seen: Vec<String> = Vec::new();
                let paths: Vec<Variant> = paths
                    .into_iter()
                    .filter(|p| match p {
                        Variant::Bstr(s) if seen.iter().any(|x| same_path(x, s)) => false,
                        Variant::Bstr(s) => {
                            seen.push(s.clone());
                            true
                        }
                        _ => true,
                    })
                    .collect();
                if paths.is_empty() {
                    return Ok(Variant::Empty);
                }
                Ok(Variant::Array(Box::new(SafeArray {
                    elem: VarType::Variant,
                    bounds: vec![(1, paths.len() as u32)],
                    data: paths,
                })))
            }
            "UpdateLink" => {
                let n = a.string(0)?.ok_or_else(|| unable("UpdateLink"))?;
                let li = self.named_link(b, &n).ok_or_else(|| unable("UpdateLink"))?;
                let l = self.book(b)?.wb.ext_links()[li].clone();

                if self.link_source(&l, self.book_index(b)).is_some() {
                    return Err(unable("UpdateLink"));
                }
                self.update_link_from_file(b, li);
                Ok(Variant::Bool(true))
            }
            "ChangeLink" => {
                let (Some(n), Some(new)) = (a.string(0)?, a.string(1)?) else {
                    return Err(unable("ChangeLink"));
                };
                let li = self.named_link(b, &n).ok_or_else(|| unable("ChangeLink"))?;
                let full = self.fs.full_path(&new).unwrap_or(new);
                let links = self.book(b)?.wb.ext_links();
                let old = links[li].path.clone();

                let same = same_path(&old, &full);

                let all: Vec<usize> = (0..links.len())
                    .filter(|&i| i == li || (!links[i].broken && same_path(&links[i].path, &old)))
                    .collect();
                self.book_mut(b)?.wb.ext_change_links(&all, &full);
                for i in all {
                    if !same {
                        self.update_link_from_file(b, i);
                    }
                }
                self.sync_links();

                self.raise_application("AfterCalculate");
                Ok(Variant::Bool(true))
            }
            "BreakLink" => {
                let n = a.string(0)?.ok_or_else(OmError::app_defined)?;
                let li = self.named_link(b, &n).ok_or_else(OmError::app_defined)?;
                let path = self.book(b)?.wb.ext_links()[li].path.clone();

                if let Some(act) = self.active.filter(|&x| x != b) {
                    if self.named_link(act, &path).is_none() {
                        return Err(OmError::app_defined());
                    }
                }
                let same: Vec<usize> = (0..self.book(b)?.wb.ext_links().len())
                    .filter(|&i| {
                        same_path(
                            &self
                                .book(b)
                                .map(|x| x.wb.ext_links()[i].path.clone())
                                .unwrap_or_default(),
                            &path,
                        )
                    })
                    .collect();
                for i in same {
                    self.book_mut(b)?.wb.ext_break(i);
                }

                self.book_mut(b)?.wb.take_notices();
                Ok(Variant::Empty)
            }

            "LinkInfo" => {
                let n = a.string(0)?.ok_or_else(OmError::app_defined)?;

                if a.i32(1)? != Some(3) {
                    return Ok(Variant::Error(crate::error::vba_scode(2042)));
                }
                let Some(li) = self.named_link(b, &n) else {
                    return Ok(Variant::Error(crate::error::vba_scode(2015)));
                };
                let l = self.book(b)?.wb.ext_links()[li].clone();
                let status = if self.link_source(&l, self.book_index(b)).is_some() {
                    9
                } else if l.fresh {
                    0
                } else {
                    3
                };
                Ok(Variant::I4(status))
            }
            "OpenLinks" => {
                let n = a.string(0)?.ok_or_else(OmError::app_defined)?;
                let li = self.named_link(b, &n).ok_or_else(OmError::app_defined)?;
                let l = self.book(b)?.wb.ext_links()[li].clone();
                if self.link_source(&l, self.book_index(b)).is_none() {
                    let read_only = a.bool(1)?.unwrap_or(false);

                    if !n.contains(['\\', '/']) {
                        return Err(OmError::app_defined_with(&format!(
                            "Sorry, we couldn't find {n}. Is it possible it was moved, renamed or deleted?"
                        )));
                    }
                    self.open_link_source(&l.path, read_only)?;
                    self.active = Some(b);
                }
                Ok(Variant::Empty)
            }
            _ => Err(OmError::app_defined()),
        }
    }
}

impl Application<'_> {
    fn open_link_source(&mut self, path: &str, read_only: bool) -> Result<(), OmError> {
        let a = Args {
            values: vec![
                Some(Variant::Bstr(path.to_string())),
                None,
                Some(Variant::Bool(read_only)),
            ],
            put: None,
            lcid: 0x0409,
        };
        let operands = self.open_operands(&a)?;
        let (v, _) = self.workbooks_open(operands, a.lcid)?;
        if let Variant::Dispatch(Some(id)) = v {
            self.release(id);
        }
        Ok(())
    }
}

fn quoted_book_paths(text: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut chars = text.chars().peekable();
    let mut in_str = false;
    while let Some(c) = chars.next() {
        if c == '"' {
            in_str = !in_str;
            continue;
        }
        if in_str || c != '\'' {
            continue;
        }
        let mut part = String::new();
        while let Some(d) = chars.next() {
            if d == '\'' {
                if chars.peek() == Some(&'\'') {
                    chars.next();
                    part.push('\'');
                    continue;
                }
                break;
            }
            part.push(d);
        }
        if chars.peek() == Some(&'!') && !part.contains('[') && part.contains(['\\', '/']) {
            out.push(part);
        }
    }
    out
}
