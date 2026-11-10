use crate::app::{Application, BookId, BookWb};
use crate::conv::Args;
use crate::error::OmError;
use xlcalc::{ExtLink, ExtName, ExtSheetData};
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

    pub(crate) fn book_index(&self, b: BookId) -> usize {
        self.books.iter().position(|x| x.id == b).unwrap_or(0)
    }

    pub(crate) fn sync_links(&mut self) {
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
            .window_order()
            .into_iter()
            .filter_map(|id| self.book(id).ok().map(|b| b.name.clone()))
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
            return;
        }
        let mut snaps: Vec<Option<(Vec<ExtSheetData>, Vec<ExtName>)>> =
            vec![None; self.books.len()];
        for bi in 0..self.books.len() {
            if !self.books[bi].wb.has_ext_links() {
                continue;
            }
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
                    if open && !was_open {
                        w.ext_reopened();
                    }
                    if let Some(d) = data {
                        touched |= !w.ext_set_data(li, d.0).changed.is_empty();
                        touched |= w.ext_set_names(li, d.1);
                    }
                    if touched && open {
                        self.books[bi].saved = false;
                    }
                }
            }
        }
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
        if let Ok(bk) = self.book_mut(b) {
            bk.wb.ext_set_data(li, data);
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
        let bi = self.book_index(b);
        for (li, l) in links.iter().enumerate() {
            if update && !l.broken && self.link_source(l, bi).is_none() {
                self.update_link_from_file(b, li);
            }
        }
        self.sync_links();
    }

    fn named_link(&self, b: BookId, name: &str) -> Option<usize> {
        let bk = self.book(b).ok()?;
        bk.wb.ext_links().iter().position(|l| {
            !l.broken
                && (same_path(&l.path, name)
                    || (!name.contains(['\\', '/']) && l.name().eq_ignore_ascii_case(name)))
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
                let paths: Vec<Variant> = self
                    .book(b)?
                    .wb
                    .ext_links()
                    .into_iter()
                    .filter(|l| !l.broken && excel_links)
                    .map(|l| Variant::Bstr(l.path))
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
                if self.link_source(&l, self.book_index(b)).is_none() {
                    self.update_link_from_file(b, li);
                }
                Ok(Variant::Bool(true))
            }
            "ChangeLink" => {
                let (Some(n), Some(new)) = (a.string(0)?, a.string(1)?) else {
                    return Err(unable("ChangeLink"));
                };
                let li = self.named_link(b, &n).ok_or_else(|| unable("ChangeLink"))?;
                let full = self.fs.full_path(&new).unwrap_or(new);

                let same = same_path(&self.book(b)?.wb.ext_links()[li].path, &full);
                self.book_mut(b)?.wb.ext_set_link(li, &full, false);
                if !same {
                    self.update_link_from_file(b, li);
                }
                self.sync_links();

                self.raise_application("AfterCalculate");
                Ok(Variant::Bool(true))
            }
            "BreakLink" => {
                let n = a.string(0)?.ok_or_else(OmError::app_defined)?;
                let li = self.named_link(b, &n).ok_or_else(OmError::app_defined)?;
                let path = self.book(b)?.wb.ext_links()[li].path.clone();
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
            "OpenLinks" => {
                let n = a.string(0)?.ok_or_else(OmError::app_defined)?;
                let li = self.named_link(b, &n).ok_or_else(OmError::app_defined)?;
                let l = self.book(b)?.wb.ext_links()[li].clone();
                if self.link_source(&l, self.book_index(b)).is_none() {
                    let read_only = a.bool(1)?.unwrap_or(false);
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
        let v = self.workbooks_open(&a)?;
        if let Variant::Dispatch(Some(id)) = v {
            self.release(id);
        }
        Ok(())
    }
}
