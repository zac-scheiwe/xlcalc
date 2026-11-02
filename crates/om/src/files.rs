use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use xlcalc::Workbook;
use xlcalc_variant::Variant;
use xlcalc_xlsx::{FileKind, Package, SaveOptions};

use crate::app::{Application, BookId, Object};
use crate::conv::Args;
use crate::error::OmError;

pub trait FileSystem: Send {
    fn read(&self, path: &str) -> io::Result<Vec<u8>>;

    fn write(&self, path: &str, bytes: &[u8]) -> io::Result<()>;

    fn exists(&self, path: &str) -> bool;

    fn full_path(&self, path: &str) -> Option<String>;
}

pub struct RealFs;

fn strip_verbatim(p: PathBuf) -> String {
    let s = p.to_string_lossy().to_string();
    match s.strip_prefix(r"\\?\UNC\") {
        Some(rest) => format!(r"\\{rest}"),
        None => s.strip_prefix(r"\\?\").map_or(s.clone(), str::to_string),
    }
}

impl FileSystem for RealFs {
    fn read(&self, path: &str) -> io::Result<Vec<u8>> {
        std::fs::read(path)
    }

    fn write(&self, path: &str, bytes: &[u8]) -> io::Result<()> {
        let target = Path::new(path);
        let dir = target.parent().unwrap_or(Path::new("."));
        let name = target
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();
        let tmp = dir.join(format!("~${name}.{}.tmp", std::process::id()));
        std::fs::write(&tmp, bytes)?;
        std::fs::rename(&tmp, target).inspect_err(|_| {
            let _ = std::fs::remove_file(&tmp);
        })
    }

    fn exists(&self, path: &str) -> bool {
        Path::new(path).is_file()
    }

    fn full_path(&self, path: &str) -> Option<String> {
        if path.is_empty() {
            return None;
        }
        let p = Path::new(path);
        if let Ok(full) = std::fs::canonicalize(p) {
            return Some(strip_verbatim(full));
        }
        let name = p.file_name()?;
        let dir = match p.parent() {
            Some(d) if !d.as_os_str().is_empty() => d.to_path_buf(),
            _ => std::env::current_dir().ok()?,
        };
        let dir = std::fs::canonicalize(dir).ok().filter(|d| d.is_dir())?;
        Some(strip_verbatim(dir.join(name)))
    }
}

#[derive(Default)]
pub struct MemFs {
    files: Mutex<HashMap<String, Vec<u8>>>,
    folders: Mutex<Vec<String>>,
}

impl MemFs {
    pub fn new() -> MemFs {
        MemFs::default()
    }

    fn key(path: &str) -> String {
        path.replace('/', "\\").to_lowercase()
    }

    pub fn add_folder(&self, path: &str) {
        self.folders.lock().unwrap().push(MemFs::key(path));
    }

    pub fn get(&self, path: &str) -> Option<Vec<u8>> {
        self.files.lock().unwrap().get(&MemFs::key(path)).cloned()
    }
}

impl FileSystem for MemFs {
    fn read(&self, path: &str) -> io::Result<Vec<u8>> {
        self.get(path)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, path.to_string()))
    }

    fn write(&self, path: &str, bytes: &[u8]) -> io::Result<()> {
        self.files
            .lock()
            .unwrap()
            .insert(MemFs::key(path), bytes.to_vec());
        Ok(())
    }

    fn exists(&self, path: &str) -> bool {
        self.files.lock().unwrap().contains_key(&MemFs::key(path))
    }

    fn full_path(&self, path: &str) -> Option<String> {
        let p = path.replace('/', "\\");
        let (dir, _) = p.rsplit_once('\\')?;
        let key = MemFs::key(dir);
        let known = self.folders.lock().unwrap().contains(&key)
            || self
                .files
                .lock()
                .unwrap()
                .keys()
                .any(|f| f.rsplit_once('\\').is_some_and(|(d, _)| d == key));
        known.then_some(p)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Writes {
    Package(FileKind),
    Csv,

    Xlsb,

    Unsupported,
}

fn format_of(code: i32) -> Option<(&'static str, Writes)> {
    Some(match code {
        51 => ("xlsx", Writes::Package(FileKind::Xlsx)),
        52 => ("xlsm", Writes::Package(FileKind::Xlsm)),
        54 => ("xltx", Writes::Package(FileKind::Xltx)),
        6 | 62 => ("csv", Writes::Csv),
        50 => ("xlsb", Writes::Xlsb),
        53 => ("xltm", Writes::Unsupported),
        55 => ("xlam", Writes::Unsupported),
        56 => ("xls", Writes::Unsupported),
        _ => {
            let known = crate::typelib::enumeration("XlFileFormat")
                .is_some_and(|e| e.constants.iter().any(|&(_, v)| v == code));
            if !known {
                return None;
            }
            ("", Writes::Unsupported)
        }
    })
}

const WORKBOOK_EXTENSIONS: [&str; 7] = ["xlsx", "xlsm", "xlsb", "xltx", "xltm", "xlam", "xls"];

fn cannot_access(path: &str) -> OmError {
    OmError {
        error_info: true,
        ..OmError::app_defined_with(&format!(
            "Microsoft Excel cannot access the file '{path}'. There are several possible reasons:\n\n\
             \u{2022} The file name or path does not exist.\n\
             \u{2022} The file is being used by another program.\n\
             \u{2022} The workbook you are trying to save has the same name as a currently open workbook."
        ))
    }
}

fn not_found(file: &str) -> OmError {
    OmError {
        error_info: true,
        ..OmError::app_defined_with(&format!(
            "Sorry, we couldn't find {file}. Is it possible it was moved, renamed or deleted?"
        ))
    }
}

fn bad_extension() -> OmError {
    OmError {
        error_info: true,
        ..OmError::app_defined_with(
            "This extension can not be used with the selected file type. Change the file extension \
             in the File name text box or select a different file type by changing the Save as type.",
        )
    }
}

fn extension(name: &str) -> Option<String> {
    Path::new(name)
        .extension()
        .map(|e| e.to_string_lossy().to_lowercase())
}

pub(crate) fn file_name(path: &str) -> String {
    path.rsplit(['\\', '/']).next().unwrap_or(path).to_string()
}

pub(crate) fn folder(path: &str) -> String {
    match path.rfind(['\\', '/']) {
        Some(i) => path[..i].to_string(),
        None => String::new(),
    }
}

#[derive(PartialEq, Eq)]
enum Content {
    Xlsb,
    Xml,
    Text,
}

fn content_of(bytes: &[u8]) -> Content {
    if !bytes.starts_with(b"PK") {
        return Content::Text;
    }
    match Package::read(bytes) {
        Ok(p) if p.part("/xl/workbook.bin").is_some() => Content::Xlsb,
        _ => Content::Xml,
    }
}

fn share_settings(from: &Workbook, to: &mut Workbook) {
    let s = from.settings().clone();
    if from.is_semiautomatic() {
        to.set_semiautomatic();
    } else {
        to.set_mode(s.mode);
    }
    to.set_iteration(s.iteration, Some(s.max_iterations), Some(s.max_change));
}

impl Application<'_> {
    pub fn set_file_system(&mut self, fs: Box<dyn FileSystem>) {
        self.fs = fs;
    }

    pub const AUTOMATION_BOOK_NUMBERS: [u32; 6] = [1, 4, 5, 6, 7, 14];

    pub fn reserve_book_numbers(&mut self, numbers: &[u32]) {
        self.reserved_numbers.extend_from_slice(numbers);
    }

    pub(crate) fn next_book_number(&mut self) -> u32 {
        while self.reserved_numbers.contains(&self.next_number) {
            self.next_number += 1;
        }
        let n = self.next_number;
        self.next_number += 1;
        n
    }

    fn open_new(&mut self, mut wb: Workbook, name: String, path: String) -> BookId {
        if let Some(active) = self.active.and_then(|id| self.engine(id)) {
            share_settings(active, &mut wb);
        }
        self.push_book(wb, name, path)
    }

    pub(crate) fn workbooks_add(&mut self, a: &Args) -> Result<Variant, OmError> {
        let failed = || OmError::app_defined_with("Add method of Workbooks class failed");
        let id = match a.get(0) {
            None => {
                let name = format!("Book{}", self.next_book_number());
                let id = self.open_new(Workbook::new(), name, String::new());
                self.book_mut(id)?.saved = true;
                id
            }
            Some(Variant::Bstr(file)) => {
                let file = file.clone();
                let full = self
                    .fs
                    .full_path(&file)
                    .filter(|p| self.fs.exists(p))
                    .ok_or_else(|| not_found(&file))?;
                let bytes = self.fs.read(&full).map_err(|_| not_found(&file))?;
                let base = file_name(&full);
                let stem = base
                    .rsplit_once('.')
                    .map_or(base.as_str(), |(s, _)| s)
                    .to_string();
                let n = self.template_counts.entry(stem.to_lowercase()).or_insert(0);
                *n += 1;
                let name = format!("{stem}{n}");
                let id = self.open_bytes(bytes, &base, String::new(), name, false)?;
                let b = self.book_mut(id)?;
                b.file_format = 51;
                b.saved = true;
                id
            }
            Some(v) => {
                let n = Args {
                    values: vec![Some(v.clone())],
                    put: None,
                    lcid: a.lcid,
                }
                .i32(0)?
                .unwrap_or(0);
                match n {
                    -4167 => {
                        let c = self.template_counts.entry("\u{0}sheet".into()).or_insert(0);
                        *c += 1;
                        let name = format!("Sheet{c}");
                        let id = self.open_new(Workbook::new(), name, String::new());
                        self.book_mut(id)?.saved = true;
                        id
                    }
                    -4109 => {
                        let c = self.template_counts.entry("\u{0}chart".into()).or_insert(0);
                        *c += 1;
                        let name = format!("Chart{c}");
                        let mut wb = Workbook::empty();
                        wb.add_chart_sheet_before("Chart1", None)
                            .map_err(|_| failed())?;
                        let id = self.open_new(wb, name, String::new());
                        let book = self.book_mut(id)?;
                        book.saved = true;
                        book.next_chart = 2;
                        id
                    }
                    _ => return Err(failed()),
                }
            }
        };
        Ok(self.alloc(Object::Workbook(id)))
    }

    pub(crate) fn workbooks_open(&mut self, a: &Args) -> Result<Variant, OmError> {
        let file = a.string(0)?.ok_or_else(OmError::app_defined)?;
        let full = self
            .fs
            .full_path(&file)
            .filter(|p| self.fs.exists(p))
            .ok_or_else(|| not_found(&file))?;
        if let Some(b) = self
            .books
            .iter()
            .find(|b| b.path.eq_ignore_ascii_case(&full))
        {
            let id = b.id;
            self.active = Some(id);
            return Ok(self.alloc(Object::Workbook(id)));
        }
        if a.get(4).is_some() || a.get(5).is_some() {
            return Err(OmError::unsupported("Workbooks.Open with a password"));
        }
        let read_only = a.bool(2)?.unwrap_or(false);
        let local = a.bool(13)?.unwrap_or(false);
        let bytes = self.fs.read(&full).map_err(|_| not_found(&file))?;
        let name = file_name(&full);
        let id = if content_of(&bytes) == Content::Text {
            let format = a.i32(3)?;
            let empty_delimiter = matches!(a.get(8), Some(Variant::Bstr(d)) if d.is_empty());
            if a.get(7).is_some() || (format == Some(6) && empty_delimiter) {
                return Err(OmError::app_defined_with(
                    "Open method of Workbooks class failed",
                ));
            }
            if extension(&name).is_some_and(|e| WORKBOOK_EXTENSIONS.contains(&e.as_str())) {
                return Err(format_not_valid(&name));
            }
            let imp = if format == Some(5) {
                xlcalc_xlsx::import_csv_unsplit(&bytes, &name, local)
            } else {
                xlcalc_xlsx::import_csv(&bytes, &name, local)
            };
            let mut wb = imp.workbook;
            if let Some(active) = self.active.and_then(|id| self.engine(id)) {
                share_settings(active, &mut wb);
            }
            let id = self.push_book(wb, imp.name, full);
            self.book_mut(id)?.file_format = imp.file_format;
            id
        } else {
            self.open_bytes(bytes, &name, full, name.clone(), true)?
        };
        let b = self.book_mut(id)?;
        b.read_only = read_only;
        b.saved = true;
        Ok(self.alloc(Object::Workbook(id)))
    }

    fn open_bytes(
        &mut self,
        bytes: Vec<u8>,
        file: &str,
        path: String,
        name: String,
        check_extension: bool,
    ) -> Result<BookId, OmError> {
        let content = content_of(&bytes);
        let ext = extension(file).unwrap_or_default();
        if check_extension
            && ((content == Content::Xlsb && ext != "xlsb" && ext != "xls")
                || (content == Content::Xml && ext == "xlsb"))
        {
            return Err(format_not_valid(file));
        }
        let imp = xlcalc_xlsx::import_bytes(bytes).map_err(|e| OmError::app_defined_with(&e))?;
        let mut wb = imp.workbook;
        if let Some(active) = self.active.and_then(|id| self.engine(id)) {
            share_settings(active, &mut wb);
        }
        let id = self.push_book(wb, name, path);
        let mut warnings = imp.warnings;
        let has_project = imp.vba.is_some();
        if !has_project {
            let b = self.book_mut(id)?;
            let names: Vec<(xlcalc::SheetId, String)> = imp
                .sheet_code_names
                .iter()
                .filter_map(|(sheet, code)| Some((b.wb.sheet_by_name(sheet)?.id, code.clone()?)))
                .collect();
            b.vba.sheet_code_names = names;
        }
        if let Some(part) = &imp.vba {
            self.ide_import(id, part);
            let (modules, mut w) = crate::vba::import_modules(part);
            warnings.append(&mut w);
            if !modules.is_empty() {
                let b = self.book_mut(id)?;
                let sheets: Vec<(xlcalc::SheetId, String)> = part
                    .sheet_code_names
                    .iter()
                    .filter_map(|(sheet, code)| {
                        let s = b.wb.sheet_by_name(sheet)?;
                        Some((s.id, code.clone()?))
                    })
                    .collect();
                b.vba.sheet_code_names = sheets;
                b.vba.workbook_code_name = part.workbook_code_name.clone();
                b.vba.modules = modules;
                for e in self.compile_vba(id)? {
                    warnings.push(format!(
                        "VBA module {} does not compile ({e}); its functions are #NAME?",
                        e.module
                    ));
                }
            }
        }
        let b = self.book_mut(id)?;
        b.warnings = warnings;
        b.file_format = match (content, ext.as_str()) {
            (Content::Xlsb, _) => 50,
            (_, "xltx") => 54,
            (_, "xltm") => 53,
            _ if has_project => 52,
            (_, "xlsm") => 52,
            _ => 51,
        };
        for sh in b.wb.sheets().map(|s| s.id).collect::<Vec<_>>() {
            let list = xlcalc_xlsx::read_controls(&imp.package, sh);
            if !list.is_empty() {
                b.controls.insert(sh, crate::controls::from_file(list));
            }
        }
        b.package = Some(imp.package);
        Ok(id)
    }

    fn file_bytes(
        &mut self,
        b: BookId,
        format: i32,
        file: &str,
        rename: bool,
    ) -> Result<Vec<u8>, OmError> {
        let Some((_, writes)) = format_of(format) else {
            return Err(OmError::app_defined_with(
                "SaveAs method of Workbook class failed",
            ));
        };
        match writes {
            Writes::Unsupported => Err(OmError::unsupported(&format!(
                "saving as FileFormat {format}"
            ))),
            Writes::Csv => {
                let bk = self.book_mut(b)?;
                if rename {
                    xlcalc_xlsx::save_as_csv(&mut bk.wb, file, format)
                        .map(|s| s.bytes)
                        .map_err(|e| OmError::app_defined_with(&e))
                } else {
                    let sheet = bk.wb.active_sheet();
                    xlcalc_xlsx::export_csv(&bk.wb, sheet, format)
                        .map_err(|e| OmError::app_defined_with(&e))
                }
            }
            Writes::Package(_) | Writes::Xlsb => {
                let kind = match writes {
                    Writes::Package(kind) => kind,
                    _ if self.has_vb_project(b)? => FileKind::Xlsm,
                    _ => FileKind::Xlsx,
                };
                let vba = match kind {
                    _ if self.has_vb_project(b)? => self.vba_write(b)?,
                    _ => None,
                };
                let controls = self.controls_for_save(b);
                let bk = self.book(b)?;
                let opts = SaveOptions {
                    kind: Some(kind),
                    vba,
                    controls,
                    ..SaveOptions::default()
                };
                let saved = xlcalc_xlsx::save(&bk.wb, bk.package.as_ref(), &opts)
                    .map_err(|e| OmError::app_defined_with(&e))?;
                let warnings = saved.warnings;
                self.book_mut(b)?.warnings.extend(warnings);
                if writes == Writes::Xlsb {
                    let (bytes, more) = xlcalc_xlsx::xlsx_to_xlsb(&saved.bytes)
                        .map_err(|e| OmError::app_defined_with(&e))?;
                    self.book_mut(b)?.warnings.extend(more);
                    return Ok(bytes);
                }
                Ok(saved.bytes)
            }
        }
    }

    pub(crate) fn save_book_as(
        &mut self,
        b: BookId,
        filename: &str,
        format: Option<i32>,
        copy: bool,
    ) -> Result<(), OmError> {
        let current = self.book(b)?.file_format;
        let format = format.unwrap_or(current);
        let Some((ext, _)) = format_of(format) else {
            return Err(OmError::app_defined_with(
                "SaveAs method of Workbook class failed",
            ));
        };
        let mut filename = filename.to_string();
        match extension(&file_name(&filename)) {
            None if !ext.is_empty() => filename = format!("{filename}.{ext}"),
            Some(e) if WORKBOOK_EXTENSIONS.contains(&e.as_str()) && e != ext && !copy => {
                return Err(bad_extension());
            }
            _ => {}
        }
        let full = self
            .fs
            .full_path(&filename)
            .ok_or_else(|| cannot_access(&filename))?;
        let name = file_name(&full);

        let bytes = self.file_bytes(b, format, &name, !copy)?;
        self.fs
            .write(&full, &bytes)
            .map_err(|_| cannot_access(&full))?;
        if copy {
            return Ok(());
        }
        let csv = matches!(format_of(format), Some((_, Writes::Csv)));
        let bk = self.book_mut(b)?;
        bk.name = name;
        bk.path = full;
        bk.file_format = format;
        bk.read_only = false;

        bk.saved = !csv || bk.wb.sheet_count() == 1;
        Ok(())
    }

    pub(crate) fn save_book(&mut self, b: BookId) -> Result<(), OmError> {
        let bk = self.book(b)?;
        if bk.read_only {
            return Err(OmError::app_defined());
        }
        let (path, format) = if bk.path.is_empty() {
            let ext = format_of(bk.file_format).map_or("xlsx", |f| f.0);
            let dir = self.default_file_path.trim_end_matches(['\\', '/']);
            (format!("{dir}\\{}.{ext}", bk.name), bk.file_format)
        } else {
            (bk.path.clone(), bk.file_format)
        };
        self.save_book_as(b, &path, Some(format), false)
    }

    pub(crate) fn close_book(&mut self, b: BookId, a: &Args) -> Result<Variant, OmError> {
        self.book(b)?;
        let save = a.bool(0)?.unwrap_or(false);
        if save {
            match a.string(1)? {
                Some(f) if !f.is_empty() => {
                    self.save_book_as(b, &f, None, false)?;
                }
                _ => {
                    if !self.book(b)?.saved {
                        self.save_book(b)?;
                    }
                }
            }
        }
        self.books.retain(|x| x.id != b);
        if self.active == Some(b) {
            self.active = self.books.last().map(|x| x.id);
        }
        Ok(Variant::Bool(true))
    }

    pub(crate) fn save_as_member(&mut self, b: BookId, a: &Args) -> Result<Variant, OmError> {
        self.book(b)?;
        if a.get(2).is_some() || a.get(3).is_some() {
            return Err(OmError::unsupported("Workbook.SaveAs with a password"));
        }
        let file = a.string(0)?.unwrap_or_default();
        let format = a.i32(1)?;
        self.save_book_as(b, &file, format, false)?;
        Ok(Variant::Bool(true))
    }

    pub(crate) fn has_vb_project(&self, b: BookId) -> Result<bool, OmError> {
        let bk = self.book(b)?;
        Ok(!bk.vba.modules.is_empty()
            || bk
                .package
                .as_ref()
                .is_some_and(|p| p.part("/xl/vbaProject.bin").is_some()))
    }
}

fn format_not_valid(file: &str) -> OmError {
    OmError::app_defined_with(&format!(
        "Excel cannot open the file '{file}' because the file format or file extension is not \
         valid. Verify that the file has not been corrupted and that the file extension matches \
         the format of the file."
    ))
}
