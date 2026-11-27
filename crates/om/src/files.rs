use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use xlcalc::Workbook;
use xlcalc_variant::Variant;
use xlcalc_xlsx::{FileKind, Package, SaveOptions};

use crate::app::{Application, BookId, Object};
use crate::cmdargs::{Oper, Operands, Reader, Refused};
use crate::conv::{command_number, sfix, Args};
use crate::error::OmError;

pub trait FileSystem: Send {
    fn read(&self, path: &str) -> io::Result<Vec<u8>>;

    fn write(&self, path: &str, bytes: &[u8]) -> io::Result<()>;

    fn exists(&self, path: &str) -> bool;

    fn full_path(&self, path: &str) -> Option<String>;

    fn save_folder(&self, full: &str) -> String {
        xlcalc::split_path(&self.expand(full)).0.to_string()
    }

    fn expand(&self, path: &str) -> String {
        path.to_string()
    }
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

    Xls,

    Unsupported,
}

fn format_of(code: i32) -> Option<(&'static str, Writes)> {
    Some(match code {
        51 => ("xlsx", Writes::Package(FileKind::Xlsx)),

        61 => ("xlsx", Writes::Package(FileKind::Xlsx)),
        52 => ("xlsm", Writes::Package(FileKind::Xlsm)),
        54 => ("xltx", Writes::Package(FileKind::Xltx)),
        6 | 62 => ("csv", Writes::Csv),
        50 => ("xlsb", Writes::Xlsb),
        53 => ("xltm", Writes::Unsupported),
        55 => ("xlam", Writes::Unsupported),
        56 | 39 => ("xls", Writes::Xls),
        17 => ("xlt", Writes::Xls),
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

fn int_in(
    r: Reader,
    o: &Operands,
    i: usize,
    range: std::ops::RangeInclusive<i32>,
) -> Result<bool, Refused> {
    Ok(r.int(o.get(i))?.is_none_or(|n| range.contains(&n)))
}

fn text_of(o: &Operands, i: usize) -> Option<&str> {
    match o.get(i) {
        Oper::Text(s) | Oper::Dated(_, s) => Some(s),
        _ => None,
    }
}

fn text_len(o: &Operands, i: usize) -> Option<usize> {
    text_of(o, i).map(|s| xlcalc::wide::units(s).count())
}

struct OpenArgs {
    file: String,
    update_links: Option<i32>,
    read_only: bool,
    origin: Option<i32>,
    password: bool,
    local: bool,
}

fn open_args(mut o: Operands, r: Reader, no_delimiter: &mut bool) -> Result<OpenArgs, Refused> {
    if !o.get(16).is_missing() {
        if !(0..=2).contains(&r.executor_int(o.get(16))?) {
            return Err(Refused);
        }
        o.take(16);
    }

    let local = r.flag(o.get(15))?.unwrap_or(false);
    o.take(15);
    o.coerce(r)?;
    let file = text_of(&o, 0).ok_or(Refused)?.to_string();

    let update = int_in(r, &o, 1, 0..=3)?;
    let format = r.int(o.get(3))?;
    let known = format.is_none_or(|n| (1..=6).contains(&n) && (n != 6 || o.given > 8));
    if !(update && known) {
        return Err(Refused);
    }
    if let Some(n) = format {
        *no_delimiter = n == 5;
    }

    let origin = int_in(r, &o, 7, 1..=3)?;
    let delimiter = format != Some(6) || text_len(&o, 8) != Some(0);

    let passwords = text_len(&o, 5).is_none_or(|n| n <= 15);
    if !(origin && delimiter && passwords) {
        return Err(Refused);
    }
    Ok(OpenArgs {
        file,
        update_links: r.int(o.get(1))?,
        read_only: r.flag(o.get(2))?.unwrap_or(false),
        origin: r.int(o.get(7))?,
        password: !o.get(4).is_missing() || !o.get(5).is_missing(),
        local,
    })
}

fn known_code_page(cp: u16) -> bool {
    matches!(
        cp,
        0..=3
            | 37
            | 437
            | 500
            | 708
            | 720
            | 737
            | 775
            | 850
            | 852
            | 855
            | 857..=866
            | 869
            | 870
            | 874
            | 875
            | 932
            | 936
            | 949
            | 950
            | 1026
            | 1047
            | 1140..=1149
            | 1200
            | 1201
            | 1250..=1258
            | 1361
            | 10000..=10007
            | 20127
            | 20866
            | 21866
            | 28591..=28606
            | 50220..=50222
            | 51932
            | 51949
            | 52936
            | 54936
            | 65000
            | 65001
    )
}

fn field_info_ok(r: Reader, o: &Operands) -> bool {
    let number = |v: &Variant| command_number(Some(v), r.lcid, r.wb).map(|o| o.map(sfix));
    let pair = |column: &Variant, kind: &Variant| {
        number(column).is_ok_and(|n| n.is_none_or(|n| (0..=0x7fff).contains(&n)))
            && number(kind).is_ok_and(|n| n.is_none_or(|n| (1..=9).contains(&n)))
    };
    match o.get(12) {
        Oper::Missing => true,
        Oper::Array(sa) => match sa.bounds.len() {
            1 => sa.data.iter().all(|row| match row {
                Variant::Array(r) => {
                    r.bounds.len() == 1 && matches!(&r.data[..], [c, k] if pair(c, k))
                }
                _ => false,
            }),

            2 if sa.bounds[1].1 == 2 => {
                let rows = sa.data.len() / 2;
                (0..rows).all(|r| pair(&sa.data[r], &sa.data[r + rows]))
            }
            _ => false,
        },
        _ => false,
    }
}

fn open_text_args(mut o: Operands, r: Reader) -> Result<String, Refused> {
    if !o.get(17).is_missing() && r.flag(o.get(17)).is_ok() {
        o.take(17);
    }
    o.coerce(r)?;
    let file = text_of(&o, 0).ok_or(Refused)?.to_string();

    let origin = r
        .int(o.get(1))?
        .is_none_or(|n| (1..=3).contains(&n) || n >= -0x7fff && known_code_page(n as u16));

    let numbers = int_in(r, &o, 2, 1..=99_999_999)?
        && int_in(r, &o, 3, 1..=2)?
        && int_in(r, &o, 4, 1..=3)?
        && int_in(r, &o, 13, 1..=2)?;

    let separators = [14, 15]
        .iter()
        .all(|&i| text_len(&o, i).is_none_or(|n| n == 1));
    if !(origin && numbers && separators && field_info_ok(r, &o)) {
        return Err(Refused);
    }
    Ok(file)
}

fn open_failed() -> OmError {
    OmError::app_defined_with("Open method of Workbooks class failed")
}

fn not_found(file: &str) -> OmError {
    let dir_named;
    let file = if file.ends_with(['\\', '/']) {
        dir_named = format!("{file}.xlsx");
        dir_named.as_str()
    } else {
        file
    };
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
    Xls,
    Xml,
    Text,
}

fn content_of(bytes: &[u8]) -> Content {
    if bytes.starts_with(&[0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1]) {
        return Content::Xls;
    }
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

    pub const AUTOMATION_BOOK_NUMBERS: [u32; 0] = [];

    pub fn reserve_book_numbers(&mut self, numbers: &[u32]) {
        self.reserved_numbers.extend_from_slice(numbers);
    }

    pub fn set_next_sheet_number(&mut self, book: BookId, n: u32) {
        if let Ok(b) = self.book_mut(book) {
            b.next_sheet = n;
        }
    }

    pub fn set_sheets_in_new_workbook(&mut self, n: u32) {
        let key = "0:SheetsInNewWorkbook".to_string();
        if n == 2 {
            self.opts.remove(&key);
        } else {
            self.opts.insert(key, Variant::R8(f64::from(n)));
        }
    }

    pub(crate) fn next_book_number(&mut self) -> u32 {
        while self.reserved_numbers.contains(&self.next_number) {
            self.next_number += 1;
        }
        let n = self.next_number;
        self.next_number += 1;
        n
    }

    pub(crate) fn open_new(&mut self, mut wb: Workbook, name: String, path: String) -> BookId {
        if let Some(active) = self.active.and_then(|id| self.engine(id)) {
            share_settings(active, &mut wb);
        }
        self.push_book(wb, name, path)
    }

    fn new_workbook(&self) -> Workbook {
        let n = match self.opts.get("0:SheetsInNewWorkbook") {
            Some(Variant::R8(n)) => *n as u32,
            _ => 2,
        };
        let mut wb = Workbook::new();
        for i in 2..=n {
            let _ = wb.add_sheet(&format!("Sheet{i}"));
        }
        let first = wb.sheet_at(0).id;
        let _ = wb.set_active_sheet(first);
        wb
    }

    fn plain_workbook(&mut self) -> BookId {
        let name = format!("Book{}", self.next_book_number());
        let id = self.open_new(self.new_workbook(), name, String::new());
        if let Ok(b) = self.book_mut(id) {
            b.saved = true;
        }
        id
    }

    pub(crate) fn workbooks_add(&mut self, a: &Args) -> Result<Variant, OmError> {
        let failed = || OmError::app_defined_with("Add method of Workbooks class failed");
        let id = match a.get(0) {
            None => {
                let name = format!("Book{}", self.next_book_number());
                let id = self.open_new(self.new_workbook(), name, String::new());
                self.book_mut(id)?.saved = true;
                id
            }

            Some(Variant::Bstr(s)) if s.is_empty() => self.plain_workbook(),
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
                let v = match v {
                    Variant::Bool(true) => Variant::I4(1),
                    Variant::R8(f) if f.abs() < 1e9 => Variant::I4(f.trunc() as i32),
                    v => v.clone(),
                };
                let n = Args {
                    values: vec![Some(v)],
                    put: None,
                    lcid: a.lcid,
                }
                .i32(0)
                .map_err(|_| failed())?
                .unwrap_or(0);
                match n {
                    5 => self.plain_workbook(),

                    6 => {
                        let name = format!("Book{}", self.next_book_number());
                        let mut wb = Workbook::new();
                        let first = wb.sheet_at(0).id;
                        let module = wb
                            .add_legacy_sheet_before(
                                "Module1",
                                xlcalc::SheetKind::Module,
                                Some(first),
                            )
                            .map_err(|_| failed())?;
                        wb.set_sheet_state(module, xlcalc::SheetState::VeryHidden)
                            .map_err(|_| failed())?;
                        let _ = wb.set_active_sheet(first);
                        let id = self.open_new(wb, name, String::new());
                        self.book_mut(id)?.saved = true;
                        id
                    }
                    7 => {
                        let name = format!("Book{}", self.next_book_number());
                        let mut wb = Workbook::empty();
                        wb.add_legacy_sheet_before("Dialog1", xlcalc::SheetKind::Dialog, None)
                            .map_err(|_| failed())?;
                        let id = self.open_new(wb, name, String::new());
                        self.book_mut(id)?.saved = true;
                        id
                    }
                    -4167 | 1 => {
                        let c = self.template_counts.entry("\u{0}sheet".into()).or_insert(0);
                        *c += 1;
                        let name = format!("Sheet{c}");
                        let id = self.open_new(Workbook::new(), name, String::new());
                        self.book_mut(id)?.saved = true;
                        id
                    }
                    -4109 | 2 => {
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
                    3 | 4 => {
                        let c = self.template_counts.entry("\u{0}macro".into()).or_insert(0);
                        *c += 1;
                        let name = format!("Macro{c}");
                        let mut wb = Workbook::empty();
                        wb.add_macro_sheet_before("Macro1", n == 4, None)
                            .map_err(|_| failed())?;
                        let id = self.open_new(wb, name, String::new());
                        let book = self.book_mut(id)?;
                        book.saved = true;
                        book.next_macro = if n == 4 { (1, 2) } else { (2, 1) };
                        id
                    }
                    _ => return Err(failed()),
                }
            }
        };
        Ok(self.alloc(Object::Workbook(id)))
    }

    pub(crate) fn open_operands(&mut self, a: &Args) -> Result<Operands, OmError> {
        self.member_operands("Workbooks", 0x783, a)
            .map_err(|_| open_failed())
    }

    pub(crate) fn workbooks_open(
        &mut self,
        operands: Operands,
        lcid: u32,
    ) -> Result<(Variant, Option<i32>), OmError> {
        let empty = Workbook::empty();
        let reader = Reader { wb: &empty, lcid };
        let args =
            open_args(operands, reader, &mut self.no_delimiter).map_err(|_| open_failed())?;
        let file = args.file;
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
            let (id, hidden) = (b.id, b.is_addin);
            if !hidden {
                self.active = Some(id);
            }
            return Ok((self.alloc(Object::Workbook(id)), args.update_links));
        }
        if args.password {
            return Err(OmError::unsupported("Workbooks.Open with a password"));
        }
        let active_before = self.active;
        let bytes = self.fs.read(&full).map_err(|_| not_found(&file))?;
        let name = file_name(&full);
        let id = if content_of(&bytes) == Content::Text {
            if extension(&name).is_some_and(|e| WORKBOOK_EXTENSIONS.contains(&e.as_str())) {
                return Err(format_not_valid(&name));
            }

            if matches!(args.origin, Some(1 | 3)) && !bytes.is_ascii() {
                return Err(OmError::unsupported(
                    "Workbooks.Open of a non-ASCII text file in another code page",
                ));
            }

            let imp = if self.no_delimiter {
                xlcalc_xlsx::import_csv_unsplit(&bytes, &name, args.local)
            } else {
                xlcalc_xlsx::import_csv(&bytes, &name, args.local)
            };
            let mut wb = imp.workbook;
            if let Some(active) = self.active.and_then(|id| self.engine(id)) {
                share_settings(active, &mut wb);
            }
            let id = self.push_book(wb, imp.name, full);
            self.vifmt.set(imp.last_format);
            self.book_mut(id)?.file_format = imp.file_format;
            id
        } else {
            self.open_bytes(bytes, &name, full, name.clone(), true)?
        };

        if extension(&name).is_some_and(|e| matches!(e.as_str(), "xlam" | "xla")) {
            self.book_mut(id)?.is_addin = true;
            self.active = active_before;
        }
        let b = self.book_mut(id)?;
        b.read_only = args.read_only;
        b.saved = true;
        self.update_links_on_open(id, args.update_links);
        Ok((self.alloc(Object::Workbook(id)), args.update_links))
    }

    pub(crate) fn workbooks_open_text(&mut self, a: &Args) -> Result<Variant, OmError> {
        let failed = || OmError::app_defined_with("OpenText method of Workbooks class failed");
        let operands = self
            .member_operands("Workbooks", 0x784, a)
            .map_err(|_| failed())?;
        let (_, mut done) = self.each_call(a.lcid, operands, |app, o| {
            let empty = Workbook::empty();
            let reader = Reader {
                wb: &empty,
                lcid: a.lcid,
            };
            let file = open_text_args(o, reader).map_err(|_| failed())?;
            let exists = app.fs.full_path(&file).is_some_and(|p| app.fs.exists(&p));
            if !file.is_empty() && exists {
                return Err(OmError::unsupported("Workbooks.OpenText of a file"));
            }
            Err(not_found(&file))
        })?;
        Ok(done.pop().unwrap_or(Variant::Empty))
    }

    pub(crate) fn open_bytes(
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
                || (content == Content::Xls
                    && matches!(ext.as_str(), "xlsx" | "xlsm" | "xlsb" | "xltx" | "xltm"))
                || (content == Content::Xml && ext == "xlsb"))
        {
            return Err(format_not_valid(file));
        }
        let imp = xlcalc_xlsx::import_bytes(bytes).map_err(|e| OmError::app_defined_with(&e))?;
        let ext_links = imp.ext_links.clone();
        let mut wb = imp.workbook;
        if let Some(active) = self.active.and_then(|id| self.engine(id)) {
            share_settings(active, &mut wb);
        } else {
            self.calculate_before_save = wb.settings().calc_on_save;
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
        self.protection_from_file(id, &imp.package);
        let b = self.book_mut(id)?;
        b.warnings = warnings;
        b.compat = content == Content::Xls;
        b.file_format = match (content, ext.as_str()) {
            (Content::Xlsb, _) => 50,
            (Content::Xls, _) => 56,
            (_, "xltx") => 54,
            (_, "xltm") => 53,
            _ if !b.wb.date_compat() => 61,
            _ if has_project => 52,
            (_, "xlsm") => 52,
            _ => 51,
        };
        for sh in b.wb.sheets().map(|s| s.id).collect::<Vec<_>>() {
            let list = xlcalc_xlsx::read_controls(&imp.package, sh);
            if !list.is_empty() {
                let mut sc = crate::controls::from_file(list);
                for c in &mut sc.list {
                    b.wb.legacy_shape_loaded(sh, c.spid);
                    crate::controls::anchor_loaded(&mut b.wb, sh, c);
                    crate::controls::bind(&mut b.wb, sh, c);
                }
                b.controls.insert(sh, sc);
            }
        }
        b.ro_recommended = imp
            .package
            .part("xl/workbook.xml")
            .is_some_and(|x| String::from_utf8_lossy(x).contains("readOnlyRecommended=\"1\""));
        b.package = Some(imp.package);
        self.retrack_book(id);
        if let Some(package) = self.book_mut(id)?.package.take() {
            self.load_ui(id, &package);
            self.book_mut(id)?.package = Some(package);
        }
        let dir = match folder(&self.book(id)?.path) {
            d if d.is_empty() => d,
            d => format!("{d}\\"),
        };
        self.resolve_file_links(id, &dir, &ext_links);
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
            Writes::Package(_) | Writes::Xlsb | Writes::Xls => {
                let kind = match writes {
                    Writes::Package(kind) => kind,
                    _ if self.has_vb_project(b)? => FileKind::Xlsm,

                    _ if self.book(b)?.wb.sheets().any(|s| s.kind.is_macro()) => FileKind::Xlsm,
                    _ => FileKind::Xlsx,
                };
                let vba = match kind {
                    _ if self.has_vb_project(b)? => self.vba_write(b)?,
                    _ => None,
                };
                if writes == Writes::Xls {
                    self.titles_for_xls(b)?;
                }
                let controls = self.controls_for_save(b);
                let protection = self.protection_for_save(b);
                let ui = self.ui_for_save(b);
                let properties = self.properties_for_save(b);
                let bk = self.book(b)?;
                let opts = SaveOptions {
                    kind: Some(kind),
                    vba,
                    controls,
                    dir: xlcalc::split_path(file).0.to_string(),
                    read_only_recommended: bk.ro_recommended,
                    protection,
                    ui,
                    properties,
                    compat: bk.compat && matches!(writes, Writes::Package(_)),
                    ..SaveOptions::default()
                };
                let saved = xlcalc_xlsx::save(&bk.wb, bk.package.as_ref(), &opts)
                    .map_err(|e| OmError::app_defined_with(&e))?;
                let warnings = saved.warnings;
                self.book_mut(b)?.warnings.extend(warnings);
                if writes == Writes::Xls {
                    let bk = self.book(b)?;
                    let (bytes, more) =
                        xlcalc_xlsx::xlsx_to_xls_of(&saved.bytes, &bk.wb, bk.compat)
                            .map_err(|e| OmError::app_defined_with(&e))?;
                    self.book_mut(b)?.warnings.extend(more);
                    return Ok(bytes);
                }
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

        self.retrack_book(b);

        let settings = self.book(b)?.wb.settings();
        if self.calculate_before_save
            && (settings.mode != xlcalc::CalcMode::Automatic
                || settings.except_tables
                || self.book(b)?.wb.save_calculates())
        {
            self.book_mut(b)?.wb.recalc_before_save();
            self.sync_links();
        }

        let on = self.calculate_before_save;
        if self.book(b)?.wb.settings().calc_on_save != on {
            self.book_mut(b)?.wb.set_calc_on_save(on);
        }

        let dir = self.fs.save_folder(&full);
        let expanded: std::collections::HashMap<String, String> = self
            .book(b)?
            .wb
            .hyperlink_addresses()
            .into_iter()
            .map(|a| {
                let e = self.fs.expand(&a);
                (a, e)
            })
            .collect();
        let expand = |a: &str| expanded.get(a).cloned().unwrap_or_else(|| a.to_string());
        self.book_mut(b)?.wb.hyperlinks_save_relative(&dir, &expand);

        let compat_before = self.book(b)?.wb.date_compat();
        let compat_after = format != 61;
        let flip = !copy && compat_before != compat_after;
        if flip {
            self.book_mut(b)?.wb.set_date_compat(compat_after);
        }

        let written = match self.file_bytes(b, format, &name, !copy) {
            Ok(bytes) => self
                .fs
                .write(&full, &bytes)
                .map_err(|_| cannot_access(&full)),
            Err(e) => Err(e),
        };
        if let Err(e) = written {
            if flip {
                self.book_mut(b)?.wb.set_date_compat(compat_before);
            }
            return Err(e);
        }
        if copy {
            return Ok(());
        }
        let csv = matches!(format_of(format), Some((_, Writes::Csv)));

        let bi = self.book_index(b);
        let follow: Vec<(usize, usize)> = (0..self.books.len())
            .filter(|&i| i != bi)
            .flat_map(|i| {
                let links = match &self.books[i].wb {
                    crate::app::BookWb::Own(w) => w.ext_links(),
                    _ => Vec::new(),
                };
                links
                    .into_iter()
                    .enumerate()
                    .filter(|(_, l)| !l.broken && self.link_source(l, i) == Some(bi))
                    .map(move |(li, _)| (i, li))
                    .collect::<Vec<_>>()
            })
            .collect();
        for (i, li) in follow {
            if let crate::app::BookWb::Own(w) = &mut self.books[i].wb {
                if w.ext_rename_link(li, &full) {
                    w.recalc();
                }
            }
        }
        let bk = self.book_mut(b)?;
        bk.wb.set_book_name(&name);
        bk.name = name;
        bk.path = full;
        bk.file_format = format;
        bk.read_only = false;
        bk.wb.recompute_used_range();

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
        let had_window = self.window_list().iter().any(|w| w.0 == b);
        self.books.retain(|x| x.id != b);
        self.wins.retain(|w| w.0 != b);
        self.cur_win.retain(|w| w.0 != b);
        if had_window {
            let top = self.window_list().first().copied();
            self.active = top.map(|w| w.0);
            if let Some(w) = top {
                self.set_current_window(w);
            }
        } else if self.active == Some(b) {
            self.active = self.window_order().first().copied();
        }
        Ok(Variant::Bool(true))
    }

    pub(crate) fn save_as_member(&mut self, b: BookId, a: &Args) -> Result<Variant, OmError> {
        self.book(b)?;

        let pw = a.string(2)?.is_some_and(|p| !p.is_empty());
        let wp = a.string(3)?.is_some_and(|p| !p.is_empty());
        let file = a.string(0)?.unwrap_or_default();
        let format = a.i32(1)?;

        if let Some(ro) = a.bool(4)? {
            self.book_mut(b)?.ro_recommended = ro;
        }
        self.save_book_as(b, &file, format, false)?;
        if pw {
            self.book_mut(b)?.props.password = true;
        }
        if wp {
            self.book_mut(b)?.write_reserved = true;
        }
        Ok(Variant::Bool(true))
    }

    pub(crate) fn has_vba_content(&self, b: BookId) -> Result<bool, OmError> {
        let bk = self.book(b)?;
        Ok(!bk.vba.modules.is_empty()
            || bk.ide.opened
            || bk
                .package
                .as_ref()
                .is_some_and(|p| p.part("/xl/vbaProject.bin").is_some()))
    }

    pub(crate) fn has_vb_project(&self, b: BookId) -> Result<bool, OmError> {
        let bk = self.book(b)?;
        Ok(!bk.vba.modules.is_empty()
            || bk.wb.sheets().any(|s| s.kind == xlcalc::SheetKind::Module)
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
