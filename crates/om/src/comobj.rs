use xlcalc_variant::{ObjectId, SafeArray, VarType, Variant};
use xlcalc_vba::{InvokeKind, VbaError};

const TAG: u64 = 1 << 62;

pub(crate) fn com_id(i: usize) -> ObjectId {
    ObjectId(TAG | i as u64)
}

const NO_KEY: i32 = 32811;

#[derive(Clone, Debug, PartialEq)]
enum Key {
    Text(String),
    Int(i64),
    Float(u64),
    Bool(bool),
}

#[derive(Debug, Default)]
pub(crate) struct Dict {
    mode: i32,
    entries: Vec<(Variant, Variant)>,
}

#[derive(Debug)]
pub(crate) enum ComObj {
    Dictionary(Dict),
    FileSystem,

    File(String),
    Folder(String),

    Drive(char),

    Files(String),
    SubFolders(String),
    TextStream(Box<crate::fsobj::Stream>),

    Bridged(u64),

    RegExp(crate::regexobj::RegExp),
    Matches(std::sync::Arc<crate::regexobj::MatchSet>),
    Match(std::sync::Arc<crate::regexobj::MatchSet>, usize),
    SubMatches(std::sync::Arc<crate::regexobj::MatchSet>, usize),

    AdoStream(Box<crate::adodb::Stream>),
    AdoRecordset(Box<crate::adors::Recordset>),
    AdoFields(usize),
    AdoField(usize, usize, crate::adors::FieldDef),

    Xml(usize),
    XmlList(crate::xmlcom::XList),
    XmlErr(Box<crate::xmldom::ParseErr>),
    XmlImpl,
    XmlSchemas,
}

pub(crate) fn is_com(id: ObjectId) -> bool {
    id.0 & TAG != 0 && id.0 >> 63 == 0
}

pub(crate) fn index(id: ObjectId) -> usize {
    (id.0 & !TAG) as usize
}

impl Dict {
    fn number(x: f64) -> Key {
        if x.fract() == 0.0 && x.abs() < 9e15 {
            Key::Int(x as i64)
        } else {
            Key::Float(x.to_bits())
        }
    }

    fn key(&self, v: &Variant) -> Result<Key, VbaError> {
        Ok(match v.clone().canon() {
            Variant::Bstr(s) if self.mode != 0 => Key::Text(s.to_lowercase()),
            Variant::Bstr(s) => Key::Text(s),
            Variant::Bool(b) => Key::Bool(b),
            Variant::I2(n) => Key::Int(n.into()),
            Variant::I4(n) => Key::Int(n.into()),
            Variant::I8(n) => Key::Int(n),
            Variant::Ui1(n) => Key::Int(n.into()),
            Variant::R4(x) => Self::number(f64::from(x)),
            Variant::R8(x) | Variant::Date(x) => Self::number(x),
            Variant::Empty => Key::Text(String::new()),
            _ => return Err(VbaError::new(5)),
        })
    }

    fn find(&self, k: &Variant) -> Result<Option<usize>, VbaError> {
        let want = self.key(k)?;
        for (i, (e, _)) in self.entries.iter().enumerate() {
            if self.key(e)? == want {
                return Ok(Some(i));
            }
        }
        Ok(None)
    }

    fn array(items: Vec<Variant>) -> Variant {
        Variant::Array(Box::new(SafeArray {
            elem: VarType::Variant,
            bounds: vec![(0, items.len() as u32)],
            data: items,
        }))
    }

    fn compare_mode(&mut self, v: &Variant) -> Result<Variant, VbaError> {
        if !self.entries.is_empty() {
            return Err(VbaError::new(5));
        }
        let n = match v {
            Variant::I2(n) => i32::from(*n),
            Variant::I4(n) => *n,
            _ => return Err(VbaError::new(5)),
        };
        if !(0..=2).contains(&n) {
            return Err(VbaError::new(5));
        }
        self.mode = n;
        Ok(Variant::Empty)
    }

    fn call(
        &mut self,
        name: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let name = if name.is_empty() { "item" } else { name }.to_ascii_lowercase();
        let put = matches!(kind, InvokeKind::Put | InvokeKind::PutRef);
        let get = kind == InvokeKind::Get;
        match (name.as_str(), args.len()) {
            ("count", 0) if get => Ok(Variant::I4(self.entries.len() as i32)),
            ("comparemode", 0) if get => Ok(Variant::I4(self.mode)),
            ("comparemode", 1) if put => self.compare_mode(&args[0]),
            ("exists", 1) if get => Ok(Variant::Bool(self.find(&args[0])?.is_some())),
            ("add", 2) if get => {
                if self.find(&args[0])?.is_some() {
                    return Err(VbaError::new(457));
                }
                self.entries
                    .push((args[0].clone().canon(), args[1].clone()));
                Ok(Variant::Empty)
            }
            ("item", 1) if get => match self.find(&args[0])? {
                Some(i) => Ok(self.entries[i].1.clone()),
                None => {
                    self.entries.push((args[0].clone().canon(), Variant::Empty));
                    Ok(Variant::Empty)
                }
            },
            ("item", 2) if put => {
                match self.find(&args[0])? {
                    Some(i) => self.entries[i].1 = args[1].clone(),
                    None => self
                        .entries
                        .push((args[0].clone().canon(), args[1].clone())),
                }
                Ok(Variant::Empty)
            }
            ("key", 2) if put => {
                let i = self.find(&args[0])?.ok_or_else(|| VbaError::new(NO_KEY))?;
                if self.find(&args[1])?.is_some() {
                    return Err(VbaError::new(457));
                }
                self.entries[i].0 = args[1].clone().canon();
                Ok(Variant::Empty)
            }
            ("remove", 1) if get => {
                let i = self.find(&args[0])?.ok_or_else(|| VbaError::new(NO_KEY))?;
                self.entries.remove(i);
                Ok(Variant::Empty)
            }
            ("removeall", 0) if get => {
                self.entries.clear();
                Ok(Variant::Empty)
            }
            ("keys", 0) if get => Ok(Dict::array(
                self.entries.iter().map(|e| e.0.clone()).collect(),
            )),
            ("items", 0) if get => Ok(Dict::array(
                self.entries.iter().map(|e| e.1.clone()).collect(),
            )),
            (
                "count" | "comparemode" | "exists" | "add" | "item" | "key" | "remove"
                | "removeall" | "keys" | "items",
                _,
            ) => Err(VbaError::new(450)),
            _ => Err(VbaError::new(438)),
        }
    }
}

impl crate::Application<'_> {
    pub(crate) fn create_com_object(&mut self, progid: &str) -> Result<ObjectId, VbaError> {
        let obj = match progid.to_ascii_lowercase().as_str() {
            "scripting.dictionary" => ComObj::Dictionary(Dict::default()),
            "scripting.filesystemobject" => ComObj::FileSystem,
            "vbscript.regexp" => ComObj::RegExp(crate::regexobj::RegExp::default()),
            "adodb.stream" => ComObj::AdoStream(Box::default()),
            "adodb.recordset" => ComObj::AdoRecordset(Box::default()),
            "msxml2.domdocument.6.0" | "msxml2.freethreadeddomdocument.6.0" => {
                self.create_xml_doc(false)
            }
            "msxml2.domdocument"
            | "msxml2.domdocument.3.0"
            | "msxml2.freethreadeddomdocument"
            | "msxml2.freethreadeddomdocument.3.0"
            | "msxml.domdocument"
            | "microsoft.xmldom" => self.create_xml_doc(true),
            _ => return self.bridge_create(progid),
        };
        self.com.push(obj);
        Ok(ObjectId(TAG | (self.com.len() as u64 - 1)))
    }

    pub(crate) fn get_com_object(
        path: Option<&str>,
        class: Option<&str>,
    ) -> Result<ObjectId, VbaError> {
        Err(VbaError::new(if path.is_some() && class.is_none() {
            432
        } else {
            429
        }))
    }

    pub(crate) fn com_invoke(
        &mut self,
        id: ObjectId,
        member: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let i = index(id);
        match self.com.get_mut(i) {
            Some(ComObj::Dictionary(d)) => d.call(member, kind, args),
            Some(ComObj::FileSystem) => self.fso_call(member, kind, args),
            Some(ComObj::TextStream(s)) => s.call(member, kind, args),
            Some(ComObj::Bridged(_)) => self.bridged_invoke(id, member, kind, args),
            Some(ComObj::RegExp(_)) => self.regexp_call(i, member, kind, args),
            Some(ComObj::AdoStream(_)) => self.stream_call(i, member, kind, args),
            Some(ComObj::AdoRecordset(_)) => self.recordset_call(i, member, kind, args),
            Some(ComObj::AdoFields(rs)) => {
                let rs = *rs;
                self.fields_obj_call(rs, member, kind, args)
            }
            Some(ComObj::AdoField(rs, k, def)) => {
                let (rs, k, def) = (*rs, *k, def.clone());
                self.field_call(rs, k, &def, member, kind, args)
            }
            Some(ComObj::Xml(n)) => {
                let n = *n;
                self.xml_call(n, member, kind, args)
            }
            Some(ComObj::XmlList(_)) => {
                let put = matches!(kind, InvokeKind::Put | InvokeKind::PutRef);
                self.xlist_call(i, member, put, args)
            }
            Some(ComObj::XmlErr(e)) => {
                let e = e.clone();
                let put = matches!(kind, InvokeKind::Put | InvokeKind::PutRef);
                self.xerr_call(&e, member, put)
            }
            Some(ComObj::XmlImpl) => self.ximpl_call(member, args),
            Some(ComObj::XmlSchemas) => self.xschemas_call(member, args),
            Some(ComObj::Matches(set)) => {
                let set = set.clone();
                self.matches_call(&set, member, args)
            }
            Some(ComObj::Match(set, n)) => {
                let (set, n) = (set.clone(), *n);
                self.match_call(&set, n, member, args)
            }
            Some(ComObj::SubMatches(set, n)) => {
                crate::regexobj::submatches_call(set, *n, member, args)
            }
            Some(ComObj::File(p)) => {
                let p = p.clone();
                self.file_call(i, &p, member, kind, args)
            }
            Some(ComObj::Folder(p)) => {
                let p = p.clone();
                self.folder_call(i, &p, member, kind, args)
            }
            Some(ComObj::Drive(l)) => {
                let l = *l;
                self.drive_call(l, member, kind)
            }
            Some(ComObj::Files(p)) => {
                let p = p.clone();
                self.collection_call(&p, true, member, args)
            }
            Some(ComObj::SubFolders(p)) => {
                let p = p.clone();
                self.collection_call(&p, false, member, args)
            }
            None => Err(VbaError::new(424)),
        }
    }

    pub(crate) fn com_type_name(&self, id: ObjectId) -> &'static str {
        match self.com.get(index(id)) {
            Some(ComObj::Dictionary(_)) => "Dictionary",
            Some(ComObj::FileSystem) => "FileSystemObject",
            Some(ComObj::File(_)) => "File",
            Some(ComObj::Folder(_)) => "Folder",
            Some(ComObj::Drive(_)) => "Drive",
            Some(ComObj::Files(_)) => "Files",
            Some(ComObj::SubFolders(_)) => "Folders",
            Some(ComObj::TextStream(_)) => "TextStream",
            Some(ComObj::Bridged(_)) => "Object",
            Some(ComObj::RegExp(_)) => "RegExp",
            Some(ComObj::AdoStream(_)) => "Stream",
            Some(ComObj::AdoRecordset(_)) => "Recordset",
            Some(ComObj::AdoFields(_)) => "Fields",
            Some(ComObj::AdoField(..)) => "Field",
            Some(ComObj::Xml(n)) => self.xml_type_name(*n),
            Some(ComObj::XmlList(_)) => "IXMLDOMNodeList",
            Some(ComObj::XmlErr(_)) => "IXMLDOMParseError2",
            Some(ComObj::XmlImpl) => "IXMLDOMImplementation",
            Some(ComObj::XmlSchemas) => "IXMLDOMSchemaCollection2",
            Some(ComObj::Matches(_)) => "MatchCollection",
            Some(ComObj::Match(..)) => "Match",
            Some(ComObj::SubMatches(..)) => "SubMatches",
            None => "Object",
        }
    }

    pub(crate) fn com_enumerate(&mut self, id: ObjectId) -> Result<Vec<Variant>, VbaError> {
        match self.com.get(index(id)) {
            Some(ComObj::Dictionary(d)) => Ok(d.entries.iter().map(|e| e.0.clone()).collect()),
            Some(ComObj::Files(p)) => {
                let p = p.clone();
                self.fs_collection_items(&p, true)
            }
            Some(ComObj::SubFolders(p)) => {
                let p = p.clone();
                self.fs_collection_items(&p, false)
            }
            Some(ComObj::Matches(set)) => {
                let set = set.clone();
                Ok((0..set.len())
                    .map(|n| self.new_com(ComObj::Match(set.clone(), n)))
                    .collect())
            }
            Some(ComObj::Xml(n)) => {
                let n = *n;
                Ok(self.xml_enumerate(n))
            }
            Some(ComObj::XmlList(_)) => Ok(self.xlist_enumerate(index(id))),
            Some(ComObj::AdoFields(rs)) => {
                let rs = *rs;
                self.ado_fields_items(rs)
            }

            Some(ComObj::AdoRecordset(_)) => Ok(vec![Variant::Dispatch(Some(id))]),
            Some(ComObj::SubMatches(set, n)) => Ok((0..set.groups(*n))
                .map(|g| crate::regexobj::submatch(set, *n, g))
                .collect()),
            Some(
                ComObj::FileSystem
                | ComObj::RegExp(_)
                | ComObj::Match(..)
                | ComObj::Bridged(_)
                | ComObj::File(_)
                | ComObj::Folder(_)
                | ComObj::Drive(_)
                | ComObj::TextStream(_)
                | ComObj::AdoStream(_)
                | ComObj::AdoField(..)
                | ComObj::XmlErr(_)
                | ComObj::XmlImpl
                | ComObj::XmlSchemas,
            )
            | None => Err(VbaError::new(438)),
        }
    }
}

const SYSTEM_LIBS: &[&str] = &[
    "kernel32", "user32", "msvcrt", "advapi32", "shell32", "gdi32", "ole32", "winmm",
];

impl crate::Application<'_> {
    pub(crate) fn dll_call(
        &mut self,
        lib: &str,
        name: &str,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let key = lib.to_ascii_lowercase();
        let key = key.strip_suffix(".dll").unwrap_or(&key);
        if !SYSTEM_LIBS.contains(&key) {
            return Err(VbaError {
                number: 53,
                description: format!("File not found: {lib}"),
            });
        }
        let num = |i: usize| match args.get(i) {
            Some(Variant::I4(n)) => Ok(*n),
            Some(Variant::I2(n)) => Ok(i32::from(*n)),
            Some(Variant::Ui1(n)) => Ok(i32::from(*n)),
            _ => Err(VbaError::new(13)),
        };
        match (key, name.to_ascii_lowercase().as_str()) {
            ("kernel32", "sleep") => {
                self.ticks = self.ticks.wrapping_add(num(0)?);
                Ok(Variant::Empty)
            }
            ("kernel32", "gettickcount") => Ok(Variant::I4(self.ticks)),
            ("kernel32", "getcurrentprocessid") => Ok(Variant::I4(4242)),
            ("msvcrt", "abs") => Ok(Variant::I4(num(0)?.wrapping_abs())),
            ("kernel32", "getacp") => Ok(Variant::I4(1252)),
            ("kernel32", "muldiv") => {
                let (a, b, c) = (
                    i128::from(num(0)?),
                    i128::from(num(1)?),
                    i128::from(num(2)?),
                );
                if c == 0 {
                    return Ok(Variant::I4(-1));
                }
                let n = a * b;
                let q = (n.abs() + c.abs() / 2) / c.abs();
                let q = if (n < 0) != (c < 0) { -q } else { q };
                Ok(Variant::I4(i32::try_from(q).unwrap_or(-1)))
            }
            ("kernel32", "lstrlena") => Ok(Variant::I4(ansi_arg(args.first())?.len() as i32)),
            ("kernel32", "lstrcmpa" | "lstrcmpia") => {
                let to_str = |b: Vec<u8>| -> String { b.into_iter().map(ansi_char).collect() };
                let (x, y) = (
                    to_str(ansi_arg(args.first())?),
                    to_str(ansi_arg(args.get(1))?),
                );
                let o = if name.eq_ignore_ascii_case("lstrcmpia") {
                    xlcalc::collate_compare(&x, &y)
                } else {
                    xlcalc::collate_compare_match_case(&x, &y)
                };
                Ok(Variant::I4(o as i32))
            }
            ("user32", f @ ("ischaralphaa" | "ischaruppera" | "ischarlowera")) => {
                let c = ansi_char(num(0)? as u8);
                let r = match f {
                    "ischaralphaa" => c.is_alphabetic(),
                    "ischaruppera" => c.is_uppercase(),
                    _ => c.is_lowercase(),
                };
                Ok(Variant::I4(i32::from(r)))
            }
            _ => Err(VbaError {
                number: 453,
                description: format!("Can't find DLL entry point {name} in {lib}"),
            }),
        }
    }
}

fn ansi_arg(v: Option<&Variant>) -> Result<Vec<u8>, VbaError> {
    match v {
        Some(Variant::Bstr(s)) => {
            let mut b = xlcalc_vba::rt::ansi_bytes(s);
            if let Some(n) = b.iter().position(|&c| c == 0) {
                b.truncate(n);
            }
            Ok(b)
        }
        Some(Variant::BstrNull | Variant::Empty) => Ok(Vec::new()),
        Some(Variant::Null) => Err(VbaError::new(94)),
        Some(Variant::I2(n)) => Ok(n.to_string().into_bytes()),
        Some(Variant::I4(n)) => Ok(n.to_string().into_bytes()),
        _ => Err(VbaError::new(13)),
    }
}

fn ansi_char(b: u8) -> char {
    const HIGH: [u16; 32] = [
        0x20AC, 0x81, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160,
        0x2039, 0x0152, 0x8D, 0x017D, 0x8F, 0x90, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013,
        0x2014, 0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x9D, 0x017E, 0x0178,
    ];
    match b {
        0x80..=0x9F => char::from_u32(u32::from(HIGH[usize::from(b) - 0x80])).unwrap_or('?'),
        _ => char::from(b),
    }
}
