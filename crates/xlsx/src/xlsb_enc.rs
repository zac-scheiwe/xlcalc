use crate::xlsb_ftab::ftab;
use std::collections::HashMap;
use std::sync::OnceLock;
use xlcalc::parser::ast::{ArrayItem, BinOp, Coord, Expr, RefKind, SheetSpec, UnaryOp};

const MAX_ROW: u32 = 0xF_FFFF;
const MAX_COL: u32 = 0x3FFF;

pub(crate) struct NameEnt {
    pub name: String,
    pub scope: Option<u32>,
    pub flags: u32,
    pub rgce: Vec<u8>,
    pub rgcb: Vec<u8>,
}

pub(crate) struct TableEnt {
    pub id: u32,

    pub names: [String; 2],
    pub columns: Vec<String>,

    pub sheet: u32,

    pub area: Option<TableArea>,
}

#[derive(Clone, Copy)]
pub(crate) struct TableArea {
    pub sheet: u32,
    pub r0: u32,
    pub c0: u32,
    pub r1: u32,
    pub header: bool,
    pub totals: bool,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct ExtBookEnt {
    pub sheets: Vec<String>,
    pub names: Vec<String>,
}

#[derive(Default)]
pub(crate) struct State {
    pub xti: Vec<(u32, i32, i32)>,
    pub names: Vec<NameEnt>,

    pub ext: Vec<ExtBookEnt>,

    pub legacy: bool,

    pub row: u32,

    pub addins: Vec<String>,
}

pub(crate) const ADDIN_BOOK: u32 = u32::MAX;

const LAST_LEGACY_FUNC: u16 = 367;

const ADD_IN_FUNCTIONS: &[&str] = &[
    "ACCRINT",
    "ACCRINTM",
    "AMORDEGRC",
    "AMORLINC",
    "BESSELI",
    "BESSELJ",
    "BESSELK",
    "BESSELY",
    "BIN2DEC",
    "BIN2HEX",
    "BIN2OCT",
    "COMPLEX",
    "CONVERT",
    "COUPDAYBS",
    "COUPDAYS",
    "COUPDAYSNC",
    "COUPNCD",
    "COUPNUM",
    "COUPPCD",
    "CUMIPMT",
    "CUMPRINC",
    "DEC2BIN",
    "DEC2HEX",
    "DEC2OCT",
    "DELTA",
    "DISC",
    "DOLLARDE",
    "DOLLARFR",
    "DURATION",
    "EDATE",
    "EFFECT",
    "EOMONTH",
    "ERF",
    "ERFC",
    "FACTDOUBLE",
    "FVSCHEDULE",
    "GCD",
    "GESTEP",
    "HEX2BIN",
    "HEX2DEC",
    "HEX2OCT",
    "IMABS",
    "IMAGINARY",
    "IMARGUMENT",
    "IMCONJUGATE",
    "IMCOS",
    "IMDIV",
    "IMEXP",
    "IMLN",
    "IMLOG10",
    "IMLOG2",
    "IMPOWER",
    "IMPRODUCT",
    "IMREAL",
    "IMSIN",
    "IMSQRT",
    "IMSUB",
    "IMSUM",
    "INTRATE",
    "ISEVEN",
    "ISODD",
    "LCM",
    "MDURATION",
    "MROUND",
    "MULTINOMIAL",
    "NETWORKDAYS",
    "NOMINAL",
    "OCT2BIN",
    "OCT2DEC",
    "OCT2HEX",
    "ODDFPRICE",
    "ODDFYIELD",
    "ODDLPRICE",
    "ODDLYIELD",
    "PRICE",
    "PRICEDISC",
    "PRICEMAT",
    "QUOTIENT",
    "RANDBETWEEN",
    "RECEIVED",
    "SERIESSUM",
    "SQRTPI",
    "TBILLEQ",
    "TBILLPRICE",
    "TBILLYIELD",
    "WEEKNUM",
    "WORKDAY",
    "XIRR",
    "XNPV",
    "YEARFRAC",
    "YIELD",
    "YIELDDISC",
    "YIELDMAT",
    "IMSINH",
    "IMCOSH",
    "IMTAN",
];

impl State {
    fn xti_index(&mut self, first: i32, last: i32) -> u16 {
        self.xti_book(0, first, last)
    }

    fn xti_book(&mut self, book: u32, first: i32, last: i32) -> u16 {
        let key = (book, first, last);
        match self.xti.iter().position(|x| *x == key) {
            Some(i) => i as u16,
            None => {
                self.xti.push(key);
                (self.xti.len() - 1) as u16
            }
        }
    }

    fn future_name(&mut self, name: &str) -> u32 {
        if let Some(i) = self.names.iter().position(|n| n.name == name) {
            return i as u32 + 1;
        }
        self.names.push(NameEnt {
            name: name.to_string(),
            scope: None,
            flags: 0x0002_000B,
            rgce: vec![0x1C, 0x1D],
            rgcb: Vec::new(),
        });
        self.names.len() as u32
    }

    fn param_name(&mut self, name: &str) -> u32 {
        let full = format!("_xlpm.{}", name.strip_prefix("_xlpm.").unwrap_or(name));
        if let Some(i) = self
            .names
            .iter()
            .position(|n| n.name.eq_ignore_ascii_case(&full))
        {
            return i as u32 + 1;
        }
        self.names.push(NameEnt {
            name: full,
            scope: None,
            flags: 0x0002_0019,
            rgce: vec![0x1C, 0x1D],
            rgcb: Vec::new(),
        });
        self.names.len() as u32
    }

    fn find_name(&self, name: &str, scope: Option<u32>) -> Option<u32> {
        self.names
            .iter()
            .position(|n| n.scope == scope && n.name.eq_ignore_ascii_case(name))
            .map(|i| i as u32 + 1)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Cls {
    Value,
    Ref,
    Arr,
}

impl Cls {
    fn bits(self) -> u8 {
        match self {
            Cls::Ref => 0x20,
            Cls::Value => 0x40,
            Cls::Arr => 0x60,
        }
    }
}

pub(crate) struct Rgce {
    pub rgce: Vec<u8>,
    pub rgcb: Vec<u8>,
}

impl Rgce {
    pub fn parsed(&self) -> Vec<u8> {
        let mut v = Vec::new();
        v.extend((self.rgce.len() as u32).to_le_bytes());
        v.extend(&self.rgce);
        v.extend((self.rgcb.len() as u32).to_le_bytes());
        v.extend(&self.rgcb);
        v
    }
}

fn func_ids() -> &'static HashMap<&'static str, u16> {
    static IDS: OnceLock<HashMap<&'static str, u16>> = OnceLock::new();
    IDS.get_or_init(|| {
        let mut m = HashMap::new();
        for id in 0..0x400u16 {
            if let Some((name, _)) = ftab(id) {
                m.entry(name).or_insert(id);
            }
        }
        m
    })
}

pub(crate) fn error_code(e: &str) -> Option<u8> {
    Some(match e {
        "#NULL!" => 0x00,
        "#DIV/0!" => 0x07,
        "#VALUE!" => 0x0F,
        "#REF!" => 0x17,
        "#NAME?" => 0x1D,
        "#NUM!" => 0x24,
        "#N/A" => 0x2A,
        "#GETTING_DATA" => 0x2B,
        "#SPILL!" => 0x2D,
        "#CONNECT!" => 0x2E,
        "#BLOCKED!" => 0x2F,
        "#UNKNOWN!" => 0x30,
        "#FIELD!" => 0x31,
        "#CALC!" => 0x32,
        "#BUSY!" => 0x33,
        _ => return None,
    })
}

pub(crate) fn error_byte(text: &str) -> Option<u8> {
    error_code(text)
}

struct Enc<'a> {
    sheets: &'a [String],
    tables: &'a [TableEnt],
    st: &'a mut State,
    cur: Option<u32>,
    out: Vec<u8>,
    ex: Vec<u8>,
    volatile: bool,

    lets: Vec<String>,
}

pub(crate) fn encode(
    sheets: &[String],
    tables: &[TableEnt],
    st: &mut State,
    cur: Option<u32>,
    src: &str,
    top: Cls,
) -> Result<Rgce, String> {
    let expr = xlcalc::parser::parse(src).map_err(|e| e.to_string())?;
    let mut e = Enc {
        sheets,
        tables,
        st,
        cur,
        out: Vec::new(),
        ex: Vec::new(),
        volatile: false,
        lets: Vec::new(),
    };
    e.emit(&expr, top)?;
    let mut rgce = Vec::new();
    if e.volatile {
        rgce.extend([0x19, 0x01, 0, 0]);
    }
    rgce.extend(e.out);
    Ok(Rgce { rgce, rgcb: e.ex })
}

fn word(rel_col: bool, rel_row: bool, col: u32) -> u16 {
    col as u16 | if rel_col { 0x4000 } else { 0 } | if rel_row { 0x8000 } else { 0 }
}

fn cmp_bin(op: BinOp) -> Option<u8> {
    Some(match op {
        BinOp::Add => 0x03,
        BinOp::Sub => 0x04,
        BinOp::Mul => 0x05,
        BinOp::Div => 0x06,
        BinOp::Pow => 0x07,
        BinOp::Concat => 0x08,
        BinOp::Lt => 0x09,
        BinOp::Le => 0x0A,
        BinOp::Eq => 0x0B,
        BinOp::Ge => 0x0C,
        BinOp::Gt => 0x0D,
        BinOp::Ne => 0x0E,
        BinOp::Intersect => 0x0F,
        BinOp::Union => 0x10,
        BinOp::Range => 0x11,
    })
}

impl Enc<'_> {
    fn sheet_index(&self, name: &str) -> Result<i32, String> {
        self.sheets
            .iter()
            .position(|s| s.eq_ignore_ascii_case(name))
            .map(|i| i as i32)
            .ok_or_else(|| format!("unknown sheet {name}"))
    }

    fn ext_book(&self, book: &str) -> Result<(u32, ExtBookEnt), String> {
        let n: usize = book
            .parse()
            .map_err(|_| "reference into another workbook".to_string())?;
        let ent = n
            .checked_sub(1)
            .and_then(|i| self.st.ext.get(i))
            .ok_or("no such linked workbook")?;
        Ok((n as u32, ent.clone()))
    }

    fn ixti(&mut self, spec: &SheetSpec) -> Result<u16, String> {
        match spec {
            SheetSpec::Single(s) => {
                let i = self.sheet_index(s)?;
                Ok(self.st.xti_index(i, i))
            }
            SheetSpec::Range(a, b) => {
                let (i, j) = (self.sheet_index(a)?, self.sheet_index(b)?);
                Ok(self.st.xti_index(i.min(j), i.max(j)))
            }
            SheetSpec::External { book, sheet } => {
                let (n, ent) = self.ext_book(book)?;
                match sheet {
                    Some(s) => {
                        let find = |name: &str| {
                            ent.sheets
                                .iter()
                                .position(|x| x.eq_ignore_ascii_case(name))
                                .map(|i| i as i32)
                                .ok_or_else(|| format!("unknown sheet {name} in linked workbook"))
                        };

                        let (i, j) = match s.split_once(':') {
                            Some((a, b)) => (find(a)?, find(b)?),
                            None => {
                                let i = find(s)?;
                                (i, i)
                            }
                        };
                        Ok(self.st.xti_book(n, i.min(j), i.max(j)))
                    }
                    None => Ok(self.st.xti_book(n, -2, -2)),
                }
            }
        }
    }

    fn sub(&mut self, e: &Expr, cls: Cls) -> Result<Vec<u8>, String> {
        let saved = std::mem::take(&mut self.out);
        let r = self.emit(e, cls);
        let got = std::mem::replace(&mut self.out, saved);
        r.map(|()| got)
    }

    fn coord_ref(&mut self, sheet: Option<&SheetSpec>, ptg: u8, cls: Cls) -> Result<(), String> {
        match sheet {
            None => self.out.push(ptg | cls.bits()),
            Some(s) => {
                let ix = self.ixti(s)?;
                self.out.push((ptg + 0x16) | cls.bits());
                self.out.extend(ix.to_le_bytes());
            }
        }
        Ok(())
    }

    fn emit(&mut self, e: &Expr, cls: Cls) -> Result<(), String> {
        let child = match cls {
            Cls::Arr => Cls::Arr,

            Cls::Ref if self.st.legacy => Cls::Ref,
            _ => Cls::Value,
        };
        match e {
            Expr::Number(n) => {
                if n.fract() == 0.0 && (0.0..=65535.0).contains(n) && !n.is_sign_negative() {
                    self.out.push(0x1E);
                    self.out.extend((*n as u16).to_le_bytes());
                } else {
                    self.out.push(0x1F);
                    self.out.extend(n.to_le_bytes());
                }
            }
            Expr::Text(t) => {
                let units: Vec<u16> = t.encode_utf16().collect();
                if units.len() > 255 {
                    return Err("text longer than 255 characters".into());
                }
                self.out.push(0x17);
                self.out.extend((units.len() as u16).to_le_bytes());
                for u in units {
                    self.out.extend(u.to_le_bytes());
                }
            }
            Expr::Bool(b) => self.out.extend([0x1D, u8::from(*b)]),
            Expr::Error(x) => {
                let c = error_code(x.as_str()).ok_or("error value")?;
                self.out.extend([0x1C, c]);
            }
            Expr::Array(rows) => self.array(rows, cls)?,
            Expr::Ref { sheet, kind } => self.reference(sheet.as_ref(), kind, cls)?,
            Expr::Name { sheet, name } => self.name(sheet.as_ref(), name, cls)?,
            Expr::StructRef { table, spec } => self.struct_ref(table.as_deref(), spec, cls)?,
            Expr::Func { name, args } => self.func(name, args, cls)?,
            Expr::Unary { op, expr } => {
                let tok = match op {
                    UnaryOp::Neg => 0x13,
                    UnaryOp::Plus => 0x12,
                    _ => return Err("implicit intersection operator".into()),
                };
                self.emit(expr, child)?;
                self.out.push(tok);
            }
            Expr::Percent(x) => {
                self.emit(x, child)?;
                self.out.push(0x14);
            }
            Expr::Binary { op, left, right } => {
                let c = if matches!(op, BinOp::Range | BinOp::Intersect | BinOp::Union) {
                    Cls::Ref
                } else {
                    child
                };
                self.emit(left, c)?;
                self.emit(right, c)?;
                self.out.push(cmp_bin(*op).ok_or("operator")?);
            }
            Expr::Paren(x) => {
                self.emit(x, cls)?;
                self.out.push(0x15);
            }
            Expr::Missing => self.out.push(0x16),
            Expr::Ws { expr, .. } => self.emit(expr, cls)?,
            Expr::Spill(_) => return Err("spill reference operator".into()),
            Expr::Field { .. } => return Err("data type field".into()),
            Expr::Call { callee, args } => {
                self.emit(callee, Cls::Value)?;
                for a in args {
                    self.emit(a, Cls::Value)?;
                }
                self.out.push(0x02 | Cls::Value.bits());
                self.out.push((args.len() + 1) as u8);
                self.out.extend(0x00FFu16.to_le_bytes());
            }
        }
        Ok(())
    }

    fn array(&mut self, rows: &[Vec<ArrayItem>], cls: Cls) -> Result<(), String> {
        let h = rows.len();
        let w = rows.first().map_or(0, Vec::len);
        if h == 0 || w == 0 || rows.iter().any(|r| r.len() != w) {
            return Err("ragged array constant".into());
        }
        self.out.push(cls.bits());
        self.out.extend(((w - 1) as u16).to_le_bytes());
        self.out.extend(((h - 1) as u16).to_le_bytes());
        self.out.extend([0u8; 10]);
        self.ex.extend((h as u32).to_le_bytes());
        self.ex.extend((w as u32).to_le_bytes());
        for it in rows.iter().flatten() {
            match it {
                ArrayItem::Number(n) => {
                    self.ex.push(0);
                    self.ex.extend(n.to_le_bytes());
                }
                ArrayItem::Text(t) => {
                    let units: Vec<u16> = t.encode_utf16().collect();
                    if units.len() > 255 {
                        return Err("array text longer than 255 characters".into());
                    }
                    self.ex.push(1);
                    self.ex.extend((units.len() as u16).to_le_bytes());
                    for u in units {
                        self.ex.extend(u.to_le_bytes());
                    }
                }
                ArrayItem::Bool(b) => self.ex.extend([2, u8::from(*b)]),
                ArrayItem::Error(x) => {
                    self.ex
                        .extend([4, error_code(x.as_str()).ok_or("error value")?, 0, 0, 0]);
                }
            }
        }
        Ok(())
    }

    fn reference(
        &mut self,
        sheet: Option<&SheetSpec>,
        kind: &RefKind,
        cls: Cls,
    ) -> Result<(), String> {
        let rel = |c: &Coord| !c.abs;
        match kind {
            RefKind::Cell { row, col } => {
                self.coord_ref(sheet, 0x04, cls)?;
                self.out.extend(row.index.to_le_bytes());
                self.out
                    .extend(word(rel(col), rel(row), col.index).to_le_bytes());
            }
            RefKind::Area { r0, c0, r1, c1 } => {
                self.coord_ref(sheet, 0x05, cls)?;
                self.out.extend(r0.index.to_le_bytes());
                self.out.extend(r1.index.to_le_bytes());
                self.out
                    .extend(word(rel(c0), rel(r0), c0.index).to_le_bytes());
                self.out
                    .extend(word(rel(c1), rel(r1), c1.index).to_le_bytes());
            }
            RefKind::Cols { c0, c1 } => {
                self.coord_ref(sheet, 0x05, cls)?;
                self.out.extend(0u32.to_le_bytes());
                self.out.extend(MAX_ROW.to_le_bytes());
                self.out
                    .extend(word(rel(c0), false, c0.index).to_le_bytes());
                self.out
                    .extend(word(rel(c1), false, c1.index).to_le_bytes());
            }
            RefKind::Rows { r0, r1 } => {
                self.coord_ref(sheet, 0x05, cls)?;
                self.out.extend(r0.index.to_le_bytes());
                self.out.extend(r1.index.to_le_bytes());
                self.out.extend(word(false, rel(r0), 0).to_le_bytes());
                self.out.extend(word(false, rel(r1), MAX_COL).to_le_bytes());
            }
        }
        Ok(())
    }

    fn name(&mut self, sheet: Option<&SheetSpec>, name: &str, cls: Cls) -> Result<(), String> {
        match sheet {
            None if self.lets.iter().any(|l| l.eq_ignore_ascii_case(name)) => {
                let idx = self.st.param_name(name);
                self.out.push(0x03 | cls.bits());
                self.out.extend(idx.to_le_bytes());
            }
            None => {
                let idx = self
                    .cur
                    .and_then(|c| self.st.find_name(name, Some(c)))
                    .or_else(|| self.st.find_name(name, None))
                    .ok_or_else(|| format!("name {name} is not defined"))?;
                self.out.push(0x03 | cls.bits());
                self.out.extend(idx.to_le_bytes());
            }
            Some(SheetSpec::Single(s)) => {
                let i = self.sheet_index(s)?;
                let (idx, ix) = match self.st.find_name(name, Some(i as u32)) {
                    Some(idx) => (idx, self.st.xti_index(i, i)),
                    None => (
                        self.st
                            .find_name(name, None)
                            .ok_or_else(|| format!("name {name} is not defined"))?,
                        self.st.xti_index(-2, -2),
                    ),
                };
                self.out.push(0x19 | cls.bits());
                self.out.extend(ix.to_le_bytes());
                self.out.extend(idx.to_le_bytes());
            }
            Some(SheetSpec::External { book, sheet: None }) => {
                let (n, ent) = self.ext_book(book)?;
                let idx = ent
                    .names
                    .iter()
                    .position(|x| x.eq_ignore_ascii_case(name))
                    .ok_or_else(|| format!("name {name} is not cached in the linked workbook"))?
                    as u32
                    + 1;
                let ix = self.st.xti_book(n, -2, -2);
                self.out.push(0x19 | cls.bits());
                self.out.extend(ix.to_le_bytes());
                self.out.extend(idx.to_le_bytes());
            }
            Some(_) => return Err("name of another workbook".into()),
        }
        Ok(())
    }

    fn struct_ref(&mut self, table: Option<&str>, spec: &str, cls: Cls) -> Result<(), String> {
        let tname = table.ok_or("structured reference without a table name")?;
        let t = self
            .tables
            .iter()
            .find(|t| t.names.iter().any(|n| n.eq_ignore_ascii_case(tname)))
            .ok_or_else(|| format!("unknown table {tname}"))?;
        let col_of = |n: &str| -> Result<u16, String> {
            t.columns
                .iter()
                .position(|c| c.eq_ignore_ascii_case(n))
                .map(|i| i as u16)
                .ok_or_else(|| format!("table {tname} has no column {n}"))
        };
        let mut rows = 0u16;
        let mut cols: Vec<u16> = Vec::new();
        let mut range = false;
        let mut this_row = false;
        for item in split_items(spec) {
            let item = item.trim();
            let (tag, a, rest) = if let Some(tag) = item.strip_prefix('#') {
                (Some(tag.to_string()), String::new(), "")
            } else if let Some(inner) = item.strip_prefix('[') {
                let (a, rest) = bracketed(inner)?;
                match a.strip_prefix('#') {
                    Some(t) => (Some(t.to_string()), String::new(), rest),
                    None => (None, a, rest),
                }
            } else {
                (None, String::new(), "")
            };
            if let Some(tag) = tag {
                match tag.to_ascii_lowercase().as_str() {
                    "all" => rows |= 1,
                    "headers" => rows |= 2,
                    "data" => rows |= 4,
                    "totals" => rows |= 8,
                    "this row" => {
                        rows |= 0x10;
                        this_row = true;
                    }
                    _ => return Err(format!("unknown table item #{tag}")),
                }
            } else if item.starts_with('[') {
                cols.push(col_of(&a)?);
                if let Some(r) = rest.strip_prefix(":[") {
                    let (b, _) = bracketed(r)?;
                    cols.push(col_of(&b)?);
                    range = true;
                }
            } else if !item.is_empty() {
                cols.push(col_of(&unescape_col(item))?);
            }
        }
        let first = cols.first().copied().unwrap_or(0);
        let last = cols.last().copied().unwrap_or(0);
        if self.st.legacy {
            let a = t
                .area
                .ok_or("structured reference without a table location")?;
            let (c0, c1) = if cols.is_empty() {
                (a.c0, a.c0 + t.columns.len().max(1) as u32 - 1)
            } else {
                (
                    a.c0 + u32::from(first.min(last)),
                    a.c0 + u32::from(first.max(last)),
                )
            };
            let hdr = u32::from(a.header);
            let (mut r0, mut r1) = (a.r0 + hdr, a.r1 - u32::from(a.totals));
            if this_row {
                (r0, r1) = (self.st.row, self.st.row);
            } else if rows & 1 != 0 {
                (r0, r1) = (a.r0, a.r1);
            } else if rows & 2 != 0 && rows & 4 == 0 && rows & 8 == 0 {
                (r0, r1) = (a.r0, a.r0);
            } else if rows & 8 != 0 && rows & 4 == 0 {
                (r0, r1) = (a.r1, a.r1);
            } else if rows & 2 != 0 {
                r0 = a.r0;
            } else if rows & 8 != 0 {
                r1 = a.r1;
            }

            if self.cur == Some(a.sheet) {
                if (r0, c0) == (r1, c1) {
                    self.out.push(0x04 | cls.bits());
                    self.out.extend(r0.to_le_bytes());
                    self.out.extend(word(true, true, c0).to_le_bytes());
                } else {
                    self.out.push(0x05 | cls.bits());
                    self.out.extend(r0.to_le_bytes());
                    self.out.extend(r1.to_le_bytes());
                    self.out.extend(word(true, true, c0).to_le_bytes());
                    self.out.extend(word(true, true, c1).to_le_bytes());
                }
                return Ok(());
            }
            let ix = self.st.xti_index(a.sheet as i32, a.sheet as i32);
            let rel_row = this_row;
            if (r0, c0) == (r1, c1) {
                self.out.push(0x1A | cls.bits());
                self.out.extend(ix.to_le_bytes());
                self.out.extend(r0.to_le_bytes());
                self.out.extend(word(false, rel_row, c0).to_le_bytes());
            } else {
                self.out.push(0x1B | cls.bits());
                self.out.extend(ix.to_le_bytes());
                self.out.extend(r0.to_le_bytes());
                self.out.extend(r1.to_le_bytes());
                self.out.extend(word(false, rel_row, c0).to_le_bytes());
                self.out.extend(word(false, rel_row, c1).to_le_bytes());
            }
            return Ok(());
        }
        let n_cols: u16 = match (cols.len(), range) {
            (0, _) => 0,
            (1, _) => 1,
            _ => 2,
        };
        let mut flags = n_cols | (rows << 2);
        if rows != 0 || n_cols == 2 {
            flags |= 0x0400;
        }
        if this_row {
            flags |= 0x0200;
        }
        let sheet = t.sheet as i32;
        let ix = self.st.xti_index(sheet, sheet);
        self.out.extend([0x18, 0x19]);
        self.out.extend(ix.to_le_bytes());
        self.out.extend(flags.to_le_bytes());
        self.out.extend(t.id.to_le_bytes());
        self.out.extend(first.to_le_bytes());
        self.out.extend(last.to_le_bytes());
        Ok(())
    }

    fn legacy_call(
        &mut self,
        upper: &str,
        args: &[Expr],
        cls: Cls,
        param: &dyn Fn(usize) -> Cls,
    ) -> Result<(), String> {
        if ADD_IN_FUNCTIONS.contains(&upper) {
            let ix = self.st.xti_book(ADDIN_BOOK, -2, -2);
            let n = match self.st.addins.iter().position(|a| a == upper) {
                Some(i) => i + 1,
                None => {
                    self.st.addins.push(upper.to_string());
                    self.st.addins.len()
                }
            };
            self.out.push(0x19 | Cls::Ref.bits());
            self.out.extend(ix.to_le_bytes());
            self.out.extend((n as u32).to_le_bytes());
        } else {
            let idx = self.st.future_name(&format!("_xlfn.{upper}"));
            self.out.push(0x03 | Cls::Ref.bits());
            self.out.extend(idx.to_le_bytes());
        }
        for (i, a) in args.iter().enumerate() {
            self.emit(a, param(i))?;
        }
        self.out.push(0x02 | cls.bits());
        self.out.push((args.len() + 1) as u8);
        self.out.extend(0x00FFu16.to_le_bytes());
        Ok(())
    }

    fn let_call(&mut self, args: &[Expr], cls: Cls) -> Result<(), String> {
        if args.len() < 3 || args.len().is_multiple_of(2) {
            return Err("LET needs name/value pairs and a result".into());
        }
        let depth = self.lets.len();
        let idx = self.st.future_name("_xlfn.LET");
        self.out.push(0x03 | Cls::Ref.bits());
        self.out.extend(idx.to_le_bytes());
        for pair in args[..args.len() - 1].chunks(2) {
            let Expr::Name { sheet: None, name } = &pair[0] else {
                return Err("LET variable is not a name".into());
            };
            let ix = self.st.param_name(name);
            self.out.push(0x03 | Cls::Ref.bits());
            self.out.extend(ix.to_le_bytes());
            self.emit(&pair[1], Cls::Value)?;
            self.lets.push(name.clone());
        }
        self.emit(&args[args.len() - 1], Cls::Value)?;
        self.lets.truncate(depth);
        self.out.push(0x02 | cls.bits());
        self.out.push((args.len() + 1) as u8);
        self.out.extend(0x00FFu16.to_le_bytes());
        Ok(())
    }

    fn lambda_call(&mut self, args: &[Expr], cls: Cls) -> Result<(), String> {
        if args.is_empty() {
            return Err("LAMBDA needs a body".into());
        }
        let depth = self.lets.len();
        let idx = self.st.future_name("_xlfn.LAMBDA");
        self.out.push(0x03 | Cls::Ref.bits());
        self.out.extend(idx.to_le_bytes());
        for p in &args[..args.len() - 1] {
            let Expr::Name { sheet: None, name } = p else {
                return Err("LAMBDA parameter is not a name".into());
            };
            let ix = self.st.param_name(name);
            self.out.push(0x03 | Cls::Ref.bits());
            self.out.extend(ix.to_le_bytes());
            self.lets.push(name.clone());
        }
        let body = if cls == Cls::Arr {
            Cls::Arr
        } else {
            Cls::Value
        };
        self.emit(&args[args.len() - 1], body)?;
        self.lets.truncate(depth);
        self.out.push(0x02 | Cls::Value.bits());
        self.out.push((args.len() + 1) as u8);
        self.out.extend(0x00FFu16.to_le_bytes());
        Ok(())
    }

    fn func(&mut self, name: &str, args: &[Expr], cls: Cls) -> Result<(), String> {
        let lower = name.to_ascii_lowercase();
        let future = lower.starts_with("_xlfn.") || lower.starts_with("_xlws.");
        let bare = if future { &name[6..] } else { name };
        let upper = bare.to_ascii_uppercase();

        if self.st.legacy && matches!(upper.as_str(), "LAMBDA" | "LET") {
            let (out, ex) = (std::mem::take(&mut self.out), std::mem::take(&mut self.ex));
            let _ = if upper == "LET" {
                self.let_call(args, cls)
            } else {
                self.lambda_call(args, cls)
            };
            (self.out, self.ex) = (out, ex);
            self.st.future_name(&format!("_xlfn.{upper}"));
            return Err(format!("{upper} is not encoded"));
        }
        if upper == "LET" {
            return self.let_call(args, cls);
        }
        if upper == "LAMBDA" {
            return self.lambda_call(args, cls);
        }
        let info = xlcalc::function_info(&upper);
        let legacy = self.st.legacy;
        let param = |i: usize| -> Cls {
            if legacy {
                if let Some(l) = xlcalc::argument_class(&upper, i) {
                    return match l {
                        b'R' | b'Q' => Cls::Ref,
                        b'A' => Cls::Arr,
                        _ if cls == Cls::Arr => Cls::Arr,
                        _ => Cls::Value,
                    };
                }
            }
            if matches!(upper.as_str(), "IFERROR" | "IFNA") {
                return Cls::Value;
            }
            if matches!(
                upper.as_str(),
                "SUMPRODUCT" | "MMULT" | "TRANSPOSE" | "MINVERSE" | "MDETERM"
            ) {
                return Cls::Arr;
            }
            let p = info.map_or("", |f| f.params).as_bytes();
            if p.is_empty() {
                return Cls::Value;
            }
            match p[i.min(p.len() - 1)] {
                b'R' | b'Q' => Cls::Ref,
                _ => Cls::Value,
            }
        };
        if info.is_some_and(|f| f.volatile) {
            self.volatile = true;
        }
        if future {
            let inner = upper
                .strip_prefix("_XLWS.")
                .map_or_else(|| upper.clone(), |rest| format!("_xlws.{rest}"));
            let idx = self.st.future_name(&format!("{}{}", &name[..6], inner));
            self.out.push(0x03 | Cls::Ref.bits());
            self.out.extend(idx.to_le_bytes());
            for (i, a) in args.iter().enumerate() {
                self.emit(a, param(i))?;
            }
            self.out.push(0x02 | cls.bits());
            self.out.push((args.len() + 1) as u8);
            self.out.extend(0x00FFu16.to_le_bytes());
            return Ok(());
        }

        if self.st.legacy
            && !func_ids().contains_key(upper.as_str())
            && self
                .st
                .names
                .iter()
                .any(|n| n.name.eq_ignore_ascii_case(name))
        {
            return Err(format!("LAMBDA is not encoded (call of the name {name})"));
        }
        let id = *func_ids()
            .get(upper.as_str())
            .ok_or_else(|| format!("function {upper} is not in the function table"))?;
        if self.st.legacy && id > LAST_LEGACY_FUNC {
            return self.legacy_call(&upper, args, cls, &param);
        }
        let n = args.len();
        match upper.as_str() {
            "IF" if (2..=3).contains(&n) => return self.if_fn(args, cls, id),
            "CHOOSE" if n >= 2 => return self.choose(args, cls, id),
            "IFERROR" if n == 2 => {
                if !self.st.legacy {
                    self.st.future_name("_xlfn.IFERROR");
                }
                self.emit(&args[0], param(0))?;
                let f = self.sub(&args[1], param(1))?;
                self.out.extend([0x19, 0x80]);
                self.out.extend((f.len() as u16).to_le_bytes());
                self.out.extend(&f);
                self.out.extend([0x19, 0x08]);
                self.out.extend(2u16.to_le_bytes());
                self.out.push(0x01 | cls.bits());
                self.out.extend(id.to_le_bytes());
                return Ok(());
            }
            _ => {}
        }
        for (i, a) in args.iter().enumerate() {
            self.emit(a, param(i))?;
        }
        let fixed = info.is_some_and(|f| f.min_args == f.max_args && usize::from(f.min_args) == n);
        if upper == "SUM" && n == 1 {
            self.out.extend([0x19, 0x10, 0, 0]);
        } else if fixed {
            self.out.push(0x01 | cls.bits());
            self.out.extend(id.to_le_bytes());
        } else {
            self.out.push(0x02 | cls.bits());
            self.out.push(n as u8);
            self.out.extend(id.to_le_bytes());
        }
        Ok(())
    }

    fn if_fn(&mut self, args: &[Expr], cls: Cls, id: u16) -> Result<(), String> {
        let first = if self.st.legacy && cls == Cls::Arr {
            Cls::Arr
        } else {
            Cls::Value
        };
        self.emit(&args[0], first)?;
        let t = self.sub(&args[1], cls)?;
        self.out.extend([0x19, 0x02]);
        self.out.extend((t.len() as u16 + 4).to_le_bytes());
        self.out.extend(&t);
        if args.len() == 3 {
            let f = self.sub(&args[2], cls)?;
            self.out.extend([0x19, 0x08]);
            self.out.extend((f.len() as u16 + 7).to_le_bytes());
            self.out.extend(&f);
        }
        self.out.extend([0x19, 0x08]);
        self.out.extend(3u16.to_le_bytes());
        self.out.push(0x02 | cls.bits());
        self.out.push(args.len() as u8);
        self.out.extend(id.to_le_bytes());
        Ok(())
    }

    fn choose(&mut self, args: &[Expr], cls: Cls, id: u16) -> Result<(), String> {
        let first = if self.st.legacy && cls == Cls::Arr {
            Cls::Arr
        } else {
            Cls::Value
        };
        self.emit(&args[0], first)?;
        let picks: Vec<Vec<u8>> = args[1..]
            .iter()
            .map(|a| self.sub(a, cls))
            .collect::<Result<_, _>>()?;
        let n = picks.len();
        self.out.extend([0x19, 0x04]);
        self.out.extend((n as u16).to_le_bytes());
        let mut at = (n as u16 + 1) * 2;
        for p in &picks {
            self.out.extend(at.to_le_bytes());
            at += p.len() as u16 + 4;
        }
        self.out.extend(at.to_le_bytes());
        let mut after: usize = picks.iter().map(|p| p.len() + 4).sum::<usize>() + 4;
        for p in &picks {
            after -= p.len() + 4;
            self.out.extend(p);
            self.out.extend([0x19, 0x08]);
            self.out.extend(((after - 1) as u16).to_le_bytes());
        }
        self.out.push(0x02 | cls.bits());
        self.out.push((n + 1) as u8);
        self.out.extend(id.to_le_bytes());
        Ok(())
    }
}

fn split_items(spec: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut depth = 0;
    let mut chars = spec.chars();
    while let Some(c) = chars.next() {
        match c {
            '\'' => {
                cur.push(c);
                if let Some(n) = chars.next() {
                    cur.push(n);
                }
            }
            '[' => {
                depth += 1;
                cur.push(c);
            }
            ']' => {
                depth -= 1;
                cur.push(c);
            }
            ',' if depth == 0 => out.push(std::mem::take(&mut cur)),
            _ => cur.push(c),
        }
    }
    if !cur.trim().is_empty() {
        out.push(cur);
    }
    out
}

fn bracketed(s: &str) -> Result<(String, &str), String> {
    let mut name = String::new();
    let mut it = s.char_indices();
    while let Some((i, c)) = it.next() {
        match c {
            '\'' => {
                if let Some((_, n)) = it.next() {
                    name.push(n);
                }
            }
            ']' => return Ok((name, &s[i + 1..])),
            _ => name.push(c),
        }
    }
    Err("unbalanced table item".into())
}

fn unescape_col(s: &str) -> String {
    let mut o = String::new();
    let mut it = s.chars();
    while let Some(c) = it.next() {
        if c == '\'' {
            if let Some(n) = it.next() {
                o.push(n);
            }
        } else {
            o.push(c);
        }
    }
    o
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hex(b: &[u8]) -> String {
        b.iter().map(|x| format!("{x:02x}")).collect()
    }

    fn enc(src: &str) -> String {
        let sheets = vec![
            "Sheet1".to_string(),
            "Sheet2".to_string(),
            "My Sheet".to_string(),
        ];
        let tables = [TableEnt {
            id: 1,
            names: ["Tbl".into(), "Tbl".into()],
            columns: vec!["col".into(), "col2".into()],
            sheet: 0,
            area: None,
        }];
        let mut st = State::default();
        let r = encode(&sheets, &tables, &mut st, Some(0), src, Cls::Value).unwrap();
        hex(&r.rgce)
    }

    #[test]
    fn operators_and_literals() {
        assert_eq!(enc("1+2*3"), "1e01001e02001e03000503");
        assert_eq!(enc("-A5"), "440400000000c013");
        assert_eq!(enc("A5%"), "440400000000c014");
        assert_eq!(enc("TRUE"), "1d01");
        assert_eq!(enc("#N/A"), "1c2a");
        assert_eq!(enc("1.5"), "1f000000000000f83f");
        assert_eq!(enc("65535"), "1effff");
        assert_eq!(enc("65536"), "1f000000000000f040");
        assert_eq!(enc("\"a\"\"b\""), "170300610022006200");
        assert_eq!(enc("A1<>B1"), "440000000000c0440000000001c00e");
        assert_eq!(enc("A$1+$A1"), "440000000000404400000000008003");
    }

    #[test]
    fn references() {
        assert_eq!(enc("$A$1"), "44000000000000");
        assert_eq!(enc("Sheet2!A1"), "5a00000000000000c0");
        assert_eq!(enc("Sheet1!A1"), "5a00000000000000c0");
        assert_eq!(
            enc("Sheet1!A1+Sheet2!A1"),
            "5a00000000000000c05a01000000000000c003"
        );
        assert_eq!(enc("A:A"), "4500000000ffff0f0000400040");
        assert_eq!(enc("1:3"), "4500000000020000000080ffbf");
        assert_eq!(enc("$A:$B"), "4500000000ffff0f0000000100");
    }

    #[test]
    fn functions() {
        assert_eq!(enc("SUM(A1:A5)"), "25000000000400000000c000c019100000");
        assert_eq!(enc("SUM(A1,B1)"), "240000000000c0240000000001c042020400");
        assert_eq!(enc("SUM(1,2)"), "1e01001e020042020400");
        assert_eq!(
            enc("IF(A1,1,2)"),
            "440000000000c0190207001e010019080a001e02001908030042030100"
        );
        assert_eq!(
            enc("IF(A1,1)"),
            "440000000000c0190207001e01001908030042020100"
        );
        assert_eq!(
            enc("IF(A1,1,)"),
            "440000000000c0190207001e010019080800161908030042030100"
        );
        assert_eq!(
            enc("CHOOSE(2,1,2,3)"),
            "1e02001904030008000f0016001d001e0100190811001e020019080a001e03001908030042046400"
        );
        assert_eq!(enc("NOW()"), "19010000414a00");
        assert_eq!(enc("ROW()"), "42000800");
        assert_eq!(enc("ABS(A1)"), "440000000000c0411800");
        assert_eq!(
            enc("IFERROR(A1,0)"),
            "440000000000c0198003001e00001908020041e001"
        );
        assert_eq!(
            enc("VLOOKUP(A1,B1:C5,2,FALSE)"),
            "240000000000c025000000000400000001c002c01e02001d0042046600"
        );
    }

    #[test]
    fn arrays_and_tables() {
        let sheets = vec!["Sheet1".to_string()];
        let mut st = State::default();
        let r = encode(&sheets, &[], &mut st, None, "{\"a\",TRUE}", Cls::Value).unwrap();
        assert_eq!(
            hex(&r.rgce),
            "40010000000000000000000000000000".get(..30).unwrap()
        );
        assert_eq!(hex(&r.rgcb), "010000000200000001010061000201");
        assert_eq!(
            enc("Tbl[col]"),
            "181900000100010000000000000000".get(..28).unwrap()
        );
        assert_eq!(
            enc("Tbl[[#This Row],[col]]"),
            "1819000041060100000000000000"
        );
        assert_eq!(enc("Tbl[[col]:[col2]]"), "1819000002040100000000000100");
        assert_eq!(enc("Tbl[#All]"), "1819000004040100000000000000");
    }

    #[test]
    fn future_functions_use_a_hidden_name() {
        let sheets = vec!["Sheet1".to_string()];
        let mut st = State::default();
        let r = encode(&sheets, &[], &mut st, None, "_xlfn.SEQUENCE(3)", Cls::Value).unwrap();
        assert_eq!(hex(&r.rgce), "23010000001e03004202ff00");
        assert_eq!(st.names[0].name, "_xlfn.SEQUENCE");
    }

    #[test]
    fn undefined_names_are_errors() {
        let sheets = vec!["Sheet1".to_string()];
        let mut st = State::default();
        assert!(encode(&sheets, &[], &mut st, None, "nope+1", Cls::Value).is_err());
    }
}
