use xlcalc::Rect;
use xlcalc::Workbook;
use xlcalc_variant::{change_type, ObjectId, SafeArray, VarType, Variant};

use crate::app::{Application, BookId, Call, Mode, Object, RangeRef, Track, DISPID_VALUE};
use crate::cmdtab::{COMMANDS, CONSTANTS, MASKS, MEMBERS, OBJECTS, PARENTS, PUTS};
use crate::conv::{input_locale, is_missing, scode_error, sfix, variant_locale, Args};
use crate::error::{OmError, DISP_E_TYPEMISMATCH};
use crate::style::StyleOwner;
use crate::typelib::Member;

const NUMBER: u16 = 1;
const TEXT: u16 = 2;
const BOOLEAN: u16 = 4;
const REFERENCE: u16 = 8;
const ERROR: u16 = 0x10;
const ARRAY: u16 = 0x40;
const MISSING: u16 = 0x80;
const NIL: u16 = 0x100;
const INTEGER: u16 = 0x800;
const LONG_TEXT: u16 = 0x1000;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct Refused;

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Oper {
    Number(f64),
    Integer(i32),
    Text(String),

    Dated(f64, String),
    Boolean(bool),
    Error,
    Array(Box<SafeArray>),

    Object(ObjectId),
    Missing,
    Nil,
}

impl Oper {
    fn kind(&self) -> u16 {
        match self {
            Oper::Number(_) => NUMBER,
            Oper::Integer(_) => INTEGER,

            Oper::Text(s) | Oper::Dated(_, s) if s.encode_utf16().nth(255).is_some() => LONG_TEXT,
            Oper::Text(_) | Oper::Dated(..) => TEXT,
            Oper::Boolean(_) => BOOLEAN,
            Oper::Error => ERROR,
            Oper::Array(_) => ARRAY,
            Oper::Object(_) => REFERENCE,
            Oper::Missing => MISSING,
            Oper::Nil => NIL,
        }
    }

    fn from_variant(v: &Variant, lcid: u32) -> Result<Oper, Refused> {
        let number = |x: f64| {
            if !x.is_finite() {
                Oper::Error
            } else if x == 0.0 || x.is_subnormal() {
                Oper::Number(0.0)
            } else {
                Oper::Number(x)
            }
        };
        let converted = |to| change_type(v, to, variant_locale(lcid), 0).map_err(|_| Refused);
        Ok(match v {
            Variant::Empty => Oper::Nil,
            Variant::Null => Oper::Missing,
            v if is_missing(v) => Oper::Missing,
            Variant::Error(code) if scode_error(*code).is_some() => Oper::Error,
            Variant::Error(_) | Variant::Unknown(_) | Variant::Dispatch(None) => {
                return Err(Refused)
            }
            Variant::Dispatch(Some(id)) => Oper::Object(*id),
            Variant::I2(n) => Oper::Integer(i32::from(*n)),
            Variant::Ui1(n) => Oper::Integer(i32::from(*n)),
            Variant::I4(n) if i16::try_from(*n).is_ok() => Oper::Integer(*n),
            Variant::I4(n) | Variant::Int(n) => number(f64::from(*n)),
            Variant::R4(x) => number(f64::from(*x)),
            Variant::R8(x) => number(*x),
            Variant::I8(_) | Variant::Decimal(_) => match converted(VarType::R8)? {
                Variant::R8(x) => number(x),
                _ => return Err(Refused),
            },
            Variant::Cy(_) | Variant::Date(_) => {
                match (converted(VarType::R8)?, converted(VarType::Bstr)?) {
                    (Variant::R8(x), Variant::Bstr(s)) => Oper::Dated(x, s),
                    _ => return Err(Refused),
                }
            }
            Variant::Bstr(s) if s.encode_utf16().nth(0x7fff).is_some() => return Err(Refused),
            Variant::Bstr(s) => Oper::Text(s.clone()),
            Variant::BstrNull => Oper::Text(String::new()),
            Variant::Bool(b) => Oper::Boolean(*b),
            Variant::Array(a) => Oper::Array(a.clone()),
        })
    }

    fn whole(&self) -> Option<i32> {
        match self {
            Oper::Number(x) if x.fract() == 0.0 => Some(sfix(*x)),
            Oper::Integer(n) => Some(*n),
            Oper::Boolean(b) => Some(i32::from(*b)),
            _ => None,
        }
    }

    pub(crate) fn is_missing(&self) -> bool {
        *self == Oper::Missing
    }
}

#[derive(Clone, Copy)]
pub(crate) struct Reader<'a> {
    pub wb: &'a Workbook,
    pub lcid: u32,
}

impl Reader<'_> {
    fn number_in(self, s: &str) -> Result<f64, Refused> {
        self.wb
            .number_in_text(s, &input_locale(self.lcid, self.wb.locale()))
            .ok_or(Refused)
    }

    pub(crate) fn coerce(self, oper: Oper, mask: u16) -> Result<Oper, Refused> {
        let has = |bit: u16| mask & bit != 0;
        if has(oper.kind()) {
            return Ok(oper);
        }

        let wrap = |v: Variant| {
            if mask & ARRAY == 0 {
                return Err(Refused);
            }
            Ok(Oper::Array(Box::new(SafeArray {
                elem: VarType::Variant,
                bounds: vec![(1, 1)],
                data: vec![v],
            })))
        };
        Ok(match oper {
            Oper::Array(a) => {
                let first = a.data.first().ok_or(Refused)?;
                return self.coerce(Oper::from_variant(first, self.lcid)?, mask);
            }
            Oper::Integer(n) if has(NUMBER) => Oper::Number(f64::from(n)),
            Oper::Integer(n) if has(TEXT) => Oper::Text(n.to_string()),
            Oper::Integer(n) if has(BOOLEAN) => Oper::Boolean(n != 0),
            Oper::Integer(n) => return wrap(Variant::I4(n)),
            Oper::Text(ref s) if s.encode_utf16().nth(255).is_some() => {
                return wrap(Variant::Bstr(s.clone()));
            }
            Oper::Text(ref s) if has(NUMBER) => Oper::Number(self.number_in(s)?),
            Oper::Text(ref s) if has(INTEGER) => Oper::Integer(sfix(self.number_in(s)?)),

            Oper::Text(ref s) if has(BOOLEAN) && s.eq_ignore_ascii_case("true") => {
                Oper::Boolean(true)
            }
            Oper::Text(ref s) if has(BOOLEAN) && s.eq_ignore_ascii_case("false") => {
                Oper::Boolean(false)
            }
            Oper::Text(ref s) => {
                return wrap(Variant::Bstr(s.clone()));
            }
            Oper::Dated(x, _) if has(NUMBER) => Oper::Number(x),
            Oper::Dated(x, _) if has(INTEGER) => Oper::Integer(sfix(x)),
            Oper::Dated(_, ref s) => {
                return wrap(Variant::Bstr(s.clone()));
            }
            Oper::Boolean(b) if has(NUMBER) => Oper::Number(f64::from(u8::from(b))),
            Oper::Boolean(b) if has(TEXT) => {
                Oper::Text(if b { "TRUE" } else { "FALSE" }.to_string())
            }
            Oper::Boolean(b) if has(INTEGER) => Oper::Integer(i32::from(b)),
            Oper::Boolean(b) => return wrap(Variant::Bool(b)),
            Oper::Number(x) if has(TEXT) => {
                let to = change_type(&Variant::R8(x), VarType::Bstr, variant_locale(self.lcid), 0);
                match to {
                    Ok(Variant::Bstr(s)) => Oper::Text(s),
                    _ => return Err(Refused),
                }
            }
            Oper::Number(x) if has(INTEGER) => Oper::Integer(sfix(x)),
            Oper::Number(x) if has(BOOLEAN) => Oper::Boolean(x != 0.0),
            Oper::Number(x) => return wrap(Variant::R8(x)),

            Oper::Missing | Oper::Nil if has(NIL) => Oper::Nil,
            Oper::Missing | Oper::Nil if has(NUMBER) => Oper::Number(0.0),
            Oper::Missing | Oper::Nil if has(TEXT) => Oper::Text(String::new()),
            Oper::Missing | Oper::Nil if has(INTEGER) => Oper::Integer(0),
            Oper::Missing | Oper::Nil if has(BOOLEAN) => Oper::Boolean(false),
            Oper::Missing | Oper::Nil | Oper::Error | Oper::Object(_) => return Err(Refused),
        })
    }

    pub(crate) fn int(self, oper: &Oper) -> Result<Option<i32>, Refused> {
        if oper.is_missing() {
            return Ok(None);
        }
        Ok(match self.coerce(oper.clone(), NUMBER)? {
            Oper::Number(x) => Some(sfix(x)),
            _ => return Err(Refused),
        })
    }

    pub(crate) fn number(self, oper: &Oper) -> Result<Option<f64>, Refused> {
        if oper.is_missing() {
            return Ok(None);
        }
        Ok(match self.coerce(oper.clone(), NUMBER)? {
            Oper::Number(x) => Some(x),
            _ => return Err(Refused),
        })
    }

    pub(crate) fn flag(self, oper: &Oper) -> Result<Option<bool>, Refused> {
        if oper.is_missing() {
            return Ok(None);
        }
        Ok(match self.coerce(oper.clone(), BOOLEAN)? {
            Oper::Boolean(b) => Some(b),
            _ => return Err(Refused),
        })
    }

    pub(crate) fn executor_int(self, oper: &Oper) -> Result<i32, Refused> {
        match self.coerce(oper.clone(), INTEGER)? {
            Oper::Integer(n) => Ok(n),
            _ => Err(Refused),
        }
    }
}

struct Calls {
    rows: u32,
    cols: u32,
    operands: Vec<Operands>,
}

const COLUMN_WIDTH: u16 = 0x2f;

#[derive(Clone, Copy)]
pub(crate) struct PutCommand {
    obt: u8,
    dispid: u16,
    command: u16,
    mask: u16,
    interface: &'static str,
    book: BookId,
}

impl PutCommand {
    pub(crate) fn of(obj: &Object, m: &Member) -> Option<PutCommand> {
        let (interface, book) = match obj {
            Object::Range(r) => ("Range", r.book),
            Object::Multi(rs) => ("Range", rs.first()?.book),
            Object::Font(StyleOwner::Range(r)) => ("Font", r.book),
            Object::Font(StyleOwner::Areas(rs)) => ("Font", rs.first()?.book),
            Object::Outline(book, _) => ("Outline", *book),
            _ => return None,
        };
        let dispid = u16::try_from(m.dispid).ok()?;
        let obt = OBJECTS.iter().find(|o| o.1 == interface)?.0;
        let &(_, _, command, operand) = PUTS.iter().find(|p| (p.0, p.1) == (obt, dispid))?;
        let (count, index) = COMMANDS[usize::from(command)];
        if operand + 1 >= count {
            return None;
        }
        let mask = MASKS[usize::from(index) + usize::from(operand)];

        let mask = if operand == 0 && mask & 0xa0 == 0xa0 {
            mask & !0xa0 | NIL
        } else {
            mask
        };
        Some(PutCommand {
            obt,
            dispid,
            command,
            mask,
            interface,
            book,
        })
    }

    fn value(self, reader: Reader, v: &Variant, direct: bool) -> Result<Variant, Refused> {
        let oper = Oper::from_variant(v, reader.lcid)?;
        let constants = || {
            CONSTANTS
                .iter()
                .filter(move |c| (c.0, c.1, c.2) == (self.obt, self.dispid, 0))
        };
        let public = |n: i32| constants().any(|c| i32::from(c.3) == n);
        if direct && oper.whole().is_some_and(public) {
            return Ok(v.clone());
        }
        let number = |x: f64| {
            let n = sfix(x);
            match constants().find(|c| i32::from(c.4) == n) {
                Some(c) => Ok(Some(Variant::I4(i32::from(c.3)))),
                None if public(n) => Err(Refused),
                None => Ok(None),
            }
        };
        Ok(match reader.coerce(oper, self.mask)? {
            Oper::Number(x) => number(x)?.unwrap_or(Variant::R8(x)),
            Oper::Integer(n) => number(f64::from(n))?.unwrap_or(Variant::I4(n)),
            Oper::Text(s) => Variant::Bstr(s),
            Oper::Boolean(b) => Variant::Bool(b),
            _ => v.clone(),
        })
    }
}

type Answered = (Option<(u32, u32)>, Vec<Variant>);

pub(crate) fn answers(rows: u32, cols: u32, data: Vec<Variant>) -> Variant {
    let bounds = if rows == 1 {
        vec![(1, cols)]
    } else {
        vec![(1, rows), (1, cols)]
    };
    Variant::Array(Box::new(SafeArray {
        elem: VarType::Variant,
        bounds,
        data,
    }))
}

impl Application<'_> {
    fn object_value(&mut self, id: ObjectId) -> Result<Variant, Refused> {
        let mut id = id;
        for _ in 0..8 {
            let value = if matches!(self.arena.get(id), Some(Object::Range(_))) {
                self.invoke(id, "Value2", Call::GET, &[], &[])
            } else {
                self.invoke_dispid(id, DISPID_VALUE, Call::GET, &[], &[])
            };
            match value.map_err(|_| Refused)? {
                Variant::Dispatch(Some(inner)) => id = inner,
                value => return Ok(value),
            }
        }
        Err(Refused)
    }

    pub(crate) fn direct_put_value(
        &mut self,
        interface: &str,
        member: &str,
        id: ObjectId,
        lcid: u32,
    ) -> Option<Result<Variant, OmError>> {
        let colour = matches!(
            member,
            "Color" | "ColorIndex" | "ThemeColor" | "TintAndShade"
        );
        let flag = interface == "Font" && matches!(member, "Bold" | "Italic");
        let covered = match interface {
            "Font" => colour || flag,
            "Interior" => {
                colour
                    || matches!(
                        member,
                        "Pattern"
                            | "PatternColor"
                            | "PatternColorIndex"
                            | "PatternThemeColor"
                            | "PatternTintAndShade"
                    )
            }
            "Border" | "Borders" | "Tab" => colour,
            _ => false,
        };
        if !covered {
            return None;
        }
        let mut id = id;
        let mut scalar = None;
        for _ in 0..8 {
            match self.invoke_dispid(id, DISPID_VALUE, Call::GET, &[], &[]) {
                Ok(Variant::Dispatch(Some(inner))) => id = inner,
                Ok(Variant::Array(_)) | Err(_) => break,
                Ok(value) => {
                    scalar = Some(value);
                    break;
                }
            }
        }
        let tint = member.ends_with("TintAndShade");
        Some(match scalar {
            Some(Variant::Bool(b)) if !flag => {
                let n = if b { -1 } else { 0 };
                Ok(if tint {
                    Variant::R8(f64::from(n))
                } else {
                    Variant::I4(n)
                })
            }
            Some(v) if tint => {
                Ok(change_type(&v, VarType::R8, variant_locale(lcid), 0).unwrap_or(v))
            }
            Some(v) => Ok(v),
            None if tint => Err(OmError::exception(
                xlcalc_variant::HResult::E_INVALIDARG.0,
                None,
            )),
            None => Err(OmError::type_mismatch()),
        })
    }

    pub(crate) fn put_command(
        &mut self,
        obj: &Object,
        m: &'static Member,
        put: PutCommand,
        value: &Variant,
        a: &Args,
        call: Call,
    ) -> Result<Variant, OmError> {
        let refused = OmError::unable_to_set(m.name, put.interface);
        let one = |app: &mut Self, obj: &Object, v: &Variant, direct: bool| {
            let wb = &app.book(put.book)?.wb;
            let reader = Reader { wb, lcid: a.lcid };
            let value = put.value(reader, v, direct).map_err(|_| refused.clone())?;
            let args = Args {
                put: Some(value),
                ..a.clone()
            };
            app.member(obj, m, &args, call)
        };
        let (value, literal) = match value {
            Variant::Dispatch(Some(id)) => {
                let value = self.object_value(*id).map_err(|_| refused.clone())?;
                (value, false)
            }
            v => (v.clone(), true),
        };
        let Variant::Array(array) = &value else {
            return one(self, obj, &value, literal);
        };
        let (rows, cols) = array_size(array);
        if array.data.is_empty() {
            return Err(refused);
        }
        match obj {
            Object::Range(r) if literal => {
                let (sheet, rect) = self.resolve(r)?;
                let book = r.book;

                let part = |app: &mut Self, at: (u32, u32), to: (u32, u32)| {
                    let element =
                        array_element(array, at.0, at.1).ok_or_else(|| refused.clone())?;
                    let cells = Object::Range(RangeRef {
                        book,
                        track: Track::Fixed(
                            sheet,
                            Rect {
                                r0: rect.r0 + at.0,
                                c0: rect.c0 + at.1,
                                r1: rect.r0 + to.0,
                                c1: rect.c0 + to.1,
                            },
                        ),
                        mode: Mode::Cells,
                        axis: None,
                    });
                    one(app, &cells, element, false)
                };
                let (last_row, last_col) = (rect.r1 - rect.r0, rect.c1 - rect.c0);

                if cols == 1 {
                    for row in 0..=last_row {
                        part(self, (row, 0), (row, last_col))?;
                        if rows == 1 {
                            if row < last_row && put.command != COLUMN_WIDTH {
                                part(self, (row + 1, 0), (last_row, last_col))?;
                            }
                            break;
                        }
                    }
                } else if rows == 1 {
                    for col in 0..=last_col {
                        part(self, (0, col), (0, col))?;
                    }
                    if last_row > 0 && put.command != COLUMN_WIDTH {
                        for col in 0..=last_col {
                            part(self, (1, col), (last_row, col))?;
                        }
                    }
                } else {
                    for row in 0..=last_row {
                        for col in 0..=last_col {
                            part(self, (row, col), (row, col))?;
                        }
                    }
                }
                Ok(Variant::Empty)
            }
            _ => {
                let mut failed = None;
                for col in 0..cols {
                    for row in 0..rows {
                        let done = match array_element(array, row, col) {
                            Some(v) => one(self, obj, v, false),
                            None => Err(refused.clone()),
                        };
                        failed = failed.or(done.err());
                    }
                }
                failed.map_or(Ok(Variant::Empty), Err)
            }
        }
    }

    pub(crate) fn range_operands(&mut self, dispid: i32, a: &Args) -> Result<Operands, Refused> {
        self.member_operands("Range", dispid, a)
    }

    pub(crate) fn member_operands(
        &mut self,
        interface: &str,
        dispid: i32,
        a: &Args,
    ) -> Result<Operands, Refused> {
        let mut operands = Operands::bind(interface, dispid, a)?;
        for i in 0..operands.opers.len() {
            let takes_reference = operands.masks.get(i).is_none_or(|m| m & REFERENCE != 0);
            if let (false, &Oper::Object(id)) = (takes_reference, &operands.opers[i]) {
                let value = self.object_value(id)?;
                operands.opers[i] = Oper::from_variant(&value, a.lcid)?;
            }
        }
        Ok(operands)
    }

    pub(crate) fn run_command(
        &mut self,
        book: BookId,
        lcid: u32,
        operands: Operands,
        refused: &OmError,
        mut run: impl FnMut(&mut Self, &Operands) -> Result<Variant, OmError>,
    ) -> Result<Variant, OmError> {
        let (shape, mut done) = self.each_call(lcid, operands, |app, mut operands| {
            let wb = &app.book(book)?.wb;
            operands
                .coerce(Reader { wb, lcid })
                .map_err(|_| refused.clone())?;
            run(app, &operands)
        })?;
        Ok(match shape {
            Some((rows, cols)) => answers(rows, cols, done),
            None => done.pop().unwrap_or(Variant::Empty),
        })
    }

    pub(crate) fn each_call(
        &mut self,
        lcid: u32,
        operands: Operands,
        mut run: impl FnMut(&mut Self, Operands) -> Result<Variant, OmError>,
    ) -> Result<Answered, OmError> {
        let Some(calls) = operands.calls(lcid)? else {
            return Ok((None, vec![run(self, operands)?]));
        };
        let mut done = Vec::new();
        let mut failed = None;
        for one in calls.operands {
            match run(self, one) {
                Ok(answer) => done.push(answer),
                Err(e) => failed = failed.or(Some(e)),
            }
        }
        match failed {
            Some(e) => Err(e),
            None => Ok((Some((calls.rows, calls.cols)), done)),
        }
    }
}

fn array_size(a: &SafeArray) -> (u32, u32) {
    match a.bounds[..] {
        [(_, n)] => (1, n),
        [(_, rows), (_, cols)] => (rows, cols),
        _ => (0, 0),
    }
}

fn array_element(a: &SafeArray, row: u32, col: u32) -> Option<&Variant> {
    let (rows, cols) = array_size(a);
    let row = if rows == 1 { 0 } else { row };
    let col = if cols == 1 { 0 } else { col };
    (row < rows && col < cols)
        .then(|| a.data.get((col * rows + row) as usize))
        .flatten()
}

#[derive(Clone)]
pub(crate) struct Operands {
    masks: &'static [u16],
    opers: Vec<Oper>,

    pub given: usize,
}

impl Operands {
    pub(crate) fn bind(interface: &str, dispid: i32, a: &Args) -> Result<Operands, Refused> {
        let dispid = u16::try_from(dispid).map_err(|_| Refused)?;
        let mut obt = OBJECTS.iter().find(|o| o.1 == interface).ok_or(Refused)?.0;

        let (command, positions) = loop {
            if obt == 0 {
                return Err(Refused);
            }
            let found = MEMBERS.iter().find(|m| (m.0, m.1, m.2) == (obt, dispid, 0));
            if let Some(m) = found {
                break (m.3, m.4);
            }
            obt = PARENTS[usize::from(obt)];
        };
        let (count, index) = COMMANDS[usize::from(command)];
        let start = usize::from(index);
        let masks = &MASKS[start..start + usize::from(count).saturating_sub(1)];
        let size = positions
            .iter()
            .map(|&p| usize::from(p) + 1)
            .fold(masks.len(), usize::max);
        let mut opers = vec![Oper::Missing; size];
        let mut given = 0;
        for (i, (&position, v)) in positions.iter().zip(&a.values).enumerate() {
            let Some(v) = v.as_ref().filter(|v| !is_missing(v)) else {
                continue;
            };
            let mut oper = Oper::from_variant(v, a.lcid)?;
            if let Some(whole) = oper.whole() {
                let translated = CONSTANTS.iter().find(|c| {
                    (c.0, c.1, usize::from(c.2), i32::from(c.3)) == (obt, dispid, i + 1, whole)
                });
                if let Some(c) = translated {
                    oper = Oper::Integer(i32::from(c.4));
                }
            }
            opers[usize::from(position)] = oper;
            given = given.max(usize::from(position) + 1);
        }
        Ok(Operands {
            masks,
            opers,
            given,
        })
    }

    pub(crate) fn get(&self, position: usize) -> &Oper {
        self.opers.get(position).unwrap_or(&Oper::Missing)
    }

    fn spread(&self, i: usize) -> Option<&SafeArray> {
        let takes_array = self.masks.get(i).is_none_or(|m| m & ARRAY != 0);
        match &self.opers[i] {
            Oper::Array(a) if !takes_array => Some(a),
            _ => None,
        }
    }

    pub(crate) fn has_array(&self, lcid: u32) -> Result<bool, OmError> {
        Ok(self.calls(lcid)?.is_some())
    }

    fn calls(&self, lcid: u32) -> Result<Option<Calls>, OmError> {
        let bad = || OmError::arg(DISP_E_TYPEMISMATCH, 0);
        let mut shape = None;
        for i in 0..self.opers.len() {
            let Some(a) = self.spread(i) else { continue };

            if a.data.is_empty() {
                return Err(bad());
            }
            let (rows, cols) = array_size(a);
            let (most_rows, most_cols) = shape.unwrap_or((0, 0));
            shape = Some((rows.max(most_rows), cols.max(most_cols)));
        }
        let Some((rows, cols)) = shape else {
            return Ok(None);
        };
        let mut operands = Vec::new();
        for col in 0..cols {
            for row in 0..rows {
                let mut one = self.clone();
                for i in 0..self.opers.len() {
                    let Some(a) = self.spread(i) else { continue };
                    one.opers[i] = match array_element(a, row, col) {
                        Some(v) => Oper::from_variant(v, lcid).map_err(|_| bad())?,
                        None => Oper::Error,
                    };
                }
                operands.push(one);
            }
        }
        Ok(Some(Calls {
            rows,
            cols,
            operands,
        }))
    }

    pub(crate) fn read_by_executor(&mut self, reader: Reader, position: usize) {
        if let Some(oper @ Oper::Array(_)) = self.opers.get_mut(position) {
            if let Ok(n) = reader.executor_int(oper) {
                *oper = Oper::Integer(n);
            }
        }
    }

    pub(crate) fn take(&mut self, position: usize) {
        if let Some(oper) = self.opers.get_mut(position) {
            *oper = Oper::Missing;
        }
    }

    pub(crate) fn coerce(&mut self, reader: Reader) -> Result<(), Refused> {
        for (i, oper) in self.opers.iter_mut().enumerate() {
            let Some(&mask) = self.masks.get(i) else {
                if oper.is_missing() {
                    continue;
                }
                return Err(Refused);
            };

            let mask = if i == 0 && mask & 0xa0 == 0xa0 {
                mask & !0xa0 | NIL
            } else {
                mask
            };
            *oper = reader.coerce(std::mem::replace(oper, Oper::Missing), mask)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{Oper, Operands, Reader, Refused};
    use crate::cmdtab::{COMMANDS, MASKS, MEMBERS};
    use crate::conv::Args;
    use xlcalc::Workbook;
    use xlcalc_variant::{SafeArray, VarType, Variant};

    fn read(v: Variant, mask: u16) -> Result<Oper, Refused> {
        let wb = Workbook::empty();
        let reader = Reader { wb: &wb, lcid: 0 };
        reader.coerce(Oper::from_variant(&v, 0)?, mask)
    }

    #[test]
    fn coerces_to_the_mask() {
        let text = |s: &str| Variant::Bstr(s.into());
        assert_eq!(read(text("True"), 0x84), Ok(Oper::Boolean(true)));
        assert_eq!(read(text("1"), 0x84), Err(Refused));
        assert_eq!(read(Variant::R8(2.0), 0x84), Ok(Oper::Boolean(true)));
        assert_eq!(read(text("2.5"), 0x81), Ok(Oper::Number(2.5)));
        assert_eq!(read(text("x"), 0x81), Err(Refused));
        assert_eq!(read(Variant::R8(2.5), 0x81), Ok(Oper::Number(2.5)));
        assert_eq!(read(Variant::R8(2.5), 0x82), Ok(Oper::Text("2.5".into())));
        assert_eq!(
            read(Variant::Bool(true), 0x82),
            Ok(Oper::Text("TRUE".into()))
        );
        assert_eq!(read(Variant::Null, 0x84), Ok(Oper::Missing));
        assert_eq!(read(Variant::Empty, 0x81), Ok(Oper::Number(0.0)));
        assert_eq!(read(Variant::Empty, 0x82), Ok(Oper::Text(String::new())));
        assert_eq!(read(Variant::Empty, 0x84), Ok(Oper::Boolean(false)));
        assert_eq!(
            read(Variant::I4(70000), 0x82),
            Ok(Oper::Text("70000".into()))
        );
        assert_eq!(read(text(&"x".repeat(256)), 0x82), Err(Refused));
        assert!(read(text(&"x".repeat(255)), 0x82).is_ok());
    }

    #[test]
    fn an_array_makes_one_call_per_element() {
        let row = |n: &[i32]| {
            Oper::Array(Box::new(SafeArray {
                elem: VarType::Variant,
                bounds: vec![(0, n.len() as u32)],
                data: n.iter().map(|&n| Variant::I4(n)).collect(),
            }))
        };
        let operands = Operands {
            masks: &[0x81, 0x81],
            opers: vec![row(&[1, 2, 3]), row(&[4, 5])],
            given: 2,
        };
        let calls = operands.calls(0).ok().flatten().expect("calls");
        assert_eq!((calls.rows, calls.cols), (1, 3));
        let second: Vec<Oper> = calls.operands.iter().map(|o| o.get(1).clone()).collect();
        assert_eq!(second, [Oper::Integer(4), Oper::Integer(5), Oper::Error]);
    }

    #[test]
    fn every_member_has_its_masks() {
        for m in MEMBERS {
            let (count, index) = COMMANDS[usize::from(m.3)];
            assert!(usize::from(index) + usize::from(count).saturating_sub(1) <= MASKS.len());
        }
    }

    #[test]
    fn open_places_and_translates_arguments() {
        let mut values = vec![None; 15];
        values[7] = Some(Variant::R8(2.0));
        values[14] = Some(Variant::I4(1));
        let a = Args {
            values,
            put: None,
            lcid: 0,
        };

        let o = Operands::bind("Workbooks", 0x783, &a).expect("Open");
        assert_eq!(o.get(7), &Oper::Number(2.0));
        assert_eq!(o.get(16), &Oper::Integer(1));
        assert_eq!(o.given, 17);

        let mut values = vec![None; 18];
        values[4] = Some(Variant::I4(-4142));
        let a = Args {
            values,
            put: None,
            lcid: 0,
        };
        let o = Operands::bind("Workbooks", 0x784, &a).expect("OpenText");
        assert_eq!(o.get(4), &Oper::Integer(3));
    }
}
