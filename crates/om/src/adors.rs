use std::cmp::Ordering;
use std::sync::{Arc, Mutex};

use xlcalc_variant::{change_type, Decimal, Locale, SafeArray, VarType, Variant};
use xlcalc_vba::{InvokeKind, VbaError};

use crate::adodb::{ado_error, arity, bad_arg, closed, context, long_arg, missing, opened, opt};
use crate::comobj::ComObj;

const NO_RECORD_CODE: u32 = 0x0BCD;
const NOT_FOUND_CODE: u32 = 0x0CC1;

fn no_record() -> VbaError {
    ado_error(
        NO_RECORD_CODE,
        "Either BOF or EOF is True, or the current record has been deleted. Requested operation requires a current record.",
    )
}

fn not_found() -> VbaError {
    ado_error(
        NOT_FOUND_CODE,
        "Item cannot be found in the collection corresponding to the requested name or ordinal.",
    )
}

fn db_error(text: &str) -> VbaError {
    VbaError {
        number: 0x8004_0E21_u32 as i32,
        description: text.to_string(),
    }
}

fn multi_step() -> VbaError {
    db_error("Multiple-step operation generated errors. Check each status value.")
}

fn deleted_row() -> VbaError {
    VbaError {
        number: 0x8004_0E23_u32 as i32,
        description: "Row handle referred to a deleted row or a row marked for deletion.".into(),
    }
}

fn type_mismatch() -> VbaError {
    VbaError {
        number: 0x8002_0005_u32 as i32,
        description: "Type mismatch.".into(),
    }
}

#[derive(Clone, Debug)]
pub(crate) struct FieldDef {
    pub(crate) name: String,
    pub(crate) ty: i32,
    pub(crate) size: i32,
    attrs: i32,
}

fn fixed_size(ty: i32) -> Option<i32> {
    Some(match ty {
        2 | 11 | 18 => 2,
        3 | 4 | 10 | 19 => 4,
        5 | 6 | 7 | 8 | 9 | 13 | 20 | 21 | 64 => 8,
        12 => 24,
        14 | 72 | 135 => 16,
        16 | 17 => 1,
        131 => 19,
        133 | 134 => 6,
        _ => return None,
    })
}

fn var_size(ty: i32) -> bool {
    matches!(ty, 128..=130 | 200..=204 | 205)
}

#[derive(Debug)]
struct Row {
    key: u64,
    vals: Vec<Variant>,
    deleted: bool,

    added: bool,

    editing: bool,
    saved: Vec<Variant>,

    set: Vec<bool>,
}

#[derive(Debug, Default)]
pub(crate) struct Data {
    defs: Vec<FieldDef>,
    rows: Vec<Row>,
    next_key: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Pos {
    Bof,
    Eof,
    Row(usize),
}

#[derive(Debug)]
pub(crate) struct Recordset {
    open: bool,
    defs: Vec<FieldDef>,
    data: Arc<Mutex<Data>>,
    cursor_loc: i32,
    cursor_type: i32,
    lock: i32,
    lock_set: bool,
    page_size: i32,
    cache_size: i32,
    filter: String,
    sort: String,
    pos: Pos,
}

impl Default for Recordset {
    fn default() -> Recordset {
        Recordset {
            open: false,
            defs: Vec::new(),
            data: Arc::new(Mutex::new(Data::default())),
            cursor_loc: 2,
            cursor_type: 0,
            lock: 1,
            lock_set: false,
            page_size: 10,
            cache_size: 1,
            filter: String::new(),
            sort: String::new(),
            pos: Pos::Bof,
        }
    }
}

pub(crate) enum Reply {
    V(Variant),
    Fields,
    Field(usize, FieldDef),
    Cloned(Box<Recordset>),
}

fn ok(v: Variant) -> Result<Reply, VbaError> {
    Ok(Reply::V(v))
}

fn done() -> Result<Reply, VbaError> {
    ok(Variant::Empty)
}

fn to_f64(v: &Variant) -> Option<f64> {
    match v {
        Variant::I2(n) => Some(f64::from(*n)),
        Variant::I4(n) => Some(f64::from(*n)),
        Variant::R4(n) => Some(f64::from(*n)),
        Variant::R8(n) | Variant::Date(n) => Some(*n),
        Variant::Cy(n) => Some(*n as f64 / 10000.0),
        Variant::Ui1(n) => Some(f64::from(*n)),
        Variant::I8(n) => Some(*n as f64),
        Variant::Bool(b) => Some(if *b { -1.0 } else { 0.0 }),
        Variant::Decimal(_) => match change_type(v, VarType::R8, Locale::EN_US, 0) {
            Ok(Variant::R8(x)) => Some(x),
            _ => None,
        },
        _ => None,
    }
}

fn text_of(v: &Variant) -> Option<String> {
    match v.clone().canon() {
        Variant::Bstr(s) => Some(s),
        _ => None,
    }
}

fn compare(a: &Variant, b: &Variant) -> Ordering {
    match (to_f64(a), to_f64(b)) {
        (Some(x), Some(y)) => x.partial_cmp(&y).unwrap_or(Ordering::Equal),
        _ => match (text_of(a), text_of(b)) {
            (Some(x), Some(y)) => x.to_lowercase().cmp(&y.to_lowercase()),
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (None, None) => Ordering::Equal,
        },
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Op {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Like,
}

#[derive(Debug, Clone)]
enum Lit {
    Text(String),
    Num(f64),
    Null,
}

#[derive(Debug, Clone)]
enum Cond {
    Cmp(usize, Op, Lit),
    And(Box<Cond>, Box<Cond>),
    Or(Box<Cond>, Box<Cond>),
}

struct Lexer<'a> {
    chars: Vec<char>,
    i: usize,
    defs: &'a [FieldDef],
}

impl Lexer<'_> {
    fn skip(&mut self) {
        while self.chars.get(self.i).is_some_and(|c| c.is_whitespace()) {
            self.i += 1;
        }
    }

    fn word(&mut self) -> String {
        self.skip();
        let mut s = String::new();
        while let Some(&c) = self.chars.get(self.i) {
            if c.is_whitespace() || "=<>()".contains(c) {
                break;
            }
            s.push(c);
            self.i += 1;
        }
        s
    }

    fn keyword(&mut self, w: &str) -> bool {
        let save = self.i;
        let got = self.word();
        if got.eq_ignore_ascii_case(w) {
            true
        } else {
            self.i = save;
            false
        }
    }

    fn column(&mut self) -> Result<usize, VbaError> {
        self.skip();
        let name = if self.chars.get(self.i) == Some(&'[') {
            let end = self.chars[self.i..]
                .iter()
                .position(|&c| c == ']')
                .ok_or_else(bad_arg)?;
            let n: String = self.chars[self.i + 1..self.i + end].iter().collect();
            self.i += end + 1;
            n
        } else {
            self.word()
        };
        if name.is_empty() {
            return Err(bad_arg());
        }
        self.defs
            .iter()
            .position(|d| d.name.eq_ignore_ascii_case(&name))
            .ok_or_else(not_found)
    }

    fn op(&mut self) -> Result<Op, VbaError> {
        self.skip();
        let rest: String = self.chars[self.i..].iter().take(2).collect();
        for (t, op) in [
            ("<>", Op::Ne),
            ("<=", Op::Le),
            (">=", Op::Ge),
            ("=", Op::Eq),
            ("<", Op::Lt),
            (">", Op::Gt),
        ] {
            if rest.starts_with(t) {
                self.i += t.len();
                return Ok(op);
            }
        }
        if self.keyword("like") {
            return Ok(Op::Like);
        }
        Err(bad_arg())
    }

    fn lit(&mut self) -> Result<Lit, VbaError> {
        self.skip();
        if self.chars.get(self.i) == Some(&'\'') {
            let mut s = String::new();
            self.i += 1;
            loop {
                match self.chars.get(self.i) {
                    None => return Err(bad_arg()),
                    Some('\'') => {
                        if self.chars.get(self.i + 1) == Some(&'\'') {
                            s.push('\'');
                            self.i += 2;
                        } else {
                            self.i += 1;
                            return Ok(Lit::Text(s));
                        }
                    }
                    Some(&c) => {
                        s.push(c);
                        self.i += 1;
                    }
                }
            }
        }
        let w = self.word();
        if w.eq_ignore_ascii_case("null") {
            return Ok(Lit::Null);
        }
        w.parse::<f64>().map(Lit::Num).map_err(|_| bad_arg())
    }

    fn cmp(&mut self) -> Result<Cond, VbaError> {
        self.skip();
        if self.chars.get(self.i) == Some(&'(') {
            self.i += 1;
            let c = self.or()?;
            self.skip();
            if self.chars.get(self.i) != Some(&')') {
                return Err(bad_arg());
            }
            self.i += 1;
            return Ok(c);
        }
        let col = self.column()?;
        let op = self.op()?;
        let lit = self.lit()?;
        Ok(Cond::Cmp(col, op, lit))
    }

    fn and(&mut self) -> Result<Cond, VbaError> {
        let mut c = self.cmp()?;
        while self.keyword("and") {
            c = Cond::And(Box::new(c), Box::new(self.cmp()?));
        }
        Ok(c)
    }

    fn or(&mut self) -> Result<Cond, VbaError> {
        let mut c = self.and()?;
        while self.keyword("or") {
            c = Cond::Or(Box::new(c), Box::new(self.and()?));
        }
        Ok(c)
    }
}

fn parse_cond(defs: &[FieldDef], text: &str, single: bool) -> Result<Cond, VbaError> {
    let mut lx = Lexer {
        chars: text.chars().collect(),
        i: 0,
        defs,
    };
    let c = if single { lx.cmp()? } else { lx.or()? };
    lx.skip();
    if lx.i < lx.chars.len() {
        return Err(bad_arg());
    }
    Ok(c)
}

fn wildcard(p: &[char], s: &[char]) -> bool {
    match p.split_first() {
        None => s.is_empty(),
        Some((&'*' | &'%', rest)) => (0..=s.len()).any(|k| wildcard(rest, &s[k..])),
        Some((c, rest)) => s.first().is_some_and(|x| x == c) && wildcard(rest, &s[1..]),
    }
}

fn eval(c: &Cond, vals: &[Variant]) -> bool {
    match c {
        Cond::And(a, b) => eval(a, vals) && eval(b, vals),
        Cond::Or(a, b) => eval(a, vals) || eval(b, vals),
        Cond::Cmp(col, op, lit) => {
            let v = &vals[*col];
            let ord = match lit {
                Lit::Null => return false,
                Lit::Num(n) => {
                    match to_f64(v).or_else(|| text_of(v).and_then(|s| s.trim().parse().ok())) {
                        Some(x) => x.partial_cmp(n),
                        None => return false,
                    }
                }
                Lit::Text(t) => {
                    if let Some(x) = to_f64(v) {
                        match t.trim().parse::<f64>() {
                            Ok(n) => x.partial_cmp(&n),
                            Err(_) => return false,
                        }
                    } else if let Some(s) = text_of(v) {
                        if *op == Op::Like {
                            let p: Vec<char> = t.to_lowercase().chars().collect();
                            let s: Vec<char> = s.to_lowercase().chars().collect();
                            return wildcard(&p, &s);
                        }
                        Some(s.to_lowercase().cmp(&t.to_lowercase()))
                    } else {
                        return false;
                    }
                }
            };
            let Some(ord) = ord else { return false };
            match op {
                Op::Eq | Op::Like => ord == Ordering::Equal,
                Op::Ne => ord != Ordering::Equal,
                Op::Lt => ord == Ordering::Less,
                Op::Le => ord != Ordering::Greater,
                Op::Gt => ord == Ordering::Greater,
                Op::Ge => ord != Ordering::Less,
            }
        }
    }
}

fn parse_sort(defs: &[FieldDef], text: &str) -> Result<Vec<(usize, bool)>, VbaError> {
    let mut keys = Vec::new();
    if text.trim().is_empty() {
        return Ok(keys);
    }
    for part in text.split(',') {
        let part = part.trim();
        let (name, desc) = match part.rsplit_once(char::is_whitespace) {
            Some((n, d)) if d.eq_ignore_ascii_case("desc") => (n.trim(), true),
            Some((n, d)) if d.eq_ignore_ascii_case("asc") => (n.trim(), false),
            Some(_) => return Err(bad_arg()),
            None => (part, false),
        };
        let name = name.trim_start_matches('[').trim_end_matches(']');
        let i = defs
            .iter()
            .position(|d| d.name.eq_ignore_ascii_case(name))
            .ok_or_else(not_found)?;
        keys.push((i, desc));
    }
    Ok(keys)
}

fn text_arg(v: &Variant) -> Result<String, VbaError> {
    match v.clone().canon() {
        Variant::Bstr(s) => Ok(s),
        Variant::Null => Err(type_mismatch()),
        Variant::Empty => Ok(String::new()),
        other => match change_type(&other, VarType::Bstr, Locale::EN_US, 0) {
            Ok(Variant::Bstr(s)) => Ok(s),
            _ => Err(type_mismatch()),
        },
    }
}

impl Recordset {
    fn lock_data(&self) -> std::sync::MutexGuard<'_, Data> {
        self.data.lock().unwrap_or_else(|e| e.into_inner())
    }

    fn check_open(&self) -> Result<(), VbaError> {
        if self.open {
            Ok(())
        } else {
            Err(closed())
        }
    }

    fn defs_now(&self) -> Vec<FieldDef> {
        if self.open {
            self.lock_data().defs.clone()
        } else {
            self.defs.clone()
        }
    }

    fn order(&self, d: &Data) -> Vec<usize> {
        let mut idx: Vec<usize> = (0..d.rows.len()).collect();
        if let Ok(keys) = parse_sort(&d.defs, &self.sort) {
            if !keys.is_empty() {
                idx.sort_by(|&a, &b| {
                    for &(c, desc) in &keys {
                        let o = compare(&d.rows[a].vals[c], &d.rows[b].vals[c]);
                        let o = if desc { o.reverse() } else { o };
                        if o != Ordering::Equal {
                            return o;
                        }
                    }
                    Ordering::Equal
                });
            }
        }
        idx
    }

    fn filter_cond(&self, d: &Data) -> Option<Cond> {
        if self.filter.is_empty() {
            None
        } else {
            parse_cond(&d.defs, &self.filter, false).ok()
        }
    }

    fn visible_in(&self, d: &Data, order: &[usize]) -> Vec<usize> {
        let cond = self.filter_cond(d);
        order
            .iter()
            .copied()
            .filter(|&i| {
                let r = &d.rows[i];
                !r.deleted
                    && cond
                        .as_ref()
                        .is_none_or(|c| (r.editing && r.added) || eval(c, &r.vals))
            })
            .collect()
    }

    fn view(&self) -> Vec<usize> {
        let d = self.lock_data();
        let o = self.order(&d);
        self.visible_in(&d, &o)
    }

    fn first_pos(&self) -> Pos {
        self.view().first().map_or(Pos::Bof, |&i| Pos::Row(i))
    }

    fn bof(&self) -> bool {
        self.pos == Pos::Bof || self.view().is_empty()
    }

    fn eof(&self) -> bool {
        self.pos == Pos::Eof || self.view().is_empty()
    }

    fn current(&self) -> Result<usize, VbaError> {
        match self.pos {
            Pos::Row(i) => {
                if self.view().is_empty() {
                    return Err(no_record());
                }
                let d = self.lock_data();
                match d.rows.get(i) {
                    Some(r) if r.deleted => Err(deleted_row()),
                    Some(_) => Ok(i),
                    None => Err(no_record()),
                }
            }
            _ => Err(no_record()),
        }
    }

    fn commit_edit(&mut self) {
        if let Pos::Row(i) = self.pos {
            let batch = self.lock == 4;
            let mut d = self.lock_data();
            if let Some(r) = d.rows.get_mut(i) {
                if r.editing {
                    r.editing = false;
                    if !batch {
                        r.added = false;
                    }
                }
            }
        }
    }

    fn arg_index(&self, defs: &[FieldDef], v: &Variant) -> Result<usize, VbaError> {
        match v.clone().canon() {
            Variant::Bstr(s) => defs
                .iter()
                .position(|d| d.name.eq_ignore_ascii_case(&s))
                .ok_or_else(not_found),
            Variant::Empty | Variant::Null => Err(not_found()),
            other => {
                let n = long_arg(&other)?;
                usize::try_from(n)
                    .ok()
                    .filter(|&i| i < defs.len())
                    .ok_or_else(not_found)
            }
        }
    }

    fn store(def: &FieldDef, v: &Variant) -> Result<Variant, VbaError> {
        let nullable = def.attrs & 0x60 != 0;
        let src = v.clone().canon();
        match src {
            Variant::Null if nullable => return Ok(Variant::Null),
            Variant::Null => {
                return Err(db_error("Non-nullable column cannot be updated to Null."))
            }
            _ => {}
        }
        match def.ty {
            12 => Ok(if src == Variant::Empty {
                src
            } else {
                v.clone()
            }),
            128 | 204 | 205 => store_binary(def, &src),
            14 => store_decimal(&src),
            20 => store_big(&src),
            7 => store_date(&src),
            8 | 129 | 130 | 200..=203 => store_text(def, &src),
            _ => {
                if src == Variant::Empty {
                    return Ok(zero_of(def));
                }
                let Some(to) = vt_of(def.ty) else {
                    return Ok(v.clone());
                };
                if !convertible(&src, def.ty) {
                    return Err(multi_step());
                }
                change_type(&src, to, Locale::EN_US, 0).map_err(|_| multi_step())
            }
        }
    }
    fn open_call(&mut self, args: &[Variant]) -> Result<Reply, VbaError> {
        arity(args, 0, 5)?;
        if self.open {
            return Err(opened());
        }
        if matches!(args.first(), Some(Variant::Empty)) {
            return Err(bad_arg());
        }
        let source = opt(args, 0).is_some();
        if opt(args, 1).is_some() {
            return Err(ado_error(
                0x0E7D,
                "The connection cannot be used to perform this operation. It is either closed or invalid in this context.",
            ));
        }
        if self.defs.is_empty() && !source {
            return Err(ado_error(
                0x0E7D,
                "The connection cannot be used to perform this operation. It is either closed or invalid in this context.",
            ));
        }
        if let Some(l) = opt(args, 3) {
            let l = long_arg(l)?;
            self.lock = match l {
                1 => 1,
                3 => 3,
                2 | 4 => 4,
                _ => return Err(bad_arg()),
            };
            self.lock_set = true;
        } else if !self.lock_set || self.lock == 2 {
            self.lock = 4;
        }
        self.open = true;
        self.cursor_loc = 3;
        self.cursor_type = 3;
        {
            let mut d = self.lock_data();
            d.defs = self.defs.clone();
            d.rows.clear();
            d.next_key = 1;
        }
        self.pos = Pos::Bof;
        done()
    }

    fn close_call(&mut self) -> Result<Reply, VbaError> {
        self.check_open()?;
        self.open = false;
        self.defs.clear();
        self.data = Arc::new(Mutex::new(Data::default()));
        self.pos = Pos::Bof;
        self.filter.clear();
        done()
    }

    fn set_values(
        &mut self,
        row: usize,
        fields: &Variant,
        values: &Variant,
    ) -> Result<(), VbaError> {
        let defs = self.lock_data().defs.clone();
        let (fs, vs): (Vec<Variant>, Vec<Variant>) = match (fields.clone(), values.clone()) {
            (Variant::Array(f), Variant::Array(v)) => {
                if f.data.len() != v.data.len() || f.data.is_empty() {
                    return Err(bad_arg());
                }
                (f.data, v.data)
            }
            (f, Variant::Array(v)) if !matches!(f, Variant::Array(_)) && v.data.len() == 1 => {
                (vec![f], v.data)
            }
            (Variant::Array(_), _) | (_, Variant::Array(_)) => return Err(bad_arg()),
            (f, v) => (vec![f], vec![v]),
        };
        let mut idx = Vec::new();
        for f in &fs {
            idx.push(self.arg_index(&defs, f)?);
        }
        let mut stored = Vec::new();
        for (k, v) in vs.iter().enumerate() {
            stored.push(Self::store(&defs[idx[k]], v).map_err(|e| {
                if e.number == 0x8004_0E21_u32 as i32 && e.description.starts_with("Non-nullable") {
                    e
                } else {
                    multi_step()
                }
            })?);
        }
        let mut d = self.lock_data();
        for (k, v) in stored.into_iter().enumerate() {
            d.rows[row].vals[idx[k]] = v;
            d.rows[row].set[idx[k]] = true;
        }
        Ok(())
    }

    fn add_new(&mut self, args: &[Variant]) -> Result<Reply, VbaError> {
        arity(args, 0, 2)?;
        self.check_open()?;
        if self.lock == 1 {
            return Err(context());
        }
        self.commit_edit();
        let fields = opt(args, 0);
        let values = opt(args, 1);
        if fields.is_some() != values.is_some() {
            return Err(bad_arg());
        }
        let n = {
            let mut d = self.lock_data();
            let key = d.next_key;
            d.next_key += 1;
            let nf = d.defs.len();
            d.rows.push(Row {
                key,
                vals: vec![Variant::Empty; nf],
                deleted: false,
                added: true,
                editing: true,
                saved: Vec::new(),
                set: vec![false; nf],
            });
            d.rows.len() - 1
        };
        self.pos = Pos::Row(n);
        if let (Some(f), Some(v)) = (fields, values) {
            if let Err(e) = self.set_values(n, f, v) {
                self.lock_data().rows.pop();
                self.pos = self.view().last().map_or(Pos::Bof, |&i| Pos::Row(i));
                return Err(e);
            }
            self.commit_edit();
        }
        done()
    }

    fn update(&mut self, args: &[Variant]) -> Result<Reply, VbaError> {
        arity(args, 0, 2)?;
        self.check_open()?;
        if let Pos::Row(i) = self.pos {
            if self.lock_data().rows.get(i).is_some_and(|r| r.deleted) {
                return done();
            }
        }
        let cur = self.current()?;
        if let (Some(f), Some(v)) = (opt(args, 0), opt(args, 1)) {
            {
                let mut d = self.lock_data();
                if !d.rows[cur].editing {
                    d.rows[cur].saved = d.rows[cur].vals.clone();
                }
            }
            self.set_values(cur, f, v)?;
        }
        self.commit_edit();
        done()
    }

    fn cancel_update(&mut self) -> Result<Reply, VbaError> {
        self.check_open()?;
        let cur = self.current()?;
        let mut d = self.lock_data();
        if !d.rows[cur].editing {
            return done();
        }
        if d.rows[cur].added {
            d.rows.remove(cur);
            drop(d);
            self.pos = Pos::Bof;
            self.pos = if self.view().is_empty() {
                Pos::Bof
            } else {
                Pos::Eof
            };
            return done();
        }
        let saved = std::mem::take(&mut d.rows[cur].saved);
        d.rows[cur].vals = saved;
        d.rows[cur].editing = false;
        done()
    }

    fn delete(&mut self, args: &[Variant]) -> Result<Reply, VbaError> {
        arity(args, 0, 1)?;
        self.check_open()?;
        if let Some(a) = opt(args, 0) {
            match long_arg(a)? {
                1 => {}
                2..=4 => return Err(context()),
                _ => return Err(bad_arg()),
            }
        }
        let cur = self.current()?;
        let mut d = self.lock_data();
        d.rows[cur].deleted = true;
        d.rows[cur].editing = false;
        done()
    }

    fn step_to(&self, forward: bool) -> Pos {
        let d = self.lock_data();
        if (forward && self.pos == Pos::Bof) || (!forward && self.pos == Pos::Eof) {
            let o = self.order(&d);
            let vis = self.visible_in(&d, &o);
            let end = if forward { vis.first() } else { vis.last() };
            return end.map_or(self.pos, |&i| Pos::Row(i));
        }
        let order = self.order(&d);
        let vis = self.visible_in(&d, &order);
        let at = match self.pos {
            Pos::Row(i) => order.iter().position(|&x| x == i),
            _ => None,
        };
        let Some(at) = at else {
            return if forward { Pos::Eof } else { Pos::Bof };
        };
        if forward {
            order[at + 1..]
                .iter()
                .find(|i| vis.contains(i))
                .map_or(Pos::Eof, |&i| Pos::Row(i))
        } else {
            order[..at]
                .iter()
                .rev()
                .find(|i| vis.contains(i))
                .map_or(Pos::Bof, |&i| Pos::Row(i))
        }
    }

    fn navigate(&mut self, name: &str, args: &[Variant]) -> Result<Reply, VbaError> {
        self.check_open()?;
        let view = self.view();
        if view.is_empty() {
            return Err(no_record());
        }
        let last = *view.last().unwrap_or(&0);
        match name {
            "movefirst" => {
                self.commit_edit();
                self.pos = Pos::Row(view[0]);
            }
            "movelast" => {
                self.commit_edit();
                self.pos = Pos::Row(last);
            }
            "movenext" => {
                if self.pos == Pos::Eof {
                    return Err(no_record());
                }
                self.commit_edit();
                self.pos = self.step_to(true);
            }
            "moveprevious" => {
                if self.pos == Pos::Bof {
                    return Err(no_record());
                }
                self.commit_edit();
                self.pos = self.step_to(false);
            }
            _ => {
                let n = match opt(args, 0) {
                    Some(v) => long_arg(v).map_err(|_| type_mismatch())?,
                    None => return Err(VbaError::new(450)),
                };
                let start = opt(args, 1).map_or(Ok(0), long_arg)?;
                let base: i64 = match start {
                    0 => match self.pos {
                        Pos::Row(i) => match view.iter().position(|&x| x == i) {
                            Some(p) => p as i64,
                            None => return Err(no_record()),
                        },
                        Pos::Bof if n > 0 => -1,
                        Pos::Eof if n < 0 => view.len() as i64,
                        _ => return Err(no_record()),
                    },
                    1 => 0,
                    2 => view.len() as i64 - 1,
                    _ => return Err(bad_arg()),
                };
                self.commit_edit();
                let target = base + i64::from(n);
                self.pos = if target < 0 {
                    Pos::Bof
                } else if target >= view.len() as i64 {
                    Pos::Eof
                } else {
                    Pos::Row(view[target as usize])
                };
            }
        }
        done()
    }

    fn abs_position(&self) -> Result<Variant, VbaError> {
        self.check_open()?;
        let view = self.view();
        if view.is_empty() {
            return Ok(Variant::I4(-1));
        }
        Ok(Variant::I4(match self.pos {
            Pos::Bof => -2,
            Pos::Eof => -3,
            Pos::Row(i) => {
                if self.lock_data().rows[i].deleted {
                    return Err(deleted_row());
                }
                match view.iter().position(|&x| x == i) {
                    Some(p) => p as i32 + 1,
                    None => return Err(deleted_row()),
                }
            }
        }))
    }

    fn put_abs_position(&mut self, v: &Variant) -> Result<(), VbaError> {
        self.check_open()?;
        let n = long_arg(v)?;
        if n < 1 {
            return Err(bad_arg());
        }
        let view = self.view();
        self.commit_edit();
        self.pos = match view.get(n as usize - 1) {
            Some(&i) => Pos::Row(i),
            None => Pos::Eof,
        };
        Ok(())
    }

    fn find(&mut self, args: &[Variant]) -> Result<Reply, VbaError> {
        arity(args, 1, 4)?;
        self.check_open()?;
        let Some(c) = opt(args, 0) else {
            return Err(VbaError::new(450));
        };
        let Variant::Bstr(crit) = c.clone().canon() else {
            return Err(bad_arg());
        };
        let defs = self.lock_data().defs.clone();
        if crit.is_empty() {
            return Err(bad_arg());
        }
        let cond = parse_cond(&defs, &crit, true)?;
        let skip = opt(args, 1).map_or(Ok(0), long_arg)?;
        let dir = opt(args, 2).map_or(Ok(1), long_arg)?;
        if dir != 1 && dir != -1 {
            return Err(bad_arg());
        }
        let start = match args.get(3) {
            None => 0,
            Some(v) if missing(v) => 0,
            Some(Variant::Empty) => {
                return Err(ado_error(
                    0x0D5D,
                    "Application uses a value of the wrong type for the current operation.",
                ))
            }
            Some(v) => long_arg(v)?,
        };
        if !(0..=2).contains(&start) {
            return Err(bad_arg());
        }
        let view = self.view();
        let base = match start {
            1 => Some(0),
            2 => view.len().checked_sub(1),
            _ => match self.pos {
                Pos::Row(i) => view.iter().position(|&x| x == i),
                _ => None,
            },
        };
        let Some(base) = base else {
            return done();
        };
        let off = if skip < 0 {
            None
        } else {
            Some(i64::from(skip))
        };
        let first = off.map(|o| {
            if dir == 1 {
                base as i64 + o
            } else {
                base as i64 - o
            }
        });
        let found = first.and_then(|f| {
            let d = self.lock_data();
            let mut k = f;
            while k >= 0 && (k as usize) < view.len() {
                if eval(&cond, &d.rows[view[k as usize]].vals) {
                    return Some(view[k as usize]);
                }
                k += i64::from(dir);
            }
            None
        });
        self.commit_edit();
        self.pos = match found {
            Some(i) => Pos::Row(i),
            None if dir == 1 => Pos::Eof,
            None => Pos::Bof,
        };
        done()
    }

    fn get_rows(&mut self, args: &[Variant]) -> Result<Reply, VbaError> {
        arity(args, 0, 3)?;
        self.check_open()?;
        let want = opt(args, 0).map_or(Ok(-1), long_arg)?;
        if want == 0 {
            return Err(context());
        }
        let view = self.view();
        let defs = self.lock_data().defs.clone();
        let start = match args.get(1) {
            Some(v) if !missing(v) => long_arg(v)?,
            _ => 0,
        };
        let from = match start {
            0 => match self.pos {
                Pos::Row(i) => view.iter().position(|&x| x == i),
                _ => None,
            },
            1 => Some(0),
            2 => view.len().checked_sub(1),
            _ => return Err(bad_arg()),
        };
        let cols: Vec<usize> = match opt(args, 2) {
            None => (0..defs.len()).collect(),
            Some(Variant::Array(a)) => a
                .data
                .iter()
                .map(|v| self.arg_index(&defs, v))
                .collect::<Result<_, _>>()?,
            Some(v) => vec![self.arg_index(&defs, v)?],
        };
        let Some(from) = from else {
            return Err(no_record());
        };
        let upto = if want < 0 {
            view.len()
        } else {
            (from + want as usize).min(view.len())
        };
        let rows = &view[from..upto];
        let d = self.lock_data();
        let nf = cols.len();
        let mut data = vec![Variant::Empty; nf * rows.len()];
        for (r, &ri) in rows.iter().enumerate() {
            for (f, &c) in cols.iter().enumerate() {
                data[f + nf * r] = d.rows[ri].vals[c].clone();
            }
        }
        drop(d);
        self.pos = match view.get(upto) {
            Some(&i) => Pos::Row(i),
            None => Pos::Eof,
        };
        ok(Variant::Array(Box::new(SafeArray {
            elem: VarType::Variant,
            bounds: vec![(0, nf as u32), (0, rows.len() as u32)],
            data,
        })))
    }

    fn put_filter(&mut self, v: &Variant) -> Result<(), VbaError> {
        self.check_open()?;
        match v.clone().canon() {
            Variant::Bstr(s) => {
                if !s.is_empty() {
                    let defs = self.lock_data().defs.clone();
                    parse_cond(&defs, &s, false)?;
                }
                self.filter = s;
            }
            Variant::Null | Variant::Empty => return Err(bad_arg()),
            other => match long_arg(&other)? {
                0 => self.filter.clear(),
                _ => return Err(bad_arg()),
            },
        }
        self.pos = self.first_pos();
        Ok(())
    }

    fn put_sort(&mut self, v: &Variant) -> Result<(), VbaError> {
        let s = text_arg(v)?;
        if self.open {
            parse_sort(&self.defs_now(), &s)?;
        }
        self.sort = s;
        if self.open {
            self.pos = self.first_pos();
        }
        Ok(())
    }

    fn supports(&self, args: &[Variant]) -> Result<Reply, VbaError> {
        arity(args, 1, 1)?;
        let n = long_arg(&args[0])?;
        const MASK: i32 = 1024
            | 2048
            | 64
            | 128
            | 256
            | 512
            | 16384
            | 32768
            | 65536
            | 131072
            | 262144
            | 524288
            | 16_777_216
            | 33_554_432;
        ok(Variant::Bool(self.open && n & !MASK == 0))
    }

    fn edit_mode(&self) -> Result<Variant, VbaError> {
        self.check_open()?;
        let cur = match self.current() {
            Ok(i) => i,
            Err(e) if e.number == 0x8004_0E23_u32 as i32 => return Ok(Variant::I4(0)),
            Err(e) => return Err(e),
        };
        let d = self.lock_data();
        let r = &d.rows[cur];
        Ok(Variant::I4(if r.added {
            2
        } else if r.editing {
            1
        } else {
            0
        }))
    }

    fn clone_call(&mut self, args: &[Variant]) -> Result<Reply, VbaError> {
        arity(args, 0, 1)?;
        self.check_open()?;
        let lock = match opt(args, 0) {
            None => 4,
            Some(v) => match long_arg(v)? {
                1 => 1,
                3 => 3,
                4 => 4,
                _ => return Err(bad_arg()),
            },
        };
        let c = Recordset {
            open: true,
            defs: Vec::new(),
            data: self.data.clone(),
            cursor_loc: 3,
            cursor_type: 3,
            lock,
            lock_set: true,
            page_size: 10,
            cache_size: 1,
            filter: String::new(),
            sort: String::new(),
            pos: Pos::Bof,
        };
        let mut c = c;
        c.pos = c.first_pos();
        Ok(Reply::Cloned(Box::new(c)))
    }

    fn put_prop(&mut self, name: &str, v: &Variant) -> Result<(), VbaError> {
        match name {
            "cursorlocation" => {
                if self.open {
                    return Err(opened());
                }
                let n = long_arg(v)?;
                if !(1..=3).contains(&n) {
                    return Err(bad_arg());
                }
                self.cursor_loc = n;
            }
            "cursortype" => {
                if self.open {
                    return Err(opened());
                }
                let n = long_arg(v)?;
                if !(0..=3).contains(&n) {
                    return Err(bad_arg());
                }
                self.cursor_type = n;
            }
            "locktype" => {
                let n = long_arg(v)?;
                if !(1..=4).contains(&n) {
                    return Err(bad_arg());
                }
                if self.open {
                    return Err(opened());
                }
                self.lock = n;
                self.lock_set = true;
            }
            "filter" => self.put_filter(v)?,
            "sort" => self.put_sort(v)?,
            "pagesize" => {
                let n = long_arg(v)?;
                if n < 1 {
                    return Err(bad_arg());
                }
                self.page_size = n;
            }
            "cachesize" => {
                let n = long_arg(v)?;
                if n < 1 {
                    return Err(bad_arg());
                }
                self.cache_size = n;
            }
            "maxrecords" => {
                if self.open {
                    return Err(opened());
                }
                long_arg(v)?;
            }
            "absoluteposition" => self.put_abs_position(v)?,
            "absolutepage" => {
                self.check_open()?;
                let n = long_arg(v)?;
                let view = self.view();
                let at = (n.max(1) as usize - 1) * self.page_size as usize;
                self.pos = match view.get(at) {
                    Some(&i) => Pos::Row(i),
                    None => Pos::Eof,
                };
            }
            "bookmark" => {
                self.check_open()?;
                let Variant::R8(k) = v else {
                    return Err(bad_arg());
                };
                let d = self.lock_data();
                match d.rows.iter().position(|r| r.key as f64 == *k) {
                    Some(i) => {
                        drop(d);
                        self.pos = Pos::Row(i);
                    }
                    None => return Err(bad_arg()),
                }
            }
            "source" | "activeconnection" | "index" => {
                return Err(match name {
                    "index" => ado_error(
                        0x0CB3,
                        "Current provider does not support the necessary interface for Index functionality.",
                    ),
                    _ => type_mismatch(),
                })
            }
            _ => return Err(VbaError::new(438)),
        }
        Ok(())
    }

    pub(crate) fn call(
        &mut self,
        name: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Reply, VbaError> {
        let name = name.to_ascii_lowercase();
        if matches!(kind, InvokeKind::Put | InvokeKind::PutRef) {
            if (name.is_empty() || name == "collect") && args.len() == 2 {
                let i = self.field_index(&args[0])?;
                self.put_field(i, &args[1])?;
                return done();
            }
            if args.len() != 1 {
                return Err(VbaError::new(450));
            }
            self.put_prop(&name, &args[0])?;
            return done();
        }
        match name.as_str() {
            "" | "fields" => {
                if args.is_empty() {
                    if name.is_empty() {
                        return Err(VbaError::new(450));
                    }
                    return Ok(Reply::Fields);
                }
                self.fields_call("item", args)
            }
            "state" => {
                arity(args, 0, 0)?;
                ok(Variant::I4(i32::from(self.open)))
            }
            "cursorlocation" => ok(Variant::I4(self.cursor_loc)),
            "cursortype" => ok(Variant::I4(self.cursor_type)),
            "locktype" => ok(Variant::I4(self.lock)),
            "pagesize" => ok(Variant::I4(self.page_size)),
            "cachesize" => ok(Variant::I4(self.cache_size)),
            "maxrecords" => ok(Variant::I4(0)),
            "marshaloptions" => ok(Variant::I4(0)),
            "stayinsync" => ok(Variant::Bool(true)),
            "source" => ok(Variant::Bstr(String::new())),
            "activeconnection" => ok(Variant::Empty),
            "index" => ok(Variant::Bstr(String::new())),
            "filter" => {
                if self.filter.is_empty() {
                    ok(Variant::I4(0))
                } else {
                    ok(Variant::Bstr(self.filter.clone()))
                }
            }
            "sort" if self.sort.is_empty() => ok(Variant::Empty),
            "sort" => ok(Variant::Bstr(self.sort.clone())),
            "bof" => {
                self.check_open()?;
                ok(Variant::Bool(self.bof()))
            }
            "eof" => {
                self.check_open()?;
                ok(Variant::Bool(self.eof()))
            }
            "recordcount" => {
                self.check_open()?;
                ok(Variant::I4(self.view().len() as i32))
            }
            "pagecount" => {
                self.check_open()?;
                let n = self.view().len() as i32;
                ok(Variant::I4((n + self.page_size - 1) / self.page_size))
            }
            "absolutepage" => {
                self.check_open()?;
                let p = self.abs_position()?;
                match p {
                    Variant::I4(n) if n > 0 => ok(Variant::I4((n - 1) / self.page_size + 1)),
                    Variant::I4(n) => ok(Variant::I4(n)),
                    other => ok(other),
                }
            }
            "absoluteposition" => ok(self.abs_position()?),
            "editmode" => ok(self.edit_mode()?),
            "bookmark" => {
                self.check_open()?;
                let cur = self.current()?;
                ok(Variant::R8(self.lock_data().rows[cur].key as f64))
            }
            "open" => self.open_call(args),
            "close" => {
                arity(args, 0, 0)?;
                self.close_call()
            }
            "addnew" => self.add_new(args),
            "update" => self.update(args),
            "cancelupdate" => {
                arity(args, 0, 1)?;
                self.cancel_update()
            }
            "delete" => self.delete(args),
            "movefirst" | "movelast" | "movenext" | "moveprevious" | "move" => {
                self.navigate(&name, args)
            }
            "find" => self.find(args),
            "getrows" => self.get_rows(args),
            "supports" => self.supports(args),
            "clone" => self.clone_call(args),
            "collect" => {
                arity(args, 1, 2)?;
                if args.len() == 2 {
                    let v = args[1].clone();
                    let i = self.field_index(&args[0])?;
                    self.put_field(i, &v)?;
                    return done();
                }
                let i = self.field_index(&args[0])?;
                ok(self.get_field(i)?)
            }
            "requery" => {
                self.check_open()?;
                Err(context())
            }
            "updatebatch" => {
                self.check_open()?;
                self.commit_edit();
                for r in &mut self.lock_data().rows {
                    r.added = false;
                    r.editing = false;
                }
                done()
            }
            "cancelbatch" => {
                self.check_open()?;
                let mut d = self.lock_data();
                d.rows.retain(|r| !r.added);
                drop(d);
                self.pos = self.first_pos();
                done()
            }
            _ => Err(VbaError::new(438)),
        }
    }

    fn field_index(&self, v: &Variant) -> Result<usize, VbaError> {
        self.check_open()?;
        let defs = self.lock_data().defs.clone();
        self.arg_index(&defs, v)
    }

    pub(crate) fn get_field(&self, i: usize) -> Result<Variant, VbaError> {
        self.check_open()?;
        let cur = self.current()?;
        Ok(self.lock_data().rows[cur].vals[i].clone())
    }

    pub(crate) fn put_field(&mut self, i: usize, v: &Variant) -> Result<(), VbaError> {
        self.check_open()?;
        let cur = self.current()?;
        if self.lock == 1 {
            return Err(context());
        }
        let def = self.lock_data().defs[i].clone();
        let stored = Self::store(&def, v)?;
        let mut d = self.lock_data();
        let r = &mut d.rows[cur];
        if !r.editing {
            r.saved = r.vals.clone();
            r.editing = true;
        }
        r.vals[i] = stored;
        r.set[i] = true;
        Ok(())
    }

    pub(crate) fn field_actual_size(&self, i: usize, def: &FieldDef) -> Result<Variant, VbaError> {
        self.check_open()?;
        let cur = self.current()?;
        let d = self.lock_data();
        Ok(Variant::I4(match &d.rows[cur].vals[i] {
            _ if def.ty == 12 && d.rows[cur].set[i] => 24,
            Variant::Empty => -1,
            Variant::Null => 0,
            Variant::Bstr(s) if matches!(def.ty, 130 | 202 | 203) => 2 * s.chars().count() as i32,
            Variant::Bstr(s) => s.chars().count() as i32,
            Variant::Array(a) => a.data.len() as i32,
            _ => fixed_size(def.ty).unwrap_or(def.size),
        }))
    }

    pub(crate) fn fields_call(&mut self, name: &str, args: &[Variant]) -> Result<Reply, VbaError> {
        match name {
            "count" => {
                arity(args, 0, 0)?;
                ok(Variant::I4(self.defs_now().len() as i32))
            }
            "item" | "" => {
                arity(args, 1, 1)?;
                let defs = self.defs_now();
                let i = self.arg_index(&defs, &args[0])?;
                Ok(Reply::Field(i, defs[i].clone()))
            }
            "refresh" => done(),
            "append" => self.append(args),
            "delete" => {
                arity(args, 1, 1)?;
                if self.open {
                    return Err(context());
                }
                let defs = self.defs.clone();
                let i = self.arg_index(&defs, &args[0])?;
                self.defs.remove(i);
                done()
            }
            _ => Err(VbaError::new(438)),
        }
    }

    fn append(&mut self, args: &[Variant]) -> Result<Reply, VbaError> {
        arity(args, 2, 5)?;
        let name = match args[0].clone().canon() {
            Variant::Null => return Err(VbaError::new(13)),
            Variant::Empty => return Err(bad_arg()),
            v => match change_type(&v, VarType::Bstr, Locale::EN_US, 0) {
                Ok(Variant::Bstr(s)) => s,
                _ => return Err(VbaError::new(13)),
            },
        };
        let ty = match args[1].clone().canon() {
            Variant::Empty => return Err(bad_arg()),
            Variant::Null => return Err(VbaError::new(13)),
            v => long_arg(&v).map_err(|_| VbaError::new(13))?,
        };
        let size = match opt(args, 2) {
            Some(v) => long_arg(v).map_err(|_| VbaError::new(13))?,
            None => 0,
        };
        let attrs = match opt(args, 3) {
            Some(v) => long_arg(v).map_err(|_| VbaError::new(13))?,
            None => -1,
        };
        if name.is_empty() {
            return Err(bad_arg());
        }
        if attrs.checked_abs().is_none_or(|a| a >= 65536) && attrs != -1 {
            return Err(bad_arg());
        }
        let valid = fixed_size(ty).is_some() || matches!(ty, 128..=130 | 200..=205);
        if !valid {
            return Err(bad_arg());
        }
        let defined = match fixed_size(ty) {
            Some(s) => s,
            None => {
                if (size <= 0 && size != -1)
                    || (size == -1 && ty != 200 && ty != 201 && ty != 202 && ty != 203)
                {
                    return Err(bad_arg());
                }
                if var_size(ty) && size == 0 {
                    return Err(bad_arg());
                }
                size
            }
        };
        if self.open {
            return Err(context());
        }
        if self.defs.iter().any(|d| d.name.eq_ignore_ascii_case(&name)) {
            return Err(ado_error(
                0x0D27,
                "Object is already in collection. Cannot append.",
            ));
        }
        let attrs = if attrs < 0 { 0 } else { attrs };
        self.defs.push(FieldDef {
            name,
            ty,
            size: defined,
            attrs,
        });
        done()
    }
}

fn vt_of(ty: i32) -> Option<VarType> {
    Some(match ty {
        2 => VarType::I2,
        3 => VarType::I4,
        4 => VarType::R4,
        5 => VarType::R8,
        6 => VarType::Cy,
        7 => VarType::Date,
        11 => VarType::Bool,
        14 => VarType::Decimal,
        17 => VarType::Ui1,
        20 => VarType::I8,
        8 | 129 | 130 | 200..=203 => VarType::Bstr,
        _ => return None,
    })
}

fn store_binary(def: &FieldDef, src: &Variant) -> Result<Variant, VbaError> {
    let mut bytes: Vec<u8> = match src {
        Variant::Array(a) if a.elem == VarType::Ui1 && a.bounds.len() == 1 => a
            .data
            .iter()
            .map(|x| if let Variant::Ui1(b) = x { *b } else { 0 })
            .collect(),
        Variant::I4(n) => n.to_le_bytes().to_vec(),
        Variant::Bstr(s) if s.len() % 2 == 0 && s.chars().all(|c| c.is_ascii_hexdigit()) => (0..s
            .len())
            .step_by(2)
            .filter_map(|i| u8::from_str_radix(&s[i..i + 2], 16).ok())
            .collect(),
        _ => return Err(multi_step()),
    };
    let size = usize::try_from(def.size).unwrap_or(0);
    if def.ty != 205 && size > 0 && bytes.len() > size {
        return Err(multi_step());
    }
    if def.ty == 128 {
        bytes.resize(size, 0);
    }
    Ok(crate::adodb::byte_array(&bytes))
}

fn store_decimal(src: &Variant) -> Result<Variant, VbaError> {
    if matches!(src, Variant::Date(_) | Variant::Array(_)) || !convertible(src, 14) {
        return Err(multi_step());
    }
    if *src == Variant::Empty {
        return Ok(Variant::Decimal(Decimal::default()));
    }
    match change_type(src, VarType::Decimal, Locale::EN_US, 0) {
        Ok(Variant::Decimal(mut d)) => {
            d.mantissa /= 10u128.pow(u32::from(d.scale));
            d.scale = 0;
            d.negative &= d.mantissa != 0;
            Ok(Variant::Decimal(d))
        }
        _ => Err(multi_step()),
    }
}

fn decimal_of_i64(n: i64) -> Variant {
    Variant::Decimal(Decimal {
        negative: n < 0,
        scale: 0,
        mantissa: u128::from(n.unsigned_abs()),
    })
}

fn store_big(src: &Variant) -> Result<Variant, VbaError> {
    let x = match src {
        Variant::I8(n) => return Ok(decimal_of_i64(*n)),
        Variant::Empty => 0.0,
        Variant::Bstr(s) if s.is_empty() => 0.0,
        other if convertible(other, 20) => {
            match change_type(other, VarType::R8, Locale::EN_US, 0) {
                Ok(Variant::R8(x)) => x,
                _ => return Err(multi_step()),
            }
        }
        _ => return Err(multi_step()),
    };
    if x.is_nan() || x.abs() >= 9.223_372_036_854_776e18 {
        return Err(multi_step());
    }
    Ok(decimal_of_i64(x.round() as i64))
}

fn store_date(src: &Variant) -> Result<Variant, VbaError> {
    match src {
        Variant::Empty => Ok(Variant::Date(0.0)),
        Variant::Bstr(s) => parse_date_text(s).map(Variant::Date).ok_or_else(multi_step),
        Variant::Cy(_) | Variant::Decimal(_) | Variant::Array(_) => Err(multi_step()),
        other if convertible(other, 7) => {
            change_type(other, VarType::Date, Locale::EN_US, 0).map_err(|_| multi_step())
        }
        _ => Err(multi_step()),
    }
}

fn days_from_civil(y: i64, m: i64, d: i64) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let doy = (153 * (m + if m > 2 { -3 } else { 9 }) + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

fn parse_date_text(s: &str) -> Option<f64> {
    let mut date = None;
    let mut time = None;
    let mut pm = None;
    for part in s.split_whitespace() {
        let up = part.to_ascii_uppercase();
        if up == "AM" || up == "PM" {
            pm = Some(up == "PM");
        } else if part.contains(['-', '/']) && date.is_none() {
            let nums: Vec<i64> = part
                .split(['-', '/'])
                .map(|n| n.parse().ok())
                .collect::<Option<_>>()?;
            let [a, b, c] = nums[..] else { return None };
            let (y, m, d) = if part.contains('-') {
                (a, b, c)
            } else {
                (c, a, b)
            };
            let y = if y < 100 {
                y + if y < 30 { 2000 } else { 1900 }
            } else {
                y
            };
            if !(1..=12).contains(&m) || !(1..=31).contains(&d) || !(100..=9999).contains(&y) {
                return None;
            }
            date = Some((days_from_civil(y, m, d) - days_from_civil(1899, 12, 30)) as f64);
        } else if part.contains([':', '.']) && time.is_none() {
            let nums: Vec<f64> = part
                .split([':', '.'])
                .map(|n| n.parse().ok())
                .collect::<Option<_>>()?;
            let (h, m, sec) = match nums[..] {
                [h, m] => (h, m, 0.0),
                [h, m, sec] => (h, m, sec),
                _ => return None,
            };
            let h = match pm {
                Some(true) if h < 12.0 => h + 12.0,
                _ => h,
            };
            if !(0.0..24.0).contains(&h) || !(0.0..60.0).contains(&m) || !(0.0..60.0).contains(&sec)
            {
                return None;
            }
            time = Some((h * 3600.0 + m * 60.0 + sec) / 86400.0);
        } else {
            return None;
        }
    }
    if date.is_none() && time.is_none() {
        return None;
    }
    Some(date.unwrap_or(0.0) + time.unwrap_or(0.0))
}

pub(crate) fn r8_text(x: f64) -> String {
    if x == 0.0 {
        return "0".into();
    }
    let e = format!("{:.16e}", x.abs());
    let (m, ex) = e.split_once('e').unwrap_or((&e, "0"));
    let ex: i32 = ex.parse().unwrap_or(0);
    let digits: String = m.chars().filter(char::is_ascii_digit).collect();
    let digits = digits.trim_end_matches('0');
    let digits = if digits.is_empty() { "0" } else { digits };
    let sign = if x < 0.0 { "-" } else { "" };
    if !(-5..17).contains(&ex) {
        let frac = &digits[1..];
        let dot = if frac.is_empty() {
            String::new()
        } else {
            format!(".{frac}")
        };
        let es = if ex < 0 { '-' } else { '+' };
        return format!("{sign}{}{dot}E{es}{:02}", &digits[..1], ex.abs());
    }
    if ex >= 0 {
        let int_len = ex as usize + 1;
        if digits.len() <= int_len {
            format!("{sign}{digits}{}", "0".repeat(int_len - digits.len()))
        } else {
            format!("{sign}{}.{}", &digits[..int_len], &digits[int_len..])
        }
    } else {
        format!("{sign}0.{}{digits}", "0".repeat((-ex - 1) as usize))
    }
}

fn store_text(def: &FieldDef, src: &Variant) -> Result<Variant, VbaError> {
    let mut s = match src {
        Variant::Empty => String::new(),
        Variant::Bstr(s) => s.clone(),
        Variant::Bool(b) => if *b { "True" } else { "False" }.into(),
        Variant::R8(x) => r8_text(*x),
        Variant::Array(_) | Variant::Dispatch(_) | Variant::Unknown(_) | Variant::Error(_) => {
            return Err(multi_step())
        }
        other => match change_type(other, VarType::Bstr, Locale::EN_US, 0) {
            Ok(Variant::Bstr(s)) => s,
            _ => return Err(multi_step()),
        },
    };
    let n = s.chars().count();
    let size = usize::try_from(def.size).unwrap_or(0);
    if matches!(def.ty, 129 | 130 | 200 | 202) && size > 0 && n > size {
        return Err(multi_step());
    }
    if matches!(def.ty, 129 | 130) && n < size {
        s.extend(std::iter::repeat_n(' ', size - n));
    }
    Ok(Variant::Bstr(s))
}

fn zero_of(def: &FieldDef) -> Variant {
    match def.ty {
        2 => Variant::I2(0),
        3 => Variant::I4(0),
        4 => Variant::R4(0.0),
        5 => Variant::R8(0.0),
        6 => Variant::Cy(0),
        7 => Variant::Date(0.0),
        11 => Variant::Bool(false),
        17 => Variant::Ui1(0),
        20 => Variant::I8(0),
        8 | 129 | 130 | 200..=203 => Variant::Bstr(String::new()),
        _ => Variant::Empty,
    }
}

fn convertible(src: &Variant, ty: i32) -> bool {
    !matches!(
        (ty, src),
        (
            _,
            Variant::Array(_) | Variant::Dispatch(_) | Variant::Unknown(_) | Variant::Error(_)
        ) | (6, Variant::Date(_))
    )
}

impl crate::Application<'_> {
    fn ado_reply(&mut self, i: usize, reply: Reply) -> Variant {
        match reply {
            Reply::V(v) => v,
            Reply::Fields => self.new_com(ComObj::AdoFields(i)),
            Reply::Field(k, def) => self.new_com(ComObj::AdoField(i, k, def)),
            Reply::Cloned(c) => self.new_com(ComObj::AdoRecordset(c)),
        }
    }

    pub(crate) fn recordset_call(
        &mut self,
        i: usize,
        name: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let Some(ComObj::AdoRecordset(r)) = self.com.get_mut(i) else {
            return Err(VbaError::new(424));
        };
        let reply = r.call(name, kind, args)?;
        Ok(self.ado_reply(i, reply))
    }

    pub(crate) fn fields_obj_call(
        &mut self,
        i: usize,
        name: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        if kind != InvokeKind::Get {
            return Err(VbaError::new(438));
        }
        let Some(ComObj::AdoRecordset(r)) = self.com.get_mut(i) else {
            return Err(VbaError::new(424));
        };
        let reply = r.fields_call(&name.to_ascii_lowercase(), args)?;
        Ok(self.ado_reply(i, reply))
    }

    pub(crate) fn field_call(
        &mut self,
        i: usize,
        k: usize,
        def: &FieldDef,
        name: &str,
        kind: InvokeKind,
        args: &[Variant],
    ) -> Result<Variant, VbaError> {
        let Some(ComObj::AdoRecordset(r)) = self.com.get_mut(i) else {
            return Err(VbaError::new(424));
        };
        let name = name.to_ascii_lowercase();
        if matches!(kind, InvokeKind::Put | InvokeKind::PutRef) {
            if args.len() != 1 {
                return Err(VbaError::new(450));
            }
            if name == "type" || name == "definedsize" {
                return Ok(Variant::Empty);
            }
            if name != "value" && !name.is_empty() {
                return Err(VbaError::new(438));
            }
            r.put_field(k, &args[0])?;
            return Ok(Variant::Empty);
        }
        match name.as_str() {
            "name" => Ok(Variant::Bstr(def.name.clone())),
            "type" => Ok(Variant::I4(def.ty)),
            "definedsize" => Ok(Variant::I4(def.size)),
            "actualsize" => r.field_actual_size(k, def),
            "value" | "" => {
                arity(args, 0, 0)?;
                r.get_field(k)
            }
            _ => Err(VbaError::new(438)),
        }
    }

    pub(crate) fn ado_fields_items(&mut self, i: usize) -> Result<Vec<Variant>, VbaError> {
        let Some(ComObj::AdoRecordset(r)) = self.com.get(i) else {
            return Err(VbaError::new(424));
        };
        let defs = r.defs_now();
        Ok(defs
            .into_iter()
            .enumerate()
            .map(|(k, d)| self.new_com(ComObj::AdoField(i, k, d)))
            .collect())
    }
}
