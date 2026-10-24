use std::fmt;
use std::sync::Arc;

use crate::eval::lambda::LambdaValue;

pub type Text = Arc<str>;

pub const MAX_TEXT_LEN: usize = 32_767;

static CELL_EMPTY: std::sync::LazyLock<Text> = std::sync::LazyLock::new(|| Arc::from(""));

pub fn cell_empty() -> Text {
    CELL_EMPTY.clone()
}

pub fn is_cell_empty(t: &Text) -> bool {
    Arc::ptr_eq(t, &CELL_EMPTY)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub enum XlError {
    Null,

    Div0,

    Value,

    Ref,

    Name,

    Num,

    NA,

    GettingData,

    Spill,

    Connect,

    Blocked,

    Unknown,

    Field,

    Calc,

    Busy,

    Circ,
}

impl XlError {
    pub const ALL: [XlError; 15] = [
        XlError::Null,
        XlError::Div0,
        XlError::Value,
        XlError::Ref,
        XlError::Name,
        XlError::Num,
        XlError::NA,
        XlError::GettingData,
        XlError::Spill,
        XlError::Connect,
        XlError::Blocked,
        XlError::Unknown,
        XlError::Field,
        XlError::Calc,
        XlError::Busy,
    ];

    pub fn as_str(self) -> &'static str {
        match self {
            XlError::Null => "#NULL!",
            XlError::Div0 => "#DIV/0!",
            XlError::Value => "#VALUE!",
            XlError::Ref => "#REF!",
            XlError::Name => "#NAME?",
            XlError::Num => "#NUM!",
            XlError::NA => "#N/A",
            XlError::GettingData => "#GETTING_DATA",
            XlError::Spill => "#SPILL!",
            XlError::Connect => "#CONNECT!",
            XlError::Blocked => "#BLOCKED!",
            XlError::Unknown => "#UNKNOWN!",
            XlError::Field => "#FIELD!",
            XlError::Calc => "#CALC!",
            XlError::Busy => "#BUSY!",
            XlError::Circ => "#CIRC!",
        }
    }

    pub fn parse(s: &str) -> Option<XlError> {
        XlError::ALL
            .iter()
            .copied()
            .find(|e| e.as_str().eq_ignore_ascii_case(s))
    }

    pub fn type_number(self) -> u32 {
        self as u32 + 1
    }
}

impl fmt::Display for XlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Blank,

    Number(f64),

    Text(Text),

    Bool(bool),

    Error(XlError),

    Array(Arc<Array>),

    Lambda(Arc<LambdaValue>),
}

impl Value {
    pub const ZERO: Value = Value::Number(0.0);

    pub fn text(s: impl Into<Text>) -> Value {
        Value::Text(s.into())
    }

    pub fn empty_text() -> Value {
        Value::Text(Arc::from(""))
    }

    pub fn num(n: f64) -> Value {
        if n.is_finite() {
            Value::Number(crate::number::normalize(n))
        } else {
            Value::Error(XlError::Num)
        }
    }

    pub fn is_blank(&self) -> bool {
        matches!(self, Value::Blank)
    }

    pub fn is_cell_empty(&self) -> bool {
        matches!(self, Value::Text(t) if is_cell_empty(t))
    }

    pub fn plain_empty(self) -> Value {
        match self {
            Value::Text(t) if is_cell_empty(&t) => Value::empty_text(),
            Value::Array(a) if a.data.iter().any(Value::is_cell_empty) => {
                let mut a = Arc::unwrap_or_clone(a);
                for v in a.data.iter_mut() {
                    if v.is_cell_empty() {
                        *v = Value::empty_text();
                    }
                }
                Value::Array(Arc::new(a))
            }
            v => v,
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Value::Error(_))
    }

    pub fn as_error(&self) -> Option<XlError> {
        match self {
            Value::Error(e) => Some(*e),
            _ => None,
        }
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Value::Array(_))
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Blank => "blank",
            Value::Number(_) => "number",
            Value::Text(_) => "text",
            Value::Bool(_) => "bool",
            Value::Error(_) => "error",
            Value::Array(_) => "array",
            Value::Lambda(_) => "lambda",
        }
    }

    pub fn same_as(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Blank, Value::Blank) => true,
            (Value::Number(a), Value::Number(b)) => a.to_bits() == b.to_bits(),
            (Value::Text(a), Value::Text(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Error(a), Value::Error(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => {
                Arc::ptr_eq(a, b)
                    || (a.rows == b.rows
                        && a.cols == b.cols
                        && a.data.iter().zip(b.data.iter()).all(|(x, y)| x.same_as(y)))
            }
            (Value::Lambda(a), Value::Lambda(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::num(n)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<XlError> for Value {
    fn from(e: XlError) -> Self {
        Value::Error(e)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::Text(Arc::from(s))
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::Text(Arc::from(s))
    }
}

#[derive(Clone, Debug)]
pub struct Array {
    pub rows: u32,

    pub cols: u32,

    pub data: Vec<Value>,

    pub lam_blank: bool,
}

impl Array {
    pub fn new(rows: u32, cols: u32, data: Vec<Value>) -> Array {
        debug_assert_eq!(data.len(), rows as usize * cols as usize);
        debug_assert!(rows > 0 && cols > 0);
        Array {
            rows,
            cols,
            data,
            lam_blank: false,
        }
    }

    pub fn from_lambda(mut self) -> Array {
        self.lam_blank = self.data.iter().any(|v| matches!(v, Value::Blank));
        self
    }

    pub fn filled(rows: u32, cols: u32, v: Value) -> Array {
        Array::new(rows, cols, vec![v; rows as usize * cols as usize])
    }

    pub fn from_fn(rows: u32, cols: u32, mut f: impl FnMut(u32, u32) -> Value) -> Array {
        let mut data = Vec::with_capacity(rows as usize * cols as usize);
        for r in 0..rows {
            for c in 0..cols {
                data.push(f(r, c));
            }
        }
        Array::new(rows, cols, data)
    }

    pub fn column(values: Vec<Value>) -> Array {
        let n = values.len() as u32;
        Array::new(n, 1, values)
    }

    pub fn row(values: Vec<Value>) -> Array {
        let n = values.len() as u32;
        Array::new(1, n, values)
    }

    #[inline]
    pub fn get(&self, r: u32, c: u32) -> &Value {
        &self.data[(r * self.cols + c) as usize]
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn into_value(self) -> Value {
        Value::Array(Arc::new(self))
    }

    pub fn broadcast_get(&self, r: u32, c: u32) -> Value {
        let rr = if self.rows == 1 { 0 } else { r };
        let cc = if self.cols == 1 { 0 } else { c };
        if rr >= self.rows || cc >= self.cols {
            Value::Error(XlError::NA)
        } else {
            self.get(rr, cc).clone()
        }
    }
}
