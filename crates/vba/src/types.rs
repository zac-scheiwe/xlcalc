use xlcalc_variant::{VarType, Variant};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VType {
    Byte,
    Boolean,
    Integer,
    Long,
    LongLong,
    Single,
    Double,
    Currency,
    Date,
    String,
    Variant,

    Decimal,

    Object(Option<String>),
}

impl VType {
    pub fn named(name: &str) -> Option<VType> {
        Some(match name.to_ascii_lowercase().as_str() {
            "byte" => VType::Byte,
            "boolean" => VType::Boolean,
            "integer" => VType::Integer,
            "long" => VType::Long,
            "longlong" => VType::LongLong,
            "longptr" => VType::LongLong,
            "single" => VType::Single,
            "double" => VType::Double,
            "currency" => VType::Currency,
            "date" => VType::Date,
            "string" => VType::String,
            "variant" => VType::Variant,
            "object" => VType::Object(None),
            _ => return None,
        })
    }

    pub fn of_suffix(c: char) -> VType {
        match c {
            '%' => VType::Integer,
            '&' => VType::Long,
            '^' => VType::LongLong,
            '!' => VType::Single,
            '#' => VType::Double,
            '@' => VType::Currency,
            _ => VType::String,
        }
    }

    pub fn of_value(v: &Variant) -> VType {
        match v {
            Variant::Ui1(_) => VType::Byte,
            Variant::Bool(_) => VType::Boolean,
            Variant::I2(_) => VType::Integer,
            Variant::I4(_) => VType::Long,
            Variant::I8(_) => VType::LongLong,
            Variant::R4(_) => VType::Single,
            Variant::R8(_) => VType::Double,
            Variant::Cy(_) => VType::Currency,
            Variant::Date(_) => VType::Date,
            Variant::Bstr(_) | Variant::BstrNull => VType::String,
            Variant::Dispatch(_) | Variant::Unknown(_) => VType::Object(None),
            _ => VType::Variant,
        }
    }

    pub fn var_type(&self) -> Option<VarType> {
        Some(match self {
            VType::Byte => VarType::Ui1,
            VType::Boolean => VarType::Bool,
            VType::Integer => VarType::I2,
            VType::Long => VarType::I4,
            VType::LongLong => VarType::I8,
            VType::Single => VarType::R4,
            VType::Double => VarType::R8,
            VType::Currency => VarType::Cy,
            VType::Date => VarType::Date,
            VType::String => VarType::Bstr,
            VType::Decimal => VarType::Decimal,
            VType::Variant | VType::Object(_) => return None,
        })
    }

    pub fn of_var_type(t: VarType) -> VType {
        match t {
            VarType::Ui1 => VType::Byte,
            VarType::Bool => VType::Boolean,
            VarType::I2 => VType::Integer,
            VarType::I4 => VType::Long,
            VarType::I8 => VType::LongLong,
            VarType::R4 => VType::Single,
            VarType::R8 => VType::Double,
            VarType::Cy => VType::Currency,
            VarType::Date => VType::Date,
            VarType::Bstr => VType::String,
            VarType::Decimal => VType::Decimal,
            VarType::Dispatch | VarType::Unknown => VType::Object(None),
            _ => VType::Variant,
        }
    }

    pub fn default_value(&self) -> Variant {
        match self {
            VType::Byte => Variant::Ui1(0),
            VType::Boolean => Variant::Bool(false),
            VType::Integer => Variant::I2(0),
            VType::Long => Variant::I4(0),
            VType::LongLong => Variant::I8(0),
            VType::Single => Variant::R4(0.0),
            VType::Double => Variant::R8(0.0),
            VType::Currency => Variant::Cy(0),
            VType::Date => Variant::Date(0.0),
            VType::String => Variant::BstrNull,
            VType::Decimal | VType::Variant => Variant::Empty,
            VType::Object(_) => Variant::Dispatch(None),
        }
    }

    pub fn is_object(&self) -> bool {
        matches!(self, VType::Object(_))
    }

    pub fn is_integral(&self) -> bool {
        matches!(
            self,
            VType::Byte | VType::Boolean | VType::Integer | VType::Long | VType::LongLong
        )
    }

    pub fn is_dynamic(&self) -> bool {
        matches!(self, VType::Variant | VType::Object(_) | VType::Decimal)
    }

    pub fn name(&self) -> &'static str {
        match self {
            VType::Byte => "Byte",
            VType::Boolean => "Boolean",
            VType::Integer => "Integer",
            VType::Long => "Long",
            VType::LongLong => "LongLong",
            VType::Single => "Single",
            VType::Double => "Double",
            VType::Currency => "Currency",
            VType::Date => "Date",
            VType::String => "String",
            VType::Variant => "Variant",
            VType::Decimal => "Decimal",
            VType::Object(_) => "Object",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    IntDiv,
    Mod,
    Pow,
    Cat,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Like,
    Is,
    And,
    Or,
    Xor,
    Eqv,
    Imp,
}

impl BinOp {
    pub fn is_comparison(self) -> bool {
        matches!(
            self,
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
        )
    }

    pub fn is_logical(self) -> bool {
        matches!(
            self,
            BinOp::And | BinOp::Or | BinOp::Xor | BinOp::Eqv | BinOp::Imp
        )
    }
}

fn rank(t: &VType) -> u8 {
    match t {
        VType::Byte => 1,
        VType::Boolean | VType::Integer => 2,
        VType::Long => 3,
        VType::LongLong => 4,
        VType::Single => 5,
        VType::Double | VType::String => 6,
        VType::Currency => 7,
        _ => 8,
    }
}

fn of_rank(r: u8) -> VType {
    match r {
        1 => VType::Byte,
        2 => VType::Integer,
        3 => VType::Long,
        4 => VType::LongLong,
        5 => VType::Single,
        6 => VType::Double,
        _ => VType::Currency,
    }
}

fn integral(l: &VType, r: &VType, keep_boolean: bool) -> VType {
    let width = |t: &VType| match t {
        VType::Byte => 1,
        VType::Boolean | VType::Integer => 2,
        VType::LongLong => 4,
        _ => 3,
    };
    if keep_boolean && *l == VType::Boolean && *r == VType::Boolean {
        return VType::Boolean;
    }
    match width(l).max(width(r)) {
        1 => VType::Byte,
        2 => VType::Integer,
        4 => VType::LongLong,
        _ => VType::Long,
    }
}

pub fn binary_type(op: BinOp, l: &VType, r: &VType) -> VType {
    if op == BinOp::Is {
        return VType::Boolean;
    }
    if l.is_dynamic() || r.is_dynamic() {
        return VType::Variant;
    }
    match op {
        BinOp::Add | BinOp::Sub => {
            if *l == VType::String && *r == VType::String {
                return if op == BinOp::Add {
                    VType::String
                } else {
                    VType::Double
                };
            }
            match (l == &VType::Date, r == &VType::Date) {
                (true, true) if op == BinOp::Sub => return VType::Double,
                (true, _) | (_, true) => return VType::Date,
                _ => {}
            }
            numeric_type(l, r)
        }
        BinOp::Mul => {
            let (l, r) = (undate(l), undate(r));
            let float = |t: &VType| matches!(t, VType::Single | VType::Double);

            if (l == VType::Currency && float(&r)) || (r == VType::Currency && float(&l)) {
                return VType::Double;
            }
            numeric_type(&l, &r)
        }
        BinOp::Div => {
            let small = |t: &VType| {
                matches!(
                    t,
                    VType::Byte | VType::Boolean | VType::Integer | VType::Single
                )
            };
            if (*l == VType::Single || *r == VType::Single) && small(l) && small(r) {
                VType::Single
            } else {
                VType::Double
            }
        }
        BinOp::IntDiv | BinOp::Mod => integral(l, r, false),
        BinOp::Pow => VType::Double,
        BinOp::Cat => VType::String,
        BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge | BinOp::Like => {
            VType::Boolean
        }
        BinOp::And | BinOp::Or | BinOp::Xor | BinOp::Eqv | BinOp::Imp => integral(l, r, true),
        BinOp::Is => VType::Boolean,
    }
}

fn undate(t: &VType) -> VType {
    if *t == VType::Date {
        VType::Double
    } else {
        t.clone()
    }
}

fn numeric_type(l: &VType, r: &VType) -> VType {
    let (a, b) = (rank(l), rank(r));
    if (a == 5 && (b == 3 || b == 4)) || (b == 5 && (a == 3 || a == 4)) {
        return VType::Double;
    }
    of_rank(a.max(b))
}

pub fn unary_type(not: bool, t: &VType) -> VType {
    if t.is_dynamic() {
        return VType::Variant;
    }
    if not {
        return match t {
            VType::Boolean | VType::Byte | VType::Integer | VType::LongLong => t.clone(),
            _ => VType::Long,
        };
    }
    match t {
        VType::Byte | VType::Boolean => VType::Integer,
        VType::String => VType::Double,
        _ => t.clone(),
    }
}

pub fn date_serial(y: i64, m: i64, d: i64) -> f64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let mp = (m + 9) % 12;
    let doy = (153 * mp + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    let days = era * 146097 + doe - 719468;
    (days + 25569) as f64
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operator_types() {
        use VType::*;
        assert_eq!(binary_type(BinOp::Add, &Integer, &Integer), Integer);
        assert_eq!(binary_type(BinOp::Add, &Integer, &Long), Long);
        assert_eq!(binary_type(BinOp::Add, &Single, &Long), Double);
        assert_eq!(binary_type(BinOp::Add, &Single, &Integer), Single);
        assert_eq!(binary_type(BinOp::Add, &Boolean, &Boolean), Integer);
        assert_eq!(binary_type(BinOp::Add, &Byte, &Byte), Byte);
        assert_eq!(binary_type(BinOp::Add, &Double, &Currency), Currency);
        assert_eq!(binary_type(BinOp::Add, &Date, &Double), Date);
        assert_eq!(binary_type(BinOp::Sub, &Date, &Date), Double);
        assert_eq!(binary_type(BinOp::Mul, &Date, &Integer), Double);
        assert_eq!(binary_type(BinOp::Add, &String, &String), String);
        assert_eq!(binary_type(BinOp::Add, &String, &Integer), Double);
        assert_eq!(binary_type(BinOp::Div, &Integer, &Integer), Double);
        assert_eq!(binary_type(BinOp::Div, &Single, &Integer), Single);
        assert_eq!(binary_type(BinOp::IntDiv, &Double, &Integer), Long);
        assert_eq!(binary_type(BinOp::Mod, &Integer, &Byte), Integer);
        assert_eq!(binary_type(BinOp::And, &Boolean, &Boolean), Boolean);
        assert_eq!(binary_type(BinOp::And, &Boolean, &Integer), Integer);
        assert_eq!(binary_type(BinOp::Or, &Double, &Integer), Long);
        assert_eq!(binary_type(BinOp::Pow, &Integer, &Integer), Double);
        assert_eq!(binary_type(BinOp::Add, &Variant, &Integer), Variant);
        assert_eq!(binary_type(BinOp::Eq, &Variant, &Integer), Variant);
        assert_eq!(binary_type(BinOp::Eq, &Double, &Integer), Boolean);
        assert_eq!(unary_type(false, &Byte), Integer);
        assert_eq!(unary_type(true, &Double), Long);
    }

    #[test]
    fn dates() {
        assert_eq!(date_serial(1899, 12, 30), 0.0);
        assert_eq!(date_serial(1900, 3, 1), 61.0);
        assert_eq!(date_serial(1900, 2, 28), 60.0);
        assert_eq!(date_serial(2020, 1, 2), 43832.0);
        assert_eq!(date_serial(1899, 12, 29), -1.0);
    }
}
