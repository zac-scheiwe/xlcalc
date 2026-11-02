use crate::address::{CellAddr, Rect};
use crate::datatable::DataTable;
use crate::parser::ast::{Expr, UnaryOp};
use crate::parser::{parse, print::print};
use crate::workbook::{NameDef, Workbook};

const XLFN: &[&str] = &[
    "ACOT",
    "ACOTH",
    "AGGREGATE",
    "ANCHORARRAY",
    "ARABIC",
    "ARRAYTOTEXT",
    "BASE",
    "BETA.DIST",
    "BETA.INV",
    "BINOM.DIST",
    "BINOM.DIST.RANGE",
    "BINOM.INV",
    "BITAND",
    "BITLSHIFT",
    "BITOR",
    "BITRSHIFT",
    "BITXOR",
    "BYCOL",
    "BYROW",
    "CEILING.MATH",
    "CEILING.PRECISE",
    "CHISQ.DIST",
    "CHISQ.DIST.RT",
    "CHISQ.INV",
    "CHISQ.INV.RT",
    "CHISQ.TEST",
    "CHOOSECOLS",
    "CHOOSEROWS",
    "COMBINA",
    "CONCAT",
    "CONFIDENCE.NORM",
    "CONFIDENCE.T",
    "COT",
    "COTH",
    "COVARIANCE.P",
    "COVARIANCE.S",
    "CSC",
    "CSCH",
    "DAYS",
    "DECIMAL",
    "DROP",
    "ENCODEURL",
    "ERF.PRECISE",
    "ERFC.PRECISE",
    "EXPAND",
    "EXPON.DIST",
    "F.DIST",
    "F.DIST.RT",
    "F.INV",
    "F.INV.RT",
    "F.TEST",
    "FIELDVALUE",
    "FILTERXML",
    "FLOOR.MATH",
    "FLOOR.PRECISE",
    "FORECAST.ETS",
    "FORECAST.ETS.CONFINT",
    "FORECAST.ETS.SEASONALITY",
    "FORECAST.ETS.STAT",
    "FORECAST.LINEAR",
    "FORMULATEXT",
    "GAMMA",
    "GAMMA.DIST",
    "GAMMA.INV",
    "GAMMALN.PRECISE",
    "GAUSS",
    "HSTACK",
    "HYPGEOM.DIST",
    "IFNA",
    "IFS",
    "IMAGE",
    "IMCOSH",
    "IMCOT",
    "IMCSC",
    "IMCSCH",
    "IMSEC",
    "IMSECH",
    "IMSINH",
    "IMTAN",
    "ISFORMULA",
    "ISOMITTED",
    "ISOWEEKNUM",
    "LAMBDA",
    "LET",
    "LOGNORM.DIST",
    "LOGNORM.INV",
    "MAKEARRAY",
    "MAP",
    "MAXIFS",
    "MINIFS",
    "MODE.MULT",
    "MODE.SNGL",
    "MUNIT",
    "NEGBINOM.DIST",
    "NORM.DIST",
    "NORM.INV",
    "NORM.S.DIST",
    "NORM.S.INV",
    "NUMBERVALUE",
    "PDURATION",
    "PERCENTILE.EXC",
    "PERCENTILE.INC",
    "PERCENTRANK.EXC",
    "PERCENTRANK.INC",
    "PERMUTATIONA",
    "PHI",
    "POISSON.DIST",
    "QUARTILE.EXC",
    "QUARTILE.INC",
    "RANDARRAY",
    "RANK.AVG",
    "RANK.EQ",
    "REDUCE",
    "RRI",
    "SCAN",
    "SEC",
    "SECH",
    "SEQUENCE",
    "SHEET",
    "SHEETS",
    "SINGLE",
    "SKEW.P",
    "SORTBY",
    "STDEV.P",
    "STDEV.S",
    "STOCKHISTORY",
    "SWITCH",
    "T.DIST",
    "T.DIST.2T",
    "T.DIST.RT",
    "T.INV",
    "T.INV.2T",
    "T.TEST",
    "TAKE",
    "TEXTAFTER",
    "TEXTBEFORE",
    "TEXTJOIN",
    "TEXTSPLIT",
    "TOCOL",
    "TOROW",
    "UNICHAR",
    "UNICODE",
    "UNIQUE",
    "VALUETOTEXT",
    "VAR.P",
    "VAR.S",
    "VSTACK",
    "WEBSERVICE",
    "WEIBULL.DIST",
    "WRAPCOLS",
    "WRAPROWS",
    "XLOOKUP",
    "XMATCH",
    "XOR",
    "Z.TEST",
];

const XLWS: &[&str] = &["FILTER", "SORT"];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FileFormulaKind {
    Plain,

    Dynamic { spill: Rect },

    Array(Rect),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FileFormula {
    pub text: String,
    pub kind: FileFormulaKind,
}

struct Ctx<'a> {
    wb: &'a Workbook,
    host: Option<CellAddr>,

    scope: Vec<String>,
}

pub fn file_prefix(name: &str) -> &'static str {
    let up = name.to_uppercase();
    if XLWS.contains(&up.as_str()) {
        "_xlfn._xlws."
    } else if XLFN.contains(&up.as_str()) {
        "_xlfn."
    } else {
        ""
    }
}

fn prefixed(name: &str) -> String {
    let bare = crate::formula::strip_prefixes(name);
    let up = bare.to_uppercase();
    if XLWS.contains(&up.as_str()) {
        format!("_xlfn._xlws.{bare}")
    } else if XLFN.contains(&up.as_str()) {
        format!("_xlfn.{bare}")
    } else {
        bare.to_string()
    }
}

fn param_name(e: &Expr) -> Option<String> {
    match e.unwrap_ws() {
        Expr::Name { sheet: None, name } => Some(name.clone()),

        Expr::StructRef { table: None, spec } if !spec.is_empty() && !spec.contains(['[', ',']) => {
            Some(spec.clone())
        }
        _ => None,
    }
}

fn strip_ws(e: Expr) -> Expr {
    match e {
        Expr::Ws { expr, .. } => strip_ws(*expr),
        e => e,
    }
}

impl Ctx<'_> {
    fn bound(&self, name: &str) -> bool {
        let up = name.to_uppercase();
        self.scope.contains(&up)
    }

    fn structured(&self, table: Option<String>, spec: String) -> Expr {
        let table = table.or_else(|| {
            self.host
                .and_then(|h| self.wb.table_at(h))
                .map(|t| t.name.clone())
        });
        let spec = match spec.strip_prefix('@') {
            Some("") => "[#This Row]".to_string(),
            Some(rest) if rest.starts_with('[') => format!("[[#This Row],{rest}]"),
            Some(rest) => format!("[[#This Row],[{rest}]]"),
            None => spec,
        };

        let spec = match spec.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
            Some(inner) if spec.starts_with("[[#This Row]") => inner.to_string(),
            _ if spec == "[#This Row]" => "#This Row".to_string(),
            _ => spec,
        };
        Expr::StructRef { table, spec }
    }

    fn func(&mut self, name: String, args: Vec<Expr>) -> Expr {
        if self.bound(&name) {
            let args = args.into_iter().map(|a| self.walk(a)).collect();
            return Expr::Func {
                name: format!("_xlpm.{name}"),
                args,
            };
        }
        let bare = crate::formula::strip_prefixes(&name).to_uppercase();
        let n = args.len();
        let params: Vec<usize> = match bare.as_str() {
            "LET" => (0..n.saturating_sub(1)).step_by(2).collect(),
            "LAMBDA" => (0..n.saturating_sub(1)).collect(),
            _ => Vec::new(),
        };
        let depth = self.scope.len();
        let mut out = Vec::with_capacity(n);
        for (i, a) in args.into_iter().enumerate() {
            if params.contains(&i) {
                if let Some(p) = param_name(&a) {
                    self.scope.push(p.to_uppercase());
                    out.push(if matches!(a.unwrap_ws(), Expr::StructRef { .. }) {
                        Expr::StructRef {
                            table: None,
                            spec: format!("_xlpm.{p}"),
                        }
                    } else {
                        Expr::Name {
                            sheet: None,
                            name: format!("_xlpm.{p}"),
                        }
                    });
                    continue;
                }
            }
            out.push(self.walk(a));
        }
        self.scope.truncate(depth);
        Expr::Func {
            name: prefixed(&name),
            args: out,
        }
    }

    fn is_table_name(&self, name: &str) -> bool {
        let up = name.to_uppercase();
        let named = match self.host {
            Some(h) => self.wb.names.lookup(&up, h.sheet).is_some(),
            None => self
                .wb
                .names
                .iter()
                .any(|n| n.name.eq_ignore_ascii_case(name)),
        };
        !named
            && self
                .wb
                .tables
                .iter()
                .any(|t| t.name.eq_ignore_ascii_case(name))
    }

    fn walk(&mut self, e: Expr) -> Expr {
        crate::deep(|| match e {
            Expr::Name { sheet: None, name } if self.bound(&name) => Expr::Name {
                sheet: None,
                name: format!("_xlpm.{name}"),
            },

            Expr::Name { sheet: None, name } if self.is_table_name(&name) => Expr::Name {
                sheet: None,
                name: format!("{name}[]"),
            },
            Expr::StructRef { table, spec } => self.structured(table, spec),
            Expr::Func { name, args } => self.func(name, args),
            Expr::Call { callee, args } => Expr::Call {
                callee: Box::new(self.walk(*callee)),
                args: args.into_iter().map(|a| self.walk(a)).collect(),
            },
            Expr::Unary {
                op: UnaryOp::Implicit,
                expr,
            } => Expr::Func {
                name: "_xlfn.SINGLE".into(),
                args: vec![self.walk(strip_ws(*expr))],
            },
            Expr::Unary { op, expr } => Expr::Unary {
                op,
                expr: Box::new(self.walk(*expr)),
            },
            Expr::Spill(x) => Expr::Func {
                name: "_xlfn.ANCHORARRAY".into(),
                args: vec![self.walk(strip_ws(*x))],
            },
            Expr::Percent(x) => Expr::Percent(Box::new(self.walk(*x))),
            Expr::Paren(x) => Expr::Paren(Box::new(self.walk(*x))),
            Expr::Field { expr, field } => Expr::Field {
                expr: Box::new(self.walk(*expr)),
                field,
            },
            Expr::Binary { op, left, right } => Expr::Binary {
                op,
                left: Box::new(self.walk(*left)),
                right: Box::new(self.walk(*right)),
            },
            Expr::Ws {
                before,
                after,
                expr,
            } => Expr::Ws {
                before,
                after,
                expr: Box::new(self.walk(*expr)),
            },
            e => e,
        })
    }
}

fn excel_adds_at(e: &Expr) -> bool {
    crate::deep(|| excel_adds_at_inner(e))
}

fn excel_adds_at_inner(e: &Expr) -> bool {
    match e {
        Expr::Func { name, args } => {
            let bare = crate::formula::strip_prefixes(name).to_uppercase();
            bare == "XLOOKUP"
                || !(matches!(bare.as_str(), "LET" | "LAMBDA")
                    || crate::functions::lookup(&bare).is_some())
                || args.iter().any(excel_adds_at)
        }
        Expr::Call { .. } => true,
        Expr::Unary { expr, .. }
        | Expr::Percent(expr)
        | Expr::Spill(expr)
        | Expr::Paren(expr)
        | Expr::Ws { expr, .. }
        | Expr::Field { expr, .. } => excel_adds_at(expr),
        Expr::Binary { left, right, .. } => excel_adds_at(left) || excel_adds_at(right),
        _ => false,
    }
}

fn to_file(wb: &Workbook, text: &str, host: Option<CellAddr>) -> String {
    let body = text.strip_prefix('=').unwrap_or(text);
    match parse(body) {
        Ok(e) => {
            let mut cx = Ctx {
                wb,
                host,
                scope: Vec::new(),
            };
            let tree = cx.walk(e);
            let text = print(&tree);
            tree.drop_deep();
            text
        }
        Err(_) => body.to_string(),
    }
}

const HIDDEN_PLAIN: &[&str] = &[
    "AVERAGEIF",
    "AVERAGEIFS",
    "BAHTTEXT",
    "COUNTIFS",
    "CUBEKPIMEMBER",
    "CUBEMEMBER",
    "CUBEMEMBERPROPERTY",
    "CUBERANKEDMEMBER",
    "CUBESET",
    "CUBESETCOUNT",
    "CUBEVALUE",
    "IFERROR",
    "ISO.CEILING",
    "ISTHAIDIGIT",
    "NETWORKDAYS.INTL",
    "ROUNDBAHTDOWN",
    "ROUNDBAHTUP",
    "RTD",
    "SUMIFS",
    "THAIDAYOFWEEK",
    "THAIDIGIT",
    "THAIMONTHOFYEAR",
    "THAINUMSOUND",
    "THAINUMSTRING",
    "THAISTRINGLENGTH",
    "THAIYEAR",
    "WORKDAY.INTL",
];

fn hidden_tokens(text: &str) -> Vec<String> {
    let b = text.as_bytes();
    let ok = |c: u8| c.is_ascii_alphanumeric() || c == b'_' || c == b'.';
    let mut out = Vec::new();
    let mut i = 0;
    while i < b.len() {
        match b[i] {
            q @ (b'"' | b'\'') => {
                i += 1;
                while i < b.len() {
                    if b[i] == q {
                        if b.get(i + 1) == Some(&q) {
                            i += 2;
                            continue;
                        }
                        break;
                    }
                    i += 1;
                }
                i += 1;
            }
            b'_' if text[i..].starts_with("_xlfn.") || text[i..].starts_with("_xlpm.") => {
                if i > 0 && (ok(b[i - 1]) || b[i - 1] == b'!') {
                    i += 1;
                    continue;
                }
                let mut j = i;
                while j < b.len() && ok(b[j]) {
                    j += 1;
                }
                out.push(text[i..j].to_string());
                i = j;
            }
            _ => i += 1,
        }
    }
    out
}

impl Workbook {
    pub(crate) fn note_hidden_names(&mut self, fid: crate::workbook::FormulaId) {
        let f = self.formula(fid);
        let (root, addr) = (f.formula.root.clone(), f.addr);
        self.note_hidden_in(&root, Some(addr));
    }

    pub(crate) fn note_hidden_in(&mut self, root: &crate::formula::Node, host: Option<CellAddr>) {
        use crate::formula::Node;
        let mut maybe = false;
        let mut plain: Vec<&'static str> = Vec::new();
        crate::formula::walk(root, &mut |n| match n {
            Node::Func { id, .. } => {
                let name = crate::functions::def(*id).name;
                maybe |= !file_prefix(name).is_empty();
                if HIDDEN_PLAIN.contains(&name) {
                    plain.push(name);
                }
            }
            Node::Let { .. }
            | Node::Lambda(_)
            | Node::Spill(_)
            | Node::Name { .. }
            | Node::NamedCall { .. }
            | Node::Call { .. }
            | Node::Unary(crate::parser::ast::UnaryOp::Implicit, _) => maybe = true,
            _ => {}
        });
        for name in plain {
            self.names.note_hidden(&format!("_xlfn.{name}"));
        }
        if !maybe {
            return;
        }
        let shown = crate::formula_print::print_node(
            self,
            root,
            host.unwrap_or(CellAddr::new(crate::address::SheetId(0), 0, 0)),
        );
        let toks = hidden_tokens(&to_file(self, &shown, host));
        for t in toks {
            self.names.note_hidden(&t);
        }
    }

    pub fn file_formula(&self, addr: CellAddr) -> Option<FileFormula> {
        let fid = self.formula_at(addr)?;
        if self
            .data_tables
            .iter()
            .any(|t| t.sheet == addr.sheet && t.interior().contains(addr.row, addr.col))
        {
            return None;
        }
        let f = self.formula(fid);
        let shown = self.formula_text(addr)?;
        if let Some((rows, cols)) = f.array {
            let rect = Rect::new(addr.row, addr.col, addr.row + rows - 1, addr.col + cols - 1);
            return Some(FileFormula {
                text: to_file(self, &shown, Some(addr)),
                kind: FileFormulaKind::Array(rect),
            });
        }
        let legacy = self.legacy_formula_text(addr)?;

        let quiet = matches!(
            f.formula.root,
            crate::formula::Node::Unary(crate::parser::ast::UnaryOp::ImplicitQuiet, _)
        );
        let scalar = quiet == crate::legacy::legacy_entry_is_scalar(self, &shown, addr);
        let at_risk = parse(shown.trim_start_matches('=')).map_or(true, |e| excel_adds_at(&e));
        if f.spill.is_none()
            && scalar
            && !at_risk
            && crate::legacy::legacy_reads_back(self, &legacy, &shown)
        {
            return Some(FileFormula {
                text: to_file(self, &legacy, Some(addr)),
                kind: FileFormulaKind::Plain,
            });
        }
        let spill = f
            .spill
            .unwrap_or(Rect::new(addr.row, addr.col, addr.row, addr.col));
        Some(FileFormula {
            text: to_file(self, &shown, Some(addr)),
            kind: FileFormulaKind::Dynamic { spill },
        })
    }

    pub fn file_name_formula(&self, def: &NameDef) -> String {
        to_file(self, &def.refers_to, None)
    }

    pub fn data_tables(&self) -> &[DataTable] {
        &self.data_tables
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ff(wb: &mut Workbook, cell: (u32, u32), input: &str) -> FileFormula {
        let s = wb.sheet_at(0).id;
        let a = CellAddr::new(s, cell.0, cell.1);
        wb.set_formula(a, input).unwrap();
        wb.file_formula(a).unwrap()
    }

    #[test]
    fn prefixes_and_kinds() {
        let mut wb = Workbook::new();
        let f = ff(&mut wb, (0, 0), "=SUM(B1:B3)");
        assert_eq!(f.text, "SUM(B1:B3)");
        assert_eq!(f.kind, FileFormulaKind::Plain);
        let f = ff(&mut wb, (0, 2), "=SEQUENCE(3)");
        assert_eq!(f.text, "_xlfn.SEQUENCE(3)");
        assert!(matches!(f.kind, FileFormulaKind::Dynamic { spill } if spill.r1 == 2));
        let f = ff(&mut wb, (5, 0), "=SORT(C1#)");
        assert_eq!(f.text, "_xlfn._xlws.SORT(_xlfn.ANCHORARRAY(C1))");
        let f = ff(&mut wb, (9, 0), "=LET(x,2,y,x+1,LAMBDA(z,z*y)(x))");
        assert_eq!(
            f.text,
            "_xlfn.LET(_xlpm.x,2,_xlpm.y,_xlpm.x+1,_xlfn.LAMBDA(_xlpm.z,_xlpm.z*_xlpm.y)(_xlpm.x))"
        );
        let f = ff(&mut wb, (10, 0), "=@B1:B3+1");
        assert_eq!(f.text, "B1:B3+1");
        assert_eq!(f.kind, FileFormulaKind::Plain);
        let f = ff(&mut wb, (11, 0), "=B1:B3+1");
        assert_eq!(f.text, "B1:B3+1");
        assert!(matches!(f.kind, FileFormulaKind::Dynamic { .. }));
    }
}
