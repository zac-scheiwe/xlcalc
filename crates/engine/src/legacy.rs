use std::sync::Arc;

use rustc_hash::FxHashSet;

use crate::address::{CellAddr, SheetId};
use crate::calc::RecalcReport;
use crate::functions;
use crate::input::{self, Entered};
use crate::parser::ast::{BinOp, Expr, RefKind, SheetSpec, UnaryOp};
use crate::parser::{parse, print::print};
use crate::workbook::names::NameIndex;
use crate::workbook::{notice, Command};
use crate::workbook::{EditError, Workbook};

thread_local! {

    static LEGACY_ENTRY: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };

    static LINKED_NAMES: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };

    static TABLES: std::cell::RefCell<Vec<(String, bool)>> = const { std::cell::RefCell::new(Vec::new()) };

    static HOST_TABLE: std::cell::RefCell<Option<String>> = const { std::cell::RefCell::new(None) };

    static NAME_DEFS: std::cell::RefCell<Option<Arc<NameIndex>>> = const { std::cell::RefCell::new(None) };

    static SHEET_IDS: std::cell::RefCell<Vec<(String, SheetId)>> = const { std::cell::RefCell::new(Vec::new()) };

    static HOST_SHEET: std::cell::RefCell<Option<String>> = const { std::cell::RefCell::new(None) };

    static KNOWN_NAMES: std::cell::RefCell<Option<Known>> = const { std::cell::RefCell::new(None) };

    static IN_AGG: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

struct Known {
    names: Arc<NameIndex>,
    excluded: FxHashSet<String>,
}

fn undefined_name(name: &str) -> bool {
    let up = name.to_uppercase();
    KNOWN_NAMES.with(|k| {
        k.borrow()
            .as_ref()
            .is_some_and(|v| !v.names.all.contains(&up) || v.excluded.contains(&up))
    })
}

fn is_span3d(e: &Expr) -> bool {
    matches!(e, Expr::Ref { sheet: Some(SheetSpec::Range(a, b)), .. } if !a.eq_ignore_ascii_case(b))
}

pub(crate) enum StructKind {
    Single,

    Column(String),

    Multi,
}

const GRB_ALL: u32 = 0x01;
const GRB_HEADERS: u32 = 0x02;
const GRB_DATA: u32 = 0x04;
const GRB_TOTALS: u32 = 0x08;
const GRB_THIS_ROW: u32 = 0x10;

fn split_top(s: &str, sep: char) -> Vec<&str> {
    let (mut depth, mut esc, mut start) = (0u32, false, 0usize);
    let mut out = Vec::new();
    for (i, c) in s.char_indices() {
        if esc {
            esc = false;
            continue;
        }
        match c {
            '\'' => esc = true,
            '[' => depth += 1,
            ']' => depth = depth.saturating_sub(1),
            c if c == sep && depth == 0 => {
                out.push(s[start..i].trim());
                start = i + c.len_utf8();
            }
            _ => {}
        }
    }
    out.push(s[start..].trim());
    out
}

fn struct_inner(s: &str) -> &str {
    let s = s.trim();
    s.strip_prefix('[')
        .and_then(|x| x.strip_suffix(']'))
        .unwrap_or(s)
}

fn struct_shape(spec: &str) -> Option<(usize, u32, Option<String>)> {
    let spec = spec.trim();
    let (spec, this_row) = match spec.strip_prefix('@') {
        Some(rest) => (rest.trim(), true),
        None => (spec, false),
    };
    let (mut grbit, mut cols, mut bare) = (if this_row { GRB_THIS_ROW } else { 0 }, 0usize, None);
    let items: Vec<&str> = if spec.is_empty() {
        vec![]
    } else if spec.starts_with('[') || spec.starts_with('#') {
        split_top(spec, ',')
    } else {
        vec![spec]
    };
    let one_item = items.len() == 1 && !this_row;
    for it in &items {
        let ends = split_top(it, ':');
        let name = struct_inner(ends[0]);
        let row_bit = match name.to_lowercase().as_str() {
            "#all" => Some(GRB_ALL),
            "#headers" => Some(GRB_HEADERS),
            "#data" => Some(GRB_DATA),
            "#totals" => Some(GRB_TOTALS),
            "#this row" => Some(GRB_THIS_ROW),
            n if n.starts_with('#') => return None,
            _ => None,
        };
        match (row_bit, ends.get(1)) {
            (Some(b), None) => grbit |= b,
            (Some(_), Some(_)) => return None,
            (None, None) => {
                cols += 1;
                if one_item {
                    bare = Some(name.to_string());
                }
            }

            (None, Some(_)) => cols += 2,
        }
    }
    Some((cols, grbit, bare))
}

pub(crate) fn struct_kind(spec: &str) -> StructKind {
    let Some((cols, grbit, bare)) = struct_shape(spec) else {
        return StructKind::Multi;
    };
    if cols == 1 && matches!(grbit, GRB_HEADERS | GRB_TOTALS | GRB_THIS_ROW) {
        return StructKind::Single;
    }
    match bare {
        Some(name) if cols == 1 && grbit == 0 => StructKind::Column(name),
        _ => StructKind::Multi,
    }
}

fn is_single_cell(e: &Expr) -> bool {
    match e.unwrap_ws() {
        Expr::StructRef { spec, .. } => matches!(struct_kind(spec), StructKind::Single),

        x @ Expr::Name { .. } => defined_name(x).is_some_and(|i| i & 3 == 0),
        x => {
            matches!(
                x,
                Expr::Ref {
                    kind: RefKind::Cell { .. },
                    ..
                }
            ) && !is_span3d(x)
        }
    }
}

fn is_undefined_name(e: &Expr) -> bool {
    matches!(e.unwrap_ws(), Expr::Name { sheet: None, name } if undefined_name(name))
}

fn name_out_of_scope(e: &Expr) -> bool {
    match e {
        Expr::Name {
            sheet: Some(SheetSpec::Single(_)),
            ..
        } => true,
        Expr::Name { name, sheet: None } => {
            let up = name.to_uppercase();
            HOST_SHEET.with(|h| h.borrow().is_some())
                && NAME_DEFS.with(|c| {
                    c.borrow()
                        .as_ref()
                        .is_some_and(|i| i.merged.contains_key(&up))
                })
        }
        _ => false,
    }
}

fn is_linked_name(e: &Expr) -> bool {
    let Expr::Name {
        name,
        sheet: Some(SheetSpec::Single(book)),
    } = e
    else {
        return false;
    };
    let key = format!("{}!{}", book.to_uppercase(), name.to_uppercase());
    LINKED_NAMES.with(|m| m.borrow().contains(&key))
}

fn is_table_name(name: &str) -> bool {
    let up = name.to_uppercase();
    TABLES.with(|t| t.borrow().iter().any(|(n, _)| *n == up))
}

pub(crate) fn table_has_many_rows(table: Option<&str>) -> bool {
    let key = match table {
        Some(t) => Some(t.to_uppercase()),
        None => HOST_TABLE.with(|h| h.borrow().clone()),
    };
    key.is_some_and(|k| TABLES.with(|t| t.borrow().iter().any(|(n, many)| *n == k && *many)))
}

fn is_multi_ref(e: &Expr) -> bool {
    match e {
        Expr::StructRef { spec, .. } => !matches!(struct_kind(spec), StructKind::Single),
        Expr::Name { name, .. } => {
            if let Some(info) = defined_name(e) {
                return info & 3 != 0;
            }

            is_table_name(name)
        }

        Expr::Ref { kind, .. } => !matches!(kind, RefKind::Cell { .. }) || is_span3d(e),

        Expr::Binary {
            op: BinOp::Intersect,
            left,
            right,
        } => !is_single_cell(left) && !is_single_cell(right),
        Expr::Binary {
            op: BinOp::Range, ..
        } => true,

        Expr::Spill(_) => true,
        _ => false,
    }
}

pub(crate) type OneDim = Option<String>;

fn one_dimensional(e: &Expr) -> Option<OneDim> {
    match e.unwrap_ws() {
        n @ Expr::Name { .. } => defined_name_raw(n).and_then(|(_, d, _)| d),
        x => ptg_1d(x),
    }
}

fn ptg_1d(e: &Expr) -> Option<OneDim> {
    match e {
        Expr::StructRef { table, spec } => {
            let (cols, grbit, _) = struct_shape(spec)?;
            (cols == 1 && grbit & !GRB_DATA == 0).then(|| match table {
                Some(t) => Some(t.to_uppercase()),
                None => HOST_TABLE.with(|h| h.borrow().clone()),
            })
        }
        x => area_1d(x).then_some(None),
    }
}

pub(crate) fn at(e: Expr) -> Expr {
    Expr::Unary {
        op: UnaryOp::Implicit,
        expr: Box::new(e),
    }
}

fn strip_at(e: Expr) -> Expr {
    crate::deep(|| strip_at_inner(e))
}

fn strip_at_inner(e: Expr) -> Expr {
    match e {
        Expr::Unary {
            op: UnaryOp::Implicit,
            expr,
        } => strip_at(*expr),
        Expr::Unary { op, expr } => Expr::Unary {
            op,
            expr: Box::new(strip_at(*expr)),
        },
        Expr::Ws {
            before,
            after,
            expr,
        } => Expr::Ws {
            before,
            after,
            expr: Box::new(strip_at(*expr)),
        },
        Expr::Paren(x) => Expr::Paren(Box::new(strip_at(*x))),
        Expr::Percent(x) => Expr::Percent(Box::new(strip_at(*x))),
        Expr::Spill(x) => Expr::Spill(Box::new(strip_at(*x))),
        Expr::Binary { op, left, right } => Expr::Binary {
            op,
            left: Box::new(strip_at(*left)),
            right: Box::new(strip_at(*right)),
        },
        Expr::Func { name, args } => Expr::Func {
            name,
            args: args.into_iter().map(strip_at).collect(),
        },
        Expr::Call { callee, args } => Expr::Call {
            callee: Box::new(strip_at(*callee)),
            args: args.into_iter().map(strip_at).collect(),
        },
        e => e,
    }
}

fn strip_known_prefixes(e: Expr) -> Expr {
    let go = |x: Box<Expr>| Box::new(strip_known_prefixes(*x));
    match e {
        Expr::Func { name, args } => {
            let bare = crate::formula::strip_prefixes(&name);
            let u = bare.to_uppercase();
            let known = matches!(u.as_str(), "LET" | "LAMBDA") || functions::lookup(&u).is_some();
            Expr::Func {
                name: if known { bare.to_string() } else { name },
                args: args.into_iter().map(strip_known_prefixes).collect(),
            }
        }
        Expr::Unary { op, expr } => Expr::Unary { op, expr: go(expr) },
        Expr::Ws {
            before,
            after,
            expr,
        } => Expr::Ws {
            before,
            after,
            expr: go(expr),
        },
        Expr::Paren(x) => Expr::Paren(go(x)),
        Expr::Percent(x) => Expr::Percent(go(x)),
        Expr::Spill(x) => Expr::Spill(go(x)),
        Expr::Binary { op, left, right } => Expr::Binary {
            op,
            left: go(left),
            right: go(right),
        },
        Expr::Call { callee, args } => Expr::Call {
            callee: go(callee),
            args: args.into_iter().map(strip_known_prefixes).collect(),
        },
        e => e,
    }
}

pub(crate) fn legacy_to_dynamic_in(wb: &Workbook, text: &str) -> String {
    let text = input::auto_close_call(text);
    let Ok(e) = parse(&text) else {
        return format!("={text}");
    };
    let e = strip_known_prefixes(e);

    let known = Known {
        names: wb.names.index(),
        excluded: wb.names.macro_keys(),
    };
    KNOWN_NAMES.with(|k| *k.borrow_mut() = Some(known));
    let out = format!("={}", print(&crate::legacy_pass::annotate(e)));
    KNOWN_NAMES.with(|k| *k.borrow_mut() = None);
    out
}

fn mark_quoted_names(t: &str) -> String {
    let b: Vec<char> = t.chars().collect();
    let mut out = String::new();
    let (mut i, mut in_str) = (0, false);
    while i < b.len() {
        if b[i] == '"' {
            in_str = !in_str;
        }
        if !in_str && b[i] == '\'' {
            if let Some(len) = b[i + 1..].iter().position(|&c| c == '\'') {
                let w: String = b[i + 1..i + 1 + len].iter().collect();
                let after = b.get(i + len + 2);
                if crate::parser::r1c1::forced_name(&w)
                    && !matches!(after, Some('!' | ':'))
                    && !(i > 0 && matches!(b[i - 1], '!' | ':'))
                {
                    out.push('\u{1}');
                    out.push_str(&w);
                    i += len + 2;
                    continue;
                }
            }
        }
        out.push(b[i]);
        i += 1;
    }
    out
}

fn marks_to_quotes(t: &str) -> String {
    let mut out = String::new();
    let mut word = false;
    for c in t.chars() {
        if c == '\u{1}' {
            out.push('\'');
            word = true;
        } else if word && !(c.is_alphanumeric() || matches!(c, '_' | '.' | '\\' | '?')) {
            out.push('\'');
            out.push(c);
            word = false;
        } else {
            out.push(c);
        }
    }
    if word {
        out.push('\'');
    }
    out
}

fn legacy_loses_marks(wb: &Workbook, host: CellAddr, legacy: &str, shown: &str) -> bool {
    let Some(body) = legacy.strip_prefix('=') else {
        return false;
    };
    load_env(wb, host);
    let da = legacy_to_dynamic_in(wb, &mark_quoted_names(body));
    clear_names();
    marks_to_quotes(&da) != shown
}

pub(crate) fn legacy_reads_back(wb: &Workbook, host: CellAddr, legacy: &str, shown: &str) -> bool {
    let Some(body) = legacy.strip_prefix('=') else {
        return false;
    };
    load_env(wb, host);
    let da = legacy_to_dynamic_in(wb, body);
    clear_names();
    da == shown
}

pub(crate) fn legacy_entry_active() -> bool {
    LEGACY_ENTRY.with(|l| l.get())
}

pub(crate) fn legacy_entry_is_scalar(wb: &Workbook, shown: &str) -> bool {
    let Some(body) = shown.strip_prefix('=') else {
        return false;
    };

    if body.contains('[') || body.contains("#REF!") || body.contains("Book1!") {
        return false;
    }
    let Ok(e) = parse(body) else {
        return false;
    };
    UNDEFINED.with(|u| u.set(false));
    uses_name(&e, wb) == Some(true)
}

impl Workbook {
    pub fn set_legacy_input(
        &mut self,
        addr: CellAddr,
        input: &str,
    ) -> Result<RecalcReport, EditError> {
        self.command(Command::Entry, |wb| {
            wb.note_changed(addr.sheet, &[notice::cell(addr)]);
            wb.set_legacy_input_run(addr, input)
        })
    }

    fn set_legacy_input_run(
        &mut self,
        addr: CellAddr,
        input: &str,
    ) -> Result<RecalcReport, EditError> {
        match input::interpret(input, &self.input_env()) {
            Entered::Formula(text)
                if text.starts_with('=') || text.trim_matches(' ').is_empty() =>
            {
                Err(EditError::Parse(crate::parser::ParseError {
                    pos: 0,
                    message: "empty or doubled equals sign".into(),
                }))
            }
            Entered::Formula(text) => {
                let text = crate::parser::r1c1::rewrite(&text, addr.row, addr.col).unwrap_or(text);
                load_env(self, addr);
                let da = legacy_to_dynamic_in(self, &text);
                clear_names();
                LEGACY_ENTRY.with(|l| l.set(true));
                let r = self.set_input(addr, &da);
                LEGACY_ENTRY.with(|l| l.set(false));
                r
            }
            _ => self.set_input(addr, input),
        }
    }

    pub fn legacy_formula_text(&self, addr: CellAddr) -> Option<String> {
        let t = self.formula_text(addr)?;

        if self.array_at(addr).is_some() {
            return Some(t);
        }
        match parse(&mark_quoted_names(&t)) {
            Ok(e) => {
                let out = marks_to_quotes(&format!("={}", print(&strip_at(e))));
                if out != t && legacy_loses_marks(self, addr, &out, &t) {
                    Some(t)
                } else {
                    Some(out)
                }
            }
            Err(_) => Some(t),
        }
    }

    pub fn legacy_formula_text_r1c1(&self, addr: CellAddr) -> Option<String> {
        let r1c1 = self.formula_text_r1c1(addr)?;
        let shown = self.formula_text(addr)?;
        let legacy = self.legacy_formula_text(addr)?;

        let marks = |t: &str| -> Vec<usize> {
            let (mut quote, mut out) = (None::<char>, Vec::new());
            for (i, c) in t.char_indices() {
                match (quote, c) {
                    (Some(q), c) if c == q => quote = None,
                    (Some(_), _) => {}
                    (None, '"' | '\'') => quote = Some(c),
                    (None, '@') => out.push(i),
                    _ => {}
                }
            }
            out
        };
        let (ms, ml, mr) = (marks(&shown), marks(&legacy), marks(&r1c1));
        if ms.len() != mr.len() || ml.len() > ms.len() {
            return Some(r1c1);
        }

        let (sb, lb) = (shown.as_bytes(), legacy.as_bytes());
        let (mut i, mut j, mut k) = (0, 0, 0);
        let mut drop = Vec::new();
        while i < sb.len() {
            if ms.get(k) == Some(&i) {
                if j < lb.len() && lb[j] == b'@' && ml.contains(&j) {
                    j += 1;
                } else {
                    drop.push(mr[k]);
                }
                k += 1;
                i += 1;
            } else if j < lb.len() && sb[i] == lb[j] {
                i += 1;
                j += 1;
            } else {
                return Some(r1c1);
            }
        }
        let mut out = r1c1;
        for at in drop.into_iter().rev() {
            out.remove(at);
        }
        Some(out)
    }
}

thread_local! {
    static UNDEFINED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

fn uses_name(e: &Expr, wb: &Workbook) -> Option<bool> {
    let mut names = Vec::new();
    collect_names(e, &mut names);
    if names.iter().any(|(_, n)| wb.table_by_name(n).is_some()) {
        return None;
    }

    let mut vars = Vec::new();
    let_vars(e, &mut vars);
    let defined = |q: &Option<SheetSpec>, n: &str| {
        let k = n.to_uppercase();

        if let Some(SheetSpec::Single(b)) = q {
            if wb.sheet_by_name(b).is_none() && wb.ext_name_scalar(b, n).is_some() {
                return true;
            }
        }
        wb.names.index().all.contains(&k) || vars.contains(&k)
    };
    if !names.iter().all(|(q, n)| defined(q, n)) {
        return None;
    }
    Some(!names.is_empty())
}

fn let_vars(e: &Expr, out: &mut Vec<String>) {
    crate::deep(|| match e {
        Expr::Func { name, args } => {
            if name.eq_ignore_ascii_case("LET") || name.eq_ignore_ascii_case("_xlfn.LET") {
                for a in args.iter().step_by(2).take(args.len() / 2) {
                    if let Expr::Name { name, .. } = a.unwrap_ws() {
                        out.push(name.to_uppercase());
                    }
                }
            }
            args.iter().for_each(|a| let_vars(a, out));
        }
        Expr::Ws { expr, .. }
        | Expr::Paren(expr)
        | Expr::Percent(expr)
        | Expr::Spill(expr)
        | Expr::Unary { expr, .. }
        | Expr::Field { expr, .. } => let_vars(expr, out),
        Expr::Binary { left, right, .. } => {
            let_vars(left, out);
            let_vars(right, out);
        }
        _ => {}
    })
}

fn collect_names<'a>(e: &'a Expr, out: &mut Vec<(&'a Option<SheetSpec>, &'a str)>) {
    crate::deep(|| match e {
        Expr::Name { name, sheet } => out.push((sheet, name)),
        Expr::Ws { expr, .. }
        | Expr::Paren(expr)
        | Expr::Percent(expr)
        | Expr::Spill(expr)
        | Expr::Unary { expr, .. }
        | Expr::Field { expr, .. } => collect_names(expr, out),
        Expr::Binary { left, right, .. } => {
            collect_names(left, out);
            collect_names(right, out);
        }
        Expr::Func { args, .. } => args.iter().for_each(|a| collect_names(a, out)),
        Expr::Call { callee, args } => {
            collect_names(callee, out);
            args.iter().for_each(|a| collect_names(a, out));
        }
        _ => {}
    })
}

fn first_ptg(e: &Expr) -> &Expr {
    match e {
        Expr::Ws { expr, .. }
        | Expr::Paren(expr)
        | Expr::Percent(expr)
        | Expr::Spill(expr)
        | Expr::Unary { expr, .. }
        | Expr::Field { expr, .. } => first_ptg(expr),

        Expr::Binary {
            op: BinOp::Range | BinOp::Intersect | BinOp::Union,
            ..
        } => e,
        Expr::Binary { left, .. } => first_ptg(left),
        Expr::Func { args, .. } => args.first().map_or(e, first_ptg),
        Expr::Call { callee, .. } => first_ptg(callee),
        e => e,
    }
}

fn name_def_info(def: &Expr) -> u8 {
    match def.unwrap_ws() {
        Expr::Number(n) => crate::legacy_single::int_flags(*n),
        Expr::Text(_) | Expr::Bool(_) => 0,

        Expr::Error(_) | Expr::DeadRef(_) => 0,
        x @ Expr::Ref { kind, .. } => match kind {
            RefKind::Cell { .. } if is_span3d(x) => 3,
            RefKind::Cell { .. } => 0,
            _ => 2,
        },
        Expr::Array(_) => 1,
        _ => 3,
    }
}

fn name_def_can_multi(def: &Expr) -> bool {
    match def.unwrap_ws() {
        Expr::Number(_) | Expr::Text(_) | Expr::Bool(_) | Expr::Error(_) => false,
        x @ Expr::Ref {
            kind: RefKind::Cell { .. },
            ..
        } => is_span3d(x),
        _ => true,
    }
}

fn name_def_1d(def: &Expr) -> Option<OneDim> {
    ptg_1d(first_ptg(def))
}

fn area_1d(e: &Expr) -> bool {
    let same = |a: &crate::parser::ast::Coord, b: &crate::parser::ast::Coord| a.abs == b.abs;
    match e {
        Expr::Ref {
            kind: RefKind::Area { r0, c0, r1, c1 },
            ..
        } => {
            same(r0, r1)
                && same(c0, c1)
                && ((c0.index == c1.index && r0.index != r1.index)
                    || (r0.index == r1.index && c0.index != c1.index))
        }
        Expr::Ref {
            kind: RefKind::Cols { c0, c1 },
            ..
        } => same(c0, c1) && c0.index == c1.index,
        Expr::Ref {
            kind: RefKind::Rows { r0, r1 },
            ..
        } => same(r0, r1) && r0.index == r1.index,
        _ => false,
    }
}

fn clear_names() {
    HOST_SHEET.with(|h| *h.borrow_mut() = None);
    LINKED_NAMES.with(|m| m.borrow_mut().clear());
    TABLES.with(|m| m.borrow_mut().clear());
    HOST_TABLE.with(|h| *h.borrow_mut() = None);
    NAME_DEFS.with(|c| *c.borrow_mut() = None);
    SHEET_IDS.with(|c| c.borrow_mut().clear());
}

fn set_host(wb: &Workbook, host: CellAddr) {
    let name = wb.sheet(host.sheet).map(|s| s.name.to_uppercase());
    HOST_SHEET.with(|h| *h.borrow_mut() = name);
    let table = wb.table_at(host).map(|t| t.name.to_uppercase());
    HOST_TABLE.with(|h| *h.borrow_mut() = table);
}

fn load_env(wb: &Workbook, host: CellAddr) {
    LINKED_NAMES.with(|m| *m.borrow_mut() = load_names(wb));
    set_host(wb, host);
}

pub(crate) type DefInfo = (u8, Option<OneDim>, bool);

pub(crate) fn definition_info(refers_to: &str) -> Option<DefInfo> {
    let def = parse(refers_to.trim_start_matches('=')).ok()?;
    Some((
        name_def_info(&def),
        name_def_1d(&def),
        name_def_can_multi(&def),
    ))
}

fn defined_name(e: &Expr) -> Option<u8> {
    defined_name_raw(e).map(|(i, ..)| i)
}

fn defined_name_raw(e: &Expr) -> Option<DefInfo> {
    match e.unwrap_ws() {
        Expr::Name { name, sheet } => {
            let up = name.to_uppercase();
            let names = NAME_DEFS.with(|c| c.borrow().clone())?;

            let in_sheet = |sheet: &str| {
                let id =
                    SHEET_IDS.with(|c| c.borrow().iter().find(|(n, _)| n == sheet).map(|x| x.1));
                id.and_then(|id| names.scoped.get(&(Some(id), up.clone())))
                    .or_else(|| names.scoped.get(&(None, up.clone())))
                    .cloned()
            };
            match sheet {
                Some(SheetSpec::Single(sh)) => in_sheet(&sh.to_uppercase()),
                Some(_) => None,
                None => match HOST_SHEET.with(|h| h.borrow().clone()) {
                    Some(host) => in_sheet(&host),

                    None => names.merged.get(&up).cloned(),
                },
            }
        }
        _ => None,
    }
}

fn load_names(wb: &Workbook) -> Vec<String> {
    NAME_DEFS.with(|c| *c.borrow_mut() = Some(wb.names.index()));
    SHEET_IDS.with(|c| {
        *c.borrow_mut() = wb.sheets().map(|s| (s.name.to_uppercase(), s.id)).collect();
    });
    TABLES.with(|c| {
        let mut tables = Vec::new();
        for t in wb.tables() {
            let (first, last) = t.data_rows();
            let many = last > first;
            tables.push((t.name.to_uppercase(), many));

            let display = t.disp.to_uppercase();
            if display != t.name.to_uppercase() {
                tables.push((display, many));
            }
        }
        *c.borrow_mut() = tables;
    });
    wb.ext_link_names()
}

pub(crate) fn with_info_env<T>(wb: &Workbook, host: CellAddr, f: impl FnOnce() -> T) -> T {
    load_env(wb, host);
    let known = Known {
        names: wb.names.index(),
        excluded: FxHashSet::default(),
    };
    let saved = KNOWN_NAMES.with(|k| k.replace(Some(known)));
    let out = f();
    KNOWN_NAMES.with(|k| *k.borrow_mut() = saved);
    clear_names();
    out
}

pub(crate) fn leaf_info(e: &Expr) -> u8 {
    match e.unwrap_ws() {
        x @ Expr::Name { .. } => {
            if is_linked_name(x) {
                3
            } else if let Some(info) = defined_name(x) {
                info
            } else if name_out_of_scope(x) {
                3
            } else if is_multi_ref(x) {
                2
            } else if is_undefined_name(x) {
                3
            } else {
                0
            }
        }
        x @ Expr::Ref { kind, .. } => match kind {
            RefKind::Cell { .. } if is_span3d(x) => 3,
            RefKind::Cell { .. } => 0,
            _ => 2,
        },
        x if is_multi_ref(x) => 2,
        _ => 0,
    }
}

pub(crate) fn name_root_can_multi(e: &Expr) -> Option<bool> {
    defined_name_raw(e.unwrap_ws()).map(|(_, _, m)| m)
}

pub(crate) fn expr_one_dimensional(e: &Expr) -> Option<OneDim> {
    one_dimensional(e)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dyn_of(t: &str) -> String {
        legacy_to_dynamic_in(&Workbook::new(), t)
    }

    #[test]
    fn let_variable_ends_with_its_let() {
        assert_eq!(dyn_of("LET(q,A1:A3,q)+q"), "=@LET(q,A1:A3,q)+@q");
        assert_eq!(dyn_of("LET(q,1,q)+q"), "=LET(q,1,q)+@q");
        assert_eq!(dyn_of("q+LET(q,1,q)"), "=@q+LET(q,1,q)");
    }

    #[test]
    fn xlookup_over_one_dimensional_arrays_is_not_wrapped() {
        for t in [
            "XLOOKUP(1,A1:A9,B1:B9)",
            "XLOOKUP(1,T[a],T[b])",
            "XLOOKUP(1,T[[#Data],[a]],T[b])",
        ] {
            assert_eq!(dyn_of(t), format!("={t}"), "{t}");
        }
        for t in [
            "XLOOKUP(1,T[a],U[b])",
            "XLOOKUP(1,A1:A9,T[b])",
            "XLOOKUP(1,T[[a]:[b]],T[c])",
            "XLOOKUP(1,T[[#All],[a]],T[b])",
            "XLOOKUP(TRUE,C2:C9>1,A2:A9)",
        ] {
            assert_eq!(dyn_of(t), format!("=@{t}"), "{t}");
        }
    }

    #[test]
    fn struct_kind_follows_cicol_and_grbit() {
        let single = |s: &str| matches!(struct_kind(s), StructKind::Single);
        for s in [
            "@a",
            "@[a b]",
            "[#This Row],[a]",
            "[#Headers],[a]",
            "[#Totals],[a]",
            "[#Headers],[a,b]",
        ] {
            assert!(single(s), "{s}");
        }
        for s in [
            "a",
            "[a]",
            "@",
            "@[a]:[b]",
            "@[a]:[A]",
            "[#Headers],[a]:[A]",
            "@[a],[b]",
            "#All",
            "#Headers",
            "[#Data],[a]",
            "[#All],[a]",
            "[#Headers],[#Data],[a]",
            "[#Totals],[#Headers],[a]",
            "[#Headers],[#This Row],[a]",
            "[#Headers],[a]:[b]",
            "[#Headers],[a],[b]",
            "[#Bogus],[a]",
        ] {
            assert!(!single(s), "{s}");
        }
        assert!(matches!(struct_kind("[a b]"), StructKind::Column(n) if n == "a b"));
        assert!(matches!(struct_kind("a"), StructKind::Column(n) if n == "a"));
        assert!(matches!(struct_kind("[#Data],[a]"), StructKind::Multi));
    }

    fn def(t: &str) -> Expr {
        parse(t).unwrap()
    }

    #[test]
    fn array_class_call_is_not_wrapped() {
        assert_eq!(
            dyn_of("TYPE(LEN(MMULT(A1,{1})))"),
            "=TYPE(LEN(MMULT(A1,{1})))"
        );
        assert_eq!(
            dyn_of("TYPE(TEXT(MMULT(LEN(LAMBDA(p,ROW(p)/0)(A1)),{1}),\"0\"))"),
            "=TYPE(TEXT(MMULT(LEN(LAMBDA(p,@ROW(p)/0)(A1)),{1}),\"0\"))"
        );
        assert_eq!(dyn_of("N({1;2;3})"), "=N(@{1;2;3})");
    }

    #[test]
    fn let_result_is_not_an_undefined_name() {
        assert_eq!(
            dyn_of("INDEX(LET(x,A1:A3,x*2),2)"),
            "=INDEX(LET(x,A1:A3,@x*2),2)"
        );
    }

    #[test]
    fn name_ptg_info() {
        assert_eq!(name_def_info(&def("Sheet1!#REF!")), 0);
        assert!(name_def_can_multi(&def("Sheet1!#REF!")));
        assert!(!name_def_can_multi(&def("Sheet2!$A$2")));
        assert!(name_def_can_multi(&def("Sheet1:Sheet3!$A$2")));
        assert!(name_def_can_multi(&def("Sheet2!$A$2:$A$4")));
        assert!(!name_def_can_multi(&def("5")));
        assert_eq!(name_def_info(&def("5")), 4);
        assert_eq!(name_def_info(&def("1")), 0xc);
        assert_eq!(name_def_info(&def("0")), 0);
        assert_eq!(name_def_info(&def("1.5")), 0);
        assert_eq!(name_def_info(&def("70000")), 0);
        assert_eq!(name_def_info(&def("\"a\"")), 0);
        assert_eq!(name_def_info(&def("Sheet2!$A$2")), 0);
        assert_eq!(name_def_info(&def("Sheet1:Sheet3!$A$2")), 3);
        assert_eq!(name_def_info(&def("Sheet2!$A$2:$A$4")), 2);
        assert_eq!(name_def_info(&def("{5}")), 1);

        assert_eq!(name_def_info(&def("(Sheet2!$A$2)")), 3);
        assert_eq!(name_def_info(&def("Sheet2!$A$2:$A$4*1")), 3);
        assert_eq!(name_def_info(&def("NOW()")), 3);
        assert_eq!(name_def_info(&def("nRng")), 3);
    }

    #[test]
    fn name_first_ptg_one_dimensional() {
        assert!(name_def_1d(&def("Sheet2!$A$2:$A$4")).is_some());
        assert!(name_def_1d(&def("Sheet2!$A$2:$C$2")).is_some());
        assert!(name_def_1d(&def("Sheet2!$A:$A")).is_some());

        assert!(name_def_1d(&def("INDEX(Sheet2!$B$2:$B$11,{1;2;3})")).is_some());
        assert!(name_def_1d(&def("Sheet2!$A$2:$A$4*1")).is_some());
        assert!(!name_def_1d(&def("Sheet2!$A$2:$B$4")).is_some());
        assert!(!name_def_1d(&def("Sheet2!$A$2:$A$2")).is_some());
        assert!(!name_def_1d(&def("Sheet2!$A$2")).is_some());

        assert!(!name_def_1d(&def("Sheet2!$A2:A$4")).is_some());

        assert!(!name_def_1d(&def("(Sheet2!$A$2:$A$4,Sheet2!$B$2:$B$4)")).is_some());
        assert!(!name_def_1d(&def("nRng")).is_some());
    }

    #[test]
    fn array_constant_in_value_parameter() {
        assert_eq!(dyn_of("DISC({1,2},A5,A5,A5)"), "=DISC(@{1,2},A5,A5,A5)");
        assert_eq!(dyn_of("INDEX(A1:B3,1,1,A1:A3)"), "=INDEX(A1:B3,1,1,@A1:A3)");
        assert_eq!(dyn_of("INDEX(A1:B3,1,1,A:A)"), "=INDEX(A1:B3,1,1,@A:A)");
        assert_eq!(
            dyn_of("INDEX(A1:B3,1,1,{1;2;3})"),
            "=INDEX(A1:B3,1,1,@{1;2;3})"
        );
        assert_eq!(
            dyn_of("INDEX(A1:B3,1,1,OFFSET(A1,0,0,3,1))"),
            "=INDEX(A1:B3,1,1,@OFFSET(A1,0,0,3,1))"
        );
        assert_eq!(dyn_of("INDEX(A1:A3,B1)"), "=@INDEX(A1:A3,B1)");
    }
}
