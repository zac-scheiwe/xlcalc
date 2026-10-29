use crate::address::{col_to_letters, CellAddr, MAX_COLS, MAX_ROWS};
use crate::formula::{CRef, Node, Rc, SheetRef};
use crate::functions;
use crate::number::format_literal;
use crate::parser::ast::{BinOp, UnaryOp};
use crate::parser::print::{sheet_name_text, text_literal};
use crate::value::Value;
use crate::workbook::Workbook;

pub fn print_node(wb: &Workbook, node: &Node, host: CellAddr) -> String {
    let mut s = String::new();
    write(wb, node, host, &mut s);
    s
}

fn literal(v: &Value) -> String {
    match v {
        Value::Number(n) => format_literal(*n),
        Value::Text(t) => text_literal(t),
        Value::Bool(b) => if *b { "TRUE" } else { "FALSE" }.into(),
        Value::Error(e) => e.as_str().into(),
        Value::Array(a) => {
            let mut s = String::from("{");
            for r in 0..a.rows {
                if r > 0 {
                    s.push(';');
                }
                for c in 0..a.cols {
                    if c > 0 {
                        s.push(',');
                    }
                    s.push_str(&literal(a.get(r, c)));
                }
            }
            s.push('}');
            s
        }
        Value::Blank | Value::Lambda(_) => String::new(),
    }
}

fn coord(rc: Rc, host: u32, max: u32) -> Option<(u32, bool)> {
    rc.resolve(host, max, false).map(|v| (v, rc.abs))
}

fn col_text(c: (u32, bool)) -> String {
    format!("{}{}", if c.1 { "$" } else { "" }, col_to_letters(c.0))
}

fn row_text(r: (u32, bool)) -> String {
    format!("{}{}", if r.1 { "$" } else { "" }, r.0 + 1)
}

pub fn cref_text(cref: &CRef, host: CellAddr) -> Option<String> {
    let row = |rc| coord(rc, host.row, MAX_ROWS);
    let col = |rc| coord(rc, host.col, MAX_COLS);
    Some(match *cref {
        CRef::Cell { row: r, col: c } => format!("{}{}", col_text(col(c)?), row_text(row(r)?)),
        CRef::Area { r0, c0, r1, c1 } => {
            let (rr0, rr1, cc0, cc1) = (row(r0)?, row(r1)?, col(c0)?, col(c1)?);
            if cc0.0.min(cc1.0) == 0 && cc0.0.max(cc1.0) == MAX_COLS - 1 && cc0.1 == cc1.1 {
                return Some(format!("{}:{}", row_text(rr0), row_text(rr1)));
            }
            if rr0.0.min(rr1.0) == 0 && rr0.0.max(rr1.0) == MAX_ROWS - 1 && rr0.1 == rr1.1 {
                return Some(format!("{}:{}", col_text(cc0), col_text(cc1)));
            }
            format!(
                "{}{}:{}{}",
                col_text(col(c0)?),
                row_text(row(r0)?),
                col_text(col(c1)?),
                row_text(row(r1)?)
            )
        }
        CRef::Cols { c0, c1 } => {
            let (a, b) = (col(c0)?, col(c1)?);
            if a.0.min(b.0) == 0 && a.0.max(b.0) == MAX_COLS - 1 && a.1 == b.1 {
                format!("$1:${MAX_ROWS}")
            } else {
                format!("{}:{}", col_text(a), col_text(b))
            }
        }
        CRef::Rows { r0, r1 } => format!("{}:{}", row_text(row(r0)?), row_text(row(r1)?)),
    })
}

fn sheet_prefix(wb: &Workbook, sheet: &SheetRef) -> String {
    let name = |id| wb.sheet(id).map(|s| s.name.clone());
    match sheet {
        SheetRef::Current => String::new(),
        SheetRef::Sheet(id) => match name(*id) {
            Some(n) => format!("{}!", sheet_name_text(&n)),
            None => "#REF!".into(),
        },
        SheetRef::Span(a, b) => match (name(*a), name(*b)) {
            (Some(x), Some(y)) => {
                let (x, y) = match (wb.sheet_position(*a), wb.sheet_position(*b)) {
                    (Some(p), Some(q)) if p > q => (y, x),
                    _ => (x, y),
                };
                let combined = format!("{x}:{y}");
                if sheet_name_text(&x) == x && sheet_name_text(&y) == y {
                    format!("{combined}!")
                } else {
                    format!("'{}'!", combined.replace('\'', "''"))
                }
            }
            _ => "#REF!".into(),
        },
        SheetRef::Missing(n) => format!("{}!", sheet_name_text(n)),
        SheetRef::External(b) => format!("[{b}]!"),
    }
}

fn args(wb: &Workbook, args: &[Node], host: CellAddr, s: &mut String) {
    s.push('(');
    for (i, a) in args.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        write(wb, a, host, s);
    }
    s.push(')');
}

fn write_ws(wb: &Workbook, before: &str, node: &Node, after: &str, host: CellAddr, s: &mut String) {
    match before.split_once('\u{2}') {
        Some((outer, inner)) => {
            s.push_str(outer);
            let mut t = String::new();
            write(wb, node, host, &mut t);
            if t.ends_with(')') {
                t.pop();
                t.push_str(inner);
                t.push(')');
            }
            s.push_str(&t);
        }
        None => {
            s.push_str(before);
            write(wb, node, host, s);
        }
    }
    s.push_str(after);
}

fn write(wb: &Workbook, node: &Node, host: CellAddr, s: &mut String) {
    crate::deep(|| write_inner(wb, node, host, s))
}

fn write_inner(wb: &Workbook, node: &Node, host: CellAddr, s: &mut String) {
    match node {
        Node::Lit(v) => s.push_str(&literal(v)),
        Node::Ref(sheet, cref) => match cref_text(cref, host) {
            Some(t) => {
                s.push_str(&sheet_prefix(wb, sheet));
                s.push_str(&t);
            }
            None => {
                s.push_str(&sheet_prefix(wb, sheet));
                s.push_str("#REF!");
            }
        },
        Node::Name {
            key,
            display,
            sheet,
        } => {
            if let Some(sh) = sheet {
                let book_level = matches!(sh, SheetRef::Sheet(id)
                    if wb.sheet(*id).is_some()
                        && wb.names.get(key, Some(*id)).is_none()
                        && !wb.names.was_deleted(key, *id)
                        && wb.names.get(key, None).is_some());
                match sheet_prefix(wb, sh).as_str() {
                    _ if book_level => s.push_str("Book1!"),

                    "#REF!" => s.push_str("Book1!"),
                    p => s.push_str(p),
                }
            }

            match wb.names.lookup(key, host.sheet) {
                Some(def) => s.push_str(&def.name),
                None => {
                    let t = wb
                        .names
                        .placeholder(&display.to_uppercase())
                        .map_or(&**display, |p| &**p);

                    if crate::address::parse_a1_cell(t).is_some() {
                        s.push('\'');
                        s.push_str(t);
                        s.push('\'');
                    } else {
                        s.push_str(t);
                    }
                }
            }
        }
        Node::NamedCall {
            key,
            display,
            args: a,
        } if wb.names.lookup(key, host.sheet).is_some() => {
            s.push_str(&wb.names.lookup(key, host.sheet).unwrap().name);
            let _ = display;
            args(wb, a, host, s);
        }
        Node::Local { name, .. } => s.push_str(name),

        Node::Unary(UnaryOp::Neg, x) if matches!(**x, Node::Lit(Value::Number(_))) => {
            let Node::Lit(Value::Number(n)) = **x else {
                unreachable!()
            };
            s.push_str(&format_literal(-n));
        }

        Node::Unary(UnaryOp::Plus, x) if matches!(**x, Node::Lit(Value::Number(_))) => {
            write(wb, x, host, s);
        }
        Node::Unary(op, x) => {
            s.push_str(match op {
                UnaryOp::Neg => "-",
                UnaryOp::Plus => "+",
                UnaryOp::Implicit => "@",
                UnaryOp::ImplicitQuiet | UnaryOp::ArrayZero => "",
            });
            write(wb, x, host, s);
        }
        Node::Percent(x) => {
            write(wb, x, host, s);
            s.push('%');
        }
        Node::Spill(x) => {
            write(wb, x, host, s);
            s.push('#');
        }
        Node::Binary(op, a, b) => {
            write(wb, a, host, s);

            let typed = matches!(&**a, Node::Ws { after, .. } if !after.is_empty());
            if !(*op == BinOp::Intersect && typed) {
                s.push_str(op.symbol());
            }
            write(wb, b, host, s);
        }
        Node::Paren(x) => {
            s.push('(');
            write(wb, x, host, s);
            s.push(')');
        }
        Node::Func { id, args: a }
            if functions::def(*id).name == functions::info::FIELD_FN && a.len() >= 2 =>
        {
            let mut base = String::new();
            write(wb, &a[0], host, &mut base);

            match base.split_once(':') {
                Some((first, rest)) if a.len() == 3 => {
                    s.push_str(&format!("'{first}':{rest}"));
                }
                _ => s.push_str(&base),
            }
            if let Node::Lit(Value::Text(f)) = &a[1] {
                s.push_str(f);
            }
        }

        Node::Func { id, args: a }
            if functions::def(*id).name == crate::datatable::TABLE_FN && a.len() >= 2 =>
        {
            s.push_str("TABLE");
            args(wb, &a[..2], host, s);
        }
        Node::Func { id, args: a } => {
            s.push_str(functions::def(*id).name);
            args(wb, a, host, s);
        }
        Node::NamedCall {
            display, args: a, ..
        } => {
            s.push_str(
                wb.names
                    .placeholder(&display.to_uppercase())
                    .map_or(&**display, |p| &**p),
            );
            args(wb, a, host, s);
        }
        Node::Call { callee, args: a } => {
            write(wb, callee, host, s);
            args(wb, a, host, s);
        }
        Node::Let {
            names,
            values,
            body,
            ..
        } => {
            s.push_str("LET(");
            for (n, v) in names.iter().zip(values) {
                let v = match v {
                    Node::Ws {
                        before,
                        after,
                        node,
                    } if before.starts_with('\u{3}') => {
                        let (name_ws, rest) = before[1..].split_once('\u{3}').unwrap_or(("", ""));
                        s.push_str(name_ws);
                        s.push_str(n);
                        s.push(',');
                        write_ws(wb, rest, node, after, host, s);
                        s.push(',');
                        continue;
                    }
                    v => v,
                };
                s.push_str(n);
                s.push(',');
                write(wb, v, host, s);
                s.push(',');
            }
            write(wb, body, host, s);
            s.push(')');
        }
        Node::Lambda(def) => {
            s.push_str("LAMBDA(");
            for (i, p) in def.params.iter().enumerate() {
                if i >= def.required {
                    s.push('[');
                    s.push_str(p);
                    s.push(']');
                } else {
                    s.push_str(p);
                }
                s.push(',');
            }
            write(wb, &def.body, host, s);
            s.push(')');
        }
        Node::Missing => {}
        Node::Unsupported { text, .. } => s.push_str(text),
        Node::Ws {
            before,
            after,
            node,
        } => write_ws(wb, before, node, after, host, s),
        Node::StructRef {
            table,
            implicit,
            spec,
        } => {
            let name = wb
                .table_by_name(table)
                .map(|t| t.name.clone())
                .unwrap_or_else(|| table.to_string());

            let whole = spec.cols.is_none() && spec.items == crate::workbook::tables::Items::DATA;
            let single = spec.cols.as_ref().is_some_and(|(a, b)| a == b)
                && !spec.range
                && (spec.items.this_row || spec.items == crate::workbook::tables::Items::DATA)
                && !spec.explicit_data;
            let inside = !whole
                && single
                && wb
                    .table_by_name(table)
                    .is_some_and(|t| t.contains(host.sheet, host.row, host.col));

            let outside = !wb
                .table_by_name(table)
                .is_some_and(|t| t.contains(host.sheet, host.row, host.col));
            s.push_str(&crate::workbook::tables::spec_text(
                (outside || (!*implicit && !inside)).then_some(name.as_str()),
                spec,
            ));
        }
        Node::DeadRef(sheet) => {
            s.push_str(&sheet_prefix(wb, sheet));
            s.push_str("#REF!");
        }
    }
}
