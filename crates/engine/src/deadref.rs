use crate::formula::{walk, Node, SheetRef};
use crate::parser::ast::{BinOp, UnaryOp};
use crate::value::Text;

pub(crate) fn settle(root: Node) -> Node {
    let mut dead = false;
    walk(&root, &mut |n| {
        dead |= matches!(n, Node::DeadRef(SheetRef::Current));
    });
    if !dead {
        return root;
    }
    let mut root = root;
    visit(&mut root, &mut String::new());
    root
}

fn empty() -> Text {
    Text::from("")
}

fn before(n: &mut Node) -> &mut Text {
    ensure_ws(n);
    match n {
        Node::Ws { before, .. } => before,
        _ => unreachable!(),
    }
}

fn after(n: &mut Node) -> &mut Text {
    ensure_ws(n);
    match n {
        Node::Ws { after, .. } => after,
        _ => unreachable!(),
    }
}

fn ensure_ws(n: &mut Node) {
    if !matches!(n, Node::Ws { .. }) {
        let inner = std::mem::replace(n, Node::Missing);
        *n = Node::Ws {
            before: empty(),
            after: empty(),
            node: Box::new(inner),
        };
    }
}

fn land(slot: &mut Text, o: &mut String) {
    if slot.is_empty() {
        *slot = Text::from(std::mem::take(o));
    } else {
        o.clear();
    }
}

fn land_after(n: &mut Node, o: &mut String) {
    if !o.is_empty() {
        land(after(n), o);
    }
}

fn push_front(n: &mut Node, x: &str) {
    crate::deep(|| push_front_inner(n, x))
}

fn push_front_inner(n: &mut Node, x: &str) {
    match n {
        Node::Binary(_, a, _) => push_front(a, x),
        Node::Percent(a) | Node::Spill(a) => push_front(a, x),
        Node::Ws { before, node, .. } => {
            if matches!(
                **node,
                Node::Binary(..) | Node::Percent(_) | Node::Spill(_) | Node::Ws { .. }
            ) {
                push_front(node, x);
            } else {
                *before = Text::from(format!("{x}{before}"));
            }
        }
        other => {
            let b = before(other);
            *b = Text::from(format!("{x}{b}"));
        }
    }
}

fn visit(n: &mut Node, o: &mut String) {
    crate::deep(|| visit_inner(n, o))
}

fn visit_inner(n: &mut Node, o: &mut String) {
    match n {
        Node::Ws { before, node, .. } => {
            if !before.is_empty()
                && matches!(**node, Node::Binary(..) | Node::Percent(_) | Node::Spill(_))
            {
                let x = std::mem::replace(before, empty());
                push_front(node, &x);
            }
            visit_kind(node, before, o);
        }
        _ => {
            let mut pre = empty();
            visit_kind(n, &mut pre, o);
            if !pre.is_empty() {
                *before(n) = pre;
            }
        }
    }
}

fn visit_kind(k: &mut Node, pre: &mut Text, o: &mut String) {
    match k {
        Node::DeadRef(SheetRef::Current) => {
            let own = std::mem::replace(pre, empty());
            if !own.is_empty() {
                *o = own.to_string();
            }
        }

        Node::Unary(UnaryOp::Neg | UnaryOp::Plus, x)
            if matches!(x.unwrap_ws(), Node::Lit(crate::value::Value::Number(_))) =>
        {
            land(pre, o);
        }
        Node::Unary(op, x) => {
            visit(x, o);
            if !matches!(op, UnaryOp::ImplicitQuiet | UnaryOp::ArrayZero) {
                land(pre, o);
            }
        }
        Node::Percent(x) | Node::Spill(x) => {
            visit(x, o);
            land_after(x, o);
        }
        Node::Binary(op, a, b) => {
            visit(a, o);
            visit(b, o);
            if *op == BinOp::Intersect {
                o.clear();
            } else {
                land_after(a, o);
            }
        }
        Node::Paren(x) => {
            visit(x, o);
            if !o.is_empty() {
                if pre.is_empty() {
                    *pre = Text::from(std::mem::take(o));
                } else if after(x).is_empty() {
                    *after(x) = Text::from(std::mem::take(o));
                }
            }
        }
        Node::Func { args, .. } => {
            args.iter_mut().for_each(|a| visit(a, o));
            land(pre, o);
        }
        Node::NamedCall { args, .. } => {
            args.iter_mut().for_each(|a| visit(a, o));
            land(pre, o);
        }
        Node::Call { callee, args } => {
            visit(callee, o);
            args.iter_mut().for_each(|a| visit(a, o));
            land(pre, o);
        }
        Node::Let { values, body, .. } => {
            values.iter_mut().for_each(|v| visit(v, o));
            visit(body, o);
            o.clear();
        }
        Node::Lambda(_) => o.clear(),
        Node::Ws { .. } => visit_inner(k, o),
        Node::Lit(_)
        | Node::Ref(..)
        | Node::Name { .. }
        | Node::Local { .. }
        | Node::Missing
        | Node::Unsupported { .. }
        | Node::DeadRef(_)
        | Node::StructRef { .. } => land(pre, o),
    }
}
