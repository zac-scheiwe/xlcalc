use rustc_hash::FxHashSet;

use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::eval::{eval_formula, EvalCtx, Interrupt, ReadMode};
use crate::graph::Prec;
use crate::value::{Value, XlError};
use crate::workbook::{current_serial, Entry, FormulaId, Op, Workbook};

#[derive(Clone, Debug, Default)]
pub struct RecalcReport {
    pub evaluations: usize,

    pub changed: Vec<CellAddr>,

    pub circular: Option<CellAddr>,
}

pub fn mark_dirty(wb: &mut Workbook, fid: FormulaId) {
    let mut work = vec![fid];
    let mut deps = Vec::new();
    while let Some(f) = work.pop() {
        let cell = wb.formula(f);
        if cell.dirty {
            continue;
        }
        let addr = cell.addr;

        if !wb.data_tables.is_empty() && crate::datatable::is_table_formula(&cell.formula) {
            crate::datatable::table_cell_dirtied(wb, addr);
            continue;
        }
        let spill = cell.spill;
        wb.set_dirty(f, true);
        wb.calc.dirty_list.push(f);
        deps.clear();
        match spill {
            Some(rect) => wb.graph.dependents_of_rect(addr.sheet, &rect, &mut deps),
            None => wb
                .graph
                .dependents_of_cell(addr.sheet, addr.row, addr.col, &mut deps),
        }
        work.extend(deps.iter().copied());
    }
}

pub fn cell_changed(wb: &mut Workbook, addr: CellAddr) {
    let mut deps = Vec::new();
    wb.graph
        .dependents_of_cell(addr.sheet, addr.row, addr.col, &mut deps);
    if let Some(sh) = wb.sheet(addr.sheet) {
        deps.extend(sh.spills.anchors_wanting(addr.row, addr.col));
    }
    for d in deps {
        if wb.formula(d).addr != addr {
            mark_dirty(wb, d);
        }
    }
}

pub fn rect_changed(wb: &mut Workbook, sheet: SheetId, rect: Rect) {
    let mut deps = Vec::new();
    wb.graph.dependents_of_rect(sheet, &rect, &mut deps);
    if let Some(sh) = wb.sheet(sheet) {
        deps.extend(sh.spills.anchors_wanting_rect(&rect));
    }
    for d in deps {
        mark_dirty(wb, d);
    }
}

pub fn name_changed(wb: &mut Workbook, key: &str) {
    let mut deps = Vec::new();
    wb.graph.dependents_of_name(key, &mut deps);

    for fid in deps {
        wb.refresh_scalar_entry(fid);
        wb.unlink_formula(fid);
        wb.link_formula(fid);

        if wb.formula(fid).dirty {
            wb.calc.dirty_list.push(fid);
        } else {
            mark_dirty(wb, fid);
        }
    }
}

pub fn recalc_only(wb: &mut Workbook, cells: &[CellAddr]) -> RecalcReport {
    let mut report = RecalcReport::default();
    wb.calc.pass += 1;
    for &a in cells {
        if let Some(fid) = wb.formula_at(a) {
            if wb.formula(fid).dirty {
                calc_root(wb, fid, &mut report);
            }
        }
    }
    finish_report(report)
}

pub fn recalc(wb: &mut Workbook) -> RecalcReport {
    let tables =
        wb.settings.mode == crate::workbook::CalcMode::Automatic && !wb.settings.except_tables;
    recalc_with(wb, tables)
}

pub fn recalc_with(wb: &mut Workbook, tables: bool) -> RecalcReport {
    let mut report = recalc_cells(wb);
    if tables && wb.data_tables.iter().any(|t| t.dirty) {
        crate::datatable::table_pass(wb, &mut report);
        report = finish_report(report);
    }
    report
}

pub(crate) fn recalc_cells(wb: &mut Workbook) -> RecalcReport {
    let mut report = RecalcReport::default();
    wb.calc.pass += 1;
    wb.calc.now = wb.calc.fixed_now.unwrap_or_else(current_serial);
    wb.calc.circular_ref = None;
    wb.calc.circ_iterating = wb.settings.iteration;
    let volatile: Vec<FormulaId> = wb.calc.volatile.iter().copied().collect();
    for fid in volatile {
        mark_dirty(wb, fid);
    }

    let volatile_names: Vec<String> = wb
        .names
        .iter()
        .filter(|d| d.formula.volatile)
        .map(|d| d.name.to_uppercase())
        .collect();
    let mut users = Vec::new();
    for key in volatile_names {
        users.clear();
        wb.graph.dependents_of_name(&key, &mut users);
        for fid in users.clone() {
            mark_dirty(wb, fid);
        }
    }

    let circular: Vec<FormulaId> = wb.calc.circular.drain().collect();
    for fid in circular {
        if wb.formulas.get(fid as usize).is_some_and(|f| f.is_some()) {
            if wb.formula(fid).dirty {
                wb.calc.dirty_list.push(fid);
            } else {
                mark_dirty(wb, fid);
            }
        }
    }

    let iterative = wb.settings.iteration;
    if !iterative {
        parallel_phase(wb, &mut report);
    }
    let budget = 64 * (wb.formulas.len() + 16) + 10_000;
    loop {
        let mut todo = std::mem::take(&mut wb.calc.dirty_list);
        todo.retain(|&f| {
            wb.formulas
                .get(f as usize)
                .is_some_and(|c| c.as_ref().is_some_and(|c| c.dirty))
        });
        if todo.is_empty() {
            break;
        }
        todo.sort_unstable_by_key(|&f| wb.formula(f).seq);
        todo.dedup();
        for fid in todo {
            let f = wb.formula(fid);
            if !f.dirty || (f.circular_pass == wb.calc.pass) {
                continue;
            }
            calc_root_collect(wb, fid, &mut report);
            if report.evaluations > budget {
                break;
            }
        }
        if report.evaluations > budget {
            break;
        }
    }
    if iterative && !wb.op_on(Op::HoldIter) {
        let pass = wb.calc.pass;
        let members: Vec<FormulaId> = wb
            .calc
            .circular
            .iter()
            .copied()
            .filter(|&f| wb.formula(f).circular_pass == pass)
            .collect();
        if !members.is_empty() {
            iterate(wb, members, &mut report);
        }
    }
    report.circular = wb.calc.circular_ref;
    finish_report(report)
}

pub(crate) fn finish_report(mut report: RecalcReport) -> RecalcReport {
    let mut seen = FxHashSet::default();
    report.changed.retain(|a| seen.insert(*a));
    report
}

const PARALLEL_MIN_DIRTY: usize = 2048;

const PARALLEL_MIN_LEVEL: usize = 256;

const MAX_PARALLEL_FANIN: usize = 64;

fn parallel_phase(wb: &mut Workbook, report: &mut RecalcReport) {
    if wb.settings.threads == 1 {
        return;
    }
    let pass = wb.calc.pass;
    let mut dirty: Vec<FormulaId> = wb
        .calc
        .dirty_list
        .iter()
        .copied()
        .filter(|&f| {
            wb.formulas.get(f as usize).is_some_and(|c| {
                c.as_ref()
                    .is_some_and(|c| c.dirty && c.circular_pass != pass)
            })
        })
        .collect();
    dirty.sort_unstable();
    dirty.dedup();
    if dirty.len() < PARALLEL_MIN_DIRTY {
        return;
    }

    let mut probe = Vec::new();
    let mut ready = 0;
    for &f in &dirty {
        probe.clear();
        dirty_precedents_limited(wb, f, &mut probe, 0);
        if probe.is_empty() {
            ready += 1;
            if ready >= PARALLEL_MIN_LEVEL {
                break;
            }
        }
    }
    if ready < PARALLEL_MIN_LEVEL {
        return;
    }
    let index: rustc_hash::FxHashMap<FormulaId, usize> =
        dirty.iter().enumerate().map(|(i, f)| (*f, i)).collect();
    let n = dirty.len();
    let mut indeg = vec![0u32; n];
    let mut downstream: Vec<Vec<u32>> = vec![Vec::new(); n];
    let mut pre = Vec::new();

    let mut volatile_users: FxHashSet<FormulaId> = FxHashSet::default();
    let mut users = Vec::new();
    for d in wb.names.iter().filter(|d| d.formula.volatile) {
        users.clear();
        wb.graph
            .dependents_of_name(&d.name.to_uppercase(), &mut users);
        volatile_users.extend(users.iter().copied());
    }
    for (i, &f) in dirty.iter().enumerate() {
        if wb.formula(f).formula.volatile || volatile_users.contains(&f) {
            indeg[i] = u32::MAX;
            continue;
        }
        pre.clear();
        dirty_precedents_limited(wb, f, &mut pre, MAX_PARALLEL_FANIN);
        pre.sort_unstable();
        pre.dedup();
        if pre.len() > MAX_PARALLEL_FANIN {
            indeg[i] = u32::MAX;
            continue;
        }
        for p in &pre {
            if let Some(&j) = index.get(p) {
                downstream[j].push(i as u32);
                indeg[i] += 1;
            }
        }
    }

    let mut level: Vec<u32> = (0..n as u32).filter(|&i| indeg[i as usize] == 0).collect();
    if level.len() < PARALLEL_MIN_LEVEL {
        return;
    }
    while !level.is_empty() {
        level.sort_unstable_by_key(|&i| wb.formula(dirty[i as usize]).seq);
        let results: Vec<(u32, Value, bool)> = {
            let wb_ref: &Workbook = wb;
            let eval_one = |&i: &u32| {
                let f = wb_ref.formula(dirty[i as usize]);
                let formula = f.formula.clone();
                let mut ctx = EvalCtx::new(wb_ref, f.addr);
                ctx.own = f.array.map(|_| dirty[i as usize]);
                let v = eval_formula(&mut ctx, &formula.root);

                (i, v, ctx.interrupt.is_none() && !ctx.observed.read_own)
            };
            if level.len() >= PARALLEL_MIN_LEVEL {
                use rayon::prelude::*;
                level.par_iter().map(eval_one).collect()
            } else {
                level.iter().map(eval_one).collect()
            }
        };
        let mut next = Vec::new();

        let mut stale: FxHashSet<FormulaId> = FxHashSet::default();
        for (i, v, ok) in results {
            report.evaluations += 1;
            let fid = dirty[i as usize];
            if !ok || stale.contains(&fid) {
                continue;
            }
            let (sheet, old_spill) = {
                let f = wb.formula(fid);
                (f.addr.sheet, f.spill)
            };
            store_result(wb, fid, v, report);
            let new_spill = wb.formula(fid).spill;
            if old_spill != new_spill {
                let mut deps = Vec::new();
                for r in [old_spill, new_spill].into_iter().flatten() {
                    wb.graph.dependents_of_rect(sheet, &r, &mut deps);
                }
                stale.extend(deps);
            }
            for &j in &downstream[i as usize] {
                if indeg[j as usize] == u32::MAX {
                    continue;
                }
                indeg[j as usize] -= 1;
                if indeg[j as usize] == 0 {
                    next.push(j);
                }
            }
        }
        level = next;
    }
}

fn calc_root(wb: &mut Workbook, root: FormulaId, report: &mut RecalcReport) {
    let (value, read_dirty) = {
        let f = wb.formula(root);
        let formula = f.formula.clone();
        let mut ctx = EvalCtx::with_mode(wb, f.addr, ReadMode::Stale);
        let v = eval_formula(&mut ctx, &formula.root);
        (v, ctx.observed.read_dirty)
    };
    report.evaluations += 1;
    store_result(wb, root, value, report);
    if read_dirty {
        wb.set_dirty(root, true);
        wb.calc.dirty_list.push(root);
    }
}

fn dirty_precedents(wb: &Workbook, fid: FormulaId, out: &mut Vec<FormulaId>) {
    dirty_precedents_limited(wb, fid, out, usize::MAX);
}

fn dirty_precedents_limited(wb: &Workbook, fid: FormulaId, out: &mut Vec<FormulaId>, limit: usize) {
    let pass = wb.calc.pass;
    let consider = |g: FormulaId, out: &mut Vec<FormulaId>| {
        let f = wb.formula(g);
        if f.dirty && !f.on_stack && f.circular_pass != pass && g != fid {
            out.push(g);
        }
    };
    for p in &wb.formula(fid).precedents {
        match p {
            Prec::Cell(s, r, c) => {
                let Some(sh) = wb.sheet(*s) else { continue };
                match sh.cells.get(*r, *c) {
                    Some(Entry::Formula(g)) => consider(*g, out),
                    Some(Entry::Const(_)) => {}
                    None => {
                        if let Some(g) = sh.spills.owner_of(*r, *c) {
                            consider(g, out);
                        }
                    }
                }
            }
            Prec::Range(s, rect) => {
                let Some(sh) = wb.sheet(*s) else { continue };
                sh.dirty.in_rect(rect, |r, c| {
                    if out.len() > limit {
                        return;
                    }
                    if let Some(Entry::Formula(g)) = sh.cells.get(r, c) {
                        consider(*g, out);
                    }
                });
                for (g, _) in sh.spills.intersecting(rect) {
                    consider(g, out);
                }
            }
            Prec::Name(_) => {}
        }
    }
}

fn calc_root_collect(wb: &mut Workbook, root: FormulaId, report: &mut RecalcReport) {
    let mut stack: Vec<(FormulaId, bool)> = vec![(root, false)];
    wb.formula_mut(root).on_stack = true;
    let mut pre = Vec::new();
    while let Some(&(top, prechecked)) = stack.last() {
        if !wb.formula(top).dirty {
            let f = wb.formula_mut(top);
            f.on_stack = false;
            f.active = false;
            stack.pop();
            continue;
        }
        if !prechecked {
            stack.last_mut().unwrap().1 = true;
            pre.clear();
            dirty_precedents(wb, top, &mut pre);
            if !pre.is_empty() {
                for &g in pre.iter().rev() {
                    if !wb.formula(g).on_stack {
                        wb.formula_mut(g).on_stack = true;
                        stack.push((g, false));
                    }
                }
                continue;
            }
        }
        let (value, interrupt, read_own) = {
            let f = wb.formula(top);
            let formula = f.formula.clone();
            let mut ctx = EvalCtx::new(wb, f.addr);
            ctx.own = f.array.map(|_| top);
            let v = eval_formula(&mut ctx, &formula.root);
            (v, ctx.interrupt, ctx.observed.read_own)
        };
        report.evaluations += 1;

        let (value, interrupt) = match (interrupt, read_own) {
            (None, true) => match crate::cse::settle_array(wb, top, value, report) {
                Some(v) => (v, None),
                None => (Value::Blank, Some(Interrupt::Cycle(top))),
            },
            (i, _) => (value, i),
        };
        match interrupt {
            None => {
                store_result(wb, top, value, report);
                let f = wb.formula_mut(top);
                f.on_stack = false;
                f.active = false;
                stack.pop();
            }
            Some(Interrupt::Pending(g)) => {
                if wb.formula(g).active || stack.len() > 1_000_000 {
                    abandon_circular(wb, &mut stack);
                    continue;
                }
                wb.formula_mut(top).active = true;
                if wb.formula(g).on_stack {
                    let i = stack.iter().rposition(|(s, _)| *s == g).unwrap();
                    let e = stack.remove(i);
                    stack.push(e);
                } else {
                    wb.formula_mut(g).on_stack = true;
                    stack.push((g, false));
                }
            }
            Some(Interrupt::Cycle(c)) => {
                if cancel_speculation(wb, &mut stack, c) {
                    continue;
                }
                abandon_circular(wb, &mut stack);
            }
        }
    }
}

fn cancel_speculation(
    wb: &mut Workbook,
    stack: &mut Vec<(FormulaId, bool)>,
    read: FormulaId,
) -> bool {
    if !wb.formula(read).active {
        return false;
    }
    let Some(j) = stack.iter().rposition(|(s, _)| *s == read) else {
        return false;
    };
    let top = stack.len() - 1;
    let Some(k) = (j + 1..top).rev().find(|&k| !wb.formula(stack[k].0).active) else {
        return false;
    };
    for (g, _) in stack.drain(k + 1..) {
        let f = wb.formula_mut(g);
        f.on_stack = false;
        f.active = false;
    }
    true
}

fn abandon_circular(wb: &mut Workbook, stack: &mut Vec<(FormulaId, bool)>) {
    let pass = wb.calc.pass;
    let (f, _) = stack.pop().unwrap();
    wb.set_dirty(f, true);
    let cell = wb.formula_mut(f);
    cell.on_stack = false;
    cell.active = false;
    cell.circular_pass = pass;
    wb.calc.circular.insert(f);

    if wb.calc.circular_ref.is_none() {
        wb.calc.circular_ref = Some(wb.formula(f).addr);
    }
}

fn iterate(wb: &mut Workbook, mut members: Vec<FormulaId>, report: &mut RecalcReport) {
    let mut by_name: Vec<(SheetId, &str)> =
        wb.sheets.iter().map(|s| (s.id, s.name.as_str())).collect();
    by_name.sort_by(|a, b| crate::text::compare_ci(a.1, b.1).then(a.0 .0.cmp(&b.0 .0)));
    let rank: rustc_hash::FxHashMap<SheetId, usize> =
        by_name.iter().enumerate().map(|(i, s)| (s.0, i)).collect();
    members.sort_unstable_by_key(|&f| {
        let a = wb.formula(f).addr;
        (
            rank.get(&a.sheet).copied().unwrap_or(usize::MAX),
            a.row,
            a.col,
        )
    });
    let max_iter = wb.settings.max_iterations.max(1);
    let max_change = wb.settings.max_change;
    for _ in 0..max_iter {
        let mut delta = 0.0f64;
        for &fid in &members {
            let old = wb.formula(fid).value.clone();
            let value = {
                let f = wb.formula(fid);
                let formula = f.formula.clone();
                let mut ctx = EvalCtx::with_mode(wb, f.addr, ReadMode::Iterative);
                eval_formula(&mut ctx, &formula.root)
            };
            report.evaluations += 1;
            delta = delta.max(value_change(&old, &value));
            store_result(wb, fid, value, report);
        }
        if delta < max_change {
            break;
        }
    }
    for &fid in &members {
        wb.calc.circular.insert(fid);
    }
}

fn store_result(wb: &mut Workbook, fid: FormulaId, value: Value, report: &mut RecalcReport) {
    let (addr, old_spill) = {
        let f = wb.formula(fid);
        (f.addr, f.spill)
    };
    let value = if wb.precision_as_displayed() {
        wb.precision_result(addr, value)
    } else {
        value
    };
    let mut new_spill = None;
    let mut blocked = None;

    let block = wb.array_block(fid);
    let stored = match value {
        v if block.is_some() => {
            let b = block.unwrap();
            new_spill = (!b.is_cell()).then_some(b);
            crate::cse::fit(v, b.rows(), b.cols())
        }
        Value::Array(a) if a.rows > 1 || a.cols > 1 => {
            let r1 = addr.row as u64 + a.rows as u64 - 1;
            let c1 = addr.col as u64 + a.cols as u64 - 1;
            if r1 >= MAX_ROWS as u64 || c1 >= MAX_COLS as u64 {
                blocked = Some(Rect::new(addr.row, addr.col, MAX_ROWS - 1, MAX_COLS - 1));
                Value::Error(XlError::SPILL)
            } else {
                let want = Rect::new(addr.row, addr.col, r1 as u32, c1 as u32);
                let sh = wb.sheet(addr.sheet).unwrap();

                let in_table = wb
                    .tables
                    .iter()
                    .any(|t| t.contains(addr.sheet, addr.row, addr.col));
                let occupied = in_table
                    || wb.merge_intersects(addr.sheet, &want)
                    || sh.cells.any_in(want, Some((addr.row, addr.col)))
                    || sh.spills.intersecting(&want).iter().any(|(g, _)| *g != fid);
                if occupied {
                    blocked = Some(want);
                    Value::Error(XlError::SPILL)
                } else {
                    new_spill = Some(want);
                    Value::Array(a)
                }
            }
        }
        Value::Array(a) => a.data[0].clone(),
        v => v,
    };
    let changed = {
        let f = wb.formula(fid);
        !f.value.same_as(&stored) || f.spill != new_spill
    };
    {
        let seq = wb.calc.next_done_seq;
        wb.calc.next_done_seq += 1;
        debug_assert!(
            !crate::eval::contains_circ(&stored),
            "an internal circular placeholder reached a cell"
        );
        let f = wb.formula_mut(fid);
        f.value = stored;
        f.spill = new_spill;
        f.seq = seq;
        wb.set_dirty(fid, false);
    }
    {
        let sh = wb.sheet_mut(addr.sheet).unwrap();
        match (new_spill, blocked) {
            (Some(r), _) => sh.spills.set_active(fid, r),
            (None, Some(r)) => sh.spills.set_blocked(fid, r),
            (None, None) => {
                sh.spills.remove(fid);
            }
        }
    }
    if changed {
        report.changed.push(addr);

        let mut area: Option<Rect> = None;
        for r in [old_spill, new_spill].into_iter().flatten() {
            area = Some(area.map_or(r, |a| a.union_bounds(&r)));
        }
        if let Some(rect) = area {
            let mut deps = Vec::new();
            wb.graph.dependents_of_rect(addr.sheet, &rect, &mut deps);
            if old_spill != new_spill {
                if let Some(sh) = wb.sheet(addr.sheet) {
                    for g in sh.spills.anchors_wanting_rect(&rect) {
                        if g != fid {
                            deps.push(g);
                        }
                    }
                }
            }
            for d in deps {
                if d != fid {
                    mark_dirty(wb, d);
                }
            }
        }
    }
}

pub(crate) fn block_spill_at(wb: &mut Workbook, addr: CellAddr) {
    let Some(sh) = wb.sheet(addr.sheet) else {
        return;
    };
    let Some(fid) = sh.spills.owner_of(addr.row, addr.col) else {
        return;
    };
    let Some(rect) = sh.spills.active_rect(fid) else {
        return;
    };
    if wb.formula(fid).addr == addr {
        return;
    }
    {
        let f = wb.formula_mut(fid);
        f.value = Value::Error(XlError::SPILL);
        f.spill = None;
    }
    wb.sheet_mut(addr.sheet)
        .unwrap()
        .spills
        .set_blocked(fid, rect);
    wb.set_dirty(fid, true);
    wb.calc.dirty_list.push(fid);
    let mut deps = Vec::new();
    wb.graph.dependents_of_rect(addr.sheet, &rect, &mut deps);
    for d in deps {
        if d != fid {
            mark_dirty(wb, d);
        }
    }
}

fn value_change(old: &Value, new: &Value) -> f64 {
    match (old, new) {
        (Value::Number(a), Value::Number(b)) => (a - b).abs(),
        (Value::Array(a), Value::Array(b)) if a.rows == b.rows && a.cols == b.cols => a
            .data
            .iter()
            .zip(b.data.iter())
            .map(|(x, y)| value_change(x, y))
            .fold(0.0, f64::max),
        (Value::Array(a), n @ Value::Number(_)) | (n @ Value::Number(_), Value::Array(a)) => a
            .data
            .iter()
            .map(|x| value_change(x, n))
            .fold(0.0, f64::max),
        _ => 0.0,
    }
}
