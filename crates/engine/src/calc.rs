use rustc_hash::FxHashSet;

use crate::address::{CellAddr, Rect, SheetId, MAX_COLS, MAX_ROWS};
use crate::eval::{eval_formula, EvalCtx, Interrupt, ReadMode};
use crate::graph::Prec;
use crate::value::{Array, Value, XlError};
use crate::workbook::{current_serial, Entry, FormulaId, Op, Workbook};

#[derive(Clone, Debug, Default)]
pub struct RecalcReport {
    pub evaluations: usize,

    pub changed: Vec<CellAddr>,

    pub circular: Option<CellAddr>,

    pub sheets: Vec<SheetId>,
}

impl RecalcReport {
    pub(crate) fn evaluated(&mut self, sheet: SheetId) {
        self.evaluations += 1;
        if self.sheets.last() != Some(&sheet) && !self.sheets.contains(&sheet) {
            self.sheets.push(sheet);
        }
    }

    pub(crate) fn add_sheets(&mut self, other: &RecalcReport) {
        for &s in &other.sheets {
            if !self.sheets.contains(&s) {
                self.sheets.push(s);
            }
        }
    }
}

pub fn mark_dirty(wb: &mut Workbook, fid: FormulaId) {
    let mut work = vec![fid];
    let mut deps = Vec::new();
    while let Some(f) = work.pop() {
        let cell = wb.formula(f);
        if cell.dirty {
            let (retry, kept) = (cell.member_retry, cell.spill_kept && cell.array.is_some());
            if retry {
                wb.formula_mut(f).member_retry = false;
            }

            if kept {
                wb.formula_mut(f).spill_kept = false;
                let (addr, spill) = (wb.formula(f).addr, wb.formula(f).spill);
                deps.clear();
                match spill {
                    Some(rect) => wb.graph.dependents_of_rect(addr.sheet, &rect, &mut deps),
                    None => wb
                        .graph
                        .dependents_of_cell(addr.sheet, addr.row, addr.col, &mut deps),
                }
                work.extend(deps.iter().copied());
            }
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
        if wb.settings.mode != crate::workbook::CalcMode::Automatic {
            wb.calc.entered.push(f);
        }
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
    let circular_owner = wb.sheet(addr.sheet).and_then(|sh| {
        sh.cells.get(addr.row, addr.col)?;
        let g = sh.spills.owner_of(addr.row, addr.col)?;
        let f = wb.formula(g);
        (wb.calc.circular.contains(&g) && f.array.is_none() && f.addr != addr).then_some(g)
    });
    if circular_owner.is_some() {
        block_spill_at(wb, addr);
    }
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
    name_changed_scoped(wb, key, None);
}

pub fn name_changed_keeping_this_row(
    wb: &mut Workbook,
    key: &str,
    table: (crate::address::SheetId, crate::address::Rect),
) {
    name_changed_scoped(wb, key, Some(table));
}

pub(crate) fn name_changed_sparing(
    wb: &mut Workbook,
    key: &str,
    spared: &rustc_hash::FxHashSet<FormulaId>,
) {
    let mut deps = Vec::new();
    wb.graph.dependents_of_name(key, &mut deps);
    for fid in deps {
        wb.refresh_scalar_entry(fid);
        wb.unlink_formula(fid);
        wb.link_formula(fid);
        if spared.contains(&fid) {
            continue;
        }
        if wb.formula(fid).dirty {
            wb.calc.dirty_list.push(fid);
        } else {
            mark_dirty(wb, fid);
        }
    }
}

fn only_this_row(f: &crate::formula::Formula) -> bool {
    let (mut any, mut other) = (false, false);
    crate::formula::walk(&f.root, &mut |n| {
        if let crate::formula::Node::StructRef { spec, .. } = n {
            any = true;
            other |= !spec.items.this_row;
        }
    });
    any && !other
}

fn name_changed_scoped(
    wb: &mut Workbook,
    key: &str,
    keep_this_row: Option<(crate::address::SheetId, crate::address::Rect)>,
) {
    let mut deps = Vec::new();
    wb.graph.dependents_of_name(key, &mut deps);

    for fid in deps {
        wb.refresh_scalar_entry(fid);
        wb.unlink_formula(fid);
        wb.link_formula(fid);
        let a = wb.formula(fid).addr;
        if keep_this_row.is_some_and(|(s, r)| a.sheet == s && r.contains(a.row, a.col))
            && only_this_row(&wb.formula(fid).formula)
        {
            continue;
        }

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

    if wb.settings.mode == crate::workbook::CalcMode::Automatic {
        wb.notes.calculated(&report.sheets);
    }
    finish_report(report)
}

pub(crate) fn close_entered_group(wb: &mut Workbook, edited: &[CellAddr]) {
    let n = wb.calc.entered.len();
    let from = wb.calc.entered_groups.last().copied().unwrap_or(0).min(n);

    let mut group = wb.calc.entered.split_off(from);
    group.sort_by_key(|&g| {
        let live = wb.formulas.get(g as usize).and_then(|f| f.as_ref());
        !live.is_some_and(|f| edited.contains(&f.addr))
    });
    wb.calc.entered.extend(group);
    if wb.calc.entered_groups.last() != Some(&n) {
        wb.calc.entered_groups.push(n);
    }
}

pub(crate) fn pending_formulas(wb: &Workbook) -> Vec<FormulaId> {
    let mut out: Vec<FormulaId> = Vec::new();
    let mut end = wb.calc.entered.len();
    let mut bounds: Vec<usize> = wb.calc.entered_groups.clone();
    bounds.retain(|&b| b < end);
    let mut starts = vec![0];
    starts.extend(bounds);
    starts.reverse();
    for s in starts {
        for &g in &wb.calc.entered[s..end] {
            if wb.formulas.get(g as usize).is_some_and(|f| f.is_some()) && !out.contains(&g) {
                out.push(g);
            }
        }
        end = s;
    }
    out
}

pub(crate) fn recalc_pending(wb: &mut Workbook, fids: &[FormulaId]) -> RecalcReport {
    let mut report = RecalcReport::default();
    wb.calc.pass += 1;
    for &g in fids {
        if wb.formulas.get(g as usize).is_some_and(|f| f.is_some()) {
            wb.set_dirty(g, true);
            calc_root(wb, g, &mut report);
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
    wb.one_pass(|wb| {
        let mut report = recalc_cells(wb);
        if tables && wb.data_tables.iter().any(|t| t.dirty) {
            crate::datatable::table_pass(wb, &mut report);
            report = finish_report(report);
        }
        report
    })
}

pub(crate) fn snapshot_holes(wb: &mut Workbook, sheet: SheetId, fids: &[FormulaId]) {
    for &fid in fids {
        let Some(sh) = wb.sheet(sheet) else {
            return;
        };
        let cells = sh.spills.snapshot_cells(fid);
        if cells.is_empty() {
            continue;
        }
        let f = wb.formula(fid);
        let values = cells
            .into_iter()
            .map(|(r, c)| ((r, c), f.value_at(r, c)))
            .collect();
        if let Some(sh) = wb.sheet_mut(sheet) {
            sh.spills.set_snapshot(fid, values);
        }
    }
}

pub(crate) fn note_sheet_rows(wb: &mut Workbook) {
    for sh in wb.sheets.iter_mut() {
        let last = sh.cells.sticky_bounds().map(|b| b.r1);
        sh.spills.note_rows(last);
    }
}

pub(crate) fn recalc_cells(wb: &mut Workbook) -> RecalcReport {
    note_sheet_rows(wb);
    wb.calc.entered.clear();
    wb.calc.entered_groups.clear();
    let mut report = RecalcReport::default();
    wb.calc.pass += 1;
    wb.calc.now = wb.calc.fixed_now.unwrap_or_else(current_serial);
    wb.calc.circular_ref = None;
    wb.calc.partials.clear();
    wb.calc.waited.clear();
    wb.calc.unseen.clear_late();
    wb.calc.circ_iterating = wb.settings.iteration;
    let volatile: Vec<FormulaId> = wb
        .calc
        .volatile
        .iter()
        .chain(&wb.calc.host_volatile)
        .copied()
        .collect();
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
    for &fid in &circular {
        if wb.formulas.get(fid as usize).is_some_and(|f| f.is_some()) {
            if wb.formula(fid).dirty {
                wb.calc.dirty_list.push(fid);
            } else {
                mark_dirty(wb, fid);
            }
        }
    }

    for fid in circular {
        let Some(f) = wb.formulas.get(fid as usize).and_then(|f| f.as_ref()) else {
            continue;
        };
        if let (Some(_), Some(rect)) = (&f.partial, f.spill) {
            let sheet = f.addr.sheet;
            let mut deps = Vec::new();
            wb.graph.dependents_of_rect(sheet, &rect, &mut deps);
            for d in deps {
                if d != fid {
                    mark_dirty(wb, d);
                }
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

    for fid in std::mem::take(&mut wb.calc.spill_dirty) {
        let keep = wb.formulas.get(fid as usize).is_some_and(|f| {
            f.as_ref()
                .is_some_and(|f| !f.dirty && f.circular_pass != wb.calc.pass)
        });
        if keep {
            wb.set_dirty(fid, true);
            wb.formula_mut(fid).spill_kept = true;
            wb.calc.dirty_list.push(fid);
        }
    }

    let blocks: Vec<FormulaId> = wb.calc.circular.iter().copied().collect();
    for fid in blocks {
        if live(wb, fid) && wb.formula(fid).array.is_some() {
            wb.formula_mut(fid).member_retry = true;
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
    wb.note_pass(&report.sheets);
    finish_report(report)
}

fn note_host_volatile(wb: &mut Workbook, fid: FormulaId, volatile: Option<bool>) {
    if volatile == Some(true) {
        wb.calc.host_volatile.insert(fid);
    } else if !wb.calc.host_volatile.is_empty() {
        wb.calc.host_volatile.remove(&fid);
    }
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
        let results: Vec<(u32, Value, bool, bool)> = {
            let wb_ref: &Workbook = wb;
            let eval_one = |&i: &u32| {
                let f = wb_ref.formula(dirty[i as usize]);
                let formula = f.formula.clone();
                let mut ctx = EvalCtx::new(wb_ref, f.addr);
                ctx.own = f.array.map(|_| dirty[i as usize]);
                ctx.parallel = true;
                let v = eval_formula(&mut ctx, &formula.root);

                let spill = ctx.observed.read_spill && !ctx.observed.spill_masked;
                (
                    i,
                    v,
                    ctx.interrupt.is_none() && !ctx.observed.read_own && !ctx.observed.needs_serial,
                    spill,
                )
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
        for (i, v, ok, spill) in results {
            let fid = dirty[i as usize];
            report.evaluated(wb.formula(fid).addr.sheet);
            if !ok || stale.contains(&fid) {
                continue;
            }
            let (sheet, old_spill) = {
                let f = wb.formula(fid);
                (f.addr.sheet, f.spill)
            };
            wb.calc.read_spill = spill;
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
    let (value, read_dirty, read_spill, host_volatile) = {
        let f = wb.formula(root);
        let formula = f.formula.clone();
        let mut ctx = EvalCtx::with_mode(wb, f.addr, ReadMode::Stale);
        let v = eval_formula(&mut ctx, &formula.root);
        (
            v,
            ctx.observed.read_dirty,
            ctx.observed.read_spill && !ctx.observed.spill_masked,
            ctx.observed.host_volatile,
        )
    };
    report.evaluated(wb.formula(root).addr.sheet);
    note_host_volatile(wb, root, host_volatile);
    wb.calc.read_spill = read_spill;
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

fn first_in_range(wb: &Workbook, fid: FormulaId, g: FormulaId) -> FormulaId {
    if !wb.settings.multi_threaded {
        return g;
    }
    let a = wb.formula(g).addr;
    let pass = wb.calc.pass;
    let mut best = g;
    for p in &wb.formula(fid).precedents {
        let Prec::Range(s, rect) = p else { continue };
        if *s != a.sheet || !rect.contains(a.row, a.col) {
            continue;
        }
        let Some(sh) = wb.sheet(*s) else { continue };
        sh.dirty.in_rect(rect, |r, c| {
            if let Some(Entry::Formula(h)) = sh.cells.get(r, c) {
                let f = wb.formula(*h);
                if *h != fid
                    && f.dirty
                    && !f.on_stack
                    && !f.active
                    && f.circular_pass != pass
                    && f.seq < wb.formula(best).seq
                {
                    best = *h;
                }
            }
        });
    }
    best
}

pub(crate) fn first_dirty_in_areas(
    wb: &Workbook,
    caller: Option<FormulaId>,
    g: FormulaId,
    areas: &[crate::Area],
) -> FormulaId {
    let pass = wb.calc.pass;
    let mut best = g;
    for a in areas {
        let Some(sh) = wb.sheet(a.sheet) else {
            continue;
        };
        sh.dirty.in_rect(&a.rect, |r, c| {
            if let Some(Entry::Formula(h)) = sh.cells.get(r, c) {
                let f = wb.formula(*h);
                if Some(*h) != caller
                    && f.dirty
                    && !f.on_stack
                    && !f.active
                    && f.circular_pass != pass
                    && f.seq < wb.formula(best).seq
                {
                    best = *h;
                }
            }
        });
    }
    best
}

fn wait_for(wb: &mut Workbook, f: FormulaId, g: FormulaId) {
    let pass = wb.calc.pass;
    wb.calc.waited.insert(f, (pass, g));
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
            if !crate::host::calls_host(wb, &wb.formula(top).formula.root) {
                dirty_precedents(wb, top, &mut pre);
            }
            if let Some(&last) = pre.last() {
                wait_for(wb, top, last);

                for &g in pre.iter().rev() {
                    if !wb.formula(g).on_stack {
                        wb.formula_mut(g).on_stack = true;
                        stack.push((g, false));
                    }
                }
                continue;
            }
        }
        let partial_ok = !wb.settings.iteration;
        let (mut value, mut interrupt, mut observed, mut sources) = {
            let f = wb.formula(top);
            let formula = f.formula.clone();
            let mut ctx = EvalCtx::new(wb, f.addr);
            ctx.own = f.array.map(|_| top);
            ctx.allow_partial = partial_ok;
            let v = eval_formula(&mut ctx, &formula.root);
            (v, ctx.interrupt, ctx.observed, ctx.circ_sources)
        };
        report.evaluated(wb.formula(top).addr.sheet);

        if interrupt.is_none() && observed.read_own {
            match crate::cse::settle_array(wb, top, report) {
                Some((v, s, host_volatile)) => {
                    observed.host_volatile = host_volatile;
                    observed.partial = crate::eval::contains_circ(&v);
                    if observed.partial && !partial_ok {
                        interrupt = Some(Interrupt::Cycle(top));
                    }
                    value = v;
                    sources = s;
                }
                None => interrupt = Some(Interrupt::Cycle(top)),
            }
        }

        if let Some(c) = observed.active_cse {
            if wb.formula(c).active && force_partial(wb, &mut stack, top, c, report) {
                continue;
            }
        }
        if observed.partial && interrupt.is_none() {
            note_host_volatile(wb, top, observed.host_volatile);
            if store_partial(wb, top, value, sources, report) {
                let f = wb.formula_mut(top);
                f.on_stack = false;
                f.active = false;
                stack.pop();
            } else if !force_on_stack(wb, &mut stack, report) {
                abandon_circular(wb, &mut stack);
            }
            continue;
        }
        match interrupt {
            None => {
                wb.calc.read_spill = observed.read_spill && !observed.spill_masked;
                note_host_volatile(wb, top, observed.host_volatile);
                store_result(wb, top, value, report);

                let pass = wb.calc.pass;
                if let Some(&(p_pass, p)) = wb.calc.waited.get(&top) {
                    if p_pass == pass {
                        if wb.formula(p).seq > wb.formula(top).seq
                            && !wb.settings.multi_threaded
                            && crate::host::calls_host(wb, &wb.formula(top).formula.root)
                            && !crate::host::calls_host(wb, &wb.formula(p).formula.root)
                        {
                            wb.chain_before(p, top);
                        } else {
                            wb.chain_after(top, p);
                        }
                        if wb.settings.multi_threaded
                            && !crate::host::calls_host(wb, &wb.formula(top).formula.root)
                        {
                            wb.calc.unseen.set_late(top, pass);
                        }
                    }
                }
                let f = wb.formula_mut(top);
                f.on_stack = false;
                f.active = false;
                stack.pop();
            }
            Some(Interrupt::Pending(g)) => {
                let g = if wb.formula(g).on_stack {
                    g
                } else {
                    first_in_range(wb, top, g)
                };
                wait_for(wb, top, g);
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

                let waiting = {
                    let f = wb.formula(c);
                    !f.active
                        && f.circular_pass != wb.calc.pass
                        && f.partial.as_deref().is_some_and(|p| p.pass == wb.calc.pass)
                };
                if waiting {
                    if let Some(g) = partial_to_retry(wb) {
                        wb.formula_mut(top).active = true;
                        wb.formula_mut(g).on_stack = true;
                        stack.push((g, false));
                        continue;
                    }
                }

                let len = stack.len();
                let below = (len >= 2).then(|| stack[len - 2].0);
                if partial_ok && force_on_stack(wb, &mut stack, report) {
                    let reader_alone = below.is_some_and(|b| {
                        stack.len() + 1 == len
                            && stack.last().map(|s| s.0) == Some(top)
                            && !wb.formula(b).on_stack
                    });
                    if reader_alone && reader_deferred(wb, &stack, c, below.unwrap()) {
                        wait_for(wb, top, c);
                        wb.chain_after(top, c);
                        stack.pop();
                        let f = wb.formula_mut(top);
                        f.on_stack = false;
                        f.active = false;
                        wb.calc.dirty_list.push(top);
                    }
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
    cell.partial = None;
    cell.on_stack = false;
    cell.active = false;
    cell.circular_pass = pass;
    wb.calc.circular.insert(f);

    if !wb.settings.multi_threaded {
        if let Some(&(p_pass, p)) = wb.calc.waited.get(&f) {
            if p_pass == pass {
                wb.chain_after(f, p);
            }
        }
    }

    if wb.calc.circular_ref.is_none() {
        wb.calc.circular_ref = Some(wb.formula(f).addr);
    }
}

const PARTIAL_TRIES: u8 = 16;

fn live(wb: &Workbook, s: FormulaId) -> bool {
    wb.formulas.get(s as usize).is_some_and(|c| c.is_some())
}

fn partial_waits(wb: &Workbook, fid: FormulaId) -> bool {
    let pass = wb.calc.pass;
    let Some(p) = wb.formula(fid).partial.as_deref() else {
        return false;
    };
    p.sources.iter().any(|&s| {
        live(wb, s) && {
            let g = wb.formula(s);
            g.on_stack
                || g.partial
                    .as_deref()
                    .is_some_and(|q| q.pass == pass && g.circular_pass != pass && g.dirty)
        }
    })
}

pub(crate) fn partial_retry(wb: &Workbook, fid: FormulaId) -> bool {
    let f = wb.formula(fid);
    let Some(p) = f.partial.as_deref() else {
        return false;
    };
    p.sources
        .iter()
        .any(|&s| live(wb, s) && wb.formula(s).stored > f.stored)
}

fn store_partial(
    wb: &mut Workbook,
    fid: FormulaId,
    value: Value,
    sources: Vec<FormulaId>,
    report: &mut RecalcReport,
) -> bool {
    let Some((rows, cols)) = wb.formula(fid).array else {
        return false;
    };
    let pass = wb.calc.pass;
    let new = crate::cse::fit_circ(value, rows, cols);
    let old = crate::cse::fit(wb.formula(fid).value.clone(), rows, cols);
    let (Value::Array(new), Value::Array(old)) = (new, old) else {
        return false;
    };
    let mask: Vec<bool> = new
        .data
        .iter()
        .map(|v| matches!(v, Value::Error(XlError::CIRC)))
        .collect();
    if mask.iter().all(|&m| m) {
        return false;
    }
    let data = new
        .data
        .iter()
        .zip(old.data.iter())
        .map(|(n, o)| match n {
            Value::Error(XlError::CIRC) => match o {
                Value::Blank | Value::Error(XlError::CIRC) => Value::Number(0.0),
                o => o.clone(),
            },
            n => n.clone(),
        })
        .collect();
    let tries = wb
        .formula(fid)
        .partial
        .as_deref()
        .filter(|p| p.pass == pass)
        .map_or(0, |p| p.tries)
        .saturating_add(1);
    store_result(wb, fid, Array::new(rows, cols, data).into_value(), report);
    wb.set_dirty(fid, true);
    let mut sources = sources;
    sources.retain(|&s| s != fid);
    sources.sort_unstable();
    sources.dedup();
    let f = wb.formula_mut(fid);
    f.partial = Some(Box::new(crate::workbook::PartialCirc {
        pass,
        mask,
        sources,
        tries,
    }));
    if tries >= PARTIAL_TRIES || !partial_waits(wb, fid) {
        wb.formula_mut(fid).circular_pass = pass;
        wb.calc.circular.insert(fid);
        if wb.calc.circular_ref.is_none() {
            wb.calc.circular_ref = Some(wb.formula(fid).addr);
        }
    } else {
        wb.calc.dirty_list.push(fid);
        wb.calc.partials.push(fid);
    }
    true
}

fn partial_to_retry(wb: &Workbook) -> Option<FormulaId> {
    let pass = wb.calc.pass;
    wb.calc.partials.iter().copied().find(|&g| {
        live(wb, g) && {
            let f = wb.formula(g);
            f.dirty
                && !f.on_stack
                && f.circular_pass != pass
                && f.partial.as_deref().is_some_and(|p| p.pass == pass)
                && partial_retry(wb, g)
        }
    })
}

fn force_partial(
    wb: &mut Workbook,
    stack: &mut Vec<(FormulaId, bool)>,
    reader: FormulaId,
    block: FormulaId,
    report: &mut RecalcReport,
) -> bool {
    let tried = wb
        .formula(block)
        .partial
        .as_deref()
        .is_some_and(|p| p.pass == wb.calc.pass && p.tries >= PARTIAL_TRIES);
    let Some(j) = stack.iter().rposition(|(s, _)| *s == block) else {
        return false;
    };
    if tried {
        return false;
    }
    let was_active = wb.formula(reader).active;
    wb.formula_mut(reader).active = true;
    let (value, interrupt, observed, sources) = {
        let f = wb.formula(block);
        let formula = f.formula.clone();
        let mut ctx = EvalCtx::new(wb, f.addr);
        ctx.own = Some(block);
        ctx.allow_partial = true;
        ctx.forced_partial = true;
        let v = eval_formula(&mut ctx, &formula.root);
        (v, ctx.interrupt, ctx.observed, ctx.circ_sources)
    };
    report.evaluated(wb.formula(block).addr.sheet);
    let fail = |wb: &mut Workbook| {
        wb.formula_mut(reader).active = was_active;
        false
    };
    if interrupt.is_some() || observed.read_own {
        return fail(wb);
    }
    note_host_volatile(wb, block, observed.host_volatile);
    if observed.partial {
        let f = wb.formula_mut(block);
        f.on_stack = false;
        f.active = false;
        if !store_partial(wb, block, value, sources, report) {
            let f = wb.formula_mut(block);
            f.on_stack = true;
            f.active = true;
            return fail(wb);
        }
    } else {
        wb.calc.read_spill = observed.read_spill && !observed.spill_masked;
        store_result(wb, block, value, report);
        let f = wb.formula_mut(block);
        f.on_stack = false;
        f.active = false;
    }
    stack.remove(j);
    true
}

fn formula_area(wb: &Workbook, fid: FormulaId) -> (SheetId, Rect) {
    let f = wb.formula(fid);
    let (row, col) = (f.addr.row, f.addr.col);
    let rect = match (f.array, f.spill) {
        (Some((rows, cols)), _) => Rect::new(row, col, row + rows - 1, col + cols - 1),
        (None, Some(spill)) => spill,
        (None, None) => Rect::cell(row, col),
    };
    (f.addr.sheet, rect)
}

fn reads_areas(wb: &Workbook, fid: FormulaId, areas: &[(SheetId, Rect)]) -> bool {
    wb.formula(fid).precedents.iter().any(|p| match p {
        Prec::Cell(sh, row, col) => areas.iter().any(|(t, a)| t == sh && a.contains(*row, *col)),
        Prec::Range(sh, rect) => areas.iter().any(|(t, a)| t == sh && a.intersects(rect)),
        Prec::Name(_) => true,
    })
}

fn reader_deferred(
    wb: &Workbook,
    stack: &[(FormulaId, bool)],
    closed: FormulaId,
    block: FormulaId,
) -> bool {
    let Some((&(reader, _), rest)) = stack.split_last() else {
        return false;
    };
    let mut waited = vec![formula_area(wb, reader)];
    let b = wb.formula(block);
    let partial = b.partial.as_deref().filter(|p| p.pass == wb.calc.pass);
    if let (Some((_, cols)), Some(p)) = (b.array, partial) {
        for (i, _) in p.mask.iter().enumerate().filter(|(_, m)| **m) {
            let (row, col) = (b.addr.row + i as u32 / cols, b.addr.col + i as u32 % cols);
            waited.push((b.addr.sheet, Rect::cell(row, col)));
        }
    }
    if rest.iter().any(|&(g, _)| reads_areas(wb, g, &waited)) {
        return false;
    }
    let Some(k) = rest.iter().position(|&(g, _)| g == closed) else {
        return false;
    };
    let lower: Vec<(SheetId, Rect)> = rest[..k]
        .iter()
        .map(|&(g, _)| formula_area(wb, g))
        .collect();
    !reads_areas(wb, reader, &lower)
}

fn force_on_stack(
    wb: &mut Workbook,
    stack: &mut Vec<(FormulaId, bool)>,
    report: &mut RecalcReport,
) -> bool {
    let Some(&(reader, _)) = stack.last() else {
        return false;
    };
    let blocks: Vec<FormulaId> = stack[..stack.len() - 1]
        .iter()
        .rev()
        .map(|&(g, _)| g)
        .filter(|&g| {
            let f = wb.formula(g);
            f.active && f.array.is_some()
        })
        .collect();
    blocks
        .into_iter()
        .any(|g| force_partial(wb, stack, reader, g, report))
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
            let (value, host_volatile) = {
                let f = wb.formula(fid);
                let formula = f.formula.clone();
                let mut ctx = EvalCtx::with_mode(wb, f.addr, ReadMode::Iterative);
                let v = eval_formula(&mut ctx, &formula.root);
                (v, ctx.observed.host_volatile)
            };
            report.evaluated(wb.formula(fid).addr.sheet);
            note_host_volatile(wb, fid, host_volatile);
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
    let read_spill = std::mem::take(&mut wb.calc.read_spill);
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
        debug_assert!(
            !crate::eval::contains_circ(&stored),
            "an internal circular placeholder reached a cell"
        );

        let f = wb.formula_mut(fid);
        f.value = stored;
        f.spill = new_spill;
        f.spill_kept = false;
        f.member_retry = false;
        f.partial = None;
        let src = blocked.is_some() || read_spill;
        f.spill_src = src;
        if src && wb.settings.mode == crate::workbook::CalcMode::Automatic {
            wb.calc.spill_dirty.push(fid);
        }
        wb.calc.stores += 1;
        let stores = wb.calc.stores;
        wb.formula_mut(fid).stored = stores;
        wb.set_dirty(fid, false);
    }
    {
        let sh = wb.sheet_mut(addr.sheet).unwrap();
        sh.spills.note_rows(sh.cells.sticky_bounds().map(|b| b.r1));
        match (new_spill, blocked) {
            (Some(r), _) => sh.spills.set_active(fid, r),
            (None, Some(r)) => sh.spills.set_blocked(fid, r),
            (None, None) => {
                sh.spills.remove(fid);
            }
        }
    }
    snapshot_holes(wb, addr.sheet, &[fid]);
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

            let reread = old_spill != new_spill;
            for d in deps {
                if d != fid || reread {
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
