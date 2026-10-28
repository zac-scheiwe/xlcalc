mod persist;
mod refs;

use std::io::{self, BufRead, IsTerminal, Write};
use std::time::Instant;

use xlcalc::{
    col_to_letters, format_general, CalcMode, CellAddr, DateUnit, FillType, RecalcReport,
    SeriesSpec, SeriesType, SheetId, Value, Workbook,
};

const HELP: &str = "\
Cells (A1 or absolute R1C1 references, optionally Sheet2!A1 or 'My Sheet'!A1)
  A1 <input>          enter input as if typed: 5, hello, 'text, TRUE, 1/2/2020, =SUM(B1:B3)
                      (formulas may use R1C1 notation: =R[-1]C+1)
  A1                  show a cell: value, shown text, formula, spill, precedents, dependents
  clear A1[:C3]       empty cells
  array C1:C3 =A1:A3*2   legacy array formula over a block (Ctrl+Shift+Enter)
  legacy A1 =A1:A3    enter as Range.Formula (Excel adds @ where it implicitly intersects)
  merge A1:B2 [across] / unmerge A1:B2   merged cells
  show [A1:F10]       print a grid (default: the used area)
  used                the used range of the current sheet
Calculation
  calc                recalculate dirty and volatile formulas (F9)
  fullcalc            recalculate everything (Ctrl+Alt+F9)
  rebuild             rebuild dependencies and recalculate
  mode [auto|manual|semi]   calculation mode (semi = automatic except data tables)
  iter [on|off] [max] [change]   iterative calculation for circular references
  goalseek TARGET GOAL CHANGING   vary the CHANGING constant until TARGET (a formula) = GOAL
  datatable RANGE [row CELL] [col CELL]   what-if data table (RANGE includes the header row/column)
Editing
  insert rows 3 [n]   insert rows (also: delete rows 3 [n], insert cols C [n], delete cols C [n])
  insert cells A1:B2 down|right   insert cells with shift (also: delete cells A1:B2 up|left)
  copy A1:B2 D1       copy and paste (relative references move)
  cut A1:B2 Sheet2!D1 cut and paste (references follow the moved cells)
  fill A1 A2:A10      fill a range with a pattern (fill down / right)
  autofill A1:A2 A1:A10 [type]   AutoFill the source into the destination (which includes it):
                      default copy series formats values days weekdays months years linear growth
  series A1:A10 [rows|cols] [linear|growth|date|autofill] [day|weekday|month|year] [step=N] [stop=N] [trend]
                      Fill Series (Range.DataSeries) from the first cell of each column or row
  sort A1:C10 2 desc 1 [header] [cols] [matchcase]   sort by key columns (1 = first), asc/desc each
  replace A1:C10 \"what\" \"with\" [whole] [matchcase] [cols]   Find & Replace in the range (one cell = the
                      whole sheet); * ? ~ wildcards; replaces in the cell text as the formula bar shows it
  dedup A1:C10 1 2 [header]   remove duplicate rows by key columns (1 = first; none = no-op), like
                      Range.RemoveDuplicates
  format A1:B5 0.00   set a number format
  precision [on|off]  precision as displayed (rounds stored numbers to their format)
  paste A1:B3 D1[:E4] [values|formulas] [add|sub|mul|div] [skipblanks] [transpose]
                      paste special (tiles over a larger target)
  table NAME A1:C10   create a table (first row = headers); totals NAME [on|off]; tables
  unlist NAME         convert a table to a plain range (structured references become A1 ones)
  deltable NAME       delete a table with its contents (references to it become #REF!)
  resize NAME A1:C12  resize a table (the header row stays; a totals row follows the data)
  tablerename NAME NEW  rename a table
  headers NAME [on|off]  show or hide a table's header row
Workbook
  sheet NAME          switch to a sheet, creating it if needed; sheets lists them
  rename OLD = NEW / dropsheet NAME / movesheet NAME [before OTHER] / copysheet NAME [before OTHER]
  name [NAME =formula]   define a workbook-scoped name (no arguments: list names); delname NAME
  save FILE / load FILE   write or read the workbook as JSON
  functions           list built-in functions
  bench N             time a chain of N dependent formulas
  perf N              micro-benchmarks of range reads, fan-out and chains
  help, quit";

struct Console {
    wb: Workbook,
    sheet: SheetId,
}

fn show(v: &Value) -> String {
    match v {
        Value::Blank => String::new(),
        Value::Number(n) => format_general(*n),
        Value::Text(s) => s.to_string(),
        Value::Bool(b) => if *b { "TRUE" } else { "FALSE" }.into(),
        Value::Error(e) => e.to_string(),
        Value::Array(a) => format!("{{{}×{} array}}", a.rows, a.cols),
        Value::Lambda(_) => "<lambda>".into(),
    }
}

impl Console {
    fn sheet_name(&self, id: SheetId) -> String {
        self.wb
            .sheet(id)
            .map(|s| s.name.clone())
            .unwrap_or_default()
    }

    fn label(&self, a: CellAddr) -> String {
        if a.sheet == self.sheet {
            a.a1()
        } else {
            format!("{}!{}", self.sheet_name(a.sheet), a.a1())
        }
    }

    fn report(&self, r: &RecalcReport, elapsed: std::time::Duration) {
        let shown: Vec<String> = r
            .changed
            .iter()
            .take(12)
            .map(|a| {
                let mut s = format!("{}={}", self.label(*a), show(&self.wb.value(*a)));
                if let Some(fid) = self.wb.formula_at(*a) {
                    if let Some(rect) = self.wb.formula(fid).spill {
                        s.push_str(&format!(" (spills {})", rect.a1()));
                    }
                }
                s
            })
            .collect();
        let more = if r.changed.len() > 12 {
            format!(" … +{}", r.changed.len() - 12)
        } else {
            String::new()
        };
        println!(
            "  {} evaluation{} in {:.2?}{}{}",
            r.evaluations,
            if r.evaluations == 1 { "" } else { "s" },
            elapsed,
            if shown.is_empty() {
                String::new()
            } else {
                format!(": {}", shown.join(", "))
            },
            more
        );
        if let Some(c) = r.circular {
            println!("  circular reference: {}", self.label(c));
        }
    }

    fn describe(&self, a: CellAddr) {
        let v = self.wb.value(a);
        let kind = v.type_name();
        match self.wb.formula_text(a) {
            Some(f) => println!("  {}: {} = {} ({kind})", self.label(a), f, show(&v)),
            None => println!("  {}: {} ({kind})", self.label(a), show(&v)),
        }
        let fmt = self.wb.number_format(a);
        if !fmt.eq_ignore_ascii_case("general") {
            println!("  format {fmt}: shown as {}", self.wb.display_text(a));
        }
        if let Some(fid) = self.wb.formula_at(a) {
            let f = self.wb.formula(fid);
            if let (Some(rect), Value::Array(arr)) = (f.spill, &f.value) {
                println!("  spills {} ({}×{})", rect.a1(), arr.rows, arr.cols);
            }
            let p = self.wb.precedents(a);
            if !p.is_empty() {
                println!("  precedents: {}", p.join(", "));
            }
        } else if let Some(anchor) = self.wb.spill_anchor(a) {
            println!("  spilled from {}", self.label(anchor));
        }
        let d: Vec<String> = self
            .wb
            .dependents(a)
            .into_iter()
            .map(|x| self.label(x))
            .collect();
        if !d.is_empty() {
            println!("  dependents: {}", d.join(", "));
        }
    }

    fn grid(&self, arg: Option<&str>) {
        let sh = self.wb.sheet(self.sheet).unwrap();
        let rect = match arg {
            Some(r) => match refs::range(&self.wb, r, self.sheet) {
                Some((_, rect)) => rect,
                None => return println!("  bad range"),
            },
            None => {
                let Some(used) = sh.used_bounds() else {
                    return println!("  (empty sheet)");
                };
                xlcalc::Rect::new(0, 0, used.r1.min(39), used.c1.min(11))
            }
        };
        let rect = xlcalc::Rect::new(
            rect.r0,
            rect.c0,
            rect.r1.min(rect.r0 + 199),
            rect.c1.min(rect.c0 + 25),
        );
        let cell = |r: u32, c: u32| {
            let s = show(&self.wb.value(CellAddr::new(self.sheet, r, c)));
            if s.chars().count() > 12 {
                format!("{}…", s.chars().take(11).collect::<String>())
            } else {
                s
            }
        };
        print!("{:>6} ", "");
        for c in rect.c0..=rect.c1 {
            print!("{:>12} ", col_to_letters(c));
        }
        println!();
        for r in rect.r0..=rect.r1 {
            print!("{:>6} ", r + 1);
            for c in rect.c0..=rect.c1 {
                print!("{:>12} ", cell(r, c));
            }
            println!();
        }
    }

    fn bench(&mut self, n: u32) {
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        wb.set_mode(CalcMode::Manual);
        let t0 = Instant::now();
        wb.set_input(CellAddr::new(s, 0, 0), "1").unwrap();
        for r in 1..n {
            wb.set_formula(CellAddr::new(s, r, 0), &format!("=A{}+1", r))
                .unwrap();
            wb.set_formula(
                CellAddr::new(s, r, 1),
                &format!("=A{}*2+SUM($A$1:A{})", r + 1, r),
            )
            .unwrap();
        }
        let build = t0.elapsed();
        wb.set_mode(CalcMode::Automatic);
        let t1 = Instant::now();
        let rep = wb.set_input(CellAddr::new(s, 0, 0), "2").unwrap();
        let recalc = t1.elapsed();
        println!(
            "  {} formulas entered in {:.2?}; changing A1 recalculated {} in {:.2?}; A{n} = {}",
            2 * (n - 1),
            build,
            rep.evaluations,
            recalc,
            show(&wb.value(CellAddr::new(s, n - 1, 0)))
        );
    }

    fn perf(&mut self, n: u32) {
        let time = |label: &str, cells: f64, f: &mut dyn FnMut() -> RecalcReport| {
            let t = Instant::now();
            let r = f();
            let e = t.elapsed();
            println!(
                "  {label:<44} {:>9.2?}  ({} evals, {:.1} ns/cell)",
                e,
                r.evaluations,
                e.as_nanos() as f64 / cells.max(1.0)
            );
        };
        let mut wb = Workbook::new();
        let s = wb.sheet_at(0).id;
        let a = |r: u32, c: u32| CellAddr::new(s, r, c);
        wb.set_mode(CalcMode::Manual);
        for r in 0..n {
            wb.set_value(a(r, 0), Value::Number(r as f64 * 0.5));
            wb.set_formula(a(r, 1), &format!("=A{}*2", r + 1)).unwrap();
        }
        wb.set_formula(a(0, 3), "=SUM(A:A)").unwrap();
        wb.set_formula(a(1, 3), "=SUM(B:B)").unwrap();
        wb.set_formula(a(2, 3), "=COUNTIF(A:A,\">100\")").ok();
        wb.set_formula(a(3, 3), &format!("=MATCH({},A:A,0)", (n - 1) as f64 * 0.5))
            .unwrap();
        wb.set_formula(
            a(4, 3),
            &format!("=VLOOKUP({},A:B,2,FALSE)", (n - 1) as f64 * 0.5),
        )
        .unwrap();
        wb.set_mode(CalcMode::Automatic);
        let nf = n as f64;
        time(
            &format!("full recalc ({} formulas + 5 aggregates)", n),
            nf * 6.0,
            &mut || wb.full_recalc(),
        );
        wb.set_threads(1);
        time("  …the same on one thread", nf * 6.0, &mut || {
            wb.full_recalc()
        });
        wb.set_threads(0);
        time(
            "change A1 (fan-out to B1 and the aggregates)",
            nf * 5.0,
            &mut || wb.set_input(a(0, 0), "7").unwrap(),
        );
        let mut wb2 = Workbook::new();
        let s2 = wb2.sheet_at(0).id;
        wb2.set_mode(CalcMode::Manual);
        wb2.set_value(CellAddr::new(s2, 0, 0), Value::Number(1.0));
        for r in 1..n {
            wb2.set_formula(CellAddr::new(s2, r, 0), &format!("=A{}+1", r))
                .unwrap();
        }
        wb2.set_mode(CalcMode::Automatic);
        time(
            &format!("chain of {} dependent formulas", n),
            nf,
            &mut || wb2.set_input(CellAddr::new(s2, 0, 0), "2").unwrap(),
        );

        let m = (n / 10).max(1);
        let mut wb4 = Workbook::new();
        let s4 = wb4.sheet_at(0).id;
        wb4.set_mode(CalcMode::Manual);
        for r in 0..200 {
            wb4.set_value(CellAddr::new(s4, r, 0), Value::Number(r as f64));
        }
        for r in 0..m {
            wb4.set_formula(
                CellAddr::new(s4, r, 1),
                &format!("=SUMPRODUCT($A$1:$A$200,$A$1:$A$200)+{r}"),
            )
            .unwrap();
        }
        wb4.set_mode(CalcMode::Automatic);
        time(
            &format!("full recalc, {m} SUMPRODUCTs over 200 cells"),
            m as f64 * 200.0,
            &mut || wb4.full_recalc(),
        );
        wb4.set_threads(1);
        time("  …the same on one thread", m as f64 * 200.0, &mut || {
            wb4.full_recalc()
        });
        let t = Instant::now();
        let mut wb3 = Workbook::new();
        let s3 = wb3.sheet_at(0).id;
        wb3.set_mode(CalcMode::Manual);
        for r in 0..n {
            wb3.set_formula(CellAddr::new(s3, r, 0), &format!("=ROW()*{}", r % 7))
                .unwrap();
        }
        println!(
            "  {:<44} {:>9.2?}  ({:.1} µs/formula)",
            format!("enter {n} formulas"),
            t.elapsed(),
            t.elapsed().as_micros() as f64 / nf
        );
    }

    fn command(&mut self, line: &str) -> bool {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            return true;
        }
        let (head, rest) = refs::split_head(line);
        match head.to_ascii_lowercase().as_str() {
            "quit" | "exit" | "q" => return false,
            "help" | "?" => println!("{HELP}"),
            "calc" => self.timed(|wb| wb.recalc()),
            "fullcalc" => self.timed(|wb| wb.full_recalc()),
            "rebuild" => self.timed(|wb| wb.rebuild()),
            "mode" => {
                let t = Instant::now();
                let r = match rest.to_ascii_lowercase().as_str() {
                    "" => Default::default(),
                    "auto" | "automatic" => self.wb.set_mode(CalcMode::Automatic),
                    "manual" => self.wb.set_mode(CalcMode::Manual),
                    "semi" | "semiauto" | "semiautomatic" => self.wb.set_semiautomatic(),
                    _ => return self.usage("mode [auto|manual|semi]"),
                };
                println!(
                    "  calculation mode: {}",
                    if self.wb.is_semiautomatic() {
                        "semiautomatic (automatic except data tables)".to_string()
                    } else {
                        format!("{:?}", self.wb.settings().mode).to_lowercase()
                    }
                );
                if !rest.is_empty() {
                    self.report(&r, t.elapsed());
                }
            }
            "iter" => {
                let mut parts = rest.split_whitespace();
                let on = match parts.next().map(str::to_ascii_lowercase).as_deref() {
                    Some("on") => true,
                    Some("off") => false,
                    None => self.wb.settings().iteration,
                    _ => return self.usage("iter [on|off] [max iterations] [max change]"),
                };
                let (n, x) = (parts.next(), parts.next());
                let (n, x) = (
                    n.map(|p| p.parse::<u32>().ok()),
                    x.map(|p| p.parse::<f64>().ok().filter(|v| v.is_finite())),
                );
                if n == Some(None) || x == Some(None) || parts.next().is_some() {
                    return self.usage("iter [on|off] [max iterations] [max change]");
                }
                self.wb.set_iteration(on, n.flatten(), x.flatten());
                let s = self.wb.settings();
                println!(
                    "  iteration {} (max {} iterations, max change {})",
                    if s.iteration { "on" } else { "off" },
                    s.max_iterations,
                    s.max_change
                );
            }
            "sheet" => {
                if rest.is_empty() {
                    return self.usage("sheet NAME   (switches to it, creating it if needed)");
                }
                let id = match self.wb.sheet_by_name(rest) {
                    Some(s) => s.id,
                    None => match self.wb.add_sheet(rest) {
                        Ok(id) => id,
                        Err(e) => {
                            println!("  {e}");
                            return true;
                        }
                    },
                };
                self.sheet = id;
                println!("  on sheet {}", self.sheet_name(id));
            }
            "rename" | "dropsheet" | "movesheet" | "copysheet" => {
                let lower = head.to_ascii_lowercase();
                let (name, tail) = if lower == "rename" {
                    match rest.split_once('=') {
                        Some((a, b)) => (a.trim(), b.trim()),
                        None => {
                            println!("  usage: rename OLD = NEW");
                            return true;
                        }
                    }
                } else {
                    match rest.split_once(" before ") {
                        Some((a, b)) => (a.trim(), b.trim()),
                        None => (rest.trim(), ""),
                    }
                };
                let Some(id) = self.wb.sheet_by_name(name).map(|s| s.id) else {
                    println!("  no sheet {name}");
                    return true;
                };
                let before = if tail.is_empty() || lower == "rename" {
                    None
                } else {
                    match self.wb.sheet_by_name(tail) {
                        Some(s) => Some(s.id),
                        None => {
                            println!("  no sheet {tail}");
                            return true;
                        }
                    }
                };
                let r = match lower.as_str() {
                    "rename" if tail.is_empty() => {
                        println!("  usage: rename OLD = NEW");
                        return true;
                    }
                    "rename" => self.wb.rename_sheet(id, tail),
                    "dropsheet" => self.wb.delete_sheet(id),
                    "movesheet" => self.wb.move_sheet(id, before),
                    _ => self.wb.copy_sheet(id, before).map(|_| Default::default()),
                };
                match r {
                    Ok(_) => {
                        if self.wb.sheet(self.sheet).is_none() {
                            self.sheet = self.wb.sheet_at(0).id;
                        }
                    }
                    Err(e) => println!("  {e}"),
                }
            }
            "sheets" => {
                for s in self.wb.sheets() {
                    println!(
                        "  {}{}",
                        s.name,
                        if s.id == self.sheet {
                            "  (current)"
                        } else {
                            ""
                        }
                    );
                }
            }
            "name" => match rest.split_once(char::is_whitespace) {
                Some((n, f)) => match self.wb.define_name(n, f.trim(), None) {
                    Ok(r) => self.report(&r, Default::default()),
                    Err(e) => println!("  {e}"),
                },
                None if rest.is_empty() => {
                    let mut names: Vec<_> = self.wb.names().iter().collect();
                    names.sort_by_key(|d| d.name.to_uppercase());
                    if names.is_empty() {
                        println!("  (no names; define one with: name NAME =formula)");
                    }
                    for d in names {
                        println!("  {} = {}", d.name, d.refers_to);
                    }
                }
                None => println!("  usage: name NAME =formula"),
            },
            "delname" => {
                if rest.is_empty() {
                    return self.usage("delname NAME");
                }
                match self.wb.delete_name(rest, None) {
                    Some(r) => self.report(&r, Default::default()),
                    None => println!("  no name {rest}"),
                }
            }
            "insert" | "delete" => {
                let mut p = rest.split_whitespace();
                let (kind, at, count) = (
                    p.next().unwrap_or(""),
                    p.next().unwrap_or(""),
                    p.next().map_or(Some(1), |n| n.parse::<u32>().ok()),
                );

                let Some(count) = count.or(kind.starts_with("cell").then_some(1)) else {
                    return self.usage("insert|delete rows N [count] | cols C [count]");
                };
                let insert = head.eq_ignore_ascii_case("insert");
                let t = Instant::now();
                let r = if kind.starts_with("cell") {
                    let mut q = rest.split_whitespace().skip(1);
                    let (range, dir) = (q.next().unwrap_or(""), q.next().unwrap_or(""));
                    let vertical = match (insert, dir.to_ascii_lowercase().as_str()) {
                        (true, "down" | "") | (false, "up" | "") => true,
                        (true, "right") | (false, "left") => false,
                        _ => {
                            return self.usage(
                                "insert cells A1:B2 down|right | delete cells A1:B2 up|left",
                            )
                        }
                    };
                    match refs::range(&self.wb, range, self.sheet) {
                        Some((s, rect)) if insert => self.wb.insert_cells(s, rect, vertical),
                        Some((s, rect)) => self.wb.delete_cells(s, rect, vertical),
                        None => {
                            return self.usage(
                                "insert cells A1:B2 down|right | delete cells A1:B2 up|left",
                            )
                        }
                    }
                } else if kind.starts_with("row") {
                    match at.parse::<u32>() {
                        Ok(n) if n >= 1 => {
                            if insert {
                                self.wb.insert_rows(self.sheet, n - 1, count)
                            } else {
                                self.wb.delete_rows(self.sheet, n - 1, count)
                            }
                        }
                        _ => return self.usage("insert|delete rows N [count]"),
                    }
                } else if kind.starts_with("col") {
                    match xlcalc::letters_to_col(at) {
                        Some(c) => {
                            if insert {
                                self.wb.insert_cols(self.sheet, c, count)
                            } else {
                                self.wb.delete_cols(self.sheet, c, count)
                            }
                        }
                        None => return self.usage("insert|delete cols C [count]"),
                    }
                } else {
                    return self.usage("insert|delete rows N [count] | cols C [count]");
                };
                match r {
                    Ok(r) => self.report(&r, t.elapsed()),
                    Err(e) => println!("  {e}"),
                }
            }
            "copy" | "fill" | "cut" => {
                let mut p = rest.split_whitespace();
                let (from, to) = (p.next().unwrap_or(""), p.next().unwrap_or(""));
                let t = Instant::now();
                let r = match (
                    refs::range(&self.wb, from, self.sheet),
                    refs::range(&self.wb, to, self.sheet),
                ) {
                    (_, Some((_, dst)))
                        if head.eq_ignore_ascii_case("fill")
                            && dst.rows() as u64 * dst.cols() as u64 > 1 << 22 =>
                    {
                        println!("  fill range is too large (limit 4194304 cells)");
                        return true;
                    }
                    (Some((s, src)), Some((ds, dst)))
                        if head.eq_ignore_ascii_case("fill") && s == ds =>
                    {
                        self.wb.fill(s, src, dst)
                    }
                    (Some((s, src)), Some((ds, dst))) if head.eq_ignore_ascii_case("cut") => self
                        .wb
                        .move_range(s, src, CellAddr::new(ds, dst.r0, dst.c0)),
                    (Some((s, src)), Some((ds, dst))) => {
                        self.wb
                            .copy_range(s, src, CellAddr::new(ds, dst.r0, dst.c0))
                    }
                    _ => return self.usage("copy A1:B2 D1 | fill A1 A2:A10"),
                };
                match r {
                    Ok(r) => self.report(&r, t.elapsed()),
                    Err(e) => println!("  {e}"),
                }
            }
            "autofill" => {
                let mut p = rest.split_whitespace();
                let (from, to) = (p.next().unwrap_or(""), p.next().unwrap_or(""));
                let ty = p.next().map_or(Some(FillType::Default), FillType::parse);
                let t = Instant::now();
                let r = match (
                    refs::range(&self.wb, from, self.sheet),
                    refs::range(&self.wb, to, self.sheet),
                    ty,
                ) {
                    (Some((s, src)), Some((ds, dst)), Some(ty)) if s == ds => {
                        self.wb.autofill(s, src, dst, ty)
                    }
                    _ => return self.usage("autofill A1:A2 A1:A10 [type]"),
                };
                match r {
                    Ok(r) => self.report(&r, t.elapsed()),
                    Err(e) => println!("  {e}"),
                }
            }
            "series" => {
                let mut p = rest.split_whitespace();
                let Some((s, rect)) = refs::range(&self.wb, p.next().unwrap_or(""), self.sheet)
                else {
                    return self.usage("series A1:A10 [rows|cols] [linear|growth|date|autofill] [day|weekday|month|year] [step=N] [stop=N] [trend]");
                };
                let mut spec = SeriesSpec::default();
                for w in p {
                    let w = w.to_ascii_lowercase();
                    if let Some(v) = w.strip_prefix("step=") {
                        spec.step = v.parse().ok();
                    } else if let Some(v) = w.strip_prefix("stop=") {
                        spec.stop = v.parse().ok();
                    } else if w == "trend" {
                        spec.trend = true;
                    } else if w == "rows" || w == "cols" {
                        spec.rows = Some(w == "rows");
                    } else if let Some(t) = SeriesType::parse(&w) {
                        spec.ty = Some(t);
                    } else if let Some(u) = DateUnit::parse(&w) {
                        spec.unit = Some(u);
                    }
                }
                let t = Instant::now();
                match self.wb.data_series(s, rect, &spec) {
                    Ok(r) => self.report(&r, t.elapsed()),
                    Err(e) => println!("  {e}"),
                }
            }
            "sort" => {
                let mut p = rest.split_whitespace();
                let range = p.next().unwrap_or("");
                let mut spec = xlcalc::SortSpec::default();
                for w in p {
                    match w.to_ascii_lowercase().as_str() {
                        "header" => spec.header = true,
                        "cols" => spec.by_columns = true,
                        "matchcase" => spec.match_case = true,
                        "asc" | "desc" => {
                            if let Some(k) = spec.keys.last_mut() {
                                k.descending = w.eq_ignore_ascii_case("desc");
                            }
                        }
                        n => match n.parse::<u32>() {
                            Ok(i) if i >= 1 => spec.keys.push(xlcalc::SortKey {
                                index: i - 1,
                                descending: false,
                            }),
                            _ => {
                                return self
                                    .usage("sort A1:C10 2 desc 1 [header] [cols] [matchcase]")
                            }
                        },
                    }
                }
                let t = Instant::now();
                match refs::range(&self.wb, range, self.sheet) {
                    Some((sh, rect)) => match self.wb.sort_range(sh, rect, &spec) {
                        Ok(r) => self.report(&r, t.elapsed()),
                        Err(e) => println!("  {e}"),
                    },
                    None => return self.usage("sort A1:C10 2 desc 1 [header] [cols] [matchcase]"),
                }
            }
            "replace" => {
                const USAGE: &str = "replace A1:C10 \"what\" \"with\" [whole] [matchcase] [cols]";
                let words = quoted_words(rest);
                if words.len() < 3 {
                    return self.usage(USAGE);
                }
                let mut spec = xlcalc::ReplaceSpec {
                    what: words[1].clone(),
                    with: words[2].clone(),
                    ..Default::default()
                };
                for w in &words[3..] {
                    match w.to_ascii_lowercase().as_str() {
                        "whole" => spec.whole = true,
                        "matchcase" => spec.match_case = true,
                        "cols" => spec.by_columns = true,
                        _ => return self.usage(USAGE),
                    }
                }
                let t = Instant::now();
                match refs::range(&self.wb, &words[0], self.sheet) {
                    Some((sh, rect)) => match self.wb.replace_range(sh, rect, &spec) {
                        Ok(r) => self.report(&r, t.elapsed()),
                        Err(e) => println!("  {e}"),
                    },
                    None => return self.usage(USAGE),
                }
            }
            "dedup" => {
                const USAGE: &str = "dedup A1:C10 1 2 [header]";
                let mut words = rest.split_whitespace();
                let Some(range) = words.next() else {
                    return self.usage(USAGE);
                };
                let mut spec = xlcalc::DedupSpec::default();
                for w in words {
                    if w.eq_ignore_ascii_case("header") {
                        spec.header = true;
                    } else if let Ok(n) = w.parse::<u32>() {
                        spec.columns.push(n);
                    } else {
                        return self.usage(USAGE);
                    }
                }
                let t = Instant::now();
                match refs::range(&self.wb, range, self.sheet) {
                    Some((sh, rect)) => match self.wb.remove_duplicates(sh, rect, &spec) {
                        Ok(r) => self.report(&r, t.elapsed()),
                        Err(e) => println!("  {e}"),
                    },
                    None => return self.usage(USAGE),
                }
            }
            "paste" => {
                use xlcalc::{PasteOp, PasteSpecial, PasteWhat};
                let mut p = rest.split_whitespace();
                let (from, to) = (p.next().unwrap_or(""), p.next().unwrap_or(""));
                let mut spec = PasteSpecial::default();
                for w in p {
                    match w.to_ascii_lowercase().as_str() {
                        "values" => spec.what = PasteWhat::Values,
                        "formulas" => spec.what = PasteWhat::Formulas,
                        "add" => spec.op = PasteOp::Add,
                        "sub" => spec.op = PasteOp::Sub,
                        "mul" => spec.op = PasteOp::Mul,
                        "div" => spec.op = PasteOp::Div,
                        "skipblanks" => spec.skip_blanks = true,
                        "transpose" => spec.transpose = true,
                        _ => return self.usage("paste A1:B3 D1[:E4] [values|formulas] [add|sub|mul|div] [skipblanks] [transpose]"),
                    }
                }
                let t = Instant::now();
                match (
                    refs::range(&self.wb, from, self.sheet),
                    refs::range(&self.wb, to, self.sheet),
                ) {
                    (Some((ss, src)), Some((ds, dst))) => {
                        match self.wb.paste_special(ss, src, ds, dst, &spec) {
                            Ok(r) => self.report(&r, t.elapsed()),
                            Err(e) => println!("  {e}"),
                        }
                    }
                    _ => return self.usage("paste A1:B3 D1[:E4] [values|formulas] [add|sub|mul|div] [skipblanks] [transpose]"),
                }
            }
            "format" => {
                let (range, fmt) = rest.split_once(char::is_whitespace).unwrap_or((rest, ""));
                let t = Instant::now();
                match refs::range(&self.wb, range, self.sheet) {
                    Some((sh, rect)) if !fmt.trim().is_empty() => {
                        let r = self.wb.set_number_format(sh, rect, fmt.trim());
                        self.report(&r, t.elapsed());
                    }
                    _ => return self.usage("format A1:B5 0.00"),
                }
            }
            "precision" => {
                let t = Instant::now();
                match rest.trim().to_ascii_lowercase().as_str() {
                    "on" | "off" => {
                        let r = self
                            .wb
                            .set_precision_as_displayed(rest.trim().eq_ignore_ascii_case("on"));
                        self.report(&r, t.elapsed());
                    }
                    "" => println!(
                        "  precision as displayed: {}",
                        if self.wb.precision_as_displayed() {
                            "on"
                        } else {
                            "off"
                        }
                    ),
                    _ => return self.usage("precision [on|off]"),
                }
            }
            "table" => {
                let mut p = rest.split_whitespace();
                let (name, range) = (p.next().unwrap_or(""), p.next().unwrap_or(""));
                match refs::range(&self.wb, range, self.sheet) {
                    Some((s, rect)) => match self.wb.add_table(name, s, rect) {
                        Ok(r) => {
                            let t = self.wb.table_by_name(name).unwrap();
                            println!(
                                "  table {} on {} with columns: {}",
                                t.name,
                                t.rect.a1(),
                                t.columns
                                    .iter()
                                    .take(12)
                                    .cloned()
                                    .collect::<Vec<_>>()
                                    .join(", ")
                                    + if t.columns.len() > 12 { ", ..." } else { "" }
                            );
                            self.report(&r, Default::default());
                        }
                        Err(e) => println!("  {e}"),
                    },
                    None => return self.usage("table NAME A1:C10"),
                }
            }
            "totals" => {
                let mut p = rest.split_whitespace();
                let name = p.next().unwrap_or("");
                let on = match p.next().map(str::to_ascii_lowercase).as_deref() {
                    None | Some("on") => true,
                    Some("off") => false,
                    _ => return self.usage("totals NAME [on|off]"),
                };
                match self.wb.set_table_totals(name, on) {
                    Ok(r) => self.report(&r, Default::default()),
                    Err(e) => println!("  {e}"),
                }
            }
            "unlist" | "deltable" => {
                let name = rest.trim();
                let unlist = head.eq_ignore_ascii_case("unlist");
                if name.is_empty() {
                    return self.usage(if unlist {
                        "unlist NAME"
                    } else {
                        "deltable NAME"
                    });
                }
                let r = if unlist {
                    self.wb.unlist_table(name)
                } else {
                    self.wb.delete_table(name)
                };
                match r {
                    Ok(r) => self.report(&r, Default::default()),
                    Err(e) => println!("  {e}"),
                }
            }
            "resize" => {
                let mut p = rest.split_whitespace();
                let (name, range) = (p.next().unwrap_or(""), p.next().unwrap_or(""));
                match refs::range(&self.wb, range, self.sheet) {
                    Some((_, rect)) => match self.wb.resize_table(name, rect) {
                        Ok(r) => self.report(&r, Default::default()),
                        Err(e) => println!("  {e}"),
                    },
                    None => return self.usage("resize NAME A1:C12"),
                }
            }
            "tablerename" => {
                let mut p = rest.split_whitespace();
                let (name, new) = (p.next().unwrap_or(""), p.next().unwrap_or(""));
                if name.is_empty() || new.is_empty() {
                    return self.usage("tablerename NAME NEW");
                }
                match self.wb.rename_table(name, new) {
                    Ok(r) => self.report(&r, Default::default()),
                    Err(e) => println!("  {e}"),
                }
            }
            "headers" => {
                let mut p = rest.split_whitespace();
                let name = p.next().unwrap_or("");
                let on = match p.next().map(str::to_ascii_lowercase).as_deref() {
                    None | Some("on") => true,
                    Some("off") => false,
                    _ => return self.usage("headers NAME [on|off]"),
                };
                match self.wb.set_table_headers(name, on) {
                    Ok(r) => self.report(&r, Default::default()),
                    Err(e) => println!("  {e}"),
                }
            }
            "tables" => {
                for t in self.wb.tables() {
                    println!(
                        "  {} {}!{} [{}]{}",
                        t.name,
                        self.sheet_name(t.sheet),
                        t.rect.a1(),
                        t.columns.join(", "),
                        match (t.totals, t.headers) {
                            (true, true) => " +totals",
                            (true, false) => " +totals -headers",
                            (false, false) => " -headers",
                            _ => "",
                        }
                    );
                }
            }
            "save" => match std::fs::write(
                rest,
                serde_json::to_string_pretty(&persist::save(&self.wb)).unwrap(),
            ) {
                Ok(_) => println!("  saved {rest}"),
                Err(e) => println!("  {e}"),
            },
            "load" => match std::fs::read_to_string(rest)
                .map_err(|e| e.to_string())
                .and_then(|t| {
                    serde_json::from_str::<serde_json::Value>(&t)
                        .map_err(|e| e.to_string())
                        .and_then(|j| persist::load(&j))
                }) {
                Ok(wb) => {
                    self.wb = wb;
                    self.sheet = self.wb.sheet_at(0).id;
                    println!(
                        "  loaded {rest}: {} sheets, {} formulas",
                        self.wb.sheet_count(),
                        self.wb.formula_count()
                    );
                }
                Err(e) => println!("  {e}"),
            },
            "functions" => {
                let names = xlcalc::function_names();
                println!("  {} functions:", names.len());
                for chunk in names.chunks(8) {
                    println!("    {}", chunk.join("  "));
                }
            }
            "bench" | "perf" if !rest.is_empty() && rest.parse::<u32>().is_err() => {
                return self.usage("bench N | perf N   (N a whole number)")
            }
            "bench" => self.bench(rest.parse().unwrap_or(10_000).max(2)),
            "perf" => self.perf(rest.parse().unwrap_or(100_000).max(10)),
            "show" => self.grid((!rest.is_empty()).then_some(rest)),
            "used" => match self.wb.used_range(self.sheet) {
                Some(r) => println!("  {}", r.a1()),
                None => println!("  (empty sheet)"),
            },
            "goalseek" => {
                let usage = "goalseek TARGET GOAL CHANGING   e.g. goalseek B4 15000 B2";
                let p: Vec<&str> = rest.split_whitespace().collect();
                let [t, g, c] = p[..] else {
                    return self.usage(usage);
                };
                let (Some(target), Some(changing), Ok(goal)) = (
                    refs::cell(&self.wb, t, self.sheet),
                    refs::cell(&self.wb, c, self.sheet),
                    g.parse::<f64>(),
                ) else {
                    return self.usage(usage);
                };
                match self.wb.goal_seek(target, goal, changing) {
                    Ok(found) => {
                        println!(
                            "  goal seek {}: {} = {}, {} = {}",
                            if found {
                                "found a solution"
                            } else {
                                "did not converge"
                            },
                            self.label(changing),
                            self.wb.display_text(changing),
                            self.label(target),
                            self.wb.display_text(target)
                        );
                    }
                    Err(e) => println!("  {e}"),
                }
            }
            "datatable" => {
                let usage = "datatable RANGE [row CELL] [col CELL]   e.g. datatable C1:D5 col B2";
                let mut p = rest.split_whitespace();
                let Some((s, rect)) = p.next().and_then(|r| refs::range(&self.wb, r, self.sheet))
                else {
                    return self.usage(usage);
                };
                let (mut row_in, mut col_in) = (None, None);
                while let Some(w) = p.next() {
                    let cell = p.next().and_then(|c| refs::cell(&self.wb, c, s));
                    match (w.to_ascii_lowercase().as_str(), cell) {
                        ("row", Some(c)) => row_in = Some(c),
                        ("col" | "column", Some(c)) => col_in = Some(c),
                        _ => return self.usage(usage),
                    }
                }
                if row_in.is_none() && col_in.is_none() {
                    return self.usage(usage);
                }
                let t = Instant::now();
                match self.wb.add_data_table(s, rect, row_in, col_in) {
                    Ok(r) => self.report(&r, t.elapsed()),
                    Err(e) => println!("  {e}"),
                }
            }
            "clear" => match refs::range(&self.wb, rest, self.sheet) {
                Some((s, rect)) => {
                    let t = Instant::now();
                    match self.wb.clear_range(s, rect) {
                        Ok(r) => self.report(&r, t.elapsed()),
                        Err(e) => println!("  {e}"),
                    }
                }
                None => println!("  usage: clear A1[:B2]"),
            },
            "array" | "merge" | "unmerge" => {
                let (range, arg) = rest.split_once(char::is_whitespace).unwrap_or((rest, ""));
                let t = Instant::now();
                let Some((s, rect)) = refs::range(&self.wb, range, self.sheet) else {
                    return self
                        .usage("array C1:C3 =A1:A3*2 | merge A1:B2 [across] | unmerge A1:B2");
                };
                let r = match head.to_ascii_lowercase().as_str() {
                    "array" if arg.trim().is_empty() => {
                        return self.usage("array C1:C3 =A1:A3*2");
                    }
                    "array" => self.wb.set_array_formula(s, rect, arg.trim()),
                    "merge" => {
                        self.wb
                            .merge_cells(s, rect, arg.trim().eq_ignore_ascii_case("across"))
                    }
                    _ => Ok(self.wb.unmerge_cells(s, rect)),
                };
                match r {
                    Ok(r) => self.report(&r, t.elapsed()),
                    Err(e) => println!("  {e}"),
                }
            }
            "legacy" => {
                let (cell, input) = rest.split_once(char::is_whitespace).unwrap_or((rest, ""));
                let t = Instant::now();
                match refs::cell(&self.wb, cell, self.sheet) {
                    Some(a) => match self.wb.set_legacy_input(a, input.trim()) {
                        Ok(r) => {
                            println!("  {}", self.wb.formula_text(a).unwrap_or_default());
                            self.report(&r, t.elapsed());
                        }
                        Err(e) => println!("  {e}"),
                    },
                    None => return self.usage("legacy A1 =SUM(A2:A4*2)"),
                }
            }
            _ => match refs::cell(&self.wb, head, self.sheet) {
                Some(addr) if rest.is_empty() => self.describe(addr),
                Some(addr) => {
                    let t = Instant::now();
                    match self.wb.set_input(addr, rest) {
                        Ok(r) => self.report(&r, t.elapsed()),
                        Err(e) => println!("  {e}"),
                    }
                }
                None if head.contains('!') => {
                    println!("  '{head}' is not a valid reference (is there such a sheet?)")
                }
                None => println!("  unknown command or cell '{head}' (try help)"),
            },
        }
        true
    }

    fn usage(&self, text: &str) -> bool {
        println!("  usage: {text}");
        true
    }

    fn timed(&mut self, f: impl FnOnce(&mut Workbook) -> RecalcReport) {
        let t = Instant::now();
        let r = f(&mut self.wb);
        self.report(&r, t.elapsed());
    }
}

fn quoted_words(s: &str) -> Vec<String> {
    let (mut out, mut cur, mut q, mut any) = (Vec::new(), String::new(), false, false);
    let mut it = s.chars().peekable();
    while let Some(c) = it.next() {
        match c {
            '"' if q && it.peek() == Some(&'"') => {
                cur.push('"');
                it.next();
            }
            '"' => {
                q = !q;
                any = true;
            }
            c if c.is_whitespace() && !q => {
                if any || !cur.is_empty() {
                    out.push(std::mem::take(&mut cur));
                    any = false;
                }
            }
            c => cur.push(c),
        }
    }
    if any || !cur.is_empty() {
        out.push(cur);
    }
    out
}

fn main() {
    let child = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(run)
        .expect("spawn console thread");
    if child.join().is_err() {
        std::process::exit(101);
    }
}

fn run() {
    let wb = Workbook::new();
    let sheet = wb.sheet_at(0).id;
    let mut console = Console { wb, sheet };
    let interactive = io::stdin().is_terminal();
    if interactive {
        println!("xlcalc - an Excel-compatible calculation engine. Type help for commands.");
    }
    let stdin = io::stdin();
    let mut input = stdin.lock();
    let mut buf = Vec::new();
    loop {
        if interactive {
            print!("{}> ", console.sheet_name(console.sheet));
            io::stdout().flush().ok();
        }
        buf.clear();
        match input.read_until(b'\n', &mut buf) {
            Ok(0) | Err(_) => break,
            Ok(_) => {}
        }

        let text = String::from_utf8_lossy(&buf);
        let line = text.trim_end_matches(['\n', '\r']);
        if !interactive && !line.trim().is_empty() && !line.trim_start().starts_with('#') {
            println!("> {line}");
        }

        let outcome =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| console.command(line)));
        match outcome {
            Ok(true) => {}
            Ok(false) => break,
            Err(_) => println!("  internal error while running that command (it was not applied)"),
        }
    }
}
