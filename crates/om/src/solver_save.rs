use xlcalc::{CellAddr, Rect};
use xlcalc_variant::{Locale, Variant};

use crate::app::Application;
use crate::conv::{input_locale, is_missing, Args};
use crate::error::{OmError, DISP_E_PARAMNOTOPTIONAL};
use crate::range::enter_text;
use crate::solver::{int, missing, parse_ref, Ctx, Ref};

pub(crate) fn vba_cstr(x: f64) -> String {
    if x == 0.0 {
        return "0".into();
    }
    if !x.is_finite() {
        return x.to_string();
    }
    let e = format!("{:.14e}", x.abs());
    let (mant, exp) = e.split_once('e').unwrap_or((&e, "0"));
    let exp: i32 = exp.parse().unwrap_or(0);
    let digits: String = mant.chars().filter(char::is_ascii_digit).collect();
    let digits = digits.trim_end_matches('0');
    let digits = if digits.is_empty() { "0" } else { digits };
    let sign = if x < 0.0 { "-" } else { "" };
    if !(-4..15).contains(&exp) {
        let m = if digits.len() > 1 {
            format!("{}.{}", &digits[..1], &digits[1..])
        } else {
            digits.to_string()
        };
        return format!(
            "{sign}{m}E{}{:02}",
            if exp < 0 { '-' } else { '+' },
            exp.abs()
        );
    }
    let body = if exp >= 0 {
        let int_len = exp as usize + 1;
        if digits.len() <= int_len {
            format!("{digits}{}", "0".repeat(int_len - digits.len()))
        } else {
            format!("{}.{}", &digits[..int_len], &digits[int_len..])
        }
    } else {
        format!("0.{}{digits}", "0".repeat((-exp - 1) as usize))
    };
    format!("{sign}{body}")
}

fn utf16_lt(a: &str, b: &str) -> bool {
    a.encode_utf16().lt(b.encode_utf16())
}

fn list_item(list: &[String], i: i32) -> Option<&String> {
    usize::try_from(i - 1).ok().and_then(|k| list.get(k))
}

fn search(list: &[String], entry: &str, start: i32, end: i32) -> Option<i32> {
    if end <= start {
        let l = list_item(list, start + 1)?;
        return Some(if utf16_lt(entry, l) { start } else { start + 1 });
    }
    let mid = (end + start).div_euclid(2);
    let l = list_item(list, mid + 1)?;
    if utf16_lt(l, entry) {
        search(list, entry, mid + 1, end)
    } else {
        search(list, entry, start, mid - 1)
    }
}

fn add_item(list: &mut Vec<String>, text: String, index: i32) {
    if index == 0 {
        list.push(text);
    } else if let Ok(k) = usize::try_from(index - 1) {
        if k <= list.len() {
            list.insert(k, text);
        }
    }
}

pub(crate) struct Dlg {
    pub obj: String,
    pub adj: String,
    pub typ: i32,
    pub valof: String,
    pub list: Vec<String>,
    pub nonneg: bool,
    pub engine: i32,
}

struct Opts {
    time: String,
    iter: String,
    pre: String,
    cvg: String,
    pop: String,
    mutate: String,
    seed: String,
    limit: String,
    forward: bool,
    scaling: bool,
    show_iter: bool,
    multi: bool,
    bounds: bool,
    max_sol: String,
    max_sub: String,
    gap: String,
    relax: bool,
}

fn is_numeric(s: &str) -> bool {
    let t = s.trim();
    !t.is_empty() && t.parse::<f64>().is_ok()
}

fn cdbl(s: &str) -> f64 {
    s.trim().parse().unwrap_or(0.0)
}

fn bool_text(b: bool) -> &'static str {
    if b {
        "TRUE"
    } else {
        "FALSE"
    }
}

impl Application<'_> {
    pub(crate) fn sol_dlg(&self, c: &Ctx) -> Dlg {
        let addr_of = |t: String| parse_ref(&t, &c.sname).map_or(t, |r| r.addr);
        let obj = self
            .sol_text(c, "solver_opt")
            .map(addr_of)
            .unwrap_or_default();
        let adj = self
            .sol_text(c, "solver_adj")
            .map(|t| {
                t.split(',')
                    .map(|p| addr_of(p.to_string()))
                    .collect::<Vec<_>>()
                    .join(",")
            })
            .unwrap_or_default();

        let mut err = false;
        let valof = match self.sol_text(c, "solver_val") {
            Some(t) => t
                .trim()
                .parse::<f64>()
                .map_or_else(|_| "0".into(), vba_cstr),
            None => {
                err = true;
                "0".into()
            }
        };
        let typ = match self.sol_num(c, "solver_typ") {
            Some(t) if (1.0..=3.0).contains(&t) => t as i32,
            Some(_) => 1,
            None => {
                err = true;
                1
            }
        };
        let num = self.sol_num(c, "solver_num");
        if num.is_none() {
            err = true;
        }
        let mut list: Vec<String> = Vec::new();
        let mut index = 0;
        for i in 1..=num.unwrap_or(0.0) as i64 {
            let Some(left) = self
                .sol_text(c, &format!("solver_lhs{i}"))
                .and_then(|t| parse_ref(&t, &c.sname))
            else {
                continue;
            };
            let rel = self.sol_num(c, &format!("solver_rel{i}")).unwrap_or(0.0) as i32;
            let (middle, right) = match rel {
                1..=3 => {
                    let raw = self
                        .sol_text(c, &format!("solver_rhs{i}"))
                        .unwrap_or_default();
                    let right = if is_numeric(&raw) {
                        vba_cstr(cdbl(&raw))
                    } else {
                        err = false;
                        match parse_ref(&raw, &c.sname) {
                            Some(r) => r.addr,
                            None => {
                                err = true;
                                crate::solver::strip_sheet(&raw, &c.sname)
                            }
                        }
                    };
                    (["<=", "=", ">="][(rel - 1) as usize], right)
                }
                4 => ("=", "integer".to_string()),
                5 => ("=", "binary".to_string()),
                6 => ("=", "AllDifferent".to_string()),
                _ => continue,
            };
            let entry = format!("{} {} {}", left.addr, middle, right.trim());
            if i > 1 {
                if let Some(v) = search(&list, &entry, 1, list.len() as i32) {
                    index = v;
                }
            } else {
                index = 0;
            }
            add_item(&mut list, entry, index);
        }
        let neg = self.sol_get(c, "solver_neg");
        let nonneg = neg.as_deref() == Some("=1") || neg.is_none() || err;
        let engine = match self.sol_num(c, "solver_eng") {
            Some(e) => e as i32,
            None => {
                if self.sol_num(c, "solver_lin") == Some(1.0) {
                    2
                } else {
                    1
                }
            }
        };
        Dlg {
            obj,
            adj,
            typ,
            valof,
            list,
            nonneg,
            engine,
        }
    }

    fn sol_opts(&self, c: &Ctx) -> Opts {
        let mut o = Opts {
            time: String::new(),
            iter: String::new(),
            pre: "1E-06".into(),
            cvg: "0.0001".into(),
            pop: "100".into(),
            mutate: "0.075".into(),
            seed: "0".into(),
            limit: "30".into(),
            forward: true,
            scaling: true,
            show_iter: false,
            multi: false,
            bounds: true,
            max_sol: String::new(),
            max_sub: String::new(),
            gap: "1".into(),
            relax: false,
        };
        let text = |n: &str| self.sol_text(c, n);
        let set = |field: &mut String, n: &str| {
            if let Some(t) = text(n) {
                *field = t;
            }
        };
        set(&mut o.time, "solver_tim");
        set(&mut o.iter, "solver_itr");
        set(&mut o.pre, "solver_pre");
        set(&mut o.cvg, "solver_cvg");

        let mut dummy = String::new();
        let mut read = |n: &str| {
            if let Some(t) = self.sol_get(c, n) {
                dummy = t;
            }
            dummy == "=1"
        };
        o.show_iter = read("solver_sho");
        o.scaling = read("solver_scl");
        let mut err = false;
        let miss = |n: &str| self.sol_get(c, n).is_none();
        err |= miss("solver_drv");
        o.forward = read("solver_drv");
        set(&mut o.pop, "solver_ssz");
        set(&mut o.mutate, "solver_mrt");
        set(&mut o.seed, "solver_rsd");
        err |= miss("solver_ssz") | miss("solver_mrt") | miss("solver_rsd") | miss("solver_msl");
        o.multi = read("solver_msl") && !err;
        o.bounds = read("solver_rbv");
        set(&mut o.max_sub, "solver_nod");
        set(&mut o.max_sol, "solver_mip");
        if let Some(t) = text("solver_tol") {
            o.gap = vba_cstr(100.0 * cdbl(&t));
        }
        o.relax = read("solver_rlx");
        set(&mut o.limit, "solver_mni");
        for f in [&mut o.time, &mut o.iter, &mut o.max_sub, &mut o.max_sol] {
            if cdbl(f) == 2147483647.0 {
                f.clear();
            }
        }
        o
    }

    fn sol_cells(r: &Ref) -> Vec<(u32, u32)> {
        let mut v = Vec::new();
        for row in r.r0..r.r0 + r.rows {
            for col in r.c0..r.c0 + r.cols {
                v.push((row, col));
            }
        }
        v
    }

    fn sol_addr(c: &Ctx, cell: (u32, u32)) -> CellAddr {
        CellAddr::new(c.sheet, cell.0, cell.1)
    }

    fn sol_put_formula(
        &mut self,
        c: &Ctx,
        cell: (u32, u32),
        text: &str,
        array: bool,
    ) -> Result<(), OmError> {
        let wb = &mut self.book_mut(c.book)?.wb;
        if array {
            let locale = input_locale(Locale::EN_US.0, wb.locale());
            let rect = Rect::cell(cell.0, cell.1);
            wb.with_input_locale(&locale, |wb| wb.set_array_formula(c.sheet, rect, text))
                .map_err(crate::objects::edit_error)?;
            Ok(())
        } else {
            enter_text(wb, Self::sol_addr(c, cell), text, true, Locale::EN_US.0)
        }
    }

    fn sol_cell_formula(&self, c: &Ctx, cell: (u32, u32)) -> Result<(String, bool), OmError> {
        let wb = &self.book(c.book)?.wb;
        let a = Self::sol_addr(c, cell);
        let Variant::Bstr(t) = Self::formula_variant(wb, a, false, true) else {
            return Ok((String::new(), false));
        };
        Ok((t, wb.current_array(a).is_some()))
    }

    fn sol_area(&self, c: &Ctx, a: &Args) -> Result<Option<Ref>, OmError> {
        let Some(v) = a.get(1) else {
            return Err(OmError::exception(DISP_E_PARAMNOTOPTIONAL.0, None));
        };
        if is_missing(v) {
            return Err(OmError::exception(DISP_E_PARAMNOTOPTIONAL.0, None));
        }
        Ok(self.sol_ref(c, Some(v)))
    }

    pub(crate) fn sol_save(&mut self, c: &Ctx, a: &Args) -> Result<Variant, OmError> {
        let Some(area) = self.sol_area(c, a)? else {
            return int(1);
        };
        let dlg = self.sol_dlg(c);
        let num = self.sol_num(c, "solver_num").unwrap_or(0.0) as i64;
        let count = area.rows as i64 * area.cols as i64;
        if count > 1 && count < num + 4 {
            return int(2);
        }

        let broken = self.sol_get(c, "solver_num").is_none();
        let ncon = if broken { 1 } else { dlg.list.len() };
        let rows = if count == 1 {
            area.rows + 4 + ncon as u32
        } else {
            area.rows
        };
        let area = Ref { rows, ..area };
        let cells = Self::sol_cells(&area);
        let opts = self.sol_opts(c);
        let mut i: i64 = -1;
        for &cell in &cells {
            if i == -1 {
                if !dlg.obj.is_empty() {
                    let f = match dlg.typ {
                        1 => format!("=MAX({})", dlg.obj),
                        2 => format!("=MIN({})", dlg.obj),
                        _ => format!("={}={}", dlg.obj, dlg.valof),
                    };
                    self.sol_put_formula(c, cell, &f, false)?;
                }
            } else if i <= 0 {
                if !dlg.adj.is_empty() {
                    self.sol_put_formula(c, cell, &format!("=COUNT({})", dlg.adj), false)?;
                }
            } else if i == 1 + ncon as i64 {
                let f = format!("={{{}}}", Self::sol_options1(&dlg, &opts));
                self.sol_put_formula(c, cell, &f, false)?;
            } else if i == 2 + ncon as i64 {
                let f = format!("={{{}}}", Self::sol_options2(&dlg, &opts));
                self.sol_put_formula(c, cell, &f, false)?;
                break;
            } else if broken {
                return int(0);
            } else {
                let item = &dlg.list[(i - 1) as usize];
                let (f, multi) = self.sol_constraint_formula(c, item);
                self.sol_put_formula(c, cell, &f, multi)?;
            }
            i += 1;
            if i > 2 + ncon as i64 {
                break;
            }
        }
        self.sol_move_alldiff(c, &cells)?;
        int(0)
    }

    fn sol_options1(dlg: &Dlg, o: &Opts) -> String {
        let limit = |t: &str| {
            if t.trim().is_empty() || t == "0" || cdbl(t) > 32767.0 {
                "32767".to_string()
            } else {
                t.to_string()
            }
        };
        let v = [
            limit(&o.time),
            limit(&o.iter),
            o.pre.clone(),
            vba_cstr(cdbl(&o.gap) / 100.0),
            bool_text(dlg.engine == 2).into(),
            bool_text(o.show_iter).into(),
            bool_text(o.scaling).into(),
            "1".into(),
            if o.forward { "1" } else { "2" }.into(),
            "1".into(),
            o.cvg.clone(),
            bool_text(dlg.nonneg).into(),
        ];
        v.join(",")
    }

    fn sol_options2(dlg: &Dlg, o: &Opts) -> String {
        let zero = |t: &str| {
            if t.trim().is_empty() {
                "0".to_string()
            } else {
                t.to_string()
            }
        };
        let v = [
            zero(&o.time),
            zero(&o.iter),
            dlg.engine.to_string(),
            o.pop.clone(),
            if o.seed.is_empty() {
                "0".into()
            } else {
                o.seed.clone()
            },
            bool_text(o.multi).into(),
            bool_text(o.bounds).into(),
            o.mutate.clone(),
            zero(&o.max_sub),
            zero(&o.max_sol),
            bool_text(o.relax).into(),
            o.limit.clone(),
        ];
        v.join(",")
    }

    fn sol_constraint_formula(&self, c: &Ctx, item: &str) -> (String, bool) {
        let addr = |t: &str| parse_ref(t, &c.sname).map_or_else(|| t.to_string(), |r| r.addr);
        let multi = |t: &str| parse_ref(t, &c.sname).is_some_and(|r| r.rows * r.cols > 1);
        for (msg, make) in [("integer", 0), ("binary", 1), ("AllDifferent", 2)] {
            if item.ends_with(msg) {
                let left = item.find(&format!(" = {msg}")).map_or(item, |p| &item[..p]);
                let l = addr(left);
                let f = match make {
                    0 => format!("={l}=INT({l})"),
                    1 => format!("=({l}=0)+({l}=1)=1"),
                    _ => format!("={l}=COUNT({l})"),
                };
                return (f, multi(left));
            }
        }
        let left = item.split(' ').next().unwrap_or("");
        let f = format!("={}", item.replace(' ', ""));
        (f, multi(left))
    }

    fn sol_move_alldiff(&mut self, c: &Ctx, cells: &[(u32, u32)]) -> Result<(), OmError> {
        let n = cells.len() as i64;
        let mut new_place = n - 1;
        let mut i: i64 = 1;
        while i <= n - 2 {
            if i == new_place {
                break;
            }
            let (text, _) = self.sol_cell_formula(c, cells[(i - 1) as usize])?;
            if text.find("=COUNT(").is_some_and(|p| p + 1 > 2) {
                for j in i..=n - 2 {
                    let (t, arr) = self.sol_cell_formula(c, cells[j as usize])?;
                    self.sol_put_formula(c, cells[(j - 1) as usize], &t, arr)?;
                }
                self.sol_put_formula(c, cells[(new_place - 1) as usize], &text, true)?;
                i -= 1;
                new_place -= 1;
            }
            i += 1;
        }
        Ok(())
    }

    pub(crate) fn sol_load(&mut self, c: &Ctx, a: &Args) -> Result<Variant, OmError> {
        let Some(area) = self.sol_area(c, a)? else {
            return int(1);
        };
        let merge = a.get(2).filter(|v| !missing(Some(v)));
        let reset = match merge {
            None => true,
            Some(Variant::Bstr(s)) => s != "TRUE",
            Some(Variant::Bool(b)) => !*b,
            Some(_) => return int(2),
        };
        if reset {
            self.sol_reset(c)?;
        }
        let mut dlg = self.sol_dlg(c);
        let mut done = false;
        let mut first_options = true;
        for cell in Self::sol_cells(&area) {
            let (cand, _) = self.sol_cell_formula(c, cell)?;
            let norm = |t: &str| parse_ref(t, &c.sname).map_or_else(|| t.to_string(), |r| r.addr);
            if !done && !cand.is_empty() {
                let inner = |s: &str| {
                    let n = s.chars().count();
                    s.chars()
                        .skip(5)
                        .take(n.saturating_sub(6))
                        .collect::<String>()
                };
                if cand.starts_with("=MAX") || cand.starts_with("=MIN") {
                    dlg.obj = norm(&inner(&cand));
                    dlg.typ = if cand.starts_with("=MAX") { 1 } else { 2 };
                    dlg.valof = "0".into();
                } else if cand.starts_with("=COUNT") {
                    dlg.obj.clear();
                    dlg.typ = 1;
                    dlg.valof = "0".into();
                } else if let Some(p) = cand[1..].find('=').map(|p| p + 1) {
                    dlg.obj = norm(&cand[1..p]);
                    dlg.typ = 3;
                    dlg.valof = vba_cstr(cdbl(&cand[p + 1..]));
                } else {
                    done = true;
                }
            }
            if cand.starts_with("=COUNT") {
                let n = cand.chars().count();
                let t: String = cand.chars().skip(7).take(n.saturating_sub(8)).collect();
                dlg.adj = t.split(',').map(&norm).collect::<Vec<_>>().join(",");
            } else if done {
                if cand.is_empty() {
                    continue;
                }
                if cand[1..].find('=').is_none() {
                    if self.sol_load_options(c, &cand, first_options, &mut dlg)? {
                        break;
                    }
                    if first_options {
                        first_options = false;
                    } else {
                        break;
                    }
                } else {
                    let entry = Self::sol_load_constraint(c, &cand);
                    if let Some(entry) = entry {
                        let idx = if dlg.list.len() > 1 {
                            search(&dlg.list, &entry, 0, dlg.list.len() as i32 - 1).unwrap_or(0)
                        } else {
                            0
                        };
                        add_item(&mut dlg.list, entry, idx);
                    }
                }
            }
            if !cand.is_empty() {
                done = true;
            }
        }
        self.sol_define_model(c, &dlg)?;
        int(0)
    }

    fn sol_load_constraint(c: &Ctx, cand: &str) -> Option<String> {
        let norm = |t: &str| parse_ref(t, &c.sname).map_or_else(|| t.to_string(), |r| r.addr);
        let chars: Vec<char> = cand.chars().collect();
        let mid = |start: usize, len: usize| -> String {
            chars.iter().skip(start - 1).take(len).collect()
        };
        let pos = |pat: &str| cand.find(pat).map(|p| cand[..p].chars().count() + 1);
        if let Some(p) = pos("=0)+(") {
            return Some(format!("{} = binary", norm(&mid(3, p - 3))));
        }
        if let Some(p) = pos("=INT(") {
            return Some(format!("{} = integer", norm(&mid(3, p - 3))));
        }
        if let Some(p) = pos("=COUNT(") {
            return Some(format!("{} = AllDifferent", norm(&mid(3, p - 3))));
        }
        for op in ["<=", ">="] {
            if let Some(p) = pos(op) {
                let rest: String = chars.iter().skip(p + 1).collect();
                return Some(format!("{} {op} {}", norm(&mid(2, p - 2)), norm(&rest)));
            }
        }
        let p = chars.iter().skip(1).position(|&ch| ch == '=')? + 2;
        let rest: String = chars.iter().skip(p).collect();
        Some(format!("{} = {}", norm(&mid(2, p - 2)), norm(&rest)))
    }

    fn sol_load_options(
        &mut self,
        c: &Ctx,
        cand: &str,
        first: bool,
        dlg: &mut Dlg,
    ) -> Result<bool, OmError> {
        let n = cand.chars().count();
        let mut rest: String = cand.chars().skip(2).take(n.saturating_sub(3)).collect();
        rest = rest.trim().to_string();
        let mut ol: Vec<String> = vec![String::new(); 12];
        for slot in ol.iter_mut().skip(1) {
            if let Some(p) = rest.find(',') {
                *slot = rest[..p].to_string();
            }
            let skip = slot.chars().count() + 1;
            rest = rest.chars().skip(skip).collect();
        }
        let s = |i: usize| Some(Variant::Bstr(ol[i].clone()));
        let last = Some(Variant::Bstr(rest.clone()));
        let args = |v: Vec<Option<Variant>>, a: &Args| {
            let mut values = vec![None];
            values.extend(v);
            Args {
                values,
                put: None,
                lcid: a.lcid,
            }
        };
        let proto = Args {
            values: Vec::new(),
            put: None,
            lcid: Locale::EN_US.0,
        };
        if first {
            if ol[11].is_empty() && ol[10].is_empty() {
                return Ok(true);
            }
            let gap = Some(Variant::R8(cdbl(&ol[4]) * 100.0));
            let conv = if ol[11].is_empty() { None } else { s(11) };
            let v = vec![
                s(1),
                s(2),
                s(3),
                Some(Variant::Bool(false)),
                s(6),
                s(8),
                s(9),
                s(10),
                gap,
                s(7),
                conv,
                last,
            ];

            self.sol_options(c, &args(v, &proto))?;
            dlg.nonneg = self.sol_get(c, "solver_neg").as_deref() == Some("=1");
            if ol[5] == "TRUE" {
                dlg.engine = 2;
            }
        } else {
            let mut v = vec![s(1), s(2)];
            v.extend(vec![None; 10]);
            v.extend([s(4), s(5), s(6), s(7), s(8), s(9), s(10), s(11), last]);
            self.sol_options(c, &args(v, &proto))?;
            dlg.engine = ol[3].trim().parse().unwrap_or(dlg.engine);
        }
        Ok(false)
    }

    fn sol_define_model(&mut self, c: &Ctx, dlg: &Dlg) -> Result<(), OmError> {
        let qual_list = |c: &Ctx, t: &str| {
            t.split(',')
                .map(|p| match parse_ref(p, &c.sname) {
                    Some(r) => format!("{}!{}", crate::solver::quoted(&c.sname), r.addr),
                    None => p.to_string(),
                })
                .collect::<Vec<_>>()
                .join(",")
        };
        if dlg.obj.is_empty() {
            self.sol_del(c, "solver_opt")?;
        } else {
            self.sol_set(c, "solver_opt", &format!("={}", qual_list(c, &dlg.obj)))?;
        }
        if dlg.adj.is_empty() {
            self.sol_del(c, "solver_adj")?;
        } else {
            self.sol_set(c, "solver_adj", &format!("={}", qual_list(c, &dlg.adj)))?;
        }
        self.sol_set(c, "solver_typ", &format!("={}", dlg.typ))?;
        self.sol_set(c, "solver_val", &format!("={}", dlg.valof))?;
        self.sol_set(c, "solver_num", &format!("={}", dlg.list.len()))?;
        self.sol_set(c, "solver_eng", &format!("={}", dlg.engine))?;
        self.sol_set(c, "solver_neg", if dlg.nonneg { "=1" } else { "=2" })?;
        self.sol_set(c, "solver_ver", "=3")?;
        for (k, item) in dlg.list.iter().enumerate() {
            let i = k + 1;
            let (rel, left, right) = if let Some(p) = item.find(" <= ") {
                (1, &item[..p], item[p + 4..].to_string())
            } else if let Some(p) = item.find(" >= ") {
                (3, &item[..p], item[p + 4..].to_string())
            } else {
                let mut found = None;
                for (r, m) in [(4, "integer"), (5, "binary"), (6, "AllDifferent")] {
                    if let Some(p) = item.find(&format!(" = {m}")) {
                        found = Some((r, &item[..p], format!("\"{m}\"")));
                        break;
                    }
                }
                match found {
                    Some(f) => f,
                    None => {
                        let p = item.find(" = ").unwrap_or(item.len());
                        (2, &item[..p], item.get(p + 3..).unwrap_or("").to_string())
                    }
                }
            };
            let l = parse_ref(left, &c.sname).map_or_else(|| left.to_string(), |r| r.addr);
            self.sol_set(
                c,
                &format!("solver_lhs{i}"),
                &format!("={}!{l}", crate::solver::quoted(&c.sname)),
            )?;
            self.sol_set(c, &format!("solver_rel{i}"), &format!("={rel}"))?;
            let right = right.strip_prefix('=').unwrap_or(&right).to_string();
            self.sol_set(c, &format!("solver_rhs{i}"), &format!("={right}"))?;
        }
        self.sol_set(c, "solver_eng", &format!("={}", dlg.engine))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::vba_cstr;

    #[test]
    fn cstr_like_vba() {
        assert_eq!(vba_cstr(0.0001), "0.0001");
        assert_eq!(vba_cstr(0.00001), "1E-05");
        assert_eq!(vba_cstr(0.000001), "1E-06");
        assert_eq!(vba_cstr(100.0), "100");
        assert_eq!(vba_cstr(0.075), "0.075");
        assert_eq!(vba_cstr(7.000000000000001), "7");
        assert_eq!(vba_cstr(1e15), "1E+15");
        assert_eq!(vba_cstr(-12.5), "-12.5");
        assert_eq!(vba_cstr(123456.789), "123456.789");
    }
}
