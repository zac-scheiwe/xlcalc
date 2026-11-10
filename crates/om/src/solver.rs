use xlcalc::{format_general, CellAddr, SheetId, Value, XlError};
use xlcalc_variant::{SafeArray, VarType, Variant};

use crate::app::Application;
use crate::conv::{error_scode, Args};
use crate::error::{OmError, DISP_E_PARAMNOTOPTIONAL};

const PLUS_INF: &str = "2147483647";

pub(crate) struct Ctx {
    pub(crate) book: crate::BookId,
    pub(crate) sheet: SheetId,
    pub(crate) sname: String,
}

pub(crate) struct Ref {
    pub(crate) addr: String,
    pub(crate) rows: u32,
    pub(crate) cols: u32,
    pub(crate) r0: u32,
    pub(crate) c0: u32,
}

impl Ref {
    fn count(&self) -> u64 {
        u64::from(self.rows) * u64::from(self.cols)
    }
    fn contains(&self, o: &Ref) -> bool {
        o.r0 >= self.r0
            && o.c0 >= self.c0
            && o.r0 + o.rows <= self.r0 + self.rows
            && o.c0 + o.cols <= self.c0 + self.cols
    }
}

fn cell(s: &str) -> Option<(u32, u32)> {
    let s = s.replace('$', "");
    let split = s.find(|c: char| c.is_ascii_digit())?;
    let (l, d) = s.split_at(split);
    if l.is_empty() || !l.chars().all(|c| c.is_ascii_alphabetic()) {
        return None;
    }
    let col = xlcalc::letters_to_col(l)?;
    let row: u32 = d.parse().ok()?;
    (1..=1_048_576).contains(&row).then_some((row - 1, col))
}

pub(crate) fn parse_ref(text: &str, sheet: &str) -> Option<Ref> {
    let mut t = text.trim();
    if let Some((s, rest)) = t.rsplit_once('!') {
        if !s.trim_matches('\'').eq_ignore_ascii_case(sheet) {
            return None;
        }
        t = rest;
    }
    let (a, b) = match t.split_once(':') {
        Some((a, b)) => (cell(a)?, cell(b)?),
        None => (cell(t)?, cell(t)?),
    };
    let (r0, r1) = (a.0.min(b.0), a.0.max(b.0));
    let (c0, c1) = (a.1.min(b.1), a.1.max(b.1));
    let one = |r: u32, c: u32| format!("${}${}", xlcalc::col_to_letters(c), r + 1);
    let addr = if (r0, c0) == (r1, c1) {
        one(r0, c0)
    } else {
        format!("{}:{}", one(r0, c0), one(r1, c1))
    };
    Some(Ref {
        addr,
        rows: r1 - r0 + 1,
        cols: c1 - c0 + 1,
        r0,
        c0,
    })
}

pub(crate) fn quoted(sheet: &str) -> String {
    let plain = !sheet.is_empty()
        && sheet.chars().all(|c| c.is_alphanumeric() || c == '_')
        && !sheet.starts_with(|c: char| c.is_ascii_digit());
    if plain {
        sheet.to_string()
    } else {
        format!("'{}'", sheet.replace('\'', "''"))
    }
}

pub(crate) fn missing(v: Option<&Variant>) -> bool {
    matches!(v, None | Some(Variant::Empty | Variant::Error(_)))
}

pub(crate) fn number(v: &Variant) -> Option<f64> {
    match v {
        Variant::I2(n) => Some(f64::from(*n)),
        Variant::I4(n) => Some(f64::from(*n)),
        Variant::R4(n) => Some(f64::from(*n)),
        Variant::R8(n) => Some(*n),
        Variant::Bool(b) => Some(if *b { -1.0 } else { 0.0 }),
        Variant::Bstr(s) => s.trim().parse().ok(),
        _ => None,
    }
}

pub(crate) fn int(n: i16) -> Result<Variant, OmError> {
    Ok(Variant::I2(n))
}

pub(crate) fn value_error() -> Variant {
    Variant::Error(error_scode(XlError::VALUE))
}

fn flag(v: &Variant) -> Option<bool> {
    match v {
        Variant::Bool(b) => Some(*b),
        Variant::Bstr(s) => Some(s == "TRUE"),
        _ => None,
    }
}

impl Application<'_> {
    fn solver_ctx(&self) -> Option<Ctx> {
        let book = self.active?;
        let wb = &self.book(book).ok()?.wb;
        let sheet = wb.active_sheet();
        Some(Ctx {
            book,
            sheet,
            sname: wb.sheet(sheet)?.name.clone(),
        })
    }

    pub(crate) fn sol_get(&self, c: &Ctx, name: &str) -> Option<String> {
        let wb = &self.book(c.book).ok()?.wb;
        let d = wb.names().get(&name.to_uppercase(), Some(c.sheet))?;
        Some(format!("={}", wb.name_refers_to(d)))
    }

    pub(crate) fn sol_text(&self, c: &Ctx, name: &str) -> Option<String> {
        self.sol_get(c, name)
            .map(|s| s.strip_prefix('=').unwrap_or(&s).to_string())
    }

    pub(crate) fn sol_num(&self, c: &Ctx, name: &str) -> Option<f64> {
        self.sol_text(c, name)?.trim().parse().ok()
    }

    pub(crate) fn sol_set(&mut self, c: &Ctx, name: &str, refers: &str) -> Result<(), OmError> {
        let wb = &mut self.book_mut(c.book)?.wb;
        wb.define_name(name, refers, Some(c.sheet))
            .map_err(|_| OmError::app_defined())?;
        wb.set_name_visible(&name.to_uppercase(), Some(c.sheet), false);
        Ok(())
    }

    pub(crate) fn sol_del(&mut self, c: &Ctx, name: &str) -> Result<(), OmError> {
        let wb = &mut self.book_mut(c.book)?.wb;
        wb.delete_name(name, Some(c.sheet));
        Ok(())
    }

    pub(crate) fn sol_ref(&self, c: &Ctx, v: Option<&Variant>) -> Option<Ref> {
        match v? {
            Variant::Bstr(s) => parse_ref(s, &c.sname),
            d @ Variant::Dispatch(_) => {
                let (_, r) = self.range_arg(Some(d)).ok()?;
                parse_ref(&r.a1(), &c.sname)
            }
            _ => None,
        }
    }

    fn sol_qual(&self, c: &Ctx, r: &Ref) -> String {
        format!("={}!{}", quoted(&c.sname), r.addr)
    }

    fn sol_is_numeric(&self, c: &Ctx, text: &str) -> bool {
        let wb = &self.book(c.book).map(|b| &b.wb);
        let Ok(wb) = wb else { return false };
        let a1 = CellAddr::new(c.sheet, 0, 0);
        match wb.cfv_eval(text, a1, a1) {
            Value::Number(_) | Value::Bool(_) | Value::Blank => true,
            Value::Text(t) => t.trim().parse::<f64>().is_ok(),
            _ => false,
        }
    }

    pub(crate) fn solver_run(&mut self, proc: &str, a: &Args) -> Option<Result<Variant, OmError>> {
        if !self.addin_installed("Solver Add-in") {
            return None;
        }
        let c = self.solver_ctx()?;
        let p = proc.to_ascii_lowercase();
        Some(match p.as_str() {
            "solverok" => self.sol_ok(&c, a),
            "solveradd" => self.sol_constraint(&c, a, Kind::Add),
            "solverchange" => self.sol_constraint(&c, a, Kind::Change),
            "solverdelete" => self.sol_constraint(&c, a, Kind::Delete),
            "solverreset" => self.sol_reset(&c),
            "solveroptions" => self.sol_options(&c, a),
            "solverget" => self.sol_get_proc(&c, a),
            "solversolve" => self.sol_solve(&c, a),
            "solverfinish" => self.sol_finish(&c, a),
            "solversave" => self.sol_save(&c, a),
            "solverload" => self.sol_load(&c, a),
            _ => return None,
        })
    }

    fn sol_ok(&mut self, c: &Ctx, a: &Args) -> Result<Variant, OmError> {
        let arg = |i: usize| a.get(i + 1).filter(|v| !missing(Some(v)));
        let set = arg(0);
        let mm = arg(1);
        let mut value_of = arg(2).cloned();
        let mut set_ref = None;
        if let Some(s) = set {
            if mm.is_none() {
                return int(2);
            }

            match self.sol_ref(c, Some(s)) {
                Some(r) if r.count() == 1 => set_ref = Some(r),
                _ => return int(1),
            }
            let Some(m) = mm.and_then(number) else {
                return Ok(value_error());
            };
            if m != 1.0 && m != 2.0 && m != 3.0 {
                return int(2);
            }
            if value_of.is_none() {
                value_of = Some(Variant::I2(0));
            }
            if m == 3.0 && value_of.as_ref().and_then(number).is_none() {
                return int(3);
            }
        }
        let by = arg(3);
        if by.is_none() {
            return int(4);
        }
        let Some(by_ref) = self.sol_ref(c, by) else {
            return int(4);
        };
        if let Some(e) = arg(4) {
            match number(e) {
                Some(n) if n == 1.0 || n == 2.0 || n == 3.0 => {
                    self.sol_set(c, "solver_eng", &format!("={}", format_general(n)))?;
                }
                _ => return int(5),
            }
        }
        if let Some(d) = arg(5) {
            let Variant::Bstr(d) = d else {
                return int(6);
            };
            let n = if d.eq_ignore_ascii_case("GRG Nonlinear") {
                1
            } else if d.eq_ignore_ascii_case("Simplex LP") {
                2
            } else if d.eq_ignore_ascii_case("Evolutionary") {
                3
            } else {
                return int(6);
            };
            self.sol_set(c, "solver_eng", &format!("={n}"))?;
        }
        let q = self.sol_qual(c, &by_ref);
        self.sol_set(c, "solver_adj", &q)?;
        if let (Some(r), Some(m)) = (set_ref, mm) {
            let q = self.sol_qual(c, &r);
            self.sol_set(c, "solver_opt", &q)?;
            let m = number(m).unwrap_or(1.0);
            self.sol_set(c, "solver_typ", &format!("={}", format_general(m)))?;
            let v = value_of.as_ref().map(variant_text).unwrap_or_default();
            self.sol_set(c, "solver_val", &format!("={v}"))?;
        } else {
            self.sol_del(c, "solver_opt")?;
            self.sol_set(c, "solver_typ", "=1")?;
            self.sol_set(c, "solver_val", "=0")?;
        }
        int(0)
    }

    fn sol_constraint(&mut self, c: &Ctx, a: &Args, kind: Kind) -> Result<Variant, OmError> {
        let rel = a.get(2).filter(|v| !missing(Some(v)));
        let Some(rel) = rel else {
            return Err(OmError::exception(DISP_E_PARAMNOTOPTIONAL.0, None));
        };
        let Some(rel) = number(rel) else {
            return Err(OmError::type_mismatch());
        };
        let rel = rel.round();
        let cell_arg = a.get(1).filter(|v| !missing(Some(v)));
        let formula = a.get(3).filter(|v| !missing(Some(v)));
        let Some(lhs) = self.sol_ref(c, cell_arg) else {
            return int(1);
        };
        if !(1.0..=6.0).contains(&rel) {
            return int(2);
        }
        let rel = rel as i32;

        let mut rhs_text = None;
        let mut rhs_ref = None;
        if rel < 4 && kind != Kind::Delete {
            let Some(f) = formula else {
                return int(3);
            };
            {
                let t = match (kind, number(f), f) {
                    (Kind::Add, Some(n), _) => format_general(n),
                    (_, _, Variant::Bstr(s)) => s.clone(),
                    (_, Some(n), _) => format_general(n),
                    _ => variant_text(f),
                };
                rhs_ref = parse_ref(&t, &c.sname);
                rhs_text = Some(t);
            }
        }
        if rel > 3 {
            let adj = self
                .sol_text(c, "solver_adj")
                .and_then(|t| parse_ref(&t, &c.sname));
            if !adj.is_some_and(|r| r.contains(&lhs)) {
                return int(1);
            }
        }
        let num = self.sol_num(c, "solver_num").unwrap_or(0.0) as i64;
        match kind {
            Kind::Add => self.sol_add(c, &lhs, rel, rhs_text, rhs_ref, num),
            Kind::Change => {
                if rel > 3 {
                    return int(3);
                }
                self.sol_change(c, &lhs, rel, rhs_text, rhs_ref, num)
            }
            Kind::Delete => self.sol_delete(c, &lhs, rel, formula, num),
        }
    }

    fn sol_rhs_check(&self, c: &Ctx, lhs: &Ref, text: &str, r: &Option<Ref>) -> Option<String> {
        if let Some(r) = r {
            if r.count() > 1 && r.count() != lhs.count() {
                return None;
            }
        } else if !self.sol_is_numeric(c, text) {
            return None;
        }
        Some(if text.starts_with('=') {
            text.to_string()
        } else {
            format!("={text}")
        })
    }

    fn sol_add(
        &mut self,
        c: &Ctx,
        lhs: &Ref,
        rel: i32,
        text: Option<String>,
        r: Option<Ref>,
        num: i64,
    ) -> Result<Variant, OmError> {
        let n = num + 1;
        let q = self.sol_qual(c, lhs);
        if rel > 3 {
            self.sol_set(c, &format!("solver_lhs{n}"), &q)?;
            self.sol_set(c, &format!("solver_rel{n}"), &format!("={rel}"))?;
            self.sol_set(c, "solver_num", &format!("={n}"))?;
            let msg = match rel {
                4 => "integer",
                5 => "binary",
                _ => "AllDifferent",
            };
            self.sol_set(c, &format!("solver_rhs{n}"), &format!("=\"{msg}\""))?;
            return int(0);
        }
        let text = text.unwrap_or_default();
        let Some(f) = self.sol_rhs_check(c, lhs, &text, &r) else {
            return int(3);
        };
        self.sol_set(c, &format!("solver_lhs{n}"), &q)?;
        self.sol_set(c, &format!("solver_rel{n}"), &format!("={rel}"))?;
        self.sol_set(c, &format!("solver_rhs{n}"), &f)?;
        self.sol_set(c, "solver_num", &format!("={n}"))?;
        int(0)
    }

    fn sol_find(&self, c: &Ctx, lhs: &Ref, rel: i32, num: i64) -> Result<Option<i64>, ()> {
        let mut found = None;
        for i in 1..=num {
            let l = self
                .sol_text(c, &format!("solver_lhs{i}"))
                .and_then(|t| parse_ref(&t, &c.sname));
            let r = self.sol_num(c, &format!("solver_rel{i}"));
            if l.is_some_and(|l| l.addr == lhs.addr) && r == Some(f64::from(rel)) {
                if found.is_some() {
                    return Err(());
                }
                found = Some(i);
            }
        }
        Ok(found)
    }

    fn sol_change(
        &mut self,
        c: &Ctx,
        lhs: &Ref,
        rel: i32,
        text: Option<String>,
        r: Option<Ref>,
        num: i64,
    ) -> Result<Variant, OmError> {
        let text = text.unwrap_or_default();
        let Some(f) = self.sol_rhs_check(c, lhs, &text, &r) else {
            return int(3);
        };
        match self.sol_find(c, lhs, rel, num) {
            Ok(Some(i)) => {
                self.sol_set(c, &format!("solver_rhs{i}"), &f)?;
                int(0)
            }
            _ => int(4),
        }
    }

    fn sol_delete(
        &mut self,
        c: &Ctx,
        lhs: &Ref,
        rel: i32,
        formula: Option<&Variant>,
        num: i64,
    ) -> Result<Variant, OmError> {
        let remember = match formula {
            None => match self.sol_find(c, lhs, rel, num) {
                Ok(Some(i)) => i,
                _ => return int(4),
            },
            Some(f) => {
                let want_right = match f {
                    Variant::Bstr(s) => s.clone(),
                    other => variant_text(other),
                };
                let want = match parse_ref(&want_right, &c.sname) {
                    Some(r) => format!("{}{}{}", lhs.addr, rel, r.addr),
                    None => format!("{}{}{}", lhs.addr, rel, want_right),
                };
                let mut found = 0;
                for i in 1..=num {
                    let Some(right) = self.sol_text(c, &format!("solver_rhs{i}")) else {
                        continue;
                    };
                    let right = if let Ok(n) = right.trim().parse::<f64>() {
                        format_general(n)
                    } else if let Some(r) = parse_ref(&right, &c.sname) {
                        r.addr
                    } else {
                        strip_sheet(&right, &c.sname)
                    };
                    let l = self
                        .sol_text(c, &format!("solver_lhs{i}"))
                        .and_then(|t| parse_ref(&t, &c.sname));
                    let r = self
                        .sol_text(c, &format!("solver_rel{i}"))
                        .unwrap_or_default();
                    if l.is_some_and(|l| format!("{}{}{}", l.addr, r, right) == want) {
                        found = i;
                        break;
                    }
                }
                if found == 0 {
                    return int(4);
                }
                found
            }
        };
        self.sol_set(c, "solver_num", &format!("={}", num - 1))?;
        for part in ["lhs", "rel", "rhs"] {
            if let Some(v) = self.sol_get(c, &format!("solver_{part}{num}")) {
                self.sol_set(c, &format!("solver_{part}{remember}"), &v)?;
            }
        }
        int(0)
    }

    pub(crate) fn sol_reset(&mut self, c: &Ctx) -> Result<Variant, OmError> {
        let eng = match self.sol_num(c, "solver_eng") {
            Some(n) => n,
            None => {
                if self.sol_num(c, "solver_lin") == Some(1.0) {
                    2.0
                } else {
                    1.0
                }
            }
        };
        let neg = match self.sol_get(c, "solver_neg") {
            Some(s) if s != "=1" => 2,
            _ => 1,
        };
        self.sol_del(c, "solver_opt")?;
        self.sol_del(c, "solver_adj")?;
        self.sol_set(c, "solver_typ", "=1")?;
        self.sol_set(c, "solver_val", "=0")?;
        self.sol_set(c, "solver_num", "=0")?;
        self.sol_set(c, "solver_eng", &format!("={}", format_general(eng)))?;
        self.sol_set(c, "solver_neg", &format!("={neg}"))?;
        self.sol_set(c, "solver_ver", "=3")?;
        int(0)
    }

    pub(crate) fn sol_options(&mut self, c: &Ctx, a: &Args) -> Result<Variant, OmError> {
        let arg = |i: usize| a.get(i + 1).filter(|v| !missing(Some(v)));
        let eng = self.sol_num(c, "solver_eng").unwrap_or(1.0);

        let num = |i: usize| -> Result<Option<f64>, OmError> {
            match arg(i) {
                None => Ok(None),
                Some(v) => number(v).map(Some).ok_or_else(OmError::type_mismatch),
            }
        };
        let whole = |x: f64| x.floor() == x;
        let bad = |i: usize, x: f64| match i {
            0 | 1 | 12 | 13 => x < 0.0 || !whole(x),
            2 | 10 => x <= 0.0 || x >= 1.0,
            8 => !(0.0..100.0).contains(&x),
            16 => !(0.0..=1.0).contains(&x),
            _ => false,
        };

        for i in 0..21usize {
            let Some(v) = arg(i) else { continue };
            let code = i as i16 + 1;
            match i {
                3 | 4 | 9 | 11 | 14 | 15 | 19 => match flag(v) {
                    None => return int(code),
                    Some(true) if i == 3 && eng == 3.0 => return int(4),
                    _ => {}
                },
                5..=7 => {
                    let n = match v {
                        Variant::Bstr(s) if s == "TRUE" => 1.0,
                        Variant::Bstr(s) if s == "FALSE" => 2.0,
                        other => number(other).ok_or_else(OmError::type_mismatch)?,
                    };
                    if n != 1.0 && n != 2.0 {
                        return int(code);
                    }
                }
                17 | 18 => {
                    let x = num(i)?.unwrap_or(0.0);
                    if x < 0.0 || !whole(x) {
                        return int(code);
                    }
                }
                20 => {
                    let x = num(i)?.unwrap_or(0.0);
                    if x <= 0.0 || !whole(x) {
                        return int(21);
                    }
                }
                _ => {
                    if bad(i, num(i)?.unwrap_or(0.0)) {
                        return int(code);
                    }
                }
            }
        }
        let limit = |x: f64| {
            if x == 0.0 {
                PLUS_INF.to_string()
            } else {
                format_general(x)
            }
        };
        let put = |this: &mut Self, name: &str, text: String| -> Result<(), OmError> {
            this.sol_set(c, name, &format!("={text}"))
        };
        let on_off = |b: bool, off: &str| if b { "1".to_string() } else { off.to_string() };
        let plain = [
            (2, "solver_pre"),
            (10, "solver_cvg"),
            (12, "solver_ssz"),
            (13, "solver_rsd"),
            (16, "solver_mrt"),
            (20, "solver_mni"),
        ];
        if let Some(x) = num(0)? {
            put(self, "solver_tim", limit(x))?;
        }
        if let Some(x) = num(1)? {
            put(self, "solver_itr", limit(x))?;
        }
        if let Some(v) = arg(2) {
            put(self, "solver_pre", variant_text(v))?;
        }
        let flags = [
            (3, "solver_lin", "2"),
            (4, "solver_sho", "2"),
            (9, "solver_scl", "0"),
        ];
        for (i, n, off) in flags {
            if let Some(b) = arg(i).and_then(flag) {
                put(self, n, on_off(b, off))?;
            }
        }
        for (i, n) in [(5, "solver_est"), (6, "solver_drv"), (7, "solver_nwt")] {
            if let Some(v) = arg(i) {
                let t = match v {
                    Variant::Bstr(s) if s == "TRUE" => "1".to_string(),
                    Variant::Bstr(s) if s == "FALSE" => "2".to_string(),
                    other => variant_text(other),
                };
                put(self, n, t)?;
            }
        }
        if let Some(x) = num(8)? {
            put(self, "solver_tol", format_general(x / 100.0))?;
        }
        if let Some(v) = arg(10) {
            put(self, "solver_cvg", variant_text(v))?;
        }
        let flags2 = [
            (11, "solver_neg"),
            (14, "solver_msl"),
            (15, "solver_rbv"),
            (19, "solver_rlx"),
        ];
        for (i, n) in flags2 {
            if let Some(b) = arg(i).and_then(flag) {
                put(self, n, on_off(b, "2"))?;
            }
        }
        for (i, n) in plain {
            if matches!(i, 2 | 10) {
                continue;
            }
            if let Some(v) = arg(i) {
                put(self, n, variant_text(v))?;
            }
        }
        if let Some(x) = num(17)? {
            put(self, "solver_nod", limit(x))?;
        }
        if let Some(x) = num(18)? {
            put(self, "solver_mip", limit(x))?;
        }
        int(0)
    }

    fn sol_get_proc(&mut self, c: &Ctx, a: &Args) -> Result<Variant, OmError> {
        let Some(t) = a.get(1).filter(|v| !missing(Some(v))) else {
            return Err(OmError::exception(DISP_E_PARAMNOTOPTIONAL.0, None));
        };
        let Some(t) = number(t) else {
            return Err(OmError::type_mismatch());
        };
        let t = t.round() as i32;

        let mut ctx = Ctx {
            book: c.book,
            sheet: c.sheet,
            sname: c.sname.clone(),
        };
        if let Some(Variant::Bstr(s)) = a.get(2).filter(|v| !missing(Some(v))) {
            let wb = &self.book(c.book)?.wb;
            match wb.sheet_by_name(s) {
                Some(sh) => {
                    ctx.sheet = sh.id;
                    ctx.sname = sh.name.clone();
                }
                None => return Err(OmError::bad_index()),
            }
        }
        let c = &ctx;
        let text =
            |this: &Self, n: &str| this.sol_text(c, n).map_or_else(value_error, Variant::Bstr);
        let dbl = |this: &Self, n: &str, d: Variant| this.sol_num(c, n).map_or(d, Variant::R8);
        let boolean = |this: &Self, n: &str, d: bool| {
            Variant::Bool(this.sol_num(c, n).map_or(d, |x| x == 1.0))
        };
        Ok(match t {
            1 => text(self, "solver_opt"),
            2 => self
                .sol_num(c, "solver_typ")
                .map_or_else(value_error, Variant::R8),
            3 => text(self, "solver_val"),
            4 => text(self, "solver_adj"),
            5 => self
                .sol_num(c, "solver_num")
                .map_or_else(value_error, Variant::R8),
            6..=8 => {
                let Some(n) = self.sol_num(c, "solver_num") else {
                    return Ok(value_error());
                };
                let n = n as i64;
                let part = ["lhs", "rel", "rhs"][(t - 6) as usize];
                let mut data = vec![Variant::BstrNull];
                for i in 1..=n {
                    let Some(mut s) = self.sol_text(c, &format!("solver_{part}{i}")) else {
                        return Ok(value_error());
                    };
                    if t == 8 {
                        for m in ["integer", "binary", "AllDifferent"] {
                            if s == format!("\"{m}\"") {
                                s = m.to_string();
                            }
                        }
                    }
                    data.push(Variant::Bstr(s));
                }
                Variant::Array(Box::new(SafeArray {
                    elem: VarType::Bstr,
                    bounds: vec![(0, (n + 1) as u32)],
                    data,
                }))
            }
            9 => dbl(self, "solver_tim", Variant::I2(100)),
            10 => dbl(self, "solver_itr", Variant::I2(100)),
            11 => dbl(self, "solver_pre", Variant::R8(0.000001)),
            12 => dbl(self, "solver_tol", Variant::R8(0.01)),
            13 => boolean(self, "solver_lin", false),
            14 => boolean(self, "solver_sho", false),
            15 => boolean(self, "solver_scl", false),
            16 => dbl(self, "solver_est", Variant::I2(1)),
            17 => dbl(self, "solver_drv", Variant::I2(1)),
            18 => dbl(self, "solver_nwt", Variant::I2(1)),
            19 => dbl(self, "solver_cvg", Variant::R8(0.0001)),
            20 => boolean(self, "solver_neg", false),
            21 => dbl(self, "solver_ssz", Variant::I2(0)),
            22 => dbl(self, "solver_rsd", Variant::I2(0)),
            23 => boolean(self, "solver_msl", true),
            24 => boolean(self, "solver_rbv", true),
            25 => dbl(self, "solver_mrt", Variant::R8(0.075)),
            26 => dbl(self, "solver_nod", Variant::I2(1000)),
            27 => dbl(self, "solver_mip", Variant::I2(1000)),
            28 => boolean(self, "solver_rlx", false),
            29 => dbl(self, "solver_mni", Variant::I2(30)),
            _ => value_error(),
        })
    }
}

#[derive(Clone, Copy, PartialEq)]
enum Kind {
    Add,
    Change,
    Delete,
}

fn variant_text(v: &Variant) -> String {
    match v {
        Variant::Bstr(s) => s.clone(),
        Variant::Bool(b) => if *b { "TRUE" } else { "FALSE" }.to_string(),
        other => number(other).map_or_else(String::new, format_general),
    }
}

pub(crate) fn strip_sheet(text: &str, sheet: &str) -> String {
    let q = format!("'{sheet}'!");
    let p = format!("{sheet}!");
    if text.contains(&q) {
        text.replace(&q, "")
    } else {
        text.replace(&p, "")
    }
}
