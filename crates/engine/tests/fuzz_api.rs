use std::panic::{catch_unwind, AssertUnwindSafe};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;
use std::time::{Duration, Instant};

use xlcalc::{
    CalcMode, CellAddr, DateUnit, FillType, PasteOp, PasteSpecial, PasteWhat, Rect, SeriesSpec,
    SeriesType, SheetId, SortKey, SortSpec, Value, Workbook, XlError,
};

const MAX_ROW: u32 = 1_048_575;
const MAX_COL: u32 = 16_383;

struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
    fn below(&mut self, n: u32) -> u32 {
        (self.next() % n.max(1) as u64) as u32
    }
    fn chance(&mut self, pct: u32) -> bool {
        self.below(100) < pct
    }
    fn pick<'a, T>(&mut self, xs: &'a [T]) -> &'a T {
        &xs[self.below(xs.len() as u32) as usize]
    }
}

const COL_NAMES: [&str; 8] = ["A", "B", "C", "D", "E", "Z", "AA", "XFD"];

struct Gen {
    wild: bool,
    rng: Rng,
    funcs: Vec<&'static str>,
    sheet_names: Vec<String>,
    table_names: Vec<String>,
}

impl Gen {
    fn row(&mut self) -> u32 {
        match self.rng.below(20) {
            0..=12 => self.rng.below(25),
            13..=15 => self.rng.below(3000),
            16..=17 => MAX_ROW - self.rng.below(4),
            18 => self.rng.below(MAX_ROW + 1),
            _ => u32::MAX - self.rng.below(3),
        }
    }
    fn col(&mut self) -> u32 {
        match self.rng.below(20) {
            0..=12 => self.rng.below(10),
            13..=15 => self.rng.below(200),
            16..=17 => MAX_COL - self.rng.below(4),
            18 => self.rng.below(MAX_COL + 1),
            _ => u32::MAX - self.rng.below(3),
        }
    }
    fn small_row(&mut self) -> u32 {
        self.rng.below(25)
    }
    fn small_col(&mut self) -> u32 {
        self.rng.below(10)
    }
    fn sheet(&mut self, wb: &Workbook) -> SheetId {
        if self.rng.chance(3) {
            return SheetId(900 + self.rng.below(5));
        }
        let n = wb.sheet_count() as u32;
        wb.sheet_at(self.rng.below(n) as usize).id
    }
    fn rect(&mut self) -> Rect {
        if !self.wild {
            return match self.rng.below(20) {
                0..=13 => self.small_rect(),
                14..=16 => {
                    let (r, c) = (self.row(), self.col());
                    Rect::new(r, c, r, c)
                }
                _ => {
                    let (r, c) = (MAX_ROW - self.rng.below(4), MAX_COL - self.rng.below(4));
                    Rect::new(r, c, MAX_ROW.min(r + 3), MAX_COL.min(c + 3))
                }
            };
        }
        match self.rng.below(24) {
            0 => Rect::new(0, 0, MAX_ROW, MAX_COL),
            1 => {
                let c = self.col().min(MAX_COL);
                Rect::new(0, c, MAX_ROW, c)
            }
            2 => {
                let r = self.row().min(MAX_ROW);
                Rect::new(r, 0, r, MAX_COL)
            }
            3..=14 => {
                let (r, c) = (self.small_row(), self.small_col());
                Rect::new(r, c, r + self.rng.below(6), c + self.rng.below(5))
            }
            15..=17 => {
                let (r, c) = (self.small_row(), self.small_col());
                Rect::new(r, c, r, c)
            }
            _ => Rect::new(self.row(), self.col(), self.row(), self.col()),
        }
    }
    fn small_rect(&mut self) -> Rect {
        let (r, c) = (self.small_row(), self.small_col());
        Rect::new(r, c, r + self.rng.below(8), c + self.rng.below(5))
    }
    fn addr(&mut self, wb: &Workbook) -> CellAddr {
        let s = self.sheet(wb);
        CellAddr::new(s, self.row(), self.col())
    }
    fn small_addr(&mut self, wb: &Workbook) -> CellAddr {
        let s = self.sheet(wb);
        CellAddr::new(s, self.small_row(), self.small_col())
    }
    fn count(&mut self) -> u32 {
        if !self.wild {
            return match self.rng.below(30) {
                0 => 0,
                1 => MAX_ROW,
                2 => u32::MAX,
                3 => 1 + self.rng.below(300),
                _ => 1 + self.rng.below(3),
            };
        }
        match self.rng.below(12) {
            0..=7 => 1 + self.rng.below(3),
            8 => 0,
            9 => 1 + self.rng.below(2000),
            10 => MAX_ROW,
            _ => u32::MAX - self.rng.below(2),
        }
    }

    fn number(&mut self) -> String {
        match self.rng.below(14) {
            0 => "0".into(),
            1 => "1E308".into(),
            2 => "1E-308".into(),
            3 => "-1".into(),
            4 => "2147483648".into(),
            5 => "4294967296".into(),
            6 => "1E+19".into(),
            7 => "0.5".into(),
            8 => "-0".into(),
            9 => "1048577".into(),
            10 => "16385".into(),
            _ => format!("{}", self.rng.below(100)),
        }
    }

    fn reference(&mut self) -> String {
        let sheet = if self.rng.chance(15) {
            let n = self.rng.pick(&self.sheet_names.clone()).clone();
            if n.contains(' ') || self.rng.chance(30) {
                format!("'{n}'!")
            } else {
                format!("{n}!")
            }
        } else {
            String::new()
        };
        let d1 = if self.rng.chance(20) { "$" } else { "" };
        let d2 = if self.rng.chance(20) { "$" } else { "" };
        let col = if self.rng.chance(80) {
            COL_NAMES[self.rng.below(5) as usize].to_string()
        } else {
            self.rng.pick(&COL_NAMES).to_string()
        };
        let row = match self.rng.below(10) {
            0..=6 => 1 + self.rng.below(20),
            7 => 1_048_576,
            8 => 1_048_577,
            _ => 0,
        };
        let a = format!("{sheet}{d1}{col}{d2}{row}");
        match self.rng.below(10) {
            0..=4 => a,
            5 | 6 if self.wild || (1..=30).contains(&row) => format!(
                "{a}:{}{}",
                self.rng.pick(&COL_NAMES[..5]),
                1 + self.rng.below(30)
            ),
            7 if self.rng.chance(if self.wild { 35 } else { 0 }) => format!(
                "{sheet}{}:{}",
                self.rng.pick(&COL_NAMES),
                self.rng.pick(&COL_NAMES)
            ),
            8 if self.rng.chance(if self.wild { 35 } else { 0 }) => format!(
                "{sheet}{r}:{r2}",
                r = 1 + self.rng.below(5),
                r2 = 1 + self.rng.below(2000)
            ),
            _ => format!("{a}#"),
        }
    }

    fn table_ref(&mut self) -> String {
        let t = self.rng.pick(&self.table_names.clone()).clone();
        let col = ["Col1", "Col2", "Qty", "x y", "Nope"];
        let c = *self.rng.pick(&col);
        match self.rng.below(8) {
            0 => format!("{t}[{c}]"),
            1 => format!("{t}[@{c}]"),
            2 => format!("{t}[#All]"),
            3 => format!("{t}[[#Headers],[{c}]]"),
            4 => format!("{t}[#Totals]"),
            5 => format!("{t}[@[{c}]]"),
            6 => t,
            _ => format!("{t}[[{c}]:[Col2]]"),
        }
    }

    fn expr(&mut self, depth: u32) -> String {
        if depth == 0 || self.rng.chance(20) {
            return match self.rng.below(12) {
                0..=3 => self.number(),
                4 => format!(
                    "\"{}\"",
                    self.rng
                        .pick(&["", "a", "abc", "1", "x y", "2024-01-31", "#N/A", "é€🎉"])
                ),
                5 => self
                    .rng
                    .pick(&[
                        "TRUE",
                        "FALSE",
                        "#N/A",
                        "#REF!",
                        "#DIV/0!",
                        "{1,2;3,4}",
                        "{\"a\",1}",
                    ])
                    .to_string(),
                6 => self.table_ref(),
                7 => self
                    .rng
                    .pick(&["nm", "nm2", "Sheet1!nm", "_xlnm.Print_Area", "LAMBDA"])
                    .to_string(),
                _ => self.reference(),
            };
        }
        match self.rng.below(12) {
            0..=5 => {
                let f = *self.rng.pick(&self.funcs.clone());
                let n = match self.rng.below(6) {
                    0 => 0,
                    1 => 1,
                    2 => 2,
                    3 => 3,
                    4 => 4,
                    _ => self.rng.below(9),
                };
                let args: Vec<String> = (0..n)
                    .map(|_| {
                        if self.rng.chance(6) {
                            String::new()
                        } else {
                            self.expr(depth - 1)
                        }
                    })
                    .collect();
                format!("{f}({})", args.join(","))
            }
            6..=8 => {
                let op = *self.rng.pick(&[
                    "+", "-", "*", "/", "^", "&", "=", "<>", "<", ">=", ":", " ", ",", "%",
                ]);
                let a = self.expr(depth - 1);
                let b = self.expr(depth - 1);
                format!("{a}{op}{b}")
            }
            9 => {
                let pre = *self.rng.pick(&["-", "+", "@", "--", "!"]);
                format!("{pre}{}", self.expr(depth - 1))
            }
            10 => format!("({})", self.expr(depth - 1)),
            _ => {
                let a = self.expr(depth - 1);
                format!("LET(x,{a},y,x+1,x*y)")
            }
        }
    }

    fn formula(&mut self) -> String {
        match self.rng.below(40) {
            0 => {
                let n = *self.rng.pick(&[100usize, 300, 1000, 5000, 20000]);
                let kind = self.rng.below(5);
                match kind {
                    0 => format!("={}1{}", "(".repeat(n), ")".repeat(n)),
                    1 => format!("={}1", "-".repeat(n)),
                    2 => format!("=1{}", "+1".repeat(n)),
                    3 => format!("={}1{}", "IF(TRUE,".repeat(n), ",0)".repeat(n)),
                    _ => format!("={}A1", "SUM(".repeat(n)) + &")".repeat(n),
                }
            }
            1 => {
                let toks = [
                    "=", "(", ")", "{", "}", "[", "]", ",", ";", ":", "!", "'", "\"", "#", "A1",
                    "$", "+", "-", "%", "SUM", "1.", ".5", "1e", "E5", "R1C1", "R[1]C", "@", " ",
                    "&", "<", ">", "\u{0}", "é", "🎉",
                ];
                let n = 1 + self.rng.below(14);
                let mut s = String::from("=");
                for _ in 0..n {
                    s.push_str(self.rng.pick(&toks));
                }
                s
            }
            2 => {
                let n = self.rng.below(40);
                (0..n)
                    .map(|_| char::from_u32(self.rng.below(0x3000)).unwrap_or('x'))
                    .collect()
            }
            3 => {
                let a = format!("={}", self.reference());
                let b = self
                    .rng
                    .pick(&["", "(", ")", "+", "*", "&\"", ",", "!"])
                    .to_string();
                a + &b
            }
            4 => {
                let n = *self.rng.pick(&[255usize, 256, 8191, 8192, 40000]);
                format!("=\"{}\"", "a".repeat(n))
            }
            _ => {
                let d = 1 + self.rng.below(4);
                format!("={}", self.expr(d))
            }
        }
    }

    fn input(&mut self) -> String {
        match self.rng.below(12) {
            0 => String::new(),
            1 => self.number(),
            2 => self
                .rng
                .pick(&[
                    "abc", "1/2/2024", "12:30", "5%", "$1,000", "1E5", "TRUE", "#N/A", "'text",
                    "(5)", "1e400", "0x10", "-", "é",
                ])
                .to_string(),
            3 => format!("{}", self.rng.below(1000) as f64 / 7.0),
            _ => self.formula(),
        }
    }

    fn value(&mut self) -> Value {
        match self.rng.below(7) {
            0 => Value::Blank,
            1 => Value::Bool(self.rng.chance(50)),
            2 => Value::Error(*self.rng.pick(&[
                XlError::Div0,
                XlError::NA,
                XlError::Ref,
                XlError::Num,
                XlError::Value,
                XlError::Spill,
            ])),
            3 => Value::from("text"),
            4 => Value::Number(*self.rng.pick(&[
                0.0,
                -0.0,
                1e308,
                f64::MAX,
                f64::MIN_POSITIVE,
                f64::NAN,
                f64::INFINITY,
                -1.5,
            ])),
            _ => Value::Number(self.rng.below(100) as f64),
        }
    }

    fn sheet_name(&mut self) -> String {
        match self.rng.below(10) {
            0 => String::new(),
            1 => "Sheet1".into(),
            2 => "a'b".into(),
            3 => "x".repeat(32),
            4 => "bad[name]".into(),
            5 => "My Sheet".into(),
            6 => "History".into(),
            7 => "A1".into(),
            _ => format!("S{}", self.rng.below(6)),
        }
    }

    fn name(&mut self) -> String {
        self.rng
            .pick(&[
                "nm",
                "nm2",
                "Sheet1!nm",
                "A1",
                "R1C1",
                "x y",
                "",
                "r",
                "c",
                "_x",
                "TRUE",
                "NAME_WITH_LONG_____",
                "1a",
                "é",
                "Sheet2!nm",
            ])
            .to_string()
    }

    fn name_target(&mut self) -> String {
        match self.rng.below(6) {
            0 => self.reference(),
            1 => format!("={}", self.reference()),
            2 => self.formula(),
            3 => "=LAMBDA(x,x+1)".into(),
            4 => "=nm".into(),
            _ => format!("={}", self.expr(2)),
        }
    }

    fn table_name(&mut self) -> String {
        match self.rng.below(9) {
            0 => String::new(),
            1 => "1T".into(),
            2 => "A1".into(),
            3 => "T bad".into(),
            _ => format!("T{}", self.rng.below(4)),
        }
    }
}

fn describe<T: std::fmt::Debug>(name: &str, args: T) -> String {
    format!("{name}{args:?}")
}

fn one_op(g: &mut Gen, wb: &mut Workbook, desc: &mut String) {
    g.sheet_names = wb.sheets().map(|s| s.name.clone()).collect();
    let k = g.rng.below(100);
    match k {
        0..=24 => {
            let a = if g.rng.chance(85) {
                g.small_addr(wb)
            } else {
                g.addr(wb)
            };
            let t = g.input();
            note(desc, format!("set_input({a:?}, {:?})", trunc(&t)));
            let _ = wb.set_input(a, &t);
        }
        25..=27 => {
            let a = g.small_addr(wb);
            let t = g.formula();
            note(desc, format!("set_formula({a:?}, {:?})", trunc(&t)));
            let _ = wb.set_formula(a, &t);
        }
        28..=30 => {
            let a = if g.rng.chance(80) {
                g.small_addr(wb)
            } else {
                g.addr(wb)
            };
            let v = g.value();
            note(desc, format!("set_value({a:?}, {v:?})"));
            wb.set_value(a, v);
        }
        31..=33 => {
            let s = g.sheet(wb);
            let r = g.rect();
            if g.rng.chance(50) {
                note(desc, format!("clear_range({s:?}, {r:?})"));
                let _ = wb.clear_range(s, r);
            } else {
                let a = g.addr(wb);
                note(desc, format!("clear({a:?})"));
                wb.clear(a);
            }
        }
        34..=41 => {
            let s = g.sheet(wb);
            let at = if g.rng.chance(70) {
                g.small_row()
            } else {
                g.row()
            };
            let n = g.count();
            let which = g.rng.below(4);
            note(desc, format!("rowcol_edit{which}({s:?}, {at}, {n})"));
            let _ = match which {
                0 => wb.insert_rows(s, at, n),
                1 => wb.delete_rows(s, at, n),
                2 => wb.insert_cols(s, at, n),
                _ => wb.delete_cols(s, at, n),
            };
        }
        42..=45 => {
            let s = g.sheet(wb);
            let r = if g.rng.chance(80) {
                g.small_rect()
            } else {
                g.rect()
            };
            let down = g.rng.chance(50);
            let ins = g.rng.chance(50);
            note(desc, format!("cells_edit(ins={ins}, {s:?}, {r:?}, {down})"));
            let _ = if ins {
                wb.insert_cells(s, r, down)
            } else {
                wb.delete_cells(s, r, down)
            };
        }
        46..=52 => {
            let s = g.sheet(wb);
            let r = if g.rng.chance(80) {
                g.small_rect()
            } else {
                g.rect()
            };
            let d = if g.rng.chance(80) {
                g.small_addr(wb)
            } else {
                g.addr(wb)
            };
            let cut = g.rng.chance(40);
            note(
                desc,
                format!("copy_or_move(cut={cut}, {s:?}, {r:?}, {d:?})"),
            );
            let _ = if cut {
                wb.move_range(s, r, d)
            } else {
                wb.copy_range(s, r, d)
            };
        }
        53..=56 => {
            let s = g.sheet(wb);
            let src = g.small_rect();
            let dst = if g.rng.chance(85) {
                g.small_rect()
            } else {
                g.rect()
            };
            let ty = *g.rng.pick(&[
                FillType::Default,
                FillType::Copy,
                FillType::Series,
                FillType::Formats,
                FillType::Values,
                FillType::Days,
                FillType::Weekdays,
                FillType::Months,
                FillType::Years,
                FillType::Linear,
                FillType::Growth,
            ]);
            if g.rng.chance(50) {
                note(desc, format!("fill({s:?}, {src:?}, {dst:?})"));
                let _ = wb.fill(s, src, dst);
            } else {
                note(desc, format!("autofill({s:?}, {src:?}, {dst:?}, {ty:?})"));
                let _ = wb.autofill(s, src, dst, ty);
            }
        }
        57 => {
            let s = g.sheet(wb);
            let r = g.small_rect();
            let spec = SeriesSpec {
                rows: *g.rng.pick(&[None, Some(true), Some(false)]),
                ty: *g.rng.pick(&[
                    None,
                    Some(SeriesType::Linear),
                    Some(SeriesType::Growth),
                    Some(SeriesType::Date),
                    Some(SeriesType::AutoFill),
                ]),
                unit: *g.rng.pick(&[
                    None,
                    Some(DateUnit::Day),
                    Some(DateUnit::Weekday),
                    Some(DateUnit::Month),
                    Some(DateUnit::Year),
                ]),
                step: *g.rng.pick(&[
                    None,
                    Some(0.0),
                    Some(1.0),
                    Some(-2.5),
                    Some(1e308),
                    Some(f64::NAN),
                ]),
                stop: *g
                    .rng
                    .pick(&[None, Some(10.0), Some(-1e308), Some(f64::INFINITY)]),
                trend: g.rng.chance(30),
            };
            note(desc, format!("data_series({s:?}, {r:?}, {spec:?})"));
            let _ = wb.data_series(s, r, &spec);
        }
        58..=61 => {
            let s = g.sheet(wb);
            let r = if g.rng.chance(85) {
                g.small_rect()
            } else {
                g.rect()
            };
            let nk = g.rng.below(4);
            let keys = (0..nk)
                .map(|_| SortKey {
                    index: g.rng.below(6),
                    descending: g.rng.chance(50),
                })
                .collect();
            let spec = SortSpec {
                keys,
                header: g.rng.chance(40),
                by_columns: g.rng.chance(25),
                match_case: g.rng.chance(30),
            };
            note(desc, format!("sort_range({s:?}, {r:?}, {spec:?})"));
            let _ = wb.sort_range(s, r, &spec);
        }
        62..=66 => {
            let ss = g.sheet(wb);
            let ds = g.sheet(wb);
            let src = if g.rng.chance(85) {
                g.small_rect()
            } else {
                g.rect()
            };
            let dst = if g.rng.chance(85) {
                g.small_rect()
            } else {
                g.rect()
            };
            let spec = PasteSpecial {
                what: *g.rng.pick(&[
                    PasteWhat::All,
                    PasteWhat::Values,
                    PasteWhat::Formulas,
                    PasteWhat::Formats,
                ]),
                op: *g.rng.pick(&[
                    PasteOp::None,
                    PasteOp::Add,
                    PasteOp::Sub,
                    PasteOp::Mul,
                    PasteOp::Div,
                ]),
                skip_blanks: g.rng.chance(30),
                transpose: g.rng.chance(40),
            };
            note(
                desc,
                format!("paste_special({ss:?}, {src:?}, {ds:?}, {dst:?}, {spec:?})"),
            );
            let _ = wb.paste_special(ss, src, ds, dst, &spec);
        }
        67..=69 => {
            let s = g.sheet(wb);
            let r = if g.rng.chance(85) {
                g.small_rect()
            } else {
                g.rect()
            };
            let across = g.rng.chance(40);
            if g.rng.chance(70) {
                note(desc, format!("merge_cells({s:?}, {r:?}, {across})"));
                let _ = wb.merge_cells(s, r, across);
            } else {
                note(desc, format!("unmerge_cells({s:?}, {r:?})"));
                wb.unmerge_cells(s, r);
            }
        }
        70..=75 => {
            let tn = g.table_name();
            g.table_names = vec!["T0".into(), "T1".into(), "T2".into(), "T3".into()];
            match g.rng.below(9) {
                0..=2 => {
                    let s = g.sheet(wb);
                    let r = if g.rng.chance(85) {
                        g.small_rect()
                    } else {
                        g.rect()
                    };
                    note(desc, format!("add_table({tn:?}, {s:?}, {r:?})"));
                    let _ = wb.add_table(&tn, s, r);
                }
                3 => {
                    let show = g.rng.chance(50);
                    note(desc, format!("set_table_totals({tn:?}, {show})"));
                    let _ = wb.set_table_totals(&tn, show);
                }
                4 => {
                    let r = g.small_rect();
                    note(desc, format!("resize_table({tn:?}, {r:?})"));
                    let _ = wb.resize_table(&tn, r);
                }
                5 => {
                    let n2 = g.table_name();
                    note(desc, format!("rename_table({tn:?}, {n2:?})"));
                    let _ = wb.rename_table(&tn, &n2);
                }
                6 => {
                    note(desc, format!("delete_or_unlist_table({tn:?})"));
                    let _ = if g.rng.chance(50) {
                        wb.delete_table(&tn)
                    } else {
                        wb.unlist_table(&tn)
                    };
                }
                7 => {
                    let show = g.rng.chance(50);
                    note(desc, format!("set_table_headers({tn:?}, {show})"));
                    let _ = wb.set_table_headers(&tn, show);
                }
                _ => {
                    let a = g.small_addr(wb);
                    note(desc, format!("table_text_at({a:?})"));
                    let _ = wb.table_at(a);
                }
            }
        }
        76..=79 => {
            let n = g.name();
            if g.rng.chance(75) {
                let t = g.name_target();
                let scope = if g.rng.chance(30) {
                    Some(g.sheet(wb))
                } else {
                    None
                };
                note(
                    desc,
                    format!("define_name({n:?}, {:?}, {scope:?})", trunc(&t)),
                );
                let _ = wb.define_name(&n, &t, scope);
            } else {
                note(desc, format!("delete_name({n:?})"));
                let _ = wb.delete_name(&n, None);
            }
        }
        80..=83 => {
            let s = g.sheet(wb);
            let n = g.sheet_name();
            match g.rng.below(5) {
                0 => {
                    note(desc, format!("add_sheet({n:?})"));
                    let _ = wb.add_sheet(&n);
                }
                1 => {
                    note(desc, format!("rename_sheet({s:?}, {n:?})"));
                    let _ = wb.rename_sheet(s, &n);
                }
                2 => {
                    let b = if g.rng.chance(70) {
                        Some(g.sheet(wb))
                    } else {
                        None
                    };
                    note(desc, format!("move_sheet({s:?}, {b:?})"));
                    let _ = wb.move_sheet(s, b);
                }
                3 => {
                    let b = if g.rng.chance(70) {
                        Some(g.sheet(wb))
                    } else {
                        None
                    };
                    note(desc, format!("copy_sheet({s:?}, {b:?})"));
                    if wb.sheet_count() < 12 {
                        let _ = wb.copy_sheet(s, b);
                    }
                }
                _ => {
                    note(desc, format!("delete_sheet({s:?})"));
                    let _ = wb.delete_sheet(s);
                }
            }
        }
        84..=87 => {
            let s = g.sheet(wb);
            let r = if g.rng.chance(85) {
                g.small_rect()
            } else {
                g.rect()
            };
            match g.rng.below(4) {
                0 => {
                    let ri = if g.rng.chance(60) {
                        Some(g.small_addr(wb))
                    } else {
                        None
                    };
                    let ci = if g.rng.chance(60) {
                        Some(g.small_addr(wb))
                    } else {
                        None
                    };
                    note(
                        desc,
                        format!("add_data_table({s:?}, {r:?}, {ri:?}, {ci:?})"),
                    );
                    let _ = wb.add_data_table(s, r, ri, ci);
                }
                1 => {
                    let t = g.small_addr(wb);
                    let c = g.small_addr(wb);
                    let goal = *g.rng.pick(&[0.0, 10.0, -3.5, 1e300, f64::NAN]);
                    note(desc, format!("goal_seek({t:?}, {goal}, {c:?})"));
                    let _ = wb.goal_seek(t, goal, c);
                }
                2 => {
                    let t = g.formula();
                    note(
                        desc,
                        format!("set_array_formula({s:?}, {r:?}, {:?})", trunc(&t)),
                    );
                    let _ = wb.set_array_formula(s, r, &t);
                }
                _ => {
                    let a = g.small_addr(wb);
                    let t = g.input();
                    note(desc, format!("set_legacy_input({a:?}, {:?})", trunc(&t)));
                    let _ = wb.set_legacy_input(a, &t);
                }
            }
        }
        88..=93 => match g.rng.below(9) {
            0 => {
                let m = *g.rng.pick(&[CalcMode::Automatic, CalcMode::Manual]);
                note(desc, format!("set_mode({m:?})"));
                wb.set_mode(m);
            }
            1 => {
                note(desc, "set_semiautomatic");
                wb.set_semiautomatic();
            }
            2 => {
                let on = g.rng.chance(60);
                let mi = *g.rng.pick(&[None, Some(0), Some(1), Some(3), Some(100)]);
                let mc = *g.rng.pick(&[
                    None,
                    Some(0.0),
                    Some(0.001),
                    Some(-1.0),
                    Some(f64::NAN),
                    Some(1e308),
                ]);
                note(desc, format!("set_iteration({on}, {mi:?}, {mc:?})"));
                wb.set_iteration(on, mi, mc);
            }
            3 => {
                note(desc, "recalc");
                wb.recalc();
            }
            4 => {
                note(desc, "full_recalc");
                wb.full_recalc();
            }
            5 => {
                note(desc, "rebuild");
                wb.rebuild();
            }
            6 => {
                let on = g.rng.chance(50);
                note(desc, format!("set_precision_as_displayed({on})"));
                wb.set_precision_as_displayed(on);
            }
            7 => {
                let s = g.sheet(wb);
                let r = if g.rng.chance(85) {
                    g.small_rect()
                } else {
                    g.rect()
                };
                let f = *g.rng.pick(&[
                    "0.00",
                    "General",
                    "0%",
                    "@",
                    "yyyy-mm-dd",
                    "[h]:mm:ss",
                    "#,##0;[Red]-#,##0",
                    "\"x",
                    "[",
                    "0.0E+00",
                    "# ?/?",
                    "",
                ]);
                note(desc, format!("set_number_format({s:?}, {r:?}, {f:?})"));
                wb.set_number_format(s, r, f);
            }
            _ => {
                wb.seed_random(g.rng.next());
                note(desc, "seed_random");
            }
        },
        _ => {
            let a = g.addr(wb);
            let s = g.sheet(wb);
            note(desc, format!("reads({a:?})"));
            let v = wb.value(a);
            let _ = wb.display_text(a);
            let _ = wb.formula_text(a);
            let _ = wb.dependents(a);
            let _ = wb.precedents(a);
            let _ = wb.used_range(s);
            let _ = wb.spill_anchor(a);
            let _ = format!("{v:?}");
        }
    }
}

fn trunc(s: &str) -> String {
    if s.chars().count() > 160 {
        let head: String = s.chars().take(100).collect();
        format!("{head}...({} chars)", s.chars().count())
    } else {
        s.to_string()
    }
}

static LAST_PANIC: Mutex<String> = Mutex::new(String::new());
static OPS_DONE: AtomicU64 = AtomicU64::new(0);
static SEED_NOW: AtomicU64 = AtomicU64::new(0);
static OP_NOW: AtomicU64 = AtomicU64::new(0);

fn fresh(g: &mut Gen) -> Workbook {
    let mut wb = Workbook::new();
    let s1 = wb.sheet_at(0).id;
    let s2 = wb.add_sheet("Sheet2").unwrap();
    let _ = wb.add_sheet("Sheet 3");

    for r in 0..12u32 {
        for c in 0..5u32 {
            let t = match (r + c) % 4 {
                0 => format!("{}", r * 10 + c),
                1 => format!("=A{}+{c}", r + 1),
                2 => format!("s{r}"),
                _ => format!("={}", g.expr(2)),
            };
            let _ = wb.set_input(CellAddr::new(if c == 4 { s2 } else { s1 }, r, c), &t);
        }
    }
    let _ = wb.define_name("nm", "=Sheet1!$A$1:$A$5", None);
    wb
}

fn run_seed(seed: u64, wild: bool, ops: usize, failures: &mut Vec<(u64, usize, String, String)>) {
    let funcs = xlcalc::function_names();
    let mut g = Gen {
        wild,
        rng: Rng(seed.wrapping_mul(0x2545_F491_4F6C_DD1D) ^ 0xDEAD_BEEF),
        funcs,
        sheet_names: vec!["Sheet1".into()],
        table_names: vec!["T0".into()],
    };
    let mut wb = fresh(&mut g);
    let mut desc = String::new();
    for i in 0..ops {
        desc.clear();
        if std::env::var("FUZZ_DUMP_AT")
            .ok()
            .and_then(|v| v.parse::<usize>().ok())
            == Some(i)
        {
            for fid in wb.formula_ids().take(400) {
                let fc = wb.formula(fid);
                eprintln!(
                    "   F {:?} {} spill={:?}",
                    fc.addr,
                    trunc(&fc.text),
                    fc.spill
                );
            }
            eprintln!("   SETTINGS {:?}", wb.settings());
            for sh in wb.sheets() {
                for (r, c, _) in sh.entries() {
                    eprintln!(
                        "   CELL {:?} {r} {c} {:?} {:?}",
                        sh.id,
                        wb.value(CellAddr::new(sh.id, r, c)),
                        wb.formula_text(CellAddr::new(sh.id, r, c))
                    );
                }
                eprintln!(
                    "   SHEET {:?} {} used={:?} cells={} spills={:?}",
                    sh.id,
                    sh.name,
                    wb.used_range(sh.id),
                    sh.cell_count(),
                    sh.used_bounds()
                );
            }
        }
        OP_START_MS.store(now_ms(), Ordering::Relaxed);
        SEED_NOW.store(seed, Ordering::Relaxed);
        OP_NOW.store(i as u64, Ordering::Relaxed);
        let t = Instant::now();
        let r = catch_unwind(AssertUnwindSafe(|| one_op(&mut g, &mut wb, &mut desc)));
        OPS_DONE.fetch_add(1, Ordering::Relaxed);
        if let Err(e) = r {
            let msg = e
                .downcast_ref::<&str>()
                .map(|s| s.to_string())
                .or_else(|| e.downcast_ref::<String>().cloned())
                .unwrap_or_default();
            let loc = LAST_PANIC.lock().unwrap().clone();
            failures.push((seed, i, desc.clone(), format!("{msg} @ {loc}")));

            wb = fresh(&mut g);
        }
        if !xlcalc::testing::ops_idle(&wb) {
            failures.push((seed, i, desc.clone(), "operation state left set".into()));
            wb = fresh(&mut g);
        }
        let took = t.elapsed();
        if std::env::var_os("FUZZ_PROFILE").is_some() {
            let kind = desc.split('(').next().unwrap_or("").to_string();
            let mut p = PROFILE.lock().unwrap();
            let e = p
                .entry(kind)
                .or_insert((0u32, Duration::ZERO, Duration::ZERO, String::new()));
            e.0 += 1;
            e.1 += took;
            if took > e.2 {
                e.2 = took;
                e.3 = trunc(&desc);
            }
        }
        if took > Duration::from_millis(300) && std::env::var("FUZZ_DUMP").is_ok() {
            eprintln!("SLOWOP {took:.1?} {desc}");
            for fid in wb.formula_ids().take(400) {
                let fc = wb.formula(fid);
                eprintln!("   F {:?} {}", fc.addr, trunc(&fc.text));
            }
        }
        if took > Duration::from_secs(20) {
            failures.push((seed, i, desc.clone(), format!("SLOW: {took:.1?}")));
        }
    }
}

fn fuzz(start: u64, seeds: u64, wild: bool, ops: usize) -> Vec<(u64, usize, String, String)> {
    std::panic::set_hook(Box::new(|info| {
        if std::thread::current().name() != Some("fuzz") {
            eprintln!("{info}");
        }
        if let Some(l) = info.location() {
            *LAST_PANIC.lock().unwrap() = format!("{}:{}", l.file(), l.line());
        }
    }));
    let mut failures = Vec::new();
    for seed in start..start + seeds {
        run_seed(seed, wild, ops, &mut failures);
    }
    for (k, (n, total, max, worst)) in PROFILE.lock().unwrap().iter() {
        eprintln!("PROFILE {k:<24} n={n:<6} total={total:>9.2?} max={max:>9.2?} {worst}");
    }
    failures
}

fn on_small_stack(
    limit: Duration,
    f: impl FnOnce() -> Vec<(u64, usize, String, String)> + Send + 'static,
) -> Vec<(u64, usize, String, String)> {
    let h = std::thread::Builder::new()
        .name("fuzz".into())
        .stack_size(1 << 20)
        .spawn(f)
        .unwrap();
    while !h.is_finished() {
        let stuck = now_ms().saturating_sub(OP_START_MS.load(Ordering::Relaxed));
        if stuck > limit.as_millis() as u64 {
            panic!(
                "one fuzz operation ran over {limit:?} (hang?) at seed {} op {} ({} ops done): {}",
                SEED_NOW.load(Ordering::Relaxed),
                OP_NOW.load(Ordering::Relaxed),
                OPS_DONE.load(Ordering::Relaxed),
                CURRENT.lock().unwrap()
            );
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    h.join().unwrap()
}

fn report(failures: &[(u64, usize, String, String)]) {
    for (seed, i, d, m) in failures {
        eprintln!("FAIL seed={seed} op={i}: {d}\n    {m}");
    }
    assert!(failures.is_empty(), "{} fuzz failures", failures.len());
}

#[test]
fn random_api_calls_never_panic() {
    let failures = on_small_stack(Duration::from_secs(20), || fuzz(0, 12, false, 150));
    report(&failures);
}

#[test]
#[ignore]
fn random_api_calls_long() {
    let env = |k: &str, d: u64| {
        std::env::var(k)
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(d)
    };
    let wild = env("FUZZ_WILD", 1) != 0;
    let (start, seeds, ops) = (
        env("FUZZ_START", 1000),
        env("FUZZ_SEEDS", 100),
        env("FUZZ_OPS", 2000),
    );
    let failures = on_small_stack(Duration::from_secs(60), move || {
        fuzz(start, seeds, wild, ops as usize)
    });
    eprintln!("ops done: {}", OPS_DONE.load(Ordering::Relaxed));
    report(&failures);
}

#[allow(dead_code)]
fn _unused() -> String {
    describe("x", ())
}

static PROFILE: Mutex<std::collections::BTreeMap<String, (u32, Duration, Duration, String)>> =
    Mutex::new(std::collections::BTreeMap::new());
static CURRENT: Mutex<String> = Mutex::new(String::new());
static OP_START_MS: AtomicU64 = AtomicU64::new(0);
static EPOCH: std::sync::OnceLock<Instant> = std::sync::OnceLock::new();

fn now_ms() -> u64 {
    EPOCH.get_or_init(Instant::now).elapsed().as_millis() as u64
}

fn note(desc: &mut String, s: impl Into<String>) {
    *desc = s.into();
    if std::env::var_os("FUZZ_TRACE").is_some() {
        eprintln!("OP {desc}");
    }
    if let Ok(mut c) = CURRENT.lock() {
        c.clone_from(desc);
    }
}
