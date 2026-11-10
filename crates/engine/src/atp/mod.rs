use crate::address::{col_to_letters, CellAddr, Rect, SheetId};
use crate::style::{Border, BorderLine, CellColor, Edge, HAlign, Style};
use crate::value::{Value, XlError};
use crate::workbook::notice::Command;
use crate::Workbook;

mod anova;
mod descr;
mod fourier;
mod hist;
mod pair;
mod random;
mod rank;
mod regress;
mod sample;
mod series;
mod ttest;

#[derive(Clone, Debug)]
pub enum Arg {
    Missing,
    Range(SheetId, Rect),
    Num(f64),
    Bool(bool),
    Text(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AtpError {
    Message(String),
}

#[derive(Clone, Debug)]
pub enum Cell {
    Val(Value),

    Formula(String),
}

#[derive(Clone, Debug)]
pub struct Item {
    pub row: u32,
    pub col: u32,
    pub cell: Cell,
}

#[derive(Clone, Debug)]
pub struct Fmt {
    pub row: u32,
    pub col: u32,
    pub code: &'static str,
}

#[derive(Clone, Copy, Debug)]
pub enum Mark {
    Header(Rect, bool),

    Navy(Rect, Edge),

    Label(Rect),

    Line(Rect, Edge, bool),
}

#[derive(Clone, Debug, Default)]
pub struct Out {
    pub items: Vec<Item>,
    pub marks: Vec<Mark>,
    pub formats: Vec<Fmt>,
}

impl Out {
    pub fn num(&mut self, row: u32, col: u32, n: f64) {
        self.val(row, col, Value::Number(n));
    }

    pub fn text(&mut self, row: u32, col: u32, s: &str) {
        self.val(row, col, Value::text(s));
    }

    pub fn na(&mut self, row: u32, col: u32) {
        self.val(row, col, Value::Error(XlError::NA));
    }

    pub fn val(&mut self, row: u32, col: u32, v: Value) {
        self.items.push(Item {
            row,
            col,
            cell: Cell::Val(v),
        });
    }

    pub fn format(&mut self, row: u32, col: u32, code: &'static str) {
        self.formats.push(Fmt { row, col, code });
    }

    pub fn header(&mut self, r: Rect, across: bool) {
        self.marks.push(Mark::Header(r, across));
    }

    pub fn navy(&mut self, r: Rect, edge: Edge) {
        self.marks.push(Mark::Navy(r, edge));
    }

    pub fn label(&mut self, r: Rect) {
        self.marks.push(Mark::Label(r));
    }

    pub fn line(&mut self, r: Rect, edge: Edge, medium: bool) {
        self.marks.push(Mark::Line(r, edge, medium));
    }

    pub fn formula(&mut self, row: u32, col: u32, f: String) {
        self.items.push(Item {
            row,
            col,
            cell: Cell::Formula(f),
        });
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Src {
    pub sheet: SheetId,
    pub rect: Rect,
}

impl Src {
    pub fn rows(&self) -> u32 {
        self.rect.r1 - self.rect.r0 + 1
    }

    pub fn cols(&self) -> u32 {
        self.rect.c1 - self.rect.c0 + 1
    }

    pub fn at(&self, row: u32, col: u32) -> CellAddr {
        CellAddr::new(self.sheet, self.rect.r0 + row, self.rect.c0 + col)
    }
}

pub(crate) fn a1_col(col: u32, r0: u32, r1: u32) -> String {
    let c = col_to_letters(col);
    if r0 == r1 {
        format!("{c}{}", r0 + 1)
    } else {
        format!("{c}{}:{c}{}", r0 + 1, r1 + 1)
    }
}

pub struct Run {
    pub sheet: SheetId,
    pub row: u32,
    pub col: u32,
    pub out: Out,
}

fn arg(args: &[Arg], i: usize) -> &Arg {
    args.get(i).unwrap_or(&Arg::Missing)
}

pub(crate) fn flag(args: &[Arg], i: usize) -> bool {
    match arg(args, i) {
        Arg::Bool(b) => *b,
        Arg::Num(n) => *n != 0.0,
        _ => false,
    }
}

pub(crate) fn number(args: &[Arg], i: usize) -> Option<f64> {
    match arg(args, i) {
        Arg::Num(n) => Some(*n),
        Arg::Bool(b) => Some(f64::from(u8::from(*b))),
        _ => None,
    }
}

pub(crate) fn input(args: &[Arg], i: usize) -> Result<Src, AtpError> {
    match arg(args, i) {
        Arg::Range(sheet, rect) => Ok(Src {
            sheet: *sheet,
            rect: *rect,
        }),
        _ => Err(AtpError::Message("Input range is not valid.".into())),
    }
}

pub(crate) fn output(args: &[Arg], i: usize, out: Out) -> Result<Run, AtpError> {
    match arg(args, i) {
        Arg::Range(sheet, rect) => Ok(Run {
            sheet: *sheet,
            row: rect.r0,
            col: rect.c0,
            out,
        }),
        _ => Err(AtpError::Message("Output range is not valid.".into())),
    }
}

impl Workbook {
    pub fn atp_run(&self, name: &str, args: &[Arg]) -> Option<Result<Run, AtpError>> {
        Some(match name.to_ascii_lowercase().as_str() {
            "anova1" => anova::anova1(self, args),
            "anova2" => anova::anova2(self, args),
            "anova3" => anova::anova3(self, args),
            "correl" | "mcorrel" => pair::correl(self, args),
            "covar" | "mcovar" => pair::covar(self, args),
            "ttestm" => ttest::ttest_equal(self, args),
            "pttestv" => ttest::ttest_unequal(self, args),
            "pttestm" => ttest::ttest_paired(self, args),
            "ztestm" => ttest::ztest(self, args),
            "ftestv" => ttest::ftest(self, args),
            "rankperc" => rank::rankperc(self, args),
            "random" => random::random(self, args),
            "sample" => sample::sample(self, args),
            "fourier" => fourier::fourier(self, args),
            "moveavg" => series::moveavg(args),
            "expon" => series::expon(args),
            "descr" => descr::descr(self, args),
            "histogram" => hist::histogram(self, args),
            "regress" => regress::regress(self, args),
            _ => return None,
        })
    }

    pub fn atp_apply(&mut self, run: Run) {
        let Run {
            sheet,
            row,
            col,
            out,
        } = run;
        self.command(Command::Atp, |wb| {
            for it in &out.items {
                let addr = CellAddr::new(sheet, row + it.row, col + it.col);
                match &it.cell {
                    Cell::Val(v) => {
                        wb.set_value(addr, v.clone());
                    }
                    Cell::Formula(f) => {
                        let _ = wb.set_input(addr, &format!("={f}"));
                    }
                }
            }
            for f in &out.formats {
                wb.set_number_format(sheet, Rect::cell(row + f.row, col + f.col), f.code);
            }
            wb.atp_marks(sheet, row, col, &out.marks);
        });
    }
}

pub(crate) fn abs_ref(wb: &Workbook, s: &Src) -> String {
    let name = wb
        .sheet(s.sheet)
        .map(|x| x.name.clone())
        .unwrap_or_default();
    let cell = |r: u32, c: u32| format!("${}${}", col_to_letters(c), r + 1);
    let body = if s.rect.is_cell() {
        cell(s.rect.r0, s.rect.c0)
    } else {
        format!(
            "{}:{}",
            cell(s.rect.r0, s.rect.c0),
            cell(s.rect.r1, s.rect.c1)
        )
    };
    format!("{}!{body}", crate::parser::print::sheet_name_text(&name))
}

pub(crate) fn eval(wb: &Workbook, sheet: SheetId, text: &str) -> Value {
    let host = CellAddr::new(sheet, 0, 0);
    wb.cfv_eval(text, host, host)
}

impl Workbook {
    fn atp_marks(&mut self, sheet: SheetId, row: u32, col: u32, marks: &[Mark]) {
        let abs = |r: Rect| Rect::new(row + r.r0, col + r.c0, row + r.r1, col + r.c1);
        for m in marks {
            match *m {
                Mark::Header(r, across) => {
                    self.set_style_with(sheet, abs(r), &|s: &Style| {
                        let mut n = s.clone();
                        n.font.italic = true;
                        n.alignment.horizontal = if across {
                            HAlign::CenterAcrossSelection
                        } else {
                            HAlign::Center
                        };
                        n
                    });
                }
                Mark::Label(r) => {
                    self.set_style_with(sheet, abs(r), &|s: &Style| {
                        let mut n = s.clone();
                        n.font.italic = true;
                        n.alignment.horizontal = HAlign::Right;
                        n.font.size_twips = 200;
                        n
                    });
                }
                Mark::Line(r, edge, medium) => {
                    self.atp_line(sheet, abs(r), edge, medium, CellColor::Auto);
                }
                Mark::Navy(r, edge) => {
                    self.atp_line(sheet, abs(r), edge, true, CellColor::Rgb(0x80));
                }
            }
        }
    }

    fn atp_line(&mut self, sheet: SheetId, rect: Rect, edge: Edge, medium: bool, color: CellColor) {
        let line = Border {
            line: if medium {
                BorderLine::Medium
            } else {
                BorderLine::Thin
            },
            color,
        };
        let side = match edge {
            Edge::Top => Rect::new(rect.r0, rect.c0, rect.r0, rect.c1),
            Edge::Bottom => Rect::new(rect.r1, rect.c0, rect.r1, rect.c1),
            Edge::Left => Rect::new(rect.r0, rect.c0, rect.r1, rect.c0),
            Edge::Right => Rect::new(rect.r0, rect.c1, rect.r1, rect.c1),
        };
        self.set_style_with(sheet, side, &|s: &Style| {
            let mut n = s.clone();
            *n.borders.edge_mut(edge) = line;
            n
        });
        let near = match edge {
            Edge::Top => side
                .r0
                .checked_sub(1)
                .map(|r| Rect::new(r, side.c0, r, side.c1)),
            Edge::Bottom => Some(Rect::new(side.r0 + 1, side.c0, side.r0 + 1, side.c1)),
            Edge::Left => side
                .c0
                .checked_sub(1)
                .map(|c| Rect::new(side.r0, c, side.r1, c)),
            Edge::Right => Some(Rect::new(side.r0, side.c0 + 1, side.r1, side.c0 + 1)),
        };
        if let Some(n) = near {
            let opp = edge.opposite();
            self.set_style_with(sheet, n, &|s: &Style| {
                let mut n = s.clone();
                *n.borders.edge_mut(opp) = line;
                n
            });
        }
    }
}

pub(crate) fn text(args: &[Arg], i: usize) -> Option<&str> {
    match arg(args, i) {
        Arg::Text(s) => Some(s),
        _ => None,
    }
}

pub(crate) fn func(wb: &Workbook, sheet: SheetId, name: &str, args: &[f64]) -> Value {
    let a: Vec<crate::host::Arg> = args
        .iter()
        .map(|x| crate::host::Arg::Value(Value::Number(*x)))
        .collect();
    wb.call_function(CellAddr::new(sheet, 0, 0), name, &a)
        .unwrap_or(Value::Error(XlError::NAME))
}
