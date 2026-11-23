use super::parse::Ph;

#[derive(Clone, Debug, PartialEq)]
pub(super) enum DenOp {
    Run(Ph, usize),

    Literal(String),

    Point,

    Group,

    Text(String),

    Slash,
}

pub(super) struct Start<'a> {
    pub whole: &'a [char],

    pub n_int: i64,
    pub num: &'a [char],
    pub n_num: i64,
    pub den: &'a [char],

    pub spec: i64,

    pub shown: bool,

    pub point: bool,

    pub q: bool,
    pub decimal: char,
    pub group: char,
}

pub(super) struct Layout {
    pub text: String,

    pub overflow: bool,
}

struct Machine {
    out: Vec<char>,
    buf: Vec<char>,
    pos: usize,
    num: Vec<char>,
    n_num: i64,
    den: Vec<char>,
    spec: i64,
    state: u8,
    digits: i64,
    left: i64,
    point: bool,

    q: bool,
    shown: bool,
    mark: usize,
    grouping: bool,
    decimal: char,
    group: char,
    overflow: bool,
}

impl Machine {
    fn separator(&mut self, fill: bool) {
        let c = self.left as i32 as u32;
        if self.grouping && c != 0 && c.is_multiple_of(3) {
            self.out.push(if fill { ' ' } else { self.group });
        }
    }

    fn emit(&mut self, n: i64) {
        for _ in 0..n.max(0) {
            let c = self.buf.get(self.pos).copied();
            if c.is_some() {
                self.pos += 1;
            }
            self.out.push(c.unwrap_or('0'));
            self.left -= 1;
            self.digits -= 1;
            self.separator(false);
        }
    }

    fn aligned(&mut self, kind: Ph, mut n: i64) {
        if self.digits < self.left {
            let take = (self.left - self.digits).min(n);
            n -= take;
            if kind == Ph::Hash {
                self.left -= take;
                if self.left == 0 && self.state == 5 {
                    if self.shown {
                        self.state = 4;
                    } else {
                        self.out.push('0');
                        self.state = 3;
                    }
                }
            } else {
                for _ in 0..take {
                    self.left -= 1;
                    let end = self.left == 0 && self.state == 5;
                    let mut c = if kind == Ph::Zero { '0' } else { ' ' };
                    if end && kind == Ph::Q {
                        if self.shown {
                            self.state = 4;
                        } else {
                            c = '0';
                        }
                    }
                    self.out.push(c);
                    self.separator(kind == Ph::Q);
                }
            }
        } else {
            n += self.digits - self.left;
            self.left = self.digits;
        }
        self.emit(n);
        if self.left == 0 && self.state > 2 {
            if kind != Ph::Q {
                self.q = false;
            }

            if self.state == 5 {
                self.state = 2;
            }
            self.load_numerator();
        }
    }

    fn load_numerator(&mut self) {
        self.mark = self.out.len();
        self.buf = self.num.clone();
        self.pos = 0;
        self.left = self.n_num;
        self.digits = self.num.len() as i64;
        self.grouping = false;
    }

    fn left_aligned(&mut self, kind: Ph, mut n: i64) {
        if self.digits < 0 {
            let z = n.min(-self.digits).min(0x100);
            self.out.extend(std::iter::repeat_n('0', z as usize));
            self.digits += z;
            n -= z;
        }
        let avail = (self.buf.len() - self.pos) as i64;
        let shown = n.min(avail).max(0);
        self.out
            .extend(&self.buf[self.pos..self.pos + shown as usize]);
        match kind {
            Ph::Hash => {}
            Ph::Zero => self
                .out
                .extend(std::iter::repeat_n('0', (n - shown).max(0) as usize)),
            Ph::Q => self
                .out
                .extend(std::iter::repeat_n(' ', (n - shown).max(0) as usize)),
        }
        self.pos += shown as usize;
        self.left -= n;
        self.digits -= n;
    }

    fn blank(&mut self) {
        let from = self.mark.min(self.out.len());
        if self.q {
            for c in &mut self.out[from..] {
                *c = ' ';
            }
        } else {
            self.out.truncate(from);
        }
    }

    fn placeholder(&mut self, kind: Ph, n: usize) {
        let n = n as i64;
        if self.state == 3 {
            self.state = 2;
            if kind == Ph::Zero {
                self.mark = self.mark.saturating_sub(1);
                self.blank();
            }
        } else if self.state == 4 {
            self.blank();
            self.state = 2;
        }

        if self.state == 2 && kind == Ph::Zero && !self.shown {
            self.shown = true;
        }
        if !self.point && !(self.state == 1 && kind == Ph::Q) {
            self.aligned(kind, n);
        } else {
            self.left_aligned(kind, n);
        }
        if self.state == 1 && !self.shown {
            self.blank();
        }
    }
}

pub(super) fn lay_out(ops: &[DenOp], st: &Start) -> Layout {
    let mut m = Machine {
        out: Vec::new(),
        buf: st.whole.to_vec(),
        pos: 0,
        num: st.num.to_vec(),
        n_num: st.n_num,
        den: st.den.to_vec(),
        spec: st.spec,
        state: 5,
        digits: st.whole.len() as i64,
        left: st.n_int,
        point: st.point,
        q: st.q,
        shown: st.shown,
        mark: 0,
        grouping: false,
        decimal: st.decimal,
        group: st.group,
        overflow: false,
    };
    if st.n_int == 0 {
        m.state = 2;
        m.load_numerator();
    }
    for op in ops {
        match op {
            DenOp::Run(kind, n) => {
                if *kind == Ph::Q {
                    m.q = true;
                }
                m.placeholder(*kind, *n);
            }
            DenOp::Literal(s) => {
                let lit: Vec<char> = s.chars().collect();
                for (slot, c) in m.buf.iter_mut().zip(&lit) {
                    *slot = *c;
                }
                m.left = lit.len() as i64;
                let kind = if m.q { Ph::Q } else { Ph::Hash };
                m.placeholder(kind, lit.len());
            }
            DenOp::Point => {
                if m.digits > 0 {
                    if m.state > 2 {
                        m.overflow = true;
                        break;
                    }
                    m.left = m.digits;
                    let n = m.digits;
                    m.emit(n);
                    if m.state == 1 && !m.shown {
                        m.blank();
                    }
                }
                m.out.push(m.decimal);
                m.point = true;
            }
            DenOp::Group => m.grouping = true,
            DenOp::Text(s) => m.out.extend(s.chars()),
            DenOp::Slash => {
                m.out.push('/');
                m.buf = m.den.clone();
                m.pos = 0;
                m.digits = m.buf.len() as i64;
                m.left = m.spec;
                m.state = 1;
            }
        }
    }
    Layout {
        text: m.out.into_iter().collect(),
        overflow: m.overflow,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn chars(s: &str) -> Vec<char> {
        s.chars().collect()
    }

    fn run(digits: &str, spec: i64, ops: &[DenOp], shown: bool, q: bool) -> String {
        let d = chars(digits);
        let st = Start {
            whole: &[],
            n_int: 0,
            num: &[],
            n_num: 0,
            den: &d,
            spec,
            shown,
            point: false,
            q,
            decimal: '.',
            group: ',',
        };
        let mut all = vec![DenOp::Slash];
        all.extend_from_slice(ops);
        let text = lay_out(&all, &st).text;

        text.chars().skip(1).collect()
    }

    fn mixed(whole: &str, n_int: i64, num: &str, n_num: i64, shown: bool, ops: &[DenOp]) -> Layout {
        let (w, n, d) = (chars(whole), chars(num), chars("2"));
        let st = Start {
            whole: &w,
            n_int,
            num: &n,
            n_num,
            den: &d,
            spec: 1,
            shown,
            point: false,
            q: false,
            decimal: '.',
            group: ',',
        };
        lay_out(ops, &st)
    }

    fn q(n: usize) -> DenOp {
        DenOp::Run(Ph::Q, n)
    }
    fn h(n: usize) -> DenOp {
        DenOp::Run(Ph::Hash, n)
    }
    fn z(n: usize) -> DenOp {
        DenOp::Run(Ph::Zero, n)
    }
    fn sp() -> DenOp {
        DenOp::Text(" ".into())
    }

    #[test]
    fn full_denominator_tail() {
        assert_eq!(run("2", 1, &[q(1), sp(), q(1)], true, false), "2  ");

        assert_eq!(run("2", 1, &[q(1), sp(), h(1)], true, false), "2 0");

        assert_eq!(
            run("2", 1, &[q(1), sp(), q(1), sp(), q(1)], true, false),
            "2   0"
        );

        assert_eq!(run("2", 1, &[q(1), sp(), q(1), z(1)], true, false), "2  0");

        assert_eq!(run("2", 1, &[q(1), sp(), h(1), q(1)], true, false), "2 00");
    }

    #[test]
    fn short_denominator_tail() {
        assert_eq!(run("2", 2, &[q(2), sp(), h(1)], true, false), "2  ");
        assert_eq!(run("2", 2, &[q(2), sp(), q(1)], true, false), "2  0");
        assert_eq!(run("2", 2, &[q(2), sp(), z(1)], true, false), "2  0");

        assert_eq!(run("2", 2, &[q(2), sp(), h(2)], true, false), "2  0");
    }

    #[test]
    fn point_flushes_leftover_digits() {
        let ops = [DenOp::Literal("8".into()), sp(), DenOp::Point, z(1)];
        assert_eq!(run("10", 0, &ops, true, false), "80 .0");
    }

    #[test]
    fn zero_fraction_blanking() {
        assert_eq!(run("2", 1, &[h(1), sp(), h(1)], false, false), "");
        assert_eq!(run("2", 1, &[q(1), sp(), h(1)], false, true), "   ");
    }

    #[test]
    fn whole_part_pads_and_ends_on_the_count() {
        let ops = [q(1), h(1), sp(), z(1), DenOp::Slash, z(1)];
        let r = mixed("", 2, "1", 1, true, &ops);
        assert_eq!(r.text, " 1/2");
        assert!(!r.overflow);

        let ops = [h(1), q(1), sp(), z(1), DenOp::Slash, z(1)];
        let r = mixed("", 2, "1", 1, true, &ops);
        assert_eq!(r.text, "  1/2");

        let ops = [h(1), sp(), z(1), DenOp::Slash, z(1)];
        assert_eq!(mixed("123", 1, "1", 1, true, &ops).text, "123 1/2");
    }

    #[test]
    fn whole_part_of_a_zero_fraction() {
        let ops = [z(1), h(1), sp(), z(1), DenOp::Slash, z(1)];
        assert_eq!(mixed("", 2, "0", 1, false, &ops).text, "00/2");

        let ops = [h(1), sp(), q(1), DenOp::Slash, q(1)];
        assert_eq!(mixed("", 1, "0", 1, false, &ops).text, "0    ");
    }

    #[test]
    fn point_after_a_padded_whole_part_never_settles() {
        let ops = [h(1), DenOp::Point, DenOp::Slash, q(1)];
        assert!(mixed("", 1, "1", 0, true, &ops).overflow);

        let r = mixed("2", 1, "1", 0, true, &ops);
        assert!(!r.overflow);
        assert_eq!(r.text, "21./2");
    }

    #[test]
    fn whole_part_grouping() {
        let ops = [
            DenOp::Group,
            h(1),
            h(1),
            h(1),
            h(1),
            sp(),
            z(1),
            DenOp::Slash,
            z(1),
        ];
        assert_eq!(mixed("1234", 4, "1", 1, true, &ops).text, "1,234 1/2");
        let ops = [DenOp::Group, q(4), sp(), z(1), DenOp::Slash, z(1)];
        assert_eq!(mixed("12", 4, "1", 1, true, &ops).text, "   12 1/2");
    }
}
