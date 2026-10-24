#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DD(pub f64, pub f64);

#[inline]
fn two_sum(a: f64, b: f64) -> DD {
    let s = a + b;
    let bb = s - a;
    DD(s, (a - (s - bb)) + (b - bb))
}

#[inline]
fn quick_two_sum(a: f64, b: f64) -> DD {
    let s = a + b;
    DD(s, b - (s - a))
}

#[inline]
fn two_prod(a: f64, b: f64) -> DD {
    let p = a * b;
    DD(p, a.mul_add(b, -p))
}

pub const LN2: DD = DD(0.6931471805599453, 2.3190468138462996e-17);

pub const LN_SQRT_2PI: DD = DD(0.9189385332046728, -3.8782941580672414e-17);

impl DD {
    #[inline]
    pub fn new(x: f64) -> DD {
        DD(x, 0.0)
    }

    #[inline]
    pub fn hi(self) -> f64 {
        self.0 + self.1
    }

    pub fn add(self, o: DD) -> DD {
        let s = two_sum(self.0, o.0);
        let t = two_sum(self.1, o.1);
        let s = quick_two_sum(s.0, s.1 + t.0);
        quick_two_sum(s.0, s.1 + t.1)
    }

    pub fn add_f(self, o: f64) -> DD {
        let s = two_sum(self.0, o);
        quick_two_sum(s.0, s.1 + self.1)
    }

    pub fn round64(self) -> DD {
        let h = self.0;
        if h == 0.0 || !h.is_finite() || self.1 == 0.0 {
            return self;
        }
        let e = ((h.abs().to_bits() >> 52) & 0x7ff) as i32;
        if e < 64 {
            return self;
        }

        let ulp = f64::from_bits(((e - 63) as u64) << 52);
        let q = (self.1 / ulp).round_ties_even() * ulp;
        quick_two_sum(h, q)
    }

    pub fn neg(self) -> DD {
        DD(-self.0, -self.1)
    }

    pub fn sub(self, o: DD) -> DD {
        self.add(o.neg())
    }

    pub fn mul(self, o: DD) -> DD {
        let p = two_prod(self.0, o.0);
        let e = p.1 + (self.0 * o.1 + self.1 * o.0);
        quick_two_sum(p.0, e)
    }

    pub fn mul_f(self, o: f64) -> DD {
        let p = two_prod(self.0, o);
        quick_two_sum(p.0, p.1 + self.1 * o)
    }

    pub fn div(self, o: DD) -> DD {
        let q1 = self.0 / o.0;
        let r = self.sub(o.mul_f(q1));
        let q2 = r.0 / o.0;
        let r = r.sub(o.mul_f(q2));
        let q3 = r.0 / o.0;
        quick_two_sum(q1, q2).add_f(q3)
    }

    pub fn recip(self) -> DD {
        DD::new(1.0).div(self)
    }

    pub fn exp(self) -> DD {
        let x = self.hi();
        if x > 709.8 {
            return DD(f64::INFINITY, 0.0);
        }
        if x < -745.2 {
            return DD(0.0, 0.0);
        }
        let k = (x / LN2.0).round();
        let r = self.sub(LN2.mul_f(k));

        let r = DD(r.0 / 1024.0, r.1 / 1024.0);
        let mut term = r;
        let mut sum = r;
        let mut i = 2.0;
        while i < 14.0 {
            term = term.mul(r).div(DD::new(i));
            sum = sum.add(term);
            if term.0.abs() < 1e-36 {
                break;
            }
            i += 1.0;
        }

        let mut s = sum;
        for _ in 0..10 {
            s = s.mul_f(2.0).add(s.mul(s));
        }
        let v = s.add_f(1.0);

        let k = k as i32;
        let (k1, k2) = (k / 2, k - k / 2);
        let (s1, s2) = (2f64.powi(k1), 2f64.powi(k2));
        DD(v.0 * s1 * s2, v.1 * s1 * s2)
    }

    pub fn ln(self) -> DD {
        let h = self.hi();
        let e = if h > 0.0 && h.is_finite() {
            h.log2().floor()
        } else {
            0.0
        };
        if e.abs() > 500.0 {
            let k = e as i32;
            let (k1, k2) = (k / 2, k - k / 2);
            let (s1, s2) = (2f64.powi(-k1), 2f64.powi(-k2));
            let m = DD(self.0 * s1 * s2, self.1 * s1 * s2);
            return m.ln().add(LN2.mul_f(e));
        }
        let y = DD::new(self.hi().ln());

        let y = y.add(self.mul(y.neg().exp()).add_f(-1.0));
        y.add(self.mul(y.neg().exp()).add_f(-1.0))
    }
}
