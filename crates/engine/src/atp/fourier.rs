use super::{flag, input, output, Arg, AtpError, Cell, Item, Out, Run};
use crate::value::Value;
use crate::Workbook;

const TWO_PI_15: f64 = f64::from_bits(0x401921FB54442D1C);

fn fft(re: &mut [f64], im: &mut [f64], inverse: bool) {
    let n = re.len();
    let mut d = vec![0.0; 2 * n + 1];
    for i in 0..n {
        d[2 * i + 1] = re[i];
        d[2 * i + 2] = im[i];
    }
    let nn = 2 * n;
    let mut j = 1usize;
    let mut i = 1usize;
    while i < nn {
        if j > i {
            d.swap(j, i);
            d.swap(j + 1, i + 1);
        }
        let mut m = nn >> 1;
        while m >= 2 && j > m {
            j -= m;
            m >>= 1;
        }
        j += m;
        i += 2;
    }
    let mut mmax = 2usize;
    while nn > mmax {
        let istep = mmax << 1;
        let signed = if inverse { mmax as f64 } else { -(mmax as f64) };
        let theta = TWO_PI_15 / signed;
        let wtemp = (theta * 0.5).sin();
        let wpr = -2.0 * wtemp * wtemp;
        let wpi = theta.sin();
        let (mut wr, mut wi) = (1.0f64, 0.0f64);
        let mut m = 1;
        while m < mmax {
            let mut i = m;
            while i <= nn {
                let j = i + mmax;
                let tempr = wr * d[j] - wi * d[j + 1];
                let tempi = wi * d[j] + wr * d[j + 1];
                d[j] = d[i] - tempr;
                d[j + 1] = d[i + 1] - tempi;
                d[i] += tempr;
                d[i + 1] += tempi;
                i += istep;
            }
            let wold = wr;
            wr += wr * wpr - wi * wpi;
            wi += wi * wpr + wpi * wold;
            m += 2;
        }
        mmax = istep;
    }
    for i in 0..n {
        re[i] = d[2 * i + 1];
        im[i] = d[2 * i + 2];
    }
}

fn g15(x: f64) -> String {
    crate::format_general(format!("{x:.14e}").parse::<f64>().unwrap_or(x))
}

fn complex(re: f64, im: f64) -> String {
    let (re, im) = (g15(re), g15(im));
    if im == "0" {
        re
    } else if im.starts_with('-') {
        format!("{re}{im}i")
    } else {
        format!("{re}+{im}i")
    }
}

pub(super) fn fourier(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let src = input(args, 0)?;
    let inverse = flag(args, 2);
    let lab = u32::from(flag(args, 3));
    let n = (src.rows() - lab) as usize;
    if !n.is_power_of_two() {
        return Err(AtpError::Message(
            "The number of data points must be a power of 2.".into(),
        ));
    }
    let mut re = Vec::new();
    for i in 0..n {
        match wb.value(src.at(lab + i as u32, 0)) {
            Value::Number(x) => re.push(x),
            _ => re.push(0.0),
        }
    }
    let mut im = vec![0.0; n];
    fft(&mut re, &mut im, inverse);
    let mut o = Out::default();
    for i in 0..n {
        let (r, m) = if inverse {
            (re[i] / n as f64, im[i] / n as f64)
        } else {
            (re[i], im[i])
        };
        o.items.push(Item {
            row: i as u32,
            col: 0,
            cell: Cell::Val(Value::text(complex(r, m).as_str())),
        });
    }
    output(args, 1, o)
}
