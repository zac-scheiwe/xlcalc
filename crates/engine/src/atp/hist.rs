use super::{flag, input, output, Arg, AtpError, Out, Run, Src};
use crate::address::Rect;
use crate::style::Edge;
use crate::value::Value;
use crate::Workbook;

fn numbers(wb: &Workbook, s: &Src, skip_first: bool) -> Vec<f64> {
    let mut v = Vec::new();
    for r in u32::from(skip_first)..s.rows() {
        for c in 0..s.cols() {
            if let Value::Number(x) = wb.value(s.at(r, c)) {
                v.push(x);
            }
        }
    }
    v
}

pub(super) fn histogram(wb: &Workbook, args: &[Arg]) -> Result<Run, AtpError> {
    let src = input(args, 0)?;
    let pareto = flag(args, 3);
    let cumulative = flag(args, 4);
    let labels = flag(args, 6);
    let data = numbers(wb, &src, labels);
    let mut bin_label = "Bin".to_string();
    let mut bins: Vec<f64> = match args.get(2) {
        Some(Arg::Range(sheet, rect)) => {
            let b = Src {
                sheet: *sheet,
                rect: *rect,
            };
            if labels {
                if let Value::Text(t) = wb.value(b.at(0, 0)) {
                    bin_label = t.to_string();
                }
            }
            numbers(wb, &b, labels)
        }
        _ => {
            let min = data.iter().copied().fold(f64::INFINITY, f64::min);
            let max = data.iter().copied().fold(f64::NEG_INFINITY, f64::max);
            let k = (data.len() as f64).sqrt().floor() as usize;
            let step = (max - min) / k as f64;
            (0..k).map(|i| min + i as f64 * step).collect()
        }
    };
    bins.sort_by(f64::total_cmp);
    let mut freq = vec![0u32; bins.len() + 1];
    for x in &data {
        let i = bins.iter().position(|b| x <= b).unwrap_or(bins.len());
        freq[i] += 1;
    }
    let rows = bins.len() + 1;
    let total = data.len() as f64;
    let mut o = Out::default();
    let mut width = 2 + usize::from(cumulative);
    let block = |o: &mut Out, col: u32, order: &[usize]| {
        let heads = ["", "Frequency", "Cumulative %"];
        o.text(0, col, &bin_label);
        for (j, h) in heads.iter().enumerate().skip(1).take(width - 1) {
            o.text(0, col + j as u32, h);
        }
        let mut cum = 0u32;
        for (r, &i) in order.iter().enumerate() {
            let row = r as u32 + 1;
            if i < bins.len() {
                o.num(row, col, bins[i]);
            } else {
                o.text(row, col, "More");
            }
            o.num(row, col + 1, f64::from(freq[i]));
            cum += freq[i];
            if cumulative {
                o.num(row, col + 2, f64::from(cum) / total);
                o.format(row, col + 2, "0.00%");
            }
        }
    };
    let natural: Vec<usize> = (0..rows).collect();
    block(&mut o, 0, &natural);
    if pareto {
        let mut order = natural;
        order.sort_by(|a, b| freq[*b].cmp(&freq[*a]));
        block(&mut o, width as u32, &order);
        width *= 2;
    }
    let last = Rect::new(rows as u32, 0, rows as u32, width as u32 - 1);
    o.header(Rect::new(0, 0, 0, width as u32 - 1), false);
    o.line(Rect::new(0, 0, 0, width as u32 - 1), Edge::Top, true);
    o.line(Rect::new(0, 0, 0, width as u32 - 1), Edge::Bottom, false);
    o.line(last, Edge::Bottom, true);
    output(args, 1, o)
}
