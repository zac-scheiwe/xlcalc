pub(crate) type R4 = [i64; 4];

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct End {
    pub v: i64,
    pub rel: bool,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Tok {
    pub row: [End; 2],
    pub col: [End; 2],
}

fn isect(a: &R4, b: &R4) -> Option<R4> {
    let r = [
        a[0].max(b[0]),
        a[1].min(b[1]),
        a[2].max(b[2]),
        a[3].min(b[3]),
    ];
    (r[0] <= r[1] && r[2] <= r[3]).then_some(r)
}

fn inside(a: &R4, b: &R4) -> bool {
    a[0] >= b[0] && a[1] <= b[1] && a[2] >= b[2] && a[3] <= b[3]
}

fn span(t: &Tok, h: &R4) -> R4 {
    let at = |e: End, base: i64| e.v + if e.rel { base } else { 0 };
    [
        at(t.row[0], h[0]),
        at(t.row[1], h[1]),
        at(t.col[0], h[2]),
        at(t.col[1], h[3]),
    ]
}

fn inv(t: &Tok, r: &R4, host: &R4, alias: bool) -> Option<R4> {
    let back = |e: End, x: i64, h: i64| if e.rel { x - e.v } else { h };
    let out = [
        back(t.row[0], r[0], host[0]),
        back(t.row[1], r[1], host[1]),
        back(t.col[0], r[2], host[2]),
        back(t.col[1], r[3], host[3]),
    ];
    let chk = if alias { out } else { *r };
    if (!t.row[0].rel && chk[0] > t.row[0].v)
        || (!t.col[0].rel && chk[2] > t.col[0].v)
        || (!t.row[1].rel && chk[1] < t.row[1].v)
        || (!t.col[1].rel && chk[3] < t.col[1].v)
    {
        return None;
    }
    (out[0] <= out[1] && out[2] <= out[3]).then_some(out)
}

fn swap_split(buf: &mut Vec<R4>, g: &mut usize, x: &R4, idx: usize) {
    let rect = buf[idx];
    let live = buf.len() - *g;
    buf[idx] = buf[live - 1];
    *g += 1;
    if rect == *x {
        return;
    }
    let keep = buf.len() - *g;
    let mut ps = Vec::new();
    if x[2] > rect[2] {
        ps.push([rect[0], rect[1], rect[2], x[2] - 1]);
    }
    if x[3] < rect[3] {
        ps.push([rect[0], rect[1], x[3] + 1, rect[3]]);
    }
    if x[0] > rect[0] {
        ps.push([rect[0], x[0] - 1, x[2], x[3]]);
    }
    if x[1] < rect[1] {
        ps.push([x[1] + 1, rect[1], x[2], x[3]]);
    }
    let need = keep + ps.len();
    if need > buf.len() {
        buf.truncate(keep);
        buf.extend(ps);
        *g = 0;
    } else {
        for (i, p) in ps.into_iter().enumerate() {
            buf[keep + i] = p;
        }
        *g = buf.len() - need;
    }
}

fn subtract(cpt: &[R4], sub: &[R4]) -> Option<Vec<R4>> {
    if cpt.is_empty() {
        return None;
    }
    let mut buf = cpt.to_vec();
    let mut g = 0;
    for s in sub.iter().rev() {
        for i in (0..buf.len() - g).rev() {
            if let Some(x) = isect(&buf[i], s) {
                swap_split(&mut buf, &mut g, &x, i);
            }
        }
    }
    let live = buf.len() - g;
    if live == 0 {
        return None;
    }
    buf.truncate(live);
    Some(buf)
}

fn split_by(pl: &mut Vec<Vec<R4>>, idx: usize, x: &R4) -> u8 {
    let ints: Vec<R4> = pl[idx].iter().filter_map(|r| isect(r, x)).collect();
    if ints.is_empty() {
        return 0;
    }
    match subtract(&pl[idx], &[*x]) {
        None => 1,
        Some(rest) => {
            pl[idx] = rest;
            pl.push(ints);
            2
        }
    }
}

fn carve(a: &R4, b: &R4) -> Vec<R4> {
    let mut out = Vec::new();
    if b[0] > a[0] {
        out.push([a[0], b[0] - 1, a[2], a[3]]);
    }
    if b[2] > a[2] {
        out.push([b[0], b[1], a[2], b[2] - 1]);
    }
    if b[3] < a[3] {
        out.push([b[0], b[1], b[3] + 1, a[3]]);
    }
    if b[1] < a[1] {
        out.push([b[1] + 1, a[1], a[2], a[3]]);
    }
    out
}

fn split_rect(
    pl: &mut Vec<Vec<R4>>,
    idx: usize,
    t: &Tok,
    host: &R4,
    im: Option<R4>,
    id: Option<R4>,
) {
    if let Some(a) = im
        .and_then(|im| inv(t, &im, host, false))
        .and_then(|a| isect(&a, host))
    {
        if split_by(pl, idx, &a) == 1 {
            return;
        }
    }
    let Some(id) = id else {
        return;
    };
    if im
        .and_then(|im| isect(&im, &id))
        .is_some_and(|c| inside(&id, &c))
    {
        return;
    }
    let Some(b) = inv(t, &id, host, false).and_then(|b| isect(&b, host)) else {
        return;
    };
    let Some(rest) = subtract(&pl[idx], &[b]) else {
        return;
    };
    pl[idx] = rest;
    let p = match im
        .and_then(|im| isect(&im, &id))
        .and_then(|c| inv(t, &c, host, true))
    {
        Some(c) => carve(&b, &c),
        None => vec![b],
    };
    pl.push(p);
}

fn split_tok(pl: &mut Vec<Vec<R4>>, idx: usize, t: &Tok, moved: &R4, dest: &R4) {
    if pl[idx].is_empty() || (t.row.iter().chain(&t.col).all(|e| !e.rel)) {
        return;
    }
    let mut n = pl[idx].len();
    let (mut start, mut i) = (0usize, 0usize);
    while i < n {
        let rect = pl[idx][i];
        let r = span(t, &rect);
        let im = isect(&r, moved);
        let skip = (rect[0] == rect[1] && rect[2] == rect[3])
            || r[0] > r[1]
            || r[2] > r[3]
            || (im == Some(r) && (i == 0 || i != n - 1));
        if skip {
            i += 1;
            continue;
        }
        if i > start {
            let buf = &mut pl[idx];
            let mut p: Vec<R4> = buf[start..i].to_vec();
            p.extend(im);
            let mut g = i - start;
            buf.copy_within(i..n, start);
            n -= g;
            i = start;
            if let Some(im) = im {
                swap_split(buf, &mut g, &im, start);
            }
            let live = buf.len().saturating_sub(g);
            buf.truncate(live);
            pl.push(p);
        }
        split_rect(pl, idx, t, &rect, im, isect(dest, &r));
        start = i + 1;
        i += 1;
    }
}

pub(crate) fn split_hosed(area: R4, toks: &[Tok], moved: &R4, dest: &R4) -> Vec<Vec<R4>> {
    let mut pl: Vec<Vec<R4>> = vec![vec![area]];

    for region in [moved, dest] {
        for idx in (0..pl.len()).rev() {
            let cut: Vec<R4> = pl[idx].iter().filter_map(|r| isect(r, region)).collect();
            if cut.is_empty() {
                continue;
            }
            let mut rest = Vec::new();
            for r in &pl[idx] {
                rest.extend(subtract_ref(r, region));
            }
            if rest.is_empty() {
                continue;
            }
            pl[idx] = rest;
            pl.push(cut);
        }
    }
    let mut idx = 0;
    while idx < pl.len() {
        for t in toks {
            split_tok(&mut pl, idx, t, moved, dest);
        }
        idx += 1;
    }
    pl
}

fn subtract_ref(r: &R4, h: &R4) -> Vec<R4> {
    let Some(x) = isect(r, h) else {
        return vec![*r];
    };
    let mut out = Vec::new();
    if x[0] > r[0] {
        out.push([r[0], x[0] - 1, r[2], r[3]]);
    }
    if x[1] < r[1] {
        out.push([x[1] + 1, r[1], r[2], r[3]]);
    }
    if x[2] > r[2] {
        out.push([x[0], x[1], r[2], x[2] - 1]);
    }
    if x[3] < r[3] {
        out.push([x[0], x[1], x[3] + 1, r[3]]);
    }
    out
}

pub(crate) fn minimize(v: &mut Vec<R4>) {
    let mut x = v.len() as isize - 1;
    while x >= 0 {
        let mut y = x - 1;
        while y >= 0 {
            if y != x {
                let (a, b) = (v[x as usize], v[y as usize]);
                let rows = a[0] == b[0] && a[1] == b[1] && a[2] <= b[3] + 1 && a[3] >= b[2] - 1;
                let cols = a[2] == b[2] && a[3] == b[3] && a[0] <= b[1] + 1 && a[1] >= b[0] - 1;
                if rows || cols || inside(&a, &b) || inside(&b, &a) {
                    v[x as usize] = [
                        a[0].min(b[0]),
                        a[1].max(b[1]),
                        a[2].min(b[2]),
                        a[3].max(b[3]),
                    ];
                    v.remove(y as usize);
                    if y < x {
                        x -= 1;
                    }
                    y = v.len() as isize;
                }
            }
            y -= 1;
        }
        x -= 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cell(r: i64, rr: bool, c: i64, cr: bool) -> Tok {
        let row = End { v: r, rel: rr };
        let col = End { v: c, rel: cr };
        Tok {
            row: [row, row],
            col: [col, col],
        }
    }

    #[test]
    fn q22_pieces() {
        let toks = [cell(-4, true, 0, false), cell(0, false, 1, true)];
        let moved = [5, 1048575, 0, 0];
        let dest = [3, 1048573, 0, 0];
        let pl = split_hosed([4, 11, 0, 1], &toks, &moved, &dest);
        let want: Vec<Vec<R4>> = vec![
            vec![[8, 11, 0, 0], [5, 6, 1, 1]],
            vec![[5, 6, 0, 0]],
            vec![[4, 4, 0, 0]],
            vec![[5, 6, 0, 0]],
            vec![[9, 11, 1, 1]],
            vec![[7, 8, 1, 1]],
            vec![[9, 11, 0, 0]],
            vec![[7, 8, 0, 0]],
            vec![[4, 4, 1, 1]],
            vec![[7, 7, 0, 0]],
            vec![[9, 11, 1, 1]],
        ];
        assert_eq!(pl, want);
    }

    #[test]
    fn cut_14_pieces() {
        let toks = [cell(0, true, 16383, true), cell(-1, true, 16383, true)];
        let toks = [toks[0], toks[1], toks[0], toks[1]];
        let pl = split_hosed([1, 8, 1, 1], &toks, &[2, 5, 0, 1], &[6, 9, 3, 4]);
        let want: Vec<Vec<R4>> = vec![vec![[6, 8, 1, 1]], vec![[2, 5, 1, 1]], vec![[1, 1, 1, 1]]];
        assert_eq!(pl, want);

        let pl = split_hosed(
            [2, 7, 0, 0],
            &[cell(-2, true, 1, true)],
            &[2, 5, 0, 1],
            &[6, 9, 3, 4],
        );
        let want: Vec<Vec<R4>> = vec![vec![[6, 7, 0, 0]], vec![[2, 3, 0, 0]], vec![[4, 5, 0, 0]]];
        assert_eq!(pl, want);
    }

    #[test]
    fn minimize_q22() {
        let mut v = vec![
            [6, 9, 0, 0],
            [5, 6, 1, 1],
            [7, 8, 1, 1],
            [5, 6, 0, 0],
            [5, 5, 0, 0],
        ];
        minimize(&mut v);
        assert_eq!(v, vec![[6, 9, 0, 0], [7, 8, 1, 1], [5, 6, 0, 1]]);
    }
}
