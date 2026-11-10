use serde_json::{json, Value as J};
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
use std::time::Instant;

pub const MANIFEST: &str = "minimal.json";
const ID_MASK: u64 = (1 << 48) - 1;

pub fn hash_bytes(b: &[u8]) -> u64 {
    let mut h: u64 = 0x9E37_79B9_7F4A_7C15 ^ b.len() as u64;
    let (words, rest) = b.as_chunks::<8>();
    for c in words {
        let w = u64::from_le_bytes(*c);
        h = (h ^ w).wrapping_mul(0xFF51_AFD7_ED55_8CCD).rotate_left(29);
    }
    for &x in rest {
        h = (h ^ x as u64).wrapping_mul(0x0000_0100_0000_01B3);
    }
    h ^= h >> 32;
    h = h.wrapping_mul(0xC4CE_B9FE_1A85_EC53);
    h ^ (h >> 29)
}

pub fn item_id(it: &J) -> u64 {
    let mut s = String::new();
    s.push_str(it["formula"].as_str().unwrap_or(""));
    s.push('\0');
    s.push_str(it["host"].as_str().unwrap_or(""));
    s.push('\0');
    s.push_str(&it["value"].to_string());
    s.push('\0');
    s.push_str(&it["invalid"].to_string());
    hash_bytes(s.as_bytes()) & ID_MASK
}

pub fn occ_id(id: u64, occ: u32) -> u64 {
    if occ == 0 {
        id
    } else {
        hash_bytes(&[id.to_le_bytes().as_slice(), &occ.to_le_bytes()].concat()) & ID_MASK
    }
}

fn hex(id: u64) -> String {
    format!("{id:012x}")
}

pub enum SuiteSel {
    All,

    Skip,

    Items(HashSet<u64>),
}

pub struct Entry {
    pub golden: u64,
    pub keep: Keep,
}

pub enum Keep {
    All,
    None,
    Items(Vec<u64>),
}

pub struct Manifest {
    pub suites: BTreeMap<String, Entry>,
}

pub fn manifest_path() -> PathBuf {
    crate::conf_dir().join(MANIFEST)
}

impl Manifest {
    pub fn load() -> Option<Manifest> {
        let text = fs::read_to_string(manifest_path()).ok()?;
        let j: J = serde_json::from_str(&text).ok()?;
        let mut suites = BTreeMap::new();
        for (name, e) in j["suites"].as_object()? {
            let golden = u64::from_str_radix(e["golden"].as_str()?, 16).ok()?;
            let keep = match e["keep"].as_str()? {
                "all" => Keep::All,
                "none" => Keep::None,
                _ => Keep::Items(
                    e["ids"]
                        .as_str()
                        .unwrap_or("")
                        .split_whitespace()
                        .filter_map(|h| u64::from_str_radix(h, 16).ok())
                        .collect(),
                ),
            };
            suites.insert(name.clone(), Entry { golden, keep });
        }
        Some(Manifest { suites })
    }

    pub fn save(&self, meta: J) {
        let mut suites = serde_json::Map::new();
        for (name, e) in &self.suites {
            let mut o = serde_json::Map::new();
            o.insert("golden".into(), json!(hex(e.golden)));
            match &e.keep {
                Keep::All => o.insert("keep".into(), json!("all")),
                Keep::None => o.insert("keep".into(), json!("none")),
                Keep::Items(ids) => {
                    let mut ids = ids.clone();
                    ids.sort_unstable();
                    let s: Vec<String> = ids.iter().map(|&i| hex(i)).collect();
                    o.insert("keep".into(), json!("items"));
                    o.insert("ids".into(), json!(s.join(" ")))
                }
            };
            suites.insert(name.clone(), J::Object(o));
        }
        let doc = json!({ "format": 1, "meta": meta, "suites": suites });
        let mut text = serde_json::to_string_pretty(&doc).unwrap();
        text.push('\n');
        fs::write(manifest_path(), text).unwrap();
    }

    pub fn select(&self, suite: &str, golden: u64) -> SuiteSel {
        match self.suites.get(suite) {
            Some(e) if e.golden == golden => match &e.keep {
                Keep::All => SuiteSel::All,
                Keep::None => SuiteSel::Skip,
                Keep::Items(v) => SuiteSel::Items(v.iter().copied().collect()),
            },
            _ => SuiteSel::All,
        }
    }
}

#[derive(Default)]
pub struct OccCounter(HashMap<u64, u32>);

impl OccCounter {
    pub fn next(&mut self, it: &J) -> u64 {
        let id = item_id(it);
        let n = self.0.entry(id).or_insert(0);
        let r = occ_id(id, *n);
        *n += 1;
        r
    }
}

#[cfg(feature = "covsig")]
mod cov {
    extern "C" {
        fn __llvm_profile_begin_counters() -> *mut u64;
        fn __llvm_profile_end_counters() -> *mut u64;
        fn __llvm_profile_begin_data() -> *const u8;
        fn __llvm_profile_end_data() -> *const u8;
    }

    pub struct Layout {
        pub start: usize,
        pub n: usize,

        pub funcs: Vec<(u32, u32)>,
    }

    pub fn layout() -> Layout {
        unsafe {
            let b = __llvm_profile_begin_counters() as usize;
            let e = __llvm_profile_end_counters() as usize;
            let start = (b + 7) & !7;
            let n = (e - start) / 8;
            let d0 = __llvm_profile_begin_data();
            let nb = __llvm_profile_end_data() as usize - d0 as usize;

            let mut funcs = Vec::new();
            let mut sum = 0usize;
            for r in 0..nb / 72 {
                let rec = d0.add(r * 72);
                let rel = (rec.add(16) as *const i64).read_unaligned();
                let cnt = (rec.add(56) as *const u32).read_unaligned();
                let first = (rec as usize as i64 + rel) as usize;
                assert!(
                    first >= start && first < start + n * 8,
                    "profile data layout"
                );
                funcs.push((((first - start) / 8) as u32, cnt));
                sum += cnt as usize;
            }
            assert_eq!(sum, n, "profile data layout: counters {sum} vs {n}");
            funcs.sort_unstable();
            Layout { start, n, funcs }
        }
    }

    pub fn zero(l: &Layout) {
        unsafe { std::ptr::write_bytes(l.start as *mut u64, 0, l.n) }
    }

    pub fn nonzero(l: &Layout, out: &mut Vec<u32>) {
        out.clear();

        let s = unsafe { std::slice::from_raw_parts(l.start as *const u64, l.n) };
        for (c, chunk) in s.chunks(256).enumerate() {
            if chunk.iter().any(|&x| x != 0) {
                for (i, &x) in chunk.iter().enumerate() {
                    if x != 0 {
                        out.push((c * 256 + i) as u32);
                    }
                }
            }
        }
    }
}

#[cfg(not(feature = "covsig"))]
mod cov {
    pub struct Layout {
        pub n: usize,
        pub funcs: Vec<(u32, u32)>,
    }
    pub fn layout() -> Layout {
        eprintln!("conform sigs needs a coverage-instrumented build; run `conform minimize`");
        std::process::exit(2)
    }
    pub fn zero(_: &Layout) {}
    pub fn nonzero(_: &Layout, _: &mut Vec<u32>) {}
}

struct Sink {
    out: std::io::BufWriter<fs::File>,

    covout: std::io::BufWriter<fs::File>,
    seen: HashSet<u64>,
    layout: cov::Layout,
    covered: Vec<bool>,
    nz: Vec<u32>,
    suite: String,
    t0: Instant,
}

thread_local! {
    static SINK: RefCell<Option<Sink>> = const { RefCell::new(None) };
}

fn sig_hash(v: &[u32]) -> u64 {
    let b: Vec<u8> = v.iter().flat_map(|x| x.to_le_bytes()).collect();
    hash_bytes(&b)
}

impl Sink {
    fn record(&mut self, kind: char, key: &str, class: &str, failed: bool, formula: &str) {
        let us = self.t0.elapsed().as_micros();
        cov::nonzero(&self.layout, &mut self.nz);
        let sig = sig_hash(&self.nz);
        if self.seen.insert(sig) {
            let w = &mut self.covout;
            w.write_all(&sig.to_le_bytes()).unwrap();
            w.write_all(&(self.nz.len() as u32).to_le_bytes()).unwrap();
            let mut prev = 0u32;
            for &c in &self.nz {
                let mut d = c - prev;
                prev = c;
                while d >= 0x80 {
                    w.write_all(&[(d as u8 & 0x7f) | 0x80]).unwrap();
                    d >>= 7;
                }
                w.write_all(&[d as u8]).unwrap();
            }
        }
        let mut fs_: Vec<u32> = Vec::new();
        let mut novel = false;
        let mut fi = 0usize;
        for &c in &self.nz {
            if !self.covered[c as usize] {
                self.covered[c as usize] = true;
                novel = true;
            }
            while fi + 1 < self.layout.funcs.len() && self.layout.funcs[fi + 1].0 <= c {
                fi += 1;
            }
            if fs_.last() != Some(&(fi as u32)) {
                fs_.push(fi as u32);
            }
        }
        let fsig = sig_hash(&fs_);
        writeln!(
            self.out,
            "{kind}\t{}\t{key}\t{sig:x}\t{fsig:x}\t{}\t{class}\t{}\t{us}\t{}\t{formula}",
            self.suite,
            novel as u8,
            failed as u8,
            self.nz.len()
        )
        .unwrap();
    }
}

pub fn item_begin() {
    SINK.with(|s| {
        if let Some(s) = s.borrow_mut().as_mut() {
            cov::zero(&s.layout);
            s.t0 = Instant::now();
        }
    });
}

pub fn item_end(it: &J, idx: usize, failed: bool) {
    SINK.with(|s| {
        if let Some(s) = s.borrow_mut().as_mut() {
            let class = value_class(it);
            let key = format!("{idx}\t{:012x}", item_id(it));
            let formula = it["formula"]
                .as_str()
                .unwrap_or("")
                .replace(['\t', '\r', '\n'], " ");
            s.record('I', &key, &class, failed, &formula);
        }
    });
}

pub fn suite_begin() {
    item_begin();
}

pub fn suite_end(kind: &str, failed: bool) {
    SINK.with(|s| {
        if let Some(s) = s.borrow_mut().as_mut() {
            s.record('S', kind, "", failed, "");
        }
    });
}

pub fn value_class(it: &J) -> String {
    if it["invalid"].as_bool().unwrap_or(false) {
        return "invalid".into();
    }
    let v = &it["value"];
    let nd = if it["nondeterministic"].as_bool().unwrap_or(false) {
        "~"
    } else {
        ""
    };
    let t = v["t"].as_str().unwrap_or("-");
    let body = match t {
        "n" => {
            let x = v["v"].as_f64().unwrap_or(f64::NAN);
            if x.is_nan() {
                "nan".to_string()
            } else if x == 0.0 {
                if x.is_sign_negative() { "-0" } else { "0" }.into()
            } else {
                let dec = x.abs().log10().floor().clamp(-30.0, 30.0) as i32;
                let int = if x.fract() == 0.0 { "i" } else { "f" };
                format!("{}{int}{dec}", if x < 0.0 { '-' } else { '+' })
            }
        }
        "s" => {
            let s = v["v"].as_str().unwrap_or("");
            let n = s.chars().count();
            let len = match n {
                0 => "0",
                1 => "1",
                2..=8 => "s",
                9..=64 => "m",
                _ => "l",
            };
            let ascii = if s.is_ascii() { "a" } else { "u" };
            let num = if s.trim().parse::<f64>().is_ok() {
                "n"
            } else {
                ""
            };
            format!("{len}{ascii}{num}")
        }
        "e" => v["v"].as_str().unwrap_or("").to_string(),
        "b" => v["v"].to_string(),
        "a" => {
            let rows = v["v"].as_array().map_or(0, Vec::len);
            let cols = v["v"][0].as_array().map_or(0, Vec::len);
            let kinds: BTreeSet<&str> = v["v"]
                .as_array()
                .into_iter()
                .flatten()
                .flat_map(|r| r.as_array().into_iter().flatten())
                .map(|c| c["t"].as_str().unwrap_or("-"))
                .collect();
            format!(
                "{}x{}{}",
                rows.min(4),
                cols.min(4),
                kinds.into_iter().collect::<String>()
            )
        }
        _ => "-".into(),
    };
    format!("{nd}{t}{body}")
}

pub fn sigs_cmd(args: &[String]) -> ExitCode {
    let flag = |name: &str| {
        args.iter()
            .position(|a| a == name)
            .and_then(|i| args.get(i + 1))
            .cloned()
    };
    let out = flag("--out").expect("--out FILE");
    let (k, n) = flag("--slice")
        .and_then(|s| {
            let (a, b) = s.split_once('/')?;
            Some((a.parse::<usize>().ok()?, b.parse::<usize>().ok()?))
        })
        .unwrap_or((0, 1));
    let skip: Vec<String> = ["--out", "--slice"]
        .iter()
        .filter_map(|f| flag(f))
        .collect();
    let want: Vec<String> = args
        .iter()
        .filter(|a| !a.starts_with('-') && !skip.contains(a))
        .cloned()
        .collect();
    let all = crate::suites();
    let mut names: Vec<(u64, String)> = all
        .keys()
        .filter(|s| want.is_empty() || want.contains(s))
        .filter_map(|s| {
            let p = crate::conf_dir().join("golden").join(format!("{s}.json"));
            Some((fs::metadata(p).ok()?.len(), s.clone()))
        })
        .collect();

    names.sort();
    let mine: Vec<String> = names
        .into_iter()
        .enumerate()
        .filter(|(i, _)| i % n == k)
        .map(|(_, (_, s))| s)
        .collect();
    let layout = cov::layout();
    let nctr = layout.n;
    let f = fs::File::create(&out).unwrap();
    let covf = fs::File::create(format!("{out}.cov")).unwrap();
    let mut sink = Sink {
        out: std::io::BufWriter::new(f),
        covout: std::io::BufWriter::new(covf),
        seen: HashSet::new(),
        covered: vec![false; nctr],
        layout,
        nz: Vec::new(),
        suite: String::new(),
        t0: Instant::now(),
    };
    writeln!(
        sink.out,
        "#\tcounters\t{nctr}\tfuncs\t{}",
        sink.layout.funcs.len()
    )
    .unwrap();
    SINK.with(|s| *s.borrow_mut() = Some(sink));
    for (i, name) in mine.iter().enumerate() {
        SINK.with(|s| s.borrow_mut().as_mut().unwrap().suite = name.clone());
        let path = crate::conf_dir()
            .join("golden")
            .join(format!("{name}.json"));
        let Ok(bytes) = fs::read(&path) else { continue };
        let gh = hash_bytes(&bytes);
        SINK.with(|s| {
            let mut b = s.borrow_mut();
            let s = b.as_mut().unwrap();
            writeln!(s.out, "H\t{name}\t{gh:x}").unwrap();
        });
        let t = crate::run_suite_bytes(name, &path, &bytes, None, false, &SuiteSel::All, true);
        if let Some(t) = t {
            let bad = t.close + t.near + t.wrong;
            SINK.with(|s| {
                let mut b = s.borrow_mut();
                let s = b.as_mut().unwrap();
                writeln!(
                    s.out,
                    "T\t{name}\t{}\t{}\t{}\t{}",
                    t.exact, t.close, t.near, t.wrong
                )
                .unwrap();
                for f in &t.failures {
                    writeln!(s.out, "F\t{name}\t{}", f.replace(['\r', '\n', '\t'], " ")).unwrap();
                }
            });
            let _ = bad;
        }
        if i % 200 == 0 {
            eprintln!("[{k}/{n}] {i}/{} {name}", mine.len());
        }
    }
    SINK.with(|s| {
        if let Some(mut s) = s.borrow_mut().take() {
            s.out.flush().unwrap();
            s.covout.flush().unwrap();
        }
    });
    ExitCode::SUCCESS
}

struct Rec {
    suite: String,
    idx: usize,
    id: u64,
    sig: u64,
    class: String,
    failed: bool,
    us: u64,
    formula: String,
}

struct Params {
    k_item: usize,

    k_suite: usize,

    depth: usize,

    rare: usize,
}

pub fn minimize_cmd(args: &[String]) -> ExitCode {
    let flag = |name: &str| {
        args.iter()
            .position(|a| a == name)
            .and_then(|i| args.get(i + 1))
            .cloned()
    };
    let p = Params {
        k_item: flag("--k-item").and_then(|s| s.parse().ok()).unwrap_or(2),
        k_suite: flag("--k-suite").and_then(|s| s.parse().ok()).unwrap_or(1),
        depth: flag("--depth").and_then(|s| s.parse().ok()).unwrap_or(2),
        rare: flag("--rare").and_then(|s| s.parse().ok()).unwrap_or(400),
    };
    let jobs: usize = flag("--jobs").and_then(|s| s.parse().ok()).unwrap_or(3);
    let dir = flag("--sigs-dir")
        .map(PathBuf::from)
        .unwrap_or_else(|| crate::root().join(".tgt").join("sigs"));

    if !args.iter().any(|a| a == "--reuse") {
        if let Err(e) = record_sigs(&dir, jobs) {
            eprintln!("minimize: {e}");
            return ExitCode::FAILURE;
        }
    }

    let mut recs: Vec<Rec> = Vec::new();
    let mut golden: BTreeMap<String, u64> = BTreeMap::new();
    let mut failures: Vec<(String, String)> = Vec::new();
    let mut ctrs = 0usize;
    for e in fs::read_dir(&dir).unwrap().flatten() {
        if e.path().extension().is_none_or(|x| x != "sig") {
            continue;
        }
        let text = fs::read_to_string(e.path()).unwrap();
        for l in text.lines() {
            let f: Vec<&str> = l.split('\t').collect();
            match f[0] {
                "#" => ctrs = ctrs.max(f[2].parse().unwrap_or(0)),
                "H" => {
                    golden.insert(f[1].into(), u64::from_str_radix(f[2], 16).unwrap());
                }
                "F" => failures.push((f[1].into(), f[2].into())),
                "I" => recs.push(Rec {
                    suite: f[1].into(),
                    idx: f[2].parse().unwrap(),
                    id: u64::from_str_radix(f[3], 16).unwrap(),
                    sig: u64::from_str_radix(f[4], 16).unwrap(),
                    class: f[7].into(),
                    failed: f[8] == "1",
                    us: f[9].parse().unwrap(),
                    formula: f.get(11).unwrap_or(&"").to_string(),
                }),
                "S" => recs.push(Rec {
                    suite: f[1].into(),
                    idx: usize::MAX,
                    id: 0,
                    sig: u64::from_str_radix(f[3], 16).unwrap(),
                    class: f[2].into(),
                    failed: f[7] == "1",
                    us: f[8].parse().unwrap(),
                    formula: String::new(),
                }),
                _ => {}
            }
        }
    }
    eprintln!(
        "minimize: {} records, {} suites, {ctrs} counters",
        recs.len(),
        golden.len()
    );
    let pins = Pins::collect(&golden);

    let cov = Cover::load(&dir, ctrs, p.rare);
    let mut keep: BTreeMap<String, BTreeSet<u64>> = BTreeMap::new();
    let mut keep_suite: BTreeSet<String> = BTreeSet::new();
    let mut why: BTreeMap<&'static str, usize> = BTreeMap::new();
    let mut note = |k: &'static str| *why.entry(k).or_insert(0) += 1;
    let mut failing_suites: BTreeSet<&str> = BTreeSet::new();
    for (s, _) in &failures {
        failing_suites.insert(s);
    }

    struct Unit<'a> {
        r: &'a Rec,
        cost: f64,
    }
    let mut units: Vec<Unit> = Vec::new();
    let mut seen: HashSet<(&str, u64)> = HashSet::new();
    let mut item_total: HashMap<&str, usize> = HashMap::new();
    for r in &recs {
        if r.idx != usize::MAX {
            *item_total.entry(&r.suite).or_default() += 1;
            if !seen.insert((&r.suite, r.id)) {
                continue;
            }
        }
        units.push(Unit {
            r,
            cost: (r.us as f64).max(50.0),
        });
    }
    let select =
        |u: &Unit, keep: &mut BTreeMap<String, BTreeSet<u64>>, ks: &mut BTreeSet<String>| {
            if u.r.idx == usize::MAX {
                ks.insert(u.r.suite.clone())
            } else {
                keep.entry(u.r.suite.clone()).or_default().insert(u.r.id)
            }
        };

    for u in &units {
        let r = u.r;
        let pinned_fail =
            r.failed || (failing_suites.contains(r.suite.as_str()) && r.idx == usize::MAX);
        if pinned_fail && select(u, &mut keep, &mut keep_suite) {
            note(if r.idx == usize::MAX {
                "suite: failing (todo/out_of_scope)"
            } else {
                "item: failing (todo/out_of_scope)"
            });
        }
        let hist = if r.idx == usize::MAX {
            pins.suites.contains(&r.suite)
        } else {
            pins.formulas.contains(&r.formula)
                || pins
                    .suite_formulas
                    .contains(&(r.suite.clone(), r.formula.clone()))
        };
        if hist && select(u, &mut keep, &mut keep_suite) {
            note(if r.idx == usize::MAX {
                "suite: named by a fix commit"
            } else {
                "item: pinned by history/docs"
            });
        }
    }

    {
        let mut demand: Vec<u8> = cov
            .count
            .iter()
            .map(|&c| {
                if c as usize <= p.rare {
                    c.min(p.depth as u32) as u8
                } else {
                    0
                }
            })
            .collect();
        let mut used: HashSet<u64> = HashSet::new();
        let take = |sig: u64, demand: &mut Vec<u8>, used: &mut HashSet<u64>| {
            if used.insert(sig) {
                if let Some(l) = cov.rare.get(&sig) {
                    for &c in l {
                        demand[c as usize] = demand[c as usize].saturating_sub(1);
                    }
                }
            }
        };

        for u in &units {
            let in_keep = if u.r.idx == usize::MAX {
                keep_suite.contains(&u.r.suite)
            } else {
                keep.get(&u.r.suite).is_some_and(|k| k.contains(&u.r.id))
            };
            if in_keep {
                take(u.r.sig, &mut demand, &mut used);
            }
        }
        let gain = |sig: u64, demand: &[u8]| -> usize {
            cov.rare
                .get(&sig)
                .map_or(0, |l| l.iter().filter(|&&c| demand[c as usize] > 0).count())
        };
        let mut heap: std::collections::BinaryHeap<(u64, usize)> = Default::default();
        let key = |g: usize, cost: f64| ((g as f64 / cost) * 1e9) as u64;
        for (i, u) in units.iter().enumerate() {
            let g = gain(u.r.sig, &demand);
            if g > 0 {
                heap.push((key(g, u.cost), i));
            }
        }
        while let Some((k, i)) = heap.pop() {
            let u = &units[i];
            if used.contains(&u.r.sig) {
                continue;
            }
            let g = gain(u.r.sig, &demand);
            if g == 0 {
                continue;
            }
            let nk = key(g, u.cost);
            if nk < k {
                heap.push((nk, i));
                continue;
            }
            take(u.r.sig, &mut demand, &mut used);
            if select(u, &mut keep, &mut keep_suite) {
                note(if u.r.idx == usize::MAX {
                    "suite: region cover"
                } else {
                    "item: region cover"
                });
            }
        }
    }

    let mut groups: HashMap<(String, String), Vec<&Rec>> = HashMap::new();
    for u in &units {
        if u.r.idx != usize::MAX {
            groups
                .entry((top_function(&u.r.formula), u.r.class.clone()))
                .or_default()
                .push(u.r);
        }
    }
    for (_, mut g) in groups {
        g.sort_by_key(|r| (hash_bytes(&r.id.to_le_bytes()), r.idx));
        for r in g.iter().take(p.k_item) {
            if keep.entry(r.suite.clone()).or_default().insert(r.id) {
                note("item: class representative");
            }
        }
    }

    let mut fam: HashMap<String, Vec<&Unit>> = HashMap::new();
    for u in &units {
        if u.r.idx == usize::MAX {
            fam.entry(family(&u.r.suite)).or_default().push(u);
        }
    }
    for (_, mut g) in fam {
        g.sort_by(|a, b| a.cost.total_cmp(&b.cost).then(a.r.suite.cmp(&b.r.suite)));
        for u in g.iter().take(p.k_suite) {
            if keep_suite.insert(u.r.suite.clone()) {
                note("suite: family representative");
            }
        }
    }
    {
        let (mut kept_us, mut total_us) = (0u64, 0u64);
        for u in &units {
            total_us += u.r.us;
            let k = if u.r.idx == usize::MAX {
                keep_suite.contains(&u.r.suite)
            } else {
                keep.get(&u.r.suite).is_some_and(|k| k.contains(&u.r.id))
            };
            if k {
                kept_us += u.r.us;
            }
        }
        eprintln!(
            "minimize: engine time of kept units {:.0} s of {:.0} s (instrumented, one thread)",
            kept_us as f64 / 1e6,
            total_us as f64 / 1e6
        );
    }

    let all = crate::suites();
    let mut m = Manifest {
        suites: BTreeMap::new(),
    };
    let (mut kept_items, mut total_items) = (0usize, 0usize);
    for (name, (kind, _)) in &all {
        let Some(&gh) = golden.get(name) else {
            continue;
        };
        let entry = if *kind == "formulas" {
            let ids: Vec<u64> = keep
                .get(name)
                .map(|s| s.iter().copied().collect())
                .unwrap_or_default();
            total_items += item_total.get(name.as_str()).copied().unwrap_or(0);
            kept_items += ids.len();
            if ids.is_empty() {
                Keep::None
            } else {
                Keep::Items(ids)
            }
        } else if keep_suite.contains(name) {
            Keep::All
        } else {
            Keep::None
        };
        m.suites.insert(
            name.clone(),
            Entry {
                golden: gh,
                keep: entry,
            },
        );
    }
    let meta = json!({
        "tool": "conform minimize",
        "k_item": p.k_item,
        "k_suite": p.k_suite,
        "depth": p.depth,
        "rare": p.rare,
        "counters": ctrs,
        "formula_items_total": total_items,
        "formula_items_kept": kept_items,
        "why": why,
    });
    m.save(meta);
    for (k, v) in &why {
        eprintln!("  {v:>8}  {k}");
    }
    eprintln!(
        "minimize: kept {kept_items} of {total_items} formula items; {} of {} scenario/workbook suites; wrote {}",
        keep_suite.len(),
        all.iter().filter(|(_, (k, _))| *k != "formulas").count(),
        manifest_path().display()
    );
    ExitCode::SUCCESS
}

fn record_sigs(dir: &Path, jobs: usize) -> Result<(), String> {
    let root = crate::root();
    let tgt = root.join(".tgt").join("cov");
    fs::create_dir_all(dir).map_err(|e| e.to_string())?;
    for e in fs::read_dir(dir).map_err(|e| e.to_string())?.flatten() {
        if e.path()
            .extension()
            .is_some_and(|x| x == "sig" || x == "cov")
        {
            let _ = fs::remove_file(e.path());
        }
    }
    eprintln!(
        "minimize: building the coverage-instrumented binary in {}",
        tgt.display()
    );
    let st = Command::new("cargo")
        .args([
            "build",
            "--release",
            "--bin",
            "conform",
            "--features",
            "covsig",
        ])
        .env("RUSTFLAGS", "-Cinstrument-coverage")
        .env("CARGO_TARGET_DIR", &tgt)
        .env("LLVM_PROFILE_FILE", tgt.join("build-%p.profraw"))
        .current_dir(&root)
        .status()
        .map_err(|e| e.to_string())?;
    if !st.success() {
        return Err("instrumented build failed".into());
    }
    let exe = tgt.join("release").join("conform.exe");
    let exe = if exe.exists() {
        exe
    } else {
        tgt.join("release").join("conform")
    };
    eprintln!("minimize: recording signatures in {jobs} processes");
    let mut kids = Vec::new();
    for k in 0..jobs {
        let out = dir.join(format!("slice{k}.sig"));
        kids.push(
            Command::new(&exe)
                .args(["sigs", "--out"])
                .arg(&out)
                .args(["--slice", &format!("{k}/{jobs}")])
                .env("LLVM_PROFILE_FILE", dir.join(format!("p{k}-%p.profraw")))
                .spawn()
                .map_err(|e| e.to_string())?,
        );
    }
    for mut c in kids {
        let st = c.wait().map_err(|e| e.to_string())?;
        if !st.success() {
            return Err(format!("sigs process failed: {st}"));
        }
    }
    Ok(())
}

struct Pins {
    formulas: HashSet<String>,

    suite_formulas: HashSet<(String, String)>,

    suites: BTreeSet<String>,
}

fn git(args: &[&str]) -> String {
    Command::new("git")
        .args(args)
        .current_dir(crate::root())
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).into_owned())
        .unwrap_or_default()
}

fn backticked_formulas(text: &str, out: &mut HashSet<String>) {
    for (i, part) in text.split('`').enumerate() {
        if i % 2 == 1 && part.starts_with('=') && part.len() > 2 {
            out.insert(part.to_string());
        }
    }
}

impl Pins {
    fn collect(golden: &BTreeMap<String, u64>) -> Pins {
        let mut p = Pins {
            formulas: HashSet::new(),
            suite_formulas: HashSet::new(),
            suites: BTreeSet::new(),
        };

        let hist = git(&[
            "log",
            "-p",
            "--format=",
            "--",
            "conformance/todo.txt",
            "conformance/out_of_scope.txt",
        ]);
        for l in hist.lines() {
            let Some(l) = l.strip_prefix('+').or_else(|| l.strip_prefix('-')) else {
                continue;
            };
            let mut it = l.splitn(3, " | ");
            let (Some(suite), Some(rest)) = (it.next(), it.next()) else {
                continue;
            };
            if !golden.contains_key(suite.trim()) {
                continue;
            }
            let f = rest.split("  → ").next().unwrap_or("").trim();
            if f.starts_with('=') || f.starts_with('{') {
                p.suite_formulas
                    .insert((suite.trim().into(), f.to_string()));
            }
        }

        let log = git(&["log", "--format=%H%x1f%B%x1e"]);
        for c in log.split('') {
            let Some((_, msg)) = c.split_once('') else {
                continue;
            };
            let low = msg.to_lowercase();
            let fix = [
                "fix", "wrong", "bug", "regress", "correct", "close ", "mismatch",
            ]
            .iter()
            .any(|w| low.contains(w));
            if !fix {
                continue;
            }
            backticked_formulas(msg, &mut p.formulas);
            for w in msg.split(|c: char| !(c.is_ascii_alphanumeric() || c == '_')) {
                if golden.contains_key(w) {
                    p.suites.insert(w.to_string());
                }
            }
        }

        if let Ok(rd) = fs::read_dir(crate::root().join("docs").join("rules")) {
            for e in rd.flatten() {
                if let Ok(t) = fs::read_to_string(e.path()) {
                    backticked_formulas(&t, &mut p.formulas);
                }
            }
        }
        p
    }
}

fn top_function(f: &str) -> String {
    let Some(rest) = f.strip_prefix('=') else {
        return "~text".into();
    };
    let name: String = rest
        .chars()
        .take_while(|c| c.is_ascii_alphanumeric() || *c == '.' || *c == '_')
        .collect();
    if rest[name.len()..].starts_with('(') && !name.is_empty() {
        name.to_ascii_uppercase()
    } else {
        "-".into()
    }
}

fn family(name: &str) -> String {
    name.trim_end_matches(|c: char| c.is_ascii_digit())
        .to_string()
}

struct Cover {
    rare: HashMap<u64, Vec<u32>>,

    count: Vec<u32>,
}

fn read_cov(path: &Path, mut f: impl FnMut(u64, &[u32])) {
    let b = fs::read(path).unwrap_or_default();
    let mut i = 0usize;
    let mut v: Vec<u32> = Vec::new();
    while i + 12 <= b.len() {
        let sig = u64::from_le_bytes(b[i..i + 8].try_into().unwrap());
        let n = u32::from_le_bytes(b[i + 8..i + 12].try_into().unwrap()) as usize;
        i += 12;
        v.clear();
        let mut prev = 0u32;
        for _ in 0..n {
            let mut d = 0u32;
            let mut sh = 0;
            loop {
                let x = b[i];
                i += 1;
                d |= ((x & 0x7f) as u32) << sh;
                if x & 0x80 == 0 {
                    break;
                }
                sh += 7;
            }
            prev += d;
            v.push(prev);
        }
        f(sig, &v);
    }
}

impl Cover {
    fn load(dir: &Path, ctrs: usize, rare_max: usize) -> Cover {
        let files: Vec<PathBuf> = fs::read_dir(dir)
            .unwrap()
            .flatten()
            .map(|e| e.path())
            .filter(|p| p.extension().is_some_and(|x| x == "cov"))
            .collect();
        let mut count = vec![0u32; ctrs];
        let mut seen: HashSet<u64> = HashSet::new();
        for f in &files {
            read_cov(f, |sig, v| {
                if seen.insert(sig) {
                    for &c in v {
                        count[c as usize] += 1;
                    }
                }
            });
        }
        let mut rare: HashMap<u64, Vec<u32>> = HashMap::new();
        for f in &files {
            read_cov(f, |sig, v| {
                rare.entry(sig).or_insert_with(|| {
                    v.iter()
                        .copied()
                        .filter(|&c| count[c as usize] as usize <= rare_max)
                        .collect()
                });
            });
        }
        eprintln!(
            "minimize: {} distinct paths, {} counters ({} rare)",
            seen.len(),
            ctrs,
            count
                .iter()
                .filter(|&&c| c > 0 && c as usize <= rare_max)
                .count()
        );
        Cover { rare, count }
    }
}
