use std::process::Command;
use std::time::{Duration, Instant};

struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
    fn below(&mut self, n: usize) -> usize {
        (self.next() % n.max(1) as u64) as usize
    }
    fn chance(&mut self, pct: usize) -> bool {
        self.below(100) < pct
    }
    fn pick<'a>(&mut self, xs: &[&'a str]) -> &'a str {
        xs[self.below(xs.len())]
    }
}

const HUGE: [&str; 11] = [
    "perf",
    "bench",
    "XFD",
    "XFE",
    "1048576",
    "1048577",
    "A:A",
    "1:1",
    "4294967295",
    "99999999",
    "1e308",
];

const SHEETS: [&str; 5] = ["", "Sheet1!", "Sheet2!", "'My Sheet'!", "Nope!"];

fn cell(g: &mut Rng) -> String {
    let col = match g.below(10) {
        0 => "XFD".to_string(),
        1 => "XFE".to_string(),
        2 => "AA".to_string(),
        3 => "".to_string(),
        _ => g.pick(&["A", "B", "C", "D", "E"]).to_string(),
    };
    let row = match g.below(10) {
        0 => "1048576".to_string(),
        1 => "1048577".to_string(),
        2 => "0".to_string(),
        3 => "-1".to_string(),
        4 => "99999999999999999999".to_string(),
        _ => (1 + g.below(12)).to_string(),
    };
    let sheet = if g.chance(20) { g.pick(&SHEETS) } else { "" };
    format!("{sheet}{col}{row}")
}

fn range(g: &mut Rng) -> String {
    match g.below(12) {
        0 => "A:A".into(),
        1 => "1:1".into(),
        2 => "A1:XFD1048576".into(),
        3 => "R1C1:R3C3".into(),
        _ => format!("{}:{}", cell(g), cell(g)),
    }
}

fn formula(g: &mut Rng) -> String {
    match g.below(12) {
        0 => format!("={}1{}", "(".repeat(g.below(400)), ")".repeat(g.below(400))),
        1 => format!("={}A1", "-".repeat(g.below(1500))),
        2 => "=".to_string() + &"SUM(".repeat(g.below(80)),
        3 => format!("=SEQUENCE({},{})", g.below(30), g.below(30)),
        4 => format!("=SUM({})", range(g)),
        5 => format!("={}+{}", cell(g), cell(g)),
        6 => format!("=LET(x,{},x*2)", cell(g)),
        7 => format!("={}#", cell(g)),
        8 => format!("=INDEX({},{},{})", range(g), g.below(5), g.below(5)),
        9 => "=\"".to_string() + &"x".repeat(g.below(300)),
        10 => g
            .pick(&[
                "=1/0",
                "=NOW()",
                "=RAND()",
                "=A1:A3*2",
                "=@A1:A3",
                "=#REF!",
                "={1,2;3,4}",
                "=ROW()",
            ])
            .to_string(),
        _ => format!(
            "={}{}{}",
            cell(g),
            g.pick(&["+", "*", "&", "^", "<", ":"]),
            g.below(100)
        ),
    }
}

fn input(g: &mut Rng) -> String {
    match g.below(8) {
        0 => g.below(1000).to_string(),
        1 => g
            .pick(&[
                "hello", "'text", "TRUE", "1/2/2020", "12:30", "5%", "1e400", "-", "", "é🎉",
                "#N/A",
            ])
            .to_string(),
        _ => formula(g),
    }
}

fn line(g: &mut Rng) -> String {
    match g.below(60) {
        0..=14 => format!("{} {}", cell(g), input(g)),
        15 => cell(g),
        16 => format!("clear {}", range(g)),
        17 => format!("array {} {}", range(g), formula(g)),
        18 => format!("legacy {} {}", cell(g), formula(g)),
        19 => {
            format!("{} {}", g.pick(&["merge", "unmerge"]), range(g))
                + g.pick(&["", " across", " junk"])
        }
        20 => format!("show {}", range(g)),
        21 => g
            .pick(&[
                "used",
                "calc",
                "fullcalc",
                "rebuild",
                "help",
                "sheets",
                "tables",
                "name",
                "functions",
            ])
            .to_string(),
        22 => format!("mode {}", g.pick(&["auto", "manual", "semi", "", "bogus"])),
        23 => format!(
            "iter {} {} {}",
            g.pick(&["on", "off", "", "x"]),
            g.pick(&["1", "100", "0", "-5", "99999999999", "x"]),
            g.pick(&["0.001", "0", "-1", "nan", "inf", "1e308", "x"])
        ),
        24 => format!(
            "goalseek {} {} {}",
            cell(g),
            g.pick(&["0", "10", "x", "1e308", "-0"]),
            cell(g)
        ),
        25 => format!("datatable {} row {} col {}", range(g), cell(g), cell(g)),
        26..=28 => format!(
            "{} {} {} {}",
            g.pick(&["insert", "delete"]),
            g.pick(&["rows", "cols", "cells", "row", "col"]),
            g.pick(&[
                "1",
                "3",
                "A",
                "XFD",
                "1048576",
                "1048577",
                "0",
                "-1",
                "99999999999",
                "B2:D4",
                "A1:XFD1048576"
            ]),
            g.pick(&[
                "",
                "1",
                "5",
                "1048576",
                "4294967295",
                "-3",
                "down",
                "right",
                "up",
                "left",
                "x"
            ])
        ),
        29..=31 => format!(
            "{} {} {}",
            g.pick(&["copy", "cut", "fill"]),
            range(g),
            if g.chance(50) { cell(g) } else { range(g) }
        ),
        32 => format!(
            "autofill {} {} {}",
            range(g),
            range(g),
            g.pick(&["", "copy", "series", "days", "months", "growth", "linear", "zzz"])
        ),
        33 => format!(
            "series {} {} {} {} step={} stop={} {}",
            range(g),
            g.pick(&["rows", "cols", ""]),
            g.pick(&["linear", "growth", "date", "autofill", ""]),
            g.pick(&["day", "weekday", "month", "year", ""]),
            g.pick(&["1", "0", "-2.5", "1e308", "nan", "x"]),
            g.pick(&["10", "-1e308", "inf", "x"]),
            g.pick(&["trend", ""])
        ),
        34 => format!(
            "sort {} {} {} {} {} {}",
            range(g),
            g.pick(&["1", "2", "0", "99", "-1"]),
            g.pick(&["asc", "desc", ""]),
            g.pick(&["2", "1", ""]),
            g.pick(&["header", ""]),
            g.pick(&["cols", "matchcase", ""])
        ),
        35 => format!(
            "format {} {}",
            range(g),
            g.pick(&[
                "0.00",
                "General",
                "@",
                "yyyy-mm-dd",
                "[h]:mm",
                "\"x",
                "[",
                "0%",
                "#,##0;[Red]-#,##0"
            ])
        ),
        36 => format!("precision {}", g.pick(&["on", "off", ""])),
        37 => format!(
            "paste {} {} {} {} {} {}",
            range(g),
            range(g),
            g.pick(&["values", "formulas", "formats", "all", ""]),
            g.pick(&["add", "sub", "mul", "div", ""]),
            g.pick(&["skipblanks", ""]),
            g.pick(&["transpose", ""])
        ),
        38 => format!(
            "table {} {}",
            g.pick(&["T1", "T2", "1T", "A1", "", "x y"]),
            range(g)
        ),
        39 => format!(
            "totals {} {}",
            g.pick(&["T1", "T2", "Nope"]),
            g.pick(&["on", "off", ""])
        ),
        40 => format!(
            "{} {}",
            g.pick(&["unlist", "deltable"]),
            g.pick(&["T1", "T2", "Nope"])
        ),
        41 => format!("resize {} {}", g.pick(&["T1", "T2"]), range(g)),
        42 => format!(
            "tablerename {} {}",
            g.pick(&["T1", "T2"]),
            g.pick(&["T3", "T1", "", "1x"])
        ),
        43 => format!(
            "headers {} {}",
            g.pick(&["T1", "T2"]),
            g.pick(&["on", "off", ""])
        ),
        44 => format!(
            "sheet {}",
            g.pick(&[
                "Sheet1",
                "Sheet2",
                "My Sheet",
                "",
                "a'b",
                "[x]",
                &"y".repeat(40)
            ])
        ),
        45 => format!(
            "rename {} = {}",
            g.pick(&["Sheet1", "Sheet2", "Nope"]),
            g.pick(&["Sheet9", "", "A1", "Sheet2", "x/y"])
        ),
        46 => format!(
            "dropsheet {}",
            g.pick(&["Sheet1", "Sheet2", "Nope", "My Sheet"])
        ),
        47 => format!(
            "movesheet {} before {}",
            g.pick(&["Sheet1", "Sheet2"]),
            g.pick(&["Sheet1", "Sheet2", "Nope", ""])
        ),
        48 => format!(
            "copysheet {} before {}",
            g.pick(&["Sheet1", "Sheet2"]),
            g.pick(&["Sheet1", "Sheet2", "Nope", ""])
        ),
        49 => format!(
            "name {} ={}",
            g.pick(&["nm", "nm2", "A1", "", "x y", "R1C1"]),
            formula(g).trim_start_matches('=')
        ),
        50 => format!("delname {}", g.pick(&["nm", "nm2", "Nope", ""])),
        51 => format!(
            "{} {}",
            g.pick(&["bench", "perf"]),
            g.pick(&["10", "300", "0", "-1", "x", "", "99999999999"])
        ),
        52 => format!(
            "save {}",
            g.pick(&["fuzz_con_a.json", "", "C:/nonexistent_dir/x/y.json"])
        ),
        53 => format!(
            "load {}",
            g.pick(&[
                "fuzz_con_a.json",
                "",
                "C:/nonexistent_dir/x/y.json",
                "fuzz_con_junk.json"
            ])
        ),
        54 => {
            let n = g.below(30);
            (0..n)
                .map(|_| char::from_u32(g.below(0x700) as u32).unwrap_or('?'))
                .collect()
        }
        55 => {
            let toks = [
                "clear", "sort", "paste", "A1", ":", "=", "(", ")", "'", "\"", "Sheet1!", "$",
                "R1C1", "#",
            ];
            (0..1 + g.below(8))
                .map(|_| g.pick(&toks))
                .collect::<Vec<_>>()
                .join(" ")
        }
        _ => format!("{} {}", cell(g), input(g)),
    }
}

fn session(seed: u64, lines: usize, wild: bool, dir: &std::path::Path) -> Option<String> {
    let mut g = Rng(seed.wrapping_mul(0x2545_F491_4F6C_DD1D) ^ 0x00C0_FFEE);
    let mut script = String::from("A1 1\nB1 =A1+1\ntable T1 A1:B3\n");
    for _ in 0..lines {
        let mut l = line(&mut g);

        if !wild && HUGE.iter().any(|h| l.contains(h)) {
            l = "A1 1".into();
        }
        script.push_str(&l);
        script.push('\n');
    }
    std::fs::write(dir.join("fuzz_con_junk.json"), "{ not json").ok();
    let script_path = dir.join(format!("fuzz_con_{seed}.in"));
    let out_path = dir.join(format!("fuzz_con_{seed}.out"));
    let err_path = dir.join(format!("fuzz_con_{seed}.err"));
    std::fs::write(&script_path, &script).unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_xlcalc"))
        .current_dir(dir)
        .stdin(std::fs::File::open(&script_path).unwrap())
        .stdout(std::fs::File::create(&out_path).unwrap())
        .stderr(std::fs::File::create(&err_path).unwrap())
        .spawn()
        .expect("start console");
    let start = Instant::now();
    let status = loop {
        match child.try_wait().expect("wait") {
            Some(st) => break st,
            None if start.elapsed() > Duration::from_secs(60) => {
                let _ = child.kill();
                let _ = child.wait();
                let text = std::fs::read(&out_path).unwrap_or_default();
                let text = String::from_utf8_lossy(&text).into_owned();
                let last = text
                    .lines()
                    .rfind(|l| l.starts_with("> "))
                    .unwrap_or("")
                    .to_string();
                let keep = std::env::temp_dir().join(format!("xlcalc_fuzz_hang_{seed}.txt"));
                let _ = std::fs::write(&keep, &script);
                return Some(format!(
                    "seed {seed}: console hung (>60 s) on {last:?} (script kept in {})",
                    keep.display()
                ));
            }
            None => std::thread::sleep(Duration::from_millis(20)),
        }
    };
    if start.elapsed() > Duration::from_secs(5) {
        let keep = std::env::temp_dir().join(format!("xlcalc_fuzz_slow_{seed}.txt"));
        let _ = std::fs::write(&keep, &script);
        eprintln!(
            "SLOW seed {seed}: {:?} (script in {})",
            start.elapsed(),
            keep.display()
        );
    }
    let out = std::process::Output {
        status,
        stdout: std::fs::read(&out_path).unwrap_or_default(),
        stderr: std::fs::read(&err_path).unwrap_or_default(),
    };
    for p in [&script_path, &out_path, &err_path] {
        let _ = std::fs::remove_file(p);
    }
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    if stdout.contains("internal error") || stderr.contains("panicked") || !out.status.success() {
        let mut last = String::new();
        let mut culprit = String::new();
        for l in stdout.lines() {
            if let Some(c) = l.strip_prefix("> ") {
                last = c.to_string();
            }
            if l.contains("internal error") {
                culprit = last.clone();
                break;
            }
        }
        let keep = std::env::temp_dir().join(format!("xlcalc_fuzz_crash_{seed}.txt"));
        let _ = std::fs::write(&keep, &script);
        return Some(format!(
            "seed {seed}: status {:?}; failing line: {culprit:?}; stderr: {}",
            out.status,
            stderr.lines().take(4).collect::<Vec<_>>().join(" | ")
        ));
    }
    None
}

fn run(start: u64, seeds: u64, lines: usize, wild: bool) {
    let dir = std::env::temp_dir().join(format!("xlcalc_fuzz_{}_{start}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let mut failures = Vec::new();
    for seed in start..start + seeds {
        failures.extend(session(seed, lines, wild, &dir));
    }
    let _ = std::fs::remove_dir_all(&dir);
    for f in &failures {
        eprintln!("FAIL {f}");
    }
    assert!(
        failures.is_empty(),
        "{} console fuzz failures",
        failures.len()
    );
}

#[test]
fn random_console_lines_never_crash() {
    run(0, 12, 150, false);
}

#[test]
#[ignore]
fn random_console_lines_long() {
    let env = |k: &str, d: u64| {
        std::env::var(k)
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(d)
    };
    run(
        env("CONSOLE_FUZZ_START", 1000),
        env("CONSOLE_FUZZ_SEEDS", 100),
        env("CONSOLE_FUZZ_LINES", 1000) as usize,
        env("CONSOLE_FUZZ_WILD", 1) != 0,
    );
}
