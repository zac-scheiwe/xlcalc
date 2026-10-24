use std::io::Write;
use std::process::{Command, Stdio};

fn run(input: &[u8]) -> (String, String, bool) {
    let mut child = Command::new(env!("CARGO_BIN_EXE_xlcalc"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("start console");
    child.stdin.take().unwrap().write_all(input).unwrap();
    let out = child.wait_with_output().unwrap();
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.success(),
    )
}

fn normalise(out: &str) -> String {
    out.lines()
        .map(|l| match (l.find(" evaluation"), l.find(" in ")) {
            (Some(e), Some(i)) if i > e => {
                let rest = &l[i + 4..];
                let end = rest.find(':').unwrap_or(rest.len());
                format!("{} in <t>{}", &l[..i], &rest[end..])
            }
            _ => l.to_string(),
        })
        .collect::<Vec<_>>()
        .join("\n")
        + "\n"
}

#[test]
fn basic_script_matches_golden() {
    let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/scripts");
    let script = std::fs::read(format!("{dir}/basic.txt")).unwrap();
    let (out, err, ok) = run(&script);
    assert!(ok, "console failed: {err}");
    assert!(!err.contains("panicked"), "panic: {err}");
    let got = normalise(&out);
    let path = format!("{dir}/basic.expected");
    if std::env::var_os("UPDATE_GOLDEN").is_some() {
        std::fs::write(&path, &got).unwrap();
    }
    let want = std::fs::read_to_string(&path)
        .unwrap()
        .replace("\r\n", "\n");
    assert_eq!(
        got, want,
        "console output changed (UPDATE_GOLDEN=1 to accept)"
    );
}

#[test]
fn help_lists_every_command() {
    let (out, _, ok) = run(b"help\n");
    assert!(ok);
    for cmd in [
        "clear",
        "array",
        "legacy",
        "merge",
        "unmerge",
        "show",
        "used",
        "calc",
        "fullcalc",
        "rebuild",
        "mode",
        "iter",
        "goalseek",
        "datatable",
        "insert",
        "delete",
        "copy",
        "cut",
        "fill",
        "sort",
        "format",
        "precision",
        "paste",
        "table",
        "totals",
        "tables",
        "sheet",
        "sheets",
        "rename",
        "dropsheet",
        "movesheet",
        "copysheet",
        "name",
        "delname",
        "save",
        "load",
        "functions",
        "bench",
        "perf",
        "help",
        "quit",
    ] {
        assert!(out.contains(cmd), "help does not mention `{cmd}`");
    }
}

#[test]
fn garbage_input_never_panics() {
    let mut input: Vec<u8> = Vec::new();
    let junk = [
        "insert",
        "insert rows",
        "insert rows 4294967295 4294967295",
        "delete cols XFE 3",
        "copy ::: !!!",
        "cut A1 'x",
        "fill",
        "sort",
        "sort A1 0",
        "sort A1:B2 99999999999",
        "paste",
        "paste A1 B1 nonsense",
        "format",
        "format A1",
        "table",
        "table T",
        "totals",
        "name x",
        "name 1 =1",
        "name x =(((",
        "array",
        "array A1 =",
        "legacy",
        "merge",
        "goalseek",
        "goalseek A1 x B1",
        "datatable",
        "datatable A1:B2 row",
        "iter on -1",
        "iter on 1e999",
        "mode",
        "sheet 'a",
        "rename",
        "rename = ",
        "movesheet x before y",
        "copysheet",
        "load",
        "load /no/such/file",
        "save",
        "'",
        "'''!A1 1",
        "R0C0 1",
        "R1C1:R9C9 x",
        "=",
        "=1+",
        "A1 =((((((",
        "A1 =\"unterminated",
        "A1 {=1}",
        "\u{1F600} \u{1F600}",
        "A:A 1",
        "1:1 1",
        "A1:B2 5",
        "show ZZ",
        "show A1:",
        "bench x",
        "bench 1",
    ];
    for l in junk {
        input.extend_from_slice(l.as_bytes());
        input.push(b'\n');
    }
    input.extend_from_slice(&[0xff, 0xfe, 0x80, b'\n', 0xc3, 0x28, b'\n']);
    input.extend_from_slice(b"A1 still works\nA1\n");
    let (out, err, ok) = run(&input);
    assert!(ok, "console exited abnormally: {err}");
    assert!(!err.contains("panicked"), "panic: {err}");
    assert!(out.contains("still works"), "session ended early:\n{out}");
}
