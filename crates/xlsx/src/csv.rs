use xlcalc::{CellAddr, Locale, NumberFormat, Rect, SheetId, Value, Workbook, XlError};

pub const XL_CSV: i32 = 6;

pub const XL_CSV_UTF8: i32 = 62;

pub const XL_UNICODE_TEXT: i32 = 42;

pub const XL_SYLK: i32 = 2;

pub const XL_CURRENT_PLATFORM_TEXT: i32 = -4158;

pub struct CsvImport {
    pub workbook: Workbook,

    pub name: String,

    pub file_format: i32,
}

pub fn import_csv(bytes: &[u8], file_name: &str, local: bool) -> CsvImport {
    import(bytes, file_name, local, false)
}

pub fn import_csv_unsplit(bytes: &[u8], file_name: &str, local: bool) -> CsvImport {
    import(bytes, file_name, local, true)
}

fn import(bytes: &[u8], file_name: &str, local: bool, unsplit: bool) -> CsvImport {
    let mut wb = Workbook::empty();
    let sheet = wb
        .add_sheet(&sheet_name_for(file_name))
        .expect("a sheet name made by sheet_name_for is valid");
    let (text, mut delim, mut file_format) = decode(bytes);
    if unsplit && file_format != XL_SYLK {
        delim = '\0';
        file_format = XL_CURRENT_PLATFORM_TEXT;
    }
    let rows = if file_format == XL_SYLK {
        Vec::new()
    } else {
        split(&text, delim)
    };
    let mut locale = wb.locale().clone();
    if !local {
        locale.date_order = Locale::en_us().date_order;
    }
    wb.with_input_locale(&locale, |wb| {
        for (r, row) in rows.iter().enumerate() {
            for (c, field) in row.iter().enumerate() {
                if field.is_empty() {
                    continue;
                }
                let addr = CellAddr::new(sheet, r as u32, c as u32);
                enter(wb, addr, field);
            }
        }
    });
    CsvImport {
        workbook: wb,
        name: file_name.to_string(),
        file_format,
    }
}

fn enter(wb: &mut Workbook, addr: CellAddr, field: &str) {
    if field.eq_ignore_ascii_case("#SPILL!") {
        wb.set_value(addr, Value::Error(XlError::SPILL));
    } else if field.starts_with('\'')
        || field.starts_with("==")
        || wb.set_input(addr, field).is_err()
    {
        wb.set_value(addr, Value::from(field));
    } else if matches!(wb.value(addr), Value::Text(_)) && field.contains(['e', 'E']) {
        let big = field.trim().parse::<f64>().is_ok_and(f64::is_infinite);
        wb.set_number_format(
            addr.sheet,
            Rect::new(addr.row, addr.col, addr.row, addr.col),
            if big { "0.00E+00" } else { "General" },
        );
    }
}

fn sheet_name_for(file_name: &str) -> String {
    let base = file_name.rsplit_once('.').map_or(file_name, |(b, _)| b);
    let s: String = base
        .chars()
        .map(|c| if ":\\/?*[]".contains(c) { '_' } else { c })
        .take(31)
        .collect();
    if s.is_empty() {
        "Sheet1".into()
    } else {
        s
    }
}

const CP1252_HIGH: [char; 32] = [
    '\u{20AC}', '\u{81}', '\u{201A}', '\u{0192}', '\u{201E}', '\u{2026}', '\u{2020}', '\u{2021}',
    '\u{02C6}', '\u{2030}', '\u{0160}', '\u{2039}', '\u{0152}', '\u{8D}', '\u{017D}', '\u{8F}',
    '\u{90}', '\u{2018}', '\u{2019}', '\u{201C}', '\u{201D}', '\u{2022}', '\u{2013}', '\u{2014}',
    '\u{02DC}', '\u{2122}', '\u{0161}', '\u{203A}', '\u{0153}', '\u{9D}', '\u{017E}', '\u{0178}',
];

fn decode_ansi(bytes: &[u8]) -> String {
    bytes
        .iter()
        .map(|&b| match b {
            0x80..=0x9F => CP1252_HIGH[(b - 0x80) as usize],
            b => b as char,
        })
        .collect()
}

fn decode(bytes: &[u8]) -> (String, char, i32) {
    if bytes.starts_with(&[0xFF, 0xFE]) {
        let units: Vec<u16> = bytes[2..]
            .as_chunks::<2>()
            .0
            .iter()
            .map(|p| u16::from_le_bytes(*p))
            .collect();
        return (String::from_utf16_lossy(&units), '\t', XL_UNICODE_TEXT);
    }
    if bytes.starts_with(b"ID;") {
        return (String::new(), ',', XL_SYLK);
    }

    let csv = bytes.len() >= 4;
    let (text, fmt) = match bytes.strip_prefix(&[0xEF, 0xBB, 0xBF]) {
        Some(rest) if csv => (String::from_utf8_lossy(rest).into_owned(), XL_CSV_UTF8),
        _ => (
            decode_ansi(bytes),
            if csv {
                XL_CSV
            } else {
                XL_CURRENT_PLATFORM_TEXT
            },
        ),
    };
    (
        text.replace(char::from(0), ""),
        if csv { ',' } else { char::from(9) },
        fmt,
    )
}

const QUOTED_CUT: usize = 32759;

fn split(text: &str, delim: char) -> Vec<Vec<String>> {
    let mut rows = Vec::new();
    let mut row: Vec<String> = Vec::new();
    let mut field = String::new();
    let mut it = text.chars().peekable();
    let mut pending = false;
    while let Some(c) = it.next() {
        pending = true;
        if c == '"' && field.is_empty() {
            let mut cut = false;
            let mut len = 0usize;
            loop {
                if len == QUOTED_CUT {
                    cut = true;
                    break;
                }
                len += 1;
                match it.next() {
                    Some('"') => {
                        if it.peek() == Some(&'"') {
                            it.next();
                            field.push('"');
                        } else {
                            break;
                        }
                    }
                    Some(ch) => field.push(ch),
                    None => break,
                }
            }
            if cut {
                it.next();
                row.push(std::mem::take(&mut field));
                rows.push(std::mem::take(&mut row));
                pending = false;
                continue;
            }
            while let Some(&n) = it.peek() {
                if n == delim || n == '\r' || n == '\n' {
                    break;
                }
                field.push(n);
                it.next();
            }
        } else if c == delim {
            row.push(std::mem::take(&mut field));
        } else if c == '\r' || c == '\n' {
            if c == '\r' && it.peek() == Some(&'\n') {
                it.next();
            }
            row.push(std::mem::take(&mut field));
            rows.push(std::mem::take(&mut row));
            pending = false;
        } else {
            field.push(c);
        }
    }
    if pending {
        row.push(field);
        rows.push(row);
    }
    rows
}

pub struct CsvSaved {
    pub bytes: Vec<u8>,

    pub name: String,

    pub file_format: i32,
}

pub fn save_as_csv(
    wb: &mut Workbook,
    file_name: &str,
    file_format: i32,
) -> Result<CsvSaved, String> {
    let sheet = wb.active_sheet();
    let bytes = export_csv(wb, sheet, file_format)?;
    let new = sheet_name_for(file_name);
    if wb.sheet(sheet).map(|s| s.name.as_str()) != Some(new.as_str()) {
        let _ = wb.rename_sheet(sheet, &new);
    }
    Ok(CsvSaved {
        bytes,
        name: file_name.to_string(),
        file_format,
    })
}

pub fn export_csv(wb: &Workbook, sheet: SheetId, file_format: i32) -> Result<Vec<u8>, String> {
    if file_format != XL_CSV && file_format != XL_CSV_UTF8 {
        return Err(format!("not a CSV format: {file_format}"));
    }
    let mut out = String::new();
    if let Some(Rect { r1, c1, .. }) = wb.used_range(sheet) {
        for r in 0..=r1 {
            for c in 0..=c1 {
                if c > 0 {
                    out.push(',');
                }
                let mut t = cell_text(wb, CellAddr::new(sheet, r, c));

                if let Some((i, _)) = t.char_indices().nth(32767) {
                    t.truncate(i);
                }
                if t.contains([',', '"', '\r', '\n', '\t']) {
                    out.push('"');
                    out.push_str(&t.replace('"', "\"\""));
                    out.push('"');
                } else {
                    out.push_str(&t);
                }
            }
            out.push_str("\r\n");
        }
    }
    Ok(if file_format == XL_CSV_UTF8 {
        let mut b = vec![0xEF, 0xBB, 0xBF];
        b.extend_from_slice(out.as_bytes());
        b
    } else {
        encode_ansi(&out)
    })
}

fn cell_text(wb: &Workbook, addr: CellAddr) -> String {
    let v = wb.value(addr);
    match &v {
        Value::Blank => return String::new(),
        Value::Error(e) => return e.to_string(),
        Value::Bool(b) => return if *b { "TRUE" } else { "FALSE" }.into(),
        Value::Text(t) if wb.number_format(addr) == "@" => return t.to_string(),
        _ => {}
    }
    let short = &wb.locale().short_date_format;
    let code = wb.number_format(addr);
    let code = if wb.cell_style(addr).custom_code {
        code.to_string()
    } else if code.eq_ignore_ascii_case(short) {
        "m/d/yyyy".to_string()
    } else if code
        .strip_suffix(" h:mm")
        .is_some_and(|d| d.eq_ignore_ascii_case(short))
    {
        "m/d/yyyy h:mm".to_string()
    } else {
        code.to_string()
    };
    let has_ampm = has_ampm(&code);
    match NumberFormat::parse(&code)
        .and_then(|f| f.format(&v, &Locale::en_us(), wb.settings().date_system))
    {
        Ok(r) if has_ampm => upper_designator(&r.text, frac(&v) < 0.5),
        Ok(r) => r.text,
        Err(_) => wb.display_text(addr),
    }
}

fn frac(v: &Value) -> f64 {
    match v {
        Value::Number(n) => n.rem_euclid(1.0),
        _ => 0.0,
    }
}

fn has_ampm(code: &str) -> bool {
    let (mut quoted, mut escaped) = (false, false);
    let mut i = 0;
    while let Some(c) = code[i..].chars().next() {
        if escaped {
            escaped = false;
        } else if c == '"' {
            quoted = !quoted;
        } else if !quoted && c == '\\' {
            escaped = true;
        } else if !quoted
            && code[i..]
                .get(..5)
                .is_some_and(|t| t.eq_ignore_ascii_case("am/pm"))
        {
            return true;
        }
        i += c.len_utf8();
    }
    false
}

fn upper_designator(text: &str, am: bool) -> String {
    let (low, up) = if am { ("am", "AM") } else { ("pm", "PM") };
    match text.rfind(low) {
        Some(i) => format!("{}{}{}", &text[..i], up, &text[i + 2..]),
        None => text.to_string(),
    }
}

fn encode_ansi(s: &str) -> Vec<u8> {
    let mut out = Vec::new();
    for ch in s.chars() {
        let cp = ch as u32;
        if cp < 0x80 || (0xA0..=0xFF).contains(&cp) {
            out.push(cp as u8);
        } else if let Some(i) = CP1252_HIGH
            .iter()
            .position(|&h| h == ch && !('\u{80}'..='\u{9F}').contains(&h))
        {
            out.push(0x80 + i as u8);
        } else {
            out.extend(std::iter::repeat_n(b'?', ch.len_utf16()));
        }
    }
    out
}
