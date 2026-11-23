use crate::package::Package;
use crate::rawxml::{self, escape_attr};
use crate::xml::{self, Node};
use xlcalc::{Rect, SheetId};

const ALLOW_ATTRS: [&str; 11] = [
    "formatCells",
    "formatColumns",
    "formatRows",
    "insertColumns",
    "insertRows",
    "insertHyperlinks",
    "deleteColumns",
    "deleteRows",
    "sort",
    "autoFilter",
    "pivotTables",
];

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PassHash {
    Legacy(u16),

    Salted {
        algorithm: String,
        hash: String,
        salt: String,
        spin: u32,
    },
}

impl PassHash {
    pub fn matches(&self, given: &str) -> bool {
        match self {
            PassHash::Legacy(h) => legacy_hash(given) == *h,
            PassHash::Salted {
                algorithm,
                hash,
                salt,
                spin,
            } => {
                if !algorithm.eq_ignore_ascii_case("SHA-512") {
                    return false;
                }
                let (Some(want), Some(salt)) = (base64_decode(hash), base64_decode(salt)) else {
                    return false;
                };
                let mut buf = salt;
                buf.extend(given.encode_utf16().flat_map(u16::to_le_bytes));
                let mut h = sha512(&buf).to_vec();
                for i in 0..*spin {
                    h.extend_from_slice(&i.to_le_bytes());
                    h = sha512(&h).to_vec();
                }
                h == want
            }
        }
    }

    fn attrs(&self, prefix: &str) -> String {
        let name = |n: &str| {
            if prefix.is_empty() {
                n.to_string()
            } else {
                let mut c = n.chars();
                let first = c.next().map(|f| f.to_ascii_uppercase());
                format!("{prefix}{}{}", first.unwrap_or_default(), c.as_str())
            }
        };
        match self {
            PassHash::Legacy(h) => format!(" {}=\"{h:04X}\"", name("password")),
            PassHash::Salted {
                algorithm,
                hash,
                salt,
                spin,
            } => format!(
                " {}=\"{}\" {}=\"{}\" {}=\"{}\" {}=\"{spin}\"",
                name("algorithmName"),
                escape_attr(algorithm),
                name("hashValue"),
                escape_attr(hash),
                name("saltValue"),
                escape_attr(salt),
                name("spinCount"),
            ),
        }
    }

    fn read(n: &Node, prefix: &str) -> Option<PassHash> {
        let key = |k: &str| {
            if prefix.is_empty() {
                k.to_string()
            } else {
                let mut c = k.chars();
                let first = c.next().map(|f| f.to_ascii_uppercase());
                format!("{prefix}{}{}", first.unwrap_or_default(), c.as_str())
            }
        };
        if let (Some(a), Some(h), Some(s)) = (
            n.attr(&key("algorithmName")),
            n.attr(&key("hashValue")),
            n.attr(&key("saltValue")),
        ) {
            return Some(PassHash::Salted {
                algorithm: a.to_string(),
                hash: h.to_string(),
                salt: s.to_string(),
                spin: n
                    .attr(&key("spinCount"))
                    .and_then(|v| v.parse().ok())
                    .unwrap_or(0),
            });
        }
        let h = n.attr(&key("password"))?;
        u16::from_str_radix(h, 16).ok().map(PassHash::Legacy)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SheetProtection {
    pub contents: bool,
    pub objects: bool,
    pub scenarios: bool,
    pub allow: [bool; 11],
    pub password: Option<PassHash>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EditRangeDef {
    pub title: String,
    pub rects: Vec<Rect>,
    pub password: Option<PassHash>,
}

fn flag(n: &Node, k: &str, default: bool) -> bool {
    match n.attr(k) {
        Some("1" | "true") => true,
        Some("0" | "false") => false,
        _ => default,
    }
}

fn kid(pkg: &Package, part: &str, name: &str) -> Option<Node> {
    let split = rawxml::split(pkg.part(part)?).ok()?;
    let k = split.kids.into_iter().find(|k| k.name == name)?;
    xml::parse(k.raw.as_bytes()).ok()
}

pub fn read_sheet_protection(
    pkg: &Package,
    sheet: SheetId,
) -> (Option<SheetProtection>, Vec<EditRangeDef>) {
    let Some(src) = pkg.source_sheet(sheet) else {
        return (None, Vec::new());
    };
    let Some(split) = pkg.part(&src.part).and_then(|b| rawxml::split(b).ok()) else {
        return (None, Vec::new());
    };
    let node = |name: &str| {
        split
            .kids
            .iter()
            .find(|k| k.name == name)
            .and_then(|k| xml::parse(k.raw.as_bytes()).ok())
    };
    let prot = node("sheetProtection").map(|n| {
        let mut allow = [false; 11];
        for (a, k) in allow.iter_mut().zip(ALLOW_ATTRS) {
            *a = !flag(&n, k, true);
        }
        SheetProtection {
            contents: flag(&n, "sheet", false),
            objects: flag(&n, "objects", false),
            scenarios: flag(&n, "scenarios", false),
            allow,
            password: PassHash::read(&n, ""),
        }
    });
    let ranges = node("protectedRanges")
        .map(|n| {
            n.children("protectedRange")
                .filter_map(|r| {
                    let rects: Vec<Rect> = r
                        .attr("sqref")?
                        .split_whitespace()
                        .filter_map(crate::parse_rect)
                        .collect();
                    Some(EditRangeDef {
                        title: r.attr("name").unwrap_or("").to_string(),
                        rects,
                        password: PassHash::read(r, ""),
                    })
                })
                .collect()
        })
        .unwrap_or_default();
    (prot, ranges)
}

pub fn read_book_protection(pkg: &Package) -> Option<Option<PassHash>> {
    let n = kid(pkg, "xl/workbook.xml", "workbookProtection")?;
    flag(&n, "lockStructure", false).then(|| PassHash::read(&n, "workbook"))
}

pub fn sheet_protection_xml(p: &SheetProtection) -> String {
    let mut s = String::from("<sheetProtection");
    if let Some(h) = &p.password {
        s.push_str(&h.attrs(""));
    }
    for (on, k) in [
        (p.contents, "sheet"),
        (p.objects, "objects"),
        (p.scenarios, "scenarios"),
    ] {
        if on {
            s.push_str(&format!(" {k}=\"1\""));
        }
    }
    for (allowed, k) in p.allow.iter().zip(ALLOW_ATTRS) {
        if *allowed {
            s.push_str(&format!(" {k}=\"0\""));
        }
    }
    s.push_str("/>");
    s
}

pub fn protected_ranges_xml(ranges: &[EditRangeDef]) -> Option<String> {
    if ranges.is_empty() {
        return None;
    }
    let mut s = String::from("<protectedRanges>");
    for r in ranges {
        s.push_str("<protectedRange");
        if let Some(h) = &r.password {
            s.push_str(&h.attrs(""));
        }
        let sqref: Vec<String> = r.rects.iter().map(crate::write::rect_text).collect();
        s.push_str(&format!(
            " sqref=\"{}\" name=\"{}\"/>",
            sqref.join(" "),
            escape_attr(&r.title)
        ));
    }
    s.push_str("</protectedRanges>");
    Some(s)
}

pub fn book_protection_xml(password: Option<&PassHash>) -> String {
    let pw = password.map(|h| h.attrs("workbook")).unwrap_or_default();
    format!("<workbookProtection{pw} lockStructure=\"1\"/>")
}

pub fn legacy_hash(password: &str) -> u16 {
    let bytes: Vec<u16> = password
        .encode_utf16()
        .map(|c| if c < 0x100 { c } else { c & 0xff })
        .collect();
    let mut h: u16 = 0;
    for &c in bytes.iter().rev() {
        h = ((h >> 14) & 1) | ((h << 1) & 0x7fff);
        h ^= c;
    }
    h = ((h >> 14) & 1) | ((h << 1) & 0x7fff);
    h ^= bytes.len() as u16;
    h ^ 0xCE4B
}

fn base64_decode(s: &str) -> Option<Vec<u8>> {
    let val = |c: u8| -> Option<u32> {
        Some(match c {
            b'A'..=b'Z' => u32::from(c - b'A'),
            b'a'..=b'z' => u32::from(c - b'a') + 26,
            b'0'..=b'9' => u32::from(c - b'0') + 52,
            b'+' => 62,
            b'/' => 63,
            _ => return None,
        })
    };
    let mut out = Vec::new();
    let (mut acc, mut bits) = (0u32, 0u32);
    for c in s.bytes().filter(|c| !c.is_ascii_whitespace() && *c != b'=') {
        acc = (acc << 6) | val(c)?;
        bits += 6;
        if bits >= 8 {
            bits -= 8;
            out.push((acc >> bits) as u8);
            acc &= (1 << bits) - 1;
        }
    }
    Some(out)
}

const K: [u64; 80] = [
    0x428a_2f98_d728_ae22,
    0x7137_4491_23ef_65cd,
    0xb5c0_fbcf_ec4d_3b2f,
    0xe9b5_dba5_8189_dbbc,
    0x3956_c25b_f348_b538,
    0x59f1_11f1_b605_d019,
    0x923f_82a4_af19_4f9b,
    0xab1c_5ed5_da6d_8118,
    0xd807_aa98_a303_0242,
    0x1283_5b01_4570_6fbe,
    0x2431_85be_4ee4_b28c,
    0x550c_7dc3_d5ff_b4e2,
    0x72be_5d74_f27b_896f,
    0x80de_b1fe_3b16_96b1,
    0x9bdc_06a7_25c7_1235,
    0xc19b_f174_cf69_2694,
    0xe49b_69c1_9ef1_4ad2,
    0xefbe_4786_384f_25e3,
    0x0fc1_9dc6_8b8c_d5b5,
    0x240c_a1cc_77ac_9c65,
    0x2de9_2c6f_592b_0275,
    0x4a74_84aa_6ea6_e483,
    0x5cb0_a9dc_bd41_fbd4,
    0x76f9_88da_8311_53b5,
    0x983e_5152_ee66_dfab,
    0xa831_c66d_2db4_3210,
    0xb003_27c8_98fb_213f,
    0xbf59_7fc7_beef_0ee4,
    0xc6e0_0bf3_3da8_8fc2,
    0xd5a7_9147_930a_a725,
    0x06ca_6351_e003_826f,
    0x1429_2967_0a0e_6e70,
    0x27b7_0a85_46d2_2ffc,
    0x2e1b_2138_5c26_c926,
    0x4d2c_6dfc_5ac4_2aed,
    0x5338_0d13_9d95_b3df,
    0x650a_7354_8baf_63de,
    0x766a_0abb_3c77_b2a8,
    0x81c2_c92e_47ed_aee6,
    0x9272_2c85_1482_353b,
    0xa2bf_e8a1_4cf1_0364,
    0xa81a_664b_bc42_3001,
    0xc24b_8b70_d0f8_9791,
    0xc76c_51a3_0654_be30,
    0xd192_e819_d6ef_5218,
    0xd699_0624_5565_a910,
    0xf40e_3585_5771_202a,
    0x106a_a070_32bb_d1b8,
    0x19a4_c116_b8d2_d0c8,
    0x1e37_6c08_5141_ab53,
    0x2748_774c_df8e_eb99,
    0x34b0_bcb5_e19b_48a8,
    0x391c_0cb3_c5c9_5a63,
    0x4ed8_aa4a_e341_8acb,
    0x5b9c_ca4f_7763_e373,
    0x682e_6ff3_d6b2_b8a3,
    0x748f_82ee_5def_b2fc,
    0x78a5_636f_4317_2f60,
    0x84c8_7814_a1f0_ab72,
    0x8cc7_0208_1a64_39ec,
    0x90be_fffa_2363_1e28,
    0xa450_6ceb_de82_bde9,
    0xbef9_a3f7_b2c6_7915,
    0xc671_78f2_e372_532b,
    0xca27_3ece_ea26_619c,
    0xd186_b8c7_21c0_c207,
    0xeada_7dd6_cde0_eb1e,
    0xf57d_4f7f_ee6e_d178,
    0x06f0_67aa_7217_6fba,
    0x0a63_7dc5_a2c8_98a6,
    0x113f_9804_bef9_0dae,
    0x1b71_0b35_131c_471b,
    0x28db_77f5_2304_7d84,
    0x32ca_ab7b_40c7_2493,
    0x3c9e_be0a_15c9_bebc,
    0x431d_67c4_9c10_0d4c,
    0x4cc5_d4be_cb3e_42b6,
    0x597f_299c_fc65_7e2a,
    0x5fcb_6fab_3ad6_faec,
    0x6c44_198c_4a47_5817,
];

fn sha512(data: &[u8]) -> [u8; 64] {
    let mut h: [u64; 8] = [
        0x6a09_e667_f3bc_c908,
        0xbb67_ae85_84ca_a73b,
        0x3c6e_f372_fe94_f82b,
        0xa54f_f53a_5f1d_36f1,
        0x510e_527f_ade6_82d1,
        0x9b05_688c_2b3e_6c1f,
        0x1f83_d9ab_fb41_bd6b,
        0x5be0_cd19_137e_2179,
    ];
    let mut msg = data.to_vec();
    msg.push(0x80);
    while msg.len() % 128 != 112 {
        msg.push(0);
    }
    msg.extend_from_slice(&((data.len() as u128) * 8).to_be_bytes());
    for block in msg.as_chunks::<128>().0 {
        let mut w = [0u64; 80];
        for (i, c) in block.as_chunks::<8>().0.iter().enumerate() {
            w[i] = u64::from_be_bytes(*c);
        }
        for i in 16..80 {
            let s0 = w[i - 15].rotate_right(1) ^ w[i - 15].rotate_right(8) ^ (w[i - 15] >> 7);
            let s1 = w[i - 2].rotate_right(19) ^ w[i - 2].rotate_right(61) ^ (w[i - 2] >> 6);
            w[i] = w[i - 16]
                .wrapping_add(s0)
                .wrapping_add(w[i - 7])
                .wrapping_add(s1);
        }
        let mut v = h;
        for i in 0..80 {
            let [a, b, c, d, e, f, g, hh] = v;
            let s1 = e.rotate_right(14) ^ e.rotate_right(18) ^ e.rotate_right(41);
            let ch = (e & f) ^ (!e & g);
            let t1 = hh
                .wrapping_add(s1)
                .wrapping_add(ch)
                .wrapping_add(K[i])
                .wrapping_add(w[i]);
            let s0 = a.rotate_right(28) ^ a.rotate_right(34) ^ a.rotate_right(39);
            let maj = (a & b) ^ (a & c) ^ (b & c);
            let t2 = s0.wrapping_add(maj);
            v = [t1.wrapping_add(t2), a, b, c, d.wrapping_add(t1), e, f, g];
        }
        for (x, y) in h.iter_mut().zip(v) {
            *x = x.wrapping_add(y);
        }
    }
    let mut out = [0u8; 64];
    for (o, x) in out.as_chunks_mut::<8>().0.iter_mut().zip(h) {
        *o = x.to_be_bytes();
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sha512_matches_the_standard_vector() {
        let h = sha512(b"abc");
        let hex: String = h.iter().map(|b| format!("{b:02x}")).collect();
        assert_eq!(
            hex,
            "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a\
             2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
        );
    }

    #[test]
    fn legacy_hash_of_known_passwords() {
        assert_eq!(legacy_hash("password"), 0x83AF);
        assert!(PassHash::Legacy(legacy_hash("pW")).matches("pW"));
        assert!(!PassHash::Legacy(legacy_hash("pW")).matches("pw"));
    }

    #[test]
    fn salted_hash_checks_the_password() {
        let salt = vec![1u8, 2, 3, 4];
        let mut buf = salt.clone();
        buf.extend("ab".encode_utf16().flat_map(u16::to_le_bytes));
        let mut h = sha512(&buf).to_vec();
        for i in 0..2u32 {
            h.extend_from_slice(&i.to_le_bytes());
            h = sha512(&h).to_vec();
        }
        let enc = |v: &[u8]| {
            const A: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
            let mut s = String::new();
            for c in v.chunks(3) {
                let n = (u32::from(c[0]) << 16)
                    | (u32::from(*c.get(1).unwrap_or(&0)) << 8)
                    | u32::from(*c.get(2).unwrap_or(&0));
                for k in 0..4 {
                    if k <= c.len() {
                        s.push(A[((n >> (18 - 6 * k)) & 63) as usize] as char);
                    } else {
                        s.push('=');
                    }
                }
            }
            s
        };
        let p = PassHash::Salted {
            algorithm: "SHA-512".into(),
            hash: enc(&h),
            salt: enc(&salt),
            spin: 2,
        };
        assert!(p.matches("ab"));
        assert!(!p.matches("ac"));
    }
}
