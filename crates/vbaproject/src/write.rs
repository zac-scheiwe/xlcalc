use crate::cfb::CfbBuilder;
use crate::{cp1252, ovba, ModuleKind, VbaError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleSource {
    pub name: String,
    pub kind: ModuleKind,

    pub source: String,
}

pub const CODE_PAGE: u16 = 1252;

pub fn header(kind: ModuleKind, name: &str, document_base: Option<&str>) -> String {
    let mut s = format!("Attribute VB_Name = \"{name}\"\r\n");
    let (base, predeclared, exposed, customizable) = match kind {
        ModuleKind::Standard => return s,
        ModuleKind::Class => (
            "{FCFB3D2A-A0FA-1068-A738-08002B3371B5}",
            false,
            false,
            false,
        ),
        ModuleKind::Document => (
            document_base.unwrap_or("{00020820-0000-0000-C000-000000000046}"),
            true,
            true,
            true,
        ),
    };
    let b = |v: bool| if v { "True" } else { "False" };
    s.push_str(&format!("Attribute VB_Base = \"0{base}\"\r\n"));
    s.push_str("Attribute VB_GlobalNameSpace = False\r\n");
    s.push_str("Attribute VB_Creatable = False\r\n");
    s.push_str(&format!(
        "Attribute VB_PredeclaredId = {}\r\n",
        b(predeclared)
    ));
    s.push_str(&format!("Attribute VB_Exposed = {}\r\n", b(exposed)));
    s.push_str("Attribute VB_TemplateDerived = False\r\n");
    s.push_str(&format!(
        "Attribute VB_Customizable = {}\r\n",
        b(customizable)
    ));
    s
}

fn encode(text: &str) -> Result<Vec<u8>, VbaError> {
    text.chars()
        .map(|c| {
            if (c as u32) < 0x80 {
                return Ok(c as u8);
            }
            (0x80u8..=0xFF)
                .find(|&b| cp1252(b) == c)
                .ok_or_else(|| VbaError::BadText(format!("{c:?} is not in code page 1252")))
        })
        .collect()
}

fn utf16(text: &str) -> Vec<u8> {
    text.encode_utf16().flat_map(u16::to_le_bytes).collect()
}

fn record(out: &mut Vec<u8>, id: u16, data: &[u8]) {
    out.extend(id.to_le_bytes());
    out.extend((data.len() as u32).to_le_bytes());
    out.extend(data);
}

fn reference(out: &mut Vec<u8>, name: &str, libid: &str) {
    record(out, 0x0016, name.as_bytes());
    record(out, 0x003E, &utf16(name));
    let mut d = Vec::new();
    d.extend((libid.len() as u32).to_le_bytes());
    d.extend(libid.as_bytes());
    d.extend(0u32.to_le_bytes());
    d.extend(0u16.to_le_bytes());
    record(out, 0x000D, &d);
}

fn dir_stream(name: &str, modules: &[ModuleSource]) -> Result<Vec<u8>, VbaError> {
    let mut d = Vec::new();
    record(&mut d, 0x0001, &3u32.to_le_bytes());
    record(&mut d, 0x004A, &6u32.to_le_bytes());
    record(&mut d, 0x0002, &0x409u32.to_le_bytes());
    record(&mut d, 0x0014, &0x409u32.to_le_bytes());
    record(&mut d, 0x0003, &CODE_PAGE.to_le_bytes());
    record(&mut d, 0x0004, &encode(name)?);
    record(&mut d, 0x0005, &[]);
    record(&mut d, 0x0040, &[]);
    record(&mut d, 0x0006, &[]);
    record(&mut d, 0x003D, &[]);
    record(&mut d, 0x0007, &0u32.to_le_bytes());
    record(&mut d, 0x0008, &0u32.to_le_bytes());

    d.extend(0x0009u16.to_le_bytes());
    d.extend(4u32.to_le_bytes());
    d.extend(1u32.to_le_bytes());
    d.extend(0u16.to_le_bytes());
    record(&mut d, 0x000C, &[]);
    record(&mut d, 0x003C, &[]);
    reference(
        &mut d,
        "stdole",
        "*\\G{00020430-0000-0000-C000-000000000046}#2.0#0#C:\\Windows\\System32\\stdole2.tlb#OLE Automation",
    );
    reference(
        &mut d,
        "Office",
        "*\\G{2DF8D04C-5BFA-101B-BDE5-00AA0044DE52}#2.0#0#C:\\Program Files\\Common Files\\Microsoft Shared\\OFFICE16\\MSO.DLL#Microsoft Office 16.0 Object Library",
    );
    record(&mut d, 0x000F, &(modules.len() as u16).to_le_bytes());
    record(&mut d, 0x0013, &0xFFFFu16.to_le_bytes());
    for m in modules {
        record(&mut d, 0x0019, &encode(&m.name)?);
        record(&mut d, 0x0047, &utf16(&m.name));
        record(&mut d, 0x001A, &encode(&m.name)?);
        record(&mut d, 0x0032, &utf16(&m.name));
        record(&mut d, 0x001C, &[]);
        record(&mut d, 0x0048, &[]);
        record(&mut d, 0x0031, &0u32.to_le_bytes());
        record(&mut d, 0x001E, &0u32.to_le_bytes());
        record(&mut d, 0x002C, &0xFFFFu16.to_le_bytes());
        record(
            &mut d,
            if m.kind == ModuleKind::Standard {
                0x0021
            } else {
                0x0022
            },
            &[],
        );
        record(&mut d, 0x002B, &[]);
    }
    record(&mut d, 0x0010, &[]);
    Ok(d)
}

fn project_key(guid: &str) -> u8 {
    guid.bytes().fold(0u8, u8::wrapping_add)
}

fn protected(seed: u8, key: u8, data: &[u8]) -> String {
    let ignored = usize::from((seed & 6) >> 1);
    let mut plain = vec![0x16; ignored];
    plain.extend((data.len() as u32).to_le_bytes());
    plain.extend(data);
    let mut enc = vec![seed, seed ^ 2, seed ^ key];
    let mut prev = key;
    for u in plain {
        let e = u ^ enc[enc.len() - 2].wrapping_add(prev);
        enc.push(e);
        prev = u;
    }
    enc.iter().map(|b| format!("{b:02X}")).collect()
}

fn project_guid(name: &str, modules: &[ModuleSource]) -> String {
    let mut h: [u64; 2] = [0xcbf2_9ce4_8422_2325, 0x8422_2325_cbf2_9ce4];
    let mut feed = |b: &[u8]| {
        for &x in b {
            for (i, s) in h.iter_mut().enumerate() {
                *s ^= u64::from(x) + i as u64;
                *s = s.wrapping_mul(0x0000_0100_0000_01b3);
            }
        }
    };
    feed(name.as_bytes());
    for m in modules {
        feed(m.name.as_bytes());
        feed(m.source.as_bytes());
    }
    let (a, b) = (h[0], h[1]);
    format!(
        "{{{:08X}-{:04X}-4{:03X}-{:04X}-{:012X}}}",
        (a >> 32) as u32,
        (a >> 16) as u16,
        a as u16 & 0xFFF,
        0x8000 | (b >> 48) as u16 & 0x3FFF,
        b & 0xFFFF_FFFF_FFFF
    )
}

fn project_stream(name: &str, modules: &[ModuleSource]) -> Result<Vec<u8>, VbaError> {
    let guid = project_guid(name, modules);
    let key = project_key(&guid);
    let mut s = format!("ID=\"{guid}\"\r\n");
    for m in modules {
        let line = match m.kind {
            ModuleKind::Document => format!("Document={}/&H00000000", m.name),
            ModuleKind::Class => format!("Class={}", m.name),
            ModuleKind::Standard => format!("Module={}", m.name),
        };
        s.push_str(&line);
        s.push_str("\r\n");
    }
    s.push_str(&format!("Name=\"{name}\"\r\n"));
    s.push_str("HelpContextID=\"0\"\r\n");
    s.push_str("VersionCompatible32=\"393222000\"\r\n");
    s.push_str(&format!(
        "CMG=\"{}\"\r\nDPB=\"{}\"\r\nGC=\"{}\"\r\n",
        protected(0x16, key, &0u32.to_le_bytes()),
        protected(0x2C, key, &[0]),
        protected(0x42, key, &[0xFF]),
    ));
    s.push_str("\r\n[Host Extender Info]\r\n");
    s.push_str("&H00000001={3832D640-CF90-11CF-8E43-00A0C911005A};VBE;&H00000000\r\n");
    s.push_str("\r\n[Workspace]\r\n");
    for m in modules {
        let tail = if m.kind == ModuleKind::Document {
            "C"
        } else {
            ""
        };
        s.push_str(&format!("{}=0, 0, 0, 0, {tail}\r\n", m.name));
    }
    encode(&s)
}

pub fn build(name: &str, modules: &[ModuleSource]) -> Result<Vec<u8>, VbaError> {
    let mut seen: Vec<String> = Vec::new();
    for m in modules {
        let lower = m.name.to_lowercase();
        if seen.contains(&lower) {
            return Err(VbaError::Dir(format!("module {} twice", m.name)));
        }
        seen.push(lower);
    }
    let mut cfb = CfbBuilder::new();

    let mut wm = Vec::new();
    for m in modules {
        wm.extend(encode(&m.name)?);
        wm.push(0);
        wm.extend(utf16(&m.name));
        wm.extend([0, 0]);
    }
    wm.extend([0, 0]);
    cfb.add_stream("PROJECT", project_stream(name, modules)?);
    cfb.add_stream("PROJECTwm", wm);
    cfb.add_stream("VBA/dir", ovba::compress(&dir_stream(name, modules)?));
    cfb.add_stream("VBA/_VBA_PROJECT", vec![0xCC, 0x61, 0xFF, 0xFF, 0, 0, 0]);
    for m in modules {
        cfb.add_stream(
            &format!("VBA/{}", m.name),
            ovba::compress(&encode(&m.source)?),
        );
    }
    cfb.finish()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::VbaProject;

    fn module(name: &str, kind: ModuleKind, body: &str) -> ModuleSource {
        let doc =
            (kind == ModuleKind::Document).then_some("{00020820-0000-0000-C000-000000000046}");
        ModuleSource {
            name: name.into(),
            kind,
            source: format!("{}{body}", header(kind, name, doc)),
        }
    }

    #[test]
    fn written_project_reads_back() {
        let modules = vec![
            module("ThisWorkbook", ModuleKind::Document, ""),
            module("Sheet1", ModuleKind::Document, "Option Explicit\r\n"),
            module(
                "Mod1",
                ModuleKind::Standard,
                "Sub A()\r\n    x = \"caf\u{e9}\"\r\nEnd Sub\r\n",
            ),
            module("Cls", ModuleKind::Class, "Public X As Long\r\n"),
        ];
        let bytes = build("VBAProject", &modules).unwrap();
        let p = VbaProject::parse(bytes).unwrap();
        assert_eq!(p.name, "VBAProject");
        assert_eq!(p.code_page, 1252);
        assert_eq!(p.modules.len(), 4);
        for (m, w) in p.modules.iter().zip(&modules) {
            assert_eq!(m.name, w.name);
            assert_eq!(m.kind, w.kind);
            assert_eq!(m.source.as_ref().unwrap(), &w.source);
        }
        assert_eq!(
            p.modules[2].code().unwrap(),
            "Sub A()\r\n    x = \"caf\u{e9}\"\r\nEnd Sub\r\n"
        );
    }

    #[test]
    fn large_module_round_trips() {
        let body: String = (0..2000)
            .map(|i| format!("    Cells({i}, 1).Value = {}\r\n", i * 7919 % 1000))
            .collect();
        let m = module("Big", ModuleKind::Standard, &body);
        let p = VbaProject::parse(build("P", std::slice::from_ref(&m)).unwrap()).unwrap();
        assert_eq!(p.modules[0].source.as_ref().unwrap(), &m.source);
    }

    #[test]
    fn protection_fields_match_the_ones_excel_wrote() {
        let key = project_key("{E3334A0A-DD66-41AA-8F7F-C168D8BFA15D}");
        assert_eq!(
            protected(0x16, key, &[0; 4]),
            "1614013D01451349134913491349"
        );
        assert_eq!(protected(0x2C, key, &[0]), "2C2E3B53476848684868");
        assert_eq!(protected(0x42, key, &[0xFF]), "424055416A426A4295");
    }

    #[test]
    fn rejects_text_outside_the_code_page() {
        let m = module("M", ModuleKind::Standard, "' \u{4e2d}\r\n");
        assert!(build("P", &[m]).is_err());
    }

    #[test]
    fn rejects_a_module_twice() {
        let a = module("M", ModuleKind::Standard, "");
        let b = module("m", ModuleKind::Class, "");
        assert!(build("P", &[a, b]).is_err());
    }
}
