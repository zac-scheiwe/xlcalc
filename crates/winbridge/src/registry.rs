use windows::core::{HSTRING, PCWSTR, PWSTR};
use windows::Win32::System::Registry::{
    RegCloseKey, RegEnumKeyExW, RegEnumValueW, RegOpenKeyExW, RegQueryValueExW, HKEY,
    HKEY_CLASSES_ROOT, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE, KEY_READ, REG_VALUE_TYPE,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Registered {
    pub prog_id: String,
    pub description: String,
    pub friendly_name: String,

    pub load_behavior: u32,

    pub guid: String,
    pub per_user: bool,
}

const ADDINS: &str = r"Software\Microsoft\Office\Excel\Addins";

fn open(root: HKEY, path: &str) -> Option<HKEY> {
    let mut key = HKEY::default();
    let r = unsafe { RegOpenKeyExW(root, &HSTRING::from(path), None, KEY_READ, &mut key) };
    r.is_ok().then_some(key)
}

fn read(key: HKEY, name: &str) -> Option<(REG_VALUE_TYPE, Vec<u8>)> {
    let mut ty = REG_VALUE_TYPE::default();
    let mut len = 0u32;
    let n = HSTRING::from(name);
    unsafe {
        RegQueryValueExW(
            key,
            PCWSTR(n.as_ptr()),
            None,
            Some(&mut ty),
            None,
            Some(&mut len),
        )
        .ok()
        .ok()?;
        let mut buf = vec![0u8; len as usize];
        RegQueryValueExW(
            key,
            PCWSTR(n.as_ptr()),
            None,
            Some(&mut ty),
            Some(buf.as_mut_ptr()),
            Some(&mut len),
        )
        .ok()
        .ok()?;
        buf.truncate(len as usize);
        Some((ty, buf))
    }
}

fn read_string(key: HKEY, name: &str) -> String {
    match read(key, name) {
        Some((_, b)) => {
            let units: Vec<u16> = b
                .as_chunks::<2>()
                .0
                .iter()
                .map(|c| u16::from_le_bytes(*c))
                .take_while(|&u| u != 0)
                .collect();
            String::from_utf16_lossy(&units)
        }
        None => String::new(),
    }
}

fn read_dword(key: HKEY, name: &str) -> u32 {
    match read(key, name) {
        Some((_, b)) if b.len() >= 4 => u32::from_le_bytes([b[0], b[1], b[2], b[3]]),
        _ => 0,
    }
}

fn subkeys(key: HKEY) -> Vec<String> {
    let mut out = Vec::new();
    for i in 0.. {
        let mut buf = [0u16; 256];
        let mut len = buf.len() as u32;
        let r = unsafe {
            RegEnumKeyExW(
                key,
                i,
                Some(PWSTR(buf.as_mut_ptr())),
                &mut len,
                None,
                None,
                None,
                None,
            )
        };
        if r.is_err() {
            break;
        }
        out.push(String::from_utf16_lossy(&buf[..len as usize]));
    }
    out
}

pub fn class_guid(prog_id: &str) -> String {
    let Some(k) = open(HKEY_CLASSES_ROOT, &format!(r"{prog_id}\CLSID")) else {
        return String::new();
    };
    let g = read_string(k, "");
    unsafe {
        let _ = RegCloseKey(k);
    }
    g.to_uppercase()
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AutomationEntry {
    pub prog_id: String,

    pub file: String,

    pub class: String,
}

const MANAGER: &str = r"Software\Microsoft\Office\16.0\Excel\Add-in Manager";

fn value_names(key: HKEY) -> Vec<String> {
    let mut out = Vec::new();
    for i in 0.. {
        let mut buf = [0u16; 512];
        let mut len = buf.len() as u32;
        let r = unsafe {
            RegEnumValueW(
                key,
                i,
                Some(PWSTR(buf.as_mut_ptr())),
                &mut len,
                None,
                None,
                None,
                None,
            )
        };
        if r.is_err() {
            break;
        }
        out.push(String::from_utf16_lossy(&buf[..len as usize]));
    }
    out
}

pub fn automation_entries() -> Vec<AutomationEntry> {
    let Some(base) = open(HKEY_CURRENT_USER, MANAGER) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for name in value_names(base) {
        if name.contains(['\\', '/', ':']) {
            continue;
        }
        let guid = class_guid(&name);
        if guid.is_empty() {
            continue;
        }
        let mut file = String::new();
        let mut class = name.clone();
        if let Some(k) = open(HKEY_CLASSES_ROOT, &format!(r"CLSID\{guid}\InprocServer32")) {
            file = read_string(k, "");
            let c = read_string(k, "Class");
            if !c.is_empty() {
                class = c;
            }
            unsafe {
                let _ = RegCloseKey(k);
            }
        }
        let file = file.rsplit('\\').next().unwrap_or_default().to_string();
        out.push(AutomationEntry {
            prog_id: name,
            file,
            class,
        });
    }
    unsafe {
        let _ = RegCloseKey(base);
    }
    out
}

pub fn registered() -> Vec<Registered> {
    let mut out: Vec<Registered> = Vec::new();
    for (root, per_user) in [(HKEY_CURRENT_USER, true), (HKEY_LOCAL_MACHINE, false)] {
        let Some(base) = open(root, ADDINS) else {
            continue;
        };
        let mut names = subkeys(base);
        names.sort_by_key(|n| n.to_lowercase());
        for name in names {
            if out.iter().any(|r| r.prog_id.eq_ignore_ascii_case(&name)) {
                continue;
            }
            let Some(k) = open(base, &name) else { continue };
            out.push(Registered {
                description: read_string(k, "Description"),
                friendly_name: read_string(k, "FriendlyName"),
                load_behavior: read_dword(k, "LoadBehavior"),
                guid: class_guid(&name),
                prog_id: name,
                per_user,
            });
            unsafe {
                let _ = RegCloseKey(k);
            }
        }
        unsafe {
            let _ = RegCloseKey(base);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;

    fn reg(args: &[&str]) {
        let ok = Command::new("reg.exe")
            .args(args)
            .output()
            .is_ok_and(|o| o.status.success());
        assert!(ok, "reg {args:?}");
    }

    #[test]
    fn discovers_an_addin_key() {
        let key = format!(r"HKCU\{ADDINS}\XlcRegistryTest.Probe");
        reg(&["add", &key, "/v", "FriendlyName", "/d", "Probe name", "/f"]);
        reg(&["add", &key, "/v", "Description", "/d", "Probe text", "/f"]);
        reg(&[
            "add",
            &key,
            "/v",
            "LoadBehavior",
            "/t",
            "REG_DWORD",
            "/d",
            "3",
            "/f",
        ]);
        let found = registered()
            .into_iter()
            .find(|r| r.prog_id == "XlcRegistryTest.Probe");
        reg(&["delete", &key, "/f"]);
        let r = found.expect("the key is found");
        assert_eq!(r.friendly_name, "Probe name");
        assert_eq!(r.description, "Probe text");
        assert_eq!(r.load_behavior, 3);
        assert!(r.per_user);
        assert_eq!(r.guid, "");
    }
}
