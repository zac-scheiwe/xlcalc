use windows::core::{HSTRING, PCWSTR};
use windows::Win32::Foundation::ERROR_FILE_NOT_FOUND;
use windows::Win32::System::LibraryLoader::GetModuleFileNameW;
use windows::Win32::System::Registry::{
    RegCloseKey, RegCreateKeyExW, RegDeleteTreeW, RegSetValueExW, HKEY, HKEY_CURRENT_USER,
    KEY_WRITE, REG_OPTION_NON_VOLATILE, REG_SZ,
};

use crate::server::CLSID;

pub(crate) const PROGID: &str = "XlCalc.Application";
const DESCRIPTION: &str = "XlCalc Application";

fn clsid_text() -> String {
    format!("{{{:?}}}", CLSID)
}

fn set(path: &str, value: &str) -> windows::core::Result<()> {
    let mut key = HKEY::default();
    unsafe {
        RegCreateKeyExW(
            HKEY_CURRENT_USER,
            &HSTRING::from(format!("Software\\Classes\\{path}")),
            None,
            PCWSTR::null(),
            REG_OPTION_NON_VOLATILE,
            KEY_WRITE,
            None,
            &mut key,
            None,
        )
        .ok()?;
        let data: Vec<u8> = value
            .encode_utf16()
            .chain([0])
            .flat_map(u16::to_le_bytes)
            .collect();
        let r = RegSetValueExW(key, PCWSTR::null(), None, REG_SZ, Some(&data)).ok();
        let _ = RegCloseKey(key);
        r
    }
}

fn delete(path: &str) -> windows::core::Result<()> {
    let r = unsafe {
        RegDeleteTreeW(
            HKEY_CURRENT_USER,
            &HSTRING::from(format!("Software\\Classes\\{path}")),
        )
    };
    if r.is_ok() || r == ERROR_FILE_NOT_FOUND {
        Ok(())
    } else {
        r.ok()
    }
}

fn exe_path() -> String {
    let mut buf = vec![0u16; 32768];
    let n = unsafe { GetModuleFileNameW(None, &mut buf) } as usize;
    String::from_utf16_lossy(&buf[..n])
}

pub(crate) fn register(open_macros: bool, excel_books: bool) -> windows::core::Result<()> {
    let clsid = clsid_text();
    set(PROGID, DESCRIPTION)?;
    set(&format!("{PROGID}\\CLSID"), &clsid)?;
    set(&format!("CLSID\\{clsid}"), DESCRIPTION)?;
    set(
        &format!("CLSID\\{clsid}\\LocalServer32"),
        &format!(
            "\"{}\" /automation{}{}",
            exe_path(),
            if open_macros { " /openmacros" } else { "" },
            if excel_books { " /excelbooks" } else { "" }
        ),
    )?;
    set(&format!("CLSID\\{clsid}\\ProgID"), PROGID)
}

pub(crate) fn unregister() -> windows::core::Result<()> {
    delete(PROGID)?;
    delete(&format!("CLSID\\{}", clsid_text()))
}
