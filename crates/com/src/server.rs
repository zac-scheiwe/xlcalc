use windows::core::GUID;
use windows::Win32::System::Com::{
    CoAddRefServerProcess, CoInitializeEx, CoRegisterClassObject, CoReleaseServerProcess,
    CoResumeClassObjects, CoRevokeClassObject, CoUninitialize, CLSCTX_LOCAL_SERVER,
    COINIT_APARTMENTTHREADED, REGCLS_SINGLEUSE, REGCLS_SUSPENDED,
};
use windows::Win32::UI::WindowsAndMessaging::{
    DispatchMessageW, GetMessageW, PostQuitMessage, TranslateMessage, MSG,
};

use crate::dispatch;

pub(crate) const CLSID: GUID = GUID::from_u128(0x1d9a431f_cf92_4d0e_9b99_6898a01711a1);

pub(crate) fn object_created() {
    unsafe { CoAddRefServerProcess() };
}

pub(crate) fn object_released() {
    if unsafe { CoReleaseServerProcess() } == 0 {
        unsafe { PostQuitMessage(0) };
    }
}

pub(crate) fn run() -> windows::core::Result<()> {
    unsafe { CoInitializeEx(None, COINIT_APARTMENTTHREADED).ok()? };
    let factory = dispatch::factory();
    let cookie = unsafe {
        CoRegisterClassObject(
            &CLSID,
            &factory,
            CLSCTX_LOCAL_SERVER,
            REGCLS_SINGLEUSE | REGCLS_SUSPENDED,
        )?
    };
    unsafe { CoResumeClassObjects()? };
    let mut msg = MSG::default();
    while unsafe { GetMessageW(&mut msg, None, 0, 0) }.as_bool() {
        unsafe {
            let _ = TranslateMessage(&msg);
            DispatchMessageW(&msg);
        }
    }
    unsafe {
        let _ = CoRevokeClassObject(cookie);
    }
    drop(factory);
    unsafe { CoUninitialize() };
    Ok(())
}
