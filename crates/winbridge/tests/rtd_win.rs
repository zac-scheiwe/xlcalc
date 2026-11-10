#![cfg(windows)]

use xlcalc_variant::Variant;
use xlcalc_winbridge::rtd::RtdServer;

#[test]
fn start_connect_refresh() {
    if xlcalc_winbridge::create("Xlc.TestRtd").is_err() {
        eprintln!("skipped: Xlc.TestRtd is not registered");
        return;
    }
    let mut s = RtdServer::start("Xlc.TestRtd").expect("ServerStart");
    let (id, v) = s
        .connect(&["const".to_string(), "x".to_string()])
        .expect("ConnectData");
    assert_eq!(v, Variant::Bstr("c:x".into()));
    let (count, v) = s.connect(&["count".to_string()]).expect("ConnectData");
    assert_eq!(v, Variant::I4(0));
    let changed = s.refresh().expect("RefreshData");
    assert_eq!(changed, vec![(count, Variant::I4(1))]);
    s.disconnect(id);
    s.terminate();
}
