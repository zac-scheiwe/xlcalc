use std::collections::HashSet;

const BELOW_485: [&str; 3] = ["ISO.CEILING", "NETWORKDAYS.INTL", "WORKDAY.INTL"];

#[test]
fn xlfn_list_is_the_function_index_rule() {
    let binary: HashSet<&str> = (0u16..485).filter_map(xlcalc_xlsx::function_name).collect();
    for name in xlcalc::function_names() {
        if name.starts_with('\u{1}') {
            continue;
        }
        let future = !binary.contains(name) && !BELOW_485.contains(&name);
        let prefix = xlcalc::file_prefix(name);
        let want = if matches!(name, "FILTER" | "SORT") {
            "_xlfn._xlws."
        } else if future {
            "_xlfn."
        } else {
            ""
        };
        assert_eq!(prefix, want, "{name}");
    }
}
