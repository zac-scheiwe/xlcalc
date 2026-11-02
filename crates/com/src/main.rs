#[cfg(windows)]
mod dispatch;
#[cfg(windows)]
mod events;
#[cfg(windows)]
mod marshal;
#[cfg(windows)]
mod register;
#[cfg(windows)]
mod server;

#[cfg(windows)]
fn main() -> std::process::ExitCode {
    let args: Vec<String> = std::env::args()
        .skip(1)
        .map(|a| a.trim_start_matches(['/', '-']).to_ascii_lowercase())
        .collect();
    let open_macros = args.iter().any(|a| a == "openmacros");
    dispatch::set_open_macros(open_macros);
    let excel_books = args.iter().any(|a| a == "excelbooks");
    dispatch::set_excel_books(excel_books);
    let r = match args.first().map_or("", String::as_str) {
        "regserver" => register::register(open_macros, excel_books)
            .map(|()| println!("registered {}", register::PROGID)),
        "unregserver" => {
            register::unregister().map(|()| println!("unregistered {}", register::PROGID))
        }
        _ => server::run(),
    };
    match r {
        Ok(()) => std::process::ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("xlcalc-com: {e}");
            std::process::ExitCode::FAILURE
        }
    }
}

#[cfg(not(windows))]
fn main() -> std::process::ExitCode {
    eprintln!("xlcalc-com: COM servers run on Windows only");
    std::process::ExitCode::FAILURE
}
