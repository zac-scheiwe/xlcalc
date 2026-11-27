#![cfg(windows)]

use xlcalc_stack::grow;
use xlcalc_variant::{ObjectId, Variant};
use xlcalc_winbridge::{call_function, create, proxy, Server};

fn here() -> usize {
    let x = 0u8;
    std::hint::black_box(std::ptr::addr_of!(x)) as usize
}

#[derive(Default)]
struct Where(Vec<usize>);

impl proxy::Callback for Where {
    fn ids(&mut self, _: ObjectId, _: &str) -> Result<i32, i32> {
        Ok(1)
    }

    fn invoke(
        &mut self,
        _: ObjectId,
        _: i32,
        _: u16,
        _: u32,
        _: &[Variant],
    ) -> Result<Variant, i32> {
        self.0.push(here());

        grow(1 << 20, || {
            xlcalc_stack::on_thread_stack(|| self.0.push(here()))
        });
        Ok(Variant::Bstr("$A$1".into()))
    }
}

fn run(server: &Server, name: &str, args: &[Variant], cb: &mut Where) -> Option<Variant> {
    call_function(server.dispatch(), name, args, cb).ok()
}

#[test]
fn managed_server_runs_on_the_thread_stack() {
    let start = here();
    let Ok(server) = create("Xlc.TestFuncs") else {
        eprintln!("skipped: Xlc.TestFuncs is not registered");
        return;
    };
    let mut cb = Where::default();

    fn nested(depth: u32, server: &Server, cb: &mut Where) {
        grow(8 << 20, || {
            assert_eq!(run(server, "Boom", &[], cb), None);
            let range = Variant::Dispatch(Some(ObjectId(1)));
            let got = run(
                server,
                "Prop",
                &[range, Variant::Bstr("Address".into())],
                cb,
            );
            assert_eq!(got, Some(Variant::Bstr("$A$1".into())));
            if depth > 0 {
                nested(depth - 1, server, cb);
            }
        });
    }
    nested(7, &server, &mut cb);
    drop(server);
    assert!(!cb.0.is_empty());
    for at in cb.0 {
        assert!(at.abs_diff(start) < 512 * 1024, "{at:#x} {start:#x}");
    }
}
