#[cfg(windows)]
mod fiber;
#[cfg(windows)]
pub use fiber::{grow, maybe_grow, on_thread_stack};

#[cfg(not(windows))]
pub use stacker::{grow, maybe_grow};

#[cfg(not(windows))]
pub fn on_thread_stack<R>(f: impl FnOnce() -> R) -> R {
    f()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn here() -> usize {
        let x = 0u8;
        std::hint::black_box(std::ptr::addr_of!(x)) as usize
    }

    #[test]
    fn grows_and_returns_the_result() {
        fn depth(n: u32) -> u32 {
            maybe_grow(64 * 1024, 1024 * 1024, || {
                let pad = std::hint::black_box([1u8; 4096]);
                if n == 0 {
                    0
                } else {
                    depth(n - 1) + u32::from(pad[0])
                }
            })
        }
        assert_eq!(depth(5000), 5000);
    }

    #[test]
    fn panics_cross_both_ways() {
        let p = std::panic::catch_unwind(|| grow(1 << 20, || -> () { panic!("grown") }));
        assert_eq!(p.unwrap_err().downcast_ref::<&str>(), Some(&"grown"));
        grow(1 << 20, || {
            let p = std::panic::catch_unwind(|| on_thread_stack(|| -> () { panic!("back") }));
            assert_eq!(p.unwrap_err().downcast_ref::<&str>(), Some(&"back"));
            assert_eq!(on_thread_stack(|| 7), 7);
        });
    }

    #[cfg(windows)]
    #[test]
    fn on_thread_stack_leaves_every_grown_stack() {
        let start = here();
        let near = move || here().abs_diff(start) < 256 * 1024;
        let mut log = Vec::new();
        assert!(near());
        on_thread_stack(|| assert!(near()));
        grow(8 << 20, || {
            assert!(!near());
            grow(8 << 20, || {
                on_thread_stack(|| {
                    assert!(near());
                    log.push(1);
                    grow(8 << 20, || {
                        assert!(!near());
                        on_thread_stack(|| {
                            assert!(near());
                            log.push(2);
                        });
                        assert!(!near());
                        log.push(3);
                    });
                    assert!(near());
                });
                assert!(!near());
                on_thread_stack(|| log.push(4));
            });
            on_thread_stack(|| assert!(near()));
            log.push(5);
        });
        assert!(near());
        assert_eq!(log, [1, 2, 3, 4, 5]);
    }
}
