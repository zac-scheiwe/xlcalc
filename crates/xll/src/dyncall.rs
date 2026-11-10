use std::arch::global_asm;

global_asm!(
    ".globl xlc_dyncall",
    "xlc_dyncall:",
    "push rbp",
    "mov rbp, rsp",
    "push rbx",
    "push rdi",
    "push rsi",
    "push r12",
    "mov rax, rcx",
    "mov rsi, rdx",
    "mov rbx, r8",
    "mov rdi, r9",
    "mov rcx, rbx",
    "sub rcx, 4",
    "jg 2f",
    "xor rcx, rcx",
    "2:",
    "lea rdx, [rcx*8 + 32 + 15]",
    "and rdx, -16",
    "sub rsp, rdx",
    "xor r12, r12",
    "3:",
    "cmp r12, rcx",
    "jge 4f",
    "mov r10, [rsi + r12*8 + 32]",
    "mov [rsp + r12*8 + 32], r10",
    "inc r12",
    "jmp 3b",
    "4:",
    "mov rcx, [rsi]",
    "movq xmm0, rcx",
    "mov rdx, [rsi + 8]",
    "movq xmm1, rdx",
    "mov r8, [rsi + 16]",
    "movq xmm2, r8",
    "mov r9, [rsi + 24]",
    "movq xmm3, r9",
    "call rax",
    "mov [rdi], rax",
    "movq [rdi + 8], xmm0",
    "lea rsp, [rbp - 32]",
    "pop r12",
    "pop rsi",
    "pop rdi",
    "pop rbx",
    "pop rbp",
    "ret",
);

extern "C" {
    fn xlc_dyncall(function: usize, slots: *const u64, count: usize, out: *mut [u64; 2]);
}

pub struct Returned {
    pub rax: u64,
    pub xmm0: f64,
}

pub unsafe fn call(function: usize, args: &[u64]) -> Returned {
    let mut slots = [0u64; 4].to_vec();
    if args.len() > 4 {
        slots = args.to_vec();
    } else {
        slots[..args.len()].copy_from_slice(args);
    }
    let mut out = [0u64; 2];
    xlc_dyncall(function, slots.as_ptr(), args.len(), &mut out);
    Returned {
        rax: out[0],
        xmm0: f64::from_bits(out[1]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    extern "C" fn mixed(a: i32, b: f64, c: i32, d: f64, e: i32, f: f64) -> f64 {
        f64::from(a) + b * 10.0 + f64::from(c) * 100.0 + d * 1000.0 + f64::from(e) * 1e4 + f * 1e5
    }

    extern "C" fn int_result(a: i64, b: i64) -> i64 {
        a * 7 + b
    }

    #[test]
    fn mixed_int_and_double_slots() {
        let args = [
            1u64,
            2.0f64.to_bits(),
            3,
            4.0f64.to_bits(),
            5,
            6.0f64.to_bits(),
        ];
        let r = unsafe { call(mixed as *const () as usize, &args) };
        assert_eq!(r.xmm0, 1.0 + 20.0 + 300.0 + 4000.0 + 5e4 + 6e5);
    }

    #[test]
    fn integer_result_and_few_arguments() {
        let r = unsafe { call(int_result as *const () as usize, &[6, 5]) };
        assert_eq!(r.rax, 47);
    }
}
