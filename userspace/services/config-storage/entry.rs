// The entry of the Ada-hosted Rust programs (this worker and the native
// Turso probe), which keep their own `_start` and `rust_main` instead of the
// std runtime's weak `_start`/`main`. Like that start code and the C one
// (userspace/c/crt0.S), it keeps the kernel's entry RDI, the launch block's
// length, in __cubit_launch_block_length: CuBit.Launch_Block (elaborated by
// the host's Ada initializer) and cubit::startup read the CAP-002 startup
// table through it. Without it every startup entry ("config", "clock",
// "filesystem") is absent. Neither program reads launch arguments.
core::arch::global_asm!(
    r#"
    .section .text._start,"ax",@progbits
    .global _start
    _start:
        mov [rip + __cubit_launch_block_length], rdi
        xor rbp, rbp
        and rsp, -16
        cld
        call rust_main
        ud2
    .section .bss.cubit_launch,"aw",@nobits
    .balign 8
    .global __cubit_launch_block_length
    __cubit_launch_block_length: .quad 0
    .section .note.GNU-stack,"",@progbits
"#
);
