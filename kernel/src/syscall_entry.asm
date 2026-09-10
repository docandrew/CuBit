;------------------------------------------------------------------------------
; CuBitOS
; Copyright (C) 2020 Jon Andrew
;
; Syscall kernel entry point
;
; TODO: if we implement KPTI, we'll need to make sure the syscall code gets
;  aligned in its own page(s), so everything else in the kernel can be
;  un-mapped except this... and maybe the interrupt handlers?
;------------------------------------------------------------------------------
bits 64

%include "cubit.inc"

extern syscallHandler

global syscallEntry
global syscallReturn

;------------------------------------------------------------------------------
; syscallEntry
;
; CuBit syscall ABI: RAX is the result, RCX/R11 are destroyed by SYSCALL,
; and all other general-purpose registers are preserved. This differs from
; the SysV function-call ABI used by syscallHandler: its argument registers
; are caller-saved. Save them here, not in each userspace wrapper.
;
; rax  - syscall number
; rcx  - process' return address (placed there by CPU)
; r11  - process' RFLAGS (placed there by CPU)
; rdi  - syscall arg 0 -> syscallHandler arg0
; rsi  - syscall arg 1 -> syscallHandler arg1
; rdx  - syscall arg 2 -> syscallHandler arg2
; r10  - syscall arg 3 -> rcx -> syscallHandler arg3
; r8   - syscall arg 4 -> syscallHandler arg4
; r9   - syscall arg 5 -> syscallHandler arg5
; r12  - syscall arg 6 (async completion token), passed on the C stack
;        All other syscalls ignore arg6. R12 remains preserved for userspace.
;
; Interrupts are already cleared here for us by the CPU via our FMASK MSR
; (see PerCPUData.setupPerCPUData)
;
;------------------------------------------------------------------------------
syscallEntry:

    swapgs                                          ; gs now has this CPU's PerCPUData struct

    mov     [gs:SAVED_PROCESS_RSP], qword rsp       ; save the process' stack pointer
    mov     rsp, qword [gs:SAVED_KERNEL_RSP]        ; and put us in the process' kernel stack

    ; Save user RSP on the kernel stack so it survives context switches.
    ; percpu.savedProcessRSP is per-CPU, NOT per-process, so if this
    ; syscall blocks (e.g. IPC send/receive) and another process runs
    ; its own syscalls, percpu.savedProcessRSP gets overwritten.
    ; The kernel stack IS per-process, so pushing here is safe.
    push qword [gs:SAVED_PROCESS_RSP]

    push r11        ; save the process' RFLAGS
    push rcx        ; save the return address

    ; The Ada/C wrappers use input-only constraints for argument registers.
    ; syscallHandler may overwrite all six under the SysV function ABI.
    ; Keep these saves on the process' stack across blocking/context switches.
    ; Six additional words retain the existing call-site stack alignment.
    push rdi
    push rsi
    push rdx
    push r10
    push r8
    push r9

    sub rsp, 8      ; alignment padding for the two C stack arguments
    push r12        ; arg6 (second C stack argument)
    push rax        ; syscall number (first C stack argument)
    mov rcx, r10    ; set 3rd argument to syscallHandler

    call syscallHandler

;------------------------------------------------------------------------------
; syscallReturn
;
; Return back to user code after restoring state. Per SysV syscall ABI, the
; syscall return value is passed back to the process via rax, which was set
; as the return value from syscallHandler.
;
; Stack layout at this point (from syscallEntry pushes):
;   [rsp+0]  = rax (syscall number) - discarded, rax has return value
;   [rsp+8] = arg6; [rsp+16] = alignment padding
;   [rsp+24 .. rsp+64] = r9, r8, r10, rdx, rsi, rdi
;   [rsp+72] = rcx (user return address)
;   [rsp+80] = r11 (user RFLAGS)
;   [rsp+88] = user RSP (saved from percpu at entry)
;------------------------------------------------------------------------------
syscallReturn:

    ; The final stack/GS transition must be atomic with respect to maskable
    ; interrupts. SYSRET restores user IF from r11 only after changing CPL.
    cli

    add rsp, 24     ; discard C stack arguments and alignment padding

    pop r9
    pop r8
    pop r10
    pop rdx
    pop rsi
    pop rdi

    pop rcx         ; restore process' return address
    pop r11         ; restore process' RFLAGS

    ; Restore user RSP from kernel stack (correct even after context switches).
    ; Save the kernel stack pointer (past the saved user RSP slot).
    mov qword [gs:SAVED_KERNEL_RSP], rsp
    add qword [gs:SAVED_KERNEL_RSP], 8         ; account for user RSP still on stack
    mov rsp, qword [rsp]                       ; load user RSP directly from stack

    swapgs                                      ; user process gets its GS back.

    o64 sysret                                  ; return to user mode.
