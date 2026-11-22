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
; The entry saves the caller's whole register file as an interrupt from
; ring 3 would (Stackframe.InterruptStackFrame, 22 words, at the top of the
; thread's kernel stack): SS, RSP, RFLAGS (R11), CS, RIP (RCX), an error-code
; tag naming a system-call frame, the call number, then every
; general-purpose register in isrCommon's order. So a thread inside the
; kernel, blocked in a call or preempted, has its exact user registers at one
; place a debugger can read (DBG-001, Debug_Sessions), callee-saved ones
; included. The frame survives blocking and context switches: the kernel
; stack is per thread.
;
; Interrupts are already cleared here for us by the CPU via our FMASK MSR
; (see PerCPUData.setupPerCPUData)
;
;------------------------------------------------------------------------------
syscallEntry:
    swapgs                                          ; gs now has this CPU's PerCPUData struct
    mov     [gs:SAVED_PROCESS_RSP], qword rsp       ; save the process' stack pointer
    mov     rsp, qword [gs:SAVED_KERNEL_RSP]        ; and put us in the process' kernel stack
    ; percpu.savedProcessRSP is per-CPU, NOT per-process, so if this syscall
    ; blocks and another process runs its own syscalls it is overwritten.
    ; The kernel stack IS per-process: the frame below keeps the user RSP.
    push long USER_SS_SELECTOR                      ; SS
    push qword [gs:SAVED_PROCESS_RSP]               ; RSP
    push r11                                        ; RFLAGS
    push long USER_CS_SELECTOR                      ; CS
    push rcx                                        ; RIP
    push long SYSCALL_FRAME_TAG                     ; errorCode: a system-call frame
    push rax                                        ; interruptNumber: the call number
    push r15
    push r14
    push r13
    push r12
    push r11
    push r10
    push r9
    push r8
    push rdi
    push rsi
    push rbp
    push rdx
    push rcx
    push rbx
    push rax
    ; Two C stack arguments (the frame keeps the call-site alignment: 22
    ; words, then these two).
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
;   [rsp+0]  = syscall number, [rsp+8] = arg6 (the C stack arguments)
;   [rsp+16 .. rsp+128] = rax, rbx, rcx, rdx, rbp, rsi, rdi, r8 .. r15
;   [rsp+136] = call number, [rsp+144] = SYSCALL_FRAME_TAG
;   [rsp+152] = RIP, [rsp+160] = CS, [rsp+168] = RFLAGS, [rsp+176] = RSP,
;   [rsp+184] = SS
; Callee-saved registers come back from the frame unchanged (syscallHandler
; preserved them); RCX and R11 come from RIP and RFLAGS for SYSRET.
;------------------------------------------------------------------------------
syscallReturn:
    ; The final stack/GS transition must be atomic with respect to maskable
    ; interrupts. SYSRET restores user IF from r11 only after changing CPL.
    cli
    add rsp, 24     ; the C stack arguments, and the saved rax (rax is the result)
    pop rbx
    add rsp, 8      ; the saved rcx (SYSCALL destroyed it; RIP below)
    pop rdx
    pop rbp
    pop rsi
    pop rdi
    pop r8
    pop r9
    pop r10
    add rsp, 8      ; the saved r11 (RFLAGS below)
    pop r12
    pop r13
    pop r14
    pop r15
    add rsp, 16     ; call number and frame tag
    pop rcx         ; restore process' return address
    add rsp, 8      ; CS
    pop r11         ; restore process' RFLAGS
    ; [rsp] = user RSP, [rsp+8] = SS. The kernel stack top for the next
    ; entry is just past the frame.
    mov qword [gs:SAVED_KERNEL_RSP], rsp
    add qword [gs:SAVED_KERNEL_RSP], 16
    mov rsp, qword [rsp]                       ; load user RSP directly from stack
    swapgs                                      ; user process gets its GS back.
    o64 sysret                                  ; return to user mode.
