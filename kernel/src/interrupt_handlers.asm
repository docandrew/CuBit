;------------------------------------------------------------------------------
; CuBit OS
; Copyright (C) 2020 Jon Andrew
; 
; Wrapper for interrupt handlers. Ada doesn't have naked functions, so the 
; functions here are used as the actual interrupt vectors. They push the
; interrupt code (and a dummy error code if the exception itself didn't)
; onto the stack, preserve existing registers, jump to the interrupt 
; handler, and then when that returns we restore the registers and return
; from the interrupt.
;------------------------------------------------------------------------------

%include "cubit.inc"

bits 64

extern interruptHandler     ; in interrupt_pic.adb
extern cubitEntryClearsAC   ; Interrupt_Entry.Clears_Alignment_Check

; CLAC (0F 01 CA) when the CPU has SMAP (Interrupt_Entry). The flag is
; read RIP-relative, so it is safe before any GS or stack switch.
%macro clearACIfSMAP 0
    cmp byte [rel cubitEntryClearsAC], 0
    je %%keep
    db 0x0F, 0x01, 0xCA                 ; clac
%%keep:
%endmacro

global interruptReturn
global enterUsermode

global isr0
global isr1
global isr2
global isr3
global isr4
global isr5
global isr6
global isr7
global isr8
global isr9
global isr10
global isr11
global isr12
global isr13
global isr14
global isr15
global isr16
global isr17
global isr18
global isr19

global isr32
global isr33
global isr34
global isr35
global isr36
global isr37
global isr38
global isr39
global isr40
global isr41
global isr42
global isr43
global isr44
global isr45
global isr46
global isr47
global deviceVectorStubs     ; device vectors, DEVICE_STUB_BYTES apart
global deviceVectorStubsEnd

global isr127   ; KERNEL PANIC
global isr128   ; SYSCALL
global isr249   ; Reschedule IPI
global isr250   ; IOMMU fault event
global isr255   ; Spurious

; If this interrupt occurred while executing a user-mode process, we need to 
; switch over to our kernel stack. We can see if this was the case by checking
; the bottom two bits of the CS register, which hold the current privilege
; level (CPL). CS is the 2nd-to-last pushed value on the stack during
; interrupt.

; For exceptions WITHOUT an error code, the CPU pushes:
;   [rsp+0] = RIP, [rsp+8] = CS, ...
; so CS is at rsp+8.
; Entry macros end with LFENCE on both paths (the "SWAPGS" Spectre variant):
; without it the CPU can speculate past the CS test with the wrong GS base
; and load through an attacker-chosen GS. The return path needs no fence.
%macro swapGSIfFromProcess 0
    test qword [rsp+8], 3             ; are bottom 2 bits of CS set?
    je %%fromKernel
    swapgs
%%fromKernel:
    lfence
%endmacro

; Return path: CS is at rsp+8 once the frame is popped back to RIP.
%macro swapGSOnReturnToProcess 0
    test qword [rsp+8], 3
    je %%toKernel
    swapgs
%%toKernel:
%endmacro

; For exceptions WITH an error code pushed by the CPU, the stack is:
;   [rsp+0] = error code, [rsp+8] = RIP, [rsp+16] = CS, ...
; so CS is at rsp+16, NOT rsp+8.
%macro swapGSIfFromProcessErr 0
    test qword [rsp+16], 3            ; are bottom 2 bits of CS set?
    je %%fromKernel
    swapgs
%%fromKernel:
    lfence
%endmacro

;------------------------------------------------------------------------------
; Processor Exceptions
;------------------------------------------------------------------------------

; divide by 0
isr0:
    swapGSIfFromProcess
	push long 0				; dummy error code
	push long 0				; number of interrupt
	jmp isrCommon

; debug exception (IST 4)
; A #DB from user mode (CS's RPL is 3: the CPU just left ring 3, so the user's
; GS base is live) moves to this thread's kernel stack and takes the ordinary
; path, so its handler may block or switch like any other user exception.
; A #DB in kernel mode is a data breakpoint whose condition MOV SS or POP SS
; deferred onto the kernel's first instruction (CVE-2018-8897): after SYSCALL
; that instruction still runs on the user's stack pointer, and after INT n
; before its SWAPGS, so neither the stack nor the pushed CS can be trusted.
; It runs on IST 4 through the GS-exact entry and returns without blocking.
isr1:
    test qword [rsp+8], 3               ; CS of the interrupted code
    jz .fromKernel
    swapgs
    lfence
    ; Interrupts are off (interrupt gate) and the CPU came from ring 3, so
    ; SAVED_PROCESS_RSP is free scratch (SYSCALL entry uses it only with
    ; interrupts off, before it pushes) and SAVED_KERNEL_RSP is this
    ; thread's kernel stack top (TSS.rsp0).
    mov [gs:SAVED_PROCESS_RSP], rax
    mov rax, rsp                        ; the hardware frame on IST 4
    mov rsp, [gs:SAVED_KERNEL_RSP]
    push qword [rax + 32]               ; SS
    push qword [rax + 24]               ; RSP
    push qword [rax + 16]               ; RFLAGS
    push qword [rax + 8]                ; CS
    push qword [rax]                    ; RIP
    mov rax, [gs:SAVED_PROCESS_RSP]
	push long 0
	push long 1
	jmp isrCommon
.fromKernel:
	push long 0
	push long 1
	jmp istCommon

; NMI (IST 1, GS-exact entry)
isr2:
	push long 0
	push long 2
	jmp istCommon

; breakpoint
isr3:
    swapGSIfFromProcess
	push long 0
	push long 3
	jmp isrCommon

; into detected overflow
isr4:
    swapGSIfFromProcess
	push long 0
	push long 4
	jmp isrCommon

; out of bounds
isr5:
    swapGSIfFromProcess
	push long 0
	push long 5
	jmp isrCommon

; invalid opcode
isr6:
    swapGSIfFromProcess
	push long 0
	push long 6
	jmp isrCommon

; no co-processor
isr7:
    swapGSIfFromProcess
	push long 0
	push long 7
	jmp isrCommon

; double fault (IST 2, GS-exact entry; CPU pushes error code)
isr8:
	push long 8
	jmp istCommon

; coprocessor segment overrun
isr9:
    swapGSIfFromProcess
	push long 0
	push long 9
	jmp isrCommon

; bad TSS exception (CPU pushes error code)
isr10:
    swapGSIfFromProcessErr
	; returns error code
	push long 10
	jmp isrCommon

; segment not present (CPU pushes error code)
isr11:
    swapGSIfFromProcessErr
	; returns error code
	push long 11
	jmp isrCommon

; stack fault (CPU pushes error code)
isr12:
    swapGSIfFromProcessErr
	; returns error code
	push long 12
	jmp isrCommon

; GPF exception (CPU pushes error code)
; A #GP raised by one of the recovering MSR accesses below (kernel mode
; only: user code cannot run at their addresses) resumes at its fault
; label instead: the error code is dropped and IRETQ returns there.
isr13:
    push rax
    lea rax, [rel msrSafeWriteInsn]
    cmp [rsp + 16], rax                ; rax, error code, then RIP
    je .msrWriteFaulted
    lea rax, [rel msrSafeReadInsn]
    cmp [rsp + 16], rax
    je .msrReadFaulted
    pop rax
    swapGSIfFromProcessErr
	; returns error code
	push long 13
	jmp isrCommon
.msrWriteFaulted:
    lea rax, [rel msrSafeWriteFault]
    jmp .msrResume
.msrReadFaulted:
    lea rax, [rel msrSafeReadFault]
.msrResume:
    mov [rsp + 16], rax
    pop rax
    add rsp, 8                         ; the error code
    iretq

;------------------------------------------------------------------------------
; MSR accesses that survive a refusal (Msr_Safe): a CPU may advertise a
; feature yet refuse its MSR (the N150's HWP read, image 27). Each returns
; 0 on success and 1 when the instruction raised #GP (isr13 above).
;   cubitWrmsrSafe (rdi = MSR, rsi = value)
;   cubitRdmsrSafe (rdi = MSR, rsi = address of the 64-bit result)
;------------------------------------------------------------------------------
global cubitWrmsrSafe
global cubitRdmsrSafe
cubitWrmsrSafe:
    mov ecx, edi
    mov eax, esi
    mov rdx, rsi
    shr rdx, 32
msrSafeWriteInsn:
    wrmsr
    xor eax, eax
    ret
msrSafeWriteFault:
    mov eax, 1
    ret

cubitRdmsrSafe:
    mov ecx, edi
msrSafeReadInsn:
    rdmsr
    shl rdx, 32
    mov eax, eax
    or rax, rdx
    mov [rsi], rax
    xor eax, eax
    ret
msrSafeReadFault:
    mov qword [rsi], 0
    mov eax, 1
    ret

; page fault (CPU pushes error code)
; A #PF raised by one of the recovering accesses below (kernel mode only:
; user code cannot run at their addresses) resumes at its fault label, as
; isr13 does for the MSR accesses: the error code is dropped and IRETQ
; returns there, with the interrupted RFLAGS.
%macro resumeIfFaultAt 2                ; instruction label, fault label
    lea rax, [rel %1]
    cmp [rsp + 16], rax                ; rax, error code, then RIP
    jne %%other
    lea rax, [rel %2]
    jmp .pfResume
%%other:
%endmacro

isr14:
    push rax
    resumeIfFaultAt userReadProbeInsn, userReadProbeFault
    resumeIfFaultAt kernelReadProbeInsn, kernelReadProbeFault
    resumeIfFaultAt copyFromUserInsn, copyFromUserFault
    resumeIfFaultAt copyToUserInsn, copyToUserFault
    resumeIfFaultAt userLoad32Insn, userLoad32Fault
    resumeIfFaultAt userStore32Insn, userStore32Fault
    pop rax
    swapGSIfFromProcessErr
	; returns error code
	push long 14
	jmp isrCommon
.pfResume:
    mov [rsp + 16], rax
    pop rax
    add rsp, 8                         ; the error code
    iretq

;------------------------------------------------------------------------------
; A kernel read of a user address that survives a refusal, for the
; interrupt-entry SMAP probe (Build.Smap_Entry_Probe, Interrupts): returns 0
; when the read succeeded and 1 when it raised #PF (isr14 above).
;   cubitUserReadProbe (rdi = user address)
;------------------------------------------------------------------------------
global cubitUserReadProbe
cubitUserReadProbe:
userReadProbeInsn:
    mov al, [rdi]
    xor eax, eax
    ret
userReadProbeFault:
    mov eax, 1
    ret

;------------------------------------------------------------------------------
; A kernel read of one 64-bit word at a kernel address that survives a page
; fault, for the kernel-map probe (KERN-014, Build.Kernel_Map_Probe,
; Process.Map_Probe): returns 0 and stores the word when the read went
; through, 1 (storing 0) when it raised #PF (isr14 above).
;   cubitKernelReadProbe (rdi = kernel address, rsi = address of the result)
;------------------------------------------------------------------------------
global cubitKernelReadProbe
cubitKernelReadProbe:
kernelReadProbeInsn:
    mov rax, [rdi]
    mov [rsi], rax
    xor eax, eax
    ret
kernelReadProbeFault:
    mov qword [rsi], 0
    mov eax, 1
    ret

;------------------------------------------------------------------------------
; The kernel's access to the current process's own memory, through that
; process's own mapping (KERN-014 slice 2, Process.User_Memory): no page
; walk, no frame pin, no linear-map address. SMAP is opened (STAC) for the
; one access instruction and closed (CLAC) after it, on both paths. A page
; fault on that instruction (an unmapped page, a read-only page under a
; store, a page a sibling thread just unmapped) resumes at its fault label
; (isr14 above), which counts it in cubitUserAccessFaults.
;
; The caller (Ada) admits the range below User_Limit. Each routine clamps
; the user address again without a branch, so a mispredicted admission
; cannot steer a speculative access into the kernel half (SEC-026): an
; address past USER_LIMIT - length becomes 0, the unmapped null page.
; Each returns 0 on success and 1 on a fault.
;   cubitCopyFromUser (rdi = kernel destination, rsi = user source, rdx = bytes)
;   cubitCopyToUser   (rdi = user destination, rsi = kernel source, rdx = bytes)
;   cubitUserLoad32   (rdi = user address, rsi = address of the 32-bit result)
;   cubitUserStore32  (rdi = user address, esi = value)
;------------------------------------------------------------------------------
USER_LIMIT equ 0x0000800000000000        ; User_Page_Walk.User_Limit

extern cubitUserAccessFaults

%macro openUserAccess 0                  ; STAC when the CPU has SMAP
    cmp byte [rel cubitEntryClearsAC], 0
    je %%open
    db 0x0F, 0x01, 0xCB                 ; stac
%%open:
%endmacro

; reg: the user address, len: its byte count (a register or a constant)
%macro clampUserAddress 2
    mov rax, USER_LIMIT
    sub rax, %2
    xor r8d, r8d
    cmp %1, rax
    cmova %1, r8
%endmacro

%macro countUserFault 0
    lock inc qword [rel cubitUserAccessFaults]
%endmacro

global cubitCopyFromUser
global cubitCopyToUser
global cubitUserLoad32
global cubitUserStore32

cubitCopyFromUser:
    clampUserAddress rsi, rdx
    mov rcx, rdx
    openUserAccess
copyFromUserInsn:
    rep movsb
    clearACIfSMAP
    xor eax, eax
    ret
copyFromUserFault:
    clearACIfSMAP
    countUserFault
    mov eax, 1
    ret

cubitCopyToUser:
    clampUserAddress rdi, rdx
    mov rcx, rdx
    openUserAccess
copyToUserInsn:
    rep movsb
    clearACIfSMAP
    xor eax, eax
    ret
copyToUserFault:
    clearACIfSMAP
    countUserFault
    mov eax, 1
    ret

cubitUserLoad32:
    clampUserAddress rdi, 4
    openUserAccess
userLoad32Insn:
    mov eax, [rdi]
    clearACIfSMAP
    mov [rsi], eax
    xor eax, eax
    ret
userLoad32Fault:
    clearACIfSMAP
    countUserFault
    mov dword [rsi], 0
    mov eax, 1
    ret

cubitUserStore32:
    clampUserAddress rdi, 4
    openUserAccess
userStore32Insn:
    mov [rdi], esi
    clearACIfSMAP
    xor eax, eax
    ret
userStore32Fault:
    clearACIfSMAP
    countUserFault
    mov eax, 1
    ret

; unknown interrupt
;isr15:
;	push long 0
;	push long 15
;	jmp isrCommon

; coprocessor fault
isr16:
    swapGSIfFromProcess
	push long 0
	push long 16
	jmp isrCommon

; alignment check exception (CPU pushes error code)
isr17:
    swapGSIfFromProcessErr
	; returns error code
	push long 17
	jmp isrCommon

; machine check exception (IST 3, GS-exact entry)
isr18:
	push long 0
	push long 18
	jmp istCommon

; Interrupts 19-31: reserved
isr19:
    swapGSIfFromProcess
	push long 0
	push long 19
	jmp isrCommon

;------------------------------------------------------------------------------
; Hardware IRQs
;------------------------------------------------------------------------------

isr32:
    swapGSIfFromProcess
	push long 0
	push long 32
	jmp isrCommon

isr33:
    swapGSIfFromProcess
	push long 0
	push long 33
	jmp isrCommon

isr34:
    swapGSIfFromProcess
	push long 0
	push long 34
	jmp isrCommon

isr35:
    swapGSIfFromProcess
	push long 0
	push long 35
	jmp isrCommon

isr36:
    swapGSIfFromProcess
	push long 0
	push long 36
	jmp isrCommon

isr37:
    swapGSIfFromProcess
	push long 0
	push long 37
	jmp isrCommon

isr38:
    swapGSIfFromProcess
	push long 0
	push long 38
	jmp isrCommon

isr39:
    swapGSIfFromProcess
	push long 0
	push long 39
	jmp isrCommon

isr40:
    swapGSIfFromProcess
	push long 0
	push long 40
	jmp isrCommon

isr41:
    swapGSIfFromProcess
	push long 0
	push long 41
	jmp isrCommon

isr42:
    swapGSIfFromProcess
	push long 0
	push long 42
	jmp isrCommon

isr43:
    swapGSIfFromProcess
	push long 0
	push long 43
	jmp isrCommon

isr44:
    swapGSIfFromProcess
	push long 0
	push long 44
	jmp isrCommon

isr45:
    swapGSIfFromProcess
	push long 0
	push long 45
	jmp isrCommon

isr46:
    swapGSIfFromProcess
	push long 0
	push long 46
	jmp isrCommon

isr47:
    swapGSIfFromProcess
	push long 0
	push long 47
	jmp isrCommon

; Device vectors (docs/msi-interrupts.md): one fixed-size stub per vector
; from DEVICE_VECTOR_FIRST, so stub V is at deviceVectorStubs +
; DEVICE_STUB_BYTES * (V - DEVICE_VECTOR_FIRST). The padding fails to
; assemble if a stub ever outgrows its slot.
%macro deviceVectorStub 1
%%start:
    swapGSIfFromProcess
    push long 0
    push long %1
    jmp near isrCommon
    times DEVICE_STUB_BYTES - ($ - %%start) db 0xCC
%endmacro

align DEVICE_STUB_BYTES
deviceVectorStubs:
%assign deviceVector DEVICE_VECTOR_FIRST
%rep DEVICE_VECTOR_COUNT
    deviceVectorStub deviceVector
%assign deviceVector deviceVector + 1
%endrep
deviceVectorStubsEnd:

; Kernel Panic
isr127:
    swapGSIfFromProcess
    push long 0
    push long 0x7F
    jmp isrCommon

; Kernel Syscall (legacy interrupt - may remove)
isr128:
    swapGSIfFromProcess
	push long 0
	push long 0x80
	jmp isrCommon

; Reschedule IPI
isr249:
    swapGSIfFromProcess
    push long 0
    push long 249
    jmp isrCommon

; IOMMU fault event (VT-d, IOMMU-001)
isr250:
    swapGSIfFromProcess
    push long 0
    push long 250
    jmp isrCommon

; Spurious Interrupt
isr255:
    swapGSIfFromProcess
    push long 0
    push long 255
    jmp isrCommon

isrCommon:
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

    ; RFLAGS.DF is the interrupted code's: user code may hold it set (a
    ; backward memmove is STD; REP MOVS; CLD), and an interrupt does not
    ; clear it. The kernel's compiled code assumes DF clear (the ABI), so
    ; every REP MOVS/STOS it runs would copy backwards and write below its
    ; buffers. IRETQ restores the interrupted DF. (SYSCALL clears DF itself:
    ; x86.SYSCALL_ENTRY_FLAGS_MASK.)
    cld
    ; RFLAGS.AC likewise (SEC-026): user code may hold it set, and under SMAP
    ; a set AC lets supervisor code touch user pages, so the handler would
    ; run with SMAP off. CLAC raises #UD without SMAP, so only where kmain
    ; found it (Interrupt_Entry); without SMAP AC means nothing at CPL 0.
    clearACIfSMAP

    ; single argument to interruptHandler, containing this frame
    mov rdi, rsp
    call interruptHandler

interruptReturn:
    ; Keep the return frame and GS transition protected until IRETQ restores
    ; the destination context's interrupt state and privilege level together.
    cli
    pop rax
    pop rbx
    pop rcx
    pop rdx
    pop rbp
    pop rsi
    pop rdi
    pop r8
    pop r9
    pop r10
    pop r11
    pop r12
    pop r13
    pop r14
    pop r15	

    ; adjust stack pointer to discard our interrupt # and error code
    add rsp, 16
    
    ; Put the user's GS back.
    swapGSOnReturnToProcess
    iretq

; Entry for the IST exceptions (NMI, double fault, machine check, and a #DB
; in kernel mode). They can
; arrive between SYSCALL and SWAPGS or between SWAPGS and SYSRET, where the
; saved CS does not tell which GS base is live, and user code chooses its own
; GS base with WRGSBASE. So never SWAPGS here: save the live GS base, load this
; CPU's PerCPUData from the slot at the top of the IST stack (each IST stack is
; aligned to its size), and on return restore exactly what was live. r12 holds
; the saved base across the call (callee-saved); the frame keeps its own copy
; of r12. RDMSR/WRMSR rather than RDGSBASE so this works before CR4.FSGSBASE.
istCommon:
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

    mov ecx, MSR_GS_BASE
    rdmsr
    shl rdx, 32
    or rax, rdx
    mov r12, rax                        ; GS base live at the exception

    mov rax, rsp
    and rax, ~(IST_STACK_SIZE - 1)      ; base of this IST stack
    mov rax, [rax + IST_CPU_DATA_OFFSET]
    mov rdx, rax
    shr rdx, 32
    wrmsr                               ; GS base := this CPU's PerCPUData

    cld                                 ; DF as in isrCommon
    clearACIfSMAP                       ; AC as in isrCommon
    mov rdi, rsp
    call interruptHandler

    cli
    mov rax, r12
    mov rdx, r12
    shr rdx, 32
    mov ecx, MSR_GS_BASE
    wrmsr                               ; restore the GS base that was live

    pop rax
    pop rbx
    pop rcx
    pop rdx
    pop rbp
    pop rsi
    pop rdi
    pop r8
    pop r9
    pop r10
    pop r11
    pop r12
    pop r13
    pop r14
    pop r15

    add rsp, 16                         ; interrupt # and error code
    iretq
