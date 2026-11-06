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

; debug exception
isr1:
    swapGSIfFromProcess
	push long 0
	push long 1
	jmp isrCommon

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
isr13:
    swapGSIfFromProcessErr
	; returns error code
	push long 13
	jmp isrCommon

; page fault (CPU pushes error code)
isr14:
    swapGSIfFromProcessErr
	; returns error code
	push long 14
	jmp isrCommon

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

; Entry for the IST exceptions (NMI, double fault, machine check). They can
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
