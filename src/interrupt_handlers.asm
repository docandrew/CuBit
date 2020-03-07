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

global isr127   ; KERNEL PANIC
global isr128   ; SYSCALL
global isr255   ; Spurious

; If this interrupt occurred while executing a user-mode process, we need to 
; switch over to our kernel stack. We can see if this was the case by checking
; the bottom two bits of the CS register, which hold the current privilege
; level (CPL). CS is the 2nd-to-last pushed value on the stack during
; interrupt.

%macro swapGSIfFromProcess 0
    test qword [rsp+8], 3             ; are bottom 2 bits of CS set?
    je .fromKernel
    swapgs
.fromKernel
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

; NMI
isr2:
    swapGSIfFromProcess
	push long 0
	push long 2
	jmp isrCommon

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

; double fault
isr8:
    swapGSIfFromProcess
	; returns error code
	push long 8
	jmp isrCommon

; coprocessor segment overrun
isr9:
    swapGSIfFromProcess
	push long 0
	push long 9
	jmp isrCommon

; bad TSS exception
isr10:
    swapGSIfFromProcess
	; returns error code
	push long 10
	jmp isrCommon

; segment not present
isr11:
    swapGSIfFromProcess
	; returns error code
	push long 11
	jmp isrCommon

; stack fault
isr12:
    swapGSIfFromProcess
	; returns error code
	push long 12
	jmp isrCommon

; GPF exception
isr13:
    swapGSIfFromProcess
	; returns error code
	push long 13
	jmp isrCommon

; page fault
isr14:
    swapGSIfFromProcess
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

; alignment check exception
isr17:
    swapGSIfFromProcess
	push long 0
	push long 17
	jmp isrCommon

; machine check exception
isr18:
    swapGSIfFromProcess
	push long 0
	push long 18
	jmp isrCommon

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
    swapGSIfFromProcess
	iretq
