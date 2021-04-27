;------------------------------------------------------------------------------
; Context switch
;  Other registers are caller-saved and already pushed here when this function
;  is called.
;------------------------------------------------------------------------------

bits 64

global asm_switch_to
asm_switch_to:
    push rbp
    push rbx
    push r11
    push r12
    push r13
    push r14
    push r15

    mov [rdi], rsp  ; first argument, old context
    mov rsp, rsi    ; second argument, new context

    pop r15
    pop r14
    pop r13
    pop r12
    pop r11
    pop rbx
    pop rbp

    ret