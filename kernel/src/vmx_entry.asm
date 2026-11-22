; CuBit OS -- VMM-001 slice 1: one Intel VMX guest entry and exit (VMX_Run).
;
; uint64 cubitVmxRun (uint64 unused, uint64 *gprs, uint64 *rax_slot,
;                     uint64 *spec_ctrl)
;   rsi = the guest's rbx, rcx, rdx, rsi, rdi, rbp, r8 .. r15 (14 words)
;   rdx = where the guest's RAX lives (the shared state page)
;   rcx = the vCPU's SPEC_CTRL block: guest value, host value, mode
;         (Guest_Speculation.Switch, as in svm_run.asm)
; The current VMCS is loaded and fully written, launch state clear (the
; caller VMCLEARs after every exit, so a thread may migrate between entries).
; Returns 0 after a VM exit, 1 when VMLAUNCH failed (VMfail; the VMCS's
; instruction-error field says why). Called with interrupts disabled; a VM
; exit loads host RFLAGS with IF clear. The VMX instructions are bytes
; (Intel SDM Vol. 2), as svm_run.asm's are.
;
; SPEC_CTRL in Linux KVM's vmx/vmenter.S order: the guest's value is
; written just before VMLAUNCH when it differs from the host's (no return
; or indirect branch between); after the exit, once the guest's registers
; are saved, it is read back (mode 2) and the host's written when they
; differ, before the first return. A VMLAUNCH that fails puts the host's
; back the same way. Unlike SVM there is no GIF: an NMI that arrives
; between the exit and the host's write runs with the guest's value, as it
; does under KVM.

bits 64

%define VMLAUNCH        db 0x0F, 0x01, 0xC2
%define VMWRITE_RAX_RSP db 0x0F, 0x79, 0xC4    ; field in RAX, value RSP
%define VMWRITE_RAX_RBX db 0x0F, 0x79, 0xC3    ; field in RAX, value RBX
HOST_RSP equ 0x6C14
HOST_RIP equ 0x6C16

MSR_IA32_SPEC_CTRL equ 0x48         ; Guest_Speculation.IA32_SPEC_CTRL
SPEC_GUEST  equ 0                   ; the block's words
SPEC_HOST   equ 8
SPEC_MODE   equ 16
SPEC_NO_SWITCH     equ 0            ; Guest_Speculation.Switch
SPEC_LOAD_AND_SAVE equ 2

; The host's SPEC_CTRL back; rbx = the block, %1 = 1 to read the guest's
; first in mode 2. Clobbers rax, rcx, rdx, rsi.
%macro HOST_SPEC_CTRL 1
    mov     rsi, [rbx + SPEC_MODE]
    cmp     rsi, SPEC_NO_SWITCH
    je      %%done
%if %1
    cmp     rsi, SPEC_LOAD_AND_SAVE
    jne     %%compare
    mov     ecx, MSR_IA32_SPEC_CTRL
    rdmsr                       ; the guest may have written it
    shl     rdx, 32
    or      rax, rdx
    mov     [rbx + SPEC_GUEST], rax
%%compare:
%endif
    mov     rax, [rbx + SPEC_HOST]
    cmp     rax, [rbx + SPEC_GUEST]
    je      %%done
    mov     rdx, rax
    shr     rdx, 32
    mov     ecx, MSR_IA32_SPEC_CTRL
    wrmsr
%%done:
%endmacro

global cubitVmxRun
cubitVmxRun:
    push    rbp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    push    rcx                 ; [rsp+16] SPEC_CTRL block
    push    rdx                 ; [rsp+8]  rax slot
    push    rsi                 ; [rsp]    gprs

    mov     rax, HOST_RSP
    VMWRITE_RAX_RSP
    mov     rax, HOST_RIP
    lea     rbx, [rel .exit]
    VMWRITE_RAX_RBX

    ; The guest's SPEC_CTRL, if it differs from the host's.
    mov     rbx, [rsp + 16]
    cmp     qword [rbx + SPEC_MODE], SPEC_NO_SWITCH
    je      .guest_spec_ctrl_done
    mov     rax, [rbx + SPEC_GUEST]
    cmp     rax, [rbx + SPEC_HOST]
    je      .guest_spec_ctrl_done
    mov     rdx, rax
    shr     rdx, 32
    mov     ecx, MSR_IA32_SPEC_CTRL
    wrmsr
.guest_spec_ctrl_done:

    mov     rax, [rsp]          ; gprs
    mov     rbx, [rax + 0]
    mov     rcx, [rax + 8]
    mov     rdx, [rax + 16]
    mov     rsi, [rax + 24]
    mov     rdi, [rax + 32]
    mov     rbp, [rax + 40]
    mov     r8,  [rax + 48]
    mov     r9,  [rax + 56]
    mov     r10, [rax + 64]
    mov     r11, [rax + 72]
    mov     r12, [rax + 80]
    mov     r13, [rax + 88]
    mov     r14, [rax + 96]
    mov     r15, [rax + 104]
    mov     rax, [rsp + 8]
    mov     rax, [rax]
    VMLAUNCH

    ; VMfail: no guest ran; the host's SPEC_CTRL back, then 1.
    mov     rbx, [rsp + 16]
    HOST_SPEC_CTRL 0
    add     rsp, 24
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    mov     eax, 1
    ret

.exit:
    ; Host RSP as written above: [rsp] gprs, [rsp+8] rax slot, [rsp+16]
    ; the SPEC_CTRL block.
    push    rax                 ; the guest's RAX; gprs at [rsp+8]
    mov     rax, [rsp + 8]
    mov     [rax + 0],   rbx
    mov     [rax + 8],   rcx
    mov     [rax + 16],  rdx
    mov     [rax + 24],  rsi
    mov     [rax + 32],  rdi
    mov     [rax + 40],  rbp
    mov     [rax + 48],  r8
    mov     [rax + 56],  r9
    mov     [rax + 64],  r10
    mov     [rax + 72],  r11
    mov     [rax + 80],  r12
    mov     [rax + 88],  r13
    mov     [rax + 96],  r14
    mov     [rax + 104], r15
    mov     rbx, [rsp + 16]     ; the rax slot
    pop     rax
    mov     [rbx], rax
    mov     rbx, [rsp + 16]     ; the SPEC_CTRL block
    HOST_SPEC_CTRL 1
    add     rsp, 24
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    xor     eax, eax
    ret
