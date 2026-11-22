; CuBit OS -- VMM-001 slice 1: one AMD SVM guest entry and exit.
;
; void cubitSvmRun (uint64 vmcb_phys, uint64 host_area_phys, uint64 *gprs,
;                   uint64 *spec_ctrl)
;   rdi = the guest VMCB's physical address
;   rsi = this CPU's host VMSAVE area (physical)
;   rdx = the guest's general registers other than RAX and RSP, which live
;         in the VMCB: rbx, rcx, rdx, rsi, rdi, rbp, r8 .. r15 (14 words)
;   rcx = the vCPU's SPEC_CTRL block: guest value, host value, mode
;         (Guest_Speculation.Switch: 0 none, 1 load the guest's, 2 load it
;         and read it back at exit)
;
; Called with interrupts disabled (IF = 0); returns with GIF set and IF
; still clear, so a host interrupt that caused the exit is taken when the
; caller next enables interrupts. Inside, with GIF clear (so the host takes
; nothing), IF is set for VMRUN: with V_INTR_MASKING the host's saved IF
; decides whether a physical interrupt exits the guest (APM 15.21.1), and
; it must, or a guest spinning with its interrupts off would never leave. Host state VMRUN does not save (FS, GS,
; TR, LDTR, KERNEL_GS_BASE, STAR/LSTAR/CSTAR/SFMASK, SYSENTER) goes through
; VMSAVE/VMLOAD of the host area; the guest's through its VMCB. Between
; VMRUN's exit and the host VMLOAD nothing touches GS (the per-CPU base).
; The SVM instructions are written as bytes (AMD APM Vol. 3).
;
; SPEC_CTRL (docs/hypervisor.md 4.3, 9.2), in Linux KVM's svm/vmenter.S
; order: the guest's value is written just before VMRUN when it differs
; from the host's, with no return or indirect branch between that write and
; VMRUN; after the exit, once the guest's registers are saved, the guest's
; value is read back (mode 2) and the host's written when they differ,
; before the first return or indirect branch. GIF is clear throughout, so
; no interrupt or NMI runs host code in between. With V_SPEC_CTRL the mode
; is 0 and VMRUN itself switches the MSR.

bits 64

%define VMRUN  db 0x0F, 0x01, 0xD8
%define VMLOAD db 0x0F, 0x01, 0xDA
%define VMSAVE db 0x0F, 0x01, 0xDB
%define STGI   db 0x0F, 0x01, 0xDC
%define CLGI   db 0x0F, 0x01, 0xDD

MSR_IA32_SPEC_CTRL equ 0x48         ; Guest_Speculation.IA32_SPEC_CTRL
SPEC_GUEST  equ 0                   ; the block's words
SPEC_HOST   equ 8
SPEC_MODE   equ 16
SPEC_NO_SWITCH     equ 0            ; Guest_Speculation.Switch
SPEC_LOAD_AND_SAVE equ 2

global cubitSvmRun
cubitSvmRun:
    push    rbp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    push    rcx                 ; [rsp+24] SPEC_CTRL block
    push    rsi                 ; [rsp+16] host area
    push    rdx                 ; [rsp+8]  gprs
    push    rdi                 ; [rsp]    vmcb

    CLGI
    mov     rax, rsi
    VMSAVE                      ; host FS/GS/TR/LDTR/KGS/STAR... to host area

    ; The guest's SPEC_CTRL, if it differs from the host's.
    mov     rbx, [rsp + 24]
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

    mov     rax, [rsp + 8]      ; gprs
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
    mov     rax, [rsp]          ; the VMCB
    VMLOAD
    sti                         ; GIF is clear: nothing is taken here
    VMRUN
    cli
    VMSAVE                      ; RAX is the VMCB again (host RAX restored)

    push    rbx                 ; the guest's RBX; gprs now at [rsp+16]
    mov     rbx, [rsp + 16]
    mov     [rbx + 8],   rcx
    mov     [rbx + 16],  rdx
    mov     [rbx + 24],  rsi
    mov     [rbx + 32],  rdi
    mov     [rbx + 40],  rbp
    mov     [rbx + 48],  r8
    mov     [rbx + 56],  r9
    mov     [rbx + 64],  r10
    mov     [rbx + 72],  r11
    mov     [rbx + 80],  r12
    mov     [rbx + 88],  r13
    mov     [rbx + 96],  r14
    mov     [rbx + 104], r15
    pop     rax
    mov     [rbx + 0], rax

    ; The host's SPEC_CTRL back, before any return or indirect branch.
    mov     rbx, [rsp + 24]     ; the SPEC_CTRL block
    mov     rsi, [rbx + SPEC_MODE]
    cmp     rsi, SPEC_NO_SWITCH
    je      .host_spec_ctrl_done
    cmp     rsi, SPEC_LOAD_AND_SAVE
    jne     .host_spec_ctrl_compare
    mov     ecx, MSR_IA32_SPEC_CTRL
    rdmsr                       ; the guest may have written it
    shl     rdx, 32
    or      rax, rdx
    mov     [rbx + SPEC_GUEST], rax
.host_spec_ctrl_compare:
    mov     rax, [rbx + SPEC_HOST]
    cmp     rax, [rbx + SPEC_GUEST]
    je      .host_spec_ctrl_done
    mov     rdx, rax
    shr     rdx, 32
    mov     ecx, MSR_IA32_SPEC_CTRL
    wrmsr
.host_spec_ctrl_done:

    add     rsp, 16             ; the VMCB and gprs words
    pop     rax                 ; the host area
    add     rsp, 8              ; the SPEC_CTRL block
    VMLOAD
    STGI

    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret
