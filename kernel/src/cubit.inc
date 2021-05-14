;------------------------------------------------------------------------------
; CuBitOS
; Copyright (C) 2020 Jon Andrew
;
; Variables and offsets from the CuBit SPARK code. In most cases, these need
; to match the equivalent variable from either a SPARK/Ada file or the linker,
; and should be annotated accordingly.
;------------------------------------------------------------------------------

; offsets from the PerCPUData base address - make sure these match the
; PerCPUData record layout
SAVED_PROCESS_RSP       equ 8
SAVED_KERNEL_RSP        equ 16

; GDT segment offsets - make sure these match those in segment.ads
GDT_OFFSET_KERNEL_CODE  equ 8
GDT_OFFSET_KERNEL_DATA  equ 16
GDT_OFFSET_USER_CODE    equ 24
GDT_OFFSET_USER_DATA    equ 32

; Make sure this matches linker.ld
KERNEL_BASE             equ 0xFFFF_FFFF_8000_0000
LINEAR_BASE             equ 0xFFFF_8000_0000_0000

; Make sure this matches virtmem.ads
STACK_TOP               equ 0xFFFF_8000_0100_0000
STACK_BOTTOM            equ 0xFFFF_8000_0080_0000

; Make sure this matches config.ads, also percpudata.adb
MAX_CPUS                equ 128
PER_CPU_STACK_SIZE      equ (STACK_TOP - STACK_BOTTOM) / MAX_CPUS

; Make sure this matches s-parame.ads in the runtime
SEC_STACK_SIZE          equ 2048

; Syscall numbers (rax) - make sure this matches syscall.ads
SYSCALL_EXIT            equ 0
SYSCALL_READ            equ 1
SYSCALL_CLOSE           equ 2
SYSCALL_EXECVE          equ 3
SYSCALL_FORK            equ 4
SYSCALL_FSTAT           equ 5
SYSCALL_GETPID          equ 6
SYSCALL_KILL            equ 7
SYSCALL_SBRK            equ 8
SYSCALL_TIMES           equ 9
SYSCALL_UNLINK          equ 10
SYSCALL_WAIT            equ 11
SYSCALL_WRITE           equ 12
SYSCALL_OPEN            equ 13

SYSCALL_INFO            equ 15
SYSCALL_SEND            equ 16
SYSCALL_RECEIVE         equ 17
SYSCALL_REPLY           equ 18
SYSCALL_SEND_EVENT      equ 19
SYSCALL_RECEIVE_EVENT   equ 20

SYSCALL_CONTROLACCESS   equ 100
SYSCALL_GETTICKET       equ 101
SYSCALL_GRANT           equ 102
SYSCALL_REVOKE          equ 103

; Useful constants for init.asm
STDIN                   equ 0
STDOUT                  equ 1
STDERR                  equ 2