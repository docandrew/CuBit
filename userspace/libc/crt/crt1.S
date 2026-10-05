/*
 * CuBit libc program entry. The loader starts _start with the stack pointer
 * at the top of an empty stack and RDI = the launch block's length; the
 * start itself is Ada (CuBit.Libc_Start, userspace/libc/ada).
 */
	.text
	.global _start
	.type _start,@function
_start:
	xor %ebp,%ebp
	mov %rdi,%rsi            /* the launch block's length */
	mov %rsp,%rdi            /* the initial stack pointer */
	and $-16,%rsp
	call __cubit_start
	hlt
	.section .note.GNU-stack,"",@progbits
