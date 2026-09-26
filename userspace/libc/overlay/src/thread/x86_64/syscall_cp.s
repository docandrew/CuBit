/* CuBit: cancellable system calls. Cancellation is checked before the call
 * (as musl does); a blocked CuBit call is not interrupted by cancellation. */
.text
.global __cp_begin
.hidden __cp_begin
.global __cp_end
.hidden __cp_end
.global __cp_cancel
.hidden __cp_cancel
.hidden __cancel
.hidden __cubit_syscall
.global __syscall_cp_asm
.hidden __syscall_cp_asm
.type   __syscall_cp_asm,@function
__syscall_cp_asm:
__cp_begin:
	mov (%rdi),%eax
	test %eax,%eax
	jnz __cp_cancel
	mov %rsi,%rdi            /* nr */
	mov %rdx,%rsi            /* u */
	mov %rcx,%rdx            /* v */
	mov %r8,%rcx             /* w */
	mov %r9,%r8              /* x */
	mov 8(%rsp),%r9          /* y */
	mov 16(%rsp),%rax        /* z */
	sub $8,%rsp
	push %rax
	call __cubit_syscall
	add $16,%rsp
__cp_end:
	ret
__cp_cancel:
	jmp __cancel
