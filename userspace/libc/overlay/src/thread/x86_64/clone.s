/* CuBit: __clone(func, stack, flags, arg, ptid, tls, ctid) over
 * THREAD_CREATE (90): entry, stack, argument, FS base, and the word the
 * kernel clears and futex-wakes at thread exit (CLONE_CHILD_CLEARTID).
 * The new thread starts at 1: with func and arg on its stack. Only the
 * thread-creation form musl's pthread_create uses is supported: flags are
 * implied (shared address space, TLS, parent/child tid words).
 * The syscall instruction clobbers rcx and r11. */
.text
.global __clone
.hidden __clone
.type   __clone,@function
__clone:
	push %r12
	mov 16(%rsp),%rax        /* ctid (7th argument) */
	mov %r8,%r12             /* ptid, kept across the syscall */
	and $-16,%rsi
	sub $16,%rsi
	mov %rdi,(%rsi)          /* func */
	mov %rcx,8(%rsi)         /* arg */
	lea 1f(%rip),%rdi        /* entry */
	                         /* rsi: the new thread's stack pointer */
	xor %edx,%edx            /* argument: unused, 1: pops its own */
	mov %r9,%r10             /* FS base */
	mov %rax,%r8             /* exit word */
	mov $90,%eax
	syscall
	cmp $-1,%rax
	je 2f
	test %r12,%r12
	jz 3f
	mov %eax,(%r12)          /* CLONE_PARENT_SETTID */
3:	pop %r12
	ret
2:	mov $-11,%rax            /* -EAGAIN */
	pop %r12
	ret

1:	xor %ebp,%ebp
	pop %rax                 /* func */
	pop %rdi                 /* arg */
	call *%rax
	mov %eax,%edi
	mov $91,%eax             /* THREAD_EXIT */
	syscall
	hlt
