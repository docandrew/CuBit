/* musl supplies the exact owned stack/TLS mapping in rdi/rsi after
 * unlinking the detached thread. Release it, then exit without touching
 * either stack or TLS. Both syscalls run on the kernel stack; SYSRET does
 * not dereference user RSP. The clear-tid word is musl's process-global
 * thread-list lock, outside the released mapping (see clone.s).
 * If release is refused, still exit so the thread-list lock is cleared;
 * retained memory is then reclaimed with the process.
 * First, while the stack and TLS are still this thread's, the user
 * runtime's thread exit (__cubit_secondary_stack_release, weak: only in
 * programs whose Ada uses the secondary stack) gives back its secondary
 * stack. Signals are blocked; the call preserves the mapping in rdi/rsi.
 */
.text
.global __unmapself
.hidden __unmapself
.type   __unmapself,@function
.weak __cubit_secondary_stack_release
__unmapself:
	lea __cubit_secondary_stack_release(%rip),%rax
	test %rax,%rax
	jz 1f
	push %rdi
	push %rsi
	sub $8,%rsp              /* aligned for the call */
	call *%rax
	add $8,%rsp
	pop %rsi
	pop %rdi
1:	mov $116,%eax            /* RELEASE_OWNED_MEMORY(base, bytes) */
	syscall
	mov $91,%eax             /* THREAD_EXIT */
	syscall
	ud2
.size __unmapself,.-__unmapself
.section .note.GNU-stack,"",@progbits
