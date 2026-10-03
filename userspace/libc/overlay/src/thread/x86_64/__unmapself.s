/* musl supplies the exact owned stack/TLS mapping in rdi/rsi after
 * unlinking the detached thread. Release it, then exit without touching
 * either stack or TLS. Both syscalls run on the kernel stack; SYSRET does
 * not dereference user RSP. The clear-tid word is musl's process-global
 * thread-list lock, outside the released mapping (see clone.s).
 * If release is refused, still exit so the thread-list lock is cleared;
 * retained memory is then reclaimed with the process.
 */
.text
.global __unmapself
.hidden __unmapself
.type   __unmapself,@function
__unmapself:
	mov $116,%eax            /* RELEASE_OWNED_MEMORY(base, bytes) */
	syscall
	mov $91,%eax             /* THREAD_EXIT */
	syscall
	ud2
.size __unmapself,.-__unmapself
.section .note.GNU-stack,"",@progbits
