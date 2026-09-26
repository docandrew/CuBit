/* CuBit: the thread pointer is the FS base, set in user mode (FSGSBASE);
 * the kernel saves and restores it per thread. */
.text
.global __set_thread_area
.hidden __set_thread_area
.type __set_thread_area,@function
__set_thread_area:
	wrfsbase %rdi
	xor %eax,%eax
	ret
