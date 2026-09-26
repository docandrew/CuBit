/* CuBit: a detached thread exiting. There is no munmap yet (the stack
 * stays mapped); end the thread. */
.text
.global __unmapself
.type   __unmapself,@function
__unmapself:
	mov $91,%eax             /* THREAD_EXIT */
	syscall
	hlt
