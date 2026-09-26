#include <cubit/debug.h>

/* See <cubit/debug.h>: the kernel console, a temporary debugging channel. */
void cubit_debug_write(const char *text, size_t length)
{
	unsigned long ret;
	__asm__ __volatile__ ("syscall" : "=a"(ret)
		: "a"(12UL), "D"(1UL), "S"(text), "d"(length)
		: "rcx", "r11", "memory");
	(void)ret;
}
