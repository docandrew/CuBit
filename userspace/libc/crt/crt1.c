/*
 * CuBit libc program start. CuBit's loader enters _start with the stack
 * pointer at the top of the stack and nothing on it: no argc, argv,
 * environment or auxiliary vector. Build the minimal block musl's
 * __libc_start_main reads: argv = { program name }, an empty environment,
 * and the auxiliary entries it needs (program headers for the TLS template,
 * page size, 16 random bytes for the stack protector).
 */
#include <elf.h>
#include <stddef.h>

int main(int, char **, char **);
int __libc_start_main(int (*)(int, char **, char **), int, char **,
	void (*)(void), void (*)(void), void (*)(void));

extern const Elf64_Ehdr __ehdr_start;

/* The main thread's stack: its top (the loader's initial stack pointer)
 * and its size (the PT_GNU_STACK contract), for pthread_getattr_np. */
unsigned long __cubit_stack_top, __cubit_stack_size;

static char program_name[] = "cubit-program";
static unsigned char random_bytes[16];
static unsigned long start_block[16];

__attribute__((used)) void _start_c(unsigned long initial_sp)
{
	__cubit_stack_top = (initial_sp + 4095) & ~4095UL;
	for (size_t i = 0; i < sizeof random_bytes; i += 8) {
		unsigned long v = 0;
		unsigned char ok = 0;
		for (int t = 0; t < 16 && !ok; t++)
			__asm__ __volatile__ ("rdrand %0; setc %1" : "=r"(v), "=qm"(ok));
		if (!ok) __asm__ __volatile__ ("rdtsc; shl $32, %%rdx; or %%rdx, %0"
			: "=a"(v) : : "rdx");
		__builtin_memcpy(random_bytes + i, &v, 8);
	}
	const Elf64_Ehdr *eh = &__ehdr_start;
	const Elf64_Phdr *ph = (const void *)((const char *)eh + eh->e_phoff);
	__cubit_stack_size = 1UL << 20;
	for (int k = 0; k < eh->e_phnum; k++)
		if (ph[k].p_type == PT_GNU_STACK && ph[k].p_memsz)
			__cubit_stack_size = ph[k].p_memsz;
	int i = 0;
	start_block[i++] = (unsigned long)program_name;     /* argv[0] */
	start_block[i++] = 0;                               /* end of argv */
	start_block[i++] = 0;                               /* end of envp */
	start_block[i++] = AT_PHDR;  start_block[i++] = (unsigned long)eh + eh->e_phoff;
	start_block[i++] = AT_PHNUM; start_block[i++] = eh->e_phnum;
	start_block[i++] = AT_PHENT; start_block[i++] = eh->e_phentsize;
	start_block[i++] = AT_PAGESZ; start_block[i++] = 4096;
	start_block[i++] = AT_RANDOM; start_block[i++] = (unsigned long)random_bytes;
	start_block[i++] = AT_NULL;  start_block[i++] = 0;
	__libc_start_main(main, 1, (char **)start_block, 0, 0, 0);
	for (;;) ;
}

__asm__(
	".text\n"
	".global _start\n"
	".type _start,@function\n"
	"_start:\n"
	"	xor %ebp,%ebp\n"
	"	mov %rsp,%rdi\n"
	"	and $-16,%rsp\n"
	"	call _start_c\n"
	"	hlt\n");
