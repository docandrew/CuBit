/*
 * musl's pthread_getattr_np (MIT, see COPYRIGHT) for CuBit. The main
 * thread's stack is described by the start code (crt/crt1.c: the loader's
 * initial stack pointer and the PT_GNU_STACK size contract). musl instead
 * probes downward from the auxiliary vector with mremap, which assumes the
 * vector sits on the stack and that mremap reports mapped pages; neither
 * holds here.
 */
#define _GNU_SOURCE
#include "pthread_impl.h"

extern unsigned long __cubit_stack_top, __cubit_stack_size;

int pthread_getattr_np(pthread_t t, pthread_attr_t *a)
{
	*a = (pthread_attr_t){0};
	a->_a_detach = t->detach_state>=DT_DETACHED;
	a->_a_guardsize = t->guard_size;
	if (t->stack) {
		a->_a_stackaddr = (uintptr_t)t->stack;
		a->_a_stacksize = t->stack_size;
	} else {
		a->_a_stackaddr = __cubit_stack_top;
		a->_a_stacksize = __cubit_stack_size;
	}
	return 0;
}
