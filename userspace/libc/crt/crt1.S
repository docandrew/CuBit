/*
 * CuBit libc program entry: the shared _start
 * (userspace/runtime/start/cubit_start.S), which keeps the launch block's
 * lengths and calls __cubit_language_start (the initial stack pointer, the
 * payload's length) with the stack aligned: that is the libc's Ada start
 * itself (CuBit.Libc_Start, userspace/libc/ada).
 */
	.intel_syntax noprefix
	.include "cubit_start.S"
	.att_syntax prefix
