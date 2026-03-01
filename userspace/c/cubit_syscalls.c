/*
 * CuBit OS - C Syscall Wrappers
 * Copyright (C) 2026 Jon Andrew
 */
#include "cubit.h"

void cubit_exit(int code)
{
    syscall1(SYSCALL_EXIT, code);
    __builtin_unreachable();
}

long cubit_write(int fd, const void *buf, size_t count)
{
    return syscall3(SYSCALL_WRITE, fd, buf, count);
}
