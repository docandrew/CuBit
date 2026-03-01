/*
 * CuBit OS - Time Syscall Wrappers
 * Copyright (C) 2026 Jon Andrew
 */
#include "cubit.h"

uint64_t cubit_gettime_ms(void)
{
    return (uint64_t)syscall0(SYSCALL_GETTIME);
}

void cubit_sleep_ms(uint32_t ms)
{
    syscall1(SYSCALL_SLEEP, ms);
}
