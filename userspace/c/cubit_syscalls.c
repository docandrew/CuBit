/*
 * CuBit OS - C Syscall Wrappers
 * Copyright (C) 2026 Jon Andrew
 */
#include "cubit.h"

int cubit_cap_submit(uint64_t cap_slot, const cubit_async_message_t *message,
                     uint64_t token)
{
    uint64_t tag;

    if (message == NULL || token == CUBIT_NO_COMPLETION_TOKEN)
        return 0;

    memcpy(&tag, &message->tag, sizeof(tag));
    return syscall6(SYSCALL_CAP_SUBMIT, cap_slot, tag,
                    message->words[0], message->words[1],
                    message->words[2], token) == 1;
}

int cubit_poll_completion(cubit_completion_t *completion)
{
    if (completion == NULL)
        return 0;
    return syscall1(SYSCALL_POLL_COMPLETION, completion) == 1;
}

void cubit_exit(int code)
{
    syscall1(SYSCALL_EXIT, code);
    __builtin_unreachable();
}

long cubit_write(int fd, const void *buf, size_t count)
{
    return syscall3(SYSCALL_WRITE, fd, buf, count);
}
