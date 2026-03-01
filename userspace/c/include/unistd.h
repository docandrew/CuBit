/*
 * CuBit OS - unistd.h stub
 */
#ifndef _UNISTD_H
#define _UNISTD_H

#include "cubit.h"

#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2

static inline unsigned int sleep(unsigned int seconds)
{
    cubit_sleep_ms(seconds * 1000);
    return 0;
}

#endif /* _UNISTD_H */
