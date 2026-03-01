/*
 * CuBit OS - sys/stat.h stub
 */
#ifndef _SYS_STAT_H
#define _SYS_STAT_H

#include "cubit.h"

typedef unsigned int mode_t;

static inline int mkdir(const char *pathname, mode_t mode)
{
    (void)pathname; (void)mode;
    return -1;
}

#endif /* _SYS_STAT_H */
