/*
 * CuBit OS - Framebuffer Access
 * Copyright (C) 2026 Jon Andrew
 */
#include "cubit.h"

/* Sysinfo query IDs matching kernel/src/sysinfo.ads */
#define SYSINFO_FB_WIDTH   1100
#define SYSINFO_FB_HEIGHT  1101
#define SYSINFO_FB_PITCH   1102
#define SYSINFO_FB_BPP     1103

long cubit_map_framebuffer(cubit_framebuffer_t *fb)
{
    long addr = syscall0(SYSCALL_MAPFB);
    if (addr == -1) return -1;

    fb->addr   = (void *)addr;
    fb->width  = (uint32_t)syscall2(SYSCALL_INFO, SYSINFO_FB_WIDTH, 0);
    fb->height = (uint32_t)syscall2(SYSCALL_INFO, SYSINFO_FB_HEIGHT, 0);
    fb->pitch  = (uint32_t)syscall2(SYSCALL_INFO, SYSINFO_FB_PITCH, 0);
    fb->bpp    = (uint32_t)syscall2(SYSCALL_INFO, SYSINFO_FB_BPP, 0);

    return 0;
}
