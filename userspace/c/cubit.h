/*
 * CuBit OS - C Userspace System Interface
 * Copyright (C) 2026 Jon Andrew
 *
 * Syscall numbers and inline wrapper for C programs running on CuBit.
 *
 * Syscall convention (x86-64):
 *   RAX = syscall number
 *   RDI = arg0, RSI = arg1, RDX = arg2, R10 = arg3, R8 = arg4, R9 = arg5
 *   Return value in RAX
 *
 * Note: The syscall instruction clobbers RCX (return address) and
 * R11 (RFLAGS), so arg3 uses R10 instead of RCX.
 */
#ifndef CUBIT_H
#define CUBIT_H

/*---------------------------------------------------------------------------
 * Basic types (guarded so they don't conflict with <stdint.h>)
 *---------------------------------------------------------------------------*/
#ifndef _STDINT_H
#ifndef _CUBIT_TYPES_DEFINED
#define _CUBIT_TYPES_DEFINED
typedef unsigned long       uint64_t;
typedef long                int64_t;
typedef unsigned int        uint32_t;
typedef int                 int32_t;
typedef unsigned short      uint16_t;
typedef short               int16_t;
typedef unsigned char       uint8_t;
typedef char                int8_t;
typedef long                intptr_t;
typedef unsigned long       uintptr_t;
#endif
#endif

#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned long       size_t;
typedef long                ssize_t;
#endif

#ifndef NULL
#define NULL ((void *)0)
#endif

/*---------------------------------------------------------------------------
 * Syscall Numbers
 *---------------------------------------------------------------------------*/
#define SYSCALL_EXIT            0
#define SYSCALL_READ            1
#define SYSCALL_CLOSE           2
#define SYSCALL_EXECVE          3
#define SYSCALL_FORK            4
#define SYSCALL_FSTAT           5
#define SYSCALL_GETPID          6
#define SYSCALL_KILL            7
#define SYSCALL_SBRK            8
#define SYSCALL_TIMES           9
#define SYSCALL_UNLINK          10
#define SYSCALL_WAIT            11
#define SYSCALL_WRITE           12
#define SYSCALL_OPEN            13
#define SYSCALL_INFO            15

/* IPC - Synchronous */
#define SYSCALL_SEND            16
#define SYSCALL_RECEIVE         17
#define SYSCALL_REPLY           18
#define SYSCALL_SEND_EVENT      19
#define SYSCALL_RECEIVE_EVENT   20
#define SYSCALL_CALL            21
#define SYSCALL_RECEIVE_NB      22

/* IPC - Async */
#define SYSCALL_SUBMIT          23
#define SYSCALL_WAIT_COMPLETION 24
#define SYSCALL_POLL_COMPLETION 25
#define SYSCALL_RECEIVE_EVENT_NB 26

/* Time */
#define SYSCALL_GETTIME         27
#define SYSCALL_SLEEP           28

/* Framebuffer */
#define SYSCALL_MAPFB           29

/* Port I/O (for userspace drivers) */
#define SYSCALL_INB             30
#define SYSCALL_OUTB            31
#define SYSCALL_INW             32
#define SYSCALL_OUTW            33
#define SYSCALL_INS16           34
#define SYSCALL_OUTS16          35

/* Capability-aware IPC */
#define SYSCALL_CAP_SEND        40
#define SYSCALL_CAP_CALL        41
#define SYSCALL_CAP_SUBMIT      42

/* Notification IPC */
#define SYSCALL_NOTIFY          43
#define SYSCALL_NOTIFY_WAIT     44
#define SYSCALL_NOTIFY_POLL     45
#define SYSCALL_BIND_NOTIFICATION   46
#define SYSCALL_UNBIND_NOTIFICATION 47
#define SYSCALL_REPLY_WAIT      48

/* Access Controller */
#define SYSCALL_CONTROLACCESS   100
#define SYSCALL_GETTICKET       101

/* Grants */
#define SYSCALL_GRANT           102
#define SYSCALL_REVOKE          103

/* Well-known capability slots */
#define CAP_SLOT_SELF           0
#define CAP_SLOT_FS             1
#define CAP_SLOT_KEYBOARD       2
#define CAP_SLOT_SELF_PROC      3
#define CAP_SLOT_ATA            10

/* Driver registration */
#define SYSCALL_REGISTER_DRIVER 2000

/*---------------------------------------------------------------------------
 * Standard Descriptors
 *---------------------------------------------------------------------------*/
#define STDIN   0
#define STDOUT  1
#define STDERR  2

/*---------------------------------------------------------------------------
 * Inline syscall wrapper
 *
 * Uses register variables for R10/R8/R9 since GCC's inline asm constraints
 * don't have dedicated letters for these registers.
 *---------------------------------------------------------------------------*/
static inline long cubit_syscall(long num, long a0, long a1, long a2,
                                  long a3, long a4, long a5)
{
    long ret;
    register long r10 __asm__("r10") = a3;
    register long r8  __asm__("r8")  = a4;
    register long r9  __asm__("r9")  = a5;

    __asm__ volatile (
        "syscall"
        : "=a"(ret)
        : "a"(num), "D"(a0), "S"(a1), "d"(a2),
          "r"(r10), "r"(r8), "r"(r9)
        : "rcx", "r11", "memory"
    );

    return ret;
}

/* Convenience wrappers for fewer arguments */
#define syscall0(num)                   cubit_syscall(num, 0, 0, 0, 0, 0, 0)
#define syscall1(num, a0)              cubit_syscall(num, (long)(a0), 0, 0, 0, 0, 0)
#define syscall2(num, a0, a1)          cubit_syscall(num, (long)(a0), (long)(a1), 0, 0, 0, 0)
#define syscall3(num, a0, a1, a2)      cubit_syscall(num, (long)(a0), (long)(a1), (long)(a2), 0, 0, 0)
#define syscall4(num, a0, a1, a2, a3)  cubit_syscall(num, (long)(a0), (long)(a1), (long)(a2), (long)(a3), 0, 0)
#define syscall6(num, a0, a1, a2, a3, a4, a5) \
    cubit_syscall(num, (long)(a0), (long)(a1), (long)(a2), (long)(a3), (long)(a4), (long)(a5))

/*---------------------------------------------------------------------------
 * System Call Wrappers
 *---------------------------------------------------------------------------*/
void cubit_exit(int code);
long cubit_write(int fd, const void *buf, size_t count);

/* Memory */
void *cubit_sbrk(intptr_t increment);

/* Heap allocation */
void *malloc(size_t size);
void *calloc(size_t nmemb, size_t size);
void *realloc(void *ptr, size_t size);
void  free(void *ptr);

/* Time */
uint64_t cubit_gettime_ms(void);
void     cubit_sleep_ms(uint32_t ms);

/* Framebuffer */
typedef struct {
    void    *addr;
    uint32_t width;
    uint32_t height;
    uint32_t pitch;
    uint32_t bpp;
} cubit_framebuffer_t;

long cubit_map_framebuffer(cubit_framebuffer_t *fb);

/* Keyboard input */
typedef struct {
    uint8_t scancode;   /* PS/2 Set 1 scan code (without break bit) */
    int     pressed;    /* 1 = pressed, 0 = released */
} cubit_key_event_t;

void cubit_keyboard_init(void);
void cubit_keyboard_poll(void);
int  cubit_keyboard_get(cubit_key_event_t *ev);

/* String functions */
void *memcpy(void *dest, const void *src, size_t n);
void *memmove(void *dest, const void *src, size_t n);
void *memset(void *s, int c, size_t n);
int   memcmp(const void *s1, const void *s2, size_t n);
size_t strlen(const char *s);
char  *strcpy(char *dest, const char *src);
char  *strncpy(char *dest, const char *src, size_t n);
int    strcmp(const char *s1, const char *s2);
int    strncmp(const char *s1, const char *s2, size_t n);
char  *strcat(char *dest, const char *src);
char  *strchr(const char *s, int c);
char  *strrchr(const char *s, int c);
char  *strstr(const char *haystack, const char *needle);
long   strtol(const char *nptr, char **endptr, int base);
unsigned long strtoul(const char *nptr, char **endptr, int base);
int    atoi(const char *nptr);
long   atol(const char *nptr);
int    abs(int x);

int    snprintf(char *str, size_t size, const char *fmt, ...);

/*---------------------------------------------------------------------------
 * Convenience: print a null-terminated string to serial console (STDOUT)
 *---------------------------------------------------------------------------*/
static inline void cubit_puts(const char *s)
{
    size_t len = 0;
    while (s[len]) len++;
    cubit_write(STDOUT, s, len);
}

#endif /* CUBIT_H */
