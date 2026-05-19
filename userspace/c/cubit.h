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
#define SYSCALL_POLL_ANY_IPC    22

/* IPC - Async */
#define SYSCALL_SUBMIT          23
#define SYSCALL_WAIT_COMPLETION 24
#define SYSCALL_POLL_COMPLETION 25
#define SYSCALL_RECEIVE_EVENT_NB 26
#define SYSCALL_POLL_EVENT      26
#define SYSCALL_POLL_SERVICE_REQUEST 80

/* Time */
#define SYSCALL_GETTIME         27
#define SYSCALL_SLEEP           28

/* Framebuffer */
#define SYSCALL_MAPFB           29

/* Port I/O (for userspace drivers) */
#define SYSCALL_INP8            30
#define SYSCALL_OUTP8           31
#define SYSCALL_INP16           32
#define SYSCALL_OUTP16          33
#define SYSCALL_INPS16          34
#define SYSCALL_OUTPS16         35
#define SYSCALL_INP32           36
#define SYSCALL_OUTP32          37

/* Virtual-to-physical address translation */
#define SYSCALL_VIRT_TO_PHYS    50

/* Move reply cap from slot 63 to another slot (deferred replies) */
#define SYSCALL_SAVE_REPLY_CAP  51
#define SYSCALL_REPLY_CAP       52

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
#define SYSCALL_GRANT_VIA_CAP   106
#define SYSCALL_SET_WELL_KNOWN  107

/* Well-known service roles */
#define ROLE_FILESYSTEM         1
#define ROLE_PROCMGR            2

/* Well-known capability slots */
#define CAP_SLOT_SELF           0
#define CAP_SLOT_FS             1
#define CAP_SLOT_KEYBOARD       2
#define CAP_SLOT_SELF_PROC      3
#define CAP_SLOT_ATA            10
#define CAP_SLOT_MIXER          14
#define CAP_SLOT_MIXER_NTF      15
#define CAP_SLOT_CONFIG         20
#define CAP_SLOT_DESKTOP        21

/* Capability minting (for process managers) */
#define SYSCALL_MINT_CAP        72
#define SYSCALL_RESUME          73

/* Driver registration */
#define SYSCALL_REGISTER_DRIVER 2000

/* Sysinfo queries */
#define SYSINFO_REGISTERED_DRIVER 2000
#define DRIVER_CONFIG             11
#define DRIVER_IPCTEST            14
#define DRIVER_DESKTOP            15
#define DRIVER_DISPLAY            16

/* Config store IPC labels */
#define OP_CONFIG_GET     0x0600
#define OP_CONFIG_SET     0x0601
#define OP_CONFIG_DELETE  0x0602
#define OP_CONFIG_LIST    0x0603
#define OP_CONFIG_LOAD    0x0604
#define OP_CONFIG_SAVE    0x0605

/* Log store IPC labels */
#define OP_LOG_QUERY      0x0800
#define OP_LOG_CLEAR      0x0801

/* Stream IPC labels */
#define OP_STREAM_SUBSCRIBE   0x0700
#define OP_STREAM_UNSUBSCRIBE 0x0701
#define OP_STREAM_NOTIFY      0x0702
#define OP_STREAM_WAKEUP      0x0704
#define OP_STREAM_LIST        0x0705
#define OP_STREAM_AVAILABLE   0x0706
#define OP_STREAM_GONE        0x0707

/*---------------------------------------------------------------------------
 * ELF Capability Manifest (.cubit.caps section)
 *
 * Allows ELF binaries to declare required capabilities. The process manager
 * reads this section and mints capabilities into the spawned process.
 *
 * Header (8 bytes): magic(4) + version(2) + count(2)
 * Entry (16 bytes): reqType(1) + rights(1) + slot(1) + reserved(1) +
 *                   param0(4) + param1(8)
 *---------------------------------------------------------------------------*/
#define CUBIT_CAP_MAGIC      0x43424954  /* "CBIT" in little-endian */
#define CUBIT_ID_MAGIC       0x44494243  /* "CBID" LE */
#define CUBIT_STREAMS_MAGIC  0x54534243  /* "CBST" LE */

#define CUBIT_STREAM_DECL(stream_id, pages, type_tag, flags) \
    ((stream_id) & 0xFF), (((stream_id) >> 8) & 0xFF), \
    ((pages) & 0xFF), (((pages) >> 8) & 0xFF), \
    ((type_tag) & 0xFF), (((type_tag) >> 8) & 0xFF), \
    ((flags) & 0xFF), (((flags) >> 8) & 0xFF)

/* Request types */
#define CUBIT_REQ_FRAMEBUFFER  1
#define CUBIT_REQ_SERVICE      2
#define CUBIT_REQ_IOPORT       3
#define CUBIT_REQ_IRQ          4
#define CUBIT_REQ_DEVICE_MEM   5
#define CUBIT_REQ_PROCESS      6
#define CUBIT_REQ_STREAM       8
#define CUBIT_REQ_RESOURCE     9

/* Well-known stream IDs */
#define CUBIT_STREAM_STDIN    0x01
#define CUBIT_STREAM_STDOUT   0x02
#define CUBIT_STREAM_STDERR   0x03
#define CUBIT_STREAM_LOG      0x04
#define CUBIT_STREAM_METRIC   0x06
#define CUBIT_STREAM_HEALTH   0x09

/* Stream type tags */
#define CUBIT_TYPE_RAW        0x0000
#define CUBIT_TYPE_TEXT        0x0001

/* Rights bitmask */
#define CUBIT_RIGHT_R     0x01
#define CUBIT_RIGHT_W     0x02
#define CUBIT_RIGHT_RW    0x03
#define CUBIT_RIGHT_X     0x04
#define CUBIT_RIGHT_GRANT 0x08

/* Convenience macro: declare a framebuffer capability manifest */
/*---------------------------------------------------------------------------
 * ELF Filesystem Access Declaration (.cubit.access section)
 *
 * Allows ELF binaries to declare required filesystem paths. The process
 * manager reads this section and sends ACL entries to the FS server.
 *
 * Header (16 bytes): magic(4) + version(2) + count(2) + uid(2) + gid(2) +
 *                    trustFloor(1) + sandboxMode(1) + reserved(2)
 * Entry (80 bytes):  rights(1) + prefixLen(1) + flags(1) + reserved(1) +
 *                    uid(2) + gid(2) + prefix(64) + reserved64(8)
 *---------------------------------------------------------------------------*/
#define CUBIT_ACCESS_MAGIC  0x43434143  /* "CACC" in little-endian */
#define CUBIT_ACL_READ    0x01
#define CUBIT_ACL_WRITE   0x02
#define CUBIT_ACL_EXEC    0x04
#define CUBIT_ACL_CREATE  0x08

/* Service identifiers for .cubit.access section routing */
#define CUBIT_SERVICE_FS      0
#define CUBIT_SERVICE_CONFIG  1
#define CUBIT_SERVICE_SECRETS 2

/* Sandbox modes for .cubit.access header byte 13 */
#define CUBIT_SANDBOX_NONE        0
#define CUBIT_SANDBOX_RUN_FOLDER  1
#define CUBIT_SANDBOX_APP_FOLDER  2

#define CUBIT_MANIFEST_FRAMEBUFFER(slot_num) \
    static const unsigned char __cubit_manifest[] \
        __attribute__((section(".cubit.caps"), used)) = { \
        0x54, 0x49, 0x42, 0x43, \
        0x01, 0x00, 0x01, 0x00, \
        0x01, 0x03, (slot_num), 0x00, \
        0x00, 0x00, 0x00, 0x00, \
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  \
    };

/* Convenience macro: declare a resource quota manifest entry.
 * slot       = capability slot number
 * max_frames = max physical frames (0 = unlimited)
 * cpu_q_us   = CPU quota in microseconds per period (0 = unlimited)
 * cpu_p_us   = CPU period in microseconds
 * Packed param1: cpu_q_us in low 32 bits, cpu_p_us in high 32 bits. */
#define CUBIT_MANIFEST_RESOURCE_ENTRY(slot, max_frames, cpu_q_us, cpu_p_us) \
    0x09, 0x01, (slot), 0x00, \
    ((max_frames) & 0xFF), (((max_frames) >> 8) & 0xFF), \
    (((max_frames) >> 16) & 0xFF), (((max_frames) >> 24) & 0xFF), \
    ((cpu_q_us) & 0xFF), (((cpu_q_us) >> 8) & 0xFF), \
    (((cpu_q_us) >> 16) & 0xFF), (((cpu_q_us) >> 24) & 0xFF), \
    ((cpu_p_us) & 0xFF), (((cpu_p_us) >> 8) & 0xFF), \
    (((cpu_p_us) >> 16) & 0xFF), (((cpu_p_us) >> 24) & 0xFF)

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

/* Mouse input */
typedef struct {
    int16_t dx, dy;
    int8_t  dz;
    uint8_t buttons;    /* bit 0=L, 1=R, 2=M */
} cubit_mouse_event_t;

void cubit_mouse_init(void);
void cubit_mouse_poll(void);
int  cubit_mouse_get(cubit_mouse_event_t *ev);

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

/* Config store client API */
int cubit_config_get(const char *key, void *buf,
                     size_t buf_size, size_t *out_len);
int cubit_config_set(const char *key, const void *value,
                     size_t value_len);
int cubit_config_delete(const char *key);
int cubit_config_list(const char *prefix, char *buf,
                      size_t buf_size, int *out_count);

/* Scheme resolution */
typedef struct {
    uint64_t driver_id;
    uint64_t cap_slot;
    uint64_t pid;
    int      found;
} cubit_scheme_info_t;

int cubit_resolve_scheme(const char *name, cubit_scheme_info_t *info);

/* File open flags */
#define O_RDONLY   0
#define O_WRONLY   1
#define O_RDWR     2
#define O_CREAT    64
#define O_TRUNC    512

/*---------------------------------------------------------------------------
 * I/O Streams - Producer-owned ring buffer model
 *
 * Same shared memory layout as the Ada CuBit.Streams package.
 * A C producer can serve Ada subscribers and vice versa.
 *---------------------------------------------------------------------------*/

/* Stream IDs */
#define CUBIT_STREAM_STDIN    0x01
#define CUBIT_STREAM_STDOUT   0x02
#define CUBIT_STREAM_STDERR   0x03

/* Stream type tags */
#define CUBIT_TYPE_RAW_BYTES  0x0000
#define CUBIT_TYPE_TEXT_LINE  0x0001

/* IPC labels for stream subscription */
#define OP_STREAM_SUBSCRIBE   0x0700
#define OP_STREAM_UNSUBSCRIBE 0x0701

/* Producer API */
void     cubit_stream_create(uint16_t stream_id, unsigned pages,
                             uint16_t type_tag);
uint32_t cubit_stream_write(uint16_t stream_id, const void *data,
                            uint32_t len, uint16_t type_tag);
void     cubit_stream_print(uint16_t stream_id, const char *msg);
int      cubit_stream_handle_subscription(void);
void     cubit_stream_flush(uint16_t stream_id);

#endif /* CUBIT_H */
