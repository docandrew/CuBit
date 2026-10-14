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

/* Capability handles (CAP-002): opaque per-kind structs, never slots. */
#include "cubit_handles.h"

/*---------------------------------------------------------------------------
 * Syscall Numbers
 *---------------------------------------------------------------------------*/
#define SYSCALL_EXIT            0
#define SYSCALL_GETPID          6
#define SYSCALL_KILL            7
#define SYSCALL_SBRK            8
#define SYSCALL_WRITE           12

/* IPC - Synchronous */
#define SYSCALL_RECEIVE         17
#define SYSCALL_REPLY           18
#define SYSCALL_SEND_EVENT      19
#define SYSCALL_RECEIVE_UNTIL_MONOTONIC_MILLISECOND 21
#define SYSCALL_POLL_ANY_IPC    22

/* IPC - Async */
#define SYSCALL_WAIT_COMPLETION 24
#define SYSCALL_POLL_COMPLETION 25
#define SYSCALL_RECEIVE_EVENT_NB 26
#define SYSCALL_POLL_EVENT      26
#define SYSCALL_POLL_SERVICE_REQUEST 80

/* Time */
#define SYSCALL_GETTIME         27
#define SYSCALL_SLEEP           28
#define SYSCALL_YIELD           118
/* Sleep until an absolute time on the monotonic microsecond clock (114). */
#define SYSCALL_SLEEP_UNTIL_MONOTONIC_MICROSECOND 119

/* Framebuffer */
#define SYSCALL_MAPFB           29

/* Port I/O (for userspace drivers) */
#define SYSCALL_INP8            30
#define SYSCALL_OUTP8           31
#define SYSCALL_INP16           32
#define SYSCALL_OUTP16          33
#define SYSCALL_INP32           36
#define SYSCALL_OUTP32          37

/* Save the current request's reply as a handle; answer a saved reply. */
#define SYSCALL_MOVE_REPLY_CAPABILITY  51
#define SYSCALL_REPLY_AND_CONSUME_REPLY_CAPABILITY 52

/* Capability-aware IPC */
#define SYSCALL_SEND_VIA_ENDPOINT_CAPABILITY   129
#define SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY   128
/* Synchronous calls take an absolute monotonic-ms deadline, or this, spelled
 * out: there is no default (docs/ipc-fastpath.md, "Call deadlines"). */
#define CUBIT_WAIT_FOREVER UINT64_MAX
/* Call outcomes only the kernel gives; a server's reply may not use them. */
#define CUBIT_KERNEL_REPLY_FIRST 0xFFFF0000u
#define CUBIT_REPLY_TIMEOUT      0xFFFF0001u
#define SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY 42

#define SYSCALL_REPLY_WAIT      48

/* Non-consuming readiness: 1 = work, 0 = deadline, UINT64_MAX = unavailable. */
#define SYSCALL_WAIT_FOR_IPC_OR_COMPLETION_UNTIL_MONOTONIC_MILLISECOND 113

/* Shared-memory grants */
#define SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID      102
#define SYSCALL_REVOKE_SHARED_MEMORY_GRANT                     103
#define SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY      106
/* Owned inactive slot => 0; active/revoking => generation; error => -1. */
#define SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION       108
#define SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT                    109
#define SYSCALL_REVOKE_SHARED_MEMORY_GRANT_REFERENCE           110
#define SYSCALL_RETURN_SHARED_MEMORY_GRANT_ACQUISITION         111
#define SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT_VIA_CAPABILITY     112

/* Scheduler latency contracts */
#define SYSCALL_SET_LATENCY_CONTRACT 81
#define LATENCY_BACKGROUND      0
#define LATENCY_NORMAL          1
#define LATENCY_INTERACTIVE     2
#define LATENCY_REALTIME        3

/* Kernel trace controls */
#define SYSCALL_TRACE_RESET     82
#define SYSCALL_TRACE_SUMMARY   83

/* Asynchronous capability IPC. Message layout matches CuBit.Messages. */
#define CUBIT_NO_COMPLETION_TOKEN (~(uint64_t)0)
#define CUBIT_COMPLETION_OK             0
#define CUBIT_COMPLETION_TARGET_DIED    1
#define CUBIT_COMPLETION_CANCELLED      2
#define CUBIT_COMPLETION_QUEUE_OVERFLOW 3

typedef struct {
    uint32_t label;
    uint8_t length;
    /* One byte, two fields (Capability_ABI): the protocol's own flags in the
     * low nibble; the IPC layer's in the high nibble (bit 4 accepts a
     * capability; 5 transferred, 6 refused, 7 offered are the kernel's, and
     * a send claiming them is refused). */
    uint8_t protocol : 4;
    uint8_t ipc : 4;
    uint16_t reserved;
} cubit_async_message_tag_t;
#ifdef __cplusplus
static_assert(sizeof(cubit_async_message_tag_t) == 8, "message tag is one word");
#else
_Static_assert(sizeof(cubit_async_message_tag_t) == 8, "message tag is one word");
#endif

typedef struct {
    cubit_async_message_tag_t tag;
    uint64_t authority_tag;
    uint64_t words[4];
} cubit_async_message_t;

typedef struct {
    uint64_t request_id;
    uint64_t token;
    cubit_async_message_t message;
    uint64_t from;
    uint64_t status;
    uint8_t valid;
    uint8_t reserved[7];
} cubit_completion_t;

typedef char cubit_async_tag_size_must_be_8[
    sizeof(cubit_async_message_tag_t) == 8 ? 1 : -1];
typedef char cubit_async_message_size_must_be_48[
    sizeof(cubit_async_message_t) == 48 ? 1 : -1];
typedef char cubit_reserved_header_offset_must_be_6[
    __builtin_offsetof(cubit_async_message_tag_t, reserved) == 6 ? 1 : -1];
typedef char cubit_authority_tag_offset_must_be_8[
    __builtin_offsetof(cubit_async_message_t, authority_tag) == 8 ? 1 : -1];
typedef char cubit_message_words_offset_must_be_16[
    __builtin_offsetof(cubit_async_message_t, words) == 16 ? 1 : -1];
typedef char cubit_completion_size_must_be_88[
    sizeof(cubit_completion_t) == 88 ? 1 : -1];


/* Service roles of the kernel's service registry (read from its
   publication, CuBit.System_Facts; there is no query system call). */
#define DRIVER_CONFIG             11
#define DRIVER_IPCTEST            14
#define DRIVER_DESKTOP            15
#define DRIVER_DISPLAY            16
#define DRIVER_GPU                17
#define DRIVER_CCL_TEST           18

/* Stream IPC labels */
#define OP_STREAM_SUBSCRIBE   0x0700
#define OP_STREAM_UNSUBSCRIBE 0x0701
#define OP_STREAM_NOTIFY      0x0702
#define OP_STREAM_WAKEUP      0x0704
#define OP_STREAM_LIST        0x0705
#define OP_STREAM_AVAILABLE   0x0706
#define OP_STREAM_GONE        0x0707

/*---------------------------------------------------------------------------
 * ELF identity and stream declarations. A program's capabilities are
 * declared in its typed CCL manifest (manifest.ccl), never in C.
 *---------------------------------------------------------------------------*/
#define CUBIT_ID_MAGIC       0x44494243  /* "CBID" LE */
#define CUBIT_STREAMS_MAGIC  0x54534243  /* "CBST" LE */

#define CUBIT_STREAM_DECL(stream_id, pages, type_tag, flags) \
    ((stream_id) & 0xFF), (((stream_id) >> 8) & 0xFF), \
    ((pages) & 0xFF), (((pages) >> 8) & 0xFF), \
    ((type_tag) & 0xFF), (((type_tag) >> 8) & 0xFF), \
    ((flags) & 0xFF), (((flags) >> 8) & 0xFF)

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

/* Seven-value register ABI for async IPC: R12 carries the completion token.
 * The kernel preserves R12; GCC preserves its caller's R12 around this helper.
 */
static inline long cubit_syscall7(long num, long a0, long a1, long a2,
                                 long a3, long a4, long a5, long a6)
{
    long ret;
    register long r10 __asm__("r10") = a3;
    register long r8  __asm__("r8") = a4;
    register long r9  __asm__("r9") = a5;
    register long r12 __asm__("r12") = a6;
    __asm__ volatile (
        "syscall"
        : "=a"(ret)
        : "a"(num), "D"(a0), "S"(a1), "d"(a2),
          "r"(r10), "r"(r8), "r"(r9), "r"(r12)
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

/* Producer API (the libc's CuBit.Libc_Streams) */
void     cubit_stream_create(uint16_t stream_id, unsigned pages,
                             uint16_t type_tag);
uint32_t cubit_stream_write(uint16_t stream_id, const void *data,
                            uint32_t len, uint16_t type_tag);
int      cubit_stream_handle_message(long from, const void *msg);
extern int cubit_stream_poll_on_write;

#endif /* CUBIT_H */
