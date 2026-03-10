/*
 * CuBit OS - C Standard I/O Library
 * Copyright (C) 2026 Jon Andrew
 *
 * Provides fopen/fread/fseek/ftell/fclose by sending IPC messages
 * to the userspace filesystem server.
 *
 * Protocol:
 *   Client grants a shared buffer to the FS server, then sends
 *   OP_OPEN/OP_READ/OP_SEEK/OP_CLOSE messages via SYSCALL_CAP_CALL.
 *   The FS server reads/writes the grant buffer and replies with results.
 *
 * All file operations route through CAP_SLOT_FS. The FS server is the
 * namespace authority — it parses @scheme: prefixes and routes to
 * backends (ramdisk, ATA, etc.) via its own capabilities.
 */
#include "cubit.h"

/* Filesystem server PID (used for grants, must match kernel config.ads) */
#define FS_SERVER_PID   10

/* IPC operation labels (must match kernel/src/ipc_labels.ads) */
#define OP_OPEN     0x0001
#define OP_CLOSE    0x0002
#define OP_READ     0x0003
#define OP_WRITE    0x0004
#define OP_SEEK     0x0006
#define REPLY_OK    0xF000
#define REPLY_ERR   0xF001

/* Grant permissions */
#define GRANT_READ      0
#define GRANT_READWRITE 1

/* Seek whence values */
#define CUBIT_SEEK_SET  0
#define CUBIT_SEEK_CUR  1
#define CUBIT_SEEK_END  2

/* Grant buffer size per file (8 pages = 32KB for bulk reads) */
#define GRANT_BUF_SIZE  32768
#define GRANT_BUF_PAGES 8

/* Maximum open files */
#define MAX_OPEN_FILES  16

/* Page size */
#define PAGE_SIZE       4096

/*
 * IPC Message structure — must match kernel's Process.Message layout.
 * MessageTag: label(u32) + length(u8) + flags(u8) + badge(u16) = 8 bytes
 * Words: 4 x u64 = 32 bytes
 * Total: 40 bytes
 */
typedef struct __attribute__((packed)) {
    uint32_t label;
    uint8_t  length;
    uint8_t  flags;
    uint16_t badge;
} ipc_tag_t;

typedef struct __attribute__((packed)) {
    ipc_tag_t tag;
    uint64_t  capBadge;
    uint64_t  words[4];
} ipc_message_t;

/*
 * FILE structure
 */
struct _FILE {
    int      active;        /* 1 if this entry is in use */
    int      handle;        /* file handle from server */
    uint64_t grant_id;      /* grant ID for data buffer */
    void    *grant_buf;     /* local address of grant buffer */
    uint64_t offset;        /* current file position */
};

typedef struct _FILE FILE;

/* File table */
static FILE file_table[MAX_OPEN_FILES];

/* Align pointer up to PAGE_SIZE boundary */
static void *page_align(void *ptr)
{
    uintptr_t addr = (uintptr_t)ptr;
    addr = (addr + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);
    return (void *)addr;
}

/* Allocate a page-aligned buffer of GRANT_BUF_SIZE bytes */
static void *alloc_grant_buf(void)
{
    /* Allocate GRANT_BUF_SIZE + PAGE_SIZE to guarantee alignment */
    void *raw = cubit_sbrk(GRANT_BUF_SIZE + PAGE_SIZE);
    if ((long)raw == -1)
        return NULL;
    return page_align(raw);
}

/* Send an IPC call to the filesystem server and get the reply */
static int fs_call(ipc_message_t *msg)
{
    long ret = syscall2(SYSCALL_CAP_CALL, CAP_SLOT_FS, msg);
    if (ret == (long)(-1UL))
        return -1;
    return 0;
}

/*
 * fopen - Open a file on the filesystem server
 *
 * Sends the full path (including any @scheme: prefix) to the FS server
 * which handles namespace routing internally.
 */
FILE *fopen(const char *path, const char *mode)
{
    (void)mode;  /* mode ignored for now (read-only FS) */

    if (!path)
        return NULL;

    /* Find a free file table entry */
    int slot = -1;
    for (int i = 0; i < MAX_OPEN_FILES; i++) {
        if (!file_table[i].active) {
            slot = i;
            break;
        }
    }
    if (slot < 0)
        return NULL;

    FILE *f = &file_table[slot];

    /* Allocate a page-aligned grant buffer */
    f->grant_buf = alloc_grant_buf();
    if (!f->grant_buf)
        return NULL;

    /* Create a read-write grant to the FS server */
    long gid = syscall4(SYSCALL_GRANT, FS_SERVER_PID,
                         f->grant_buf, GRANT_BUF_PAGES, GRANT_READWRITE);
    if (gid == (long)(-1UL)) {
        return NULL;
    }
    f->grant_id = (uint64_t)gid;

    /* Copy full path into grant buffer */
    size_t pathlen = strlen(path);
    if (pathlen > GRANT_BUF_SIZE - 1)
        pathlen = GRANT_BUF_SIZE - 1;
    memcpy(f->grant_buf, path, pathlen);

    /* Send OP_OPEN to FS server */
    ipc_message_t msg;
    msg.tag.label  = OP_OPEN;
    msg.tag.length = 3;
    msg.tag.flags  = 0;
    msg.tag.badge  = 0;
    msg.words[0] = f->grant_id;    /* grant_id (where path is) */
    msg.words[1] = pathlen;        /* path length */
    msg.words[2] = 0;              /* flags (unused) */
    msg.words[3] = 0;

    if (fs_call(&msg) < 0) {
        syscall1(SYSCALL_REVOKE, f->grant_id);
        return NULL;
    }

    /* Check reply */
    if (msg.tag.label != REPLY_OK) {
        syscall1(SYSCALL_REVOKE, f->grant_id);
        return NULL;
    }

    f->handle = (int)msg.words[0];
    f->offset = 0;
    f->active = 1;

    return f;
}

/*
 * fread - Read data from an open file
 *
 * Sends OP_READ to the server. The server writes file data
 * into our grant buffer, and we copy it to the user's buffer.
 */
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream)
{
    if (!stream || !stream->active || !ptr)
        return 0;

    size_t total = size * nmemb;
    if (total == 0)
        return 0;

    size_t bytes_copied = 0;
    char *dst = (char *)ptr;

    while (bytes_copied < total) {
        /* Read in chunks that fit in the grant buffer */
        size_t chunk = total - bytes_copied;
        if (chunk > GRANT_BUF_SIZE)
            chunk = GRANT_BUF_SIZE;

        ipc_message_t msg;
        msg.tag.label  = OP_READ;
        msg.tag.length = 3;
        msg.tag.flags  = 0;
        msg.tag.badge  = 0;
        msg.words[0] = (uint64_t)stream->handle;
        msg.words[1] = stream->grant_id;
        msg.words[2] = chunk;
        msg.words[3] = 0;

        if (fs_call(&msg) < 0)
            break;

        if (msg.tag.label != REPLY_OK)
            break;

        uint64_t bytes_read = msg.words[0];
        if (bytes_read == 0)
            break;  /* EOF */

        /* Copy from grant buffer to user buffer */
        memcpy(dst + bytes_copied, stream->grant_buf, (size_t)bytes_read);
        bytes_copied += (size_t)bytes_read;
        stream->offset += bytes_read;

        if (bytes_read < chunk)
            break;  /* Short read = EOF */
    }

    return bytes_copied / size;
}

/*
 * fwrite - Write data to an open file
 *
 * Copies data into the grant buffer and sends OP_WRITE to the server.
 */
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream)
{
    if (!ptr)
        return 0;

    /* If no valid stream, write to stdout (for printf/debug output) */
    if (!stream || !stream->active) {
        size_t total = size * nmemb;
        if (total > 0)
            syscall3(SYSCALL_WRITE, 1 /*STDOUT*/, ptr, total);
        return nmemb;
    }

    size_t total = size * nmemb;
    if (total == 0)
        return 0;

    size_t bytes_copied = 0;
    const char *src = (const char *)ptr;

    while (bytes_copied < total) {
        /* Write in chunks that fit in the grant buffer */
        size_t chunk = total - bytes_copied;
        if (chunk > GRANT_BUF_SIZE)
            chunk = GRANT_BUF_SIZE;

        /* Copy data into grant buffer */
        memcpy(stream->grant_buf, src + bytes_copied, chunk);

        ipc_message_t msg;
        msg.tag.label  = OP_WRITE;
        msg.tag.length = 3;
        msg.tag.flags  = 0;
        msg.tag.badge  = 0;
        msg.words[0] = (uint64_t)stream->handle;
        msg.words[1] = stream->grant_id;
        msg.words[2] = chunk;
        msg.words[3] = 0;

        if (fs_call(&msg) < 0)
            break;

        if (msg.tag.label != REPLY_OK)
            break;

        uint64_t bytes_written = msg.words[0];
        if (bytes_written == 0)
            break;

        bytes_copied += (size_t)bytes_written;
        stream->offset += bytes_written;

        if (bytes_written < chunk)
            break;  /* Short write */
    }

    return bytes_copied / size;
}

/*
 * fseek - Seek to a position in the file
 */
int fseek(FILE *stream, long offset, int whence)
{
    if (!stream || !stream->active)
        return -1;

    uint64_t w;
    switch (whence) {
        case 0: w = CUBIT_SEEK_SET; break;  /* SEEK_SET */
        case 1: w = CUBIT_SEEK_CUR; break;  /* SEEK_CUR */
        case 2: w = CUBIT_SEEK_END; break;  /* SEEK_END */
        default: return -1;
    }

    ipc_message_t msg;
    msg.tag.label  = OP_SEEK;
    msg.tag.length = 3;
    msg.tag.flags  = 0;
    msg.tag.badge  = 0;
    msg.words[0] = (uint64_t)stream->handle;
    msg.words[1] = (uint64_t)offset;
    msg.words[2] = w;
    msg.words[3] = 0;

    if (fs_call(&msg) < 0)
        return -1;

    if (msg.tag.label != REPLY_OK)
        return -1;

    stream->offset = msg.words[0];
    return 0;
}

/*
 * ftell - Return current file position
 */
long ftell(FILE *stream)
{
    if (!stream || !stream->active)
        return -1;
    return (long)stream->offset;
}

/*
 * fclose - Close an open file
 */
int fclose(FILE *stream)
{
    if (!stream || !stream->active)
        return -1;

    ipc_message_t msg;
    msg.tag.label  = OP_CLOSE;
    msg.tag.length = 1;
    msg.tag.flags  = 0;
    msg.tag.badge  = 0;
    msg.words[0] = (uint64_t)stream->handle;
    msg.words[1] = 0;
    msg.words[2] = 0;
    msg.words[3] = 0;

    fs_call(&msg);

    /* Revoke grant */
    syscall1(SYSCALL_REVOKE, stream->grant_id);

    stream->active = 0;
    return 0;
}
