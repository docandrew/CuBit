/*
 * CuBit OS - I/O Streams C Implementation
 * Copyright (C) 2026 Jon Andrew
 *
 * Producer-owned ring buffer model for structured process output.
 * Mirrors the Ada CuBit.Streams package: same shared memory layout,
 * same header offsets, same entry format. A C producer can serve
 * Ada subscribers and vice versa.
 *
 * Ring buffer layout:
 *   Offset 0x000: StreamHeader (128 bytes)
 *   Offset 0x080: Ring data (capacity bytes)
 *
 * Ring entry format:
 *   [length:u16][typeTag:u16][payload bytes][pad to 8-byte alignment]
 *
 * Sentinel entry:
 *   length=0xFFFF means "skip to offset 0". Written when an entry
 *   would straddle the buffer boundary.
 */
#include "cubit.h"

/*---------------------------------------------------------------------------
 * Constants matching Ada CuBit.Streams
 *---------------------------------------------------------------------------*/
#define STREAM_MAGIC         0x53545249  /* "STRI" */
#define HEADER_SIZE          128
#define DATA_OFFSET          128
#define ENTRY_HEADER_SIZE    4
#define MAX_STREAMS          4
#define MAX_SUBSCRIBERS      8
#define SUBSCRIBER_ENTRY_SIZE 12
#define SENTINEL_LENGTH      0xFFFF

/* Header field offsets */
#define HDR_MAGIC            0x00
#define HDR_VERSION          0x04
#define HDR_FLAGS            0x06
#define HDR_SUBSCRIBER_COUNT 0x08
#define HDR_PRODUCER_IDX     0x0C
#define HDR_CAPACITY         0x10
#define HDR_DEFAULT_TYPE_TAG 0x14
#define HDR_OVERFLOW_POLICY  0x16
#define HDR_STREAM_ID        0x18

/* Subscriber table starts at offset 0x20 */
#define SUBSCRIBER_TABLE_OFF 0x20
#define SUB_OFF_PID          0
#define SUB_OFF_CURSOR       4
#define SUB_OFF_FLAGS        8

/* IPC reply labels */
#define STREAM_REPLY_OK      0xF000
#define STREAM_REPLY_ERR     0xF001

/* Page size */
#define STREAM_PAGE_SIZE     4096

/*---------------------------------------------------------------------------
 * IPC message structure (must match kernel Process.Message)
 *---------------------------------------------------------------------------*/
typedef struct __attribute__((packed)) {
    uint32_t label;
    uint8_t  length;
    uint8_t  flags;
    uint16_t badge;
} stream_tag_t;

typedef struct __attribute__((packed)) {
    stream_tag_t tag;
    uint64_t     capBadge;
    uint64_t     words[4];
} stream_msg_t;

/*---------------------------------------------------------------------------
 * Internal stream state (producer side)
 *---------------------------------------------------------------------------*/
typedef struct {
    int      active;
    uint16_t id;
    unsigned pages;
    uint64_t base_addr;
    uint32_t capacity;
    uint64_t grant_ids[MAX_SUBSCRIBERS];
} stream_state_t;

static stream_state_t stream_tab[MAX_STREAMS];

/*---------------------------------------------------------------------------
 * Volatile memory access helpers
 *---------------------------------------------------------------------------*/
static inline void write_u32(uint64_t addr, uint32_t val)
{
    *(volatile uint32_t *)(uintptr_t)addr = val;
}

static inline uint32_t read_u32(uint64_t addr)
{
    return *(volatile uint32_t *)(uintptr_t)addr;
}

static inline void write_u16(uint64_t addr, uint16_t val)
{
    *(volatile uint16_t *)(uintptr_t)addr = val;
}

static inline uint16_t read_u16(uint64_t addr)
{
    return *(volatile uint16_t *)(uintptr_t)addr;
}

static inline void write_u8(uint64_t addr, uint8_t val)
{
    *(volatile uint8_t *)(uintptr_t)addr = val;
}

static inline uint8_t read_u8(uint64_t addr)
{
    return *(volatile uint8_t *)(uintptr_t)addr;
}

/*---------------------------------------------------------------------------
 * Helpers
 *---------------------------------------------------------------------------*/
static inline uint32_t align_up8(uint32_t v)
{
    return (v + 7) & ~(uint32_t)7;
}

static int find_stream(uint16_t id)
{
    for (int i = 0; i < MAX_STREAMS; i++) {
        if (stream_tab[i].active && stream_tab[i].id == id)
            return i;
    }
    return -1;
}

static uint32_t get_min_cursor(uint64_t base, uint8_t sub_cnt,
                               uint32_t p_idx)
{
    uint32_t min_cur = p_idx;
    if (sub_cnt == 0)
        return p_idx;

    for (int i = 0; i < sub_cnt; i++) {
        uint64_t slot_off = base + SUBSCRIBER_TABLE_OFF +
            (uint64_t)i * SUBSCRIBER_ENTRY_SIZE;
        uint32_t pid = read_u32(slot_off + SUB_OFF_PID);
        if (pid != 0) {
            uint32_t cur = read_u32(slot_off + SUB_OFF_CURSOR);
            if (i == 0 || cur < min_cur)
                min_cur = cur;
        }
    }
    return min_cur;
}

/*---------------------------------------------------------------------------
 * cubit_stream_create - Create a named stream
 *---------------------------------------------------------------------------*/
void cubit_stream_create(uint16_t stream_id, unsigned pages,
                         uint16_t type_tag)
{
    /* Find a free slot */
    int idx = -1;
    for (int i = 0; i < MAX_STREAMS; i++) {
        if (!stream_tab[i].active) {
            idx = i;
            break;
        }
    }
    if (idx < 0)
        return;

    /* Allocate memory via sbrk */
    uint64_t total = (uint64_t)pages * STREAM_PAGE_SIZE;
    long ret = syscall1(SYSCALL_SBRK, total);
    if (ret == (long)(-1UL))
        return;

    uint64_t base = (uint64_t)ret;
    uint32_t cap = (uint32_t)total - HEADER_SIZE;

    /* Zero the header */
    for (unsigned i = 0; i < HEADER_SIZE; i++)
        write_u8(base + i, 0);

    /* Initialize header fields */
    write_u32(base + HDR_MAGIC, STREAM_MAGIC);
    write_u16(base + HDR_VERSION, 1);
    write_u16(base + HDR_FLAGS, 0);
    write_u8 (base + HDR_SUBSCRIBER_COUNT, 0);
    write_u32(base + HDR_PRODUCER_IDX, 0);
    write_u32(base + HDR_CAPACITY, cap);
    write_u16(base + HDR_DEFAULT_TYPE_TAG, type_tag);
    write_u8 (base + HDR_OVERFLOW_POLICY, 1);  /* DROP_OLDEST */
    write_u16(base + HDR_STREAM_ID, stream_id);

    /* Store in local table */
    stream_tab[idx].active    = 1;
    stream_tab[idx].id        = stream_id;
    stream_tab[idx].pages     = pages;
    stream_tab[idx].base_addr = base;
    stream_tab[idx].capacity  = cap;
    for (int i = 0; i < MAX_SUBSCRIBERS; i++)
        stream_tab[idx].grant_ids[i] = 0;
}

/*---------------------------------------------------------------------------
 * cubit_stream_handle_subscription - Handle one pending subscribe/unsub
 *
 * Returns 1 if a request was handled, 0 otherwise.
 *---------------------------------------------------------------------------*/
int cubit_stream_handle_subscription(void)
{
    stream_msg_t msg;
    memset(&msg, 0, sizeof(msg));
    long from = syscall1(SYSCALL_RECEIVE_NB, &msg);
    if (from == 0)
        return 0;

    if (msg.tag.label == OP_STREAM_SUBSCRIBE) {
        uint16_t req_id = (uint16_t)(msg.words[0] & 0xFFFF);
        int idx = find_stream(req_id);

        stream_msg_t reply;
        memset(&reply, 0, sizeof(reply));

        if (idx < 0) {
            reply.tag.label = STREAM_REPLY_ERR;
            syscall6(SYSCALL_REPLY, from,
                     *(long *)&reply.tag, reply.words[0],
                     reply.words[1], reply.words[2], reply.words[3]);
            return 1;
        }

        stream_state_t *s = &stream_tab[idx];
        uint64_t base = s->base_addr;
        uint8_t sub_cnt = read_u8(base + HDR_SUBSCRIBER_COUNT);

        if (sub_cnt >= MAX_SUBSCRIBERS) {
            reply.tag.label = STREAM_REPLY_ERR;
            syscall6(SYSCALL_REPLY, from,
                     *(long *)&reply.tag, reply.words[0],
                     reply.words[1], reply.words[2], reply.words[3]);
            return 1;
        }

        unsigned slot = sub_cnt;
        uint64_t slot_off = base + SUBSCRIBER_TABLE_OFF +
            (uint64_t)slot * SUBSCRIBER_ENTRY_SIZE;

        /* Page-align base_addr for grant (it should already be aligned
         * since sbrk returns page-aligned memory) */
        long gid = syscall4(SYSCALL_GRANT, from, s->base_addr,
                            s->pages, 0 /* read-only */);
        if (gid == (long)(-1UL)) {
            reply.tag.label = STREAM_REPLY_ERR;
            syscall6(SYSCALL_REPLY, from,
                     *(long *)&reply.tag, reply.words[0],
                     reply.words[1], reply.words[2], reply.words[3]);
            return 1;
        }

        /* Store grant ID */
        s->grant_ids[slot] = (uint64_t)gid;

        /* Fill subscriber entry */
        uint32_t p_idx = read_u32(base + HDR_PRODUCER_IDX);
        write_u32(slot_off + SUB_OFF_PID, (uint32_t)from);
        write_u32(slot_off + SUB_OFF_CURSOR, p_idx);
        write_u16(slot_off + SUB_OFF_FLAGS, 0);

        /* Increment subscriber count */
        write_u8(base + HDR_SUBSCRIBER_COUNT, sub_cnt + 1);

        /* Reply with grant info */
        reply.tag.label = STREAM_REPLY_OK;
        reply.words[0] = (uint64_t)gid;
        reply.words[1] = (uint64_t)slot;
        reply.words[2] = (uint64_t)s->capacity;
        reply.words[3] = (uint64_t)p_idx;
        syscall6(SYSCALL_REPLY, from,
                 *(long *)&reply.tag, reply.words[0],
                 reply.words[1], reply.words[2], reply.words[3]);
        return 1;

    } else if (msg.tag.label == OP_STREAM_UNSUBSCRIBE) {
        uint16_t req_id = (uint16_t)(msg.words[0] & 0xFFFF);
        uint64_t req_slot = msg.words[1];
        int idx = find_stream(req_id);

        stream_msg_t reply;
        memset(&reply, 0, sizeof(reply));

        if (idx < 0 || req_slot > 7) {
            reply.tag.label = STREAM_REPLY_ERR;
            syscall6(SYSCALL_REPLY, from,
                     *(long *)&reply.tag, reply.words[0],
                     reply.words[1], reply.words[2], reply.words[3]);
            return 1;
        }

        stream_state_t *s = &stream_tab[idx];
        uint64_t base = s->base_addr;
        unsigned slot = (unsigned)req_slot;
        uint64_t slot_off = base + SUBSCRIBER_TABLE_OFF +
            (uint64_t)slot * SUBSCRIBER_ENTRY_SIZE;
        uint32_t sub_pid = read_u32(slot_off + SUB_OFF_PID);

        if (sub_pid != (uint32_t)from) {
            /* PID mismatch */
            reply.tag.label = STREAM_REPLY_ERR;
            syscall6(SYSCALL_REPLY, from,
                     *(long *)&reply.tag, reply.words[0],
                     reply.words[1], reply.words[2], reply.words[3]);
            return 1;
        }

        /* Revoke grant */
        if (s->grant_ids[slot] != 0) {
            syscall1(SYSCALL_REVOKE, s->grant_ids[slot]);
            s->grant_ids[slot] = 0;
        }

        /* Clear subscriber entry */
        write_u32(slot_off + SUB_OFF_PID, 0);
        write_u32(slot_off + SUB_OFF_CURSOR, 0);
        write_u16(slot_off + SUB_OFF_FLAGS, 0);

        /* Decrement subscriber count */
        uint8_t sub_cnt = read_u8(base + HDR_SUBSCRIBER_COUNT);
        if (sub_cnt > 0)
            write_u8(base + HDR_SUBSCRIBER_COUNT, sub_cnt - 1);

        reply.tag.label = STREAM_REPLY_OK;
        syscall6(SYSCALL_REPLY, from,
                 *(long *)&reply.tag, reply.words[0],
                 reply.words[1], reply.words[2], reply.words[3]);
        return 1;
    }

    /* Unknown message: drop */
    return 0;
}

/*---------------------------------------------------------------------------
 * cubit_stream_write - Write data to a stream
 *
 * Uses sentinel wraparound and slowest-subscriber flow control.
 *---------------------------------------------------------------------------*/
uint32_t cubit_stream_write(uint16_t stream_id, const void *data,
                            uint32_t len, uint16_t type_tag)
{
    int idx = find_stream(stream_id);
    if (idx < 0 || len == 0)
        return 0;

    /* Handle pending subscriptions */
    cubit_stream_handle_subscription();

    stream_state_t *s = &stream_tab[idx];
    uint64_t base = s->base_addr;
    uint64_t data_base = base + DATA_OFFSET;
    uint32_t cap = s->capacity;
    uint32_t entry_len = align_up8(ENTRY_HEADER_SIZE + len);

    if (entry_len > cap)
        return 0;

    uint32_t p_idx = read_u32(base + HDR_PRODUCER_IDX);
    const uint8_t *src = (const uint8_t *)data;

    /* Sentinel wraparound: if entry would straddle boundary */
    uint32_t write_off = p_idx % cap;
    if (write_off + entry_len > cap) {
        write_u16(data_base + write_off, SENTINEL_LENGTH);
        p_idx += (cap - write_off);
        write_off = 0;
    }

    /* Flow control: check slowest subscriber */
    uint8_t sub_cnt = read_u8(base + HDR_SUBSCRIBER_COUNT);
    if (sub_cnt > 0) {
        uint32_t min_cur = get_min_cursor(base, sub_cnt, p_idx);
        uint32_t used = p_idx - min_cur;
        if (used > cap)
            used = cap;
        uint32_t avail = cap - used;

        if (avail < entry_len) {
            /* DROP_OLDEST: advance lagging subscribers */
            uint32_t advance = entry_len - avail;
            for (int i = 0; i < sub_cnt; i++) {
                uint64_t slot_off = base + SUBSCRIBER_TABLE_OFF +
                    (uint64_t)i * SUBSCRIBER_ENTRY_SIZE;
                uint32_t pid = read_u32(slot_off + SUB_OFF_PID);
                if (pid != 0) {
                    uint32_t cur = read_u32(slot_off + SUB_OFF_CURSOR);
                    if ((p_idx - cur) + advance > cap)
                        write_u32(slot_off + SUB_OFF_CURSOR,
                                  cur + advance);
                }
            }
        }
    }

    /* Write entry header */
    write_u16(data_base + write_off, (uint16_t)len);
    write_u16(data_base + write_off + 2, type_tag);

    /* Write payload (contiguous with sentinel scheme) */
    for (uint32_t i = 0; i < len; i++)
        write_u8(data_base + write_off + ENTRY_HEADER_SIZE + i, src[i]);

    /* Advance producer index */
    write_u32(base + HDR_PRODUCER_IDX, p_idx + entry_len);

    return len;
}

/*---------------------------------------------------------------------------
 * cubit_stream_print - Write a text string to a stream
 *---------------------------------------------------------------------------*/
void cubit_stream_print(uint16_t stream_id, const char *msg)
{
    if (!msg)
        return;
    uint32_t len = (uint32_t)strlen(msg);
    if (len == 0)
        return;
    cubit_stream_write(stream_id, msg, len, CUBIT_TYPE_TEXT_LINE);
}

/*---------------------------------------------------------------------------
 * cubit_stream_flush - Best-effort flush
 *
 * Poll subscriber cursors until all caught up or ~100ms timeout.
 *---------------------------------------------------------------------------*/
void cubit_stream_flush(uint16_t stream_id)
{
    int idx = find_stream(stream_id);
    if (idx < 0)
        return;

    stream_state_t *s = &stream_tab[idx];
    uint64_t base = s->base_addr;

    for (int attempt = 0; attempt < 100; attempt++) {
        uint32_t p_idx = read_u32(base + HDR_PRODUCER_IDX);
        uint8_t sub_cnt = read_u8(base + HDR_SUBSCRIBER_COUNT);

        if (sub_cnt == 0)
            return;

        int all_caught_up = 1;
        for (int i = 0; i < sub_cnt; i++) {
            uint64_t slot_off = base + SUBSCRIBER_TABLE_OFF +
                (uint64_t)i * SUBSCRIBER_ENTRY_SIZE;
            uint32_t pid = read_u32(slot_off + SUB_OFF_PID);
            if (pid != 0) {
                uint32_t cur = read_u32(slot_off + SUB_OFF_CURSOR);
                if (cur != p_idx) {
                    all_caught_up = 0;
                    break;
                }
            }
        }

        if (all_caught_up)
            return;

        syscall1(SYSCALL_SLEEP, 1);
    }
}
