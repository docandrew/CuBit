/*
 * doomgeneric platform implementation for CuBit OS
 * Copyright (C) 2026 Jon Andrew
 *
 * Implements the 6 doomgeneric platform functions:
 *   DG_Init, DG_DrawFrame, DG_SleepMs, DG_GetTicksMs, DG_GetKey,
 *   DG_SetWindowTitle
 *
 * Desktop:     draw into an app-owned window surface buffer
 * Keyboard:    desktop input events
 * Timer:       SYSCALL_GETTIME (millisecond PIT ticks)
 */

#include "doomkeys.h"
#include "m_argv.h"
#include "doomgeneric.h"
#include "d_event.h"

#include "cubit.h"
#include <stdio.h>
#include <stdlib.h>

/* Package identity (.cubit.id section) */
static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    /* Header: magic "CBID" LE, version 1, count 2 */
    0x43, 0x42, 0x49, 0x44,
    0x01, 0x00,
    0x02, 0x00,

    /* Entry 0: id = "com.cubit.doom" */
    0x02,                       /* keyLen = 2 */
    0x0E, 0x00,                 /* valLen = 14 */
    'i', 'd',
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.', 'd', 'o', 'o', 'm',

    /* Entry 1: version = "1.0.0" */
    0x07,                       /* keyLen = 7 */
    0x05, 0x00,                 /* valLen = 5 */
    'v', 'e', 'r', 's', 'i', 'o', 'n',
    '1', '.', '0', '.', '0'
};

/* Stream declarations (.cubit.streams section):
 *   LOG stream (id=4, 4 pages, TYPE_TEXT) — startup/debug messages */
static const unsigned char __cubit_streams[]
    __attribute__((section(".cubit.streams"), used)) = {
    /* Header: magic "CBST" LE, version 1, count 1 */
    0x43, 0x42, 0x53, 0x54,
    0x01, 0x00,
    0x01, 0x00,

    /* Entry 0: streamID=4(LOG), pages=4, typeTag=1(TEXT), flags=0 */
    0x04, 0x00, 0x04, 0x00, 0x01, 0x00, 0x00, 0x00
};

/* Declare capability requirements in ELF manifest:
 *   slot 1  - CAP_ENDPOINT to FS server (DRIVER_FS = 6)
 *   slot 14 - CAP_ENDPOINT to mixer (DRIVER_MIXER = 9)
 *   slot 21 - CAP_ENDPOINT to desktop (DRIVER_DESKTOP = 15)
 */
static const unsigned char __cubit_manifest[]
    __attribute__((section(".cubit.caps"), used)) = {
    /* Header: magic "CBIT" LE, version 1, count 3 */
    0x54, 0x49, 0x42, 0x43,
    0x01, 0x00,
    0x03, 0x00,

    /* Entry 0: REQ_SERVICE, RW, slot 1, driver_id=6 (DRIVER_FS) */
    CUBIT_REQ_SERVICE, CUBIT_RIGHT_RW, 1, 0x00,
    0x06, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

    /* Entry 1: REQ_SERVICE, RW|GRANT, slot 14, driver_id=9 (DRIVER_MIXER) */
    CUBIT_REQ_SERVICE, CUBIT_RIGHT_RW | CUBIT_RIGHT_GRANT, 14, 0x00,
    0x09, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

    /* Entry 2: REQ_SERVICE, RW, slot 21, driver_id=15 (DRIVER_DESKTOP) */
    CUBIT_REQ_SERVICE, CUBIT_RIGHT_RW, 21, 0x00,
    0x0F, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

/* Declare filesystem access requirements in ELF .cubit.access section:
 *   Entry 0: READ  doom1.wad           (ramdisk fallback)
 *   Entry 1: READ  @ata:0/doom1.wad    (ATA disk)
 *   Entry 2: READ  @nvme:0/doom1.wad   (NVMe disk)
 */
static const unsigned char __cubit_access[]
    __attribute__((section(".cubit.access"), used)) = {
    /* Header (16 bytes): magic "CACC" LE, version 1, count 5,
     * uid=0, gid=0, trustFloor=0, reserved=0 */
    0x43, 0x41, 0x43, 0x43,             /* magic */
    0x01, 0x00,                         /* version */
    0x05, 0x00,                         /* count */
    0x00, 0x00,                         /* uid */
    0x00, 0x00,                         /* gid */
    0x00,                               /* trustFloor */
    0x00, 0x00, 0x00,                   /* reserved */

    /* Entry 0 (80 bytes): READ doom1.wad (prefixLen=9) */
    CUBIT_ACL_READ,                     /* rights */
    9,                                  /* prefixLen */
    0x00,                               /* flags */
    0x00,                               /* reserved */
    0x00, 0x00,                         /* uid */
    0x00, 0x00,                         /* gid */
    'd','o','o','m','1','.','w','a','d', 0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   /* 64 bytes prefix */
    0,0,0,0,0,0,0,0,                   /* reserved64 */

    /* Entry 1 (80 bytes): READ @ata:0/doom1.wad (prefixLen=16) */
    CUBIT_ACL_READ,                     /* rights */
    16,                                 /* prefixLen */
    0x00,                               /* flags */
    0x00,                               /* reserved */
    0x00, 0x00,                         /* uid */
    0x00, 0x00,                         /* gid */
    '@','a','t','a',':','0','/','d','o','o','m','1','.','w','a','d',
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   /* 64 bytes prefix */
    0,0,0,0,0,0,0,0,                   /* reserved64 */

    /* Entry 2 (80 bytes): READ @nvme:0/doom1.wad (prefixLen=17) */
    CUBIT_ACL_READ,                     /* rights */
    17,                                 /* prefixLen */
    0x00,                               /* flags */
    0x00,                               /* reserved */
    0x00, 0x00,                         /* uid */
    0x00, 0x00,                         /* gid */
    '@','n','v','m','e',':','0','/','d','o','o','m','1','.','w','a',
    'd',0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   /* 64 bytes prefix */
    0,0,0,0,0,0,0,0,                   /* reserved64 */

    /* Entry 3 (80 bytes): RW+CREATE @ata:0/ (prefixLen=7) — saves on ATA */
    CUBIT_ACL_READ | CUBIT_ACL_WRITE | CUBIT_ACL_CREATE, /* rights */
    7,                                  /* prefixLen */
    0x00,                               /* flags */
    0x00,                               /* reserved */
    0x00, 0x00,                         /* uid */
    0x00, 0x00,                         /* gid */
    '@','a','t','a',':','0','/',0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   /* 64 bytes prefix */
    0,0,0,0,0,0,0,0,                   /* reserved64 */

    /* Entry 4 (80 bytes): RW+CREATE @nvme:0/ (prefixLen=8) — saves on NVMe */
    CUBIT_ACL_READ | CUBIT_ACL_WRITE | CUBIT_ACL_CREATE, /* rights */
    8,                                  /* prefixLen */
    0x00,                               /* flags */
    0x00,                               /* reserved */
    0x00, 0x00,                         /* uid */
    0x00, 0x00,                         /* gid */
    '@','n','v','m','e',':','0','/',0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   /* 64 bytes prefix */
    0,0,0,0,0,0,0,0                    /* reserved64 */
};

/* Desktop surface protocol. These labels intentionally match the current
 * prototype in desktop.svc; they are not yet a stable CuBit UI ABI. */
#define OP_DESKTOP_HELLO          0x0800
#define OP_DESKTOP_BYE            0x0801
#define OP_SURFACE_CREATE         0x0810
#define OP_SURFACE_DESTROY        0x0811
#define OP_SURFACE_PRESENT        0x0812
#define OP_SURFACE_ATTACH_BUFFER  0x0814
#define OP_WINDOW_SET_LIMITS      0x0841
#define OP_INPUT_POLL             0x0821

#define SURFACE_FLAG_WINDOW       2
#define PIXEL_FORMAT_BGRA8888     1
#define INPUT_NONE                0
#define INPUT_KEY_DOWN            1
#define INPUT_KEY_UP              2

#define WINDOW_FLAG_DECORATED     1
#define WINDOW_FLAG_MINIMIZABLE   4
#define WINDOW_FLAG_CLOSEABLE     16
#define WINDOW_FLAG_FIXED_SIZE    128

#define DOOM_WINDOW_W             (DOOMGENERIC_RESX + 20)
#define DOOM_WINDOW_H             (DOOMGENERIC_RESY + 44)
#define DOOM_BUFFER_PITCH         (DOOMGENERIC_RESX * 4)
#define DOOM_BUFFER_BYTES         (DOOM_BUFFER_PITCH * DOOMGENERIC_RESY)

typedef struct {
    uint32_t label;
    uint8_t  length;
    uint8_t  flags;
    uint16_t badge;
} cubit_message_tag_t;

typedef struct {
    cubit_message_tag_t tag;
    uint64_t capBadge;
    uint64_t words[4];
} cubit_message_t;

static int desktop_mode = 0;
static uint64_t desktop_surface = 0;
static uint64_t desktop_last_input_serial = 0;
static uint64_t desktop_buffer_grant = 0;
static uint32_t *desktop_buffer = NULL;
static uint64_t desktop_last_present_ms = 0;
static uint64_t desktop_present_count = 0;
static uint64_t desktop_skip_count = 0;
static uint64_t desktop_present_ms_total = 0;
static uint64_t desktop_stats_ms = 0;
static uint64_t desktop_next_idle_poll_ms = 0;
static int desktop_shutdown_registered = 0;
static int desktop_shutdown_done = 0;

/* DOOM's native game tic rate is 35 Hz. Keep the desktop path close to that
 * cadence; a 50 ms throttle made windowed DOOM look artificially sluggish even
 * after the compositor got faster. */
#define DESKTOP_PRESENT_INTERVAL_MS 28
#define NO_COMPLETION_TOKEN       (~(uint64_t)0)

static uint64_t pack_u32_pair(uint32_t lo, uint32_t hi)
{
    return ((uint64_t)lo) | (((uint64_t)hi) << 32);
}

static uintptr_t align_up_page_uintptr(uintptr_t value)
{
    return (value + 4095UL) & ~4095UL;
}

static int desktop_call(uint32_t label,
                        uint64_t w0, uint64_t w1,
                        uint64_t w2, uint64_t w3,
                        cubit_message_t *reply)
{
    cubit_message_t msg;
    memset(&msg, 0, sizeof(msg));
    msg.tag.label = label;
    msg.tag.length = 4;
    msg.words[0] = w0;
    msg.words[1] = w1;
    msg.words[2] = w2;
    msg.words[3] = w3;

    if (syscall2(SYSCALL_CAP_CALL, CAP_SLOT_DESKTOP, &msg) == (long)(-1UL))
        return -1;

    if (reply)
        *reply = msg;
    return 0;
}

static int desktop_submit(uint32_t label,
                          uint64_t w0, uint64_t w1, uint64_t w2)
{
    cubit_message_tag_t tag;
    uint64_t tag_word;

    memset(&tag, 0, sizeof(tag));
    tag.label = label;
    tag.length = 3;
    memcpy(&tag_word, &tag, sizeof(tag_word));

    return syscall6(SYSCALL_CAP_SUBMIT,
                    CAP_SLOT_DESKTOP,
                    tag_word,
                    w0,
                    w1,
                    w2,
                    NO_COMPLETION_TOKEN) == 1 ? 0 : -1;
}

static void shutdown_desktop_surface(void)
{
    cubit_message_t reply;

    if (desktop_shutdown_done)
        return;
    desktop_shutdown_done = 1;

    if (desktop_surface != 0) {
        (void)desktop_call(OP_SURFACE_DESTROY,
                           desktop_surface, 0, 0, 0, &reply);
    }

    if (desktop_mode || desktop_surface != 0) {
        (void)desktop_call(OP_DESKTOP_BYE, 0, 0, 0, 0, &reply);
    }

    desktop_mode = 0;
    desktop_surface = 0;
    desktop_buffer = NULL;
    desktop_buffer_grant = 0;
}

static void append_dec(char *buf, int *pos, int max, uint64_t value)
{
    char tmp[21];
    int n = 0;

    if (*pos >= max - 1)
        return;

    if (value == 0) {
        buf[(*pos)++] = '0';
        buf[*pos] = '\0';
        return;
    }

    while (value > 0 && n < (int)sizeof(tmp)) {
        tmp[n++] = (char)('0' + (value % 10));
        value /= 10;
    }
    while (n > 0 && *pos < max - 1) {
        buf[(*pos)++] = tmp[--n];
    }
    buf[*pos] = '\0';
}

static void append_str(char *buf, int *pos, int max, const char *s)
{
    while (*s && *pos < max - 1) {
        buf[(*pos)++] = *s++;
    }
    buf[*pos] = '\0';
}

static void maybe_print_desktop_stats(uint64_t now)
{
    char buf[128];
    int pos = 0;

    if (desktop_stats_ms == 0) {
        desktop_stats_ms = now;
        return;
    }

    if (now < desktop_stats_ms || now - desktop_stats_ms < 1000)
        return;

    append_str(buf, &pos, sizeof(buf), "DOOM: desktop presents=");
    append_dec(buf, &pos, sizeof(buf), desktop_present_count);
    append_str(buf, &pos, sizeof(buf), " skipped=");
    append_dec(buf, &pos, sizeof(buf), desktop_skip_count);
    append_str(buf, &pos, sizeof(buf), " present_ms=");
    append_dec(buf, &pos, sizeof(buf), desktop_present_ms_total);
    append_str(buf, &pos, sizeof(buf), "\n");
    cubit_stream_print(CUBIT_STREAM_LOG, buf);

    desktop_present_count = 0;
    desktop_skip_count = 0;
    desktop_present_ms_total = 0;
    desktop_stats_ms = now;
}

static int init_desktop_surface(void)
{
    cubit_message_t reply;
    uint64_t pages;
    long grant;
    void *raw;
    uint64_t fixed_flags;

    if (desktop_call(OP_DESKTOP_HELLO,
                     0x0000000100000000ULL, 0, 0, 0, &reply) < 0 ||
        reply.words[0] == 0) {
        return -1;
    }

    if (desktop_call(OP_SURFACE_CREATE,
                     DOOM_WINDOW_W, DOOM_WINDOW_H,
                     SURFACE_FLAG_WINDOW, 0, &reply) < 0 ||
        reply.words[0] == 0) {
        return -1;
    }
    desktop_surface = reply.words[0];

    fixed_flags = WINDOW_FLAG_DECORATED |
                  WINDOW_FLAG_MINIMIZABLE |
                  WINDOW_FLAG_CLOSEABLE |
                  WINDOW_FLAG_FIXED_SIZE;
    (void)desktop_call(OP_WINDOW_SET_LIMITS,
                       desktop_surface,
                       pack_u32_pair(DOOM_WINDOW_W, DOOM_WINDOW_H),
                       pack_u32_pair(DOOM_WINDOW_W, DOOM_WINDOW_H),
                       fixed_flags,
                       &reply);

    pages = (DOOM_BUFFER_BYTES + 4095) / 4096;
    raw = cubit_sbrk((intptr_t)(pages * 4096 + 4096));
    if ((long)raw == -1)
        return -1;

    desktop_buffer = (uint32_t *)align_up_page_uintptr((uintptr_t)raw);
    memset(desktop_buffer, 0, DOOM_BUFFER_BYTES);

    grant = syscall4(SYSCALL_GRANT_VIA_CAP, CAP_SLOT_DESKTOP,
                     desktop_buffer, pages, 0);
    if (grant == (long)(-1UL))
        return -1;
    desktop_buffer_grant = (uint64_t)grant;

    if (desktop_call(OP_SURFACE_ATTACH_BUFFER,
                     desktop_surface,
                     desktop_buffer_grant,
                     pack_u32_pair(DOOMGENERIC_RESX, DOOMGENERIC_RESY),
                     ((uint64_t)DOOM_BUFFER_PITCH) |
                        (((uint64_t)PIXEL_FORMAT_BGRA8888) << 32),
                     &reply) < 0 ||
        reply.words[0] != 0) {
        return -1;
    }

    desktop_mode = 1;
    cubit_stream_print(CUBIT_STREAM_LOG, "DOOM: Desktop surface attached.\n");
    return 0;
}

/*---------------------------------------------------------------------------
 * PS/2 Set 1 scancode -> DOOM key translation
 *
 * PS/2 Set 1 make codes (press) have bit 7 clear.
 * Break codes (release) have bit 7 set; we strip it before lookup.
 *---------------------------------------------------------------------------*/

/* Maps PS/2 Set 1 scancode (0x00-0x58) to ASCII or DOOM key constant.
 * Only the keys DOOM cares about are populated. */
static const unsigned char scancode_to_doom[128] = {
    /* 0x00 */  0,
    /* 0x01 */  KEY_ESCAPE,
    /* 0x02 */  '1',
    /* 0x03 */  '2',
    /* 0x04 */  '3',
    /* 0x05 */  '4',
    /* 0x06 */  '5',
    /* 0x07 */  '6',
    /* 0x08 */  '7',
    /* 0x09 */  '8',
    /* 0x0A */  '9',
    /* 0x0B */  '0',
    /* 0x0C */  KEY_MINUS,
    /* 0x0D */  KEY_EQUALS,
    /* 0x0E */  KEY_BACKSPACE,
    /* 0x0F */  KEY_TAB,
    /* 0x10 */  'q',
    /* 0x11 */  'w',
    /* 0x12 */  'e',
    /* 0x13 */  'r',
    /* 0x14 */  't',
    /* 0x15 */  'y',
    /* 0x16 */  'u',
    /* 0x17 */  'i',
    /* 0x18 */  'o',
    /* 0x19 */  'p',
    /* 0x1A */  '[',
    /* 0x1B */  ']',
    /* 0x1C */  KEY_ENTER,
    /* 0x1D */  KEY_FIRE,       /* Left Ctrl -> fire */
    /* 0x1E */  'a',
    /* 0x1F */  's',
    /* 0x20 */  'd',
    /* 0x21 */  'f',
    /* 0x22 */  'g',
    /* 0x23 */  'h',
    /* 0x24 */  'j',
    /* 0x25 */  'k',
    /* 0x26 */  'l',
    /* 0x27 */  ';',
    /* 0x28 */  '\'',
    /* 0x29 */  '`',
    /* 0x2A */  KEY_RSHIFT,     /* Left Shift */
    /* 0x2B */  '\\',
    /* 0x2C */  'z',
    /* 0x2D */  'x',
    /* 0x2E */  'c',
    /* 0x2F */  'v',
    /* 0x30 */  'b',
    /* 0x31 */  'n',
    /* 0x32 */  'm',
    /* 0x33 */  ',',
    /* 0x34 */  '.',
    /* 0x35 */  '/',
    /* 0x36 */  KEY_RSHIFT,     /* Right Shift */
    /* 0x37 */  '*',            /* Keypad * */
    /* 0x38 */  KEY_RALT,       /* Left Alt -> strafe */
    /* 0x39 */  KEY_USE,        /* Space -> use */
    /* 0x3A */  KEY_CAPSLOCK,
    /* 0x3B */  KEY_F1,
    /* 0x3C */  KEY_F2,
    /* 0x3D */  KEY_F3,
    /* 0x3E */  KEY_F4,
    /* 0x3F */  KEY_F5,
    /* 0x40 */  KEY_F6,
    /* 0x41 */  KEY_F7,
    /* 0x42 */  KEY_F8,
    /* 0x43 */  KEY_F9,
    /* 0x44 */  KEY_F10,
    /* 0x45 */  KEY_NUMLOCK,
    /* 0x46 */  KEY_SCRLCK,
    /* 0x47 */  KEY_HOME,       /* Keypad 7 */
    /* 0x48 */  KEY_UPARROW,    /* Keypad 8 / Up arrow */
    /* 0x49 */  KEY_PGUP,       /* Keypad 9 */
    /* 0x4A */  KEY_MINUS,      /* Keypad - */
    /* 0x4B */  KEY_LEFTARROW,  /* Keypad 4 / Left arrow */
    /* 0x4C */  '5',            /* Keypad 5 */
    /* 0x4D */  KEY_RIGHTARROW, /* Keypad 6 / Right arrow */
    /* 0x4E */  KEY_EQUALS,     /* Keypad + */
    /* 0x4F */  KEY_END,        /* Keypad 1 */
    /* 0x50 */  KEY_DOWNARROW,  /* Keypad 2 / Down arrow */
    /* 0x51 */  KEY_PGDN,       /* Keypad 3 */
    /* 0x52 */  KEY_INS,        /* Keypad 0 */
    /* 0x53 */  KEY_DEL,        /* Keypad . */
    /* 0x54-0x58 */
    0, 0, 0,
    /* 0x57 */  KEY_F11,
    /* 0x58 */  KEY_F12,
};

/* Key event queue (ring buffer) */
#define KEY_QUEUE_SIZE 32

typedef struct {
    unsigned char key;
    int pressed;
} doom_key_event_t;

static doom_key_event_t key_queue[KEY_QUEUE_SIZE];
static int key_queue_head = 0;
static int key_queue_tail = 0;

static void push_key(unsigned char doomKey, int pressed)
{
    if (doomKey == 0) return;
    int next = (key_queue_head + 1) % KEY_QUEUE_SIZE;
    if (next == key_queue_tail) return; /* full */
    key_queue[key_queue_head].key = doomKey;
    key_queue[key_queue_head].pressed = pressed;
    key_queue_head = next;
}

static void poll_desktop_input(void)
{
    cubit_message_t reply;
    uint64_t now = cubit_gettime_ms();
    int saw_event = 0;

    if (now != (uint64_t)-1 && now < desktop_next_idle_poll_ms)
        return;

    while (desktop_mode) {
        if (desktop_call(OP_INPUT_POLL,
                         desktop_surface,
                         desktop_last_input_serial,
                         0, 0, &reply) < 0) {
            return;
        }

        if (reply.words[0] == INPUT_NONE)
            break;

        saw_event = 1;
        desktop_last_input_serial = reply.words[1];
        if (reply.words[0] == INPUT_KEY_DOWN ||
            reply.words[0] == INPUT_KEY_UP) {
            uint8_t scancode = (uint8_t)(reply.words[2] & 0x7F);
            int pressed = (reply.words[0] == INPUT_KEY_DOWN);
            if (scancode < 128)
                push_key(scancode_to_doom[scancode], pressed);
        }
    }

    /* Keep active input crisp, but avoid hammering the compositor with
     * dozens of synchronous "nothing happened" polls per second while DOOM
     * is just animating. An 8 ms idle backoff is below a 60 Hz frame period
     * and still much shorter than a DOOM tic. */
    if (!saw_event && now != (uint64_t)-1)
        desktop_next_idle_poll_ms = now + 8;
    else
        desktop_next_idle_poll_ms = 0;
}

static void poll_keyboard(void)
{
    if (desktop_mode)
        poll_desktop_input();
    else
        return;
}

/*---------------------------------------------------------------------------
 * DG_Init - Initialize desktop window and keyboard input
 *---------------------------------------------------------------------------*/
void DG_Init(void)
{
    if (!desktop_shutdown_registered) {
        if (atexit(shutdown_desktop_surface) == 0)
            desktop_shutdown_registered = 1;
    }

    if (init_desktop_surface() == 0) {
        cubit_stream_print(CUBIT_STREAM_LOG,
                           "DOOM: Running inside desktop surface.\n");
        return;
    }

    cubit_stream_print(CUBIT_STREAM_LOG,
                       "DOOM: desktop.svc unavailable; windowed DOOM requires the desktop.\n");
    cubit_exit(1);
}

/*---------------------------------------------------------------------------
 * DG_DrawFrame - Blit DOOM's 640x400 screen buffer to desktop surface
 *
 * DOOM renders at DOOMGENERIC_RESX x DOOMGENERIC_RESY (640x400) in XRGB8888.
 * The compositor clips and presents this app-owned buffer inside window chrome.
 *---------------------------------------------------------------------------*/
void DG_DrawFrame(void)
{
    if (desktop_mode && desktop_buffer) {
        uint64_t now;

        now = cubit_gettime_ms();
        if (desktop_last_present_ms == 0 ||
            now < desktop_last_present_ms ||
            now - desktop_last_present_ms >= DESKTOP_PRESENT_INTERVAL_MS) {
            uint64_t t0 = now;
            uint64_t t1;
            memcpy(desktop_buffer, DG_ScreenBuffer, DOOM_BUFFER_BYTES);
            (void)desktop_submit(OP_SURFACE_PRESENT,
                                 desktop_surface, 0, 0);
            t1 = cubit_gettime_ms();
            desktop_last_present_ms = now;
            desktop_present_count++;
            if (t1 != (uint64_t)-1 && t1 >= t0)
                desktop_present_ms_total += t1 - t0;
        } else {
            desktop_skip_count++;
        }

        maybe_print_desktop_stats(now);
        poll_keyboard();
        return;
    }

    poll_keyboard();
}

/*---------------------------------------------------------------------------
 * DG_SleepMs - Sleep for the given number of milliseconds
 *---------------------------------------------------------------------------*/
void DG_SleepMs(uint32_t ms)
{
    cubit_sleep_ms(ms);
}

/*---------------------------------------------------------------------------
 * DG_GetTicksMs - Return monotonic time in milliseconds
 *---------------------------------------------------------------------------*/
uint32_t DG_GetTicksMs(void)
{
    return (uint32_t)cubit_gettime_ms();
}

/*---------------------------------------------------------------------------
 * DG_GetKey - Get next keyboard event for DOOM
 * Returns 1 if an event was available, 0 otherwise.
 *---------------------------------------------------------------------------*/
int DG_GetKey(int *pressed, unsigned char *doomKey)
{
    /* Try to drain keyboard events first */
    poll_keyboard();

    if (key_queue_tail == key_queue_head)
        return 0;

    *pressed = key_queue[key_queue_tail].pressed;
    *doomKey = key_queue[key_queue_tail].key;
    key_queue_tail = (key_queue_tail + 1) % KEY_QUEUE_SIZE;
    return 1;
}

/*---------------------------------------------------------------------------
 * DG_SetWindowTitle - Title protocol can be added to desktop.svc later.
 *---------------------------------------------------------------------------*/
void DG_SetWindowTitle(const char *title)
{
    (void)title;
}

/*---------------------------------------------------------------------------
 * main - Entry point
 *---------------------------------------------------------------------------*/
int main(void)
{
    /* Create LOG stream for debug output */
    cubit_stream_create(CUBIT_STREAM_LOG, 4, CUBIT_TYPE_TEXT);

    cubit_stream_print(CUBIT_STREAM_LOG, "DOOM: Starting on CuBit OS...\n");

    /* Try NVMe first, then ATA, fall back to ramdisk */
    static char *wad_path;
    FILE *test = fopen("@nvme:0/doom1.wad", "r");
    if (test) {
        fclose(test);
        wad_path = "@nvme:0/doom1.wad";
        cubit_stream_print(CUBIT_STREAM_LOG, "DOOM: Using WAD from NVMe disk.\n");
    } else {
        test = fopen("@ata:0/doom1.wad", "r");
        if (test) {
            fclose(test);
            wad_path = "@ata:0/doom1.wad";
            cubit_stream_print(CUBIT_STREAM_LOG, "DOOM: Using WAD from ATA disk.\n");
        } else {
            wad_path = "doom1.wad";
            cubit_stream_print(CUBIT_STREAM_LOG, "DOOM: No disk, using ramdisk WAD.\n");
        }
    }

    static char *argv[] = {"doom", "-iwad", NULL, NULL};
    argv[2] = wad_path;
    doomgeneric_Create(3, argv);

    /* Override savegame directory to use flat paths on the disk
     * since CuBit doesn't have mkdir/subdirectory creation yet. */
    {
        extern char *savegamedir;
        if (wad_path[0] == '@') {
            /* Extract scheme prefix (e.g. "@nvme:0/") for saves */
            int slash = 0;
            for (int i = 0; wad_path[i]; i++) {
                if (wad_path[i] == '/') { slash = i + 1; break; }
            }
            if (slash > 0) {
                char *sd = malloc(slash + 1);
                memcpy(sd, wad_path, slash);
                sd[slash] = '\0';
                savegamedir = sd;
            }
        }
    }

    while (1) {
        doomgeneric_Tick();
    }

    return 0;
}
