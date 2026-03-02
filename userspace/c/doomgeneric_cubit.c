/*
 * doomgeneric platform implementation for CuBit OS
 * Copyright (C) 2026 Jon Andrew
 *
 * Implements the 6 doomgeneric platform functions:
 *   DG_Init, DG_DrawFrame, DG_SleepMs, DG_GetTicksMs, DG_GetKey,
 *   DG_SetWindowTitle
 *
 * Framebuffer: mapped via SYSCALL_MAPFB
 * Keyboard:    polled via SYSCALL_RECEIVE_NB (PS/2 Set 1 scancodes)
 * Timer:       SYSCALL_GETTIME (millisecond PIT ticks)
 */

#include "doomkeys.h"
#include "m_argv.h"
#include "doomgeneric.h"

#include "cubit.h"

/* Framebuffer state */
static cubit_framebuffer_t fb;
static uint32_t *fb_ptr = NULL;

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
    /* 0x1D */  KEY_RCTRL,      /* Left Ctrl -> fire */
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
    /* 0x39 */  ' ',            /* Space -> use */
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

static void poll_keyboard(void)
{
    cubit_key_event_t ev;
    while (cubit_keyboard_get(&ev)) {
        unsigned char doomKey = 0;
        if (ev.scancode < 128)
            doomKey = scancode_to_doom[ev.scancode];
        push_key(doomKey, ev.pressed);
    }
}

/*---------------------------------------------------------------------------
 * DG_Init - Initialize framebuffer and keyboard
 *---------------------------------------------------------------------------*/
void DG_Init(void)
{
    /* Map framebuffer */
    if (cubit_map_framebuffer(&fb) < 0) {
        cubit_puts("DOOM: Failed to map framebuffer!\n");
        cubit_exit(1);
    }

    fb_ptr = (uint32_t *)fb.addr;

    cubit_puts("DOOM: Framebuffer mapped at ");
    /* Simple hex print since printf may not be available yet */
    char buf[64];
    snprintf(buf, sizeof(buf), "0x%lx (%ux%u, pitch=%u, bpp=%u)\n",
             (unsigned long)fb.addr, fb.width, fb.height, fb.pitch, fb.bpp);
    cubit_puts(buf);

    /* Register as keyboard driver and start receiving scan codes */
    cubit_keyboard_init();

    cubit_puts("DOOM: Keyboard initialized.\n");
}

/*---------------------------------------------------------------------------
 * DG_DrawFrame - Blit DOOM's 640x400 screen buffer to framebuffer
 *
 * DOOM renders at DOOMGENERIC_RESX x DOOMGENERIC_RESY (640x400) in XRGB8888.
 * We center the image on screen if the framebuffer is larger.
 *---------------------------------------------------------------------------*/
void DG_DrawFrame(void)
{
    if (!fb_ptr) return;

    /* Calculate centering offset */
    int off_x = 0, off_y = 0;
    if (fb.width > DOOMGENERIC_RESX)
        off_x = (fb.width - DOOMGENERIC_RESX) / 2;
    if (fb.height > DOOMGENERIC_RESY)
        off_y = (fb.height - DOOMGENERIC_RESY) / 2;

    /* Pitch is in bytes, convert to uint32_t stride */
    int fb_stride = fb.pitch / 4;

    /* Copy each scanline */
    for (int y = 0; y < DOOMGENERIC_RESY; y++) {
        if ((unsigned int)(y + off_y) >= fb.height) break;
        uint32_t *dst = fb_ptr + (y + off_y) * fb_stride + off_x;
        uint32_t *src = DG_ScreenBuffer + y * DOOMGENERIC_RESX;
        memcpy(dst, src, DOOMGENERIC_RESX * 4);
    }

    /* Poll keyboard each frame */
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
 * DG_SetWindowTitle - No-op on framebuffer console
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
    cubit_puts("DOOM: Starting on CuBit OS...\n");

    /* Fixed argv for doomgeneric - no command-line args on CuBit */
    static char *argv[] = {"doom", "-iwad", "doom1.wad", NULL};
    doomgeneric_Create(3, argv);

    while (1) {
        doomgeneric_Tick();
    }

    return 0;
}
