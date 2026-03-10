/*
 * CuBit OS - Keyboard & Mouse Input
 * Copyright (C) 2026 Jon Andrew
 *
 * Polls keyboard and mouse services for events via non-blocking event receive.
 * Both event types arrive through the same receiveEventNB syscall, dispatched
 * by label: label=1 for keyboard, label=2 for mouse.
 *
 * Protocol:
 *   1. Register as DRIVER_KEYBOARD (1) / DRIVER_MOUSE (10) via
 *      SYSCALL_REGISTER_DRIVER
 *   2. Kernel services send events with label distinguishing type
 *   3. We poll with SYSCALL_RECEIVE_EVENT_NB (no reply needed)
 */
#include "cubit.h"

#define DRIVER_KEYBOARD 1
#define DRIVER_MOUSE    10

/* IPC Message layout matching kernel Process.Message (48 bytes) */
typedef struct {
    uint32_t label;
    uint8_t  length;
    uint8_t  flags;
    uint16_t badge;
    uint64_t capBadge;
    uint64_t words[4];
} cubit_message_t;

/* IPC reply tag: REPLY_OK = 0xF000 */
#define REPLY_OK_LABEL  0xF000

/*---------------------------------------------------------------------------
 * Keyboard ring buffer
 *---------------------------------------------------------------------------*/
#define KEY_BUFFER_SIZE 64
static cubit_key_event_t key_buffer[KEY_BUFFER_SIZE];
static int key_head = 0;
static int key_tail = 0;

static int key_buf_empty(void)
{
    return key_head == key_tail;
}

static int key_buf_full(void)
{
    return ((key_head + 1) % KEY_BUFFER_SIZE) == key_tail;
}

static void key_buf_push(cubit_key_event_t ev)
{
    if (!key_buf_full()) {
        key_buffer[key_head] = ev;
        key_head = (key_head + 1) % KEY_BUFFER_SIZE;
    }
}

static int key_buf_pop(cubit_key_event_t *ev)
{
    if (key_buf_empty()) return 0;
    *ev = key_buffer[key_tail];
    key_tail = (key_tail + 1) % KEY_BUFFER_SIZE;
    return 1;
}

/*---------------------------------------------------------------------------
 * Mouse ring buffer
 *---------------------------------------------------------------------------*/
#define MOUSE_BUFFER_SIZE 64
static cubit_mouse_event_t mouse_buffer[MOUSE_BUFFER_SIZE];
static int mouse_head = 0;
static int mouse_tail = 0;

static int mouse_buf_empty(void)
{
    return mouse_head == mouse_tail;
}

static int mouse_buf_full(void)
{
    return ((mouse_head + 1) % MOUSE_BUFFER_SIZE) == mouse_tail;
}

static void mouse_buf_push(cubit_mouse_event_t ev)
{
    if (!mouse_buf_full()) {
        mouse_buffer[mouse_head] = ev;
        mouse_head = (mouse_head + 1) % MOUSE_BUFFER_SIZE;
    }
}

static int mouse_buf_pop(cubit_mouse_event_t *ev)
{
    if (mouse_buf_empty()) return 0;
    *ev = mouse_buffer[mouse_tail];
    mouse_tail = (mouse_tail + 1) % MOUSE_BUFFER_SIZE;
    return 1;
}

/*---------------------------------------------------------------------------
 * Sign-extend a 12-bit value to int16_t
 *---------------------------------------------------------------------------*/
static int16_t sign_extend_12(uint64_t val)
{
    val &= 0xFFF;
    if (val & 0x800)
        return (int16_t)(val | 0xF000);
    return (int16_t)val;
}

/*---------------------------------------------------------------------------
 * drain_events - Drain all pending events, dispatch by label
 *
 * label=1: keyboard scan code in words[0]
 * label=2: packed mouse event in words[0]
 *---------------------------------------------------------------------------*/
static void drain_events(void)
{
    cubit_message_t msg;

    for (;;) {
        long found = syscall1(SYSCALL_RECEIVE_EVENT_NB, &msg);
        if (!found) break;

        if (msg.label == 1) {
            /* Keyboard event */
            uint8_t raw = (uint8_t)(msg.words[0] & 0xFF);
            cubit_key_event_t ev;
            if (raw & 0x80) {
                ev.scancode = raw & 0x7F;
                ev.pressed = 0;
            } else {
                ev.scancode = raw;
                ev.pressed = 1;
            }
            key_buf_push(ev);
        } else if (msg.label == 2) {
            /* Mouse event - unpack from services-mouse.adb packing */
            uint64_t packed = msg.words[0];
            cubit_mouse_event_t ev;
            ev.buttons = (uint8_t)(packed & 0xFF);
            ev.dx = sign_extend_12(packed >> 8);
            ev.dy = sign_extend_12(packed >> 20);
            ev.dz = (int8_t)((packed >> 32) & 0xFF);
            mouse_buf_push(ev);
        }
        /* Other labels silently ignored */
    }
}

/*---------------------------------------------------------------------------
 * cubit_keyboard_init - Register as the keyboard driver
 *---------------------------------------------------------------------------*/
void cubit_keyboard_init(void)
{
    syscall1(SYSCALL_REGISTER_DRIVER, DRIVER_KEYBOARD);
}

/*---------------------------------------------------------------------------
 * cubit_keyboard_poll - Drain pending events into ring buffers
 *---------------------------------------------------------------------------*/
void cubit_keyboard_poll(void)
{
    drain_events();
}

/*---------------------------------------------------------------------------
 * cubit_keyboard_get - Get next key event (non-blocking)
 * Returns 1 if event available, 0 otherwise.
 *---------------------------------------------------------------------------*/
int cubit_keyboard_get(cubit_key_event_t *ev)
{
    drain_events();
    return key_buf_pop(ev);
}

/*---------------------------------------------------------------------------
 * cubit_mouse_init - Register as the mouse driver
 *---------------------------------------------------------------------------*/
void cubit_mouse_init(void)
{
    syscall1(SYSCALL_REGISTER_DRIVER, DRIVER_MOUSE);
}

/*---------------------------------------------------------------------------
 * cubit_mouse_poll - Drain pending events into ring buffers
 *---------------------------------------------------------------------------*/
void cubit_mouse_poll(void)
{
    drain_events();
}

/*---------------------------------------------------------------------------
 * cubit_mouse_get - Get next mouse event (non-blocking)
 * Returns 1 if event available, 0 otherwise.
 *---------------------------------------------------------------------------*/
int cubit_mouse_get(cubit_mouse_event_t *ev)
{
    drain_events();
    return mouse_buf_pop(ev);
}
