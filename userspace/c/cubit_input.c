/*
 * CuBit OS - Keyboard Input
 * Copyright (C) 2026 Jon Andrew
 *
 * Polls the keyboard service for scan codes via non-blocking IPC.
 * Translates PS/2 Set 1 scan codes for use by applications.
 *
 * Protocol:
 *   1. Register as DRIVER_KEYBOARD (1) via SYSCALL_REGISTER_DRIVER
 *   2. Keyboard kernel service sends IPC messages with scan code in words[0]
 *   3. We poll with SYSCALL_RECEIVE_NB and reply with SYSCALL_REPLY
 */
#include "cubit.h"

#define DRIVER_KEYBOARD 1

/* IPC Message layout matching kernel Process.Message (40 bytes) */
typedef struct {
    uint32_t label;
    uint8_t  length;
    uint8_t  flags;
    uint16_t badge;
    uint64_t words[4];
} cubit_message_t;

/* IPC reply tag: REPLY_OK = 0xF000 */
#define REPLY_OK_LABEL  0xF000

/* Internal key event ring buffer */
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
 * cubit_keyboard_init - Register as the keyboard driver
 *---------------------------------------------------------------------------*/
void cubit_keyboard_init(void)
{
    syscall1(SYSCALL_REGISTER_DRIVER, DRIVER_KEYBOARD);
}

/*---------------------------------------------------------------------------
 * cubit_keyboard_poll - Drain pending keyboard IPC messages into ring buffer
 *
 * Call this at least once per frame to avoid the keyboard service blocking
 * too long on its synchronous send().
 *---------------------------------------------------------------------------*/
void cubit_keyboard_poll(void)
{
    cubit_message_t msg;
    cubit_key_event_t ev;

    /* Drain all pending keyboard messages */
    for (;;) {
        long sender = syscall1(SYSCALL_RECEIVE_NB, &msg);
        if (sender == 0) break;  /* no message */

        /* Extract scan code from words[0] */
        uint8_t raw = (uint8_t)(msg.words[0] & 0xFF);

        /* PS/2 Set 1: bit 7 = break (release) */
        if (raw & 0x80) {
            ev.scancode = raw & 0x7F;
            ev.pressed = 0;
        } else {
            ev.scancode = raw;
            ev.pressed = 1;
        }

        key_buf_push(ev);

        /* Reply to unblock the keyboard service */
        cubit_syscall(SYSCALL_REPLY, sender,
                      /* tag: label=REPLY_OK, length=0, flags=0, badge=0 */
                      ((long)REPLY_OK_LABEL),
                      0, 0, 0, 0);
    }
}

/*---------------------------------------------------------------------------
 * cubit_keyboard_get - Get next key event (non-blocking)
 * Returns 1 if event available, 0 otherwise.
 *---------------------------------------------------------------------------*/
int cubit_keyboard_get(cubit_key_event_t *ev)
{
    cubit_keyboard_poll();
    return key_buf_pop(ev);
}
