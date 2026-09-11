#ifndef CUBIT_DESKTOP_H
#define CUBIT_DESKTOP_H
#include "cubit.h"

/* Checked input envelope, matching the SPARK codec. Status-only errors must
 * never be interpreted as input event numbers. Packed payloads remain the
 * port ABI; check their bounds before a C frontend narrows them. */
static inline int cubit_desktop_input_reply_valid(
    uint32_t expected, uint32_t label, uint8_t length, uint8_t flags,
    uint16_t reserved, const uint64_t words[4])
{
    uint64_t kind = words[0], first = words[2], second = words[3];
    if ((expected != 0x0821 && expected != 0x0822) || label != expected ||
        length != 4 || flags > 1 || reserved || kind > 9)
        return 0;
    switch (kind) {
    case 0: return first == 0 && second == 0 && flags == 0;
    case 1: case 2: return first <= 127 && second <= 15;
    case 6: return first <= 255 && second == 0;
    case 8: return first <= 65535 && second <= 65535;
    default:
        if ((first & 0xffffffffu) > 65535 || (first >> 32) > 65535)
            return 0;
        if (kind == 9) return (second >> 32) <= 15;
        if (kind == 7) return 1; /* Signed 32-bit wheel + 32-bit buttons. */
        return second <= 0xffffffffu;
    }
}

/* Native fixed-BGRA8888 attachment; mirrors CuBit.Desktop_Protocol.
 * A slot is not an address. The server acquires the generation-bearing grant.
 * Successful replacement releases the previous server acquisition.
 */
static inline int cubit_desktop_attach_buffer(uint64_t surface, uint64_t slot,
                                             uint32_t width, uint32_t height,
                                             uint32_t pitch)
{
    cubit_async_message_t message = {0};
    long generation;
    if (!surface || slot > 4095 || !width || !height ||
        width > 65535 || height > 65535 || pitch < width * 4 ||
        pitch > (16u * 1024u * 1024u) / height)
        return -1;
    generation = syscall1(SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, slot);
    if (generation <= 0 || (uint64_t)generation > 0xffffffffu)
        return -1;
    message.tag.label = 0x0814;
    message.tag.length = 4;
    message.words[0] = surface;
    message.words[1] = slot;
    message.words[2] = (uint64_t)generation;
    message.words[3] = (uint64_t)width | ((uint64_t)height << 16) |
                       ((uint64_t)pitch << 32);
    if (syscall2(SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY, CAP_SLOT_DESKTOP,
                 &message) == -1 || message.tag.label != 0x0814 ||
        message.tag.length != 1 || message.words[0] != 0)
        return -1;
    return 0;
}
#endif
