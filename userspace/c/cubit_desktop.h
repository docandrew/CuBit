#ifndef CUBIT_DESKTOP_H
#define CUBIT_DESKTOP_H
#include "cubit.h"

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
    if (generation <= 0 || (uint64_t)generation > UINT32_MAX)
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
