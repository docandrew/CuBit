#include "../../userspace/c/cubit_desktop.h"

/* Exercise the actual C port adapter against the Ada codec's test corpus. */
int cubit_test_input_reply_valid(uint32_t expected, uint32_t label,
    uint8_t length, uint8_t flags, uint16_t reserved,
    uint64_t w0, uint64_t w1, uint64_t w2, uint64_t w3)
{
    const uint64_t words[4] = {w0, w1, w2, w3};
    return cubit_desktop_input_reply_valid(expected, label, length, flags,
                                          reserved, words);
}
