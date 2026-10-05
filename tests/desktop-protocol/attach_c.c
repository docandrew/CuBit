/* Hosted check of the production C attachment helper. Only syscalls are
 * replaced; actual message types, constants and helper are used. */
#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include "../../userspace/c/cubit.h"
static unsigned queries, calls;
static uint64_t expected_slot;
static long generation = 65537;
static long query_generation(long op, uint64_t slot)
{
    assert(op == SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION);
    assert(slot == expected_slot);
    ++queries;
    return generation;
}
static long call_desktop(long op, long cap, cubit_async_message_t *m)
{
    assert(op == SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY);
    assert(cap == CAP_SLOT_DESKTOP);
    assert(m->tag.label == 0x814 && m->tag.length == 4);
    assert(m->words[0] == 3 && m->words[1] == expected_slot);
    assert(m->words[2] == (uint64_t)generation);
    ++calls;
    m->tag.length = 1;
    m->words[0] = 0;
    return 0;
}
#undef syscall1
#undef syscall2
#define syscall1 query_generation
#define syscall2 call_desktop
#include "../../userspace/c/cubit_desktop.h"
int main(void)
{
    const uint64_t valid[] = {0, 4095, 4096, 143360, 1048575};
    for (unsigned i = 0; i < 5; ++i) {
        expected_slot = valid[i];
        assert(cubit_desktop_attach_buffer(3, expected_slot, 640, 400, 2560) == 0);
    }
    assert(queries == 5 && calls == 5);
    assert(cubit_desktop_attach_buffer(3, 1048576, 640, 400, 2560) == -1);
    assert(cubit_desktop_attach_buffer(3, UINT64_MAX, 640, 400, 2560) == -1);
    assert(queries == 5 && calls == 5);
    expected_slot = 143360;
    const long invalid[] = {-1, 0, 4294967296L};
    for (unsigned i = 0; i < 3; ++i) {
        generation = invalid[i];
        assert(cubit_desktop_attach_buffer(3, expected_slot, 640, 400, 2560) == -1);
    }
    assert(queries == 8 && calls == 5);
    puts("PASS desktop global grant boundaries and generation rejection");
}
