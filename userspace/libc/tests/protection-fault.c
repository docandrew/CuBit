/* Intentional user faults. The host verifier must correlate the armed PID and
 * address with a kernel fault and process retirement, not just a missing line.
 *
 * Built four times (kernel/Makefile):
 *   guard-fault.app           a PROT_NONE page read;
 *   readonly-fault.app        a PROT_READ page written;
 *   discard-readonly-fault    (FAULT_DISCARD_READ_ONLY) a read-only page
 *                             discarded (MEM-007, K12): it reads zero, and
 *                             a write still faults (no access upgrade);
 *   discard-reserved-fault    (FAULT_DISCARD_RESERVED) a reserved page
 *                             discarded: it stays reserved, a read faults.
 */
#define _GNU_SOURCE
#include <stdint.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <cubit/debug.h>
#include <cubit/memory.h>
#ifndef FAULT_READ_ONLY
#define FAULT_READ_ONLY 0
#endif
#ifndef FAULT_DISCARD_READ_ONLY
#define FAULT_DISCARD_READ_ONLY 0
#endif
#ifndef FAULT_DISCARD_RESERVED
#define FAULT_DISCARD_RESERVED 0
#endif

static void say(const char *kind, volatile unsigned char *p)
{
    char line[160];
    int n = snprintf(line, sizeof line, "PROTECTION-FAULT: %s armed pid=%ld address=%lx\n",
                     kind, (long)getpid(), (unsigned long)p);
    cubit_debug_write(line, n);
}

static int fail(const char *why)
{
    char line[160];
    int n = snprintf(line, sizeof line, "PROTECTION-FAULT: FAIL %s\n", why);
    cubit_debug_write(line, n);
    return 3;
}

int main(void)
{
    volatile unsigned char *p;
    if (FAULT_DISCARD_RESERVED) {
        /* Address space only: no page committed. */
        p = mmap(0, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
        if (p == MAP_FAILED) return 1;
        if (cubit_owned_discard((uintptr_t)p, 4096) != 0)
            return fail("discard of a reserved page refused");
        say("discard-reserved-read", p);
        { volatile unsigned char value = *p; (void)value; }
        return fail("a discarded reserved page was readable");
    }
    p = mmap(0, 4096, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (p == MAP_FAILED) return 1;
    *p = 0x5a;
    if (FAULT_DISCARD_READ_ONLY) {
        if (mprotect((void *)p, 4096, PROT_READ)) return 2;
        if (cubit_owned_discard((uintptr_t)p, 4096) != 0)
            return fail("discard of a read-only page refused");
        if (*p != 0) return fail("a discarded page did not read zero");
        say("discard-readonly-write", p);
        *p = 0x19;
        return fail("a discarded read-only page was writable");
    }
    if (mprotect((void *)p, 4096, FAULT_READ_ONLY ? PROT_READ : PROT_NONE))
        return 2;
    say(FAULT_READ_ONLY ? "readonly-write" : "guard-read", p);
    if (FAULT_READ_ONLY) *p = 0x19;
    else { volatile unsigned char value = *p; (void)value; }
    return fail("access unexpectedly succeeded");
}
