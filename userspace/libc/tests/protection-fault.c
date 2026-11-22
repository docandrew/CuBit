/* Intentional user faults. The host verifier must correlate the armed PID and
 * address with a kernel fault and process retirement, not just a missing line. */
#define _GNU_SOURCE
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <cubit/debug.h>
#ifndef FAULT_READ_ONLY
#define FAULT_READ_ONLY 0
#endif
int main(void)
{
    volatile unsigned char *p = mmap(0, 4096, PROT_READ | PROT_WRITE,
                                    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (p == MAP_FAILED) return 1;
    *p = 0x5a;
    if (mprotect((void *)p, 4096, FAULT_READ_ONLY ? PROT_READ : PROT_NONE))
        return 2;
    char line[160];
    int n = snprintf(line, sizeof line,
        "PROTECTION-FAULT: %s armed pid=%ld address=%lx\n",
        FAULT_READ_ONLY ? "readonly-write" : "guard-read",
        (long)getpid(), (unsigned long)p);
    cubit_debug_write(line, n);
    if (FAULT_READ_ONLY) *p = 0x19;
    else { volatile unsigned char value = *p; (void)value; }
    const char *failed = "PROTECTION-FAULT: FAIL access unexpectedly succeeded\n";
    cubit_debug_write(failed, strlen(failed));
    return 3;
}
