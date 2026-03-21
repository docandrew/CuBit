/*
 * CuBit OS - Spin test app
 * Simple looping program for testing process kill (Ctrl+C).
 * Prints tick count to serial and sleeps 1s between ticks.
 */
#include "cubit.h"

int main(void)
{
    int count = 0;
    char buf[64];

    cubit_puts("spin: running (Ctrl+C to kill)...\n");

    while (1) {
        count++;
        snprintf(buf, sizeof(buf), "spin: tick %d\n", count);
        cubit_puts(buf);
        cubit_sleep_ms(1000);
    }

    return 0;
}
