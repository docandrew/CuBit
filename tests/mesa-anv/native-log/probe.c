#include <stdint.h>
#include <stddef.h>
extern uint32_t cubit_test_run_logged(uint32_t (*callback)(void *));
extern void cubit_test_log(void *, const char *, uint32_t);
static uint32_t probe(void *context)
{
    static const char text[] = "MESA-LOG native bridge record";
    cubit_test_log(context, text, sizeof(text) - 1);
    cubit_test_log(context, "bad\nrecord", 10);
    cubit_test_log(context, NULL, 1);
    cubit_test_log(context, text, 0);
    return 73;
}
uint32_t cubit_log_smoke(void) { return cubit_test_run_logged(probe); }

static uint32_t burst(void *context)
{
    static const char text[] = "MESA-LOG paced burst record";
    for (unsigned i = 0; i < 80; i++)
        cubit_test_log(context, text, sizeof(text) - 1);
    return 73;
}
uint32_t cubit_log_burst(void) { return cubit_test_run_logged(burst); }
