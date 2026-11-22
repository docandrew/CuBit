#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <sched.h>
#include <stdint.h>
#include <cubit/debug.h>
static void report(const char *s) { cubit_debug_write(s, strlen(s)); }
static void *worker(void *id) {
    char output[128];
    for (unsigned i=0; i<10000; ++i) {
        volatile double value = 6.132;
        sched_yield();
        if (snprintf(output, sizeof(output), "%.3f", value) != 5 || strcmp(output, "6.132")) {
            report("FLOAT-CHECK: FAIL threaded\n"); return (void *)1;
        }
    }
    report("FLOAT-CHECK: worker done\n");
    return 0;
}
int main(void) {
    volatile double values[] = {6.132, 5.0, 14.607, 0.125, -2.5, 123456.789};
    const char *expected[] = {"6.132", "5.000", "14.607", "0.125", "-2.500", "123456.789"};
    char output[128];
    report("FLOAT-CHECK: started\n");
    for (unsigned i = 0; i < sizeof(values)/sizeof(values[0]); ++i) {
        snprintf(output, sizeof(output), "FLOAT-CHECK: begin %u\n", i);
        report(output);
        int n = snprintf(output, sizeof(output), "%.3f", values[i]);
        if (n < 0 || strcmp(output, expected[i])) {
            report("FLOAT-CHECK: FAIL result="); report(output); report("\n"); return 1;
        }
        report("FLOAT-CHECK: value="); report(output); report("\n");
    }
    pthread_t threads[3];
    report("FLOAT-CHECK: threads begin\n");
    for (unsigned i=0;i<3;++i) if (pthread_create(&threads[i],0,worker,(void *)(uintptr_t)i)) {
        report("FLOAT-CHECK: FAIL create\n"); return 1;
    }
    for (unsigned i=0;i<3;++i) {
        void *result;
        if (pthread_join(threads[i],&result) || result) { report("FLOAT-CHECK: FAIL join\n"); return 1; }
    }
    report("FLOAT-CHECK: PASS\n");
    return 0;
}
