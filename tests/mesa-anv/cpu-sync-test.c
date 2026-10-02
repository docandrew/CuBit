/* Compile the actual implementation with Mesa types and real host pthreads.
 * No GPU or native CuBit condition-variable claim. */
#include "../../userspace/mesa/anv/anv_cubit_sync.c"
#include <assert.h>
#include <stdio.h>
#ifdef CUBIT_SYNC_NATIVE_TEST
#include <cubit/debug.h>
#include <stdlib.h>
#include <string.h>
static void report_native(const char *message)
{ cubit_debug_write(message, strlen(message)); }
#undef assert
#define assert(condition) do { if (!(condition)) { \
   report_native("MESA-SYNC-NATIVE: FAIL " #condition "\n"); abort(); \
} } while (0)
#endif
static struct vk_device device;
static struct cubit_sync a, b;
static void *signaler(void *unused)
{
   (void)unused;
   assert(signal_value(&device, &a.base, 5) == VK_SUCCESS);
   assert(signal_value(&device, &b.base, 7) == VK_SUCCESS);
   return NULL;
}
static void *lose_device(void *unused)
{
   (void)unused;
   struct timespec delay = { .tv_nsec = 2000000 };
   nanosleep(&delay, NULL);
   p_atomic_set(&device._lost.lost, 1);
   /* Intentionally no condition broadcast: bounded waits must observe loss. */
   return NULL;
}
int main(void)
{
#ifdef CUBIT_SYNC_NATIVE_TEST
   report_native("MESA-SYNC-NATIVE: starting\n");
#endif
   a.base.type = b.base.type = &anv_cubit_cpu_timeline_type;
   a.base.flags = b.base.flags = VK_SYNC_IS_TIMELINE;
   assert(initialize(&device, &a.base, 0) == VK_SUCCESS);
   assert(initialize(&device, &b.base, 0) == VK_SUCCESS);
   struct vk_sync_wait waits[] = {{.sync=&a.base, .wait_value=5}, {.sync=&b.base, .wait_value=7}};
   assert(wait_values(&device, 2, waits, 0, 0) == VK_TIMEOUT);
   assert(wait_values(&device, 2, waits, VK_SYNC_WAIT_PENDING, 0) == VK_TIMEOUT);
   assert(signal_value(&device, &a.base, 5) == VK_SUCCESS);
   assert(wait_values(&device, 2, waits, VK_SYNC_WAIT_ANY, 0) == VK_SUCCESS);
   assert(wait_values(&device, 2, waits, 0, 0) == VK_TIMEOUT);
   pthread_t thread;
   assert(!pthread_create(&thread, NULL, signaler, NULL));
   assert(wait_values(&device, 2, waits, 0, UINT64_MAX) == VK_SUCCESS);
   assert(!pthread_join(thread, NULL));
   uint64_t value;
   assert(get_value(&device, &b.base, &value) == VK_SUCCESS && value == 7);
   assert(signal_value(&device, &b.base, 6) == VK_ERROR_UNKNOWN);
   assert(get_value(&device, &b.base, &value) == VK_SUCCESS && value == 7);
   assert(wait_values(&device, 0, NULL, 0, 0) == VK_SUCCESS);
   waits[0].wait_value = 8;
   struct timespec now;
   assert(!clock_gettime(CLOCK_MONOTONIC, &now));
   uint64_t deadline = (uint64_t)now.tv_sec * UINT64_C(1000000000) + now.tv_nsec + 1000000;
   assert(wait_values(&device, 1, waits, 0, deadline) == VK_TIMEOUT);
   assert(!pthread_create(&thread, NULL, lose_device, NULL));
   assert(wait_values(&device, 1, waits, 0, UINT64_MAX) == VK_ERROR_DEVICE_LOST);
   assert(!pthread_join(thread, NULL));
   assert(signal_value(&device, &a.base, 9) == VK_ERROR_DEVICE_LOST);
   assert(get_value(&device, &a.base, &value) == VK_ERROR_DEVICE_LOST);
   struct cubit_sync shared = {.base.flags = VK_SYNC_IS_TIMELINE | VK_SYNC_IS_SHAREABLE};
   assert(initialize(&device, &shared.base, 0) == VK_ERROR_FEATURE_NOT_PRESENT);
   finish(&device, &a.base); finish(&device, &b.base);
#ifdef CUBIT_SYNC_NATIVE_TEST
   report_native("MESA-SYNC-NATIVE: PASS timeline threads deadlines loss\n");
#else
   puts("CPU timeline PASS: pthread all/any, deadline, monotonic values, device-loss wake, no sharing; host only");
#endif
}
