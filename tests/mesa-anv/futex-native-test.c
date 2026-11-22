/* Link with the patched upstream src/util/futex.c and run on CuBit.
 * No mocked syscalls: exercises Mesa -> libc -> kernel synchronization.
 */
#include <errno.h>
#include <pthread.h>
#include <sched.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include <cubit/debug.h>
#include "util/futex.h"

static uint32_t word;
static int failures;
static int waiter_result, waiter_errno;

static void check(int ok, const char *name)
{
   char line[160];
   int n = snprintf(line, sizeof line, "mesa-futex: %s %s\n",
                    name, ok ? "PASS" : "FAIL");
   if (n > 0 && (size_t)n < sizeof line)
      cubit_debug_write(line, (size_t)n);
   if (!ok) failures++;
}

static int deadline(struct timespec *out, long milliseconds)
{
   if (clock_gettime(CLOCK_MONOTONIC, out)) return -1;
   out->tv_sec += milliseconds / 1000;
   out->tv_nsec += (milliseconds % 1000) * 1000000;
   if (out->tv_nsec >= 1000000000) {
      out->tv_sec++;
      out->tv_nsec -= 1000000000;
   }
   return 0;
}

static int before(const struct timespec *a, const struct timespec *b)
{
   return a->tv_sec < b->tv_sec ||
          (a->tv_sec == b->tv_sec && a->tv_nsec < b->tv_nsec);
}

static void *waiter(void *unused)
{
   (void)unused;
   struct timespec limit;
   if (deadline(&limit, 3000)) {
      waiter_result = -2;
      return NULL;
   }
   waiter_result = futex_wait(&word, 0, &limit);
   waiter_errno = errno;
   return NULL;
}

int main(void)
{
   struct timespec limit, now;
   if (deadline(&limit, 30)) {
      check(0, "monotonic clock available");
      return 1;
   }
   /* A mismatched value must not sleep even with an unbounded timeout. */
   errno = 0;
   int result = futex_wait(&word, 1, NULL);
   check(result == -1 && errno == EAGAIN, "value mismatch");
   errno = 0;
   result = futex_wait(&word, 0, &limit);
   int timeout_errno = errno;
   int clock_ok = clock_gettime(CLOCK_MONOTONIC, &now) == 0;
   check(result == -1 && timeout_errno == ETIMEDOUT &&
         clock_ok && !before(&now, &limit), "absolute monotonic timeout");
   check(futex_wake(&word, 1) == 0, "empty wake count");

   pthread_t thread;
   if (deadline(&limit, 4000) || pthread_create(&thread, NULL, waiter, NULL)) {
      check(0, "waiter created");
      return 1;
   }
   /* Wake until the kernel reports a real queued waiter, not an arbitrary
    * startup delay. Both sides are bounded; no timing-based success claim.
    */
   int woke = 0;
   do {
      woke = futex_wake(&word, 1);
      if (woke != 0) break;
      sched_yield();
      if (clock_gettime(CLOCK_MONOTONIC, &now)) break;
   } while (before(&now, &limit));
   int joined = pthread_join(thread, NULL);
   check(joined == 0 && woke == 1 && waiter_result == 0,
         "cross-thread wait and wake count");
   (void)waiter_errno;
   check(futex_wake(&word, INT32_MAX) == 0, "wake all after join");
   check(failures == 0, "native adapter suite");
   return failures != 0;
}
