/* Hosted transport-argument test; kernel futex behavior is not simulated. */
#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/syscall.h>
#include "util/futex.h"

static uint32_t word;
static struct timespec deadline = {123, 456789};
static int expected_op, expected_value;
static const struct timespec *expected_timeout;
static long reply;

long syscall(long number, ...)
{
   va_list ap;
   va_start(ap, number);
   assert(number == SYS_futex);
   assert(va_arg(ap, uint32_t *) == &word);
   assert(va_arg(ap, int) == expected_op);
   assert(va_arg(ap, int) == expected_value);
   assert(va_arg(ap, const struct timespec *) == expected_timeout);
   assert(va_arg(ap, void *) == NULL);
   if (expected_op == 9) assert(va_arg(ap, unsigned) == UINT32_MAX);
   else assert(va_arg(ap, int) == 0);
   va_end(ap);
   if (reply == -1) errno = EAGAIN;
   return reply;
}

int main(void)
{
   expected_op = 9;
   expected_value = -7;
   expected_timeout = &deadline;
   reply = -1;
   assert(futex_wait(&word, -7, &deadline) == -1 && errno == EAGAIN);
   expected_timeout = NULL;
   reply = 0;
   assert(futex_wait(&word, -7, NULL) == 0);
   expected_op = 1;
   expected_value = INT32_MAX;
   reply = 3;
   assert(futex_wake(&word, INT32_MAX) == 3);
   puts("Mesa CuBit futex adapter PASS: absolute timeout, null wait, wake count, errno (host arguments only)");
}
