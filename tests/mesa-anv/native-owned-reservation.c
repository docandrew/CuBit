/* Native CuBit syscall integration test; no Mesa device or GPU authority.
 * Numbers follow kernel/src/syscall.ads. This is not a libc mmap replacement.
 */
#include <cubit/debug.h>
#include <stdint.h>
#include <string.h>

int cubit_test_owned_reservations(void);
int cubit_test_gpu_metadata(void);

static unsigned long call(unsigned long n, unsigned long a,
                          unsigned long b, unsigned long c)
{
   unsigned long ret;
   __asm__ __volatile__("syscall" : "=a"(ret)
      : "a"(n), "D"(a), "S"(b), "d"(c) : "rcx", "r11", "memory");
   return ret;
}

#define CHECK(condition, text) do { if (!(condition)) { \
   const char *error = "TEST: FAIL native owned reservation: " text "\n"; \
   cubit_debug_write(error, strlen(error)); return 1; } } while (0)

int cubit_test_owned_reservations(void)
{
   const unsigned long capacity = 2UL * 1024 * 1024 * 1024;
   const unsigned long rejected = ~0UL;
   CHECK(call(123, 0, 0, 0) == 0, "zero capacity");
   CHECK(call(123, 4097, 0, 0) == 0, "unaligned capacity");
   CHECK(call(123, capacity + 4096, 0, 0) == 0, "oversize capacity");
   for (unsigned round = 0; round < 8; ++round) {
      unsigned long base = call(123, capacity, 0, 0);
      CHECK(base && base % 4096 == 0, "reserve 2GiB without backing");
      CHECK(call(124, base, 4096, 4096) == rejected, "skip prefix rejected");
      CHECK(call(124, base + 4096, 0, 4096) == rejected, "interior base rejected");
      CHECK(call(124, base, 0, 0) == rejected, "zero commit rejected");
      CHECK(call(124, base, 0, 4097) == rejected, "unaligned commit rejected");
      CHECK(call(124, base, 0, 16UL * 1024 * 1024 + 4096) == rejected,
            "over-budget commit rejected");
      CHECK(call(117, base, 4096, 3) == rejected, "cannot protect unbacked capacity");
      CHECK(call(124, base, 0, 4096) == 0, "first commit");
      volatile uint64_t *first = (volatile uint64_t *)base;
      CHECK(first[0] == 0 && first[511] == 0, "initial zero fill");
      first[0] = 0x4355424900000000ULL + round;
      first[511] = 0x535441424c450000ULL + round;
      CHECK(call(124, base, 0, 4096) == rejected, "replayed prefix rejected");

      /* Interleave a normal allocation between chunk frame-list runs. */
      unsigned long other = call(115, 4096, 0, 0);
      CHECK(other && (other < base || other >= base + capacity), "reservation excludes allocation");
      volatile uint64_t *unrelated = (volatile uint64_t *)other;
      unrelated[0] = 0x494e5445524c4541ULL;
      CHECK(call(124, base, 4096, 8192) == 0, "second commit");
      volatile uint64_t *new_page = (volatile uint64_t *)(base + 8192);
      CHECK(new_page[0] == 0 && new_page[511] == 0, "growth zero fill");
      CHECK(first[0] == 0x4355424900000000ULL + round &&
            first[511] == 0x535441424c450000ULL + round, "stable old pointers");
      new_page[511] = 0x47524f57;
      CHECK(call(116, base, 4096, 0) == rejected, "legacy release cannot detach chunk");
      CHECK(call(125, base, capacity - 4096, 0) == rejected, "wrong release capacity");
      CHECK(call(125, base, capacity, 0) == 0, "retire interleaved chunks");
      CHECK(unrelated[0] == 0x494e5445524c4541ULL, "unrelated backing survives");
      CHECK(call(124, base, 12288, 4096) == rejected, "retired reservation rejected");
      CHECK(call(125, base, capacity, 0) == rejected, "duplicate release rejected");
      CHECK(call(116, other, 4096, 0) == 0, "unrelated release");
   }
   const char *pass = "TEST: PASS native owned reservation growth and retirement (NO GPU)\n";
   cubit_debug_write(pass, strlen(pass));
   return cubit_test_gpu_metadata();
}
