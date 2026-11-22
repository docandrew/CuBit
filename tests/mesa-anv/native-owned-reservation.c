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

/* Owned memory, one region API (kernel Process.Owned_Memory, ABI-002). */
enum { RESERVE_OWNED_REGION = 146, CHANGE_OWNED_RANGE = 147, RELEASE_OWNED_REGION = 148 };
enum { COMMIT = 0, DECOMMIT = 1, MAKE_READ_WRITE = 4 };

int cubit_test_owned_reservations(void)
{
   const unsigned long capacity = 2UL * 1024 * 1024 * 1024;
   const unsigned long rejected = ~0UL;
   CHECK(call(RESERVE_OWNED_REGION, 0, 0, 0) == 0, "zero capacity");
   CHECK(call(RESERVE_OWNED_REGION, 4097, 0, 0) == 0, "unaligned capacity");
   CHECK(call(RESERVE_OWNED_REGION, capacity + 4096, 0, 0) == 0, "oversize capacity");
   for (unsigned round = 0; round < 8; ++round) {
      unsigned long base = call(RESERVE_OWNED_REGION, capacity, 0, 0);
      CHECK(base && base % 4096 == 0, "reserve 2GiB without backing");
      CHECK(call(CHANGE_OWNED_RANGE, base + capacity, 4096, COMMIT) == rejected,
            "commit past the region rejected");
      CHECK(call(CHANGE_OWNED_RANGE, base, 0, COMMIT) == rejected, "zero commit rejected");
      CHECK(call(CHANGE_OWNED_RANGE, base, 4097, COMMIT) == rejected, "unaligned commit rejected");
      CHECK(call(CHANGE_OWNED_RANGE, base, 16UL * 1024 * 1024 + 4096, COMMIT) == rejected,
            "over-budget commit rejected");
      CHECK(call(CHANGE_OWNED_RANGE, base, 4096, MAKE_READ_WRITE) == rejected,
            "cannot protect unbacked capacity");
      CHECK(call(CHANGE_OWNED_RANGE, base, 4096, COMMIT) == 0, "first commit");
      volatile uint64_t *first = (volatile uint64_t *)base;
      CHECK(first[0] == 0 && first[511] == 0, "initial zero fill");
      first[0] = 0x4355424900000000ULL + round;
      first[511] = 0x535441424c450000ULL + round;
      CHECK(call(CHANGE_OWNED_RANGE, base, 4096, COMMIT) == rejected, "replayed commit rejected");

      /* Interleave another region's frames between this region's runs. */
      unsigned long other = call(RESERVE_OWNED_REGION, 4096, 0, 0);
      CHECK(other && (other < base || other >= base + capacity), "regions disjoint");
      CHECK(call(CHANGE_OWNED_RANGE, other, 4096, COMMIT) == 0, "other commit");
      volatile uint64_t *unrelated = (volatile uint64_t *)other;
      unrelated[0] = 0x494e5445524c4541ULL;
      CHECK(call(CHANGE_OWNED_RANGE, base + 4096, 8192, COMMIT) == 0, "second commit");
      volatile uint64_t *new_page = (volatile uint64_t *)(base + 8192);
      CHECK(new_page[0] == 0 && new_page[511] == 0, "growth zero fill");
      CHECK(first[0] == 0x4355424900000000ULL + round &&
            first[511] == 0x535441424c450000ULL + round, "stable old pointers");
      new_page[511] = 0x47524f57;
      CHECK(call(RELEASE_OWNED_REGION, base, 4096, 0) == rejected, "release needs the whole region");
      CHECK(call(RELEASE_OWNED_REGION, base, capacity - 4096, 0) == rejected, "wrong release capacity");
      CHECK(call(RELEASE_OWNED_REGION, base, capacity, 0) == 0, "retire interleaved runs");
      CHECK(unrelated[0] == 0x494e5445524c4541ULL, "unrelated backing survives");
      CHECK(call(CHANGE_OWNED_RANGE, base + 12288, 4096, COMMIT) == rejected,
            "retired region rejected");
      CHECK(call(RELEASE_OWNED_REGION, base, capacity, 0) == rejected, "duplicate release rejected");
      CHECK(call(RELEASE_OWNED_REGION, other, 4096, 0) == 0, "unrelated release");
   }
   const char *pass = "TEST: PASS native owned reservation growth and retirement (NO GPU)\n";
   cubit_debug_write(pass, strlen(pass));
   return cubit_test_gpu_metadata();
}
