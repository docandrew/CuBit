/* CPU backing contract used by ANV's anv_state_table_expand_range:
 * growing prefix mappings must share storage and preserve old pointers.
 * Linux-hosted OS-semantics fixture, NOT native CuBit or GPU execution.
 * Build with -DCONTRACT_PRIVATE_MUTANT to demonstrate rejection of copies.
 */
#define _GNU_SOURCE
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

int main(void)
{
   enum { VIEWS = 6, PAGE = 4096 };
   uint32_t *views[VIEWS];
   size_t sizes[VIEWS];
   int fd = memfd_create("anv-cpu-state-contract", MFD_CLOEXEC);
   assert(fd >= 0);
   /* Logical capacity only: this must not eagerly allocate 2 GiB of RAM. */
   assert(ftruncate(fd, (off_t)2 * 1024 * 1024 * 1024) == 0);

   for (unsigned i = 0; i < VIEWS; ++i) {
      sizes[i] = (size_t)PAGE << i;
#ifdef CONTRACT_PRIVATE_MUTANT
      const int mode = MAP_PRIVATE;
#else
      const int mode = MAP_SHARED;
#endif
      views[i] = mmap(NULL, sizes[i], PROT_READ | PROT_WRITE,
                      mode | MAP_POPULATE, fd, 0);
      assert(views[i] != MAP_FAILED);
      /* A pointer retained from before growth observes later updates. */
      views[i][0] = 0x43554200u + i;
      for (unsigned old = 0; old <= i; ++old)
         assert(views[old][0] == 0x43554200u + i);

      /* Updates through an older alias are also visible in the new one. */
      for (unsigned old = 0; old <= i; ++old) {
         size_t last = sizes[old] / sizeof(uint32_t) - 1;
         views[old][last] = 0x4d455300u + old;
         assert(views[i][last] == 0x4d455300u + old);
      }
      /* Previously unbacked prefix extension starts zero-filled. */
      if (i)
         assert(views[i][sizes[i - 1] / sizeof(uint32_t)] == 0);
   }

   /* Closing the descriptor or retiring an alias cannot invalidate peers. */
   assert(close(fd) == 0);
   for (unsigned i = 0; i + 1 < VIEWS; ++i)
      assert(munmap(views[i], sizes[i]) == 0);
   assert(views[VIEWS - 1][0] == 0x43554200u + VIEWS - 1);
   assert(munmap(views[VIEWS - 1], sizes[VIEWS - 1]) == 0);
   puts("PASS CPU state mapping contract: six growing shared aliases; HOST ONLY");
   return 0;
}
