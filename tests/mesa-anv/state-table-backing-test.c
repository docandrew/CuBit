/* Hosted failure-injection test of the production CuBit ANV backing helper.
 * Syscall exports are mocked; no memory mapping or GPU execution is claimed.
 */
#include "anv_private.h"
#include "anv_cubit_state_table.h"
#include <assert.h>
#include <stdio.h>

static uint64_t committed;
static unsigned commits, reserves, releases, fail_commit;
static bool fail_reserve, fail_release;
static const uint64_t address = UINT64_C(0x580000000000);

uint64_t cubit_owned_reserve(uint64_t bytes);
uint64_t cubit_owned_commit_prefix(uint64_t base, uint64_t offset, uint64_t bytes);
uint64_t cubit_owned_release_reservation(uint64_t base, uint64_t bytes);

uint64_t cubit_owned_reserve(uint64_t bytes)
{
   assert(bytes == BLOCK_POOL_MEMFD_SIZE);
   ++reserves;
   return fail_reserve ? 0 : address;
}

uint64_t cubit_owned_commit_prefix(uint64_t base, uint64_t offset, uint64_t bytes)
{
   assert(base == address && offset == committed);
   assert(bytes && bytes % 4096 == 0 && bytes <= 16u * 1024 * 1024);
   ++commits;
   if (commits == fail_commit)
      return UINT64_MAX;
   committed += bytes;
   return 0;
}

uint64_t cubit_owned_release_reservation(uint64_t base, uint64_t bytes)
{
   assert(base == address && bytes == BLOCK_POOL_MEMFD_SIZE);
   ++releases;
   if (fail_release)
      return UINT64_MAX;
   committed = 0;
   return 0;
}

VkResult __vk_errorf(const void *obj, VkResult error, const char *file,
                    int line, const char *format, ...)
{
   (void)obj; (void)file; (void)line; (void)format;
   return error;
}

static void reset(void)
{
   committed = 0;
   commits = reserves = releases = fail_commit = 0;
   fail_reserve = fail_release = false;
}

int main(void)
{
   struct anv_state_table table = {0};
   reset();
   assert(anv_cubit_state_table_init(&table, NULL, 0) == VK_ERROR_OUT_OF_HOST_MEMORY);
   assert(anv_cubit_state_table_init(&table, NULL,
          BLOCK_POOL_MEMFD_SIZE / sizeof(*table.map) + 1) ==
          VK_ERROR_OUT_OF_HOST_MEMORY);
   assert(reserves == 0);
   fail_reserve = true;
   assert(anv_cubit_state_table_init(&table, NULL, 64) == VK_ERROR_OUT_OF_HOST_MEMORY);
   assert(!table.map && !commits && !releases);

   reset();
   fail_commit = 1;
   assert(anv_cubit_state_table_init(&table, NULL, 64) == VK_ERROR_OUT_OF_HOST_MEMORY);
   assert(releases == 1 && !table.map && !committed);

   /* Initialization failure must not forget a reservation whose release
    * failed. A later finish can still retire it. */
   reset();
   fail_commit = 1;
   fail_release = true;
   assert(anv_cubit_state_table_init(&table, NULL, 64) == VK_ERROR_OUT_OF_HOST_MEMORY);
   assert(releases == 1 && (uintptr_t)table.map == address);
   assert(!table.size && !table.cubit_committed);
   fail_release = false;
   anv_cubit_state_table_finish(&table);
   assert(releases == 2 && !table.map);

   reset();
   assert(anv_cubit_state_table_init(&table, NULL, 64) == VK_SUCCESS);
   assert((uintptr_t)table.map == address && table.cubit_committed == 4096);
   uint32_t old_size = table.size;
   unsigned old_commits = commits;
   assert(anv_cubit_state_table_expand(&table, 0) == VK_ERROR_OUT_OF_HOST_MEMORY);
   assert(anv_cubit_state_table_expand(&table, BLOCK_POOL_MEMFD_SIZE + 1) ==
          VK_ERROR_OUT_OF_HOST_MEMORY);
   assert(commits == old_commits && table.size == old_size);

   /* Fail after one successful chunk. Retain the physical progress but do
    * not publish the larger logical table or replay committed pages. */
   const uint32_t wanted = 32u * 1024 * 1024 + 4096;
   fail_commit = commits + 2;
   assert(anv_cubit_state_table_expand(&table, wanted) == VK_ERROR_OUT_OF_HOST_MEMORY);
   assert(table.size == old_size && table.cubit_committed == 16u * 1024 * 1024 + 4096);
   assert(committed == table.cubit_committed && (uintptr_t)table.map == address);
   fail_commit = 0;
   old_commits = commits;
   assert(anv_cubit_state_table_expand(&table, wanted) == VK_SUCCESS);
   assert(commits == old_commits + 1 && table.size == wanted && committed == wanted);

   fail_release = true;
   anv_cubit_state_table_finish(&table);
   assert(table.map && table.size == wanted && table.cubit_committed == wanted);
   fail_release = false;
   anv_cubit_state_table_finish(&table);
   assert(!table.map && !table.size && !table.cubit_committed && !committed);
   old_commits = releases;
   anv_cubit_state_table_finish(&table);
   assert(releases == old_commits);

   reset();
   assert(anv_cubit_state_table_init(&table, NULL, 1) == VK_SUCCESS);
   old_commits = commits;
   assert(anv_cubit_state_table_expand(&table, 4095) == VK_SUCCESS);
   assert(table.size == 4095 && table.cubit_committed == 4096);
   assert(commits == old_commits);
   assert(anv_cubit_state_table_expand(&table, 4097) == VK_SUCCESS);
   assert(table.size == 4097 && table.cubit_committed == 8192);
   assert(commits == old_commits + 1);
   assert(anv_cubit_state_table_expand(&table, 4096) == VK_ERROR_OUT_OF_HOST_MEMORY);
   assert(table.size == 4097 && table.cubit_committed == 8192);
   assert(anv_cubit_state_table_expand(&table, 4097) == VK_SUCCESS);
   assert(commits == old_commits + 1);
   anv_cubit_state_table_finish(&table);
   assert(!table.map && !committed);
   puts("PASS ANV CPU backing failure paths; hosted mock syscalls only");
   return 0;
}
