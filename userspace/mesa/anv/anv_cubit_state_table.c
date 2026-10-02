/* CuBit backing for upstream ANV CPU metadata. The table's indexing, free
 * lists and allocation synchronization stay in upstream anv_allocator.c.
 * A fixed virtual reservation replaces remapped aliases of a Linux memfd.
 */
#include "anv_private.h"
#include "anv_cubit_state_table.h"

/* Common CuBit runtime exports; no process-global emulated file descriptors. */
extern uint64_t cubit_owned_reserve(uint64_t);
extern uint64_t cubit_owned_commit_prefix(uint64_t, uint64_t, uint64_t);
extern uint64_t cubit_owned_release_reservation(uint64_t, uint64_t);

VkResult
anv_cubit_state_table_expand(struct anv_state_table *table, uint32_t size)
{
   if (!table->map || !size || size < table->size || size > BLOCK_POOL_MEMFD_SIZE)
      return vk_error(table->device, VK_ERROR_OUT_OF_HOST_MEMORY);
   const uint64_t wanted = ((uint64_t)size + 4095) & ~UINT64_C(4095);
   while (table->cubit_committed < wanted) {
      uint64_t bytes = MIN2(wanted - table->cubit_committed, UINT64_C(16) * 1024 * 1024);
      if (cubit_owned_commit_prefix((uintptr_t)table->map, table->cubit_committed, bytes))
         return vk_error(table->device, VK_ERROR_OUT_OF_HOST_MEMORY);
      /* Preserve successful partial growth on a later failure. Logical table
       * capacity is published only once every requested byte is backed. */
      table->cubit_committed += bytes;
   }
   table->size = size;
   return VK_SUCCESS;
}

void
anv_cubit_state_table_finish(struct anv_state_table *table)
{
   if (!table->map)
      return;
   if (cubit_owned_release_reservation((uintptr_t)table->map, BLOCK_POOL_MEMFD_SIZE)) {
      (void)vk_errorf(table->device, VK_ERROR_UNKNOWN,
                     "CuBit CPU table retirement failed; backing retained");
      return;
   }
   table->map = NULL;
   table->size = 0;
   table->cubit_committed = 0;
}

VkResult
anv_cubit_state_table_init(struct anv_state_table *table, struct anv_device *device,
                         uint32_t initial_entries)
{
   table->device = device;
   table->fd = -1;
   table->map = NULL;
   table->size = 0;
   table->state.u64 = 0;
   table->cubit_committed = 0;
   if (!initial_entries || initial_entries > BLOCK_POOL_MEMFD_SIZE / sizeof(*table->map))
      return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);
   table->map = (void *)(uintptr_t)cubit_owned_reserve(BLOCK_POOL_MEMFD_SIZE);
   if (!table->map)
      return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);
   VkResult result = anv_cubit_state_table_expand(table, initial_entries * sizeof(*table->map));
   if (result != VK_SUCCESS)
      anv_cubit_state_table_finish(table);
   return result;
}
