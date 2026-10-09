#include "native_gpu_mapping.h"
#include "native_gpu_buffers.h"
#include "native_gpu_memory.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

uint32_t
cubit_cpu_mapping_release(struct cubit_cpu_mapping *r, bool replace)
{
   if (!r || replace)
      return 5;
   if (r->state == CUBIT_MAP_RETIRED)
      return 0;
   if (r->state == CUBIT_MAP_LIVE) {
      /* Mark uncertain before transport; do not return the same borrow twice. */
      r->state = CUBIT_MAP_FAILED;
      if (cubit_intel_return_view(r->reference) != 0)
         return 5;
      r->address = 0;
      r->state = CUBIT_MAP_RETIRING;
   }
   if (r->state != CUBIT_MAP_RETIRING)
      return 5;
   const uint32_t result = cubit_intel_retire_mapping(r->slot, r->mapping);
   if (result == 4)
      return 4;
   r->state = result == 0 ? CUBIT_MAP_RETIRED : CUBIT_MAP_FAILED;
   return result == 0 ? 0 : 5;
}

uint32_t
cubit_cpu_mapping_open(struct cubit_cpu_mapping *r, uint64_t slot,
   uint32_t handle, uint64_t offset, uint64_t bytes, uint32_t writable)
{
   if (!r || r->state != CUBIT_MAP_EMPTY)
      return 5;
   r->state = CUBIT_MAP_FAILED;
   r->slot = slot;
   r->bytes = bytes;
   r->bo_offset = offset;
   r->bo_handle = handle;
   r->lookup_address = 0;
   r->address = r->reference = 0;
   r->mapping = 0;
   if (cubit_intel_map_buffer(slot, handle, offset, bytes, writable,
                             &r->mapping, &r->reference) != 0)
      return 5;
   r->state = CUBIT_MAP_RETIRING;
   if (cubit_intel_acquire_view(slot, r->reference, 0, bytes, writable,
                              &r->address) != 0) {
      r->address = 0;
      /* No successful acquisition => no borrow to return. Retire the grant. */
      (void)cubit_cpu_mapping_release(r, false);
      return 5;
   }
   r->state = CUBIT_MAP_LIVE;
   r->lookup_address = r->address;
   return 0;
}

uint32_t
cubit_cpu_tracker_map(struct cubit_cpu_mapping_tracker *t, uint32_t handle,
   uint64_t offset, uint64_t bytes, uint32_t writable, uint64_t *address)
{
   if (!address)
      return 5;
   *address = 0;
   if (!t || t->lost)
      return 5;
   const uint32_t capacity = t->grown ? t->capacity : CUBIT_CPU_MAPPING_CAPACITY;
   struct cubit_cpu_mapping *records = cubit_cpu_tracker_records(t);
   if (t->used == capacity) {
      /* Only the tracker owns these records, under its caller's lock. Move
       * outstanding records in order so reverse lookup still selects the
       * newest borrow. A retired tombstone has no remaining transport work;
       * dropping it does NOT reclaim BOs, GPU addresses or endpoint slots. */
      uint32_t retained = 0;
      for (uint32_t i = 0; i < t->used; i++) {
         if (records[i].state != CUBIT_MAP_RETIRED) {
            if (retained != i)
               records[retained] = records[i];
            retained++;
         }
      }
      for (uint32_t i = retained; i < t->used; i++)
         records[i] = (struct cubit_cpu_mapping){0};
      t->used = retained;
   }
   if (t->used >= capacity) {
      /* No IPC or lifetime change until bookkeeping is available. Existing
       * borrows survive OOM; a later map may try again without replaying IPC. */
      if (capacity > UINT32_MAX / 2 ||
          (uint64_t)capacity * 2 > SIZE_MAX / sizeof(*records))
         return 5;
      const uint32_t next = capacity * 2;
      struct cubit_cpu_mapping *grown = calloc(next, sizeof(*grown));
      if (!grown)
         return 5;
      memcpy(grown, records, t->used * sizeof(*grown));
      free(t->grown);
      t->grown = grown;
      t->capacity = next;
      records = grown;
   }
   struct cubit_cpu_mapping *r = &records[t->used++];
   const uint32_t result = cubit_cpu_mapping_open(r, t->slot, handle, offset, bytes, writable);
   if (result != 0) {
      t->lost = true;
      return result;
   }
   *address = r->address;
   return 0;
}

static uint32_t
tracker_unmap_attempt(struct cubit_cpu_mapping_tracker *t, uint32_t handle,
   uint64_t address, uint64_t bytes, bool replace)
{
   if (!t || replace)
      return 5;
   /* Newest outstanding first: distinct grants may expose the same VA.
    * A retired record must not hide another outstanding borrow. Preserve
    * retiring/failed records in this ordering so uncertainty is not skipped.
    * As with Vulkan, callers must not unmap a stale pointer after remapping. */
   bool retired_match = false;
   for (uint32_t i = t->used; i > 0; i--) {
      struct cubit_cpu_mapping *r = &cubit_cpu_tracker_records(t)[i - 1];
      if (address && r->lookup_address == address && r->bo_handle == handle &&
          r->bytes == bytes) {
         if (r->state == CUBIT_MAP_RETIRED) {
            retired_match = true;
            continue;
         }
         const uint32_t result = cubit_cpu_mapping_release(r, false);
         if (result != 0 && result != 4)
            t->lost = true;
         return result;
      }
   }
   if (retired_match)
      return 0;
   t->lost = true;
   return 5;
}

uint32_t
cubit_cpu_tracker_unmap_wait(struct cubit_cpu_mapping_tracker *t, uint32_t handle,
   uint64_t address, uint64_t bytes, bool replace, uint32_t polls,
   void (*wait_pending)(void))
{
   uint32_t result = tracker_unmap_attempt(t, handle, address, bytes, replace);
   while (result == 4 && polls && wait_pending) {
      polls--;
      wait_pending();
      /* RETIRING records poll retirement only; the borrow was already returned.
       * Caller serialization keeps lookup stable throughout this bounded wait. */
      result = tracker_unmap_attempt(t, handle, address, bytes, replace);
   }
   if (t && result == 4)
      t->lost = true;
   return result;
}

uint32_t
cubit_cpu_tracker_unmap(struct cubit_cpu_mapping_tracker *t, uint32_t handle,
   uint64_t address, uint64_t bytes, bool replace)
{
   return cubit_cpu_tracker_unmap_wait(t, handle, address, bytes, replace, 0, NULL);
}

void
cubit_cpu_tracker_poll(struct cubit_cpu_mapping_tracker *t)
{
   if (!t)
      return;
   for (uint32_t i = 0; i < t->used; i++) {
      if (cubit_cpu_tracker_records(t)[i].state == CUBIT_MAP_RETIRING &&
          cubit_cpu_mapping_release(&cubit_cpu_tracker_records(t)[i], false) == 5)
         t->lost = true;
   }
}

bool
cubit_cpu_tracker_drain(struct cubit_cpu_mapping_tracker *t)
{
   if (!t)
      return false;
   t->lost = true;
   const uint32_t count = t->used - t->drain_cursor;
   const uint32_t limit = t->drain_cursor +
      (count < CUBIT_CPU_DRAIN_QUANTUM ? count : CUBIT_CPU_DRAIN_QUANTUM);
   for (uint32_t i = t->drain_cursor; i < limit; i++) {
      if (cubit_cpu_mapping_release(&cubit_cpu_tracker_records(t)[i], false) != 0)
         t->drain_incomplete = true;
   }
   t->drain_cursor = limit;
   if (limit < t->used)
      return false;
   const bool done = !t->drain_incomplete;
   t->drain_cursor = 0;
   t->drain_incomplete = false;
   if (done && t->grown) {
      free(t->grown);
      t->grown = NULL;
      t->capacity = t->used = 0;
      memset(t->records, 0, sizeof(t->records));
   }
   return done;
}
