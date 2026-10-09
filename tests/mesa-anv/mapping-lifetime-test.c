#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../userspace/mesa/anv/native_gpu_memory.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef CUBIT_TEST_ALLOC_FAILURE
static bool fail_metadata;
void *__real_calloc(size_t count, size_t bytes);
void *__wrap_calloc(size_t count, size_t bytes)
{
   return fail_metadata ? NULL : __real_calloc(count, bytes);
}
#endif

static uint32_t map_result, acquire_result, return_result, retire_result;
static unsigned maps, acquires, returns, retires;
static unsigned waits, finish_after;
static uint32_t finish_status;
static bool drain_sweep(struct cubit_cpu_mapping_tracker *t)
{
   const uint32_t steps = (t->used + CUBIT_CPU_DRAIN_QUANTUM - 1) /
      CUBIT_CPU_DRAIN_QUANTUM;
   for (uint32_t step = 0; step < (steps ? steps : 1); step++) {
      const unsigned before_returns = returns, before_retires = retires;
      const bool done = cubit_cpu_tracker_drain(t);
      assert(returns - before_returns <= CUBIT_CPU_DRAIN_QUANTUM);
      assert(retires - before_retires <= CUBIT_CPU_DRAIN_QUANTUM);
      if (done) return true;
   }
   return false;
}
static void wait_retirement(void)
{
   waits++;
   if (waits == finish_after)
      retire_result = finish_status;
}
uint32_t cubit_intel_map_buffer(uint64_t slot, uint32_t handle,
   uint64_t offset, uint64_t bytes, uint32_t writable,
   uint32_t *mapping, uint64_t *reference)
{
   assert(slot == 63 && handle == 1 && offset == 4096 && bytes == 8192 && writable == 1);
   maps++;
   *mapping = map_result ? 0 : 1;
   *reference = map_result ? 0 : UINT64_C(0x700000008);
   return map_result;
}
uint32_t cubit_intel_acquire_view(uint64_t slot, uint64_t reference,
   uint64_t offset, uint64_t bytes, uint64_t writable, uint64_t *output)
{
   assert(slot == 63 && reference == UINT64_C(0x700000008));
   assert(offset == 0 && bytes == 8192 && writable == 1);
   acquires++;
   *output = acquire_result ? 0 : 0x100000;
   return acquire_result;
}
uint32_t cubit_intel_return_view(uint64_t reference)
{
   assert(reference == UINT64_C(0x700000008));
   returns++;
   return return_result;
}
uint32_t cubit_intel_retire_mapping(uint64_t slot, uint32_t mapping)
{
   assert(slot == 63 && mapping == 1);
   retires++;
   return retire_result;
}
int main(void)
{
   for (unsigned pending_first = 0; pending_first < 2; pending_first++) {
      struct cubit_cpu_mapping_tracker t = {.slot = 63};
      uint64_t address;
      maps = acquires = returns = retires = 0;
      map_result = acquire_result = return_result = retire_result = 0;
      const unsigned count = 2 * CUBIT_CPU_DRAIN_QUANTUM + 1;
      for (unsigned i = 0; i < count; i++)
         assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 0);
      retire_result = pending_first ? 4 : 0;
      assert(!cubit_cpu_tracker_drain(&t));
      assert(t.lost && t.grown && t.used == count);
      assert(t.drain_cursor == CUBIT_CPU_DRAIN_QUANTUM);
      assert(returns == CUBIT_CPU_DRAIN_QUANTUM && retires == returns);
      assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 5);
      assert(!address && maps == count);
      retire_result = 0;
      assert(!cubit_cpu_tracker_drain(&t));
      assert(t.grown && t.drain_cursor == 2 * CUBIT_CPU_DRAIN_QUANTUM);
      assert(cubit_cpu_tracker_drain(&t) == !pending_first);
      assert(returns == count && retires == count);
      if (pending_first) {
         assert(t.grown && t.drain_cursor == 0);
         assert(drain_sweep(&t));
         assert(returns == count && retires == count + CUBIT_CPU_DRAIN_QUANTUM);
      }
      assert(!t.grown && !t.used && !t.drain_cursor && t.lost);
      assert(cubit_cpu_tracker_drain(&t));
      assert(returns == count);
   }
   puts("Bounded drain PASS:64-record steps, pending sweep, no early free, borrow once");
   /* Boundaries: immediate, first/last permitted poll, exhaustion, error,
    * failed borrow return, and a previously lost tracker never revived. */
   for (unsigned scenario = 0; scenario < 7; scenario++) {
      struct cubit_cpu_mapping_tracker t = {.slot = 63};
      uint64_t address;
      maps = acquires = returns = retires = waits = 0;
      map_result = acquire_result = return_result = retire_result = 0;
      assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 0);
      retire_result = scenario == 0 ? 0 : 4;
      finish_after = scenario == 2 ? 100 : scenario == 3 ? 101 : 1;
      finish_status = scenario == 4 ? 5 : 0;
      if (scenario == 5) return_result = 5;
      if (scenario == 6) t.lost = true;
      const uint32_t result = cubit_cpu_tracker_unmap_wait
         (&t, 1, address, 8192, false, 100, wait_retirement);
      const bool failed = scenario >= 3 && scenario <= 5;
      assert(result == (scenario == 3 ? 4u : failed ? 5u : 0u));
      assert(t.lost == (failed || scenario == 6));
      assert(returns == 1);
      assert(waits == (scenario == 0 || scenario == 5 ? 0u :
                       scenario == 2 || scenario == 3 ? 100u : 1u));
      assert(retires == (scenario == 5 ? 0u : waits + 1));
      assert(t.records[0].state == (scenario == 3 ? CUBIT_MAP_RETIRING :
             failed ? CUBIT_MAP_FAILED : CUBIT_MAP_RETIRED));
   }
   puts("Bounded unmap PASS: pending completion, budget, errors, sticky loss, return once");
   for (unsigned failure = 0; failure < 5; failure++) {
      struct cubit_cpu_mapping r = {0};
      maps = acquires = returns = retires = 0;
      map_result = failure == 1 ? 5 : 0;
      acquire_result = failure == 2 ? 1 : 0;
      return_result = failure == 3 ? 1 : 0;
      retire_result = failure == 4 ? 5 : 4;
      uint32_t result = cubit_cpu_mapping_open(&r, 63, 1, 4096, 8192, 1);
      assert(maps == 1);
      assert(r.bo_handle == 1 && r.bo_offset == 4096 && r.bytes == 8192);
      assert(cubit_cpu_mapping_open(&r, 63, 1, 4096, 8192, 1) == 5 && maps == 1);
      if (failure == 1) {
         assert(result == 5 && acquires == 0 && retires == 0);
         assert(r.state == CUBIT_MAP_FAILED);
         assert(r.lookup_address == 0);
         continue;
      }
      if (failure == 2) {
         assert(result == 5 && r.state == CUBIT_MAP_RETIRING && returns == 0 && retires == 1);
         assert(r.lookup_address == 0);
      } else {
         assert(result == 0 && r.address == 0x100000 && r.state == CUBIT_MAP_LIVE);
         assert(r.lookup_address == 0x100000);
         assert(cubit_cpu_mapping_release(&r, true) == 5);
         assert(r.state == CUBIT_MAP_LIVE && returns == 0 && retires == 0);
      }
      result = cubit_cpu_mapping_release(&r, false);
      if (failure == 3 || failure == 4) {
         assert(result == 5 && r.state == CUBIT_MAP_FAILED && returns == 1);
         unsigned before = returns + retires;
         assert(cubit_cpu_mapping_release(&r, false) == 5 && returns + retires == before);
      } else {
         assert(result == 4 && r.state == CUBIT_MAP_RETIRING && r.address == 0);
         assert(r.lookup_address == (failure == 2 ? 0u : 0x100000u));
         assert(returns == (failure == 2 ? 0u : 1u));
         retire_result = 0;
         assert(cubit_cpu_mapping_release(&r, false) == 0 && r.state == CUBIT_MAP_RETIRED);
         unsigned before = returns + retires;
         assert(cubit_cpu_mapping_release(&r, false) == 0 && returns + retires == before);
      }
   }
   for (unsigned pending = 0; pending < 2; pending++) {
      struct cubit_cpu_mapping_tracker t = {.slot = 63};
      uint64_t address;
      maps = acquires = returns = retires = 0;
      map_result = acquire_result = return_result = 0;
      retire_result = pending ? 4 : 0;
      assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 0);
      assert(t.used == 1 && !t.lost && address == 0x100000);
      assert(cubit_cpu_tracker_unmap(&t, 1, address, 8192, true) == 5 && !t.lost);
      assert(cubit_cpu_tracker_unmap(&t, 1, address, 8192, false) == (pending ? 4u : 0u));
      assert(t.lost == !!pending && returns == 1);
      if (pending) {
         unsigned before = maps;
         assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 5);
         assert(address == 0 && maps == before);
         retire_result = 0;
         cubit_cpu_tracker_poll(&t);
         assert(t.lost && t.records[0].state == CUBIT_MAP_RETIRED && returns == 1);
      } else {
         /* Reused CPU VA must resolve the new record, not the retired one. */
         assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 0);
         assert(cubit_cpu_tracker_unmap(&t, 1, address, 8192, false) == 0 && returns == 2);
      }
      assert(cubit_cpu_tracker_drain(&t) && t.lost);
   }
   {
      /* Distinct grants may expose the same address and BO range. Retiring
       * one must not hide the other live borrow behind a tombstone. */
      struct cubit_cpu_mapping_tracker t = {.slot = 63};
      uint64_t first, second;
      maps = acquires = returns = retires = 0;
      map_result = acquire_result = return_result = retire_result = 0;
      assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &first) == 0);
      assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &second) == 0);
      assert(first == second);
      assert(cubit_cpu_tracker_unmap(&t, 1, first, 8192, false) == 0);
      assert(returns == 1 && retires == 1);
      assert(cubit_cpu_tracker_unmap(&t, 1, second, 8192, false) == 0);
      assert(returns == 2 && retires == 2);
      assert(t.records[0].state == CUBIT_MAP_RETIRED);
      assert(t.records[1].state == CUBIT_MAP_RETIRED);
      assert(cubit_cpu_tracker_unmap(&t, 1, second, 8192, false) == 0);
      assert(returns == 2 && retires == 2);
   }
   for (unsigned failure = 0; failure < 2; failure++) {
      struct cubit_cpu_mapping_tracker t = {.slot = 63};
      uint64_t address;
      maps = acquires = returns = retires = 0;
      map_result = acquire_result = return_result = retire_result = 0;
      assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 0);
      if (failure)
         return_result = 5;
      else
         retire_result = 4;
      assert(!cubit_cpu_tracker_drain(&t) && t.lost && returns == 1);
      const unsigned retired_before = retires;
      return_result = retire_result = 0;
      if (failure) {
         /* Uncertain borrow return is never repeated or declared retired. */
         assert(!cubit_cpu_tracker_drain(&t));
         assert(retires == retired_before && returns == 1);
      } else {
         assert(cubit_cpu_tracker_drain(&t));
         assert(retires == retired_before + 1 && returns == 1);
         assert(cubit_cpu_tracker_drain(&t));
         assert(retires == retired_before + 1 && returns == 1);
      }
      assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 5);
   }
   {
      struct cubit_cpu_mapping_tracker t = {.slot = 63};
      uint64_t address;
      maps = acquires = returns = retires = 0;
      map_result = acquire_result = return_result = retire_result = 0;
      /* Repeated successful map/unmap must not exhaust a device lifetime. */
      for (unsigned i = 0; i < 4096; i++) {
         assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 0);
         assert(cubit_cpu_tracker_unmap(&t, 1, address, 8192, false) == 0);
         assert(!t.lost && t.used <= CUBIT_CPU_MAPPING_CAPACITY);
      }
      assert(maps == 4096 && returns == maps && retires == maps);
      assert(cubit_cpu_tracker_drain(&t));
   }
   {
      struct cubit_cpu_mapping_tracker t = {.slot = 63};
      uint64_t address;
      maps = acquires = returns = retires = 0;
      /* Alternate retired and live records, all sharing the same CPU VA. */
      for (unsigned i = 0; i < CUBIT_CPU_MAPPING_CAPACITY; i++) {
         assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 0);
         t.records[i].bo_offset = i; /* Distinguish retained ordering. */
         if (!(i & 1))
            assert(cubit_cpu_tracker_unmap(&t, 1, address, 8192, false) == 0);
      }
      assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 0);
      assert(t.used == CUBIT_CPU_MAPPING_CAPACITY / 2 + 1);
      for (unsigned i = 0; i < t.used; i++) {
         assert(t.records[i].state == CUBIT_MAP_LIVE);
         if (i < CUBIT_CPU_MAPPING_CAPACITY / 2)
            assert(t.records[i].bo_offset == 2 * i + 1);
      }
      assert(cubit_cpu_tracker_drain(&t));
      assert(returns == maps && retires == maps);
   }
   {
      struct cubit_cpu_mapping_tracker t = {.slot = 63};
      uint64_t address;
      maps = acquires = returns = retires = 0;
      for (unsigned i = 0; i < 4096; i++) {
         assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 0);
         cubit_cpu_tracker_records(&t)[i].bo_offset = i;
      }
      assert(t.used == 4096 && t.capacity == 4096 && !t.lost);
      for (unsigned i = 0; i < t.used; i++)
         assert(cubit_cpu_tracker_records(&t)[i].bo_offset == i);
      retire_result = 4;
      assert(!drain_sweep(&t));
      assert(t.grown && t.used == 4096 && returns == 4096);
      retire_result = 0;
      assert(drain_sweep(&t));
      assert(!t.grown && t.used == 0 && t.capacity == 0);
      assert(returns == 4096 && retires == 8192);
      /* Even a full array of retired records cannot revive a lost tracker. */
      assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 5);
      assert(address == 0 && maps == returns);
   }
#ifdef CUBIT_TEST_ALLOC_FAILURE
   {
      struct cubit_cpu_mapping_tracker t = {.slot = 63};
      uint64_t address;
      maps = acquires = returns = retires = 0;
      map_result = acquire_result = return_result = retire_result = 0;
      for (unsigned capacity = 64; capacity <= 1024; capacity *= 2) {
         while (t.used < capacity)
            assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 0);
         struct cubit_cpu_mapping *saved = cubit_cpu_tracker_records(&t);
         fail_metadata = true;
         assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 5);
         assert(address == 0 && !t.lost && t.used == capacity);
         assert(cubit_cpu_tracker_records(&t) == saved);
         assert(maps == capacity && acquires == capacity && !returns && !retires);
         for (unsigned i = 0; i < capacity; i++)
            assert(saved[i].state == CUBIT_MAP_LIVE && saved[i].address == 0x100000);
         fail_metadata = false;
         assert(cubit_cpu_tracker_map(&t, 1, 4096, 8192, 1, &address) == 0);
         assert(t.capacity == capacity * 2 && t.used == capacity + 1);
      }
      assert(drain_sweep(&t));
      assert(!t.grown && maps == returns && returns == retires);
   }
   puts("Mapping growth OOM PASS: five boundaries, no IPC on failure, existing borrows retained");
#endif
   puts("Mesa mapping lifetime PASS: 4096 live mappings, borrow-once, pending retirement, retired-only reclamation");
}
