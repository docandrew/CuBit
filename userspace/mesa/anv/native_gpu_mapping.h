#pragma once
#include <stdbool.h>
#include <stdint.h>

/* Mesa-side FFI bookkeeping only; no device policy or kernel authority.
 * Zero-initialize once, serialize access, never copy/reset a used standalone
 * record. The tracker may relocate its exclusively owned records under lock;
 * callers must not retain pointers into the tracker across operations.
 * Keep the endpoint slot stable for the entire lifetime. Retain failed records
 * until session teardown: an uncertain transport outcome is not reclamation.
 */
enum cubit_cpu_map_state {
   CUBIT_MAP_EMPTY, CUBIT_MAP_LIVE, CUBIT_MAP_RETIRING,
   CUBIT_MAP_RETIRED, CUBIT_MAP_FAILED
};
struct cubit_cpu_mapping {
   enum cubit_cpu_map_state state;
   uint64_t slot, reference, address, bytes;
   /* Stable lookup metadata, NOT permission to access retired memory. Retain
    * outside the ANV BO: internal teardown may discard the BO after unmap. */
   uint64_t lookup_address, bo_offset;
   uint32_t mapping, bo_handle;
};
/* Accepts the page-rounded range prepared for the actual BO allocation.
 * Success0 exposes address. Failure5 may leave cleanup pending in the record;
 * call release while RETIRING, never retry open. No GPU/coherence promise.
 */
uint32_t cubit_cpu_mapping_open(struct cubit_cpu_mapping *record,
   uint64_t slot, uint32_t handle, uint64_t offset, uint64_t grant_bytes,
   uint32_t writable);
/* Success0 only after grant retirement; 4=pending, 5=failure. A successful
 * borrow return occurs once even across multiple polls. Unsupported placed
 * replacement returns5 without touching an existing mapping. */
uint32_t cubit_cpu_mapping_release(struct cubit_cpu_mapping *record, bool replace);

/* Initial retained-entry tracker: device-owned, never a pointer into an ANV BO.
 * Zero-initialize once and serialize externally. Caller keeps slot immutable.
 * lost is sticky: the ANV adapter must report device loss and stop allocation/
 * submission when it becomes true, including on pending internal unmap. Poll
 * can finish cleanup but does not make that device usable again.
 * At capacity, confirmed-retired tombstones are discarded; outstanding records
 * retain their relative order. This reclaims bookkeeping only, not GPU memory
 * or endpoint authority. Duplicate unmap is idempotent only while its tombstone
 * remains; callers must never unmap a stale pointer after another map operation.
 */
#define CUBIT_CPU_MAPPING_CAPACITY 64
struct cubit_cpu_mapping_tracker {
   uint64_t slot;
   uint32_t used;
   bool lost;
   struct cubit_cpu_mapping records[CUBIT_CPU_MAPPING_CAPACITY];
};
uint32_t cubit_cpu_tracker_map(struct cubit_cpu_mapping_tracker *tracker,
   uint32_t handle, uint64_t offset, uint64_t grant_bytes, uint32_t writable,
   uint64_t *address);
uint32_t cubit_cpu_tracker_unmap(struct cubit_cpu_mapping_tracker *tracker,
   uint32_t handle, uint64_t address, uint64_t grant_bytes, bool replace);
/* Synchronous adapter variant: up to polls additional retirement observations,
 * yielding through wait_pending between them. Holds caller serialization; the
 * callback must not reenter/mutate this tracker. Pending is not loss until the
 * budget expires; errors and exhausted waits retain sticky loss. Never returns
 * the borrow twice. Does not clear preexisting loss or reclaim GPU backing. */
uint32_t cubit_cpu_tracker_unmap_wait(struct cubit_cpu_mapping_tracker *tracker,
   uint32_t handle, uint64_t address, uint64_t grant_bytes, bool replace,
   uint32_t polls, void (*wait_pending)(void));
void cubit_cpu_tracker_poll(struct cubit_cpu_mapping_tracker *tracker);
/* Stops new work and drains known records. False means retain the tracker;
 * it does not authorize releasing BO backing or GPU VM bindings either way. */
bool cubit_cpu_tracker_drain(struct cubit_cpu_mapping_tracker *tracker);
