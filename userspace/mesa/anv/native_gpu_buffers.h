#pragma once
#include <stdint.h>
/* Same pinned endpoint as allocation:0 unavailable/invalid,1 owned WB with
 * explicit maintenance,2 owned coherent WB. No aperture/import guarantee. */
uint32_t cubit_intel_memory_contract(uint64_t slot);
/* Active-session observation:0 ready,1 denied,2 malformed,3 unavailable,
 * 4 invalid transport. Read-only; success is not an ownership lease. */
uint32_t cubit_intel_session_status(uint64_t slot);
/* Poll the SAME stable capability after close: 0 quiescent,1 denied,2 bad
 * request,3 unavailable/uncertain,4 pending,5 invalid transport/reply.
 * Read-only; never repeat close to poll. No backing/ID/cap-slot reuse implied. */
uint32_t cubit_intel_poll_session_retirement(uint64_t slot);
/* One-shot admission close of this capability's own session. Output clears
 * on failure; nonzero returned tag is opaque identity, not authority.
 * NOT GPU disable/grant drain/backing release or permission to reuse slot.
 * Same statuses as create below. Never replay an uncertain close. */
uint32_t cubit_intel_close_session(uint64_t slot, uint64_t *retired_tag);
/* Slot is an authorized, stable render-session endpoint. No PID/address input.
 * 0 success; 1 denied; 2 bad request; 3 unavailable; 4 invalid transport/reply.
 * Create clears *handle on failure. An uncertain create must not be retried
 * blindly: server-side backing can remain retained. Session retirement
 * invalidates names; it does not yet reclaim backing in the initial driver.
 * These calls do not advertise a usable Vulkan device by themselves. */
uint32_t cubit_intel_create_buffer(uint64_t slot, uint64_t bytes, uint32_t *handle);
/* Retire a BO name, NOT GPU completion, unmapping or backing reclamation. */
uint32_t cubit_intel_close_buffer(uint64_t slot, uint32_t handle);
/* One-shot context VM seal/materialize/publication after offline binds.
 * No registration, execution or fence implied; same status codes as create.
 * Retire the session on uncertain replies; never replay preparation. */
uint32_t cubit_intel_prepare_context(uint64_t slot);
/* One-shot registration and driver-owned setup execution after preparation.
 * Success means setup completed and scheduling disable acknowledged;
 * no application batch is executed and this does not authorize submission.
 * Same codes as create. Retire on uncertainty; never automatically retry. */
uint32_t cubit_intel_register_context(uint64_t slot);
/* Synchronous batch submission, not admission or a Vulkan queue implementation.
 * previous=1 after setup; caller serializes and retains ALL reachable backing.
 * Success confirms exact next marker plus scheduling-disable acknowledgement,
 * not Desktop reader release. *completion clears on failure. Same codes as
 * create; retire on uncertain reply, NEVER replay. Driver admission is closed. */
uint32_t cubit_intel_submit_batch(uint64_t slot, uint32_t handle,
   uint64_t gpu_address, uint64_t bo_byte_offset, uint64_t bytes,
   uint32_t previous, uint32_t *completion);
/* Bind a whole-page BO slice into the session's unsealed GPU VM. No live
 * remap or execution. Same codes as create; status4 may mean binding occurred,
 * so retire the session instead of blindly retrying or recycling backing. */
uint32_t cubit_intel_bind_buffer(uint64_t slot, uint32_t handle,
   uint64_t gpu_address, uint64_t bo_offset, uint64_t bytes);
/* Remove an exact owned slice from an unpublished VM. Sealed VMs reject this;
 * use update_binding for live changes. No backing release, same status codes. */
uint32_t cubit_intel_unbind_buffer(uint64_t slot, uint32_t handle,
   uint64_t gpu_address, uint64_t bo_offset, uint64_t bytes);
/* Pending live-update protocol; NOT enabled by the native driver yet.
 * remove=0 bind,1 unbind; previous=0 for initial VM. Caller serializes the
 * session and retains both generations. Success checks exact successor after
 * hardware completion, not just preparation; *generation clears on failure.
 * Same codes as create. Retire on uncertain replies, never replay. */
uint32_t cubit_intel_update_binding(uint64_t slot, uint32_t handle,
   uint64_t gpu_address, uint64_t bo_offset, uint64_t bytes,
   uint32_t remove, uint32_t previous, uint32_t *generation);
/* Map requests a grant only; acquire a CPU mapping separately. Outputs clear
 * on failure. Writable must be 0/1; ranges are whole 4KiB pages. Never blindly
 * retry an uncertain map. Mapping calls use 5 for local/protocol failure;
 * retire additionally returns 4 while grant revocation is pending.
 * Return the CPU acquisition before retiring; neither is a GPU fence. */
uint32_t cubit_intel_map_buffer(uint64_t slot, uint32_t handle,
   uint64_t offset, uint64_t bytes, uint32_t writable,
   uint32_t *mapping, uint64_t *reference);
uint32_t cubit_intel_retire_mapping(uint64_t slot, uint32_t mapping);
/* Explicit read-only forwarding grant for presentation. Acquire then derive
 * to Desktop. Neither publication nor GPU completion is implied. Same mapping
 * statuses and no-retry rule. Ordinary map_buffer never enables forwarding. */
uint32_t cubit_intel_map_presentation(uint64_t slot, uint32_t handle,
   uint64_t offset, uint64_t bytes, uint32_t *mapping, uint64_t *reference);
