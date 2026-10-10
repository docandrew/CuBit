#pragma once
#include "anv_private.h"
#include "vk_sync.h"
/* Callback implementations only. The native physical-device factory must
 * supply persistent tracker state and a complete backend before installing. */
/* Called after vk_device initialization and before any BO creation. The
 * trusted factory supplies a stable, authorized endpoint slot. Tracker access
 * is internally serialized; callers still obey Vulkan object lifetime rules.
 * No capability is acquired here. */
VkResult anv_cubit_memory_init(struct anv_device *device, uint64_t slot);
/* Device-open transaction for a fresh session supplied by trusted bootstrap.
 * Does NOT acquire/delegate authority. On successful tracker attachment,
 * owns cleanup even if live health/memory-contract verification fails; caller must retain
 * the slot while memory_slot_retained is true. Attach failure must not close
 * a slot that may belong to another live device. Coherent BOs require policy2
 * from this same endpoint; policy1 requires physical memory.need_flush.
 * Use attach_owned_session for provider-managed pins; this entry point is
 * for externally process-pinned endpoints. Native broker still unwired. */
VkResult anv_cubit_attach_session(struct anv_device *device, uint64_t slot);
/* Provider-owned endpoint pin. On transfer, this descriptor is cleared even
 * if subsequent health verification fails. Otherwise it is unchanged and
 * remains caller-owned. The copied context must outlive deferred cleanup.
 * retired runs exactly once ONLY after confirmed retirement, under the
 * transport lock: it must only publish an atomic notification, never block,
 * acquire locks, call Mesa, or mutate capabilities. The provider processes
 * that notification separately before releasing/replacing its endpoint.
 * Uncertain retirement deliberately keeps the pin indefinitely. */
struct anv_cubit_endpoint_pin {
   void *context;
   void (*retired)(void *context);
};
VkResult anv_cubit_attach_owned_session(struct anv_device *device, uint64_t slot,
                                      struct anv_cubit_endpoint_pin *pin);
/* Logical one-render-queue context claim after transport attachment. Native
 * prepare/register stays deferred until the first internal batch, after BO
 * binding. Unsupported queue requests fail without acquiring resources. */
VkResult anv_cubit_setup_context(struct anv_device *device,
   const VkDeviceCreateInfo *create_info, uint32_t num_queues);
/* Common ANV calls this after queue/BO teardown. Incomplete retirement is
 * retained by process cleanup, never converted into backing reclamation. */
bool anv_cubit_destroy_context(struct anv_device *device);
/* Common failure/normal close hook, including failure before context setup.
 * Idempotent after destroy_context; process polling owns deferred cleanup. */
void anv_cubit_close_device(struct anv_device *device);
/* Bind exactly one Mesa render queue to the context's existing service
 * engine. No extra engine, protected queue or scheduling priority authority.
 * Release is logical; native context retirement remains device-owned. */
VkResult anv_cubit_create_engine(struct anv_device *device, struct anv_queue *queue,
   const VkDeviceQueueCreateInfo *create_info);
void anv_cubit_destroy_engine(struct anv_device *device, struct anv_queue *queue);
/* Read-only current session health; any failed/invalid reply is sticky device
 * loss. Success is an observation, not a submission ownership lease. */
VkResult anv_cubit_check_status(struct vk_device *device);
/* Exact physical null-heap setup/teardown only, backed by native per-VM
 * scratch. Rejects sparse work, waits/signals, BO mappings and replay. Setup
 * precedes first context preparation; teardown forbids subsequent GPU work
 * but retains all backing until normal retirement. No Xe bind timeline.
 * Not installed in the factory; coherent backing requires session policy2. */
VkResult anv_cubit_vm_bind(struct anv_device *device,
   struct anv_sparse_submission *submit, enum anv_vm_bind_flags flags);
/* Startup/internal batch callback: caller holds no ANV device mutex, retains
 * all chained BOs and serializes queue access. First call seals offline
 * binds, prepares/registers the context and opens the session queue exactly
 * once; later BO binds use live VM updates. Writes one descriptor and
 * returns: caller outputs, the private submit fence and the debug queue
 * fence get GPU timeline points. No companion engine yet. */
VkResult anv_cubit_queue_exec_async(struct anv_async_submit *submit,
   uint32_t wait_count, const struct vk_sync_wait *waits,
   uint32_t signal_count, const struct vk_sync_signal *signals);
/* After BO teardown and caller quiescence. Transfers pending CPU/session
 * cleanup into process-owned storage before clearing the device pointer.
 * No Vulkan allocator callbacks survive this call. Endpoint capability slots
 * must remain stable until process cleanup completes. No backing reclamation. */
VkResult anv_cubit_memory_finish(struct anv_device *device);
/* Poll detached lifetimes without any anv_device pointer. First drain CPU
 * views, then close admission exactly once and poll GPU/grant quiescence.
 * Uncertain close/query outcomes retain the endpoint without replay.
 * Returns count
 * still pending/quarantined. A service loop must drive this before enabling
 * the backend. Only detached, confirmed-complete tracker slots are reused. */
uint32_t anv_cubit_memory_poll(void);
/* CPU/session-cleanup retention. True forbids slot replacement/reuse; false
 * is NOT an atomic permission to mutate caps or reclaim GPU backing.
 * The trusted endpoint owner must serialize capability changes with attach.
 * Invalid slot values conservatively return true. */
bool anv_cubit_memory_slot_retained(uint64_t slot);
/* After all offline BO binds: one-shot prepare/register. This is NOT the full
 * ANV setup_context callback: live VM changes/queue sync still need integration.
 * Failure poisons the retained session; no automatic retry or resource release. */
VkResult anv_cubit_prepare_submission(struct anv_device *device);
/* Bind the full real BO before preparation. GPU must be raw48/page-aligned;
 * caller owns VA allocation. No canonical-address guessing, slab adjustment,
 * live rebind or silent no-op. Does not assign bo->offset for the allocator. */
VkResult anv_cubit_bind_bo_offline(struct anv_device *device, struct anv_bo *bo,
                                 uint64_t gpu);
/* Bind a real BO at its canonical Mesa offset. Atomically selects offline
 * binding before preparation or committed native VM update after registration.
 * No VA allocation, bind-timeline signaling, or KMD factory installation. */
VkResult anv_cubit_bind_bo(struct anv_device *device, struct anv_bo *bo);
/* Full real-BO unbind: removes an unpublished binding before preparation,
 * or completes a generation-checked live update after registration. No name
 * close, backing release or Vulkan signal is implied. */
VkResult anv_cubit_unbind_bo(struct anv_device *device, struct anv_bo *bo);
/* Explicit post-registration bind/unbind adapter for native 0A28.
 * Synchronous: the driver applies it once the GPU is idle. Holds only the
 * session's VM lock, never lifetime_mutex or the queue lock, so queued work
 * and its completion proceed meanwhile. Caller owns VA allocation and
 * retains BOs; success is a VM-generation commit, not permission to destroy
 * backing or a Vulkan synchronization event. Uncertain replies poison the
 * session without replay. Does not change offset. */
VkResult anv_cubit_update_bo_binding(struct anv_device *device, struct anv_bo *bo,
                                     uint64_t gpu, uint64_t offset, uint64_t bytes,
                                     bool remove);
/* Before device->mutex: waits until every dependency is pending (reached, or
 * a point the session queue will signal, which becomes a descriptor wait).
 * Does not submit, signal, or consume binary waits. Success is not an
 * ownership lease; the submission revalidates the session. Caller retains
 * device/sync objects and serializes the queue. */
VkResult anv_cubit_wait_dependencies(struct anv_device *device,
   uint32_t wait_count, const struct vk_sync_wait *waits,
   uint64_t abs_timeout_ns);
/* Render-only KMD callback on the session queue: translates waits, writes
 * one descriptor and records GPU timeline points on the signals, then
 * returns; it makes no blocking call. Dependencies are pending already (the
 * pre-lock hook). Optional perf/companion/trace submissions remain
 * unsupported, not silently skipped. */
VkResult anv_cubit_queue_exec_locked(struct anv_queue *queue,
   uint32_t wait_count, const struct vk_sync_wait *waits,
   uint32_t cmd_buffer_count, struct anv_cmd_buffer **cmd_buffers,
   uint32_t signal_count, const struct vk_sync_signal *signals,
   struct anv_query_pool *perf_query_pool, uint32_t perf_query_pass,
   struct anv_utrace_submit *utrace_submit);
/* Owned WB CPU grants require HOST_CACHED without HOST_COHERENT, with
 * physical memory.need_flush enabled. Flags0/WC is not backed by this service.
 * This admits explicit cache maintenance, not hardware coherence guarantees. */
uint32_t anv_cubit_gem_create(struct anv_device *device,
                             const struct intel_memory_class_instance **regions,
                             uint16_t num_regions, uint64_t size,
                             enum anv_bo_alloc_flags flags, uint64_t *actual_size);
void anv_cubit_gem_close(struct anv_device *device, struct anv_bo *bo);
void *anv_cubit_gem_mmap(struct anv_device *device, struct anv_bo *bo,
                        uint64_t offset, uint64_t size, void *placed_addr);
VkResult anv_cubit_unmap_bo(struct anv_device *device, struct anv_bo *bo,
                            void *map, size_t size, bool replace);
/* No Linux exec-object flags in the CuBit submission protocol. Allocation
 * flags are validated by gem_create and retained by common ANV separately. */
uint32_t anv_cubit_bo_flags(struct anv_device *device, enum anv_bo_alloc_flags flags);
/* Common-constructor hook: explicit native GPU VA binding, no relocations or
 * sparse mechanisms. Does not establish VM isolation or reserve addresses. */
void anv_cubit_init_addressing(struct anv_physical_device *device);

/* Transport portion of the backend, copied by the future physical factory
 * before it fills platform/open-device hooks. NOT a usable full
 * backend: do not publish this directly as physical->kmd_backend. Unsupported
 * external-memory, userptr and placed-map callbacks remain NULL. */
extern const struct anv_kmd_backend anv_cubit_transport_backend;
