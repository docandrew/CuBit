/* Mesa ANV callback adapter. IPC, grants and device policy remain in Ada. */
#include "anv_cubit_memory.h"
#include "native_gpu_mapping.h"
#include "native_gpu_buffers.h"
#include <pthread.h>
#include <unistd.h>
#include <limits.h>

/* Optional diagnostic fixture sink. Called under lifetime_mutex: it MUST
 * only capture scalar evidence, never perform IPC or reenter Mesa. */
extern void cubit_test_mesa_transport_failure(const char *operation,
   uint32_t status, uint32_t handle) __attribute__((weak));
static void transport_failure(const char *operation, uint32_t status,
                              uint32_t handle)
{
   if (cubit_test_mesa_transport_failure)
      cubit_test_mesa_transport_failure(operation, status, handle);
}

void
anv_cubit_init_addressing(struct anv_physical_device *device)
{
   /* Native VM bindings carry explicit 48-bit GPU addresses. There is no
    * Linux execbuf relocation list, TR-TT implementation, or public sparse
    * binding protocol. Debug/DRIRC options must not manufacture one. This
    * selects mechanisms only: factory admission still establishes isolation
    * and the usable VA span before common ANV initializes its VMA heaps. */
   device->uses_relocs = false;
   device->sparse_type = ANV_SPARSE_TYPE_NOT_SUPPORTED;
}

/* Keep the binding/submission paths together: in particular a queue callback
 * must not be selected without its pre-device-mutex dependency hook. The
 * factory still has to provide platform, ownership and lifecycle operations;
 * this table does not create a device or acquire any capabilities. */
const struct anv_kmd_backend anv_cubit_transport_backend = {
   .init_addressing = anv_cubit_init_addressing,
   .abort_device = anv_cubit_close_device,
   .close_device = anv_cubit_close_device,
   .setup_context = anv_cubit_setup_context,
   .destroy_context = anv_cubit_destroy_context,
   .create_engine = anv_cubit_create_engine,
   .destroy_engine = anv_cubit_destroy_engine,
   .check_status = anv_cubit_check_status,
   .gem_create = anv_cubit_gem_create,
   .gem_close = anv_cubit_gem_close,
   .gem_mmap = anv_cubit_gem_mmap,
   .unmap_bo = anv_cubit_unmap_bo,
   .vm_bind = anv_cubit_vm_bind,
   .vm_bind_bo = anv_cubit_bind_bo,
   .vm_unbind_bo = anv_cubit_unbind_bo,
   .wait_queue_dependencies = anv_cubit_wait_dependencies,
   .queue_exec_locked = anv_cubit_queue_exec_locked,
   .queue_exec_async = anv_cubit_queue_exec_async,
   .bo_alloc_flags_to_bo_flags = anv_cubit_bo_flags,
};

uint32_t
anv_cubit_bo_flags(struct anv_device *device, enum anv_bo_alloc_flags flags)
{
   /* Common ANV calls this unconditionally when constructing a BO. Like the
    * Xe backend, there is no execbuf object list/flag word to translate.
    * Returning zero does NOT authorize these allocation flags: gem_create
    * validates their semantics before common ANV constructs the wrapper. */
   (void)device;
   (void)flags;
   return 0;
}

/* Process-owned retained lifetimes. No Vulkan allocator/user-data dependency.
 * Growable stable records: recycle only detached, confirmed-complete records.
 * The current 64-slot capability namespace bounds lookup/poll work, not an
 * arbitrary smaller metadata array. Endpoint slots remain retained on failure.
 * The endpoint capability must remain stable through deferred cleanup. */
#define CUBIT_MEMORY_LIFETIMES UINT_MAX
#define CUBIT_ENDPOINT_SLOTS 64u
static pthread_mutex_t lifetime_mutex = PTHREAD_MUTEX_INITIALIZER;
struct cubit_memory_lifetime {
   struct cubit_cpu_mapping_tracker tracker;
   struct anv_cubit_endpoint_pin pin;
   bool detached, complete;
   bool cpu_drained, close_attempted, retirement_failed;
   bool submission_attempted, submission_ready;
   bool context_claimed;
   bool coherent_memory;
   bool engine_claimed;
   struct anv_queue *engine_queue; /* cleared before Vulkan wrapper teardown */
   bool null_heap_active, null_heap_closed;
   uint64_t null_heap_address, null_heap_size;
   uint32_t completion;
   uint32_t vm_generation;
};
static struct cubit_memory_lifetime **lifetimes;
static unsigned lifetime_count, lifetime_capacity;

/* Called with lifetime_mutex held. Only the pointer directory moves; exported
 * tracker addresses and outstanding cleanup references stay stable. Failure
 * publishes no record and consumes no endpoint pin. */
static bool grow_lifetimes(void)
{
   if (lifetime_count < lifetime_capacity) return true;
   if (lifetime_capacity == CUBIT_ENDPOINT_SLOTS) return false;
   unsigned capacity = lifetime_capacity ? lifetime_capacity * 2 : 4;
   if (capacity > CUBIT_ENDPOINT_SLOTS) capacity = CUBIT_ENDPOINT_SLOTS;
   void *directory = realloc(lifetimes, capacity * sizeof(*lifetimes));
   if (!directory) return false;
   lifetimes = directory;
   lifetime_capacity = capacity;
   return true;
}

static VkResult memory_init(struct anv_device *device, uint64_t slot,
                            struct anv_cubit_endpoint_pin *pin);

/* Protected by lifetime_mutex. The index is process-owned and survives device
 * destruction; no completion state is stored in an application BO wrapper. */
static unsigned
submission_lifetime(struct anv_device *device)
{
   if (vk_device_is_lost_no_report(&device->vk) || !device->cubit_cpu_mappings)
      return CUBIT_MEMORY_LIFETIMES;
   for (unsigned i = 0; i < lifetime_count; i++) {
      if (&(*lifetimes[i]).tracker == device->cubit_cpu_mappings &&
          !(*lifetimes[i]).detached && !(*lifetimes[i]).complete &&
          !(*lifetimes[i]).null_heap_closed &&
          !(*lifetimes[i]).tracker.lost)
         return i;
   }
   return CUBIT_MEMORY_LIFETIMES;
}

VkResult
anv_cubit_vm_bind(struct anv_device *device, struct anv_sparse_submission *submit,
                  enum anv_vm_bind_flags flags)
{
   /* This is the common ANV device null-heap lifecycle, not sparse binding.
    * All native application VMs already have private scratch-backed holes.
    * Initial context preparation materializes them before any GPU submission;
    * real BO updates synchronously restore fallback entries when unbound.
    * No Xe timeline exists here. No real mapping or backing is destroyed by
    * teardown; it closes this adapter's GPU work admission until session close.
    * Factory remains gated pending complete platform/cache/VM validation. */
   if (!device || !device->physical || !submit || submit->queue ||
       !submit->binds || submit->binds_len != 1 || submit->binds_capacity < 1 ||
       submit->wait_count || submit->signal_count || submit->waits || submit->signals ||
       (flags != ANV_VM_BIND_FLAG_NONE && flags != ANV_VM_BIND_FLAG_SIGNAL_BIND_TIMELINE))
      return VK_ERROR_FEATURE_NOT_PRESENT;
   const struct anv_vm_bind *bind = submit->binds;
   const struct anv_va_range *heap = &device->physical->va.null_initialized_heap;
   if (bind->bo || bind->bo_offset || !heap->addr || (heap->addr & 4095) ||
       !heap->size || (heap->size & 4095) || heap->addr >= (UINT64_C(1) << 48) ||
       heap->size > (UINT64_C(1) << 48) - heap->addr ||
       bind->address != heap->addr || bind->size != heap->size ||
       (bind->op != ANV_VM_BIND && bind->op != ANV_VM_UNBIND))
      return VK_ERROR_FEATURE_NOT_PRESENT;
   pthread_mutex_lock(&lifetime_mutex);
   unsigned i = submission_lifetime(device);
   VkResult result = VK_ERROR_INITIALIZATION_FAILED;
   if (i != CUBIT_MEMORY_LIFETIMES) {
      if (bind->op == ANV_VM_BIND && !(*lifetimes[i]).null_heap_active &&
          !(*lifetimes[i]).submission_attempted) {
         (*lifetimes[i]).null_heap_address = heap->addr;
         (*lifetimes[i]).null_heap_size = heap->size;
         (*lifetimes[i]).null_heap_active = true;
         result = VK_SUCCESS;
      } else if (bind->op == ANV_VM_UNBIND && (*lifetimes[i]).null_heap_active &&
                 bind->address == (*lifetimes[i]).null_heap_address &&
                 bind->size == (*lifetimes[i]).null_heap_size) {
         (*lifetimes[i]).null_heap_active = false;
         (*lifetimes[i]).null_heap_closed = true;
         result = VK_SUCCESS;
      }
   }
   pthread_mutex_unlock(&lifetime_mutex);
   return result;
}

static VkResult update_bo_locked(struct anv_device *device, struct anv_bo *bo,
   uint64_t gpu, uint64_t offset, uint64_t bytes, bool remove);

VkResult
anv_cubit_check_status(struct vk_device *vk_device)
{
   struct anv_device *device = container_of(vk_device, struct anv_device, vk);
   pthread_mutex_lock(&lifetime_mutex);
   unsigned i = submission_lifetime(device);
   bool healthy = i != CUBIT_MEMORY_LIFETIMES;
   if (healthy) {
      uint32_t status = cubit_intel_session_status((*lifetimes[i]).tracker.slot);
      healthy = status == 0;
      if (!healthy) transport_failure("session-health", status, 0);
      if (!healthy) (*lifetimes[i]).tracker.lost = true;
   }
   pthread_mutex_unlock(&lifetime_mutex);
   return healthy ? VK_SUCCESS :
      vk_device_set_lost(vk_device, "CuBit render session unavailable");
}

static VkResult
attach_session(struct anv_device *device, uint64_t slot,
               struct anv_cubit_endpoint_pin *pin)
{
   if (!device || !device->physical)
      return VK_ERROR_INITIALIZATION_FAILED;
   VkResult result = memory_init(device, slot, pin);
   if (result != VK_SUCCESS)
      return result; /* Did not acquire this session: do not close it. */
   result = anv_cubit_check_status(&device->vk);
   if (result == VK_SUCCESS) {
      pthread_mutex_lock(&lifetime_mutex);
      unsigned i = submission_lifetime(device);
      uint32_t policy = i == CUBIT_MEMORY_LIFETIMES ? 0 :
         cubit_intel_memory_contract((*lifetimes[i]).tracker.slot);
      if ((policy != 1 && policy != 2) ||
          (policy == 1 && !device->physical->memory.need_flush)) {
         result = VK_ERROR_INITIALIZATION_FAILED;
      } else {
         (*lifetimes[i]).coherent_memory = policy == 2;
      }
      pthread_mutex_unlock(&lifetime_mutex);
   }
   if (result != VK_SUCCESS)
      (void)anv_cubit_memory_finish(device);
   return result;
}

VkResult
anv_cubit_attach_session(struct anv_device *device, uint64_t slot)
{
   return attach_session(device, slot, NULL);
}

VkResult
anv_cubit_attach_owned_session(struct anv_device *device, uint64_t slot,
                               struct anv_cubit_endpoint_pin *pin)
{
   if (!pin || !pin->retired)
      return VK_ERROR_INITIALIZATION_FAILED;
   return attach_session(device, slot, pin);
}

VkResult
anv_cubit_setup_context(struct anv_device *device,
                        const VkDeviceCreateInfo *create_info,
                        uint32_t num_queues)
{
   if (!device || !device->physical || !create_info ||
       create_info->sType != VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO ||
       create_info->flags || num_queues != 1 ||
       create_info->queueCreateInfoCount != 1 ||
       !create_info->pQueueCreateInfos ||
       device->physical->queue.family_count != 1 ||
       device->physical->queue.families[0].engine_class != INTEL_ENGINE_CLASS_RENDER)
      return VK_ERROR_FEATURE_NOT_PRESENT;
   const VkDeviceQueueCreateInfo *q = create_info->pQueueCreateInfos;
   if (q->sType != VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO || q->pNext ||
       q->flags || q->queueFamilyIndex || q->queueCount != 1 ||
       !q->pQueuePriorities ||
       !(q->pQueuePriorities[0] >= 0.0f && q->pQueuePriorities[0] <= 1.0f))
      return VK_ERROR_FEATURE_NOT_PRESENT;
   pthread_mutex_lock(&lifetime_mutex);
   unsigned i = submission_lifetime(device);
   VkResult result = VK_ERROR_INITIALIZATION_FAILED;
   if (i != CUBIT_MEMORY_LIFETIMES && !(*lifetimes[i]).context_claimed &&
       !(*lifetimes[i]).submission_attempted) {
      /* The scoped endpoint already owns the service-side context. No GPU
       * commands or native allocation occur at this logical setup stage. */
      (*lifetimes[i]).context_claimed = true;
      result = VK_SUCCESS;
   }
   pthread_mutex_unlock(&lifetime_mutex);
   return result;
}

bool
anv_cubit_destroy_context(struct anv_device *device)
{
   return device && anv_cubit_memory_finish(device) == VK_SUCCESS;
}

void
anv_cubit_close_device(struct anv_device *device)
{
   if (device)
      (void)anv_cubit_memory_finish(device);
}

VkResult
anv_cubit_create_engine(struct anv_device *device, struct anv_queue *queue,
                        const VkDeviceQueueCreateInfo *create_info)
{
   if (!device || !device->physical || !queue || queue->device != device ||
       !create_info || create_info->sType != VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO ||
       create_info->pNext || create_info->flags || create_info->queueCount != 1 ||
       create_info->queueFamilyIndex || queue->vk.queue_family_index ||
       queue->vk.index_in_family || device->physical->queue.family_count != 1 ||
       queue->family != &device->physical->queue.families[0] ||
       queue->family->engine_class != INTEL_ENGINE_CLASS_RENDER)
      return VK_ERROR_FEATURE_NOT_PRESENT;
   pthread_mutex_lock(&lifetime_mutex);
   unsigned i = submission_lifetime(device);
   VkResult result = VK_ERROR_INITIALIZATION_FAILED;
   if (i != CUBIT_MEMORY_LIFETIMES && (*lifetimes[i]).context_claimed &&
       !(*lifetimes[i]).engine_claimed && !(*lifetimes[i]).submission_attempted) {
      (*lifetimes[i]).engine_claimed = true;
      (*lifetimes[i]).engine_queue = queue;
      result = VK_SUCCESS;
   }
   pthread_mutex_unlock(&lifetime_mutex);
   return result;
}

void
anv_cubit_destroy_engine(struct anv_device *device, struct anv_queue *queue)
{
   if (!device || !queue) return;
   pthread_mutex_lock(&lifetime_mutex);
   /* Cleanup must work even after device loss or null-heap teardown. Never
    * dereference the stored wrapper, and never release another queue's claim.
    * Keep engine_claimed set: queue creation is one-shot for this context. */
   for (unsigned i = 0; i < lifetime_count; i++) {
      if (&(*lifetimes[i]).tracker == device->cubit_cpu_mappings &&
          (*lifetimes[i]).engine_queue == queue)
         (*lifetimes[i]).engine_queue = NULL;
   }
   pthread_mutex_unlock(&lifetime_mutex);
}

static VkResult
change_binding_locked(struct anv_device *device, struct anv_bo *bo, uint64_t gpu,
                      bool allow_live, bool remove)
{
   unsigned i = submission_lifetime(device);
   if (i == CUBIT_MEMORY_LIFETIMES) {
      return vk_device_set_lost(&device->vk, "CuBit binding session unavailable");
   }
   if ((*lifetimes[i]).submission_attempted) {
      if (allow_live)
         return update_bo_locked(device, bo, gpu, 0, bo ? bo->actual_size : 0, remove);
      return VK_ERROR_FEATURE_NOT_PRESENT; /* Live VM update is a different operation. */
   }
   if (!bo || anv_bo_get_real(bo) != bo || !bo->gem_handle || !bo->actual_size ||
       (bo->actual_size & 4095) || bo->actual_size > UINT64_C(16) * 1024 * 1024 ||
       !gpu || (gpu & 4095) || gpu >= (UINT64_C(1) << 48) ||
       bo->actual_size > (UINT64_C(1) << 48) - gpu) {
      return VK_ERROR_INITIALIZATION_FAILED;
   }
   uint32_t status = remove ?
      cubit_intel_unbind_buffer((*lifetimes[i]).tracker.slot,
         bo->gem_handle, gpu, 0, bo->actual_size) :
      cubit_intel_bind_buffer((*lifetimes[i]).tracker.slot,
         bo->gem_handle, gpu, 0, bo->actual_size);
   if (status != 0) {
      /* No rollback/replay: a failed response may follow a successful change. */
      (*lifetimes[i]).tracker.lost = true;
      return vk_device_set_lost(&device->vk, "CuBit BO binding failed; session retained");
   }
   return VK_SUCCESS;
}

static VkResult
update_bo_locked(struct anv_device *device, struct anv_bo *bo,
                            uint64_t gpu, uint64_t offset, uint64_t bytes,
                            bool remove)
{
   unsigned i = submission_lifetime(device);
   if (i == CUBIT_MEMORY_LIFETIMES) {
      return vk_device_set_lost(&device->vk, "CuBit VM session unavailable");
   }
   if (!(*lifetimes[i]).submission_ready) {
      return VK_ERROR_INITIALIZATION_FAILED;
   }
   if (!bo || anv_bo_get_real(bo) != bo || !bo->gem_handle || !bytes ||
       (bytes & 4095) || bytes > UINT64_C(16) * 1024 * 1024 ||
       (offset & 4095) || offset > bo->actual_size ||
       bytes > bo->actual_size - offset ||
       offset > UINT64_C(16) * 1024 * 1024 - bytes ||
       !gpu || (gpu & 4095) || gpu >= (UINT64_C(1) << 48) ||
       bytes > (UINT64_C(1) << 48) - gpu) {
      return VK_ERROR_INITIALIZATION_FAILED;
   }
   uint32_t generation = 0;
   /* Serialize updates with submission and BO/CPU lifecycle operations.
    * A generation is a committed VM transaction, not a batch marker. */
   uint32_t status = (*lifetimes[i]).vm_generation == UINT32_MAX ? 4 :
      cubit_intel_update_binding((*lifetimes[i]).tracker.slot, bo->gem_handle,
         gpu, offset, bytes, remove, (*lifetimes[i]).vm_generation, &generation);
   if (status != 0 || (*lifetimes[i]).vm_generation == UINT32_MAX ||
       generation != (*lifetimes[i]).vm_generation + 1) {
      transport_failure(remove ? "vm-unbind" : "vm-bind", status, bo->gem_handle);
      (*lifetimes[i]).tracker.lost = true;
      (*lifetimes[i]).submission_ready = false;
      return vk_device_set_lost(&device->vk, "CuBit VM update failed; session retained");
   }
   (*lifetimes[i]).vm_generation = generation;
   return VK_SUCCESS;
}

VkResult
anv_cubit_bind_bo_offline(struct anv_device *device, struct anv_bo *bo, uint64_t gpu)
{
   pthread_mutex_lock(&lifetime_mutex);
   VkResult result = change_binding_locked(device, bo, gpu, false, false);
   pthread_mutex_unlock(&lifetime_mutex);
   return result;
}

VkResult
anv_cubit_update_bo_binding(struct anv_device *device, struct anv_bo *bo,
   uint64_t gpu, uint64_t offset, uint64_t bytes, bool remove)
{
   pthread_mutex_lock(&lifetime_mutex);
   VkResult result = update_bo_locked(device, bo, gpu, offset, bytes, remove);
   pthread_mutex_unlock(&lifetime_mutex);
   return result;
}

static VkResult
change_bo(struct anv_device *device, struct anv_bo *bo, bool remove)
{
   /* ANV stores canonical VAs. Verify before converting to the raw48 wire
    * representation. Route and transport stay in one critical section so
    * context preparation cannot race an offline bind into a sealed image. */
   if (!bo || intel_canonical_address(intel_48b_address(bo->offset)) != bo->offset)
      return VK_ERROR_INITIALIZATION_FAILED;
   pthread_mutex_lock(&lifetime_mutex);
   VkResult result = change_binding_locked(device, bo,
      intel_48b_address(bo->offset), true, remove);
   pthread_mutex_unlock(&lifetime_mutex);
   return result;
}

VkResult
anv_cubit_bind_bo(struct anv_device *device, struct anv_bo *bo)
{
   return change_bo(device, bo, false);
}

VkResult
anv_cubit_unbind_bo(struct anv_device *device, struct anv_bo *bo)
{
   return change_bo(device, bo, true);
}

static VkResult
prepare_submission(struct anv_device *device, bool allow_ready)
{
   pthread_mutex_lock(&lifetime_mutex);
   unsigned i = submission_lifetime(device);
   if (i != CUBIT_MEMORY_LIFETIMES && allow_ready &&
       (*lifetimes[i]).submission_ready) {
      pthread_mutex_unlock(&lifetime_mutex);
      return VK_SUCCESS;
   }
   if (i == CUBIT_MEMORY_LIFETIMES || (*lifetimes[i]).submission_attempted) {
      pthread_mutex_unlock(&lifetime_mutex);
      return VK_ERROR_INITIALIZATION_FAILED;
   }
   (*lifetimes[i]).submission_attempted = true;
   uint64_t slot = (*lifetimes[i]).tracker.slot;
   if (cubit_intel_prepare_context(slot) != 0 ||
       cubit_intel_register_context(slot) != 0) {
      (*lifetimes[i]).tracker.lost = true;
      pthread_mutex_unlock(&lifetime_mutex);
      return vk_device_set_lost(&device->vk, "CuBit context preparation failed; session retained");
   }
   (*lifetimes[i]).completion = 1;
   (*lifetimes[i]).submission_ready = true;
   pthread_mutex_unlock(&lifetime_mutex);
   return VK_SUCCESS;
}

VkResult
anv_cubit_prepare_submission(struct anv_device *device)
{
   return prepare_submission(device, false);
}

VkResult
anv_cubit_submit_bo(struct anv_device *device, struct anv_bo *bo,
                    uint64_t gpu, uint64_t offset, uint64_t bytes)
{
   pthread_mutex_lock(&lifetime_mutex);
   unsigned i = submission_lifetime(device);
   if (i == CUBIT_MEMORY_LIFETIMES || !(*lifetimes[i]).submission_ready) {
      if (i != CUBIT_MEMORY_LIFETIMES)
         (*lifetimes[i]).tracker.lost = true;
      pthread_mutex_unlock(&lifetime_mutex);
      return vk_device_set_lost(&device->vk, "CuBit submission context unavailable");
   }
   struct anv_bo *real = bo ? anv_bo_get_real(bo) : NULL;
   uint64_t parent_offset = offset;
   bool valid = real && !real->slab_parent && real->gem_handle && bytes &&
      offset <= bo->actual_size && bytes <= bo->actual_size - offset;
   if (valid && real != bo) {
      /* Mesa anv_slab_bo.c stores canonical VAs for parent and child.
       * Subtract raw48 addresses, not canonical encodings: a slab may cross
       * bit47. Never bind the child again or use its copied handle as owner. */
      uint64_t base = intel_48b_address(real->offset);
      uint64_t child = intel_48b_address(bo->offset);
      valid = intel_canonical_address(base) == real->offset &&
         intel_canonical_address(child) == bo->offset && child >= base &&
         real->actual_size <= (UINT64_C(1) << 48) - base &&
         child - base <= real->actual_size &&
         bo->actual_size <= real->actual_size - (child - base);
      if (valid)
         parent_offset += child - base; /* bounded by the parent extent */
   }
   if (!valid || parent_offset > real->actual_size ||
       bytes > real->actual_size - parent_offset) {
      (*lifetimes[i]).tracker.lost = true;
      pthread_mutex_unlock(&lifetime_mutex);
      return vk_device_set_lost(&device->vk, "CuBit batch outside retained BO");
   }
   uint32_t completion = 0;
   uint32_t status = cubit_intel_submit_batch((*lifetimes[i]).tracker.slot,
      real->gem_handle, gpu, parent_offset, bytes, (*lifetimes[i]).completion, &completion);
   if (status != 0 || (*lifetimes[i]).completion == UINT32_MAX ||
       completion != (*lifetimes[i]).completion + 1) {
      /* Record the submission failure before teardown can report a secondary
       * denied close after the service revokes this session. Status 4 is the
       * transport's malformed/uncertain-result classification. */
      transport_failure("submit-batch", status ? status : 4, real->gem_handle);
      (*lifetimes[i]).tracker.lost = true;
      (*lifetimes[i]).submission_ready = false;
      pthread_mutex_unlock(&lifetime_mutex);
      return vk_device_set_lost(&device->vk, "CuBit batch completion failed; session retained");
   }
   (*lifetimes[i]).completion = completion;
   pthread_mutex_unlock(&lifetime_mutex);
   return VK_SUCCESS;
}

static VkResult
sync_failure(struct anv_device *device)
{
   pthread_mutex_lock(&lifetime_mutex);
   unsigned i = submission_lifetime(device);
   if (i != CUBIT_MEMORY_LIFETIMES) {
      (*lifetimes[i]).tracker.lost = true;
      (*lifetimes[i]).submission_ready = false;
   }
   pthread_mutex_unlock(&lifetime_mutex);
   return vk_device_set_lost(&device->vk, "CuBit queue synchronization failed");
}

VkResult
anv_cubit_wait_dependencies(struct anv_device *device,
   uint32_t wait_count, const struct vk_sync_wait *waits,
   uint64_t abs_timeout_ns)
{
   pthread_mutex_lock(&lifetime_mutex);
   unsigned i = submission_lifetime(device);
   bool ready = i != CUBIT_MEMORY_LIFETIMES && (*lifetimes[i]).submission_ready;
   pthread_mutex_unlock(&lifetime_mutex);
   if (!ready || (wait_count && !waits))
      return sync_failure(device);
   /* Another queue may need the tracker lock to satisfy these dependencies.
    * Flags=0 waits ALL for completion, not ANY and not merely PENDING. The
    * submission helper revalidates the session after this unlocked wait. */
   VkResult result = vk_sync_wait_many(&device->vk, wait_count, waits,
                                      0, abs_timeout_ns);
   if (result == VK_TIMEOUT)
      return result;
   if (result != VK_SUCCESS)
      return sync_failure(device);
   pthread_mutex_lock(&lifetime_mutex);
   i = submission_lifetime(device);
   ready = i != CUBIT_MEMORY_LIFETIMES && (*lifetimes[i]).submission_ready;
   pthread_mutex_unlock(&lifetime_mutex);
   return ready ? VK_SUCCESS : sync_failure(device);
}

VkResult
anv_cubit_submit_bo_sync(struct anv_device *device, struct anv_bo *bo,
   uint64_t gpu, uint64_t offset, uint64_t bytes,
   uint32_t wait_count, const struct vk_sync_wait *waits,
   uint32_t signal_count, const struct vk_sync_signal *signals,
   uint64_t abs_timeout_ns)
{
   if (signal_count && !signals)
      return sync_failure(device);
   VkResult result = anv_cubit_wait_dependencies(device, wait_count, waits,
                                                abs_timeout_ns);
   if (result != VK_SUCCESS)
      return result;
   result = anv_cubit_submit_bo(device, bo, gpu, offset, bytes);
   if (result != VK_SUCCESS)
      return result;
   result = vk_sync_signal_many(&device->vk, signal_count, signals);
   return result == VK_SUCCESS ? VK_SUCCESS : sync_failure(device);
}

VkResult
anv_cubit_queue_exec_locked(struct anv_queue *queue,
   uint32_t wait_count, const struct vk_sync_wait *waits,
   uint32_t cmd_buffer_count, struct anv_cmd_buffer **cmd_buffers,
   uint32_t signal_count, const struct vk_sync_signal *signals,
   struct anv_query_pool *perf_query_pool, uint32_t perf_query_pass,
   struct anv_utrace_submit *utrace_submit)
{
   struct anv_device *device = queue->device;
   pthread_mutex_lock(&lifetime_mutex);
   unsigned lifetime = submission_lifetime(device);
   bool owned = lifetime != CUBIT_MEMORY_LIFETIMES &&
                (*lifetimes[lifetime]).engine_queue == queue;
   pthread_mutex_unlock(&lifetime_mutex);
   if (!owned) return sync_failure(device);
   (void)perf_query_pass;
   if (perf_query_pool || utrace_submit || !queue->family ||
       queue->family->engine_class != INTEL_ENGINE_CLASS_RENDER)
      return VK_ERROR_FEATURE_NOT_PRESENT;
   if ((cmd_buffer_count && !cmd_buffers) || (signal_count && !signals))
      return sync_failure(device);
   /* A missed pre-lock dependency is a backend sequencing error. Never wait
    * for its producer while holding the common ANV device mutex. */
   VkResult result = anv_cubit_wait_dependencies(device, wait_count, waits, 0);
   if (result != VK_SUCCESS) return sync_failure(device);
   for (uint32_t i = 0; i < cmd_buffer_count; i++) {
      struct anv_cmd_buffer *cmd = cmd_buffers[i];
      if (!cmd || cmd->device != device || list_is_empty(&cmd->batch_bos))
         return sync_failure(device);
      if (cmd->companion_rcs_cmd_buffer)
         return VK_ERROR_FEATURE_NOT_PRESENT;
      struct anv_batch_bo *batch =
         list_first_entry(&cmd->batch_bos, struct anv_batch_bo, link);
      if (!batch->bo || !batch->length || batch->length > batch->bo->actual_size ||
          intel_canonical_address(intel_48b_address(batch->bo->offset)) != batch->bo->offset)
         return sync_failure(device);
   }
   if (cmd_buffer_count) {
      /* Same common batch chaining as upstream xe_queue_exec_locked. All
       * batch-reachable BOs must remain bound and alive until this returns. */
      anv_cmd_buffer_chain_command_buffers(cmd_buffers, cmd_buffer_count);
#ifdef SUPPORT_INTEL_INTEGRATED_GPUS
      if (device->physical->memory.need_flush &&
          anv_bo_needs_host_cache_flush(device->batch_bo_pool.bo_alloc_flags))
         anv_cmd_buffer_clflush(cmd_buffers, cmd_buffer_count);
#endif
      struct anv_batch_bo *first =
         list_first_entry(&cmd_buffers[0]->batch_bos, struct anv_batch_bo, link);
      result = anv_cubit_submit_bo(device, first->bo,
         intel_48b_address(first->bo->offset), 0, first->length);
      if (result != VK_SUCCESS) return result;
   }
   /* Empty submissions are dependency/signal operations. Nonempty ones reach
    * here only after the driver marker and scheduling disable acknowledged. */
   result = vk_sync_signal_many(&device->vk, signal_count, signals);
   if (result == VK_SUCCESS && queue->sync) {
      const struct vk_sync_signal completed = {.sync = queue->sync};
      result = vk_sync_signal_many(&device->vk, 1, &completed);
   }
   return result == VK_SUCCESS ? VK_SUCCESS : sync_failure(device);
}

VkResult
anv_cubit_queue_exec_async(struct anv_async_submit *submit,
   uint32_t wait_count, const struct vk_sync_wait *waits,
   uint32_t signal_count, const struct vk_sync_signal *signals)
{
   struct anv_queue *queue = submit->queue;
   struct anv_device *device = queue->device;
   pthread_mutex_lock(&lifetime_mutex);
   unsigned lifetime = submission_lifetime(device);
   bool owned = lifetime != CUBIT_MEMORY_LIFETIMES &&
                (*lifetimes[lifetime]).engine_queue == queue;
   pthread_mutex_unlock(&lifetime_mutex);
   if (!owned) return sync_failure(device);
   if (submit->use_companion_rcs || !queue->family ||
       queue->family->engine_class != INTEL_ENGINE_CLASS_RENDER)
      return VK_ERROR_FEATURE_NOT_PRESENT;
   if ((signal_count && !signals) || !submit->bo_pool ||
       !util_dynarray_num_elements(&submit->batch_bos, struct anv_bo *))
      return sync_failure(device);
   util_dynarray_foreach(&submit->batch_bos, struct anv_bo *, bo) {
      if (!*bo || !(*bo)->map || !(*bo)->size ||
          (*bo)->size > (*bo)->actual_size ||
          intel_canonical_address(intel_48b_address((*bo)->offset)) != (*bo)->offset)
         return sync_failure(device);
   }
   /* This callback is entered without the common ANV device mutex. The
    * bring-up transport completes synchronously even though the KMD entry
    * point permits asynchronous implementations. Retain every chained BO. */
   /* setup_context runs before Mesa allocates its state/batch BOs. Seal the
    * initial bindings here, at the first internal batch, not during setup.
    * The shared lifetime lock makes concurrent first submissions observe one
    * preparation; a failed attempt remains poisoned and is never replayed. */
   VkResult result = prepare_submission(device, true);
   if (result != VK_SUCCESS) return sync_failure(device);
   result = anv_cubit_wait_dependencies(device, wait_count, waits,
                                                UINT64_MAX);
   if (result != VK_SUCCESS) return result;
#ifdef SUPPORT_INTEL_INTEGRATED_GPUS
   if (device->physical->memory.need_flush &&
       anv_bo_needs_host_cache_flush(submit->bo_pool->bo_alloc_flags)) {
      util_dynarray_foreach(&submit->batch_bos, struct anv_bo *, bo)
         util_flush_range((*bo)->map, (*bo)->size);
   }
#endif
   struct anv_bo *first =
      *util_dynarray_element(&submit->batch_bos, struct anv_bo *, 0);
   result = anv_cubit_submit_bo(device, first,
                               intel_48b_address(first->offset), 0, first->size);
   if (result != VK_SUCCESS) return result;
   result = vk_sync_signal_many(&device->vk, signal_count, signals);
   if (result == VK_SUCCESS && submit->signal.sync)
      result = vk_sync_signal_many(&device->vk, 1, &submit->signal);
   if (result == VK_SUCCESS && queue->sync) {
      const struct vk_sync_signal completed = {.sync = queue->sync};
      result = vk_sync_signal_many(&device->vk, 1, &completed);
   }
   return result == VK_SUCCESS ? VK_SUCCESS : sync_failure(device);
}

static bool
slot_retained_locked(uint64_t slot)
{
   if (slot >= CUBIT_ENDPOINT_SLOTS)
      return true;
   for (unsigned i = 0; i < lifetime_count; i++) {
      if ((*lifetimes[i]).tracker.slot == slot && !(*lifetimes[i]).complete)
         return true;
   }
   return false;
}

bool
anv_cubit_memory_slot_retained(uint64_t slot)
{
   pthread_mutex_lock(&lifetime_mutex);
   bool retained = slot_retained_locked(slot);
   pthread_mutex_unlock(&lifetime_mutex);
   return retained;
}

static VkResult
memory_init(struct anv_device *device, uint64_t slot,
            struct anv_cubit_endpoint_pin *pin)
{
   pthread_mutex_lock(&lifetime_mutex);
   if (vk_device_is_lost_no_report(&device->vk) ||
       slot_retained_locked(slot) || device->cubit_cpu_mappings) {
      pthread_mutex_unlock(&lifetime_mutex);
      return VK_ERROR_INITIALIZATION_FAILED;
   }
   unsigned index;
   for (index = 0; index < lifetime_count; index++) {
      if ((*lifetimes[index]).detached && (*lifetimes[index]).complete)
         break;
   }
   if (index == lifetime_count) {
      if (!grow_lifetimes()) {
         pthread_mutex_unlock(&lifetime_mutex);
         return VK_ERROR_OUT_OF_HOST_MEMORY;
      }
      struct cubit_memory_lifetime *record = calloc(1, sizeof(*record));
      if (!record) {
         pthread_mutex_unlock(&lifetime_mutex);
         return VK_ERROR_OUT_OF_HOST_MEMORY;
      }
      lifetimes[index] = record;
      lifetime_count++;
   }
   /* Only detached AND confirmed-complete bookkeeping may be recycled.
    * No device wrapper points here, and polling has no outstanding cleanup.
    * This clears CPU records, not driver BO storage or GPU mapping ownership. */
   memset(&(*lifetimes[index]), 0, sizeof((*lifetimes[index])));
   struct cubit_cpu_mapping_tracker *tracker = &(*lifetimes[index]).tracker;
   tracker->slot = slot;
   if (pin) {
      (*lifetimes[index]).pin = *pin;
      *pin = (struct anv_cubit_endpoint_pin){0};
   }
   device->cubit_cpu_mappings = tracker;
   pthread_mutex_unlock(&lifetime_mutex);
   return VK_SUCCESS;
}

VkResult
anv_cubit_memory_init(struct anv_device *device, uint64_t slot)
{
   return memory_init(device, slot, NULL);
}

/* No device pointer or application allocator survives detachment. CPU-view
 * retirement precedes close because per-map requests require active admission.
 * Backing is retained by the service throughout, including uncertain failures. */
static bool
drain_lifetime(unsigned i)
{
   if ((*lifetimes[i]).retirement_failed)
      return false;
   if (!(*lifetimes[i]).cpu_drained) {
      if (!cubit_cpu_tracker_drain(&(*lifetimes[i]).tracker))
         return false;
      (*lifetimes[i]).cpu_drained = true;
   }
   const uint64_t slot = (*lifetimes[i]).tracker.slot;
   if (!(*lifetimes[i]).close_attempted) {
      (*lifetimes[i]).close_attempted = true;
      uint64_t retired_tag = 0;
      if (cubit_intel_close_session(slot, &retired_tag) != 0 || !retired_tag) {
         (*lifetimes[i]).retirement_failed = true;
         return false; /* Never replay an uncertain close. */
      }
   }
   uint32_t status = cubit_intel_poll_session_retirement(slot);
   if (status == 0) {
      struct anv_cubit_endpoint_pin pin = (*lifetimes[i]).pin;
      (*lifetimes[i]).pin = (struct anv_cubit_endpoint_pin){0};
      if (pin.retired)
         pin.retired(pin.context);
      return true;
   }
   if (status != 4)
      (*lifetimes[i]).retirement_failed = true;
   return false;
}

VkResult
anv_cubit_memory_finish(struct anv_device *device)
{
   pthread_mutex_lock(&lifetime_mutex);
   struct cubit_cpu_mapping_tracker *tracker = device->cubit_cpu_mappings;
   if (!tracker) {
      pthread_mutex_unlock(&lifetime_mutex);
      return VK_SUCCESS;
   }
   bool complete = false, found = false;
   for (unsigned i = 0; i < lifetime_count; i++) {
      if (&(*lifetimes[i]).tracker != tracker)
         continue;
      found = true;
      tracker->lost = true;
      (*lifetimes[i]).detached = true;
      (*lifetimes[i]).engine_queue = NULL;
      device->cubit_cpu_mappings = NULL;
      complete = drain_lifetime(i);
      (*lifetimes[i]).complete = complete;
      break;
   }
   pthread_mutex_unlock(&lifetime_mutex);
   return complete ? VK_SUCCESS :
      vk_device_set_lost(&device->vk, found ? "CuBit session deferred to process cleanup" :
                                           "CuBit CPU tracker has no cleanup owner");
}

uint32_t
anv_cubit_memory_poll(void)
{
   uint32_t pending = 0;
   pthread_mutex_lock(&lifetime_mutex);
   for (unsigned i = 0; i < lifetime_count; i++) {
      if ((*lifetimes[i]).detached && !(*lifetimes[i]).complete) {
         (*lifetimes[i]).complete = drain_lifetime(i);
         pending += !(*lifetimes[i]).complete;
      }
   }
   pthread_mutex_unlock(&lifetime_mutex);
   return pending;
}

static bool
rounded_bytes(uint64_t bytes, uint64_t *rounded)
{
   if (!bytes || bytes > UINT64_MAX - 4095)
      return false;
   *rounded = (bytes + 4095) & ~UINT64_C(4095);
   return true;
}

static uint32_t
gem_create_locked(struct anv_device *device,
                    const struct intel_memory_class_instance **regions,
                    uint16_t num_regions, uint64_t size,
                    enum anv_bo_alloc_flags flags, uint64_t *actual_size)
{
   if (!actual_size)
      return 0;
   *actual_size = 0;
   struct cubit_cpu_mapping_tracker *tracker = device->cubit_cpu_mappings;
   /* Native owned DMA grants map CPU WB. Coherence requires a successful
    * session contract query; otherwise Mesa must perform explicit flushing.
    * On an admitted coherent session, absence of HOST_CACHED is not a WC
    * requirement (the i915 LLC mmap path also chooses WB). Explicit-only
    * sessions require HOST_CACHED so common ANV performs CPU maintenance. */
   unsigned supported = ANV_BO_ALLOC_MAPPED | ANV_BO_ALLOC_NO_LOCAL_MEM |
                              ANV_BO_ALLOC_INTERNAL | ANV_BO_ALLOC_SLAB_PARENT |
                              ANV_BO_ALLOC_HOST_CACHED | ANV_BO_ALLOC_FIXED_ADDRESS |
                              ANV_BO_ALLOC_CAPTURE | ANV_BO_ALLOC_NULL_INITIALIZED_HEAP |
                              ANV_BO_ALLOC_DESCRIPTOR_POOL | ANV_BO_ALLOC_DYNAMIC_VISIBLE_POOL |
                              ANV_BO_ALLOC_CLIENT_VISIBLE_ADDRESS | ANV_BO_ALLOC_32BIT_ADDRESS;
   /* 32BIT_ADDRESS selects common ANV's vma_lo heap (e.g. older-generation
    * scratch). It constrains GPU virtual placement, not backing DMA address.
    * Preserve the intent in common ANV; do not manufacture a physical or
    * virtual address here or weaken the subsequent native bind checks. */
   /* CLIENT_VISIBLE_ADDRESS is Vulkan buffer-device-address VA policy, not
    * external sharing. Common anv_vma_alloc reserves the requested address
    * or chooses an address in the selected heap, retaining bo.alloc_flags.
    * Native bind still authenticates the session/handle and validates the
    * mapping. Creation does not publish a VA or grant CPU/client authority. */
   /* Descriptor/sampler pool flags select common ANV's vma_desc and
    * vma_dynamic_visible GPU VA heaps, including slab-parent allocations.
    * They do not select backing memory or authorize an import. Common ANV
    * still assigns the VA and native bind still validates the mapping;
    * keep the region, size, ownership and coherence checks below unchanged. */
   /* FIXED_ADDRESS is a GPU VA policy, not physical placement or CPU mmap.
    * Mesa's anv_bo_vma_alloc_or_close assigns explicit_address after creation;
    * the later native bind validates/publishes that range. Never infer an
    * address here or bypass bind validation. COHERENT requires admission.
    * CAPTURE requests hang diagnostics, not address capture/replay. Common ANV
    * retains it in bo.alloc_flags. Our backend accepts that intent but has no
    * error-state byte-dump exporter: do not delegate grants, log contents or
    * claim I915_PARAM_HAS_EXEC_CAPTURE as a side effect of buffer creation. */
   uint64_t bytes;
   unsigned lifetime = submission_lifetime(device);
   const bool coherent = lifetime != CUBIT_MEMORY_LIFETIMES &&
                         (*lifetimes[lifetime]).coherent_memory;
   if (coherent)
      supported |= ANV_BO_ALLOC_HOST_COHERENT;
   /* Common ANV, not the KMD, implements these allocation policies:
    * anv_device_alloc_bo expands size for AUX_CCS before gem_create;
    * anv_bo_vma_calc_alignment_requirement and anv_bo_vma_alloc_or_close
    * provide AUX-TT GPU VA alignment. Allocate the supplied total exactly
    * once; neither flag requests physically contiguous/aligned metadata.
    * Keep them gated on actual auxiliary-map support and initialized tables.
    * This does not admit COMPRESSED, external/imported or scanout BOs. */
   if (device->info && device->info->has_aux_map && device->aux_map_ctx)
      supported |= ANV_BO_ALLOC_AUX_TT_ALIGNED | ANV_BO_ALLOC_AUX_CCS;
   if (vk_device_is_lost_no_report(&device->vk) ||
       lifetime == CUBIT_MEMORY_LIFETIMES ||
       ((flags & ANV_BO_ALLOC_NULL_INITIALIZED_HEAP) && !(*lifetimes[lifetime]).null_heap_active) ||
       !tracker || tracker->lost || !device->physical ||
       (!coherent && !device->physical->memory.need_flush) ||
       (!coherent && !(flags & ANV_BO_ALLOC_HOST_CACHED)) ||
       !regions || num_regions != 1 ||
       !regions[0] || regions[0] != device->physical->sys.region ||
       ((unsigned)flags & ~supported) || !rounded_bytes(size, &bytes) ||
       bytes > UINT64_C(16) * 1024 * 1024)
      return 0;
   uint32_t handle = 0;
   const uint32_t status = cubit_intel_create_buffer(tracker->slot, bytes, &handle);
   if (status != 0) {
      if (status == 4) {
         tracker->lost = true;
         vk_device_set_lost(&device->vk, "CuBit buffer creation outcome uncertain");
      }
      return 0;
   }
   *actual_size = bytes;
   return handle;
}

static void
gem_close_locked(struct anv_device *device, struct anv_bo *bo)
{
   struct cubit_cpu_mapping_tracker *tracker = device->cubit_cpu_mappings;
   if (!tracker) {
      vk_device_set_lost(&device->vk, "CuBit buffer close without transport");
      return;
   }
   const uint32_t handle = anv_bo_get_real(bo)->gem_handle;
   /* Internal ANV teardown may discard a BO after a failed unmap. The
    * device-owned records remain available even after retiring its name. */
   for (uint32_t i = 0; i < tracker->used; i++) {
      struct cubit_cpu_mapping *record = &cubit_cpu_tracker_records(tracker)[i];
      if (record->bo_handle == handle) {
         uint32_t status = cubit_cpu_mapping_release(record, false);
         if (status != 0) {
            transport_failure("close-cpu-view", status, handle);
            tracker->lost = true;
         }
      }
   }
   /* This retires only the name. The service retains backing; neither this
    * call nor CPU-view retirement substitutes for a GPU completion fence. */
   uint32_t status = cubit_intel_close_buffer(tracker->slot, handle);
   if (status != 0) {
      transport_failure("close-buffer", status, handle);
      tracker->lost = true;
   }
   if (tracker->lost)
      vk_device_set_lost(&device->vk, "CuBit buffer close incomplete");
}

static void *
gem_mmap_locked(struct anv_device *device, struct anv_bo *bo,
                   uint64_t offset, uint64_t size, void *placed_addr)
{
   struct cubit_cpu_mapping_tracker *tracker = device->cubit_cpu_mappings;
   struct anv_bo *real = anv_bo_get_real(bo);
   uint64_t bytes, address;
   if (vk_device_is_lost_no_report(&device->vk) ||
       !tracker || placed_addr || tracker->lost || (offset & 4095) ||
       !rounded_bytes(size, &bytes) || offset > real->actual_size ||
       bytes > real->actual_size - offset)
      return MAP_FAILED;
   /* anv_device_map_bo already adjusts slab offset relative to its parent. */
   if (cubit_cpu_tracker_map(tracker, real->gem_handle, offset, bytes, 1, &address)) {
      if (tracker->lost)
         vk_device_set_lost(&device->vk, "CuBit CPU mapping outcome uncertain");
      return MAP_FAILED;
   }
   return (void *)(uintptr_t)address;
}

static void
wait_cpu_retirement(void)
{
   /* Synchronous transport adapter; bounded observations, not a deadline or
    * GPU wait. Keep lifetime_mutex held so BO teardown cannot race this wait. */
   usleep(1000);
}

static VkResult
unmap_bo_locked(struct anv_device *device, struct anv_bo *bo,
                   void *map, size_t size, bool replace)
{
   struct cubit_cpu_mapping_tracker *tracker = device->cubit_cpu_mappings;
   uint64_t bytes;
   if (!tracker || replace || !rounded_bytes(size, &bytes))
      return VK_ERROR_MEMORY_MAP_FAILED;
   struct anv_bo *real = anv_bo_get_real(bo);
   const uint32_t result = cubit_cpu_tracker_unmap_wait
      (tracker, real->gem_handle, (uintptr_t)map, bytes, false, 100,
       wait_cpu_retirement);
   if (result != 0) {
      transport_failure("unmap-cpu-view", result, real->gem_handle);
      /* Internal ANV cleanup can ignore unmap's return value. Retain records
       * at device scope and make loss visible through vk_device as well. */
      return vk_device_set_lost(&device->vk, "CuBit CPU grant retirement incomplete");
   }
   return VK_SUCCESS;
}

/* Serialize tracker mutations across distinct BOs as well as detach/poll.
 * This initial coarse lock covers control-plane IPC, including synchronous
 * submission waits. It is not a parallel/asynchronous queue implementation.
 * Destruction still requires Vulkan's caller-side object lifetime rules. */
uint32_t
anv_cubit_gem_create(struct anv_device *device,
                    const struct intel_memory_class_instance **regions,
                    uint16_t num_regions, uint64_t size,
                    enum anv_bo_alloc_flags flags, uint64_t *actual_size)
{
   pthread_mutex_lock(&lifetime_mutex);
   uint32_t result = gem_create_locked(device, regions, num_regions, size, flags, actual_size);
   pthread_mutex_unlock(&lifetime_mutex);
   return result;
}

void
anv_cubit_gem_close(struct anv_device *device, struct anv_bo *bo)
{
   pthread_mutex_lock(&lifetime_mutex);
   gem_close_locked(device, bo);
   pthread_mutex_unlock(&lifetime_mutex);
}

void *
anv_cubit_gem_mmap(struct anv_device *device, struct anv_bo *bo,
                   uint64_t offset, uint64_t size, void *placed_addr)
{
   pthread_mutex_lock(&lifetime_mutex);
   void *result = gem_mmap_locked(device, bo, offset, size, placed_addr);
   pthread_mutex_unlock(&lifetime_mutex);
   return result;
}

VkResult
anv_cubit_unmap_bo(struct anv_device *device, struct anv_bo *bo,
                  void *map, size_t size, bool replace)
{
   pthread_mutex_lock(&lifetime_mutex);
   VkResult result = unmap_bo_locked(device, bo, map, size, replace);
   pthread_mutex_unlock(&lifetime_mutex);
   return result;
}
