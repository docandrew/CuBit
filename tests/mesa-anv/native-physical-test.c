#include "anv_cubit_physical.h"
#include "anv_cubit_memory.h"
#include "anv_cubit_sync.h"
#include "anv_measure.h"
#include "cubit-memory-info.h"
#include <assert.h>

static unsigned mode, retained, released, allocated, freed, common_finished;
static unsigned opened, wsi, measured, generated;
static int endpoint;
static struct anv_memory_budget shared_budget;
static unsigned budget_queries;
static unsigned fail_retain_after, fail_query_after;
enum fault_stage { NO_FAULT, ALLOCATION, RETAIN, DEVICE_QUERY, MEMORY_INFO,
                   MEMORY_TYPES, WSI_INIT };
static enum fault_stage injected_stage;
static unsigned injected_after;
static bool inject(enum fault_stage stage)
{
   if (stage != injected_stage || injected_after == 0)
      return false;
   return --injected_after == 0;
}
bool cubit_mesa_memory_budget(const struct anv_physical_device *d,
   cubit_gpu_query_call call, void *ctx,
   VkPhysicalDeviceMemoryBudgetPropertiesEXT *out)
{
   assert(d && call && ctx == &endpoint && out);
   budget_queries++;
   return false;
}
static void *alloc_cb(void *ctx, size_t bytes, size_t align, VkSystemAllocationScope scope)
{
   (void)ctx; (void)align; (void)scope;
   if (mode == 1 || inject(ALLOCATION)) return NULL;
   allocated++;
   return calloc(1, bytes);
}
static void free_cb(void *ctx, void *ptr)
{ (void)ctx; freed++; free(ptr); }
static bool retain(void *ctx)
{ assert(ctx == &endpoint); if (mode == 2 || inject(RETAIN) ||
    (fail_retain_after && --fail_retain_after==0)) return false;
  retained++; return true; }
static void release(void *ctx)
{ assert(ctx == &endpoint); released++; }
static bool query(void *ctx, const struct cubit_gpu_query_message *req,
                  struct cubit_gpu_query_message *reply)
{ (void)ctx; (void)req; (void)reply; abort(); }
static VkResult open_session(void *ctx, struct anv_device *d)
{ assert(ctx == &endpoint && d); opened++; return VK_ERROR_DEVICE_LOST; }
const struct anv_kmd_backend anv_cubit_transport_backend = {0};
bool cubit_mesa_query_runtime_device(cubit_gpu_query_call call, void *ctx,
                                    struct intel_device_info *out)
{
   assert(call == query && ctx == &endpoint);
   if (mode == 4 || inject(DEVICE_QUERY) ||
       (fail_query_after && --fail_query_after==0)) return false;
   out->verx10 = 120; out->has_context_isolation = true;
   out->gtt_size = UINT64_C(1) << 48;
   return true;
}
bool cubit_mesa_refresh_memory_info(struct anv_physical_device *d,
                                   cubit_gpu_query_call call, void *ctx)
{
   assert(call == query && ctx == &endpoint);
   d->sys.size = 16777216;
   return mode != 5 && !inject(MEMORY_INFO);
}
bool cubit_mesa_init_memory_types(struct anv_physical_device *d,
                                 cubit_gpu_query_call call, void *ctx)
{ assert(d && call == query && ctx == &endpoint);
  return mode != 6 && !inject(MEMORY_TYPES); }
VkResult anv_cubit_init_sync_types(struct anv_physical_device *d)
{ assert(d); return VK_SUCCESS; }
VkResult anv_physical_device_alloc(struct anv_instance *i,
   const struct anv_kmd_backend *b, struct anv_physical_device **out)
{
   if (mode == 3) return VK_ERROR_OUT_OF_HOST_MEMORY;
   *out = vk_zalloc(&i->vk.alloc, sizeof(**out), 8, VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE);
   if (!*out) return VK_ERROR_OUT_OF_HOST_MEMORY;
   (*out)->instance = i; (*out)->kmd_backend = b;
   return VK_SUCCESS;
}
void anv_physical_device_free(struct anv_physical_device *d)
{ vk_free(&d->instance->vk.alloc, d); }
void anv_physical_device_finish_common(struct anv_physical_device *d)
{ common_finished++; free(d->engine_info); d->engine_info = NULL; }
VkResult anv_physical_device_init_common(struct anv_physical_device *d)
{
   assert(d->info.kmd_type == INTEL_KMD_TYPE_CUBIT);
   assert(d->local_fd == -1 && d->master_fd == -1 && d->memory.heaps_budget);
   const struct anv_kmd_backend *b = d->kmd_backend;
   assert(b->finish_physical && b->init_engine_info && b->open_device);
   VkResult r = b->get_physical_parameters(d);
   if (r != VK_SUCCESS) return r;
   assert(b->restrict_sys_heap_size(d, UINT64_MAX) == d->sys.size);
   assert(b->restrict_sys_heap_size(d, 4096) == 4096);
   return b->init_memory_types(d);
}
void anv_shader_init_uuid(struct anv_physical_device *d) { assert(d); }
void anv_physical_device_init_properties(struct anv_physical_device *d)
{
   assert(d->queue.family_count == 1 && d->queue.families[0].queueCount == 1);
   assert(d->engine_info->num_engines == 1);
}
VkResult anv_init_wsi(struct anv_physical_device *d)
{ assert(d); if (mode == 7 || inject(WSI_INIT)) return VK_ERROR_INITIALIZATION_FAILED;
  wsi++; return VK_SUCCESS; }
void anv_measure_device_init(struct anv_physical_device *d) { assert(d); measured++; }
#define GEN(G) \
void gfx##G##_init_physical_device_state(struct anv_physical_device *d) { assert(d); generated++; } \
void gfx##G##_init_instructions(struct anv_physical_device *d) { assert(d); generated++; }
GEN(9) GEN(11) GEN(12) GEN(125) GEN(20) GEN(30) GEN(35)

void anv_physical_device_destroy(struct vk_physical_device *vk)
{
   struct anv_physical_device *d=container_of(vk,struct anv_physical_device,vk);
   anv_physical_device_finish_common(d);
   d->kmd_backend->finish_physical(d);
   anv_physical_device_free(d);
}

int main(void)
{
   struct anv_instance *i = calloc(1, sizeof(*i)); assert(i);
   i->vk.alloc.pfnAllocation = alloc_cb; i->vk.alloc.pfnFree = free_cb;
   struct anv_cubit_provider p = {&endpoint, retain, release, query, open_session, &shared_budget};
   struct vk_physical_device *sentinel = (void *)(uintptr_t)0x1234, *out = sentinel;
   assert(anv_cubit_physical_device_create(NULL, &p, &out) != VK_SUCCESS);
   struct anv_cubit_provider invalid = p; invalid.open_device = NULL;
   assert(anv_cubit_physical_device_create(i, &invalid, &out) != VK_SUCCESS);
   assert(out == sentinel && allocated == 0 && retained == 0);
   invalid = p; invalid.budget = NULL;
   assert(anv_cubit_physical_device_create(i, &invalid, &out) != VK_SUCCESS);
   assert(out == sentinel && allocated == 0 && retained == 0);
   for (mode = 1; mode <= 7; mode++) {
      out = sentinel;
      assert(anv_cubit_physical_device_create(i, &p, &out) != VK_SUCCESS);
      assert(out == sentinel && allocated == freed && retained == released);
      assert(!wsi && !measured && !generated && !opened);
   }
   mode = 0;
   assert(anv_cubit_physical_device_create(i, &p, &out) == VK_SUCCESS);
   assert(out != sentinel && retained == released + 1 && allocated == freed + 2);
   assert(wsi == 1 && measured == 1 && generated == 2);
   struct anv_physical_device *d = container_of(out, struct anv_physical_device, vk);
   struct anv_device logical = {.physical = d};
   assert(d->kmd_backend->open_device(&logical) == VK_ERROR_DEVICE_LOST && opened == 1);
   anv_physical_device_finish_common(d);
   d->kmd_backend->finish_physical(d);
   assert(d->memory.heaps_budget == NULL && d->kmd_backend == NULL);
   anv_physical_device_free(d);
   assert(allocated == freed && retained == released && common_finished == 4);
   struct vk_physical_device *second;
   assert(anv_cubit_physical_device_create(i, &p, &out) == VK_SUCCESS);
   assert(anv_cubit_physical_device_create(i, &p, &second) == VK_SUCCESS);
   struct anv_physical_device *a = container_of(out, struct anv_physical_device, vk);
   struct anv_physical_device *b = container_of(second, struct anv_physical_device, vk);
   assert(a->kmd_backend != b->kmd_backend);
   assert(a->memory.heaps_budget == &shared_budget &&
          b->memory.heaps_budget == &shared_budget);
   p_atomic_add(&a->memory.heaps_budget->used[0], 4096);
   assert(p_atomic_read(&b->memory.heaps_budget->used[0]) == 4096);
   /* The caller's descriptor is copied, not retained by address. Destroying
    * one physical object does not invalidate another's table or provider. */
   memset(&p, 0, sizeof(p));
   anv_physical_device_finish_common(a);
   a->kmd_backend->finish_physical(a);
   anv_physical_device_free(a);
   logical.physical = b;
   assert(b->kmd_backend->open_device(&logical) == VK_ERROR_DEVICE_LOST && opened == 2);
   assert(!b->kmd_backend->refresh_memory_info && b->kmd_backend->get_memory_budget);
   VkPhysicalDeviceMemoryBudgetPropertiesEXT budget = {0};
   b->kmd_backend->get_memory_budget(b, &budget);
   assert(budget_queries == 1);
   assert(p_atomic_read(&b->memory.heaps_budget->used[0]) == 4096);
   p_atomic_add(&b->memory.heaps_budget->used[0], -4096);
   anv_physical_device_finish_common(b);
   b->kmd_backend->finish_physical(b);
   anv_physical_device_free(b);
   assert(allocated == freed && retained == released);
   list_inithead(&i->vk.physical_devices.list);
   assert(mtx_init(&i->vk.physical_devices.mutex,mtx_plain)==thrd_success);
   assert(anv_cubit_enumerate_physical_devices(&i->vk)==VK_ERROR_INITIALIZATION_FAILED);
   p=(struct anv_cubit_provider){&endpoint,retain,release,query,open_session,&shared_budget};
   struct anv_cubit_provider inventory[]={p,p};
   mode=2;
   assert(anv_cubit_install_discovery(i,inventory,2)==VK_ERROR_INITIALIZATION_FAILED);
   assert(!i->cubit_discovery && retained==released && allocated==freed);
   mode=0;
   fail_retain_after=2;
   assert(anv_cubit_install_discovery(i,inventory,2)==VK_ERROR_INITIALIZATION_FAILED);
   assert(!i->cubit_discovery && retained==released && allocated==freed);
   assert(anv_cubit_install_discovery(i,inventory,2)==VK_SUCCESS);
   memset(inventory,0,sizeof(inventory));
   assert(anv_cubit_install_discovery(i,&p,1)==VK_ERROR_INITIALIZATION_FAILED);
   mode=4;
   assert(anv_cubit_enumerate_physical_devices(&i->vk)==VK_ERROR_INITIALIZATION_FAILED);
   assert(list_is_empty(&i->vk.physical_devices.list) && retained==released+2);
   mode=0;
   fail_query_after=2;
   assert(anv_cubit_enumerate_physical_devices(&i->vk)==VK_ERROR_INITIALIZATION_FAILED);
   assert(list_is_empty(&i->vk.physical_devices.list) && retained==released+2);
   assert(anv_cubit_enumerate_physical_devices(&i->vk)==VK_SUCCESS);
   unsigned found=0;
   list_for_each_entry_safe(struct vk_physical_device,dev,&i->vk.physical_devices.list,link) {
      found++; list_del(&dev->link); anv_physical_device_destroy(dev);
   }
   assert(found==2 && retained==released+2);
   anv_cubit_finish_discovery(i);
   anv_cubit_finish_discovery(i);
   assert(retained==released && allocated==freed);
   assert(anv_cubit_install_discovery(i,NULL,0)==VK_SUCCESS);
   assert(anv_cubit_enumerate_physical_devices(&i->vk)==VK_SUCCESS);
   assert(list_is_empty(&i->vk.physical_devices.list));
   anv_cubit_finish_discovery(i);
   /* Fail every acquisition position, including the third device after two
    * successful constructions. Nothing from a failed transaction may escape
    * to the public list. Inventory pins survive for a later retry; temporary
    * physical objects and their pins must not. These are hosted mocks, not
    * actual hardware query or GPU recovery tests.
    */
   unsigned rollback_cases = 0;
   for (enum fault_stage stage = ALLOCATION; stage <= WSI_INIT; stage++) {
      const unsigned positions = stage == ALLOCATION ? 6 : 3;
      for (unsigned position = 1; position <= positions; position++) {
         struct anv_cubit_provider three[] = {p, p, p};
         assert(anv_cubit_install_discovery(i, three, 3) == VK_SUCCESS);
         const unsigned baseline_allocations = allocated - freed;
         const unsigned baseline_pins = retained - released;
         const unsigned baseline_opened = opened;
         injected_stage = stage;
         injected_after = position;
         VkResult failure = anv_cubit_enumerate_physical_devices(&i->vk);
         assert(failure == (stage == ALLOCATION ? VK_ERROR_OUT_OF_HOST_MEMORY :
                                                 VK_ERROR_INITIALIZATION_FAILED));
         assert(injected_after == 0); /* Verify that the intended path ran. */
         assert(list_is_empty(&i->vk.physical_devices.list));
         assert(allocated - freed == baseline_allocations);
         assert(retained - released == baseline_pins);
         assert(opened == baseline_opened);
         assert(p_atomic_read(&shared_budget.used[0]) == 0);
         injected_stage = NO_FAULT;
         assert(anv_cubit_enumerate_physical_devices(&i->vk) == VK_SUCCESS);
         unsigned recovered = 0;
         list_for_each_entry_safe(struct vk_physical_device, dev,
                                  &i->vk.physical_devices.list, link) {
            recovered++;
            list_del(&dev->link);
            anv_physical_device_destroy(dev);
         }
         assert(recovered == 3);
         anv_cubit_finish_discovery(i);
         assert(retained == released && allocated == freed);
         rollback_cases++;
      }
   }
   assert(rollback_cases == 21);
   printf("Physical discovery rollback PASS: %u injected failures with retry/cleanup (hosted)\n",
          rollback_cases);
   mtx_destroy(&i->vk.physical_devices.mutex);
   assert(retained==released && allocated==freed);
   free(i);
   return 0;
}
