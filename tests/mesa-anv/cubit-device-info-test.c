#include "cubit-device-query.h"
#include "intel/dev/intel_device_info.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static uint64_t frequency = 12000000;
static unsigned clock_fault;
static bool runtime_query;
static uint64_t vm_status;

static bool exchange(void *endpoint, const struct cubit_gpu_query_message *request,
                     struct cubit_gpu_query_message *reply)
{
   unsigned *calls = endpoint;
   assert(request->words[1] == (*calls == 3 && runtime_query ? 4 : *calls));
   (*calls)++;
   *reply = *request;
   reply->words[0] = 0;
   reply->words[1] = 1;
   if (*calls == 1) {
      reply->words[2] = 0x1746d28086;
      reply->words[3] = 0;
   } else if (*calls == 2) {
      /* Deliberately sparse physical DSS IDs, not an offline default. */
      reply->words[2] = 0x20;
      reply->words[3] = 0x33;
   } else if (*calls == 4) {
      assert(runtime_query);
      reply->words[0] = vm_status;
      reply->words[2] = vm_status ? 0 : 48;
      reply->words[3] = vm_status ? 0 : 1;
   } else {
      assert(*calls == 3);
      reply->words[2] = frequency;
      reply->words[3] = 0;
      switch (clock_fault) {
      case 1: return false;
      case 2: reply->words[0] = 1; break;
      case 3: reply->words[3] = 1; break;
      default: break;
      }
   }
   return true;
}
int main(void)
{
   unsetenv("INTEL_FORCE_PROBE");
   struct intel_device_info info = {0};
   unsigned calls = 0;
   assert(cubit_mesa_query_device_defaults(exchange, &calls, &info));
   assert(calls == 3 && info.kmd_type == INTEL_KMD_TYPE_INVALID);
   /* These Linux mmap capabilities also gate ANV's slab allocator. The current
    * native presentation path requires standalone backing, and must not gain
    * pooled/slab allocations merely from an upstream default change. */
   assert(!info.has_mmap_offset && !info.has_partial_mmap_offset);
   assert(info.timestamp_frequency == frequency);
   assert(info.pci_device_id == 0x46d2 && info.pci_revision_id == 0x17);
   assert(info.revision == 0x17);
   assert(info.subslice_total == 1 && info.subslice_masks[0] == 0x20);
   assert(intel_device_info_eu_total(&info) == 4);
   assert(intel_device_info_dual_subslice_id_bound(&info) == 6);
   assert(info.eu_masks[10] == 0x33 && info.eu_masks[0] == 0);
   /* Physical DSS 5 requires IDs through six DSS, not one enabled DSS.
    * Underallocating this scratch span can corrupt neighboring allocations. */
   assert(info.max_scratch_ids[MESA_SHADER_COMPUTE] == 6 * 16 * 8);
   assert(info.engine_class_prefetch[INTEL_ENGINE_CLASS_RENDER] == 512);
   /* Upstream's small-EU Gen12 geometry workaround must see measured EUs. */
   assert(info.urb.max_entries[MESA_SHADER_GEOMETRY] == 1024);
   struct intel_device_info before = info;
   const uint64_t frequencies[] = {0, 1025000001, UINT64_MAX, 1, 19200000, 1025000000};
   for (unsigned i = 0; i < sizeof(frequencies) / sizeof(frequencies[0]); i++) {
      frequency = frequencies[i];
      calls = 0;
      bool ok = cubit_mesa_query_device_defaults(exchange, &calls, &info);
      assert(calls == 3 && ok == (i >= 3));
      if (ok) {
         assert(info.timestamp_frequency == frequency);
         before = info;
      } else {
         assert(memcmp(&info, &before, sizeof(info)) == 0);
      }
   }
   for (clock_fault = 1; clock_fault <= 3; clock_fault++) {
      calls = 0;
      assert(!cubit_mesa_query_device_defaults(exchange, &calls, &info));
      assert(calls == 3 && memcmp(&info, &before, sizeof(info)) == 0);
   }
   clock_fault = 0;
   calls = 0;
   setenv("INTEL_FORCE_PROBE", "!46d2", 1);
   assert(!cubit_mesa_query_device_defaults(exchange, &calls, &info));
   assert(memcmp(&info, &before, sizeof(info)) == 0);
   unsetenv("INTEL_FORCE_PROBE");
   assert(!cubit_mesa_query_device_defaults(NULL, NULL, &info));
   assert(memcmp(&info, &before, sizeof(info)) == 0);
   runtime_query = true;
   calls = 0;
   assert(cubit_mesa_query_runtime_device(exchange, &calls, &info));
   assert(calls == 4 && info.has_context_isolation &&
          info.gtt_size == (UINT64_C(1) << 48) &&
          info.kmd_type == INTEL_KMD_TYPE_INVALID);
   assert(!info.has_mmap_offset && !info.has_partial_mmap_offset);
   before = info;
   vm_status = 2; calls = 0;
   assert(!cubit_mesa_query_runtime_device(exchange, &calls, &info));
   assert(calls == 4 && memcmp(&info, &before, sizeof(info)) == 0);
   assert(!cubit_mesa_query_runtime_device(NULL, NULL, &info));
   assert(!cubit_mesa_query_runtime_device(exchange, &calls, NULL));
   return 0;
}
