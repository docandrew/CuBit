#!/usr/bin/env python3
"""Exercise shared Mesa base lifetime, not native transport/device admission."""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent
source = Path(sys.argv[1]).resolve()
text = (source / "src/intel/vulkan/anv_physical_device_common.c").read_text()
start = text.index("VkResult\nanv_physical_device_alloc(")
end = text.index("\nvoid\nanv_physical_device_finish_common", start)
functions = text[start:end]
fixture = r'''
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
typedef int VkResult;
enum { VK_SUCCESS = 0, VK_ERROR_OUT_OF_HOST_MEMORY = -1,
       VK_ERROR_INITIALIZATION_FAILED = -3,
       VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = 2 };
struct vk_instance { int alloc; };
struct vk_physical_device { bool initialized; };
struct anv_instance { struct vk_instance vk; };
struct anv_physical_device;
struct anv_kmd_backend { void (*finish_physical)(struct anv_physical_device *); };
struct anv_physical_device {
   struct vk_physical_device vk; struct anv_instance *instance;
   const struct anv_kmd_backend *kmd_backend;
};
struct vk_physical_device_dispatch_table { unsigned entries; };
static int anv_physical_device_entrypoints, wsi_physical_device_entrypoints;
static unsigned allocations, releases, initializations, finishes, dispatches;
static unsigned mode;
static struct anv_instance instance;
static void finish_physical(struct anv_physical_device *d) { (void)d; }
static const struct anv_kmd_backend backend = {finish_physical};
static struct anv_physical_device *allocated;
#define vk_error(instance, result) (result)
static void *vk_zalloc(int *alloc, size_t size, unsigned align, int scope) {
   assert(alloc == &instance.vk.alloc && size == sizeof(*allocated));
   assert(align == 8 && scope == VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE);
   allocations++;
   if (mode == 1) return NULL;
   allocated = calloc(1, size); assert(allocated); return allocated;
}
static void vk_free(int *alloc, void *p) {
   assert(alloc == &instance.vk.alloc && p == allocated);
   releases++; free(p); allocated = NULL;
}
static void vk_physical_device_dispatch_table_from_entrypoints(
   struct vk_physical_device_dispatch_table *table, int *entries, bool overwrite) {
   if (dispatches == 0) {
      assert(overwrite && entries == &anv_physical_device_entrypoints);
      table->entries = 1;
   } else {
      assert(!overwrite && entries == &wsi_physical_device_entrypoints);
      assert(table->entries == 1); table->entries = 2;
   }
   dispatches++;
}
static int vk_physical_device_init(struct vk_physical_device *d,
   struct vk_instance *i, void *a, void *b, void *c,
   struct vk_physical_device_dispatch_table *table) {
   assert(d == &allocated->vk && i == &instance.vk && !a && !b && !c);
   assert(table->entries == 2); initializations++;
   if (mode == 2) return -99;
   d->initialized = true; return VK_SUCCESS;
}
static void vk_physical_device_finish(struct vk_physical_device *d) {
   assert(d == &allocated->vk && d->initialized);
   assert(allocated->instance == &instance);
   d->initialized = false; finishes++;
}
FUNCTIONS
int main(void) {
   struct anv_physical_device sentinel;
   struct anv_physical_device *rejected = &sentinel;
   assert(anv_physical_device_alloc(&instance, NULL, &rejected) == VK_ERROR_INITIALIZATION_FAILED);
   assert(rejected == &sentinel && allocations == 0 && dispatches == 0);
   const struct anv_kmd_backend incomplete = {NULL};
   assert(anv_physical_device_alloc(&instance, &incomplete, &rejected) == VK_ERROR_INITIALIZATION_FAILED);
   assert(rejected == &sentinel && allocations == 0 && dispatches == 0);
   for (mode = 0; mode < 3; mode++) {
      allocations = releases = initializations = finishes = dispatches = 0;
      struct anv_physical_device *out = &sentinel;
      int result = anv_physical_device_alloc(&instance, &backend, &out);
      assert(allocations == 1);
      if (mode == 0) {
         assert(result == VK_SUCCESS && out == allocated);
         assert(out->instance == &instance && out->vk.initialized);
         assert(out->kmd_backend == &backend);
         assert(releases == 0 && finishes == 0);
         anv_physical_device_free(out);
         assert(releases == 1 && finishes == 1);
      } else {
         assert(result == (mode == 1 ? VK_ERROR_OUT_OF_HOST_MEMORY : -99));
         assert(out == &sentinel && finishes == 0);
         assert(releases == (mode == 2));
      }
      assert(initializations == (mode != 1));
      assert(dispatches == (mode == 1 ? 0 : 2));
      assert(!allocated);
   }
   puts("Physical base PASS: success, allocation failure, initialization failure");
}
'''
output = Path(tempfile.mkdtemp(prefix="physical-base.", dir=root / "target"))
mutation = functions.replace("vk_free(&instance->vk.alloc, device);", "/* omitted cleanup */", 1)
assert mutation != functions
for name, code in (("current", functions), ("leak-mutation", mutation)):
    unit, binary = output / (name + ".c"), output / name
    unit.write_text(fixture.replace("FUNCTIONS", code))
    subprocess.run(["cc", "-std=c11", "-Wall", "-Wextra", "-Werror",
                    "-fsanitize=undefined", "-fno-sanitize-recover=all",
                    str(unit), "-o", str(binary)], check=True)
    result = subprocess.run([str(binary)], capture_output=True, text=True)
    if (result.returncode == 0) != (name == "current"):
        raise SystemExit("Unexpected lifetime result: " + name + result.stderr)
    print(result.stdout.strip() if name == "current" else "Leaked-base mutation rejected")
print("Fixtures:", output)
