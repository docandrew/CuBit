#!/usr/bin/env python3
"""Exercise real common queue dispatch using mock backend operations."""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent
source = Path(sys.argv[1]).resolve()
text = (source / "src/intel/vulkan/anv_queue.c").read_text()
start = text.index("static VkResult\nanv_create_engine")
end = text.index("\nVkResult\nanv_queue_init", start)
functions = text[start:end]
fixture = r'''
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
typedef int VkResult;
typedef struct { int value; } VkDeviceQueueCreateInfo;
enum { VK_SUCCESS = 0, VK_ERROR_INITIALIZATION_FAILED = -3 };
struct anv_device;
struct anv_queue;
struct anv_kmd_backend {
   VkResult (*create_engine)(struct anv_device *, struct anv_queue *, const VkDeviceQueueCreateInfo *);
   void (*destroy_engine)(struct anv_device *, struct anv_queue *);
};
struct anv_device { const struct anv_kmd_backend *kmd_backend; };
struct anv_queue { struct anv_device *device; };
static unsigned creates, destroys;
static int injected;
static VkDeviceQueueCreateInfo info = {42};
static int create(struct anv_device *d, struct anv_queue *q, const VkDeviceQueueCreateInfo *ci) {
   assert(q->device == d && ci == &info); creates++; return injected;
}
static void destroy(struct anv_device *d, struct anv_queue *q) {
   assert(q->device == d); destroys++;
}
FUNCTIONS
int main(void) {
   for (unsigned bits = 0; bits < 8; bits++)
      for (unsigned fail = 0; fail < 2; fail++) {
         struct anv_kmd_backend ops = {bits & 2 ? create : NULL, bits & 4 ? destroy : NULL};
         struct anv_device d = {bits & 1 ? &ops : NULL};
         struct anv_queue q = {&d};
         creates = destroys = 0; injected = fail ? -2 : VK_SUCCESS;
         int result = anv_create_engine(&d, &q, &info);
         bool admitted = bits == 7;
         assert(result == (admitted ? injected : VK_ERROR_INITIALIZATION_FAILED));
         assert(creates == admitted && destroys == 0);
         if (admitted && !fail) {
            anv_destroy_engine(&q);
            assert(destroys == 1);
         }
      }
   puts("Queue backend PASS: 16 callback/error cases");
}
'''
output = Path(tempfile.mkdtemp(prefix="queue-backend.", dir=root / "target"))
for name, code in (("current", functions), ("missing-cleanup-mutation", functions.replace(
        " ||\n       backend->destroy_engine == NULL", "", 1))):
    unit, binary = output / (name + ".c"), output / name
    unit.write_text(fixture.replace("FUNCTIONS", code))
    subprocess.run(["cc", "-std=c11", "-Wall", "-Wextra", "-Werror",
                    "-fsanitize=undefined", "-fno-sanitize-recover=all",
                    str(unit), "-o", str(binary)], check=True)
    result = subprocess.run([str(binary)], capture_output=True, text=True)
    if (result.returncode == 0) != (name == "current"):
        raise SystemExit("Unexpected queue result: " + name + result.stderr)
    print(result.stdout.strip() if name == "current" else "Missing-cleanup mutation rejected")
print("Fixtures:", output)
