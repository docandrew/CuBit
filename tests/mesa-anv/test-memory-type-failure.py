#!/usr/bin/env python3
"""Exercise the real heap initializer's backend-error boundary with mock types."""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent
source = Path(sys.argv[1]).resolve()
text = (source / "src/intel/vulkan/anv_physical_device_common.c").read_text()
start = text.index("   result = backend->init_memory_types(device);")
end = text.index("   /* Some games", start)
branch = text[start:end]
fixture = r'''
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#define VK_SUCCESS 0
#define VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT 1
#define VK_MEMORY_PROPERTY_PROTECTED_BIT 2
#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))
struct anv_memory_type { unsigned propertyFlags, heapIndex; };
struct device {
  bool has_protected_contexts;
  struct { unsigned type_count; struct anv_memory_type types[32]; } memory;
};
static int injected;
static unsigned calls;
static int init_types(struct device *d) { (void)d; calls++; return injected; }
static int run(struct device *device) {
  struct { int (*init_memory_types)(struct device *); } ops = { init_types };
  const typeof(ops) *backend = &ops;
  int result;
BRANCH
  return VK_SUCCESS;
}
int main(void) {
  const int results[] = {0, -1, -2, -3, -9};
  for (unsigned r = 0; r < ARRAY_SIZE(results); r++)
    for (unsigned protected = 0; protected < 2; protected++)
      for (unsigned count = 0; count <= 3; count += 3) {
        struct device d = {0};
        d.has_protected_contexts = protected;
        d.memory.type_count = count;
        struct device before = d;
        injected = results[r]; calls = 0;
        assert(run(&d) == injected && calls == 1);
        if (injected != 0) assert(memcmp(&before, &d, sizeof d) == 0);
        else assert(d.memory.type_count == count + protected);
      }
  puts("Memory-type failure boundary PASS: 20 mock backend cases");
}
'''
output = Path(tempfile.mkdtemp(prefix="memory-type-failure.", dir=root / "target"))
for name, code in (("current", branch), ("late-error-mutation", branch.replace(
        "   if (result != VK_SUCCESS)\n      return result;\n", "", 1))):
    c = output / (name + ".c")
    exe = output / name
    c.write_text(fixture.replace("BRANCH", code))
    subprocess.run(["cc", "-std=gnu11", "-fsanitize=undefined",
                    "-fno-sanitize-recover=all", str(c), "-o", str(exe)], check=True)
    result = subprocess.run([str(exe)], capture_output=True, text=True)
    if (result.returncode == 0) != (name == "current"):
        raise SystemExit("Unexpected fixture result: " + name + result.stderr)
    print(result.stdout.strip() if name == "current" else "Late-error mutation rejected")
print("Fixtures:", output)
