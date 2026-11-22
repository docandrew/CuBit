#!/usr/bin/env python3
"""Execute the real common teardown with ownership-checking mock allocators."""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent
source = Path(sys.argv[1]).resolve()
text = (source / "src/intel/vulkan/anv_physical_device_common.c").read_text()
start = text.index("void\nanv_physical_device_finish_common(")
end = text.index("\nstatic VkResult", start)
function = text[start:end]
fixture = r'''
#include <assert.h>
#include <stddef.h>
#include <stdio.h>
struct anv_physical_device { void *engine_info, *compiler, *cache; int transport; };
static int engine, compiler, cache;
static unsigned freed_engine, freed_compiler, freed_cache;
static void mock_free(void *p) {
   if (p) { assert(p == &engine && !freed_engine); freed_engine++; }
}
static void ralloc_free(void *p) {
   if (p) { assert(p == &compiler && !freed_compiler); freed_compiler++; }
}
static void anv_physical_device_free_disk_cache(struct anv_physical_device *d) {
   if (d->cache) { assert(d->cache == &cache && !freed_cache); freed_cache++; }
   d->cache = NULL;
}
#define free mock_free
FUNCTION
int main(void) {
   for (unsigned mask = 0; mask < 8; mask++) {
      freed_engine = freed_compiler = freed_cache = 0;
      struct anv_physical_device d = {
         mask & 1 ? &engine : NULL, mask & 2 ? &compiler : NULL,
         mask & 4 ? &cache : NULL, 42
      };
      anv_physical_device_finish_common(&d);
      anv_physical_device_finish_common(&d);
      assert(!d.engine_info && !d.compiler && !d.cache && d.transport == 42);
      assert(freed_engine == !!(mask & 1));
      assert(freed_compiler == !!(mask & 2));
      assert(freed_cache == !!(mask & 4));
   }
   puts("Common cleanup PASS: 8 partial states, repeat safe, transport retained");
}
'''
out = Path(tempfile.mkdtemp(prefix="physical-cleanup.", dir=root / "target"))
for name, body in [("actual", function),
                   ("stale-engine", function.replace("device->engine_info = NULL;", "")),
                   ("stale-compiler", function.replace("device->compiler = NULL;", ""))]:
    unit = out / (name + ".c")
    unit.write_text(fixture.replace("FUNCTION", body))
    binary = out / name
    subprocess.run(["cc", "-std=c11", "-Wall", "-Wextra", "-Werror", str(unit), "-o", str(binary)], check=True)
    result = subprocess.run([str(binary)], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    if (result.returncode == 0) != (name == "actual"):
        raise SystemExit(f"Unexpected cleanup test result: {name}: {result.returncode}")
    print(result.stdout.strip() if name == "actual" else f"Mutation rejected: {name}")
print("Fixtures:", out)
