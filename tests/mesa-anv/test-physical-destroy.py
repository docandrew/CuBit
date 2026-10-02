#!/usr/bin/env python3
"""Exercise Mesa's shared destructor ordering without a DRM transport."""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent
source = Path(sys.argv[1]).resolve()
text = (source / "src/intel/vulkan/anv_physical_device_common.c").read_text()
start = text.index("void\nanv_physical_device_destroy(")
end = text.index("\nVkResult\nanv_physical_device_init_common", start)
function = text[start:end]
fixture = r'''
#include <assert.h>
#include <stddef.h>
#include <stdio.h>
struct vk_physical_device { int sentinel; };
struct anv_physical_device;
struct anv_kmd_backend { void (*finish_physical)(struct anv_physical_device *); };
struct anv_physical_device {
   int sentinel;
   struct vk_physical_device vk;
   const struct anv_kmd_backend *kmd_backend;
};
#define container_of(p, type, member) ((type *)((char *)(p) - offsetof(type, member)))
static struct anv_physical_device *expected;
static unsigned stage;
static void step(struct anv_physical_device *d, unsigned s) {
   assert(d == expected && d->sentinel == 17 && d->vk.sentinel == 29);
   assert(stage == s); stage++;
}
static void anv_finish_wsi(struct anv_physical_device *d) { step(d, 0); }
static void anv_measure_device_destroy(struct anv_physical_device *d) { step(d, 1); }
static void anv_physical_device_finish_common(struct anv_physical_device *d) { step(d, 2); }
static void finish_transport(struct anv_physical_device *d) { step(d, 3); }
static void anv_physical_device_free(struct anv_physical_device *d) { step(d, 4); }
FUNCTION
int main(void) {
   const struct anv_kmd_backend backend = {finish_transport};
   struct anv_physical_device device = {17, {29}, &backend};
   expected = &device;
   anv_physical_device_destroy(&device.vk);
   assert(stage == 5);
   puts("Physical destroy PASS: WSI/measurement/common/backend/base order");
}
'''
out = Path(tempfile.mkdtemp(prefix="physical-destroy.", dir=root / "target"))
for name, body in [("actual", function), ("missing-backend", function.replace(
        "device->kmd_backend->finish_physical(device);", ""))]:
    unit = out / (name + ".c")
    unit.write_text(fixture.replace("FUNCTION", body))
    binary = out / name
    subprocess.run(["cc", "-std=c11", "-Wall", "-Wextra", "-Werror", str(unit), "-o", str(binary)], check=True)
    result = subprocess.run([str(binary)], capture_output=True, text=True)
    if (result.returncode == 0) != (name == "actual"):
        raise SystemExit(f"Unexpected destruction result: {name}")
    print(result.stdout.strip() if name == "actual" else "Missing-backend mutation rejected")
print("Fixtures:", out)
