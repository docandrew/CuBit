#!/usr/bin/env python3
"""Test actual ANV export/tiling dispatchers with mock backend operations.

Hosted control-flow regression only: no BO import, native transport or GPU.
"""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent
source = Path(sys.argv[1]).resolve()
text = (source / "src/intel/vulkan/anv_allocator.c").read_text()
start = text.index("VkResult\nanv_device_export_bo(")
end = text.index("\nstatic bool\natomic_dec_not_one", start)
functions = text[start:end]
fixture = r'''
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
typedef int VkResult;
enum { VK_SUCCESS = 0, VK_ERROR_INVALID_EXTERNAL_HANDLE = -1,
       VK_ERROR_FEATURE_NOT_PRESENT = -2, VK_ERROR_TOO_MANY_OBJECTS = -3 };
enum isl_tiling { LINEAR, TILED };
struct anv_device;
struct anv_bo { uint32_t gem_handle; };
struct anv_kmd_backend {
   int (*export_bo_fd)(struct anv_device *, uint32_t);
   VkResult (*get_bo_tiling)(struct anv_device *, struct anv_bo *, enum isl_tiling *);
   VkResult (*set_bo_tiling)(struct anv_device *, struct anv_bo *, uint32_t, enum isl_tiling);
};
struct anv_device { const struct anv_kmd_backend *kmd_backend; };
static struct anv_bo object = {42};
static struct anv_device *expected_device;
static unsigned calls;
static bool fail;
#define vk_error(device, error) (error)
static struct anv_bo *anv_device_lookup_bo(struct anv_device *d, uint32_t h) {
   assert(d == expected_device && h == 42); return &object;
}
static bool anv_bo_is_external(struct anv_bo *bo) { return bo == &object; }
static int export_fd(struct anv_device *d, uint32_t h) {
   assert(d == expected_device && h == 42); calls++; return fail ? -1 : 71;
}
static VkResult get_tiling(struct anv_device *d, struct anv_bo *bo, enum isl_tiling *out) {
   assert(d == expected_device && bo == &object); calls++;
   if (fail) return -99;
   *out = TILED; return VK_SUCCESS;
}
static VkResult set_tiling(struct anv_device *d, struct anv_bo *bo, uint32_t pitch, enum isl_tiling t) {
   assert(d == expected_device && bo == &object && pitch == 256 && t == TILED);
   calls++; return fail ? -99 : VK_SUCCESS;
}
FUNCTIONS
int main(void) {
   for (unsigned bits = 0; bits < 16; bits++)
      for (unsigned failure = 0; failure < 2; failure++) {
         struct anv_kmd_backend ops = {
            bits & 2 ? export_fd : NULL,
            bits & 4 ? get_tiling : NULL,
            bits & 8 ? set_tiling : NULL,
         };
         struct anv_device d = {bits & 1 ? &ops : NULL};
         expected_device = &d; fail = failure;
         bool export_ok = (bits & 3) == 3;
         bool get_ok = (bits & 5) == 5;
         bool set_ok = (bits & 9) == 9;
         int fd = -777; enum isl_tiling tiling = LINEAR;
         calls = 0;
         assert(anv_device_export_bo(&d, &object, &fd) ==
                (!export_ok ? VK_ERROR_INVALID_EXTERNAL_HANDLE :
                 fail ? VK_ERROR_TOO_MANY_OBJECTS : VK_SUCCESS));
         assert(calls == export_ok && fd == (export_ok && !fail ? 71 : -777));
         calls = 0;
         assert(anv_device_get_bo_tiling(&d, &object, &tiling) ==
                (!get_ok ? VK_ERROR_INVALID_EXTERNAL_HANDLE : fail ? -99 : VK_SUCCESS));
         assert(calls == get_ok && tiling == (get_ok && !fail ? TILED : LINEAR));
         calls = 0;
         assert(anv_device_set_bo_tiling(&d, &object, 256, TILED) ==
                (!set_ok ? VK_ERROR_FEATURE_NOT_PRESENT : fail ? -99 : VK_SUCCESS));
         assert(calls == set_ok);
      }
   puts("Allocator dispatch PASS: 32 callback/error combinations, three operations each");
}
'''
output = Path(tempfile.mkdtemp(prefix="allocator-dispatch.", dir=root / "target"))
mutation = functions.replace("*fd_out = fd;", "*fd_out = 0;", 1)
assert mutation != functions
for name, code in (("current", functions), ("wrong-fd-mutation", mutation)):
    unit, binary = output / (name + ".c"), output / name
    unit.write_text(fixture.replace("FUNCTIONS", code))
    subprocess.run(["cc", "-std=c11", "-Wall", "-Wextra", "-Werror",
                    "-fsanitize=undefined", "-fno-sanitize-recover=all",
                    str(unit), "-o", str(binary)], check=True)
    result = subprocess.run([str(binary)], capture_output=True, text=True)
    if (result.returncode == 0) != (name == "current"):
        raise SystemExit("Unexpected allocator result: " + name + result.stderr)
    print(result.stdout.strip() if name == "current" else "Wrong-FD mutation rejected")
print("Fixtures:", output)
