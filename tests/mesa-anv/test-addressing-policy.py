#!/usr/bin/env python3
"""Differential test: extracted backend policy versus pristine Mesa constructor."""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent
source = Path(sys.argv[1]).resolve()
pristine = Path("/nix/store/6pnvm1jkh0a144pmcsbkykq5crhn695a-source")
original = (pristine / "src/intel/vulkan/anv_physical_device.c").read_text()
start = original.index("   device->uses_relocs =")
end = original.index("   device->always_flush_cache =", start)
reference = original[start:end]
adapted = (source / "src/intel/vulkan/anv_gem.c").read_text()
start = adapted.index("void\nanv_drm_init_addressing(")
end = adapted.index("\n}\n", start) + 3
implementation = adapted[start:end]
fixture = r'''
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
enum { INTEL_KMD_TYPE_I915 = 1, INTEL_KMD_TYPE_XE = 2 };
enum { ANV_SPARSE_TYPE_NOT_SUPPORTED, ANV_SPARSE_TYPE_TRTT,
       ANV_SPARSE_TYPE_VM_BIND, ANV_SPARSE_TYPE_FAKE };
static bool debug_NO_SPARSE, debug_SPARSE_TRTT;
#define ANV_DEBUG(flag) debug_##flag
struct anv_instance { struct { struct { bool fake_sparse; } features; } drirc; };
struct anv_physical_device {
   struct { unsigned ver, kmd_type; } info;
   struct anv_instance *instance;
   bool uses_relocs;
   int sparse_type;
};
IMPLEMENTATION
static void reference(struct anv_physical_device *device)
{
   struct anv_instance *instance = device->instance;
REFERENCE
}
int main(void)
{
   unsigned cases = 0;
   for (unsigned ver = 9; ver <= 35; ver++)
      for (unsigned kmd = INTEL_KMD_TYPE_I915; kmd <= INTEL_KMD_TYPE_XE; kmd++)
         for (unsigned options = 0; options < 8; options++) {
            struct anv_instance instance = {0};
            debug_NO_SPARSE = options & 1;
            debug_SPARSE_TRTT = options & 2;
            instance.drirc.features.fake_sparse = options & 4;
            struct anv_physical_device actual = {
               .info = {ver, kmd}, .instance = &instance,
            }, expected = actual;
            reference(&expected);
            anv_drm_init_addressing(&actual);
            assert(actual.uses_relocs == expected.uses_relocs);
            assert(actual.sparse_type == expected.sparse_type);
            cases++;
         }
   printf("Linux addressing policy matches pristine Mesa: %u cases\n", cases);
}
'''
output = Path(tempfile.mkdtemp(prefix="addressing-policy.", dir=root / "target"))
unit = output / "test.c"
unit.write_text(fixture.replace("IMPLEMENTATION", implementation).replace("REFERENCE", reference))
subprocess.run(["cc", "-std=c11", "-O2", "-Wall", "-Wextra", "-Werror",
                "-fsanitize=undefined", "-fno-sanitize-recover=all", str(unit),
                "-o", str(output / "test")], check=True)
subprocess.run([str(output / "test")], check=True)
print("Hosted policy fixture (not GPU execution):", output)
