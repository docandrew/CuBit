#!/usr/bin/env python3
"""Run the common constructor's actual admission code with mocked metadata."""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent
source = Path(sys.argv[1]).resolve()
text = (source / "src/intel/vulkan/anv_physical_device_common.c").read_text()
start = text.index("   if (devinfo.ver < 9)")
end = text.index("   device->info = devinfo;", start)
branch = text[start:end]
fixture = r'''
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#define VK_SUCCESS 0
#define VK_ERROR_INCOMPATIBLE_DRIVER -9
#define INTEL_WA_16013994831 0
#define BITSET_CLEAR(bits, bit) ((bits) &= ~(1u << (bit)))
static int vk_errorf(void *instance, int result, const char *fmt, ...) {
    (void)instance; (void)fmt; return result;
}
static int admit(unsigned version, bool forced, bool isolated,
                 unsigned *workarounds) {
    struct {
        unsigned ver, verx10, workarounds;
        bool probe_forced, has_context_isolation;
        const char *name;
    } devinfo = {version, version * 10, *workarounds, forced, isolated, "mock"};
    void *instance = NULL;
    int result;
BRANCH
    result = VK_SUCCESS;
fail_base:
    *workarounds = devinfo.workarounds;
    return result;
}
int main(void) {
    unsigned count = 0;
    for (unsigned version = 0; version <= 35; version++)
      for (unsigned forced = 0; forced < 2; forced++)
        for (unsigned isolated = 0; isolated < 2; isolated++)
          for (unsigned initial = 2; initial <= 3; initial++) {
            unsigned wa = initial;
            int result = admit(version, forced, isolated, &wa);
            bool accepted = version >= 9 && (version <= 30 || forced) && isolated;
            assert(result == (accepted ? VK_SUCCESS : VK_ERROR_INCOMPATIBLE_DRIVER));
            assert(wa == (version == 12 ? (initial & ~1u) : initial));
            count++;
          }
    printf("Common device admission PASS: %u mock metadata cases\n", count);
}
'''
output = Path(tempfile.mkdtemp(prefix="device-admission.", dir=root / "target"))
for name, code in (("current", branch), ("isolation-mutation", branch.replace(
        "!devinfo.has_context_isolation", "false", 1))):
    unit, binary = output / (name + ".c"), output / name
    unit.write_text(fixture.replace("BRANCH", code))
    subprocess.run(["cc", "-std=c11", "-Wall", "-Wextra", "-Werror",
                    "-fsanitize=undefined", "-fno-sanitize-recover=all",
                    str(unit), "-o", str(binary)], check=True)
    result = subprocess.run([str(binary)], capture_output=True, text=True)
    if (result.returncode == 0) != (name == "current"):
        raise SystemExit("Unexpected admission result: " + name + result.stderr)
    print(result.stdout.strip() if name == "current" else "Isolation-bypass mutation rejected")
print("Fixtures:", output)
