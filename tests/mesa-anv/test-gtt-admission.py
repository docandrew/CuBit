#!/usr/bin/env python3
"""Exercise the actual constructor's GTT rejection branch, with mocked logging."""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent
source = Path(sys.argv[1]).resolve()
text = (source / "src/intel/vulkan/anv_physical_device_common.c").read_text()
start = text.index("   if (device->gtt_size < (4ULL << 30 /* GiB */)) {")
end = text.index("\n   }", start) + len("\n   }")
branch = text[start:end]
output = Path(tempfile.mkdtemp(prefix="gtt-admission.", dir=root / "target"))
fixture = r'''
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
enum { VK_SUCCESS = 0, VK_ERROR_INCOMPATIBLE_DRIVER = -9 };
static unsigned logged;
static int vk_errorf(void *instance, int result, const char *fmt, ...)
{ (void)instance; (void)fmt; logged++; return result; }
static int admit(uint64_t size, bool *cleaned)
{
   struct { uint64_t gtt_size; } storage = {size}, *device = &storage;
   void *instance = NULL;
   int result = VK_SUCCESS;
   *cleaned = false;
BRANCH
   return VK_SUCCESS;
fail_base:
   *cleaned = true;
   return result;
}
int main(void)
{
   const uint64_t sizes[] = {0, 1, (4ULL << 30) - 1, 4ULL << 30,
                            (4ULL << 30) + 1, 1ULL << 48};
   for (unsigned i = 0; i < sizeof sizes / sizeof sizes[0]; i++) {
      bool cleaned;
      logged = 0;
      int result = admit(sizes[i], &cleaned);
      bool reject = sizes[i] < (4ULL << 30);
      assert(cleaned == reject && logged == reject);
      assert(result == (reject ? VK_ERROR_INCOMPATIBLE_DRIVER : VK_SUCCESS));
   }
   puts("GTT admission PASS: rejection returns failure after cleanup (mock branch harness)");
}
'''
def run_case(name, code):
    unit = output / (name + ".c")
    unit.write_text(fixture.replace("BRANCH", code))
    binary = output / name
    subprocess.run(["cc", "-std=c11", "-O2", "-Wall", "-Wextra", "-Werror",
                    "-fsanitize=undefined", "-fno-sanitize-recover=all",
                    str(unit), "-o", str(binary)], check=True)
    return subprocess.run([str(binary)], capture_output=True, text=True)

correct = run_case("correct", branch)
assert correct.returncode == 0, correct.stderr
print(correct.stdout.strip())
# Ensure the test detects the exact prior bug, not just reaching the error path.
old = branch.replace("result = vk_errorf", "vk_errorf", 1)
assert old != branch
mutant = run_case("old-bug", old)
assert mutant.returncode != 0, "Test failed to detect the prior success-on-error bug"
print("Prior bug mutation rejected; fixture:", output)
