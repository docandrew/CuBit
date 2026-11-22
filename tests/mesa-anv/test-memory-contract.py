#!/usr/bin/env python3
"""Exercise the actual common memory publication gate, using mock types."""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent
source = Path(sys.argv[1]).resolve()
text = (source / 'src/intel/vulkan/anv_physical_device_common.c').read_text()
start = text.index('   bool host_coherent = false, device_local = false;')
end = text.index('\n}', start)
branch = text[start:end]
fixture = r'''
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#define SUPPORT_INTEL_INTEGRATED_GPUS 1
#define VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT 1
#define VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT 2
#define VK_MEMORY_PROPERTY_HOST_COHERENT_BIT 4
#define VK_SUCCESS 0
#define VK_ERROR_INITIALIZATION_FAILED -3
typedef unsigned VkMemoryPropertyFlags;
struct device {
 struct { unsigned type_count;
          struct { unsigned propertyFlags; } types[2];
          bool need_flush; } memory;
};
static int vk_errorf(struct device *d, int result, const char *message) {
 (void)d; (void)message; return result;
}
static int publish(struct device *device) {
BRANCH
}
int main(void) {
 unsigned cases = 0;
 for (unsigned count=0; count<=2; count++)
  for (unsigned a=0; a<16; a++)
   for (unsigned b=0; b<16; b++) {
    struct device d = {.memory={.type_count=count, .types={{a},{b}}}};
    bool local=(count>0 && (a&1)) || (count>1 && (b&1));
    bool coherent=(count>0 && (a&6)==6) || (count>1 && (b&6)==6);
    assert(publish(&d)==((local && coherent) ? 0 : -3));
    cases++;
   }
 printf("Memory publication contract PASS: %u mock flag sets\n", cases);
}
'''
out = Path(tempfile.mkdtemp(prefix='memory-contract.', dir=root / 'target'))
for name, code in [('current', branch), ('bypass', branch.replace(
        'if (!host_coherent || !device_local)',
        'if ((!host_coherent || !device_local) && false)'))]:
    unit, executable = out / (name + '.c'), out / name
    unit.write_text(fixture.replace('BRANCH', code))
    subprocess.run(['cc', '-std=c11', '-Wall', '-Wextra', '-Werror',
                    '-fsanitize=undefined', '-fno-sanitize-recover=all',
                    str(unit), '-o', str(executable)], check=True)
    result = subprocess.run([str(executable)], capture_output=True, text=True)
    if (result.returncode == 0) != (name == 'current'):
        raise SystemExit('Unexpected contract result: ' + name + result.stderr)
    print(result.stdout.strip() if name == 'current' else 'Gate bypass rejected')
print('Hosted evidence (not hardware coherence):', out)
