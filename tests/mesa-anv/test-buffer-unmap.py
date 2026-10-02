#!/usr/bin/env python3
"""Exercise the actual patched Mesa unmap function; no GPU is emulated."""
from pathlib import Path
import subprocess
import sys
import tempfile

source = Path(sys.argv[1])
text = (source / 'src/intel/vulkan/anv_allocator.c').read_text()
start = text.index('VkResult\nanv_device_unmap_bo(')
end = text.index('\nVkResult\nanv_device_import_bo_from_host_ptr(', start)
function = text[start:end]
prefix = r'''
#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <sys/mman.h>
typedef int VkResult;
#define VK_SUCCESS 0
#define VK_ERROR_MEMORY_MAP_FAILED -5
#define ROUND_DOWN_TO(x,a) ((x) & ~((a)-1))
#define vk_error(d,e) (e)
#define vk_errorf(d,e,...) (e)
#define VG(x) x
#define VALGRIND_FREELIKE_BLOCK(p,n) (++freed)
struct anv_device;
struct anv_bo { bool from_host_ptr; uint64_t offset; struct anv_bo *real; };
struct anv_kmd_backend {
 VkResult (*unmap_bo)(struct anv_device *,struct anv_bo *,void *,size_t,bool);
};
struct physical { uint64_t page_size; };
struct anv_device { struct physical *physical; const struct anv_kmd_backend *kmd_backend; };
static struct anv_bo *anv_bo_get_real(struct anv_bo *b) { return b->real ? b->real : b; }
static int freed, maps, unmaps, calls, response;
static bool map_failure, expected_replace;
static void *expected_map;
static size_t expected_size;
static struct anv_bo *expected_bo;
static void *fake_mmap(void *p,size_t n,int prot,int flags,int fd, long off) {
 assert(p==expected_map && n==expected_size && prot==PROT_NONE);
 assert(flags==(MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED) && fd==-1 && off==0);
 ++maps; return map_failure ? MAP_FAILED : p;
}
static int fake_munmap(void *p,size_t n) {
 assert(p==expected_map && n==expected_size); ++unmaps; return 0;
}
#define mmap fake_mmap
#define munmap fake_munmap
static VkResult callback(struct anv_device *d,struct anv_bo *b,void *p,size_t n,bool r) {
 assert(d && b==expected_bo && p==expected_map && n==expected_size && r==expected_replace);
 ++calls; return response;
}
'''
suffix = r'''
int main(void) {
 struct physical phys={4096};
 struct anv_kmd_backend backend={callback};
 struct anv_device device={&phys,&backend};
 struct anv_bo real={0}, slab={.offset=128,.real=&real};
 for (int sub=0;sub<2;sub++) for(int replace=0;replace<2;replace++)
 for(int fail=0;fail<2;fail++) {
  expected_bo=sub?&slab:&real; expected_map=(void *)(uintptr_t)0x10000;
  expected_size=4096; expected_replace=replace; response=fail?-7:0;
  freed=maps=unmaps=calls=0;
  assert(anv_device_unmap_bo(&device,expected_bo,
    (char *)expected_map+(sub?128:0),4096-(sub?128:0),replace)==response);
  assert(calls==1 && maps==0 && unmaps==0 && freed==(!fail&&!replace));
 }
 for(int absent=0;absent<2;absent++) for(int replace=0;replace<2;replace++)
 for(int fail=0;fail<2;fail++) {
  backend.unmap_bo=0; device.kmd_backend=absent?0:&backend;
  map_failure=fail; freed=maps=unmaps=calls=0;
  int result=anv_device_unmap_bo(&device,&real,expected_map,4096,replace);
#ifdef __cubit__
  assert(result==VK_ERROR_MEMORY_MAP_FAILED && maps==0 && unmaps==0 && freed==0);
#else
  assert(result==((replace&&fail)?VK_ERROR_MEMORY_MAP_FAILED:0));
  assert(maps==replace && unmaps==!replace && freed==!replace);
#endif
  assert(calls==0);
 }
 return 0;
}
'''
output = Path(tempfile.mkdtemp(prefix='buffer-unmap.', dir=Path(__file__).parent / 'target'))
def run(body, native, name):
    path = output / (name + '.c')
    path.write_text(prefix + body + suffix)
    exe = output / name
    subprocess.run(['cc', '-std=gnu11', '-Wall', '-Wextra', '-Werror',
                    '-Wno-unused-function', '-fsanitize=undefined',
                    '-fno-sanitize-recover=all', *(['-D__cubit__'] if native else []),
                    str(path), '-o', str(exe)], check=True)
    return subprocess.run([str(exe)], capture_output=True).returncode
for native in (False, True):
    assert run(function, native, 'native' if native else 'linux') == 0
assert run(function.replace('#if defined(__cubit__)', '#if 0'), True, 'missing-guard') != 0
assert run(function.replace('return result;', 'return VK_SUCCESS;'), True, 'ignored-failure') != 0
print('PASS: actual Mesa unmap dispatch, slab adjustment, failure propagation, Linux fallback; two mutations rejected')
print(output)
