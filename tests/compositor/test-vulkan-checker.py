"""Build and execute descriptor-free checker on hosted Mesa lavapipe, in Nix."""
import hashlib
import json
import os
from pathlib import Path
import struct
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
assert os.environ.get("IN_NIX_SHELL"), "Use vulkan-affine-shell.nix"
work = Path(tempfile.mkdtemp(prefix="vulkan-checker-", dir=ROOT / "tests/compositor/build"))
print(work, flush=True)
inputs = {}
def read(relative):
    data = (ROOT / relative).read_bytes()
    inputs[relative] = hashlib.sha256(data).hexdigest()
    return data

header = ["#include <stdint.h>"]
for name, relative in (("vertex", "userspace/lib/compositor/vulkan_affine.vert"),
                       ("fragment", "userspace/lib/compositor/vulkan_checker.frag")):
    source = work / Path(relative).name
    source.write_bytes(read(relative))
    binary = source.with_suffix(source.suffix + ".spv")
    subprocess.run(["glslangValidator", "-V", "--target-env", "vulkan1.0", str(source), "-o", str(binary)], check=True)
    subprocess.run(["spirv-val", "--target-env", "vulkan1.0", str(binary)], check=True)
    data = binary.read_bytes()
    words = struct.unpack("<" + "I" * (len(data) // 4), data)
    header.append("static const uint32_t vulkan_checker_" + name + "[]={" + ",".join(hex(w) for w in words) + "};")
(work / "vulkan-checker-shaders.h").write_text("\n".join(header) + "\n")
for name in ("vulkan_checker.c", "vulkan_checker.h", "vulkan_checker_request.h"):
    (work / name).write_bytes(read("userspace/lib/compositor/" + name))
host = read("tests/compositor/vulkan_affine_host.c").decode()
def section(start, end):
    assert host.count(start) == 1 and host.count(end) == 1
    return host[host.index(start):host.index(end)]

prefix = '''#include "vulkan_checker.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#define W 32
#define H 24
#define BYTES (W*H*4)
#define CHECK(x) do { if(!(x)){fprintf(stderr,"FAIL line %d: %s\\n",__LINE__,#x);return 1;} } while(0)
#define VK(x) CHECK((x)==VK_SUCCESS)
static int floor_div(int x,int d){return x>=0?x/d:-((-x+d-1)/d);}
static int imax(int a,int b){return a>b?a:b;}
static int imin(int a,int b){return a<b?a:b;}
'''
helpers = section("static unsigned errors, calls;", "#ifdef CUBIT_NATIVE_SCENE_TEST\n#include \"native_scene_pixels.h\"")
setup = section("int run_vulkan_affine_tests(void)", "    VkImage source,mask,target,targets[OUTPUTS];").replace("int run_vulkan_affine_tests(void)", "int main(void)")
pixels = read("tests/compositor/vulkan_checker_pixels.inc").decode()
guards = read("tests/compositor/vulkan_checker_guards.inc").decode()
(work / "host.c").write_text(prefix + helpers + guards + setup + pixels)
(work / "inputs.json").write_text(json.dumps(inputs, indent=2) + "\n")
subprocess.run(["cc", "-std=c11", "-O2", "-Wall", "-Wextra", "-Werror", "-I", str(work),
                str(work / "host.c"), str(work / "vulkan_checker.c"), "-lvulkan", "-o", str(work / "checker")], check=True)
env = {**os.environ, "VK_DRIVER_FILES": os.environ["MESA_DRIVER_ROOT"] + "/share/vulkan/icd.d/lvp_icd.x86_64.json",
       "XDG_DATA_DIRS": os.environ["MESA_DRIVER_ROOT"] + "/share"}
result = subprocess.run([str(work / "checker")], env=env, text=True, capture_output=True, timeout=120)
(work / "pixels.log").write_text(result.stdout + result.stderr)
print(result.stdout + result.stderr, end="", flush=True)
result.check_returncode()
for relative, expected in inputs.items():
    assert hashlib.sha256((ROOT / relative).read_bytes()).hexdigest() == expected, relative
(work / "result.json").write_text(json.dumps({"status": "PASS", "scope": "hosted lavapipe pixels; not CuBit or Intel GPU execution"}) + "\n")
