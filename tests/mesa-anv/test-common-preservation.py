#!/usr/bin/env python3
"""Check relocated Mesa routines retain their pinned upstream implementations.

Source-equivalence regression, not a native device or GPU execution test.
Only internal linkage changes (static to externally declared) are permitted.
"""
from pathlib import Path
import sys

source = Path(sys.argv[1]).resolve()
upstream = Path(sys.argv[2]).resolve()
relative = Path("src/intel/vulkan")
before = (upstream / relative / "anv_physical_device.c").read_text()
after = (source / relative / "anv_physical_device_common.c").read_text()


def definition(text, name):
    # Mesa's top-level function close is unindented; inner blocks are indented.
    start = text.index("\n" + name + "(") + 1
    end = text.index("\n}\n", start) + len("\n}\n")
    start = text.rfind("\n", 0, start - 1) + 1
    declaration = text[start:end]
    return declaration.removeprefix("static ")


names = (
    "compiler_debug_log", "compiler_perf_log",
    "anv_physical_device_init_uuids",
    "anv_physical_device_init_disk_cache",
    "anv_physical_device_free_disk_cache",
    "anv_override_engine_counts",
    "anv_physical_device_init_queue_families",
)
for name in names:
    if definition(before, name) != definition(after, name):
        raise SystemExit("Upstream implementation changed: " + name)
    print("Upstream preservation PASS:", name)

if "#define MAX_DEBUG_MESSAGE_LENGTH    4096" not in after:
    raise SystemExit("Compiler debug message limit changed")

linux = (source / relative / "anv_gem.c").read_text()
original = definition(before, "anv_restrict_sys_heap_size")
relocated = definition(linux, "anv_drm_restrict_sys_heap_size")
if relocated.replace("anv_drm_restrict_sys_heap_size", "anv_restrict_sys_heap_size") != original:
    raise SystemExit("Linux system-memory budget policy changed")
print("Upstream preservation PASS: Linux system-memory budget policy")

allocator = (upstream / relative / "anv_allocator.c").read_text()
original = definition(allocator, "map_placed_addr_slab")
relocated = definition(linux, "anv_drm_map_placed_slab")
if relocated.replace("anv_drm_map_placed_slab", "map_placed_addr_slab") != original:
    raise SystemExit("Linux placed-address slab mapping changed")
print("Upstream preservation PASS: Linux placed-address slab mapping")

for operation in ("get", "set"):
    old_name = "anv_device_" + operation + "_bo_tiling"
    new_name = "anv_drm_" + operation + "_bo_tiling"
    if definition(linux, new_name).replace(new_name, old_name) != definition(allocator, old_name):
        raise SystemExit("Linux tiling implementation changed: " + operation)
    print("Upstream preservation PASS: Linux tiling", operation)
