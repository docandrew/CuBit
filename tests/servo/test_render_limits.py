#!/usr/bin/env python3
"""Cross-check CuBit SWGL settings against upstream/runtime constraints.

This catches unsupported WebRender options and overlarge fixed atlases, not
all possible engine allocations or page-dependent render targets.
"""
import ast
import pathlib
import re

ROOT = pathlib.Path(__file__).resolve().parents[2]
registry = ROOT / "userspace/rust/build/servo-work/cargo-home/registry/src"
upstream = next(registry.glob("*/webrender-0.70.0/src"))
init = (upstream / "renderer/init.rs").read_text()
minimum = int(re.search(r"const MIN_TEXTURE_SIZE: i32 = (\d+);", init)[1])
source = (ROOT / "userspace/servo/patch_servo.py").read_text()
# Inspect the exact replacement text passed to the production patcher.
replacements = [node.args[2].value for node in ast.walk(ast.parse(source))
                if isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
                and node.func.id == "edit" and len(node.args) >= 3
                and isinstance(node.args[2], ast.Constant)
                and isinstance(node.args[2].value, str)]
options = next(text for text in replacements if "texture_cache_config:" in text)
limit = int(re.search(r"max_internal_texture_size: if software_gl \{ Some\((\d+)\)", options)[1])
assert limit >= minimum, (limit, minimum)
libc = (ROOT / "userspace/libc/overlay/src/cubit/syscall.c").read_text()
megabytes = int(re.search(r"if \(len > (\d+)UL \* 1024 \* 1024\)", libc)[1])
cap = megabytes * 1024 * 1024
image_tile = int(re.search(r"image_tiling_threshold: if software_gl \{ (\d+)", options)[1])
assert image_tile <= limit and image_tile * image_tile * 4 + 8192 < cap
shared_surface = int(re.search(r"max_shared_surface_size: if software_gl \{ (\d+)", options)[1])
assert shared_surface * shared_surface * 4 + 8192 < cap
cache = (upstream / "texture_cache.rs").read_text()
defaults = cache[cache.index("pub const DEFAULT: Self = TextureCacheConfig {"):]
defaults = defaults[:defaults.index("};")]
for field, bpp in [("color8_linear", 4), ("color8_nearest", 4), ("color8_glyph", 4),
                   ("alpha8", 1), ("alpha8_glyph", 1), ("alpha16", 2)]:
    pattern = rf"{field}_texture_size: (\d+)"
    value = re.search(pattern, options) or re.search(pattern, defaults)
    side = int(value[1])
    assert side <= limit
    assert side * side * bpp + 8192 < cap, (field, side, cap)
print(f"SERVO-RENDER-LIMITS: PASS supported minimum={minimum}, six fixed atlases and shared render targets below {megabytes}MiB with allocator margin")
