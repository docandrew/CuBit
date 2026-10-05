#!/usr/bin/env python3
"""Fixed copies of crates Servo depends on, for x86_64-unknown-cubit.

Each fix is one of:
- cubitize: the crate only describes the Linux/musl C ABI, which the CuBit
  libc implements; target_os = "cubit" takes its Linux arms.
- edits: exact, asserted replacements, each with its reason.
Crates are copied from the cargo registry into userspace/rust/build/
servo-crates and patched in by servo-cargo.sh (cargo --config patch).

    crate_fixes.py <registry-src-dir> <output-dir>   # prints name=path lines
"""
import os, re, sys

here = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(here, "..", "rust", "std", "unix"))
from cubitize import cubitize_crate, writable  # noqa: E402
import shutil

FIXES = {
    # Shader and pixel attribution only while explicit GL timer queries are active.
    "swgl-0.70.0": {'edits': [('src/gl.cc', '#include <stdio.h>', '#include <stdio.h>\n#include <cubit/debug.h>', 'once'), ('src/gl.cc', '#ifdef PRINT_TIMINGS\n  uint64_t start = get_time_value();\n#endif\n\n  ctx->shaded_rows = 0;', '#ifdef PRINT_TIMINGS\n  uint64_t start = get_time_value();\n#endif\n\n  // CuBit diagnostics: only query-enabled draws pay for extra timestamps.\n  const bool penny_trace_draw = ctx->time_elapsed_query != 0;\n  const uint64_t penny_draw_start = penny_trace_draw ? get_time_value() : 0;\n\n  ctx->shaded_rows = 0;', 'once'), ('src/gl.cc', '  if (ctx->samples_passed_query) {\n    Query& q = ctx->queries[ctx->samples_passed_query];', '  if (penny_trace_draw) {\n    const uint64_t elapsed = get_time_value() - penny_draw_start;\n    if (elapsed >= 5000000) {\n      char message[512];\n      int length = snprintf(message, sizeof(message), "PENNY-SWGL: shader=%s instances=%d pixels=%d rows=%d target=%dx%d ms=%.3f\\n",\n             ctx->programs[ctx->current_program].impl->get_name(),\n             instancecount, ctx->shaded_pixels, ctx->shaded_rows,\n             colortex.width, colortex.height, double(elapsed) / 1000000.0);\n      if (length > 0) {\n        cubit_debug_write(message, size_t(length) < sizeof(message) ? size_t(length) : sizeof(message) - 1);\n      }\n    }\n  }\n\n  if (ctx->samples_passed_query) {\n    Query& q = ctx->queries[ctx->samples_passed_query];', 'once')], 'migrations': [('src/gl.cc', '  if (penny_trace_draw) {\n    const uint64_t elapsed = get_time_value() - penny_draw_start;\n    if (elapsed >= 5000000) {\n      printf("PENNY-SWGL: shader=%s instances=%d pixels=%d rows=%d target=%dx%d ms=%.3f\\n",\n             ctx->programs[ctx->current_program].impl->get_name(),\n             instancecount, ctx->shaded_pixels, ctx->shaded_rows,\n             colortex.width, colortex.height, double(elapsed) / 1000000.0);\n    }\n  }\n\n  if (ctx->samples_passed_query) {\n    Query& q = ctx->queries[ctx->samples_passed_query];', '  if (ctx->samples_passed_query) {\n    Query& q = ctx->queries[ctx->samples_passed_query];'), ('src/gl.cc', '  if (penny_trace_draw) {\n    const uint64_t elapsed = get_time_value() - penny_draw_start;\n    if (elapsed >= 5000000) {\n      char message[512];\n      int length = snprintf(message, sizeof(message), "PENNY-SWGL: shader=%s instances=%d pixels=%d rows=%d target=%dx%d ms=%.3f\\n",\n             ctx->programs[ctx->current_program].impl->get_name(),\n             instancecount, ctx->shaded_pixels, ctx->shaded_rows,\n             colortex.width, colortex.height, double(elapsed) / 1000000.0);\n      if (length > 0) {\n        cubit_debug_write(message, min(size_t(length), sizeof(message) - 1));\n      }\n    }\n  }\n\n  if (ctx->samples_passed_query) {\n    Query& q = ctx->queries[ctx->samples_passed_query];', '  if (ctx->samples_passed_query) {\n    Query& q = ctx->queries[ctx->samples_passed_query];')]},

    # The C ABI: struct layouts, constants, symbols.
    "libc-0.2.189": {"cubitize": True},

    # Socket calls and constants are C ABI (the CuBit libc serves them).
    "socket2-0.6.5": {"cubitize": True},

    # Only its libc backend (servo-cargo.sh sets --cfg rustix_use_libc):
    # never the raw-Linux-syscall backend. Two ABI items missing for an
    # unknown OS.
    "rustix-1.1.5": {"edits": [
        ("src/backend/libc/fs/types.rs",
         'target_os = "linux"', 'any(target_os = "linux", target_os = "cubit")', "all"),
        ("src/ioctl/mod.rs",
         'target_os = "linux"', 'any(target_os = "linux", target_os = "cubit")', "all"),
    ]},

    # mio: its poll(2) selector and pipe waker (servo-cargo.sh cfgs), both
    # CuBit libc objects, never epoll/eventfd. accept4 is C ABI.
    "mio-1.2.3": {"edits": [
        ("src/sys/unix/tcp.rs",
         'target_os = "illumos",\n        target_os = "linux",',
         'target_os = "illumos",\n        target_os = "linux",\n        target_os = "cubit",', "all"),
        # pipe2 is C ABI (the libc's in-process pipe). Without an arm,
        # new_raw returned [-1, -1] as success.
        ("src/sys/unix/pipe.rs",
         '        target_os = "hurd",\n        target_os = "linux",',
         '        target_os = "hurd",\n        target_os = "linux",\n        target_os = "cubit",', "once"),
    ]},

    # SpiderMonkey's own configure knows ABIs, not CuBit: build it for the
    # Linux/musl C ABI the CuBit libc implements (as the Rust side does);
    # its Linux-specific calls reach the CuBit syscall layer.
    "mozjs_sys-153.3.0-0": {"edits": [
        ("makefile.cargo",
         "\tifeq (aarch64-unknown-linux-gnu,$(TARGET))",
         "\tifeq (x86_64-unknown-cubit,$(TARGET))\n"
         "\t\tTARGET = x86_64-unknown-linux-musl\n"
         "\tendif\n\n"
         "\tifeq (aarch64-unknown-linux-gnu,$(TARGET))", "once"),
        # CuBit releases whole owned mappings. SpiderMonkey's generic Unix
        # aligned allocator trims reservations with partial munmap, which is
        # deliberately unsupported. Reuse its posix_memalign/free strategy
        # (also used by WASI), keeping accounting and real mprotect unchanged.
        ("mozjs/js/src/gc/Memory.cpp",
         "#ifdef __wasi__\n  void* region = nullptr;\n  if (int err = posix_memalign(&region, alignment, length))",
         "#if 1  // CuBit: aligned libc allocations, paired with free below.\n  void* region = nullptr;\n  if (int err = posix_memalign(&region, alignment, length))", "once"),
        ("mozjs/js/src/gc/Memory.cpp",
         "  memset(region, 0, length);\n  return region;\n#else\n\n#  ifdef JS_64BIT",
         "  memset(region, 0, length);\n  RecordMemoryAlloc(length);\n  return region;\n#else\n\n#  ifdef JS_64BIT", "once"),
        ("mozjs/js/src/gc/Memory.cpp",
         "  UnmapInternal(region, length);\n\n#ifndef __wasi__\n  RecordMemoryFree(length);",
         "  // CuBit: only MapAlignedPages allocations use libc ownership.\n  free(region);\n\n#ifndef __wasi__\n  RecordMemoryFree(length);", "once"),
        ("mozjs/js/src/gc/Memory.cpp",
         "  void* region = MapAlignedPagesLastDitch(length, alignment, StallAndRetry::No);\n  if (region) {\n    RecordMemoryAlloc(length);\n  }\n  return region;",
         "  // CuBit has no partial-unmap alignment strategy to stress.\n  return MapAlignedPages(length, alignment, StallAndRetry::No);", "once"),
        # Every CuBit program is statically linked, libstdc++ included
        # (Firefox's shipping policy against that does not apply).
        ("mozjs/build/moz.configure/flags.configure",
         '    die("Firefox does not support linking statically with libstdc++")',
         '    log.info("libstdc++ is linked statically (CuBit programs are static)")',
         "once"),
        # mozglue's interposers wrap libc (getenv, ...) in a dynamically
        # linked process and find the real functions with
        # dlsym(RTLD_NEXT); in a static CuBit program there is no next
        # object and they crash at startup. libc's own functions stand.
        ("mozjs/mozglue/moz.build",
         'if CONFIG["OS_ARCH"] == "Linux" and not CONFIG["FUZZING_SNAPSHOT"]:\n    DIRS += ["interposers"]',
         'if False:  # CuBit: static programs (crate_fixes.py)\n    DIRS += ["interposers"]',
         "once"),
    ]},

    # Peer credentials of a socket (SO_PEERCRED) are C ABI; the CuBit libc
    # answers them (or fails) at run time.
    "tokio-1.53.1": {"edits": [
        ("src/net/unix/ucred.rs",
         'target_os = "linux",', 'target_os = "linux", target_os = "cubit",', "all"),
    ]},

    # Static constructors through .init_array are an ELF fact the CuBit
    # libc honours; without an arm, registration silently compiles away
    # (Servo's baked-in resources, "No resource reader registered").
    "inventory-0.3.24": {"cubitize": True},

    # getrandom 0.2 chooses its source by OS: getrandom(2) is C ABI, which
    # the CuBit libc serves (0.3/0.4 use --cfg getrandom_backend).
    "getrandom-0.2.17": {"cubitize": True},
    "getrandom-0.3.4": {"cubitize": True},
    "getrandom-0.4.1": {"cubitize": True},

    # dlopen flag values are C ABI (musl's). CuBit programs are static, so
    # dlopen itself fails at run time; surfman gets a CuBit backend.
    "libloading-0.8.9": {"cubitize": True},

    # Servo runs single-process on CuBit: channels are in-process.
    "ipc-channel-0.23.0": {"edits": [
        ("src/platform/mod.rs",
         'target_os = "wasi",\n    target_os = "unknown"',
         'target_os = "wasi",\n    target_os = "cubit",\n    target_os = "unknown"', "all"),
    ]},
}



# SWGL has no GLES varying limit: reconstruct invariant shadow clip data once
# per vertex instead of once per pixel. Keep the upstream hardware shader path.
FIXES["swgl-0.70.0"]["edits"] += [
    ("res/ps_quad_box_shadow.glsl", "#ifdef WR_VERTEX_SHADER",
     """#ifdef SWGL
flat varying highp vec4 vPennyClipTL;
flat varying highp vec4 vPennyClipTR;
flat varying highp vec4 vPennyClipBR;
flat varying highp vec4 vPennyClipBL;
flat varying highp vec3 vPennyPlaneTL;
flat varying highp vec3 vPennyPlaneTR;
flat varying highp vec3 vPennyPlaneBR;
flat varying highp vec3 vPennyPlaneBL;
flat varying highp vec4 vPennyBounds;
#endif

#ifdef WR_VERTEX_SHADER""", "once"),
    ("res/ps_quad_box_shadow.glsl",
     """    vElemCenter_Radius_BL = vec4(elem_p0.x + r_bl.x, elem_p1.y - r_bl.y, r_bl);""",
     """    vElemCenter_Radius_BL = vec4(elem_p0.x + r_bl.x, elem_p1.y - r_bl.y, r_bl);
#ifdef SWGL
    vec2 c_tl = vElemCenter_Radius_TL.xy;
    vec2 c_tr = vElemCenter_Radius_TR.xy;
    vec2 c_br = vElemCenter_Radius_BR.xy;
    vec2 c_bl = vElemCenter_Radius_BL.xy;
    vec2 n_tl = -r_tl.yx;
    vec2 n_tr = vec2(r_tr.y, -r_tr.x);
    vec2 n_br = r_br.yx;
    vec2 n_bl = vec2(-r_bl.y, r_bl.x);
    vPennyClipTL = vec4(c_tl, inverse_radii_squared(r_tl));
    vPennyClipTR = vec4(c_tr, inverse_radii_squared(r_tr));
    vPennyClipBR = vec4(c_br, inverse_radii_squared(r_br));
    vPennyClipBL = vec4(c_bl, inverse_radii_squared(r_bl));
    vPennyPlaneTL = vec3(n_tl, dot(n_tl, vec2(c_tl.x - r_tl.x, c_tl.y)));
    vPennyPlaneTR = vec3(n_tr, dot(n_tr, vec2(c_tr.x, c_tr.y - r_tr.y)));
    vPennyPlaneBR = vec3(n_br, dot(n_br, vec2(c_br.x + r_br.x, c_br.y)));
    vPennyPlaneBL = vec3(n_bl, dot(n_bl, vec2(c_bl.x, c_bl.y + r_bl.y)));
    vPennyBounds = vec4(c_tl - r_tl, c_br + r_br);
#endif""", "once"),
    ("res/ps_quad_box_shadow.glsl",
     """    float aa_range = compute_aa_range(local_pos);

    vec2 c_tl""",
     """    float aa_range = compute_aa_range(local_pos);

#ifdef SWGL
    float elem_dist = distance_to_rounded_rect(
        local_pos,
        vPennyPlaneTL, vPennyClipTL,
        vPennyPlaneTR, vPennyClipTR,
        vPennyPlaneBR, vPennyClipBR,
        vPennyPlaneBL, vPennyClipBL,
        vPennyBounds
    );
#else
    vec2 c_tl""", "once"),
    ("res/ps_quad_box_shadow.glsl",
     """        elem_bounds
    );""",
     """        elem_bounds
    );
#endif""", "once"),
]



# A constant gradient offset across a row has no next stop boundary to cross.
# Dividing the negative distance to the prior stop by zero made subSpan clamp
# to one pixel, defeating the existing vectorized full-span implementation.
FIXES["swgl-0.70.0"]["edits"] += [
    ("src/swgl_ext.h",
     """      float offsetRange =
          delta > 0.0f ? nextOffset - offset.x : prevOffset - offset.x;
      subSpan = min(subSpan, offsetRange / delta);""",
     """      if (delta != 0.0f) {
        float offsetRange =
            delta > 0.0f ? nextOffset - offset.x : prevOffset - offset.x;
        subSpan = min(subSpan, offsetRange / delta);
      }""", "once"),
]



# Native KVM exposed a stall in libc's floating-point snprintf while logging a
# completed SWGL draw. Format the already-integral nanoseconds without x87/float
# conversion. Keep the exact milliseconds and three fractional decimal places.
for index, (path, old, new, how) in enumerate(FIXES["swgl-0.70.0"]["edits"]):
    if "char message[512];" not in new:
        continue
    instrumented = new.replace("      char message[512];",
        '      cubit_debug_write("PENNY-SWGL: format begin\\n", 25);\n      char message[512];')
    instrumented = instrumented.replace("      if (length > 0) {",
        '      cubit_debug_write("PENNY-SWGL: format end\\n", 23);\n      if (length > 0) {')
    final = new.replace('ms=%.3f', 'ms=%llu.%03llu').replace(
        'double(elapsed) / 1000000.0',
        'static_cast<unsigned long long>(elapsed / 1000000),\n             static_cast<unsigned long long>((elapsed / 1000) % 1000)')
    FIXES["swgl-0.70.0"]["migrations"] += [(path, instrumented, final), (path, new, final)]
    # Repair a duplicated experimental prefix left by an interrupted preparation.
    anchor = "  if (ctx->samples_passed_query) {"
    FIXES["swgl-0.70.0"]["migrations"] += [
        (path, instrumented.split(anchor)[0], ""),
        (path, new.split(anchor)[0], ""),
    ]
    FIXES["swgl-0.70.0"]["edits"][index] = (path, old, final, how)


def apply(registry, out):
    lines = []
    for crate, fix in FIXES.items():
        src = os.path.join(registry, crate)
        dst = os.path.join(out, crate)
        stamp = os.path.join(dst, ".cubit-fixed")
        if not os.path.exists(stamp):
            if fix.get("cubitize"):
                cubitize_crate(src, dst)
            else:
                if os.path.exists(dst):
                    writable(dst)
                    shutil.rmtree(dst)
                shutil.copytree(src, dst)
                writable(dst)
                for junk in (".cargo-checksum.json", ".cargo_vcs_info.json"):
                    j = os.path.join(dst, junk)
                    if os.path.exists(j):
                        os.remove(j)
            open(stamp, "w").close()
        # Repair the abandoned broad GC unmap edit in existing build caches.
        # Ordinary mmap probes must retain their matching munmap operation.
        if crate.startswith("mozjs_sys-"):
            memory = os.path.join(dst, "mozjs/js/src/gc/Memory.cpp")
            text = open(memory, encoding="utf-8").read()
            corrected = text.replace(
                "#elif 1  // CuBit: MapAlignedPages returns a libc-owned allocation.",
                "#elif defined(__wasi__)")
            if corrected != text:
                open(memory, "w", encoding="utf-8").write(corrected)
        # Explicitly migrate superseded edits before checking final replacements.
        for path, old, new in fix.get("migrations", []):
            p = os.path.join(dst, path)
            s = open(p, encoding="utf-8").read()
            if old in s:
                open(p, "w", encoding="utf-8").write(s.replace(old, new, 1))
        # Apply newly added exact edits to an existing patched checkout too.
        # Identical replacements preserve timestamps and Cargo's build cache.
        for path, old, new, how in fix.get("edits", []):
            p = os.path.join(dst, path)
            s = open(p, encoding="utf-8").read()
            if new in s:
                continue
            assert old in s, (crate, path, old)
            s = s.replace(old, new) if how == "all" else s.replace(old, new, 1)
            open(p, "w", encoding="utf-8").write(s)
        # name-version, where the version may itself contain '-' (153.3.0-0)
        name = re.match(r"^(.*?)-\d+\.\d+\.\d+", crate).group(1)
        lines.append(f"{name}={dst}")
    return lines


if __name__ == "__main__":
    for line in apply(sys.argv[1], sys.argv[2]):
        print(line)
