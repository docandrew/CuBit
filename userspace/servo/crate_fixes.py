#!/usr/bin/env python3
"""Fixed copies of crates Servo depends on, for x86_64-unknown-cubit.

Each fix is one of:
- a fork (FORKED): the crate is Penny's fork at its pinned commit (mozjs_sys);
- cubitize: the crate only describes the Linux/musl C ABI, which the CuBit
  libc implements; target_os = "cubit" takes its Linux arms.
- edits: exact, asserted replacements, each with its reason.
servo-cargo.sh patches them in with cargo --config patch, so cargo never
downloads these crates itself: each is fetched here from crates.io by its
pinned SHA-256 (the checksum Servo's Cargo.lock records), unpacked into
<download-dir>/src, and copied into <output-dir> with its fixes.

    crate_fixes.py <download-dir> <output-dir>   # prints name=path lines

A fixed copy is rebuilt from the pristine crate whenever its fix changes
(the stamp holds the fix's hash), and left untouched otherwise so cargo's
build cache stays valid.
"""
import hashlib, io, json, os, re, shutil, subprocess, sys, tarfile, urllib.request

here = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(here, "..", "rust", "std", "unix"))
from cubitize import cubitize_crate, writable  # noqa: E402

CRATES_IO_DOWNLOAD = "https://static.crates.io/crates/{name}/{name}-{version}.crate"
STAMP = ".cubit-fixed"

# The .crate archive's SHA-256 (Cargo.lock's checksum) of every fixed crate.
PINNED = {
    "swgl-0.70.0": "572f955ee15b7e474bca393377eef1332f80e7605f13f3d12296bcbfbe1a9004",
    "libc-0.2.189": "3eaf3ede3fee6db1a4c2ee091bf8a8b4dccdc6d17f656fb07896ee72867612f2",
    "socket2-0.6.5": "c3d1e2c7f27f8d4cb10542a02c49005dbd6e93095799d6f3be745fae9f8fedd4",
    "rustix-1.1.5": "891efababe418670775f199f0d233d84843c227a0949a883ce15b37c78d6629d",
    "mio-1.2.3": "4b18443e9c262bfe8fa82f51666e2642c53393f7e5c27b3e1aeab922cff5b9d8",
    "tokio-1.53.1": "202caea871b69668250d242070849eb495be178ed697a3e98aebce5bc81a0bed",
    "inventory-0.3.24": "a4f0c30c76f2f4ccee3fe55a2435f691ca00c0e4bd87abe4f4a851b1d4dac39b",
    "getrandom-0.2.17": "ff2abc00be7fca6ebc474524697ae276ad847ad0a6b3faa4bcb027e9a4614ad0",
    "getrandom-0.3.4": "899def5c37c4fd7b2664648c28120ecec138e4d395b459e5ca34f9cce2dd77fd",
    "getrandom-0.4.1": "139ef39800118c7683f2fd3c98c1b23c09ae076556b435f8e9064ae108aaeeec",
    "libloading-0.8.9": "d7c4b02199fee7c5d21a5ae7d8cfa79a6ef5bb2fc834d6e9058e89c825efdc55",
    "ipc-channel-0.23.0": "25c2117f45662580ea93b6fb2bc665c52667ea0370c23fab9bb9485acec8bd24",
}

# Crates Penny forks (FORKS, fork-checkout.sh): the fork's pinned commit is
# the crate with CuBit's changes as commits (mozjs_sys: SpiderMonkey built
# for the Linux/musl C ABI, GC chunks without partial munmap, static
# libstdc++, no mozglue interposers). Copied in like a fixed crate.
FORKED = {"mozjs_sys-153.3.0-0": "mozjs"}

FIXES = {
    # Shader and pixel attribution only while explicit GL timer queries are
    # active: a draw slower than PENNY_SLOW_DRAW_NS is logged. Integral
    # formatting: libc's floating-point snprintf stalled under native KVM.
    "swgl-0.70.0": {"edits": [
        ("src/gl.cc", "#include <stdio.h>", "#include <stdio.h>\n#include <cubit/debug.h>", "once"),
        ("src/gl.cc",
         "#ifdef PRINT_TIMINGS\n  uint64_t start = get_time_value();\n#endif\n\n  ctx->shaded_rows = 0;",
         "#ifdef PRINT_TIMINGS\n  uint64_t start = get_time_value();\n#endif\n\n"
         "  // CuBit diagnostics: only query-enabled draws pay for extra timestamps.\n"
         "  const bool penny_trace_draw = ctx->time_elapsed_query != 0;\n"
         "  const uint64_t penny_draw_start = penny_trace_draw ? get_time_value() : 0;\n\n"
         "  ctx->shaded_rows = 0;", "once"),
        ("src/gl.cc",
         "  if (ctx->samples_passed_query) {\n    Query& q = ctx->queries[ctx->samples_passed_query];",
         "  if (penny_trace_draw) {\n"
         "    const uint64_t PENNY_SLOW_DRAW_NS = 5000000;  // 5 ms\n"
         "    const uint64_t NS_PER_MS = 1000000, NS_PER_US = 1000, US_PER_MS = 1000;\n"
         "    const uint64_t elapsed = get_time_value() - penny_draw_start;\n"
         "    if (elapsed >= PENNY_SLOW_DRAW_NS) {\n"
         "      char message[512];\n"
         "      int length = snprintf(message, sizeof(message), \"PENNY-SWGL: shader=%s instances=%d pixels=%d rows=%d target=%dx%d ms=%llu.%03llu\\n\",\n"
         "             ctx->programs[ctx->current_program].impl->get_name(),\n"
         "             instancecount, ctx->shaded_pixels, ctx->shaded_rows,\n"
         "             colortex.width, colortex.height, static_cast<unsigned long long>(elapsed / NS_PER_MS),\n"
         "             static_cast<unsigned long long>((elapsed / NS_PER_US) % US_PER_MS));\n"
         "      if (length > 0) {\n"
         "        cubit_debug_write(message, size_t(length) < sizeof(message) ? size_t(length) : sizeof(message) - 1);\n"
         "      }\n"
         "    }\n"
         "  }\n\n"
         "  if (ctx->samples_passed_query) {\n    Query& q = ctx->queries[ctx->samples_passed_query];", "once"),
        # x86-64-v3 is CuBit's minimum CPU: a fixed baseline, never -march=native
        # (reproducible). No FMA contraction, so rasterized pixels do not change.
        ("build.rs",
         '    build\n        .file("src/gl.cc")\n',
         '    build\n        .flag("-march=x86-64-v3")\n        .flag("-ffp-contract=off")\n'
         '        .file("src/gl.cc")\n', "once"),
    ]},

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


def split_name(crate):
    """name and version of name-version, where the version may itself
    contain '-' (153.3.0-0)."""
    match = re.match(r"^(.*?)-(\d+\.\d+\.\d+.*)$", crate)
    return match.group(1), match.group(2)


def fetch(downloads):
    """Unpack every pinned crate into <downloads>/src; download (and check)
    only those not yet there."""
    assert set(PINNED) == set(FIXES), set(PINNED) ^ set(FIXES)
    sources = os.path.join(downloads, "src")
    os.makedirs(sources, exist_ok=True)
    for crate, digest in PINNED.items():
        if os.path.exists(os.path.join(sources, crate, "Cargo.toml")):
            continue
        name, version = split_name(crate)
        url = CRATES_IO_DOWNLOAD.format(name=name, version=version)
        with urllib.request.urlopen(url) as response:
            data = response.read()
        actual = hashlib.sha256(data).hexdigest()
        if actual != digest:
            sys.exit(f"crate_fixes: {crate} from {url} has SHA-256 {actual}, pinned {digest}")
        unpacked = os.path.join(sources, crate + ".partial")
        if os.path.exists(unpacked):
            shutil.rmtree(unpacked)
        with tarfile.open(fileobj=io.BytesIO(data), mode="r:gz") as archive:
            archive.extractall(unpacked, filter="data")
        os.rename(os.path.join(unpacked, crate), os.path.join(sources, crate))
        os.rmdir(unpacked)
    for crate, fork in FORKED.items():
        subprocess.run(["bash", os.path.join(here, "fork-checkout.sh"), fork,
                        os.path.join(sources, crate + ".fork")], check=True)
    return sources


def fork_commit(fork):
    for line in open(os.path.join(here, "FORKS")):
        fields = line.split()
        if fields and fields[0] == fork:
            return fields[1]
    raise KeyError(fork)


def fix_digest(fix):
    return hashlib.sha256(json.dumps(fix, sort_keys=True).encode()).hexdigest()


def apply(pristine, out):
    """Make <out>/<crate> = <pristine>/<crate> with its fix."""
    lines = []
    for crate, fix in FIXES.items():
        src = os.path.join(pristine, crate)
        dst = os.path.join(out, crate)
        stamp = os.path.join(dst, STAMP)
        digest = fix_digest(fix)
        current = open(stamp).read() if os.path.exists(stamp) else None
        if current != digest:
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
            for path, old, new, how in fix.get("edits", []):
                p = os.path.join(dst, path)
                s = open(p, encoding="utf-8").read()
                assert old in s, (crate, path, old)
                if how == "once":
                    assert s.count(old) == 1, (crate, path, old, s.count(old))
                    s = s.replace(old, new, 1)
                else:
                    s = s.replace(old, new)
                open(p, "w", encoding="utf-8").write(s)
            with open(stamp, "w") as output:
                output.write(digest)
        lines.append(f"{split_name(crate)[0]}={dst}")
    for crate, fork in FORKED.items():
        src = os.path.join(pristine, crate + ".fork")
        dst = os.path.join(out, crate)
        stamp = os.path.join(dst, STAMP)
        digest = fork_commit(fork)
        current = open(stamp).read() if os.path.exists(stamp) else None
        if current != digest:
            if os.path.exists(dst):
                writable(dst)
                shutil.rmtree(dst)
            shutil.copytree(src, dst, ignore=lambda d, names: [".git"] if d == src else [])
            writable(dst)
            for junk in (".cargo-checksum.json", ".cargo_vcs_info.json"):
                j = os.path.join(dst, junk)
                if os.path.exists(j):
                    os.remove(j)
            with open(stamp, "w") as output:
                output.write(digest)
        lines.append(f"{split_name(crate)[0]}={dst}")
    return lines


if __name__ == "__main__":
    for line in apply(fetch(sys.argv[1]), sys.argv[2]):
        print(line)
