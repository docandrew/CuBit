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
            for path, old, new, how in fix.get("edits", []):
                p = os.path.join(dst, path)
                s = open(p, encoding="utf-8").read()
                assert old in s, (crate, path, old)
                s = s.replace(old, new) if how == "all" else s.replace(old, new, 1)
                open(p, "w", encoding="utf-8").write(s)
            open(stamp, "w").close()
        # name-version, where the version may itself contain '-' (153.3.0-0)
        name = re.match(r"^(.*?)-\d+\.\d+\.\d+", crate).group(1)
        lines.append(f"{name}={dst}")
    return lines


if __name__ == "__main__":
    for line in apply(sys.argv[1], sys.argv[2]):
        print(line)
