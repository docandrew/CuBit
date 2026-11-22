#!/usr/bin/env python3
"""Which C/C++ libraries are linked into Penny (WEB-004), from the unstripped
ELF's symbol table: each library is recognised by functions it always
exports when linked. A replaced library must be absent.

    check_c_libraries.py [ELF] [--absent NAME ...]

ELF defaults to the CuBit build's unstripped cubitshell. Prints every
library present or absent; with --absent, exits 1 if a named one is
present (e.g. --absent harfbuzz after the HarfRust swap).
"""
import argparse
import pathlib
import re
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
# Library -> a pattern matching symbols only it defines (demangled names).
MARKERS = {
    "spidermonkey": r"\bJS_NewContext\b",
    "harfbuzz": r"^hb_shape(_full)?$",
    "freetype": r"^FT_Init_FreeType$",
    "sqlite": r"^sqlite3_open_v2$",
    "zstd": r"^ZSTD_decompressStream$",
    "aws-lc": r"^aws_lc_\d+_\d+_\d+_(EVP_|SHA256)",
    "ring": r"^ring_core_\d+_\d+_\d+_",
    "angle": r"\b(ShInitialize|sh::TCompiler)\b",
    "gstreamer": r"^gst_init_check$",
    "libvpx": r"^vpx_codec_decode$",
    "libopus": r"^opus_(decode|custom_decoder_ctl|decoder_create)$",
}


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("elf", nargs="?", default=str(
        ROOT / "userspace/rust/build/servo-work/target/x86_64-unknown-cubit/release/cubitshell"))
    parser.add_argument("--absent", action="append", default=[], choices=sorted(MARKERS))
    options = parser.parse_args()
    listing = subprocess.run(["nm", "-C", "--defined-only", options.elf], capture_output=True,
                             text=True, check=True).stdout
    names = [line.split(" ", 2)[2] for line in listing.splitlines() if line.count(" ") >= 2]
    present = {library for library, pattern in MARKERS.items()
               if any(re.search(pattern, name) for name in names)}
    for library in MARKERS:
        print(f"{library}: {'linked' if library in present else 'absent'}")
    leaked = [library for library in options.absent if library in present]
    for library in leaked:
        print(f"FAIL {library} is still linked")
    print("PENNY-C-LIBRARIES:", "FAIL" if leaked else "PASS")
    return 1 if leaked else 0


if __name__ == "__main__":
    sys.exit(main())
