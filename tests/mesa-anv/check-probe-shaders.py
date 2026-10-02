#!/usr/bin/env python3
"""Compare Mesa's compiled probe binaries with the native Ada constants.

This is a hosted compiler/fixture consistency check, not GPU execution.
"""
import pathlib
import re
import struct
import sys


def encoded_array(source, name):
    matches = re.findall(
        rf"\b{name}\s*:\s*constant\s+Words\s*:=\s*\[(.*?)\];",
        source, re.S)
    if len(matches) != 1:
        raise ValueError(f"expected exactly one {name} array")
    body = re.sub(r"--[^\n]*", "", matches[0])
    words = []
    for token in body.split(","):
        token = token.strip()
        if re.fullmatch(r"16#[0-9a-fA-F_]+#", token):
            value = int(token[3:-1].replace("_", ""), 16)
        elif re.fullmatch(r"[0-9][0-9_]*", token):
            value = int(token.replace("_", ""), 10)
        else:
            raise ValueError(f"unsupported {name} word: {token!r}")
        if not 0 <= value <= 0xFFFFFFFF:
            raise ValueError(f"{name} word outside uint32")
        words.append(value)
    return struct.pack("<" + "I" * len(words), *words)


def check(source, directory):
    for name in ("Vertex", "Fragment"):
        expected = encoded_array(source, name)
        actual = (directory / (name.lower() + ".bin")).read_bytes()
        if actual != expected:
            raise ValueError(
                f"{name} differs from native probe: compiler {len(actual)} bytes, "
                f"native {len(expected)} bytes")
        print(f"{name}: exact native/Mesa match ({len(actual)} bytes)")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit("usage: check-probe-shaders.py COMPILER_OUTPUT_DIRECTORY")
    root = pathlib.Path(__file__).resolve().parents[2]
    try:
        check((root / "userspace/services/intel-gpu/intel_gpu_adln_probe_shaders.ads")
              .read_text(), pathlib.Path(sys.argv[1]))
    except (OSError, ValueError) as error:
        sys.exit(f"shader consistency check failed: {error}")
