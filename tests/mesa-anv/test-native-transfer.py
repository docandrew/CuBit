#!/usr/bin/env python3
"""Nix: hosted Vulkan-call contract mocks, not Mesa/GPU execution."""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[2]
source = Path(sys.argv[1]).resolve()
with tempfile.TemporaryDirectory(prefix="cubit-transfer-test.") as temp:
    executable = Path(temp) / "test"
    subprocess.run([
        "cc", "-std=c11", "-Wall", "-Wextra", "-Werror",
        "-Wno-unused-parameter", "-I" + str(source / "include"),
        str(root / "tests/mesa-anv/native-transfer-test.c"), "-o", str(executable)
    ], check=True)
    subprocess.run([str(executable)], check=True, timeout=20)
