#!/usr/bin/env python3
"""Run hosted fault injection against the actual extracted transport function."""
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent
source = Path(sys.argv[1]).resolve()
text = (source / "src/intel/vulkan/anv_gem.c").read_text()
start = text.index("void\nanv_drm_close_device(")
open_start = text.index("VkResult\nanv_drm_open_device(", start)
end = text.index("\n}\n", open_start) + 3
output = Path(tempfile.mkdtemp(prefix="device-open.", dir=root / "target"))
# Generated fixture includes the exact function, not a hand-maintained copy.
(output / "device-open-under-test.h").write_text(text[start:end])
subprocess.run(["cc", "-std=c11", "-O2", "-Wall", "-Wextra", "-Werror",
                "-fsanitize=undefined", "-fno-sanitize-recover=all",
                "-I", str(output), str(root / "device-open-test.c"),
                "-o", str(output / "test")], check=True)
subprocess.run([str(output / "test")], check=True)
print("Fixture:", output)

start = text.index("VkResult\nanv_drm_init_sync_types(")
end = text.index("\n}\n", start) + 3
(output / "sync-types-under-test.h").write_text(text[start:end])
subprocess.run(["cc", "-std=c11", "-O2", "-Wall", "-Wextra", "-Werror",
                "-fsanitize=undefined", "-fno-sanitize-recover=all",
                "-I", str(output), str(root / "sync-types-test.c"),
                "-o", str(output / "sync-test")], check=True)
subprocess.run([str(output / "sync-test")], check=True)
