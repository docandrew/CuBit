"""Print a reproducible OS-boundary inventory; NOT a build/conformance test."""
import argparse
import hashlib
import pathlib
import re

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("source", type=pathlib.Path)
args = parser.parse_args()
root = args.source
version = (root / "VERSION").read_text().strip()
if version != "26.2.3":
    parser.error(f"expected audited baseline 26.2.3, got {version!r}")
files = (
    "src/intel/vulkan/anv_kmd_backend.h",
    "src/intel/vulkan/anv_kmd_backend.c",
    "src/intel/vulkan/anv_physical_device.c",
    "src/intel/vulkan/anv_device.c",
    "src/intel/vulkan/anv_wsi.c",
    "src/intel/vulkan/i915/anv_device.c",
    "src/intel/vulkan/i915/anv_kmd_backend.c",
    "src/intel/vulkan/i915/anv_queue.c",
    "src/intel/vulkan/meson.build",
)
print(f"Mesa {version}: source inventory only, no CuBit execution")
for name in files:
    data = (root / name).read_bytes()
    print(f"sha256 {hashlib.sha256(data).hexdigest()} {name}")
header = (root / files[0]).read_text()
print("Backend callbacks:")
for name in re.findall(r"\(\*(\w+)\)\s*\(", header):
    print(f"  {name}")
patterns = {
    "DRM/device discovery": r"drmGet|DRM_NODE_|intel_get_device_info_from_fd",
    "DRM sync": r"vk_drm_syncobj|wsi_device_setup_syncobj_fd",
    "thread primitives": r"pthread_|\bmtx_|\bcnd_|\bthrd_",
    "virtual memory": r"\bmmap\(|\bmunmap\(|\bmprotect\(",
    "kernel ioctl": r"\bioctl\(|drmIoctl|DRM_IOCTL_",
}
for category, pattern in patterns.items():
    print(f"{category} (lexical hits, includes comments; not exhaustive):")
    for name in files:
        for line, text in enumerate((root / name).read_text().splitlines(), 1):
            if re.search(pattern, text):
                print(f"  {name}:{line}: {text.strip()}")
