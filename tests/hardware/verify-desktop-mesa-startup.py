"""Validate an explicit opt-in Desktop link artifact, without executing it."""
import argparse
import hashlib
import json
from pathlib import Path


def verify(directory):
    directory = Path(directory).resolve()
    record = json.loads((directory / "result.json").read_text())
    binary = directory / "desktop-vulkan-link.svc"
    data = binary.read_bytes()
    required = {
        "status": "LINKED",
        "admitted_device_startup_enabled": True,
        "admitted_startup": True,
        "optional_render_probe": True,
        "gpu_drawing_enabled": False,
    }
    for name, expected in required.items():
        if record.get(name) != expected:
            raise ValueError(f"candidate {name} must be {expected!r}")
    if not data.startswith(b"\x7fELF"):
        raise ValueError("candidate is not an ELF executable")
    if hashlib.sha256(data).hexdigest() != record.get("binary_sha256"):
        raise ValueError("candidate binary hash mismatch")
    if len(data) != record.get("binary_bytes"):
        raise ValueError("candidate binary size mismatch")
    return binary


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    args = parser.parse_args()
    try:
        print(verify(args.directory))
    except (OSError, ValueError, TypeError) as error:
        parser.exit(1, f"Desktop startup artifact rejected: {error}\n")
