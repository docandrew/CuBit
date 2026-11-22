"""Build native Ada scene bridge archive in a private Nix snapshot.

No shared staging, runtime, Mesa output, index or live image is modified. This
is component compilation, not a replacement for native integration/boot tests.
"""
from pathlib import Path
import hashlib
import json
import os
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "tests/compositor/build"
OUT.mkdir(exist_ok=True)
SNAP = Path(tempfile.mkdtemp(prefix="native-scene-", dir=OUT))
inputs = {}


def digest(data):
    return hashlib.sha256(data).hexdigest()


def copy(source, destination):
    data = source.read_bytes()
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_bytes(data)
    original = str(source.relative_to(ROOT))
    if source.read_bytes() != data:
        raise RuntimeError(f"source changed during snapshot: {original}")
    inputs[original] = {"sha256": digest(data), "copy": str(destination.relative_to(SNAP))}


def tree(relative, destination=None):
    source = ROOT / relative
    for path in sorted(source.rglob("*")):
        if path.is_file():
            copy(path, SNAP / (destination or relative) / path.relative_to(source))


def verify():
    for original, info in inputs.items():
        if digest((ROOT / original).read_bytes()) != info["sha256"]:
            raise RuntimeError(f"input changed: {original}")
        if digest((SNAP / info["copy"]).read_bytes()) != info["sha256"]:
            raise RuntimeError(f"snapshot input was modified: {original}")


def run(args):
    print("RUN", *map(str, args), flush=True)
    subprocess.run(list(map(str, args)), cwd=SNAP, check=True,
                   env={**os.environ, "NIX_HARDENING_ENABLE": ""})


print(SNAP, flush=True)
try:
    # Copy only source files from these directories; do not copy prior outputs.
    for relative in ("userspace/lib/compositor", "userspace/lib/display"):
        for path in sorted((ROOT / relative).iterdir()):
            if path.suffix in (".ads", ".adb", ".c", ".h"):
                copy(path, SNAP / path.relative_to(ROOT))
    copy(ROOT / "userspace/mesa/service-device.h", SNAP / "userspace/mesa/service-device.h")
    tree("userspace/runtime/gnat")
    tree("userspace/runtime/adalib")
    for name in ("ada_source_path", "ada_object_path", "runtime.xml", "target_properties"):
        copy(ROOT / "userspace/runtime" / name, SNAP / "userspace/runtime" / name)
    for name in ("vulkan_submission_native.gpr", "vulkan_owned_targets_native.gpr",
                 "native_scene_bridge_native.gpr", "native_scene_bridge.ads", "native_scene_bridge.adb",
                 "native_scene_bridge.h"):
        copy(ROOT / "tests/compositor" / name, SNAP / "tests/compositor" / name)
    tree("userspace/libc/build/sysroot/include", "headers/musl")
    mesa = "userspace/mesa/build/source-aee5fe5697d39dd4/include"
    tree(mesa + "/vulkan", "headers/mesa/vulkan")
    tree(mesa + "/vk_video", "headers/mesa/vk_video")
    verify()
    (SNAP / "inputs.json").write_text(json.dumps(inputs, indent=2) + "\n")
    run(["gprbuild", "--version"])
    run(["gcc", "--version"])
    run(["gprbuild", "-p", "-c", "-P", "tests/compositor/native_scene_bridge_native.gpr"])
    objects = SNAP / "tests/compositor/build/native-scene-bridge-native"
    runtime = SNAP / "userspace/runtime"
    subprocess.run(["gnatbind", "-n", "-Lcompositor_scene", "--RTS=" + str(runtime),
                    "native_scene_bridge.ali"], cwd=objects, check=True)
    subprocess.run(["gnatmake", "-c", "-gnatA", "-gnat2022", "-O2", "-mno-red-zone",
                    "-fno-pic", "-mno-sse", "-mno-sse2", "--RTS=" + str(runtime),
                    "b~native_scene_bridge.adb"], cwd=objects, check=True)
    # Keep the new wallpaper recorder inside the archive so existing native
    # consumers do not need a second external adapter object just to link it.
    builtin = subprocess.check_output(["gcc", "-print-file-name=include"], text=True).strip()
    run(["gcc", "-std=c11", "-O2", "-ffreestanding", "-fno-pic", "-mno-red-zone",
         "-mno-sse", "-mno-sse2", "-nostdinc", "-isystem", builtin,
         "-Iheaders/musl", "-Iheaders/mesa", "-Iuserspace/lib/compositor",
         "-Wall", "-Wextra", "-Werror", "-c", "userspace/lib/compositor/vulkan_backdrop.c",
         "-o", objects / "vulkan_backdrop.o"])
    run(["gcc", "-std=c11", "-O2", "-ffreestanding", "-fno-pic", "-mno-red-zone",
         "-mno-sse", "-mno-sse2", "-nostdinc", "-isystem", builtin,
         "-Iheaders/musl", "-Iheaders/mesa", "-Iuserspace/lib/compositor",
         "-Wall", "-Wextra", "-Werror", "-c", "userspace/lib/compositor/vulkan_context.c",
         "-o", objects / "vulkan_context_native.o"])
    archive = SNAP / "libcubit-native-scene.a"
    run(["ar", "rcs", archive, *sorted(objects.glob("*.o"))])
    run(["nm", "-u", archive])
    verify()
    (SNAP / "result.json").write_text(json.dumps({"status": "PASS", "inputs": len(inputs),
        "scope": "native Ada archive with elaboration; no application link, boot or GPU execution"}, indent=2) + "\n")
    print("PASS native Ada scene archive:", SNAP, flush=True)
except Exception as error:
    (SNAP / "result.json").write_text(json.dumps({"status": "FAIL", "error": str(error)}, indent=2) + "\n")
    raise
