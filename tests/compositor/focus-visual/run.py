"""Nix-only native Desktop/Mesa software-startup regression with explicit seeds.

Kernel/initrd/services are prebuilt seeds, not current-source rebuilds. All
output is private. These seeds exercise software startup; admitted hardware
startup is a separate gate. Check two overlapping titlebars across native keyboard focus changes at 100% DPI.
"""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import socket
import subprocess
import time
from PIL import Image, ImageChops

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("linked", type=Path)
parser.add_argument("seed", type=Path)
parser.add_argument("output", type=Path, help="new evidence directory")
parser.add_argument("--approve-render", action="store_true",
                    help="optional-render fixture: unavailable GPU must retry with a fresh software child")
parser.add_argument("--toolchain-root",type=Path,default=Path(__file__).resolve().parents[3])
args = parser.parse_args()
root = args.toolchain_root.resolve()
assert os.environ.get("IN_NIX_SHELL"), "Use Nix"
linked, seed, output = args.linked.resolve(), args.seed.resolve(), args.output.resolve()
guard_path = root / "tools/verify_desktop_vulkan_compositor.py"
guard_spec = importlib.util.spec_from_file_location("compositor_guard", guard_path)
guard = importlib.util.module_from_spec(guard_spec)
guard_spec.loader.exec_module(guard)
executable = guard.verify(linked)
manifest_path = linked / "compositor-result.json"
manifest = json.loads(manifest_path.read_text())
assert manifest.get("build_variant", {}).get("metrics") == "off", "Use metrics-off diagnostic artifact"

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()
output.mkdir(parents=True, exist_ok=False)
stage = output / "stage"
stage.mkdir()
names = ("cubit_kernel", "initrd.img", "display.svc", "clock.svc", "logstore.svc", "focus.app")
builder = root / "tools/build_development_disk.py"
inputs = {str(p): digest(p) for p in
          [*[seed / n for n in names], executable, manifest_path, guard_path, Path(__file__).resolve(), builder]}
(output / "inputs.json").write_text(json.dumps(inputs, indent=2) + "\n")
for name in names:
    shutil.copyfile(seed / name, stage / name)
    assert digest(stage / name) == inputs[str(seed / name)]
shutil.copyfile(executable, stage / "desktop.svc")
assert digest(stage / "desktop.svc") == inputs[str(executable)]
shutil.copyfile(__file__, output / "runner.py")
serial, monitor = output / "serial.log", output / "monitor.sock"
result = {"status": "INCOMPLETE", "gpu_enabled": False,
          "scope": "native CuBit runtime-dispatch software fallback with explicit boot seeds",
          "binary_sha256": inputs[str(executable)], "memory": "1G",
          "render_approved": args.approve_render}
vm = None
def run(*command):
    with (output / "build.log").open("a") as log:
        subprocess.run(command, cwd=root, stdout=log, stderr=subprocess.STDOUT, check=True)
def text():
    return serial.read_text(errors="replace") if serial.exists() else ""
def healthy():
    assert vm.poll() is None, "QEMU exited unexpectedly"
    assert not re.search(r"USER-MEMORY-FAULT|EXCEPTION:|TEST: FAIL|KERNEL PANIC", text())
def wait(predicate, seconds=45):
    end = time.monotonic() + seconds
    while time.monotonic() < end:
        healthy()
        if predicate():
            return
        time.sleep(0.1)
    raise TimeoutError("Native Desktop condition timed out")
def command(value):
    def prompt(sock):
        data = b""
        while not data.endswith(b"(qemu) "):
            part = sock.recv(65536)
            if not part:
                raise RuntimeError("QEMU monitor closed")
            data += part
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as sock:
        sock.settimeout(5)
        sock.connect(str(monitor))
        prompt(sock)
        sock.sendall((value + "\n").encode())
        prompt(sock)
def screenshot(name):
    path = output / (name + ".ppm")
    command("screendump " + str(path))
    with Image.open(path) as image:
        frame = image.convert("RGB")
        frame.save(path.with_suffix(".png"))
        return frame
try:
    profile = output / "init.ccl"
    profile.write_text('(startup v1 (start "logstore.svc" (priority 5)) '
                       '(start "clock.svc" (priority 5)) '
                       '(start "display.svc" (priority 5)) '
                       '(start "desktop.svc" (priority 4)' +
                       (' (render approve-declared)' if args.approve_render else '') + ')'
                       ' (start "focus.app" (priority 3)))')
    run("python3", str(builder), str(output / "desktop.img"), "--boot",
        *[str(stage / n) for n in ("logstore.svc", "clock.svc", "display.svc", "desktop.svc", "focus.app")],
        "--file", "init.ccl=" + str(profile))
    iso = output / "iso"
    (iso / "boot/grub").mkdir(parents=True)
    for name in ("cubit_kernel", "initrd.img"):
        shutil.copyfile(stage / name, iso / "boot" / name)
    (iso / "boot/grub/grub.cfg").write_text(
        'serial --speed=115200 --unit=0\nterminal_output serial\nset timeout=0\n'
        'set default=0\nmenuentry "Desktop Vulkan compositor fallback" {\n'
        ' multiboot /boot/cubit_kernel\n set gfxpayload=1024x768x32\n'
        ' module /boot/initrd.img init.img\n}\n')
    run("grub-mkrescue", "-o", str(output / "boot.iso"), str(iso))
    with (output / "qemu.log").open("w") as log:
        vm = subprocess.Popen([
            "qemu-system-x86_64", "-accel", "tcg,thread=multi", "-machine", "q35",
            "-cpu", "Broadwell", "-smp", "4", "-m", "1G", "-cdrom", str(output / "boot.iso"),
            "-display", "none", "-serial", "file:" + str(serial),
            "-monitor", "unix:" + str(monitor) + ",server,nowait",
            "-vga", "none", "-device", "virtio-vga,xres=1024,yres=768",
            "-drive", "file=" + str(output / "desktop.img") + ",if=none,id=nvme0,format=raw",
            "-device", "nvme,serial=cubitnvme,drive=nvme0", "-no-reboot"],
            cwd=root, stdout=log, stderr=log)
    wait(lambda: "desktop: internal shell active" in text(), 90 if args.approve_render else 45)
    wait(lambda: "DESKTOP-VULKAN: startup=SOFTWARE" in text(), 30)
    startup_text = text()
    assert "DESKTOP-BOOTSTRAP: unconfirmed" not in startup_text
    assert startup_text.index("DESKTOP-BOOTSTRAP: released; renderer may start") < startup_text.index("DESKTOP-CHECKPOINT: INIT_BEFORE")
    assert "DESKTOP-CHECKPOINT: COMPLETE" not in startup_text[:startup_text.index("DESKTOP-CHECKPOINT: INIT_BEFORE")]
    assert "DESKTOP-CHECKPOINT: SUBMIT" not in startup_text[:startup_text.index("DESKTOP-CHECKPOINT: INIT_BEFORE")]
    assert text().count("DESKTOP-VULKAN: startup=SOFTWARE") == 1
    assert "DESKTOP-VULKAN: startup=READY" not in text()
    assert "DESKTOP-VULKAN: frame=" not in text()
    assert "procmgr: render software-only admitted" in text()
    assert text().count("DESKTOP-OPTIONAL-RENDER: PASS empty slot") == 1
    attempts = re.findall(r'procmgr: render attempt incarnation=\s*(\d+) software=(TRUE|FALSE)', text())
    if args.approve_render:
        assert "procmgr: render admission denied; child not resumed" in text()
        assert "procmgr: render retry software with fresh child" in text()
        assert len(attempts) == 2 and [a[1] for a in attempts] == ["FALSE", "TRUE"]
        assert attempts[0][0] != attempts[1][0]
    else:
        assert len(attempts) == 1 and attempts[0][1] == "TRUE"
    assert all(int(identity) >> 32 and int(identity) % 2**32 for identity, _ in attempts)
    result.update(render_attempts=attempts, software_fallback="PASS", hardware_validated=False)
    wait(lambda: "TEST: focus windows ready" in text(), 30)
    time.sleep(2)
    baseline=screenshot("focus-initial")
    area=(0,64,1024,700)
    frames=[]
    for cycle in range(6):
        command("sendkey alt-tab");time.sleep(1)
        frame=screenshot("focus-cycle-"+str(cycle))
        frames.append(frame)
        if cycle>=2:
            assert ImageChops.difference(frame.crop(area),frames[cycle-2].crop(area)).getbbox() is None, "Focus round trip left stale pixels"
    assert ImageChops.difference(frames[0].crop(area),frames[1].crop(area)).getbbox() is not None, "Focus did not change"
    for cycle in (1,3,5):
        frame=frames[cycle]
        assert ImageChops.difference(baseline.crop(area),frame.crop(area)).getbbox() is None, "Did not restore initial full redraw"
        for y in range(85,96):
            assert len({frame.getpixel((x,y)) for x in range(210,325)})==1
            rr,g,b=frame.getpixel((250,y))
            assert max(rr,g,b)-min(rr,g,b)<20, "Stale active title color"
    result.update(initial_full_redraw_restorations=3, old_title_exposed_strip="PASS")
    result.update(focus_cycles=6,focus_roundtrip_pixels="PASS")
    healthy()
    assert all(Path(p).is_file() and digest(Path(p)) == h for p, h in inputs.items())
    assert "DESKTOP-VULKAN: frame=" not in text()
    assert guard.verify(linked) == executable
    result.update(status="PASS", false_gpu_markers=0)
finally:
    if vm is not None and vm.poll() is None:
        vm.terminate()
        try:
            vm.wait(timeout=5)
        except subprocess.TimeoutExpired:
            vm.kill()
            vm.wait()
    (output / "result.json").write_text(json.dumps(result, indent=2) + "\n")
print(output)
