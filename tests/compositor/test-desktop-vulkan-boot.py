"""Nix-only native Desktop/Mesa software-startup regression with explicit seeds.

Kernel/initrd/services are prebuilt seeds, not current-source rebuilds. All
output is private. These seeds exercise software startup; admitted hardware
startup is a separate gate. Check keyboard-driven menu pixels.
"""
import argparse
import hashlib
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
parser.add_argument("--cursor-motion", action="store_true",
                    help="check native mouse motion and exact old/new cursor damage restoration")
parser.add_argument("--scaled-cursor-motion", action="store_true",
                    help="apply 125 percent through native Settings and check scaled cursor damage")
args = parser.parse_args()
root = Path(__file__).resolve().parents[2]
assert os.environ.get("IN_NIX_SHELL"), "Use Nix"
linked, seed, output = args.linked.resolve(), args.seed.resolve(), args.output.resolve()
executable = linked / "desktop-vulkan-link.svc"
manifest = json.loads((linked / "result.json").read_text())
def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()
assert manifest["status"] == "LINKED" and not manifest["gpu_enabled"]
assert not args.approve_render or manifest.get("optional_render_probe")
assert digest(executable) == manifest["binary_sha256"]
output.mkdir(parents=True, exist_ok=False)
stage = output / "stage"
stage.mkdir()
names = ("cubit_kernel", "initrd.img", "display.svc", "clock.svc", "logstore.svc")
builder = root / "tools/build_development_disk.py"
inputs = {str(p): digest(p) for p in
          [*[seed / n for n in names], executable, Path(__file__).resolve(), builder]}
(output / "inputs.json").write_text(json.dumps(inputs, indent=2) + "\n")
for name in names:
    shutil.copyfile(seed / name, stage / name)
    assert digest(stage / name) == inputs[str(seed / name)]
shutil.copyfile(executable, stage / "desktop.svc")
assert digest(stage / "desktop.svc") == inputs[str(executable)]
shutil.copyfile(__file__, output / "runner.py")
serial, monitor = output / "serial.log", output / "monitor.sock"
result = {"status": "INCOMPLETE", "gpu_enabled": False,
          "scope": ("native CuBit legacy Desktop with explicit boot seeds" if manifest.get("backend") == "legacy" else
                    "native CuBit software Desktop with linked Mesa and explicit boot seeds"),
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
                       (' (render approve-declared)' if args.approve_render else '') + '))')
    run("python3", str(builder), str(output / "desktop.img"), "--boot",
        *[str(stage / n) for n in ("logstore.svc", "clock.svc", "display.svc", "desktop.svc")],
        "--file", "init.ccl=" + str(profile))
    iso = output / "iso"
    (iso / "boot/grub").mkdir(parents=True)
    for name in ("cubit_kernel", "initrd.img"):
        shutil.copyfile(stage / name, iso / "boot" / name)
    (iso / "boot/grub/grub.cfg").write_text(
        'serial --speed=115200 --unit=0\nterminal_output serial\nset timeout=0\n'
        'set default=0\nmenuentry "Desktop Mesa startup" {\n'
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
    if manifest.get("device_startup_probe"):
        assert "DESKTOP-GPU-STARTUP: PASS no authority" in text()
        assert "DESKTOP-GPU-STARTUP: FAIL" not in text()
        result["device_startup_probe"] = "PASS no authority"
    if manifest.get("admitted_startup"):
        assert text().count("DESKTOP-VULKAN: startup=SOFTWARE") == 1
        assert "DESKTOP-VULKAN: admitted endpoint; starting Mesa" not in text()
        assert "DESKTOP-VULKAN: startup=READY" not in text()
        assert "DESKTOP-VULKAN: invalid render authority" not in text()
        result["admitted_startup"] = "PASS software branch; hardware branch untested"
    if manifest.get("prepare_targets"):
        assert text().count("DESKTOP-VULKAN: targets skipped; device unavailable") == 1
        assert "DESKTOP-VULKAN: targets ready=" not in text()
        result["prepare_targets"] = "PASS no allocation without ready device"
    if manifest.get("prepare_pipeline"):
        assert "DESKTOP-VULKAN: pipeline ready=" not in text()
        result["prepare_pipeline"] = "PASS no pipeline creation without targets"
    if manifest.get("optional_render_probe"):
        assert "procmgr: render software-only admitted" in text()
        assert "DESKTOP-OPTIONAL-RENDER: PASS empty slot" in text()
        assert "DESKTOP-OPTIONAL-RENDER: FAIL" not in text()
        result["optional_render_probe"] = "PASS empty slot"
        attempts = re.findall(r'procmgr: render attempt incarnation=\s*(\d+) software=(TRUE|FALSE)', text())
        if args.approve_render:
            assert "procmgr: render admission denied; child not resumed" in text()
            assert "procmgr: render retry software with fresh child" in text()
            assert len(attempts) == 2 and [a[1] for a in attempts] == ["FALSE", "TRUE"]
            assert attempts[0][0] != attempts[1][0]
        else:
            assert len(attempts) == 1 and attempts[0][1] == "TRUE"
        assert all(int(identity) >> 32 and int(identity) % 2**32 for identity, _ in attempts)
        assert text().count("DESKTOP-OPTIONAL-RENDER: PASS empty slot") == 1
        result["render_attempts"] = attempts
    time.sleep(2)
    baseline = screenshot("desktop")
    assert baseline.getextrema() != ((0, 0), (0, 0), (0, 0))
    counts = []
    for cycle in range(3):
        command("sendkey meta_l")
        time.sleep(2)
        opened = screenshot("menu-" + str(cycle))
        difference = ImageChops.difference(baseline, opened)
        changed = sum(1 for p in difference.getdata() if p != (0, 0, 0))
        assert changed > 10000, "Apps menu did not visibly open"
        counts.append(changed)
        command("sendkey esc")
        time.sleep(2)
        closed = screenshot("closed-" + str(cycle))
        # No mouse motion; exclude live clock/taskbar from restoration oracle.
        area = (0, 0, 1024, 700)
        assert ImageChops.difference(baseline.crop(area), closed.crop(area)).getbbox() is None
    if args.cursor_motion:
        # Native PS/2 input, no direct mutation of Desktop state. Check both old
        # cursor repair and the new footprint; clock/taskbar are outside the oracle.
        region = (0, 0, 1024, 700)
        reference = baseline.crop(region)
        moves = 0
        for cycle in range(8):
            x, y = 80, 80
            for dx, dy in ((64, 0), (0, 64), (-64, 0), (0, -64)):
                command(f"mouse_move {dx} {dy}")
                x += dx; y += dy
                deadline = time.monotonic() + 8
                while True:
                    healthy()
                    time.sleep(0.15)
                    observed = screenshot(f"cursor-{cycle}-{moves}").crop(region)
                    delta = ImageChops.difference(reference, observed)
                    changed = delta.getbbox() is not None
                    if (x, y) == (80, 80):
                        ready = not changed
                    else:
                        delta.paste((0, 0, 0), (78, 78, 97, 106))
                        delta.paste((0, 0, 0), (x - 2, y - 2, x + 17, y + 26))
                        ready = changed and delta.getbbox() is None
                    if ready:
                        break
                    if time.monotonic() >= deadline:
                        raise AssertionError(f"Cursor footprint/restoration mismatch cycle={cycle} position={x},{y}")
                moves += 1
        result.update(cursor_moves=moves, cursor_roundtrips=8,
                      cursor_damage="PASS exact restoration and no changes outside old/new footprints")
    if args.scaled_cursor_motion:
        pointer = [80, 80]
        def move(x, y):
            while pointer != [x, y]:
                dx, dy = [max(-60, min(60, a-b)) for a,b in zip((x,y), pointer)]
                command(f"mouse_move {dx} {dy}")
                pointer[0] += dx; pointer[1] += dy
                time.sleep(0.07)
        def click(x, y):
            move(x, y); command("mouse_button 1"); time.sleep(0.1)
            command("mouse_button 0"); time.sleep(0.4)
        command("sendkey meta_l"); time.sleep(0.3)
        for _ in range(2):
            command("sendkey up"); time.sleep(0.15)
        command("sendkey ret"); time.sleep(1)
        screenshot("settings-open")
        click(154, 180)
        screenshot("settings-displays")
        scale_results = []
        for n,d in ((5,4),):
            before = len(text())
            click(721, 420); click(648, 451)
            wait(lambda: re.search(rf"desktop: display\s*1 scale\s*{n}/\s*{d}", text()[before:]) is not None, 12)
            time.sleep(0.5)
            screenshot(f"settings-scale-{n}-{d}")
            move(16,16); time.sleep(0.5)
            reference = screenshot(f"cursor-base-{n}-{d}").crop((0,0,1024,700))
            def bounds(x,y):
                return ((x-2)*n//d, (y-2)*n//d, ((x+17)*n+d-1)//d, ((y+26)*n+d-1)//d)
            for cycle in range(4):
                for x,y in ((40,16),(40,40),(16,40),(16,16)):
                    move(x,y)
                    deadline=time.monotonic()+8
                    while True:
                        healthy(); time.sleep(0.15)
                        observed=screenshot(f"cursor-{n}-{d}-{cycle}-{x}-{y}").crop((0,0,1024,700))
                        diff=ImageChops.difference(reference,observed)
                        changed=diff.getbbox() is not None
                        if (x,y)==(16,16): ready=not changed
                        else:
                            diff.paste((0,0,0),bounds(16,16));diff.paste((0,0,0),bounds(x,y))
                            ready=changed and diff.getbbox() is None
                        if ready: break
                        if time.monotonic()>=deadline: raise AssertionError(f"Scaled cursor damage {n}/{d} {x},{y}")
            scale_results.append([n,d])
            result.update(scaled_cursor_moves=16,scaled_cursor_roundtrips=4)
        result.update(native_scales=scale_results, output_pixels=[1024,768])
    healthy()
    assert all(Path(p).is_file() and digest(Path(p)) == h for p, h in inputs.items())
    result.update(status="PASS", menu_changed_pixels=counts, restored_cycles=3)
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
