"""Nix-only native Desktop/Mesa software-startup regression with explicit seeds.

Kernel/initrd/services are prebuilt seeds, not current-source rebuilds. All
output is private. These seeds exercise software startup; admitted hardware
startup is a separate gate. Check keyboard-driven menu pixels.
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
from mesa_activation import verify as verify_mesa, verify_client_pixels, CLIENT, TEXT, SURFACE

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
parser.add_argument("--metrics-seed", type=Path,
                    help="directory containing metrics.svc and desktop-metrics-observer.app")
parser.add_argument("--metrics-stall", action="store_true",
                    help="use a test collector holding its first grant for 30 seconds")
parser.add_argument("--mesa-client", type=Path, required=True,
                    help="matched native-mesa-window.app quadrant fixture (not animated cube)")
parser.add_argument("--observer-build", type=Path, required=True)
parser.add_argument("--platform-root", type=Path, required=True)
args = parser.parse_args()
root = args.platform_root.resolve()
assert os.environ.get("IN_NIX_SHELL"), "Use Nix"
linked, seed, output = args.linked.resolve(), args.seed.resolve(), args.output.resolve()
guard_path = root / "tools/verify_desktop_vulkan_compositor.py"
guard_spec = importlib.util.spec_from_file_location("compositor_guard", guard_path)
guard = importlib.util.module_from_spec(guard_spec)
guard_spec.loader.exec_module(guard)
executable = guard.verify(linked)
manifest_path = linked / "compositor-result.json"
manifest = json.loads(manifest_path.read_text())
assert manifest.get("software_renderer") == "mesa-softpipe", "Unified Mesa artifact required"
client = args.mesa_client.resolve()
client_name = "native-mesa-window.app"
metrics_on = manifest.get("build_variant", {}).get("metrics") == "on"
assert not args.metrics_stall, "Client startup changes collector hold timing; use the dedicated stall runner"
assert not args.metrics_stall or metrics_on, "Stall test needs a metrics-enabled artifact"
assert metrics_on, "This regression requires a metrics-enabled compositor"
assert bool(args.metrics_seed) == metrics_on, "Metrics build requires its explicit collector/observer seeds"

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()
client_record = client.with_name("fixture.json")
client_manifest = json.loads(client_record.read_text())
assert client_manifest.get("status") == "LINK_PASS" and client_manifest.get("variant") == "non-cube"
client_inputs = dict(client_manifest["inputs_sha256"])
client_inputs.update(client_manifest["output_sha256"])
assert client_inputs.get(str(client)) == digest(client)
assert all(Path(p).is_file() and digest(Path(p)) == h for p,h in client_inputs.items())
client_inputs[str(client_record)] = digest(client_record)
observer_dir = args.observer_build.resolve()
observer = observer_dir / "desktop-log-observer.app"
observer_result = observer_dir / "result.json"
observer_inputs = observer_dir / "inputs.json"
assert json.loads(observer_result.read_text())["sha256"] == digest(observer)
assert all(digest(Path(p)) == h for p,h in json.loads(observer_inputs.read_text()).items())
output.mkdir(parents=True, exist_ok=False)
stage = output / "stage"
stage.mkdir()
names = ("cubit_kernel", "initrd.img", "display.svc", "clock.svc", "logstore.svc")
metric_names = (("desktop-metrics-stall.svc",) if args.metrics_stall else
                ("metrics.svc", "desktop-metrics-observer.app")) if metrics_on else ()
metric_seed = args.metrics_seed.resolve() if metrics_on else seed
builder = root / "tools/build_development_disk.py"
inputs = {str(p): digest(p) for p in
          [*[seed / n for n in names], *[metric_seed / n for n in metric_names], client, Path(__file__).with_name("mesa_activation.py"), executable, manifest_path, guard_path, Path(__file__).resolve(), builder, observer, observer_result, observer_inputs]}
for p,h in client_inputs.items():
    assert p not in inputs or inputs[p] == h
    inputs[p] = h
(output / "inputs.json").write_text(json.dumps(inputs, indent=2) + "\n")
for name in names:
    shutil.copyfile(seed / name, stage / name)
    assert digest(stage / name) == inputs[str(seed / name)]
for name in metric_names:
    shutil.copyfile(metric_seed / name, stage / name)
    assert digest(stage / name) == inputs[str(metric_seed / name)]
shutil.copyfile(client, stage / client_name)
assert digest(stage / client_name) == inputs[str(client)]
shutil.copyfile(observer, stage / observer.name)
assert digest(stage / observer.name) == inputs[str(observer)]
shutil.copyfile(executable, stage / "desktop.svc")
assert digest(stage / "desktop.svc") == inputs[str(executable)]
shutil.copyfile(__file__, output / "runner.py")
shutil.copyfile(Path(__file__).with_name("mesa_activation.py"), output / "mesa_activation.py")
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
                       '(start "display.svc" (priority 5)) ' +
                       ('(start "' + metric_names[0] + '" (priority 2)) ' if metrics_on else '') +
                       '(start "desktop.svc" (priority 4)' +
                       (' (render approve-declared)' if args.approve_render else '') + ')' +
                       (' (start "desktop-metrics-observer.app" (priority 3))' if metrics_on and not args.metrics_stall else '') + ' (start "desktop-log-observer.app" (priority 3)) (start "native-mesa-window.app" (priority 3)))')
    run("python3", str(builder), str(output / "desktop.img"), "--boot",
        *[str(stage / n) for n in ("logstore.svc", "clock.svc", "display.svc", "desktop.svc", observer.name, client_name, *metric_names)],
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
    if args.metrics_stall:
        wait(lambda: "TEST: metrics-stall grant held" in text())
    wait(lambda: all(marker in text() for marker in (CLIENT, TEXT, SURFACE)), 90)
    result["mesa_execution"] = verify_mesa(text(), manifest)
    held_restorations = 0
    time.sleep(2)
    command("mouse_move 0 80"); time.sleep(0.5)
    baseline = screenshot("desktop")
    result["client_pixels"] = verify_client_pixels(baseline)
    overlay = manifest.get("build_variant", {}).get("input_overlay", "on")
    assert overlay in ("on", "off")
    if overlay == "on":
        assert baseline.getpixel((899, 63)) == (32, 54, 72)
    result["input_overlay"] = overlay
    assert baseline.getextrema() != ((0, 0), (0, 0), (0, 0))
    counts = []
    for cycle in range(3):
        command("sendkey meta_l")
        time.sleep(2)
        opened = screenshot("menu-" + str(cycle))
        difference = ImageChops.difference(baseline, opened)
        changed = sum(1 for p in difference.get_flattened_data() if p != (0, 0, 0))
        assert changed > 10000, "Apps menu did not visibly open"
        counts.append(changed)
        command("sendkey esc")
        time.sleep(2)
        closed = screenshot("closed-" + str(cycle))
        # No mouse motion; exclude live clock/taskbar from restoration oracle.
        area = (0, 128, 1024, 700)
        assert ImageChops.difference(baseline.crop(area), closed.crop(area)).getbbox() is None
        if args.metrics_stall and "TEST: metrics-stall held page unchanged" not in text():
            held_restorations += 1
    if args.cursor_motion:
        # Native PS/2 input, no direct mutation of Desktop state. Check both old
        # cursor repair and the new footprint; clock/taskbar are outside the oracle.
        region = (0, 0, 1024, 700)
        reference = baseline.crop(region)
        moves = 0
        for cycle in range(8):
            x, y = 80, 160
            for dx, dy in ((64, 0), (0, 64), (-64, 0), (0, -64)):
                command(f"mouse_move {dx} {dy}")
                x += dx; y += dy
                deadline = time.monotonic() + 8
                while True:
                    healthy()
                    time.sleep(0.15)
                    observed = screenshot(f"cursor-{cycle}-{moves}").crop(region)
                    delta = ImageChops.difference(reference, observed)
                    delta.paste((0, 0, 0), (0, 0, 900, 128))
                    changed = delta.getbbox() is not None
                    if (x, y) == (80, 160):
                        ready = not changed
                    else:
                        delta.paste((0, 0, 0), (78, 158, 97, 186))
                        delta.paste((0, 0, 0), (x - 2, y - 2, x + 17, y + 26))
                        ready = changed and delta.getbbox() is None
                    if ready:
                        break
                    if time.monotonic() >= deadline:
                        raise AssertionError(f"Cursor footprint/restoration mismatch cycle={cycle} position={x},{y}")
                moves += 1
        result.update(cursor_moves=moves, cursor_roundtrips=8,
                      cursor_damage="PASS exact restoration and no changes outside old/new footprints")
    before_click = screenshot("indicator-before-click")
    command("mouse_button 1"); time.sleep(0.2); command("mouse_button 0")
    time.sleep(1)
    after_click = screenshot("indicator-after-click")
    click_delta = ImageChops.difference(before_click.crop((0,44,300,64)), after_click.crop((0,44,300,64))).getbbox()
    assert (click_delta is not None) if overlay == "on" else (click_delta is None)
    result["overlay_click_pixels"] = "counter changed" if overlay == "on" else "unchanged without HUD"
    if args.scaled_cursor_motion:
        pointer = [80, 160]  # Prior cursor round trips end at this logical position.
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
    if args.metrics_stall:
        wait(lambda: "TEST: metrics-stall held page unchanged checks=600" in text(), 60)
        # Loss is carried in the next published batch; generate fresh input
        # after the held grant is returned even if the pixel workload is done.
        for _ in range(3):
            command("sendkey shift")
            time.sleep(0.2)
        wait(lambda: "TEST: PASS metrics-stall resumed batches=" in text(), 60)
        assert held_restorations > 0, "No menu restoration completed during collector hold"
        assert "TEST: metrics-stall held page unchanged checks=600" in text()
        recovered = re.search(r"TEST: PASS metrics-stall resumed batches=\s*(\d+) dropped=\s*(\d+)", text())
        assert recovered and int(recovered[1]) > 2 and int(recovered[2]) > 0
        assert "desktop: metrics quarantined" not in text()
        result["metrics_stall"] = {"held_page_checks": 600,
            "menu_restorations_during_hold": held_restorations,
            "resumed_batches": int(recovered[1]), "producer_dropped": int(recovered[2])}
    elif metrics_on:
        wait(lambda: "TEST: PASS desktop-metrics frames=" in text(), 30)
        assert "stages=input,draw,submit byte-schema=valid fallback-transfer=zero" in text()
        assert "desktop: metrics quarantined" not in text()
        report = re.search(r"TEST: PASS desktop-metrics frames=\s*(\d+) batches=\s*(\d+) producer-dropped=\s*(\d+)", text())
        assert report, "Workload observer must explicitly report producer loss"
        result["metrics"] = {"status": "PASS", "frames": int(report[1]),
            "batches": int(report[2]), "producer_dropped_at_observer_exit": int(report[3]),
            "schema_rejections": 0, "batch_gaps": 0,
            "scope": "Authenticated progress and schema; loss snapshot at observer exit, not full-run sampling completeness"}
    assert "DESKTOP-VULKAN: frame=" not in text()
    assert guard.verify(linked) == executable
    wait(lambda: "TEST: PASS desktop log channel authenticated startup/backend" in text(), 35)
    result["log_channel"] = "PASS authenticated ordered startup/backend records and reader close"
    result["observer_sha256"] = inputs[str(observer)]
    result["mesa_execution"] = verify_mesa(text(), manifest)
    result.update(status="PASS", menu_changed_pixels=counts, restored_cycles=3, false_gpu_markers=0)
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
