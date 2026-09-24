"""Exercise the native Desktop through PS/2 input; inspect both real scanouts.

This deliberately does not paint host-side stand-ins or inspect private guest
memory. Timing waits allow asynchronous presentation to settle, not benchmark it.
"""
import json
import os
from dataclasses import dataclass
from pathlib import Path
import socket
import sys
import time


def require(condition, explanation):
    if not condition:
        raise RuntimeError(explanation)


serial, socket_path, timeout = sys.argv[1:]
mixed = os.environ.get("CUBIT_TEST_MIXED_OUTPUTS") == "1"
side_width, side_height = (1280, 720) if mixed else (1024, 768)
handoff = os.environ.get("CUBIT_TEST_BOOT_HANDOFF") == "1"
main_width, main_height = (1280, 720) if handoff else (1024, 768)
if handoff:
    side_width, side_height = 1024, 768


@dataclass
class Image:
    width: int
    height: int
    pixels: bytes


deadline = time.monotonic() + float(timeout)
log = Path(serial)
while not Path(socket_path).exists():
    require(time.monotonic() < deadline, "QMP socket deadline")
    time.sleep(0.05)

with socket.socket(socket.AF_UNIX) as connection:
    connection.settimeout(3)
    connection.connect(socket_path)
    stream = connection.makefile("rwb")
    require("QMP" in json.loads(stream.readline()), "QMP greeting")

    def command(name, arguments=None):
        request = {"execute": name}
        if arguments is not None:
            request["arguments"] = arguments
        stream.write(json.dumps(request).encode() + b"\n")
        stream.flush()
        while True:
            response = json.loads(stream.readline())
            require("error" not in response, str(response))
            if "return" in response:
                return response["return"]

    def hmp(text):
        result = command("human-monitor-command", {"command-line": text})
        require(not result.strip(), f"input rejected: {result}")

    def capture(phase):
        time.sleep(0.8)
        images = []
        for head in range(2):
            path = log.with_suffix(f".{phase}-head-{head}.ppm")
            command("screendump", {"device": "multi-gpu", "head": head,
                                   "filename": str(path), "format": "ppm"})
            magic, dimensions, maximum, pixels = path.read_bytes().split(b"\n", 3)
            width, height = (main_width, main_height) if head == 0 else (side_width, side_height)
            require((magic, dimensions, maximum) == (b"P6", f"{width} {height}".encode(), b"255"),
                    "unexpected scanout geometry/format")
            require(len(pixels) == width * height * 3, "truncated capture")
            images.append(Image(width, height, pixels))
            if phase.startswith("settings-"):
                command("screendump", {"device": "multi-gpu", "head": head,
                                       "filename": str(path.with_suffix(".png")), "format": "png"})
        return images

    def region(image, x, y, w, h):
        require(0 <= x and x + w <= image.width and 0 <= y and y + h <= image.height,
                "test region outside scanout")
        return b"".join(image.pixels[(row * image.width + x) * 3:
                                     (row * image.width + x + w) * 3]
                        for row in range(y, y + h))

    pointer = [80, 80]

    def move(x, y):
        while pointer != [x, y]:
            dx = max(-70, min(70, x - pointer[0]))
            dy = max(-70, min(70, y - pointer[1]))
            hmp(f"mouse_move {dx} {dy}")
            pointer[0] += dx
            pointer[1] += dy
            time.sleep(0.06)

    def click():
        hmp("mouse_button 1")
        time.sleep(0.07)
        hmp("mouse_button 0")

    command("qmp_capabilities")
    while True:
        text = log.read_text(errors="replace") if log.exists() else ""
        if ("desktop: active outputs= 2 primary= 0" in text and
                "desktop: asynchronous frame released" in text and
                "ps2: consumer registered" in text):
            break
        require(time.monotonic() < deadline, "native Desktop startup deadline")
        time.sleep(0.05)

    # Park the cursor outside window-restoration comparison regions.
    move(20, 700)
    baseline = capture("baseline")
    if handoff:
        require("display: native boot handoff 1024 x 768 -> 1280 x 720; firmware rendering retired" in text,
                "fixture must change away from the actual boot framebuffer mode")
    if not mixed and not handoff:
        require(region(baseline[0], 100, 120, 800, 500) ==
                region(baseline[1], 100, 120, 800, 500), "per-output wallpaper differs")
        require(region(baseline[0], 0, 738, 1024, 30) !=
                region(baseline[1], 0, 738, 1024, 30), "taskbar missing on primary")
    elif side_height < main_height:
        # Move into the unlit space below the shorter secondary output. The
        # cursor must remain visible on its last row, not in the packed scene.
        move(1100, 760)
        pointer[1] = side_height - 1
        confined = capture("confined")
        require(region(confined[1], 70, side_height - 10, 40, 10) !=
                region(baseline[1], 70, side_height - 10, 40, 10),
                "pointer disappeared below short output")
        move(20, 700)
        restored = capture("restored")
        require(restored[1] == baseline[1], "confined cursor left artifacts")
    hmp("sendkey meta_l")
    time.sleep(0.3)
    # CCL Workbench is the first Apps entry; exercise an actual native client.
    hmp("sendkey ret")
    while "ccl-workbench: native window ready" not in log.read_text(errors="replace"):
        require(time.monotonic() < deadline, "Workbench launch deadline")
        time.sleep(0.05)
    original = capture("opened")
    require(region(original[0], 100, 120, 550, 260) !=
            region(baseline[0], 100, 120, 550, 260), "Workbench did not open")
    move(180, 90)
    hmp("mouse_button 1")
    move(main_width - 144, 90)
    spanning = capture("spanning")
    # Window moved 700 pixels; check both halves of its client interior.
    require(region(original[0], 110, 130, 90, 200) ==
            region(spanning[0], main_width - 214, 130, 90, 200), "primary window fragment wrong")
    require(region(original[0], 350, 130, 300, 200) ==
            region(spanning[1], 26, 130, 300, 200), "secondary window fragment wrong")
    move(main_width + 156, 90)
    hmp("mouse_button 0")
    secondary = capture("secondary")
    require(region(secondary[0], 80, 60, main_width - 80, main_height - 98) ==
            region(baseline[0], 80, 60, main_width - 80, main_height - 98), "old window/cursor paint not restored")
    require(region(original[0], 110, 130, 500, 200) ==
            region(secondary[1], 86, 130, 500, 200), "window not fully on secondary")
    # New double-click sequence after the drag's release has aged out.
    time.sleep(0.6)
    click()
    time.sleep(0.09)
    click()
    maximized = capture("maximized")
    require(region(maximized[0], 100, 120, 800, 500) ==
            region(baseline[0], 100, 120, 800, 500), "maximize changed primary")
    require(region(maximized[1], 10, side_height - 68, side_width - 30, 50) !=
            region(baseline[1], 10, side_height - 68, side_width - 30, 50), "maximize did not fill secondary")
    move(main_width + side_width - 21, 12)
    click()
    move(80, 80)
    closed = capture("closed")
    require(closed[1] == baseline[1], "secondary cursor/window artifacts after close")
    # Settings is compositor-owned but must display the same actual layout.
    hmp("sendkey meta_l")
    time.sleep(0.3)
    for _ in range(6):
        hmp("sendkey down")
        time.sleep(0.12)
    hmp("sendkey ret")
    appearance = capture("settings-appearance")
    require(region(appearance[0], 120, 180, 550, 170) !=
            region(baseline[0], 120, 180, 550, 170), "Settings did not open")
    move(154, 180)
    click()
    displays = capture("settings-displays")
    require(region(displays[0], 120, 180, 550, 170) !=
            region(appearance[0], 120, 180, 550, 170), "Displays tab did not change content")
    hmp("sendkey shift-tab")
    time.sleep(0.15)
    hmp("sendkey ret")
    returned = capture("settings-returned")
    # Focus moved from the Light button to the Appearance tab; compare content
    # below that button, not its intentionally changed keyboard-focus border.
    require(region(returned[0], 120, 210, 550, 140) ==
            region(appearance[0], 120, 210, 550, 140), "Settings keyboard tab navigation failed")
    if os.environ.get("CUBIT_TEST_ARRANGEMENT") == "1":
        import math
        import re

        move(154, 180)
        click()
        origins = [(0, 0), (main_width, 0)]
        sizes = [(main_width, main_height), (side_width, side_height)]

        def arrange(dx, dy, phase):
            old_primary = origins[0]
            right = max(origins[i][0] + sizes[i][0] for i in range(2))
            bottom = max(origins[i][1] + sizes[i][1] for i in range(2))
            divisor = max(1, math.ceil(right / 530), math.ceil(bottom / 152))
            # Settings stays at (96,72) on its monitor; client inset is (4,30).
            tx = 252 + (550 - right // divisor) // 2
            ty = 178 + (172 - bottom // divisor) // 2
            sx = old_primary[0] + tx + origins[1][0] // divisor + side_width // divisor // 2
            sy = old_primary[1] + ty + origins[1][1] // divisor + side_height // divisor // 2
            move(sx, sy)
            hmp("mouse_button 1")
            time.sleep(0.1)
            move(sx + round((old_primary[0] + dx - origins[1][0]) / divisor),
                 sy + round((old_primary[1] + dy - origins[1][1]) / divisor))
            hmp("mouse_button 0")
            capture(f"settings-{phase}-preview")
            before = len(log.read_text(errors="replace"))
            move(old_primary[0] + 648, old_primary[1] + 451)
            click()
            expected = [(-min(0, dx), -min(0, dy)), (max(0, dx), max(0, dy))]
            until = time.monotonic() + 5
            while True:
                text = log.read_text(errors="replace")[before:]
                if all(re.search(rf"desktop: display {i} origin (\d+), (\d+)", text)
                       for i in (1, 2)):
                    break
                require(time.monotonic() < until, f"{phase}: Apply did not complete")
                time.sleep(0.05)
            time.sleep(0.2)
            text = log.read_text(errors="replace")[before:]
            actual = []
            for i, (x, y) in enumerate(expected, 1):
                match = re.search(rf"desktop: display {i} origin (\d+), (\d+)", text)
                require(match is not None, f"{phase}: missing applied coordinates")
                point = tuple(map(int, match.groups()))
                # A miniature's pixel represents several desktop pixels; free
                # offsets are quantized, unlike exact snapped shared edges.
                require(abs(point[0] - x) <= divisor and abs(point[1] - y) <= divisor,
                        f"{phase}: wrong applied origins: {text[:300]}")
                actual.append(point)
            expected = actual
            pointer[0] += expected[0][0] - old_primary[0]
            pointer[1] += expected[0][1] - old_primary[1]
            origins[:] = expected
            image = capture(f"settings-{phase}-applied")
            require(region(image[0], 120, 76, 500, 20) == region(displays[0], 120, 76, 500, 20),
                    f"{phase}: Settings did not follow its monitor")
            require(region(image[0], 0, main_height - 36, 100, 36) ==
                    region(displays[0], 0, main_height - 36, 100, 36),
                    f"{phase}: primary taskbar moved within its physical monitor")

        arrange(0, -side_height, "above")
        # Cross the vertical seam in both directions, without host-side warping.
        move(850, side_height - 12)
        crossed = capture("settings-seam-crossed")
        require(region(crossed[1], 840, side_height - 20, 40, 20) !=
                region(baseline[1], 840, side_height - 20, 40, 20),
                "pointer did not cross the vertical seam")
        move(80, side_height + 80)
        restored = capture("settings-seam-restored")
        require(restored[1] == baseline[1], "secondary cursor artifacts after vertical seam crossing")
        # Settings is intentionally fixed-size. Launch a real resizable client
        # on the relocated primary and exercise its work-area calculation.
        ready_before = log.read_text(errors="replace").count("ccl-workbench: native window ready")
        hmp("sendkey meta_l")
        time.sleep(0.3)
        hmp("sendkey ret")
        until = time.monotonic() + 10
        while log.read_text(errors="replace").count("ccl-workbench: native window ready") <= ready_before:
            require(time.monotonic() < until, "Workbench launch on rearranged primary failed")
            time.sleep(0.05)
        capture("settings-above-client-opened")
        # This second client is cascaded below Settings, at physical Y=100.
        move(origins[0][0] + 180, origins[0][1] + 114)
        click()
        time.sleep(0.08)
        click()
        full = capture("settings-above-maximized")
        require(region(full[0], 4, 4, 600, 20) != region(baseline[0], 4, 4, 600, 20),
                "maximize ignored primary Y origin")
        move(origins[0][0] + main_width - 21, origins[0][1] + 12)
        click()
        capture("settings-above-client-closed")
        arrange(-side_width, 0, "left")
        arrange(0, main_height, "below")
        arrange(main_width, 128, "offset")
        arrange(main_width, 0, "original")
        if os.environ.get("CUBIT_TEST_PRIMARY") == "1":
            divisor = max(1, math.ceil((main_width + side_width) / 530),
                          math.ceil(max(main_height, side_height) / 152))
            tx = 252 + (550 - (main_width + side_width) // divisor) // 2
            ty = 178 + (172 - max(main_height, side_height) // divisor) // 2

            def select_screen(index):
                x = 0 if index == 0 else main_width
                w, h = sizes[index]
                move(tx + x // divisor + w // divisor // 2,
                     ty + h // divisor // 2)
                click()

            def make_primary():
                move(330, 420)
                click()

            def apply_primary(number, x_offset=0):
                before = len(log.read_text(errors="replace"))
                move(648 + x_offset, 451)
                click()
                until = time.monotonic() + 5
                while f"desktop: primary display {number}" not in log.read_text(errors="replace")[before:]:
                    require(time.monotonic() < until, "primary Apply did not complete")
                    time.sleep(0.05)
                time.sleep(0.2)
                text = log.read_text(errors="replace")[before:]
                require(f"desktop: primary display {number}" in text, "wrong primary selected")

            select_screen(1)
            move(700, 530)  # Keep the cursor out of diagram pixel comparisons.
            selected = capture("settings-primary-selected")
            before = log.read_text(errors="replace").count("desktop: arrangement applied")
            make_primary()
            pending = capture("settings-primary-pending")
            require(region(pending[0], 252, 178, 550, 172) !=
                    region(selected[0], 252, 178, 550, 172), "primary preview bar did not move")
            require(log.read_text(errors="replace").count("desktop: arrangement applied") == before,
                    "preview changed live primary before Apply")
            move(758, 451)
            click()
            reverted = capture("settings-primary-reverted")
            require(region(reverted[0], 252, 178, 550, 172) ==
                    region(selected[0], 252, 178, 550, 172), "Revert did not restore primary preview")
            make_primary()
            apply_primary(2)
            changed = capture("settings-primary-applied")
            require(region(changed[0], 0, main_height - 36, 100, 36) !=
                    region(displays[0], 0, main_height - 36, 100, 36), "old primary retained taskbar")
            require(region(changed[1], 0, side_height - 36, 100, 36) ==
                    region(displays[0], 0, main_height - 36, 100, 36), "new primary lacks Apps button")
            ready_before = log.read_text(errors="replace").count("ccl-workbench: native window ready")
            hmp("sendkey meta_l")
            time.sleep(0.3)
            hmp("sendkey ret")
            until = time.monotonic() + 10
            while log.read_text(errors="replace").count("ccl-workbench: native window ready") <= ready_before:
                require(time.monotonic() < until, "launch on new primary failed")
                time.sleep(0.05)
            opened = capture("settings-primary-client")
            require(region(opened[1], 140, 140, 500, 260) !=
                    region(changed[1], 140, 140, 500, 260), "client did not open on new primary")
            move(main_width + 180, 114)
            click()
            time.sleep(0.08)
            click()
            maximized = capture("settings-primary-maximized")
            require(region(maximized[1], 4, 4, 600, 20) !=
                    region(baseline[1], 4, 4, 600, 20), "new-primary maximize failed")
            select_screen(0)
            make_primary()
            apply_primary(1)
            restored = capture("settings-primary-restored")
            require(region(restored[0], 0, main_height - 36, 100, 36) ==
                    region(displays[0], 0, main_height - 36, 100, 36), "primary taskbar not restored")
            require(region(restored[1], 200, side_height - 30, 400, 20) !=
                    region(maximized[1], 200, side_height - 30, 400, 20),
                    "maximized window did not reclaim former taskbar area")
            print("PASS native primary: preview/Revert/Apply, taskbar migration, new-client placement "
                  "and maximized work-area adjustment", flush=True)
            if os.environ.get("CUBIT_TEST_SCALING") == "1":
                # Close the maximized client so cursor restoration can compare
                # static wallpaper pixels on the scaled output exactly.
                move(main_width + side_width - 21, 12)
                click()
                select_screen(1)
                move(80, 80)
                unscaled = capture("settings-scale-original")

                def scale_button(up, x_offset=0):
                    move((721 if up else 679) + x_offset, 420)
                    click()
                    time.sleep(0.1)

                scale_button(True)
                apply_primary(1)
                require("desktop: display 2 scale 5/ 4" in log.read_text(errors="replace"),
                        "125% scale not applied")
                capture("settings-scale-125")
                scale_button(True)
                apply_primary(1)
                scaled = capture("settings-scale-150")
                require("desktop: display 2 scale 3/ 2" in log.read_text(errors="replace"),
                        "150% scale not applied")
                move(main_width + 150, 100)
                cursor = capture("settings-scale-cursor")
                require(region(cursor[1], 220, 145, 45, 45) !=
                        region(scaled[1], 220, 145, 45, 45), "scaled pointer mapping failed")
                move(80, 80)
                clean = capture("settings-scale-cursor-restored")
                require(clean[1] == scaled[1], "scaled cursor left stale pixels")
                scale_button(True)  # 175% would leave less than 800x480.
                rejected = capture("settings-scale-rejected")
                require(region(rejected[0], 252, 178, 550, 172) ==
                        region(scaled[0], 252, 178, 550, 172), "unsafe scale changed pending layout")
                scale_button(False)
                scale_button(False)
                apply_primary(1)
                restored = capture("settings-scale-restored")
                require(restored[1] == unscaled[1], "100% restore changed unscaled scanout")

                # Scale the primary too: logical width contracts and its
                # neighbor follows the shared edge without changing GPU modes.
                select_screen(0)
                scale_button(True)
                apply_primary(1)
                primary_scaled = capture("settings-scale-primary")
                logical_width = math.ceil(main_width * 4 / 5)
                require(f"desktop: display 2 origin {logical_width}, 0" in
                        log.read_text(errors="replace"), "scaled primary left a desktop gap")
                move(logical_width + 10, 100)
                seam = capture("settings-scale-primary-seam")
                require(region(seam[1], 0, 90, 45, 45) !=
                        region(primary_scaled[1], 0, 90, 45, 45), "mixed-scale seam crossing failed")
                move(80, 80)
                # Settings is kept wholly within the smaller primary work area.
                settings_dx = min(96, logical_width - 742) - 96
                scale_button(False, settings_dx)
                apply_primary(1, settings_dx)
                capture("settings-scale-primary-restored")
                print("PASS native scaling: sidebar, 125/150%, unchanged modes, workspace floor, "
                      "cursor repair, primary reflow and mixed-scale seam", flush=True)
        print("PASS native arrangement: above/left/below/offset/restore, unchanged modes, "
              "window/taskbar relocation and vertical pointer seam", flush=True)
    print("PASS native Desktop: primary taskbar, split drag, per-monitor maximize, "
          "wallpaper/cursor restoration, Settings display view", flush=True)
