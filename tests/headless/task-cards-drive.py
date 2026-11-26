#!/usr/bin/env python3
"""UI-061 headless check (run.sh --test task-cards), driven through the run's
HMP monitor socket once both apps are up:

  python3 tests/headless/task-cards-drive.py SERIAL_LOG MONITOR_SOCKET OUT_PREFIX

- Task buttons name apps by their manifests' signed names: "Task Card Check"
  (two windows) and "Calculator" (one), never a window's title. The test
  app titles its first window "Files" and renames it "Calculator" at run
  time; neither may reach its button.
- Resting the pointer on a button shows its hover card with every window's
  title and a thumbnail; focusing the button from the keyboard (Super+T,
  Right) shows it too.
- Writes OUT_PREFIX-hover.png (the card) for the record.

Exits non-zero with the reason on the first failed check.
"""
import re
import socket
import sys
import time
from pathlib import Path

serial, monitor, prefix = Path(sys.argv[1]), sys.argv[2], sys.argv[3]
SCREEN_W, SCREEN_H = 1024, 768
STEP = 70
#  Client_Tooltip_Policy.SHOW_DELAY_MS is 500; rest a little longer.
REST_S = 1.2

BUTTON = re.compile(r'desktop: task button (\d+) label="([^"]*)" source=(\w+) identity=(\S*) '
                    r'windows=(\d+) at (\d+), (\d+) size (\d+) x (\d+)')
CARD = re.compile(r'desktop: task card app="([^"]*)" source=(\w+) identity=(\S*) windows=(\d+) '
                  r'titles=\[(.*)\] thumbnail (\d+) x (\d+)')


def fail(reason):
    print(f"task-cards: FAIL {reason}", flush=True)
    sys.exit(1)


def log_text():
    return serial.read_text(errors="replace") if serial.exists() else ""


def wait_for(text, seconds=60):
    deadline = time.monotonic() + seconds
    while text not in log_text():
        if time.monotonic() > deadline:
            fail(f"timed out waiting for {text!r}")
        time.sleep(0.1)


def hmp(command):
    with socket.socket(socket.AF_UNIX) as s:
        s.settimeout(3)
        s.connect(monitor)
        s.sendall((command + "\n").encode())
        time.sleep(0.05)


pointer = [0, 0]


def reset_pointer():
    for _ in range(40):
        hmp(f"mouse_move -{STEP} -{STEP}")
        time.sleep(0.02)
    pointer[:] = [0, 0]


def move(x, y):
    while pointer != [x, y]:
        dx = max(-STEP, min(STEP, x - pointer[0]))
        dy = max(-STEP, min(STEP, y - pointer[1]))
        hmp(f"mouse_move {dx} {dy}")
        pointer[0] += dx
        pointer[1] += dy
        time.sleep(0.04)


def buttons():
    """The last logged state of each task button, by label."""
    found = {}
    for m in BUTTON.finditer(log_text()):
        found[m.group(2)] = dict(place=int(m.group(1)), source=m.group(3), identity=m.group(4),
                                 windows=int(m.group(5)), x=int(m.group(6)), y=int(m.group(7)),
                                 w=int(m.group(8)), h=int(m.group(9)))
    return found


def cards():
    return [dict(app=m.group(1), source=m.group(2), identity=m.group(3), windows=int(m.group(4)),
                 titles=m.group(5), w=int(m.group(6)), h=int(m.group(7)))
            for m in CARD.finditer(log_text())]


def shot(path):
    ppm = Path(str(path) + ".ppm")
    hmp(f'screendump "{ppm}"')
    for _ in range(50):
        if ppm.exists() and ppm.stat().st_size > SCREEN_W * SCREEN_H * 3:
            break
        time.sleep(0.1)
    time.sleep(0.3)
    try:
        from PIL import Image
        Image.open(ppm).save(str(path) + ".png")
        ppm.unlink()
    except ImportError:
        pass


wait_for("task-card-check: renamed its window Calculator")
wait_for("calculator: first frame presented")
deadline = time.monotonic() + 30
while time.monotonic() < deadline:
    b = buttons()
    if b.get("Task Card Check", {}).get("windows") == 2 and "Calculator" in b:
        break
    time.sleep(0.2)
b = buttons()
own = b.get("Task Card Check")
if own is None or own["windows"] != 2 or own["source"] != "manifest" \
        or own["identity"] != "com.cubit.task-card-check":
    fail(f"the test app's button is not its manifest name over two windows: {b}")
calc = b.get("Calculator")
if calc is None or calc["source"] != "manifest" or calc["identity"] != "com.cubit.calculator" \
        or calc["windows"] != 1:
    fail(f"Calculator's button is not its manifest name: {b}")
if "Files" in b:
    fail("a window's title became a task button label")
print("task-cards: buttons name apps by their manifests", flush=True)

# The run-time rename reached the title, never the button.
if not re.search(r'desktop: window title set surface \d+ title="Calculator" button="Task Card Check"', log_text()):
    fail("the renamed window's button did not keep the app's name")
print("task-cards: a renamed window keeps its app's button", flush=True)

# Hover: the card lists both titles and has a thumbnail.
seen = len(cards())
reset_pointer()
move(own["x"] + own["w"] // 2, own["y"] + own["h"] // 2)
time.sleep(REST_S)
deadline = time.monotonic() + 10
while len(cards()) == seen and time.monotonic() < deadline:
    time.sleep(0.1)
if len(cards()) == seen:
    fail("no hover card after resting on the button")
card = cards()[-1]
if card["app"] != "Task Card Check" or card["windows"] != 2 or card["source"] != "manifest":
    fail(f"the card names the wrong app: {card}")
if '"Calculator"' not in card["titles"] or '"Second window"' not in card["titles"]:
    fail(f"the card does not list both titles: {card}")
if card["w"] == 0 or card["h"] == 0:
    fail(f"the card has no thumbnail: {card}")
time.sleep(0.5)
shot(prefix + "-hover")
print(f"task-cards: hover card titles=[{card['titles']}] thumbnail {card['w']}x{card['h']}", flush=True)

# Keyboard: away from the bar, then Super+T and Right to the first button.
seen = len(cards())
move(SCREEN_W // 2, SCREEN_H // 3)
time.sleep(0.5)
for key in ("meta_l-t", "right"):
    hmp(f"sendkey {key}")
    time.sleep(0.3)
time.sleep(REST_S)
deadline = time.monotonic() + 10
while len(cards()) == seen and time.monotonic() < deadline:
    time.sleep(0.1)
if len(cards()) == seen:
    fail("no hover card for the keyboard-focused button")
card = cards()[-1]
if card["windows"] < 1:
    fail(f"the keyboard card is empty: {card}")
shot(prefix + "-keyboard")
hmp("sendkey esc")
print(f"task-cards: keyboard card app={card['app']}", flush=True)
print("task-cards: PASS", flush=True)
