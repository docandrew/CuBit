#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Every input driver holds input urgency the same way (SCHED-001).

For ps2.drv, xhci.drv, virtio-input.drv and hid-i2c.drv, from the sources:

  * its manifest declares an optional "urgency" scheduling request of
    Input_Urgency's share (Budget_Us every Period_Us);
  * devmgr gives it the declared share on the path that launches it
    (Give_Declared_Scheduling in its setup, or the generic ACPI launch);
  * its main reserves it through Input_Urgency.Reserve, which reserves an
    INTERACTIVE contract Input_Wakes_Only;
  * its reports reach Desktop by an input wake: a source ring's signal
    (CuBit.Input_Source, which marks it Urgent) or an event publication
    (SEND_EVENT, which the kernel counts as an input wake).

hid-i2c.drv cannot run in QEMU (it needs the IdeaPad's I2C touchpad), so
this check is its test of the grant and lend path; the others are also
measured at run time (run.sh --test display-latency with
CUBIT_LATENCY_INPUT and CUBIT_LATENCY_LOAD).

Usage: check_drivers.py [REPO_ROOT]. Exit 1 on a mismatch.
"""
import re
import sys
from pathlib import Path

root = Path(sys.argv[1] if len(sys.argv) > 1 else Path(__file__).resolve().parents[2])
svc = root / "userspace/services"
failed = False


def fail(text):
    global failed
    print("INPUT-URGENCY: FAIL " + text)
    failed = True


urgency = (root / "userspace/lib/input/input_urgency.ads")
if not urgency.exists():
    fail("userspace/lib/input/input_urgency.ads is missing (no shared input urgency)")
    budget = period = None
else:
    text = urgency.read_text()
    budget = int(re.search(r"Budget_Us\s*:\s*constant\s*:=\s*([\d_]+)", text).group(1).replace("_", ""))
    period = int(re.search(r"Period_Us\s*:\s*constant\s*:=\s*([\d_]+)", text).group(1).replace("_", ""))
    body = (root / "userspace/lib/input/input_urgency.adb").read_text()
    if "Reserve_Interactive" not in body or "Input_Wakes_Only" not in body:
        fail("Input_Urgency.Reserve does not reserve INTERACTIVE Input_Wakes_Only")

devmgr = (svc / "devmgr/main.adb").read_text()
source = (root / "userspace/lib/input/cubit-input_source.adb").read_text()
if not re.search(r"Signal\s*\([^;]*Urgent\s*=>\s*True", source):
    fail("CuBit.Input_Source's signal to Desktop is not marked Urgent")


def body_of(text, name):
    m = re.search(r"procedure\s+%s\b.*?\n\s*end\s+%s\s*;" % (name, name), text, re.S)
    return m.group(0) if m else ""


launch = {
    "ps2": body_of(devmgr, "setupPS2"),
    "xhci": devmgr[devmgr.find('Instance_Text (I, "xhci.drv")'):][:9000],
    "virtio-input": body_of(devmgr, "setupVirtioInput"),
    "hid-i2c": body_of(devmgr, "Launch_Generic_ACPI"),
}
report_path = {
    "ps2": "Keyboards.Publish",
    "xhci": "Keyboards.Publish",
    "virtio-input": "Pointers.Publish",
    "hid-i2c": "Publication",
}
for driver in ("ps2", "xhci", "virtio-input", "hid-i2c"):
    manifest = (svc / driver / "manifest.ccl").read_text()
    main = (svc / driver / "main.adb").read_text()
    m = (re.search(r'\(scheduling\s+"urgency"\s+(\d+)\s+(\d+)\)', manifest) or
         re.search(r"\(request-scheduling\s+urgency\s+realtime\s+\(budget-us\s+(\d+)\)\s+"
                   r"\(period-us\s+(\d+)\)\)", manifest))
    if not m:
        fail(f"{driver}: its manifest requests no urgency")
    elif budget is not None and (int(m.group(1)), int(m.group(2))) != (budget, period):
        fail(f"{driver}: its manifest's urgency {m.group(1)}/{m.group(2)} is not Input_Urgency's "
             f"{budget}/{period}")
    if not (re.search(r'optional\s*=>\s*\[[^]]*"urgency"', manifest) or
            re.search(r"\(optional\s+urgency\)", manifest)):
        fail(f"{driver}: its urgency request is not optional")
    if "Give_Declared_Scheduling" not in launch[driver]:
        fail(f"{driver}: devmgr's launch of it gives no declared scheduling share")
    if not re.search(r"Input_Urgency\.Reserve\s*\(\s*Startup\.Urgency\.Value\s*\)", main):
        fail(f"{driver}: its main does not reserve Input_Urgency")
    if report_path[driver] not in main:
        fail(f"{driver}: its reports do not reach Desktop through {report_path[driver]}")
if failed:
    sys.exit(1)
print("INPUT-URGENCY: PASS ps2, xhci, virtio-input and hid-i2c hold the same input urgency "
      f"({budget} us every {period} us, Input_Wakes_Only) and lend it on their report wakes")
