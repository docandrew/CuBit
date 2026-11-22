#!/usr/bin/env python3
"""Hosted controls for the native ACPI headless evidence readers."""

import argparse
import importlib.util
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

sys.dont_write_bytecode = True
HERE = Path(__file__).absolute().parent
spec = importlib.util.spec_from_file_location("acpi_launch_checks", HERE / "check-launch.py")
checks = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checks)
consumer_spec = importlib.util.spec_from_file_location("consumer_refusal_checks", HERE.parent / "headless/check-acpi-consumer-refusal.py")
consumer_checks = importlib.util.module_from_spec(consumer_spec)
consumer_spec.loader.exec_module(consumer_checks)
CAPTURES = None
SHUTDOWN = HERE.parent / "headless/check-acpi-shutdown.sh"

PCI = """devmgr.svc: devmgr: PCI 00:00.0 vendor=8086 device=29C0 class=0600
Process.Loader: Stack reservation: devmgr.svc: devmgr: match 00:00.0 8086:29C0 class 06.00.00
Process.Loader: Loaded module acpi-pci-check.a w/ process ID 32
acpi-pci-check: host bridge 00:00.0 8086:29C0
acpi-pci-check: attempting forbidden write at 0000600000000000
USER-MEMORY-FAULT: pid 32 address 105553116266496 kind=write-protection
logstore: kernel: process killed #1 name=acpi-pci-check.a pid=0x1000020 kind=write-protection vector=14 t=401ms
"""

GPIO = """procmgr.svc: procmgr: held PID 16777251 for its supervisor
gpio-launch-check: sink admission ok status=COMPLETED badge=0x475010000001004C owner=0x0000000001000023
hid-launch-check: parameters descriptor_register=0x20
procmgr.svc: procmgr: held pid16777251 released by devmgr
"""

BEFORE_TAIL = """acpi: stage runtime ok LOCAL_IDLE
acpi: stage runtime-selection ok one-service mode=LOCAL_RUNTIME
acpi: SCI armed
acpi: SCI 1 fixed=[PWRBTN ] gpe=[]
power: power button pressed (FIXED_EVENT, count 1)
power: POWER_OFF requested by POWER_BUTTON
power: procmgr stopped the apps
power: asking acpi.svc to prepare to sleep (S5)
power: prepare to sleep PREPARED
procmgr: shutdown: keeping com.cubit.acpi pid16777248
procmgr: shutdown: keeping com.cubit.logstore pid16777246
procmgr: shutdown: stopping com.cubit.test.shutdown-stubborn pid16777250
did not end within its grace of 500 ms; ending it
procmgr: shutdown: stopping com.cubit.test.shutdown-cooperative pid16777249
shutdown-cooperative: quiesced; exiting 0
"""

TAIL = """logstore: storage stopping: keeping final lines in memory
procmgr: shutdown: logstore detached its storage: no log file open
procmgr: shutdown: the filesystem commits (deadline 10000 ms)
FS: stop: storage COMMITTED
procmgr: shutdown: storage COMMITTED
devmgr: shutdown: drivers stopped
procmgr: shutdown: stopped in 714 ms
power: procmgr stopped the system:
power: asking acpi.svc to enter S5
""".splitlines(True)


class LaunchTests(unittest.TestCase):
    def test_pci_interleaved_and_prefixed_enumeration(self):
        checks.pci_config(PCI)

    def test_pci_torn_loader_uses_complete_named_identity(self):
        checks.pci_config(PCI.replace("ID 32\n", "ID 3devmgr: another line\n2\n"))

    def test_pci_intact_loader_still_works_without_named_record(self):
        checks.pci_config("\n".join(line for line in PCI.splitlines() if "process killed #" not in line))

    def test_pci_ids_must_agree_in_every_enumeration(self):
        for text in [PCI.replace("8086:29C0 class", "8086:1234 class"),
                     PCI.replace("vendor=8086", "vendor=1234"),
                     PCI.replace("host bridge 00:00.0 8086:29C0", "host bridge 00:00.0 8086:1234")]:
            with self.subTest(text=text):
                with self.assertRaises(checks.EvidenceError):
                    checks.pci_config(text)

    def test_pci_identity_cannot_be_invented_or_conflicted(self):
        cases = [PCI.replace("ID 32", "ID torn").replace("name=acpi-pci-check.a", "name=another.app"),
                 PCI.replace("pid=0x1000020", "pid=0x1000021"),
                 PCI.replace("pid=0x1000020", "pid=0x1000000"),
                 PCI.replace("pid=0x1000020", "pid=0x10000000000000000")]
        for text in cases:
            with self.subTest(text=text):
                with self.assertRaises(checks.EvidenceError):
                    checks.pci_config(text)

    def test_pci_requires_same_slot_address_and_write_protection(self):
        cases = [PCI.replace("USER-MEMORY-FAULT: pid 32", "USER-MEMORY-FAULT: pid 33"),
                 PCI.replace("address 105553116266496", "address 4096"),
                 PCI.replace("kind=write-protection\n", "kind=unmapped\n"),
                 PCI + "forbidden write was not stopped\n"]
        for text in cases:
            with self.subTest(text=text):
                with self.assertRaises(checks.EvidenceError):
                    checks.pci_config(text)

    def test_pci_requires_unique_attempt_and_enumeration(self):
        for text in [PCI + "acpi-pci-check: attempting forbidden write at 0000600000000000\n",
                     "\n".join(line for line in PCI.splitlines() if "devmgr:" not in line)]:
            with self.subTest(text=text):
                with self.assertRaises(checks.EvidenceError):
                    checks.pci_config(text)

    def test_gpio_accepts_delayed_and_prompt_release_narration(self):
        checks.gpio_launch(GPIO)
        lines = GPIO.splitlines(True)
        checks.gpio_launch("".join([lines[0], lines[1], lines[3], lines[2]]))

    def test_gpio_rejects_run_before_admission_or_hold(self):
        lines = GPIO.splitlines(True)
        for order in [[0, 2, 1, 3], [1, 0, 2, 3], [0, 3, 1, 2]]:
            with self.subTest(order=order):
                with self.assertRaises(checks.EvidenceError):
                    checks.gpio_launch("".join(lines[index] for index in order))

    def test_gpio_rejects_wrong_child_and_sink_owner(self):
        for text in [GPIO.replace("owner=0x0000000001000023", "owner=0x0000000001000024"),
                     GPIO.replace("held pid16777251", "held pid16777252")]:
            with self.subTest(text=text):
                with self.assertRaises(checks.EvidenceError):
                    checks.gpio_launch(text)

    def test_gpio_rejects_missing_and_duplicate_evidence(self):
        lines = GPIO.splitlines(True)
        for index in range(len(lines)):
            with self.subTest(index=index):
                with self.assertRaises(checks.EvidenceError):
                    checks.gpio_launch("".join(lines[:index] + lines[index + 1:]))
                with self.assertRaises(checks.EvidenceError):
                    checks.gpio_launch(GPIO + lines[index])


class ShutdownTests(unittest.TestCase):
    def check_trace(self, text, expected=0, status=0):
        with tempfile.NamedTemporaryFile() as log:
            log.write(text.encode())
            log.flush()
            result = subprocess.run(["bash", str(SHUTDOWN), log.name, str(status)],
                                    capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, expected, result.stdout + result.stderr)

    def test_keeps_logstore_alive_through_complete_tail(self):
        self.check_trace(BEFORE_TAIL + "".join(TAIL))

    def test_every_truncated_tail_prefix_is_allowed_after_power_off(self):
        for count in range(len(TAIL)):
            with self.subTest(prefix=count):
                self.check_trace(BEFORE_TAIL + "".join(TAIL[:count]))

    def test_missing_middle_tail_stage_is_rejected(self):
        for index in range(len(TAIL) - 1):
            with self.subTest(stage=index):
                self.check_trace(BEFORE_TAIL + "".join(TAIL[:index] + TAIL[index + 1:]), expected=1)

    def test_misordered_tail_stage_is_rejected(self):
        for index in range(len(TAIL) - 1):
            tail = TAIL.copy()
            tail[index], tail[index + 1] = tail[index + 1], tail[index]
            with self.subTest(stage=index):
                self.check_trace(BEFORE_TAIL + "".join(tail), expected=1)

    def test_prepared_mirror_may_follow_first_service_stop(self):
        prepared = "power: prepare to sleep PREPARED\n"
        first_stop = "procmgr: shutdown: stopping com.cubit.test.shutdown-stubborn pid16777250\n"
        text = BEFORE_TAIL.replace(prepared, "").replace(first_stop, first_stop + prepared)
        self.check_trace(text + "".join(TAIL))

    def test_prepare_reply_keeps_power_request_and_entry_order(self):
        prepared = "power: prepare to sleep PREPARED\n"
        request = "power: asking acpi.svc to prepare to sleep (S5)\n"
        self.check_trace(BEFORE_TAIL.replace(prepared, "").replace(request, prepared + request) + "".join(TAIL), expected=1)
        self.check_trace(BEFORE_TAIL.replace(prepared, "") + "".join(TAIL) + prepared, expected=1)

    def test_no_logstore_stop_and_missing_keep_are_rejected(self):
        self.check_trace(BEFORE_TAIL + "procmgr: shutdown: stopping com.cubit.logstore pid9\n", expected=1)
        self.check_trace(BEFORE_TAIL.replace("keeping com.cubit.logstore", "unrelated com.cubit.logstore"), expected=1)

    def test_timeout_failed_acpi_and_missing_forced_stop_are_rejected(self):
        self.check_trace(BEFORE_TAIL + "".join(TAIL), expected=1, status=124)
        self.check_trace(BEFORE_TAIL + "STARTUP_REFUSED\n", expected=1)
        self.check_trace(BEFORE_TAIL.replace("did not end within its grace of 500 ms; ending it", ""), expected=1)


class ConsumerRefusalTests(unittest.TestCase):
    def trace(self):
        return BEFORE_TAIL.replace("acpi: SCI 1 fixed=", "acpi: sample refusal family=BATTERY isolated=TRUE runtime=IDLE owner-guard=EVENT_COMPLETED\nacpi: battery guard=INVENTORY_LIMIT\nacpi: lid OPEN\nacpi: AC ONLINE\nacpi: SCI 1 fixed=") + "".join(TAIL)

    def test_ordinary_refusal_continues_sci_and_preparation(self):
        consumer_checks.check(self.trace())

    def test_missing_refusal_facts_or_late_isolation_refuse(self):
        for marker in ["sample refusal family=BATTERY isolated=TRUE runtime=IDLE", "battery guard=INVENTORY_LIMIT", "lid OPEN", "AC ONLINE", "prepare to sleep PREPARED"]:
            with self.subTest(marker=marker), self.assertRaises(ValueError):
                consumer_checks.check(self.trace().replace(marker, "missing"))
        with self.assertRaises(ValueError):
            consumer_checks.check(self.trace().replace("acpi: sample refusal family=BATTERY isolated=TRUE runtime=IDLE owner-guard=EVENT_COMPLETED\n", "") + "acpi: sample refusal family=BATTERY isolated=TRUE runtime=IDLE\n")

    def test_stopped_owner_is_not_ordinary_isolation(self):
        for marker in ["acpi: stage consumer fail", "runtime-quarantine", "acpi: stage serve fail"]:
            with self.subTest(marker=marker), self.assertRaises(ValueError):
                consumer_checks.check(self.trace() + marker)


class CaptureTests(unittest.TestCase):
    def setUp(self):
        if CAPTURES is None:
            self.skipTest("pass --captures for external native serial replay")

    def test_gpio_run_required_markers_capture(self):
        import re
        run_text = (HERE.parent / "headless/run.sh").read_text()
        block = run_text.rsplit("\n    gpio-launch)\n", 1)[1].split("\n        ;;", 1)[0]
        required = re.search(r'required_markers="(.*?)"', block, re.S).group(1).splitlines()
        text = (CAPTURES / "g-gpio-launch.serial").read_text().replace("\r", "")
        for marker in required:
            if marker.strip():
                self.assertIn(marker, text)

    def test_pci_capture(self):
        checks.pci_config((CAPTURES / "g-acpi-pci-config.serial").read_text().replace("\r", ""))

    def test_gpio_capture(self):
        checks.gpio_launch((CAPTURES / "g-gpio-launch.serial").read_text().replace("\r", ""))

    def test_shutdown_capture(self):
        result = subprocess.run(["bash", str(SHUTDOWN), str(CAPTURES / "g-acpi-shutdown.serial"), "0"],
                                capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--captures", type=Path)
    args, remaining = parser.parse_known_args()
    CAPTURES = args.captures
    unittest.main(argv=[sys.argv[0], *remaining])
