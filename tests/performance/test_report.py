import math
import struct
import tempfile
import unittest
import wave
from pathlib import Path
from report import serial_report, wave_report, load_overlaps_measurement


class Reports(unittest.TestCase):
    def test_reference_clock(self):
        good = (" TSC timer calibration: 3800 ticks/us (PIT reference)\n"
                "TIMING: calibration ticks_per_ms=3800000 min=3800000 max=3800000\n")
        self.assertTrue(serial_report(good)["reference_clock_valid"])
        for bad in (good.replace("ticks_per_ms=3800000", "ticks_per_ms=7600000"),
                    good + "CLOCK: FAIL source\n", good + good,
                    good.replace(" (PIT reference)", ""), ""):
            self.assertFalse(serial_report(bad)["reference_clock_valid"])

    def test_multiple_load_workers(self):
        good = ("BENCH-LOAD: START pid=31\nBENCH-LOAD: START pid=32\n"
                "INPUT-BENCH: START\nINPUT-BENCH: COMPLETE\n"
                "BENCH-LOAD: COMPLETE pid=32 batches=1\n"
                "BENCH-LOAD: COMPLETE pid=31 batches=1\n")
        self.assertTrue(load_overlaps_measurement(good, 2))
        self.assertFalse(load_overlaps_measurement(good, 1))
        self.assertFalse(load_overlaps_measurement(good, 3))
        for bad in (good.replace("pid=32", "pid=31"),
                    good.replace("pid=32", ""),
                    good.replace("batches=1", "batches=0"),
                    good.replace("INPUT-BENCH: START\n", "") + "INPUT-BENCH: START\n"):
            self.assertFalse(load_overlaps_measurement(bad, 2))

    def test_compute_control(self):
        good = "".join(
            f"CPU-CONTROL: START pid={pid} start_ms=100 ticks_per_ms=3000\n"
            f"CPU-CONTROL: COMPLETE pid={pid} finish_ms=12100 batches=1000\n"
            f"ACCOUNTING: pid={pid} cpu=0 residency_ticks=18000000 "
            "scheduled=4000 direct=0 fault=0 saturated=0\n"
            for pid in (31, 32))
        result = serial_report(good)["compute_control"]
        self.assertTrue(result["valid"])
        self.assertEqual(result["workers"][0]["mean_charged_us_per_dispatch"], 1500)
        for bad in (good + good, good.replace("pid=32", "pid=31"),
                    good.replace("ticks_per_ms=3000", "ticks_per_ms=0"),
                    good.replace("finish_ms=12100", "finish_ms=12099"),
                    good.replace("scheduled=4000", "scheduled=0"),
                    good.replace("direct=0", "direct=1"),
                    good.replace("fault=0", "fault=1"),
                    good.replace("saturated=0", "saturated=1"),
                    good.replace("pid=32 cpu=0", "pid=32 cpu=1"),
                    good.replace("batches=1000", "batches=0"),
                    good.replace(" saturated=0", ""),
                    good[:good.rfind("ACCOUNTING:")]):
            self.assertFalse(serial_report(bad)["compute_control"]["valid"])
        self.assertIsNone(serial_report("")["compute_control"])

    def test_shadow_budgets(self):
        accounting = ("ACCOUNTING: pid= 33 cpu= 0 residency_ticks= 9000 "
                      "scheduled= 12 direct= 800 fault= 0 saturated= 0\n")
        shadow = ("SHADOW-BUDGET: mode=demand-only pid=33 cpu=0 generation=1 "
                  "charged_ticks=9000 dispatches=812 denied=810 checkpoints=1 "
                  "remaining_ticks=0 credits=0 overrun=1 fault=0 saturated=0\n")
        self.assertTrue(serial_report(accounting + shadow)["shadow_budgets_valid"])
        for bad in ("", shadow + shadow, shadow.replace("demand-only", "enforced"),
                    shadow.replace("9000", "8999"), shadow.replace("812", "811"),
                    shadow.replace("810", "813"), shadow.replace("generation=1", "generation=0"),
                    shadow.replace("checkpoints=1", "checkpoints=0"),
                    shadow.replace("fault=0", "fault=1"),
                    shadow.replace("saturated=0", "saturated=1")):
            self.assertFalse(serial_report(accounting + bad)["shadow_budgets_valid"])

    def test_execution_accounting(self):
        good = ("ACCOUNTING: pid= 33 cpu= 0 residency_ticks= 9000 "
                "scheduled= 12 direct= 800 fault= 0 saturated= 0\n")
        self.assertTrue(serial_report(good)["execution_accounting_valid"])
        for bad in ("", good + good, good.replace("pid= 33", "pid= 0"),
                    good.replace("fault= 0", "fault= 1"),
                    good.replace("saturated= 0", "saturated= 1"),
                    good.replace("direct= 800", "direct= 0"),
                    good.replace("scheduled= 12", "scheduled= 0"),
                    good.replace("residency_ticks= 9000", "residency_ticks= 0"),
                    good.replace("cpu= 0", "cpu= 4"),
                    good.replace(" saturated= 0", "")):
            self.assertFalse(serial_report(bad)["execution_accounting_valid"])

    def test_load_progress(self):
        result = serial_report("BENCH-LOAD: COMPLETE batches= 1234 max_gap_ms= 2\n")
        self.assertEqual(result["load_progress"], {"batches": 1234, "max_gap_ms": 2})
        self.assertIsNone(serial_report("BENCH-LOAD: COMPLETE")["load_progress"])

    @staticmethod
    def input_log(misses=0):
        return ("TIMING: calibration ticks_per_ms= 3000000\nINPUT-BENCH: START\n" +
                "".join(f"TIMING: input-{mode} count= 2048 p99_le_ticks= 3000000\n"
                        f"INPUT-BENCH: scenario={mode} delivered= 2048 misses_1ms= {misses} failures=0\n"
                        for mode in ("CONTINUOUS", "PACED", "REPAINTING")) +
                "INPUT-BENCH: COMPLETE\nBENCH: PASS input integrity\n")

    def test_input_exact_threshold(self):
        # Buckets may straddle the threshold; the exact miss counter decides.
        self.assertTrue(serial_report(self.input_log(20))["input_observed_target_met"])
        self.assertFalse(serial_report(self.input_log(21))["input_observed_target_met"])

    def test_input_invalid(self):
        good = self.input_log()
        for bad in (good.replace("COMPLETE", "unfinished"),
                    good.replace("count= 2048", "count= 2047", 1),
                    good.replace("failures=0", "failures=1", 1),
                    good.replace("misses_1ms= 0", "misses_1ms= 2049", 1),
                    good.replace("ticks_per_ms= 3000000", "ticks_per_ms= 0"),
                    good + "BENCH: FAIL input destroy\n", good + good):
            self.assertFalse(serial_report(bad)["input_integrity_valid"])
            self.assertFalse(serial_report(bad)["input_observed_target_met"])

    def test_input_load_coverage(self):
        good = self.input_log()
        self.assertTrue(load_overlaps_measurement("BENCH-LOAD: START\n" + good + "BENCH-LOAD: COMPLETE"))
        self.assertFalse(load_overlaps_measurement("BENCH-LOAD: START\nBENCH-LOAD: COMPLETE\n" + good))
        self.assertFalse(serial_report("BENCH-LOAD: START\nBENCH-LOAD: COMPLETE\n" + good)["input_observed_target_met"])

    def test_load_must_overlap(self):
        self.assertTrue(load_overlaps_measurement(
            "BENCH-LOAD: START\nAUDIO-BENCH: START\nAUDIO-BENCH: COMPLETE\nBENCH-LOAD: COMPLETE"))
        self.assertFalse(load_overlaps_measurement(
            "BENCH-LOAD: START\nBENCH-LOAD: COMPLETE\nAUDIO-BENCH: START\nAUDIO-BENCH: COMPLETE"))
        self.assertFalse(load_overlaps_measurement("BENCH-LOAD: START"))

    def test_quantile_conversion_and_phase(self):
        result = serial_report("TIMING: calibration ticks_per_ms= 3000000\n"
                               "BENCH: phase=untraced async_depth=16\n"
                               "TIMING: ipc-sync count= 20 p99_le_ticks= 6000 max_ticks= 7000\n")
        self.assertEqual(result["timings"][0]["phase"], "untraced")
        self.assertEqual(result["timings"][0]["microseconds"]["p99_le"], 2)

    def test_missing_calibration_not_converted(self):
        self.assertNotIn("microseconds", serial_report("TIMING: x count=1 max_ticks=42")["timings"][0])

    def test_wave_and_gap(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "signal.wav"
            for gap in (False, True):
                with wave.open(str(path), "wb") as output:
                    output.setparams((2, 2, 48000, 0, "NONE", "not compressed"))
                    payload = bytearray()
                    for i in range(48000 * 3):
                        sample = int(4000 * math.sin(2 * math.pi * 500 * i / 48000))
                        if gap and 48000 < i < 60000:
                            sample = 0
                        payload.extend(struct.pack("<hh", sample, sample))
                    output.writeframes(payload)
                result = wave_report(path)
                self.assertAlmostEqual(result["estimated_tone_hz"], 500)
                self.assertEqual(result["quiet_20ms_windows"] == 0, not gap)
                payload = bytearray(path.read_bytes())
                payload[4:8] = bytes(4)
                payload[40:44] = bytes(4)
                path.write_bytes(payload)
                repaired = wave_report(path)
                self.assertTrue(repaired["unfinalized_capture"])
                self.assertEqual(repaired["quiet_20ms_windows"], result["quiet_20ms_windows"])
                self.assertEqual(path.read_bytes(), payload)


if __name__ == "__main__":
    unittest.main()
