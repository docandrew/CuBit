import math
import struct
import tempfile
import unittest
import wave
from pathlib import Path
from report import serial_report, wave_report, load_overlaps_measurement


class Reports(unittest.TestCase):
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
