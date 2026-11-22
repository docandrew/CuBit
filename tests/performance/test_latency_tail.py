import unittest
from latency_tail import report


class TailTests(unittest.TestCase):
    def test_correlates_but_does_not_attribute_gap(self):
        text = """TIMING: calibration ticks_per_ms= 1000
LATENCY-TRACE: tsc= 10 pid= 2 event=syscall_return a= 1 b= 0
LATENCY-TRACE: tsc= 5010 pid= 2 event=timer_late a= 4900 b= 0
LATENCY-TRACE: tsc= 5110 pid= 3 event=syscall_return a= 9 b= 0
INPUT-TAIL: scenario=CONTINUOUS started= 20 published= 25 finished= 5100
"""
        result = report(text)
        self.assertEqual(result["trace_events"], 3)
        self.assertEqual(result["timer_lateness_us"], [4900])
        sample = result["samples"][0]
        self.assertTrue(sample["fully_covered_by_trace"])
        self.assertEqual(sample["publish_us"], 5)
        self.assertEqual(sample["overlapping_event_gaps"][0]["duration_us"], 5000)

    def test_no_trace_is_not_coverage(self):
        result = report("TIMING: calibration ticks_per_ms= 1\n"
                        "INPUT-TAIL: scenario=PACED started= 1 published= 2 finished= 3")
        self.assertFalse(result["samples"][0]["fully_covered_by_trace"])

    def test_full_width_timestamps_and_syscall_pairing(self):
        result = report("""TIMING: calibration ticks_per_ms= 1000
LATENCY-TRACE: tsc= 123456789000 pid= 2 event=syscall_enter a=12 b=1
LATENCY-TRACE: tsc= 123456792000 pid= 2 event=syscall_return a=12 b=0
""")
        span = result["long_syscall_spans"][0]
        self.assertEqual(span["started"], 123456789000)
        self.assertEqual(span["wall_duration_us"], 3000)

    def test_rejects_bad_clock_and_reversal(self):
        with self.assertRaises(ValueError):
            report("TIMING: calibration ticks_per_ms= 0")
        with self.assertRaises(ValueError):
            report("TIMING: calibration ticks_per_ms= 1\n"
                   "INPUT-TAIL: scenario=PACED started= 2 published= 1 finished= 3")
        with self.assertRaises(ValueError):
            report("TIMING: calibration ticks_per_ms= 1\n"
                   "LATENCY-TRACE: tsc= 2 pid= 2 event=ready a=2 b=0\n"
                   "LATENCY-TRACE: tsc= 1 pid= 2 event=ready a=2 b=0")


if __name__ == "__main__":
    unittest.main()
