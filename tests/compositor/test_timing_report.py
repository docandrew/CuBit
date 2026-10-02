import importlib.util
from pathlib import Path
import unittest
spec = importlib.util.spec_from_file_location("timing", Path(__file__).with_name("check-timing.py"))
timing = importlib.util.module_from_spec(spec)
spec.loader.exec_module(timing)
class TimingReport(unittest.TestCase):
    def setUp(self):
        self.good = "\n".join(f"COMPOSITOR-TIMING: stage={stage} count=10 min_us=1 max_us=900 p50_upper_us=8 p99_upper_us=1024 invalid=0 dropped=0" for stage in timing.STAGES)
    def test_preserves_window_quantiles(self):
        report = timing.check(self.good + "\n" + self.good)
        for row in report["stages"].values():
            self.assertEqual(row["samples"], 20)
            self.assertEqual(len(row["windows"]), 2)
            self.assertNotIn("p99_upper_us", row)
    def test_rejects_invalid_evidence(self):
        for bad in [self.good.replace("invalid=0", "invalid=1"),
                    self.good.replace("dropped=0", "dropped=1"),
                    self.good.replace("count=10", "count=1000001"),
                    self.good.replace("min_us=1", "min_us=1000"),
                    self.good.replace("p50_upper_us=8", "p50_upper_us=2048"),
                    self.good.splitlines()[0],
                    self.good.replace("max_us=900", "max_us=-1"),
                    self.good.replace("dropped=0", "invalid=0")]:
            with self.subTest(bad=bad):
                with self.assertRaises(ValueError): timing.check(bad)
if __name__ == "__main__": unittest.main()
