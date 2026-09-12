import unittest
from check_native import report


class CheckNative(unittest.TestCase):
    def test_full_cpu_set(self):
        one = ("DEADLINE-TIMER: cpu=0 requested_us=200 expired=256 cancelled=128 "
               "stale=128 early_vectors=128 failures=0 p99_late_le_us=8 "
               "max_late_ticks=40000 ticks_per_us=3500\n")
        self.assertTrue(report(one, 1)["valid"])
        four = "".join(one.replace("cpu=0", f"cpu={i}") for i in range(4))
        self.assertTrue(report(four, 4)["valid"])
        for bad in ("", one + one, one.replace("expired=256", "expired=255"),
                    one.replace("stale=128", "stale=0"),
                    one.replace("cancelled=128", "cancelled=0"),
                    one.replace("early_vectors=128", "early_vectors=0"),
                    one.replace("ticks_per_us=3500", "ticks_per_us=0"),
                    one.replace("failures=0", "failures=1"),
                    one.replace("requested_us=200", "requested_us=500")):
            self.assertFalse(report(bad, 1)["valid"])
        self.assertFalse(report(one, 4)["valid"])
        self.assertFalse(report(four, 1)["valid"])
