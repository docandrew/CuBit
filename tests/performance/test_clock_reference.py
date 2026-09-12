import unittest
from clock_reference import validate


class Reference(unittest.TestCase):
    def test_clock_scale(self):
        starts = {31: (100, 0), 32: (101, 1_000_000)}
        ends = {31: (12100, 12_000_000_000), 32: (12101, 12_001_000_000)}
        self.assertTrue(validate(starts, ends)["valid"])
        for scale in (0.5, 2.0):
            dilated = {pid: (ms, int(ns * scale)) for pid, (ms, ns) in ends.items()}
            self.assertFalse(validate(starts, dilated)["valid"])
        self.assertFalse(validate(starts, {31: ends[31]})["valid"])
        self.assertFalse(validate({}, {})["valid"])
        self.assertFalse(validate(starts, starts)["valid"])
