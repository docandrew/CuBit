import unittest

from run import parse, replace_once


def fixture():
    lines = []
    for phase in range(1, 13):
        mode = "PRIVATE_LOCK" if (phase - 1) % 4 < 2 else "SHARED_LOCK"
        measurement = "THROUGHPUT" if phase % 2 else "ACQUISITION_LATENCY"
        if phase % 2 == 0:
            for cpu in range(4):
                lines.append(f"LOCK-SAMPLE: phase= {phase} cpu= {cpu} count= 50000 p50= 160 p99= 320 max= 500")
        lines.append(f"LOCK-ROUND: phase= {phase} repeat= {(phase - 1) // 4 + 1} "
                     f"mode={mode} measurement={measurement} operations=200000 elapsed_ticks= 900000 valid=TRUE")
    return "\n".join(lines + ["LOCK-BENCH: PASS"])


class Reports(unittest.TestCase):
    def test_complete(self):
        self.assertEqual(len(parse(fixture())["samples"]), 24)

    def test_invalid(self):
        for bad in (fixture().replace("PASS", "FAIL"),
                    fixture().replace("valid=TRUE", "valid=FALSE", 1),
                    fixture().replace("PRIVATE_LOCK", "0", 1),
                    fixture().replace("cpu= 1", "cpu= 0", 1),
                    fixture().replace("count= 50000", "count= 1", 1),
                    fixture().replace("elapsed_ticks= 900000", "elapsed_ticks= 0", 1),
                    fixture().replace("phase= 12", "phase= 11"),
                    fixture().replace("p99= 320", "p99= 80", 1)):
            with self.subTest(bad=bad[-60:]), self.assertRaises(ValueError):
                parse(bad)

    def test_injection_is_exact(self):
        self.assertEqual(replace_once("a b c", "b", "B"), "a B c")
        for text in ("", "b b"):
            with self.assertRaises(ValueError):
                replace_once(text, "b", "B")


if __name__ == "__main__":
    unittest.main()
