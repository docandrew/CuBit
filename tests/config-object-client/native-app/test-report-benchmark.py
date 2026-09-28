import importlib.util
import pathlib
import unittest

spec = importlib.util.spec_from_file_location("report", pathlib.Path(__file__).with_name("report-benchmark.py"))
report = importlib.util.module_from_spec(spec)
spec.loader.exec_module(report)


def fixture():
    lines = ["CONFIG-BENCH: start samples= 64 ticks_per_ms= 1000 final_revision= 129"]
    lines += [f"CONFIG-BENCH: sample phase={phase} index= {n} ticks= {n}"
              for phase in report.PHASES for n in range(1, 65)]
    return "\n".join(lines + ["CONFIG-BENCH: reads_before_write_reply= 63",
                              "TEST: PASS config-objects-benchmark"])


class ReportTests(unittest.TestCase):
    def test_valid(self):
        rows, before = report.parse(fixture())
        self.assertEqual(before, 63)
        self.assertEqual(rows["cached-get"], list(range(1, 65)))
        self.assertEqual(report.quantile(rows["cached-get"], 99), 64)

    def test_reject_corruption(self):
        text = fixture()
        cases = [
            text.replace("samples= 64", "samples= 63"),
            text.replace("ticks_per_ms= 1000", "ticks_per_ms= 0"),
            text.replace("final_revision= 129", "final_revision= 128"),
            text.replace("final_revision= 129", "final_revision= 129garbage"),
            text.replace("ticks= 1\n", "ticks= 0\n", 1),
            text.replace("phase=cached-get", "phase=unknown", 1),
            text.replace("index= 1 ticks= 1", "index= 2 ticks= 1", 1),
            text.replace("reads_before_write_reply= 63", "reads_before_write_reply= 65"),
            text.replace("TEST: PASS config-objects-benchmark", ""),
            text.replace("CONFIG-BENCH: sample phase=cached-get index= 64 ticks= 64\n", ""),
            text + "\nCONFIG-BENCH: sample phase=cached-get index= 65 ticks= 65",
            text + "\nTEST: FAIL unexpected",
            text + "\nCONFIG-BENCH: garbage",
            text + "\nCONFIG-BENCH: reads_before_write_reply= 2",
            text + "\nTEST: PASS config-objects-benchmark",
        ]
        for invalid in cases:
            with self.subTest(invalid=invalid[-100:]):
                with self.assertRaises(AssertionError):
                    report.parse(invalid)


if __name__ == "__main__":
    unittest.main()
