"""Evidence validation regressions; run inside the Nix development shell."""
from pathlib import Path
import runpy
import unittest

check = runpy.run_path(str(Path(__file__).with_name("check-input-stream.py")))["check"]


class InputEvidenceTests(unittest.TestCase):
    row = "desktop: stats source_gap=1 source_reject=0 present_req=3 input_req=40\n"
    valid = "input-stress: resync reports 2\n" + row * 2

    def test_split_intervals(self):
        self.assertEqual(check(self.valid), dict(
            source_gap=2, source_reject=0, present_req=6, input_req=80))

    def test_faults_cannot_hide_in_other_intervals(self):
        invalid = [
            self.valid.replace("reports 2", "reports 1"),
            self.valid.replace("source_reject=0", "source_reject=1", 1),
            self.valid.replace("present_req=3", "present_req=11"),
            self.valid.replace("input_req=40", "input_req=81"),
            self.valid.replace("source_gap=1", "", 1),
            self.valid + "input-stress: resync reports 2\n",
            "input-stress: resync reports 1\n",
        ]
        for text in invalid:
            with self.subTest(text=text), self.assertRaises(ValueError):
                check(text)


if __name__ == "__main__":
    unittest.main()
