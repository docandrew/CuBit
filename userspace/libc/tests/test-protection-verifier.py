"""Negative controls for the native fault evidence checker."""
import contextlib
import io
from pathlib import Path
import runpy
import unittest
from unittest.mock import patch

CHECKER = Path(__file__).with_name("check-protection-faults.py")
VALID = """PROTECTION-FAULT: guard-read armed pid=30 address=580000000000
USER-MEMORY-FAULT: pid 30 address 96757023244288 kind=unmapped
Process.reclaimProcess: stopped PID 30
PROTECTION-FAULT: readonly-write armed pid=31 address=580000000000
USER-MEMORY-FAULT: pid 31 address 96757023244288 kind=write-protection
Process.reclaimProcess: stopped PID 31
"""


class Verifier(unittest.TestCase):
    def verify(self, log):
        with patch.object(Path, "read_text", return_value=log), \
             patch("sys.argv", [str(CHECKER), "fixture.log"]), \
             contextlib.redirect_stdout(io.StringIO()):
            runpy.run_path(str(CHECKER), run_name="__main__")

    def test_complete_evidence(self):
        self.verify(VALID)

    def test_reject_incomplete_or_wrong_evidence(self):
        reused = VALID.replace("pid=31", "pid=30").replace("pid 31", "pid 30").replace("PID 31", "PID 30")
        self.verify(reused)
        bad_logs = [
            "", VALID.split("PROTECTION-FAULT: readonly-write")[0],
            VALID.replace("pid 30 address", "pid 99 address"),
            VALID.replace("96757023244288", "96757023248384"),
            VALID.replace("kind=write-protection", "kind=unmapped"),
            VALID.replace("Process.reclaimProcess: stopped PID 30", "unrelated"),
            VALID + "PROTECTION-FAULT: guard-read armed pid=30 address=580000000000\n",
            VALID + "PROTECTION-FAULT: FAIL access unexpectedly succeeded\n",
            reused.rsplit("Process.reclaimProcess: stopped PID 30", 1)[0],
        ]
        for log in bad_logs:
            with self.subTest(log=log), self.assertRaises(SystemExit):
                self.verify(log)


if __name__ == "__main__":
    unittest.main()
