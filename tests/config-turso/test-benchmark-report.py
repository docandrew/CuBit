"""Reject misleading partial or malformed performance artifacts."""
import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location(
    "report", Path(__file__).with_name("summarize-benchmarks.py"))
report = importlib.util.module_from_spec(spec)
spec.loader.exec_module(report)


def fixture():
    lines = ["BENCH version=2 platform=linux backend=syscall engine=0.8.0-pre.12 "
             "build=O2 storage=buffered cache=warm synchronization=FULL "
             "raw_buffers=heap-unregistered batches=fixed"]
    for op, size, depth in sorted(report.EXPECTED):
        count = 5000 if op == "config-read-warm" else 1000 if not size else 1024
        lines.append(f"MEASURE backend=syscall operation={op} bytes={size} "
                     f"depth={depth} n={count} p50_ns=10 p95_ns=20 p99_ns=30 "
                     f"max_ns=40 timed_ns=50000 peak_deferred={'na' if not size else 0}")
    return "\n".join(lines + ["BENCH PASS backend=syscall"])


class BenchmarkReportTests(unittest.TestCase):
    def test_complete_matrix(self):
        backend, rows = report.parse(fixture())
        self.assertEqual(backend, "syscall")
        self.assertEqual(len(rows), 28)

    def test_reject_invalid_artifacts(self):
        good = fixture()
        lines = good.splitlines()
        cases = [
            "\n".join(lines[:-1]),  # interrupted run
            "\n".join(lines[:2] + lines[3:]),  # missing phase
            "\n".join(lines[:2] + [lines[1]] + lines[2:]),
            good.replace("n=1024", "n=1023", 1),
            good.replace("peak_deferred=0", "peak_deferred=33", 1),
            good.replace("peak_deferred=na", "peak_deferred=0", 1),
            good.replace("p99_ns=30", "p99_ns=9", 1),
            good.replace("cache=warm", "cache=cold", 1),
            good.replace("depth=1", "depth=1 depth=1", 1),
            good + "\nBENCH PASS backend=syscall",
        ]
        for bad in cases:
            with self.subTest(artifact=bad):
                with self.assertRaises(ValueError):
                    report.parse(bad)


if __name__ == "__main__":
    unittest.main()
