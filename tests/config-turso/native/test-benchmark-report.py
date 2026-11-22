"""Synthetic parser fixtures only; these numbers are NOT performance evidence."""
import importlib.util
import pathlib
import unittest

spec = importlib.util.spec_from_file_location("native_report", pathlib.Path(__file__).with_name("report-benchmark.py"))
report = importlib.util.module_from_spec(spec)
spec.loader.exec_module(report)


def fixture():
    lines = ["TURSO-BENCH: START clock=calibrated-tsc ticks_per_ms=1000000 transport=serial-grant transfer_bytes=65536 cross_cpu_tsc=assumed"]
    for op, size, depth in sorted(report.EXPECTED):
        lines.append(f"MEASURE backend=cubit-native operation={op} bytes={size} depth={depth} n=64 "
                     "p50_ns=10 p95_ns=20 p99_ns=30 max_ns=30 timed_ns=1000 peak_deferred=0")
    return "\n".join(lines + ["TURSO-BENCH: PASS", "TURSO-NATIVE: Ada typed worker publication PASS (filesystem)"])


class ReportTests(unittest.TestCase):
    def test_complete(self):
        self.assertEqual(len(report.parse(fixture())), 18)
        self.assertEqual(report.capacity(fixture()), 65536)
        old = fixture().replace("transport=serial-grant transfer_bytes=65536", "transport=serial-4k-grant")
        self.assertEqual(report.capacity(old), 4096)
        self.assertEqual(report.vector_layout(fixture()), "segmented")
        self.assertEqual(report.vector_layout(fixture().replace("cross_cpu", "vector_layout=packed cross_cpu")), "packed")
        with self.assertRaises(ValueError):
            report.vector_layout(fixture().replace("cross_cpu", "vector_layout=unknown cross_cpu"))

    def test_reject_incomplete_or_invalid_results(self):
        good = fixture()
        variants = [
            good.replace("TURSO-BENCH: PASS", ""),
            good.replace("TURSO-NATIVE: Ada typed worker publication PASS (filesystem)", ""),
            good.replace("ticks_per_ms=1000000", "ticks_per_ms=0"),
            good.replace("transfer_bytes=65536", "transfer_bytes=0"),
            good.replace("transfer_bytes=65536", "transfer_bytes=4097"),
            good.replace("n=64", "n=63", 1),
            good.replace("p50_ns=10", "p50_ns=40", 1),
            good.replace("p99_ns=30", "p99_ns=20", 1),
            good.replace("timed_ns=1000", "timed_ns=0", 1),
            good.replace("peak_deferred=0", "peak_deferred=1", 1),
            good.replace("bytes=4096", "bytes=8192", 1),
            good.replace("backend=cubit-native", "backend=linux", 1),
            "\n".join(good.splitlines()[:1] + good.splitlines()[2:]),
            good + "\n" + good.splitlines()[1],
            good + "\nTURSO-BENCH: PASS",
        ]
        for bad in variants:
            with self.subTest(bad=bad[:80]), self.assertRaises(ValueError):
                report.parse(bad)


if __name__ == "__main__":
    unittest.main()
