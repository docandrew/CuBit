import importlib.util
import pathlib
import unittest

spec = importlib.util.spec_from_file_location("profile", pathlib.Path(__file__).with_name("check-sql-profile.py"))
profile = importlib.util.module_from_spec(spec)
spec.loader.exec_module(profile)


def fixture():
    lines = ["TURSO-SQL: start samples=129 ticks_per_ms=1000"]
    lines += [f"TURSO-SQL: sample revision={n} ticks=100 io_ticks=80 reads=0 read_bytes=0 writes=1 write_bytes=4096 vectors=1 flushes=1"
              for n in range(1, 130)]
    for op in profile.OPERATIONS:
        calls, ticks, size = (129, 5160, 528384 if op == "Write" else 0) if op in ("Write", "Flush") else (0, 0, 0)
        lines.append(f"TURSO-SQL: operation={op} calls={calls} ticks={ticks} bytes={size}")
    return "\n".join(lines + ["TURSO-SQL: PASS", "STORAGE: filesystem grant retired"])


class Tests(unittest.TestCase):
    def test_valid(self):
        rate, rows, operations = profile.parse(fixture())
        self.assertEqual((rate, len(rows), operations["Write"]), (1000, 129, (129, 5160, 528384)))

    def test_corrupt(self):
        text = fixture()
        for wrong in [
            text.replace("ticks_per_ms=1000", "ticks_per_ms=0"),
            text.replace("revision=1 ", "revision=2 ", 1),
            text.replace("io_ticks=80", "io_ticks=101", 1),
            text.replace("flushes=1", "flushes=0", 1),
            text.replace("vectors=1", "vectors=2", 1),
            text.replace("bytes=528384", "bytes=1"),
            text.replace("calls=129", "calls=128", 1),
            text.replace("operation=Close", "operation=Size"),
            text.replace("TURSO-SQL: PASS", ""),
            text.replace("STORAGE: filesystem grant retired", ""),
            text + "\nTURSO-SQL: garbage",
            text + "\nTEST: FAIL simulated",
        ]:
            with self.subTest(wrong=wrong[-100:]):
                with self.assertRaises(AssertionError): profile.parse(wrong)


if __name__ == "__main__":
    unittest.main()
