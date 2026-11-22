import importlib.util
import pathlib
import unittest


def load(name, filename):
    spec = importlib.util.spec_from_file_location(name, pathlib.Path(__file__).with_name(filename))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


profile = load("nvme_waits", "check-nvme-waits.py")
sql_test = load("sql_fixture", "test-sql-profile.py")


def fixture():
    return sql_test.fixture() + "\n" + "\n".join(
        f"NVME-WAIT: interval= {interval} kind={kind} commands= 64 ticks= 8000 slow= 10 sleeps= 9"
        for interval in (1, 2) for kind in profile.KINDS)


class Tests(unittest.TestCase):
    def test_valid(self):
        rate, rows = profile.parse(fixture())
        self.assertEqual(rate, 1000)
        self.assertEqual(len(rows), 6)
        self.assertEqual(rows[-1], (2, "flush", 64, 8000, 10, 9))

    def test_corrupt(self):
        text = fixture()
        for wrong in [
            sql_test.fixture(),
            text.rsplit("\n", 1)[0],
            text.replace("interval= 2", "interval= 3"),
            text.replace("kind=write", "kind=read"),
            text.replace("kind=read", "kind=0"),
            text.replace("slow= 10", "slow= 65", 1),
            text.replace("sleeps= 9", "sleeps= 10001", 1),
            text.replace("slow= 10", "slow= 0", 1),
            text.replace("ticks= 8000", "ticks= 0", 1),
            text.replace("kind=flush commands= 64", "kind=flush commands= 63"),
            text + "\nNVME-WAIT: incomplete",
            text.replace("TURSO-SQL: PASS", ""),
        ]:
            with self.subTest(wrong=wrong[-200:]):
                with self.assertRaises(AssertionError):
                    profile.parse(wrong)


if __name__ == "__main__":
    unittest.main()
