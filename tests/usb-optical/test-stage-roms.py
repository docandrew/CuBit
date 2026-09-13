#!/usr/bin/env python3
"""Host-side tests for explicit cartridge staging; uses only our test ROM."""
import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest

root = pathlib.Path(__file__).resolve().parents[2]
fixture = root / 'userspace/c/sameboy_build/test.gb'
stager = pathlib.Path(__file__).with_name('stage-roms.py')


class CartridgeStaging(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='cubit-rom-staging.')
        self.addCleanup(self.temp.cleanup)
        self.base = pathlib.Path(self.temp.name)
        self.source = self.base / 'private'
        self.source.mkdir()
        self.destination = self.base / 'output'

    def stage(self, explicit=True):
        args = [sys.executable, str(stager), str(self.destination)]
        if explicit:
            args += ['--directory', str(self.source)]
        return subprocess.run(args, capture_output=True, text=True)

    def test_private_inputs_are_opt_in(self):
        shutil.copyfile(fixture, self.source / 'private.gb')
        self.assertEqual(self.stage(False).returncode, 0)
        self.assertEqual([p.name for p in self.destination.iterdir()], ['00.gb'])

    def test_sorted_copy_without_modifying_originals(self):
        for name in ('b.gbc', 'a.gb', 'ignored.txt'):
            shutil.copyfile(fixture, self.source / name)
        result = self.stage()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('ROM 01: a.gb', result.stdout)
        self.assertIn('ROM 02: b.gbc', result.stdout)
        self.assertEqual(sorted(p.name for p in self.destination.iterdir()),
                         ['00.gb', '01.gb', '02.gb'])
        for path in self.source.iterdir():
            self.assertEqual(path.read_bytes(), fixture.read_bytes())
        self.assertEqual((self.destination / '02.gb').read_bytes(), fixture.read_bytes())

    def test_too_many_is_rejected_before_copy(self):
        for index in range(16):
            shutil.copyfile(fixture, self.source / f'{index}.gb')
        self.assertNotEqual(self.stage().returncode, 0)
        self.assertFalse(self.destination.exists())

    def test_invalid_sizes_are_rejected_before_copy(self):
        for size in (0x14f, 8 * 1024 * 1024 + 1):
            with self.subTest(size=size):
                with (self.source / 'bad.gb').open('wb') as file:
                    file.truncate(size)
                self.assertNotEqual(self.stage().returncode, 0)
                self.assertFalse(self.destination.exists())

    def test_missing_directory_does_not_silently_omit_roms(self):
        self.source = self.base / 'missing'
        self.assertNotEqual(self.stage().returncode, 0)
        self.assertFalse(self.destination.exists())


if __name__ == '__main__':
    unittest.main()
