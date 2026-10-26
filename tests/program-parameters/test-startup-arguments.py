#!/usr/bin/env python3
"""The image build's check of startup-plan arguments (ccl-config PROFILE
--programs DIR, docs/ccl-launch-parameters.md), Linux-hosted: the programs
are the manifest objects of tests/program-parameters/check and plain (their
.cubit.parameters section is all the check reads)."""
import pathlib
import shutil
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]
MANIFEST_TOOL = ROOT / 'userspace/ccl/build/manifest/ccl-manifest'
CONFIG_TOOL = ROOT / 'userspace/ccl/build/config/ccl-config'
CATALOG = ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl'
SCHEMA = ROOT / 'userspace/ccl/interfaces/executable-manifest.ccl'
GOOD = '''(startup v1
  (start "parameters-check.app" (priority 3)
    (arguments (Check_Parameters count => 64 deadline => (Check_Deadline.Within 2500))))
  (start "parameters-plain.app" (priority 3))
  (start "elsewhere.app" (priority 3))
)
'''


class StartupArguments(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp = tempfile.TemporaryDirectory(prefix='startup-arguments.')
        cls.programs = pathlib.Path(cls.temp.name) / 'programs'
        cls.programs.mkdir()
        for name in ('check', 'plain'):
            work = pathlib.Path(cls.temp.name) / name
            work.mkdir()
            assembly = subprocess.run(
                [MANIFEST_TOOL, CATALOG, ROOT / 'tests/program-parameters' / name / 'manifest.ccl',
                 '--schema', SCHEMA, '--startup-output', work / 'startup.ads'],
                capture_output=True, text=True, check=True).stdout
            (work / 'manifest.S').write_text(assembly)
            subprocess.run(['gcc', '-c', work / 'manifest.S', '-o', cls.programs / f'parameters-{name}.app'],
                           check=True)

    @classmethod
    def tearDownClass(cls):
        cls.temp.cleanup()

    def check(self, profile, programs=None):
        path = pathlib.Path(self.temp.name) / 'init.ccl'
        path.write_text(profile)
        return subprocess.run([CONFIG_TOOL, path, '--programs', programs or self.programs],
                              capture_output=True, text=True, timeout=60)

    def test_good_plan(self):
        result = self.check(GOOD)
        self.assertEqual((result.returncode, result.stderr), (0, ''))

    def test_refusals_name_line_and_column(self):
        for old, new, message in [
                ('    (arguments (Check_Parameters count => 64 deadline => (Check_Deadline.Within 2500))))',
                 ')', 'line 2, column 10: parameters-check.app requires its parameters, a Check_Parameters'),
                ('count => 64', 'count => 0', 'line 3, column 16: parameters-check.app: not a Check_Parameters: '
                 'Value is outside the range of its type'),
                ('count => 64 ', '', 'line 3, column 17: parameters-check.app: not a Check_Parameters: '
                 'field count: This field has no default'),
                ('count => 64', 'count => 64 colour => 1', 'line 3, column 46: parameters-check.app: not a Check_Parameters: field colour: This record has no field'),
                ('(Check_Deadline.Within 2500)', 'Check_Deadline.Never', 'not a Check_Parameters'),
                ('(start "parameters-plain.app" (priority 3))',
                 '(start "parameters-plain.app" (priority 3) (arguments (Check_Parameters count => 1 '
                 'deadline => Check_Deadline.Forever)))',
                 'line 4, column 57: parameters-plain.app declares no parameters, so it takes no arguments'),
                ('(start "elsewhere.app" (priority 3))',
                 '(start "elsewhere.app" (priority 3) (arguments (X a => 1)))',
                 'line 5, column 50: elsewhere.app has arguments, but')]:
            with self.subTest(new=new):
                profile = GOOD.replace(old, new, 1)
                self.assertNotEqual(profile, GOOD)
                result = self.check(profile)
                self.assertEqual(result.returncode, 1, result.stderr)
                self.assertIn(message, result.stderr)

    def test_value_size_limit_is_reported(self):
        """The raw arguments are bounded where the plan is read."""
        long_value = '(Check_Parameters count => 1 deadline => Check_Deadline.Forever' + ' ' * 1100 + ')'
        result = self.check(GOOD.replace('(Check_Parameters count => 64 deadline => (Check_Deadline.Within 2500))',
                                         long_value, 1))
        self.assertEqual(result.returncode, 1)
        self.assertIn('INVALID_ARGUMENTS', result.stderr)

    def test_tampered_descriptor(self):
        """A signature whose identity is not its SHA-256 is refused."""
        other = pathlib.Path(self.temp.name) / 'tampered'
        if other.exists():
            shutil.rmtree(other)
        shutil.copytree(self.programs, other)
        image = other / 'parameters-check.app'
        data = bytearray(image.read_bytes())
        at = data.find(b'(type Check_Count (range 1 4096))')
        self.assertGreater(at, 0)
        data[at + len('(type Check_Count (range 1 40')] = ord('1')
        image.write_bytes(bytes(data))
        result = self.check(GOOD, other)
        self.assertEqual(result.returncode, 1)
        self.assertIn('identity is not the SHA-256 of its signature', result.stderr)


if __name__ == '__main__':
    unittest.main()
