#!/usr/bin/env python3
"""Checked CCL configuration output and equivalence to the old boot profiles."""
import pathlib
import random
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]
TOOL = ROOT / 'userspace/ccl/build/config/ccl-config'
FIXTURES = pathlib.Path(__file__).with_name('fixtures')


class Configurations(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='ccl-config-test.')
        self.addCleanup(self.temp.cleanup)
        self.source = pathlib.Path(self.temp.name) / 'profile.ccl'

    def compile(self, text):
        self.source.write_text(text)
        result = subprocess.run([TOOL, self.source, "--dump-plan"], text=True, capture_output=True, timeout=5)
        self.assertNotIn('raised ', result.stderr)
        return result

    def reject(self, text, diagnostic=None):
        result = self.compile(text)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertEqual(result.stdout, '', 'failed compile emitted partial config')
        if diagnostic:
            self.assertIn(diagnostic, result.stderr)

    def test_render_requires_explicit_approval(self):
        base = '(startup v1 (start "render.app" (priority 5) %s))'
        for clause in ('', '(render deny)'):
            result = self.compile(base % clause)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, 'render.app pri=5\n')
        result = self.compile(base % '(render approve-declared)')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'render.app pri=5 render=declared\n')
        for clause in ('(render all)', '(render true)', '(render 1)',
                       '(render)', '(render approve-declared extra)'):
            self.reject(base % clause)
        for first in ('deny', 'approve-declared'):
            for second in ('deny', 'approve-declared'):
                self.reject(base % f'(render {first}) (render {second})',
                            'DUPLICATE_FIELD')

    def test_all_existing_profiles(self):
        fixtures = sorted(FIXTURES.rglob('*.conf'))
        self.assertEqual(len(fixtures), 28)
        for fixture in fixtures:
            relative = fixture.relative_to(FIXTURES)
            source = ROOT / relative.with_suffix('.ccl')
            with self.subTest(profile=relative):
                expected = ''.join(line + '\n' for line in fixture.read_text().splitlines()
                                   if line and not line.startswith('#'))
                if relative.as_posix() in ('system.conf', 'tests/hardware/system-live.conf'):
                    # Intentional post-migration default added with RTC time.
                    # Preserve the legacy oracle and account for the exact key.
                    self.assertNotIn('clock.time-zone=', expected)
                    expected = 'clock.time-zone=UTC\n' + expected
                    expected = ('desktop.appearance.theme.light=(theme v1 (base alloy-light))\n'
                                'desktop.appearance.theme.dark=(theme v1 (base alloy-dark))\n' + expected)
                    if relative.as_posix() == 'system.conf':
                        expected = expected.replace('clock.time-zone=UTC\n',
                            'clock.time-zone=UTC\ntime.servers=time.cloudflare.com 0.pool.ntp.org 1.pool.ntp.org 2.pool.ntp.org\n')
                elif relative.as_posix() in ('init.conf', 'tests/headless/init-desktop-session.conf'):
                    # Explicit, reviewed post-migration service additions.
                    added = ('timesync.svc pri=5 network=declared\n'
                             if relative.as_posix() == 'init.conf' else '')
                    expected = expected.replace('clock.svc pri=5\n',
                        'clock.svc pri=5\n' + added + 'tls.svc pri=5 network=declared\n')
                elif relative.as_posix() == 'tests/headless/init-capability-security.conf':
                    # The committed construction-failure tests intentionally
                    # extended this profile after the original migration.
                    expected = ('bad-phdr.app pri=5\nbad-segment.app pri=5\n'
                                'bad-stack.app pri=5\n' + 'overlap.app pri=5\n' * 8 + expected)
                result = self.compile(source.read_text())
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, expected)
                self.assertFalse((ROOT / relative).exists(), 'old source .conf still present')

    def test_real_ccl_field_expressions(self):
        result = self.compile('''# Ordinary CCL, no host callbacks
          (system-config v1
            (setting (concat "net." "name") (concat "Cu" "Bit"))
            (setting "count" (* 6 7))
            (setting "negative" -4)
            (setting "enabled" (if (= 2 2) true false)))''')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'net.name=CuBit\ncount=42\nnegative=-4\nenabled=true\n')

    def test_explicit_approval_and_order(self):
        result = self.compile('''(startup v1
          (start "a.app" (priority (+ 2 3)))
          (start "a.app" (network approve-declared) (priority 5))
          (start "b.svc" (priority 4) (network deny)))''')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'a.app pri=5\na.app pri=5 network=declared\nb.svc pri=4\n')

    def test_rejected_config(self):
        for text, diagnostic in [
            ('(system-config v1 (set "x" 1))', 'UNKNOWN_DECLARATION'),
            ('(system-config v2 (setting "x" 1))', 'UNSUPPORTED_VERSION'),
            ('(system-config v1)', 'MISSING_FIELD'),
            ('(startup v1)', 'MISSING_FIELD'),
            ('(system-config v1 (setting "x" ""))', 'INVALID_VALUE'),
            ('(system-config v1 (setting "" 1))', 'INVALID_KEY'),
            ('(system-config v1 (setting "a=b" 1))', 'INVALID_KEY'),
            ('(system-config v1 (setting "a b" 1))', 'INVALID_KEY'),
            ('(system-config v1 (setting "x" "a\\nb=2"))', 'INVALID_VALUE'),
            ('(system-config v1 (setting "x" "a\\rb=2"))', 'INVALID_VALUE'),
            ('(system-config v1 (setting "x" "é"))', 'INVALID_VALUE'),
            ('(system-config v1 (setting "x" 1) (setting "x" 2))', 'DUPLICATE_KEY'),
            ('(system-config v1 (unknown "x" 1))', 'UNKNOWN_DECLARATION'),
            ('(system-config v1 (setting "x" (clock.monotonic-ms)))', 'INVALID_SYNTAX'),
            ('(system-config v1 (setting "x" 1)) trailing', 'TRAILING_INPUT')]:
            with self.subTest(text=text):
                self.reject(text, diagnostic)

    def test_storage_role_is_explicit_and_unique(self):
        start = '(start "store.svc" (priority 5) (role config-storage))'
        result = self.compile(f'(startup v1 {start})')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'store.svc pri=5 role=config-storage\n')
        self.reject(f'(startup v1 {start} {start})', 'INVALID_ROLE')
        self.reject(f'(startup v1 {start.replace("config-storage", "root")})', 'INVALID_ROLE')
        self.reject('(startup v1 (start "store.svc" (priority 5) (role application) (role config-storage)))', 'DUPLICATE_FIELD')

    def test_rejected_launches(self):
        base = '(startup v1 (start "a.app" (priority 5)))'
        for name in ('../evil.app', '/a.app', 'a.app network=declared', '@mem:0/a.app', '.app', 'a.'):
            self.reject(base.replace('a.app', name), 'INVALID_EXECUTABLE')
        for priority in ('0', '11', '-1', 'true', '"5"'):
            self.reject(base.replace('priority 5', f'priority {priority}'), 'INVALID_PRIORITY')
        self.reject('(startup v1 (start "a.app"))', 'MISSING_FIELD')
        self.reject(base.replace('(priority 5)', '(priority 5) (priority 4)'), 'DUPLICATE_FIELD')
        self.reject(base.replace('(priority 5)', '(priority 5) (network yes)'), 'INVALID_APPROVAL')
        self.reject(base.replace('(priority 5)', '(priority 5) (network deny) (network approve-declared)'),
                    'DUPLICATE_FIELD')
        self.reject(base.replace('(priority 5)', '(priority 5) (grant everything)'), 'UNKNOWN_DECLARATION')

    def test_symbolic_format_versions(self):
        for kind, fields in (("system-config", '(setting "x" 1)'),
                             ("startup", '(start "a.app" (priority 5))')):
            for token in ("1", "2", "v2", "V1", '"v1"', "(+ 0 1)"):
                with self.subTest(kind=kind, token=token):
                    self.reject(f"({kind} {token} {fields})")

    def test_bounds(self):
        self.reject('#' * 8193)
        for length, good in ((128, True), (129, False)):
            source = f'(system-config v1 (setting "{"k" * length}" 1))'
            if good:
                self.assertEqual(self.compile(source).returncode, 0)
            else:
                self.reject(source, 'INVALID_KEY')
        self.reject('(system-config v1 ' + ' '.join(f'(setting "k{i}" 1)' for i in range(129)) + ')',
                    'TOO_MANY_ENTRIES')
        self.assertEqual(self.compile('(startup v1 ' + '(start "a.app" (priority 5))' * 16 + ')').returncode, 0)
        self.reject('(startup v1 ' + '(start "a.app" (priority 5))' * 17 + ')', 'TOO_MANY_ENTRIES')
        value = 'x5'
        for i in reversed(range(1, 6)):
            value = f'(let ((x{i} (concat x{i-1} x{i-1}))) {value})'
        value = f'(let ((x0 "{"x" * 32}")) {value})'
        source = '(system-config v1 ' + ' '.join(f'(setting "k{i}" {value})' for i in range(33)) + ')'
        self.assertLessEqual(len(source), 8192)
        self.assertEqual(self.compile(source).returncode, 0)

    def test_preflight_is_silent(self):
        self.source.write_text('(startup v1 (start "a.app" (priority 5)))')
        result = subprocess.run([TOOL, self.source], text=True, capture_output=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, '')

    def test_malformed_inputs_do_not_raise(self):
        source = '(startup v1 (start "a.app" (priority (let ((n 4)) (+ n 1))) (network deny)))'
        for end in range(len(source)):
            self.reject(source[:end])
        rng = random.Random(1729)
        for _ in range(100):
            text = ''.join(rng.choice('()#"\\abc 123\n') for _ in range(rng.randrange(80)))
            result = self.compile(text)
            self.assertIn(result.returncode, (0, 1), result.stderr)

    def test_startup_policy_fields(self):
        policy = '''(startup v1
          (start "logstore.svc" (priority 5))
          (start "ps2.drv" (priority 5) (launch per-device) (approve-device)
            (after "logstore.svc") (ready-deadline-ms 2000))
          (start "hda.drv" (priority 5) (launch per-device) (approve-device))
          (start "mixer.svc" (priority 5) (after "logstore.svc" "hda.drv")
            (approve-scheduling realtime (budget-us 1500) (period-us 5000))))'''
        result = self.compile(policy)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout,
                         'logstore.svc pri=5\n'
                         'ps2.drv pri=5 launch=per-device device=approved after=1 ready-ms=2000\n'
                         'hda.drv pri=5 launch=per-device device=approved\n'
                         'mixer.svc pri=5 after=1,3 scheduling=1500/5000\n')
        for old, bad, diagnostic in [
                # Dependencies name an earlier, unique entry: acyclic by construction.
                ('(after "logstore.svc" "hda.drv")', '(after "mixer.svc")', 'INVALID_DEPENDENCY'),
                ('(after "logstore.svc" "hda.drv")', '(after "nope.svc")', 'INVALID_DEPENDENCY'),
                ('(after "logstore.svc" "hda.drv")', '(after "hda.drv" "hda.drv")', 'INVALID_DEPENDENCY'),
                ('(after "logstore.svc" "hda.drv")', '(after "logstore.svc") (after "hda.drv")',
                 'DUPLICATE_FIELD'),
                ('(after "logstore.svc" "hda.drv")', '(after 5)', 'INVALID_EXECUTABLE'),
                # Device resources go only to per-device drivers, which need them.
                ('(launch per-device) (approve-device)\n            (after',
                 '(approve-device)\n            (after', 'INVALID_LAUNCH_MODE'),
                ('(launch per-device) (approve-device))', '(launch per-device))',
                 'INVALID_LAUNCH_MODE'),
                ('(launch per-device) (approve-device))', '(launch sometimes) (approve-device))',
                 'INVALID_LAUNCH_MODE'),
                ('(launch per-device) (approve-device))',
                 '(launch per-device) (approve-device) (role config-storage))', 'INVALID_ROLE'),
                ('(launch per-device) (approve-device))',
                 '(launch per-device) (approve-device) (approve-device))', 'DUPLICATE_FIELD'),
                # The kernel admits budget <= period and at most 70% of a CPU.
                ('(budget-us 1500)', '(budget-us 3501)', 'INVALID_SCHEDULING'),
                ('(budget-us 1500)', '(budget-us 0)', 'INVALID_SCHEDULING'),
                ('(period-us 5000)', '(period-us 1073741825)', 'INVALID_SCHEDULING'),
                ('(budget-us 1500)', '(budget "1500")', 'INVALID_SCHEDULING'),
                ('realtime (budget-us', 'batch (budget-us', 'INVALID_SCHEDULING'),
                ('(ready-deadline-ms 2000)', '(ready-deadline-ms 0)', 'INVALID_DEADLINE'),
                ('(ready-deadline-ms 2000)', '(ready-deadline-ms 60001)', 'INVALID_DEADLINE'),
                ('(ready-deadline-ms 2000)', '(ready-deadline-ms 2000) (ready-deadline-ms 1)',
                 'DUPLICATE_FIELD')]:
            with self.subTest(bad=bad):
                self.assertIn(old, policy)
                self.reject(policy.replace(old, bad), diagnostic)
        self.assertEqual(self.compile(policy.replace('(budget-us 1500)', '(budget-us 3500)')).returncode, 0)
        # A name started twice is an ambiguous dependency.
        twice = policy.replace('(start "logstore.svc" (priority 5))',
                               '(start "logstore.svc" (priority 5)) (start "logstore.svc" (priority 5))')
        self.reject(twice, 'INVALID_DEPENDENCY')
        for end in range(len(policy)):
            self.reject(policy[:end])


if __name__ == '__main__':
    unittest.main()
