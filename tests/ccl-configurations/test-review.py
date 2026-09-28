#!/usr/bin/env python3
"""Linux-hosted semantic review; independent dictionary oracle, no native writes."""
import pathlib
import random
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]
TOOL = ROOT / 'userspace/ccl/build/config-review/ccl-config-review'


def document(settings):
    def expression(value):
        # Expand at evaluation time, within the frontend's source-text pool.
        if len(value) in (512, 1024) and len(set(value)) == 1:
            steps = len(value).bit_length() - 6
            result = f'x{steps}'
            for i in reversed(range(1, steps + 1)):
                result = f'(let ((x{i} (concat x{i-1} x{i-1}))) {result})'
            return f'(let ((x0 "{value[:32]}")) {result})'
        if len(value) <= 128:
            return f'"{value}"'
        middle = len(value) // 2
        return f'(concat {expression(value[:middle])} {expression(value[middle:])})'
    return '(system-config v1\n' + ''.join(
        f' (setting "{key}" {expression(value)})\n' for key, value in settings.items()) + ')'


class Review(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='config-review.')
        self.addCleanup(self.temp.cleanup)
        self.before = pathlib.Path(self.temp.name) / 'before.ccl'
        self.after = pathlib.Path(self.temp.name) / 'after.ccl'

    def review(self, before, after):
        self.before.write_text(before)
        self.after.write_text(after)
        result = subprocess.run([TOOL, self.before, self.after],
                                text=True, capture_output=True, timeout=5)
        self.assertNotIn('raised ', result.stderr)
        self.assertEqual(self.before.read_text(), before)
        self.assertEqual(self.after.read_text(), after)
        self.assertEqual(set(pathlib.Path(self.temp.name).iterdir()),
                         {self.before, self.after})
        return result

    def test_actions_and_no_payload_disclosure(self):
        result = self.review(document({'keep': 'same', 'edit': 'secret-old', 'gone': 'x'}),
                             document({'edit': 'secret-new', 'keep': 'same', 'new': 'private'}))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'replace edit\nadd new\nremove gone\n')
        self.assertEqual(result.stderr, '')

    def test_evaluated_values_not_expression_spelling(self):
        result = self.review('(system-config v1 (setting "n" (+ 20 22)) (setting "b" true))',
                             '(system-config v1 (setting "b" "true") (setting "n" "42"))')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'no effective setting changes\n')

    def test_failures_publish_no_partial_review(self):
        valid = document({'one': '1'})
        invalid = ['', '(', '(system-config v2 (setting "x" 1))',
                   '(system-config v1 (setting "x" 1) (setting "x" 2))',
                   '(startup v1 (start "foo.app" (priority 5)))',
                   valid + ' trailing', ' ' * 8193,
                   document({'a' * 129: 'x'}), document({'a': 'x' * 1025})]
        for bad in invalid:
            for before, after in [(bad, valid), (valid, bad)]:
                with self.subTest(before=before[:60], after=after[:60]):
                    result = self.review(before, after)
                    self.assertEqual(result.returncode, 1)
                    self.assertEqual(result.stdout, '')
                    self.assertTrue(result.stderr)

    def test_maximum_entries_all_replaced_or_disjoint(self):
        old = {f'a{i}': 'old' for i in range(128)}
        for new in [{key: 'new' for key in old}, {f'b{i}': 'new' for i in range(128)}]:
            result = self.review(document(old), document(new))
            self.assertEqual(result.returncode, 0, result.stderr)
            expected = [('replace ' if key in old else 'add ') + key for key in new]
            expected += ['remove ' + key for key in old if key not in new]
            self.assertEqual(result.stdout.splitlines(), expected)

    def test_maximum_key_and_value_and_length_changes(self):
        key = 'k' * 128
        for old, new in [('x' * 1024, 'x' * 512), ('x' * 1024, 'y' * 1024),
                         ('x', 'xx'), ('same', 'same')]:
            result = self.review(document({key: old}), document({key: new}))
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, ('no effective setting changes\n' if old == new
                                             else 'replace ' + key + '\n'))

    def test_random_dictionary_oracle(self):
        rng = random.Random(20260926)
        for trial in range(150):
            old = {f'k{i}': str(rng.randrange(4)) for i in rng.sample(range(50), rng.randrange(1, 50))}
            new = {f'k{i}': str(rng.randrange(4)) for i in rng.sample(range(50), rng.randrange(1, 50))}
            expected = [('replace ' if key in old else 'add ') + key
                        for key in new if key not in old or old[key] != new[key]]
            expected += ['remove ' + key for key in old if key not in new]
            with self.subTest(trial=trial):
                result = self.review(document(old), document(new))
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout.splitlines(), expected or ['no effective setting changes'])

    def test_missing_file_and_usage(self):
        for args in [[], [self.before], [self.before, self.after], ['a', 'b', 'c']]:
            result = subprocess.run([TOOL, *args], text=True, capture_output=True, timeout=5)
            self.assertEqual(result.returncode, 1)
            self.assertEqual(result.stdout, '')
            self.assertNotIn('raised ', result.stderr)


if __name__ == '__main__':
    unittest.main()
