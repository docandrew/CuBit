#!/usr/bin/env python3
"""Record-chain regression only; no native compilation or execution."""
import hashlib
import json
from pathlib import Path
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / 'tools'))
from verify_native_mesa_build import load_record

class ContinuationTests(unittest.TestCase):
    def test_chain(self):
        with tempfile.TemporaryDirectory() as name:
            root = Path(name)
            original = {'status': 'FAILED', 'inputs_sha256': {'source': 'identity'}}
            initial = root / 'build.json'
            initial.write_text(json.dumps(original))
            continued = {'status': 'LINK_PASS', 'inputs_sha256': original['inputs_sha256'],
                         'prior_failed_build_sha256': hashlib.sha256(initial.read_bytes()).hexdigest()}
            path = root / 'continuation.json'
            path.write_text(json.dumps(continued))
            self.assertEqual(load_record(root, True), continued)
            with self.assertRaisesRegex(ValueError, 'not complete'):
                load_record(root)
            for field, value, error in (
                ('status', 'BUILDING', 'not complete'),
                ('inputs_sha256', {}, 'input identities'),
                ('prior_failed_build_sha256', 'wrong', 'prior failed')):
                path.write_text(json.dumps(continued | {field: value}))
                with self.assertRaisesRegex(ValueError, error):
                    load_record(root, True)
            path.write_text(json.dumps(continued))
            initial.write_text(json.dumps(original) + '\n')
            with self.assertRaisesRegex(ValueError, 'prior failed'):
                load_record(root, True)
            initial.write_text(json.dumps(original | {'status': 'LINK_PASS'}))
            with self.assertRaisesRegex(ValueError, 'retained failed'):
                load_record(root, True)
            self.assertEqual(load_record(root)['status'], 'LINK_PASS')

if __name__ == '__main__':
    unittest.main()
