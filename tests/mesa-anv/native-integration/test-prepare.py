"""Exercise preparation and fail-closed guards without building or booting."""
import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location(
    'prepare', Path(__file__).with_name('prepare-admission-probe.py'))
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)
ROOT = Path(__file__).resolve().parents[3]


class Hooks(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='cubit-admission-hooks-')
        self.addCleanup(self.temp.cleanup)
        self.source = Path(self.temp.name)
        self.workspace = self.source / '.build-workspaces' / 'fixture'
        self.workspace.mkdir(parents=True)
        self.entries = []
        for role in ('client', 'server'):
            for filename in ('main.adb', f'ipctest_{role}.gpr'):
                name = f'userspace/apps/ipctest-{role}/{filename}'
                data = (ROOT / name).read_bytes()
                destination = self.workspace / name
                destination.parent.mkdir(parents=True, exist_ok=True)
                destination.write_bytes(data)
                self.entries.append({'path': name,
                                     'sha256': hashlib.sha256(data).hexdigest()})
        self.metadata = {'complete': True, 'source': str(self.source),
                         'inputs': self.entries}
        self.save_metadata()

    def save_metadata(self):
        (self.workspace / '.cubit-build-workspace.json').write_text(
            json.dumps(self.metadata))

    def test_hooks_and_repeat_refusal(self):
        MODULE.prepare(self.workspace)
        receipt = json.loads((self.workspace / 'admission-probe-hooks.json').read_text())
        self.assertEqual(len(receipt['edits']), 4)
        client = (self.workspace / self.entries[0]['path']).read_text()
        self.assertIn('GPU_Admission_Probe.Client (CAP_SLOT_IPCTEST);', client)
        self.assertLess(client.index('if readFPUProbe /= 0'),
                        client.index('GPU_Admission_Probe.Client'))
        with self.assertRaisesRegex(ValueError, 'already prepared'):
            MODULE.prepare(self.workspace)

    def test_incomplete(self):
        self.metadata['complete'] = False
        self.save_metadata()
        with self.assertRaisesRegex(ValueError, 'complete'):
            MODULE.prepare(self.workspace)

    def test_main_checkout_rejected(self):
        self.metadata['source'] = str(self.workspace)
        self.save_metadata()
        with self.assertRaisesRegex(ValueError, 'independent'):
            MODULE.prepare(self.workspace)

    def test_changed_source_no_partial_edits(self):
        target = self.workspace / self.entries[-1]['path']
        target.write_text(target.read_text() + '\n-- changed\n')
        before = {e['path']: (self.workspace / e['path']).read_bytes()
                  for e in self.entries}
        with self.assertRaisesRegex(ValueError, 'already changed'):
            MODULE.prepare(self.workspace)
        for name, data in before.items():
            self.assertEqual((self.workspace / name).read_bytes(), data)

    def test_ambiguous_anchor(self):
        entry = self.entries[-1]
        path = self.workspace / entry['path']
        data = path.read_bytes() + b'\n"-O2",\n'
        path.write_bytes(data)
        entry['sha256'] = hashlib.sha256(data).hexdigest()
        self.save_metadata()
        with self.assertRaisesRegex(ValueError, 'ambiguous'):
            MODULE.prepare(self.workspace)
        self.assertFalse((self.workspace / 'admission-probe-hooks.json').exists())


unittest.main()
