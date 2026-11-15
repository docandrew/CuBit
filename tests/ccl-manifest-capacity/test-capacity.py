#!/usr/bin/env python3
"""The manifest compiler with storage sized by its input (no fixed program
limit): manifests past the old CCL limits compile to loadable sections, and
one past a named ceiling is refused with its typed capacity error, never
truncated. Against the built tool (make -C kernel ccl-manifest)."""
import pathlib
import struct
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]
TOOL = ROOT / 'userspace/ccl/build/manifest/ccl-manifest'
CATALOG = ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl'
SCHEMA = ROOT / 'userspace/ccl/interfaces/executable-manifest.ccl'
LARGE = ROOT / 'tests/large-manifest-probe/manifest.ccl'
DESKTOP = ROOT / 'userspace/services/desktop/manifest.ccl'
# The section magics (CCL.Manifests.Encoding, CuBit.Launch_Authority,
# CuBit.Program_Descriptions).
ACCESS_MAGIC, LAUNCH_MAGIC, DESCRIPTION_MAGIC = b'CACC', b'LNCH', b'PDSC'
# The old manifest compiler read at most this many bytes of manifest.
OLD_FILE_BOUND = 4096


def sections(assembly):
    """Each .section's bytes, from the tool's assembly."""
    out, name = {}, None
    for line in assembly.splitlines():
        if line.startswith('.section'):
            name = line.split()[1].split(',')[0]
            out[name] = bytearray()
        elif name and line.startswith('.byte 0x'):
            out[name].append(int(line[len('.byte 0x'):], 16))
    return {k: bytes(v) for k, v in out.items()}


class Capacity(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='ccl-manifest-capacity.')
        self.addCleanup(self.temp.cleanup)
        self.dir = pathlib.Path(self.temp.name)

    def compile(self, source):
        manifest = self.dir / 'manifest.ccl'
        manifest.write_text(source)
        return subprocess.run([TOOL, CATALOG, manifest, '--startup-output', self.dir / 'startup.ads',
                               '--schema', SCHEMA], capture_output=True, text=True, timeout=60)

    def test_large_manifest_compiles_to_loadable_sections(self):
        source = LARGE.read_text()
        self.assertGreater(len(source), 2 * OLD_FILE_BOUND)
        result = self.compile(source)
        self.assertEqual(result.returncode, 0, result.stderr)
        found = sections(result.stdout)
        access = found['.cubit.access']
        self.assertEqual(access[:4], ACCESS_MAGIC)
        self.assertEqual(struct.unpack_from('<H', access, 6)[0], 16)
        launch = found['.cubit.launch']
        self.assertEqual(launch[:4], LAUNCH_MAGIC)
        self.assertEqual(struct.unpack_from('<H', launch, 6)[0], 24)
        self.assertEqual(found['.cubit.description'][:4], DESCRIPTION_MAGIC)

    def test_desktop_typed_manifest_compiles(self):
        result = self.compile(DESKTOP.read_text())
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('.cubit.startup', sections(result.stdout))
        # Its metric outlet's optional startup entry (MET-002).
        self.assertIn('stdmetric', (self.dir / 'startup.ads').read_text())

    def test_named_ceilings_refuse_with_their_capacity_error(self):
        source = LARGE.read_text()
        # Two long program names more than the launch table's 512 bytes hold.
        long_names = ' '.join(f'(Launch "lmp-child-with-a-long-name-for-the-launch-table-ceiling-{n}.app")'
                              for n in (25, 26))
        more = source.replace('(Launch "lmp-child-24.app")]', '(Launch "lmp-child-24.app") ' + long_names + ']')
        self.assertNotEqual(more, source)
        result = self.compile(more)
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stdout, '', 'a refused manifest emitted metadata')
        self.assertIn('over the 512 a launch table holds', result.stderr)
        # A seventeenth access scope: the manifest's 16 (TOO_MANY_SCOPES).
        more = source.replace('"@mem:0/large-manifest-probe/declared-read-only-area-number-16/")]',
                              '"@mem:0/large-manifest-probe/declared-read-only-area-number-16/")\n'
                              '    (filesystem [File_Right.Read] "@mem:0/large-manifest-probe/one-more/")]')
        self.assertNotEqual(more, source)
        result = self.compile(more)
        self.assertEqual(result.returncode, 1)
        self.assertIn('TOO_MANY_SCOPES', result.stderr)


if __name__ == '__main__':
    unittest.main(verbosity=1)
