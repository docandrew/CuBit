#!/usr/bin/env python3
"""Hosted declaration rejection tests and independent ELF ABI comparison."""
import pathlib
import random
import struct
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]
TOOL = ROOT / 'userspace/ccl/build/manifest/ccl-manifest'
CATALOG = (ROOT / 'userspace/ccl/catalogs/bootstrap-services.ccl').read_text()
SOURCE = (ROOT / 'userspace/ccl/apps/ccl-vm/manifest.ccl').read_text()


class Manifests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='ccl-manifest-test.')
        self.addCleanup(self.temp.cleanup)
        self.directory = pathlib.Path(self.temp.name)

    def compile(self, source=SOURCE, catalog=CATALOG):
        manifest = self.directory / 'manifest.ccl'
        definitions = self.directory / 'catalog.ccl'
        manifest.write_text(source)
        definitions.write_text(catalog)
        result = subprocess.run([TOOL, definitions, manifest, '--ada-output',
                                 self.directory / 'ccl_manifest_bindings.ads'], capture_output=True,
                                text=True, timeout=5)
        self.assertNotIn('raised ', result.stderr, result.stderr)
        return result

    def reject(self, source=SOURCE, catalog=CATALOG, diagnostic=None):
        result = self.compile(source, catalog)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertEqual(result.stdout, '', 'failed compilation emitted partial metadata')
        if diagnostic:
            self.assertIn(diagnostic, result.stderr)

    def sections(self, assembly):
        source = self.directory / 'manifest.S'
        obj = self.directory / 'manifest.o'
        source.write_text(assembly)
        subprocess.run(['gcc', '-c', source, '-o', obj], check=True, capture_output=True)
        return self.extract(obj)

    def extract(self, obj):
        sections = {}
        for name in ('.cubit.id', '.cubit.caps'):
            output = self.directory / (obj.name + name + '.bin')
            subprocess.run(['objcopy', '--dump-section', f'{name}={output}', obj],
                           check=True, capture_output=True)
            sections[name] = output.read_bytes()
        return sections

    def test_existing_elf_bytes_unchanged(self):
        legacy = self.directory / 'legacy.o'
        subprocess.run(['gcc', '-c', ROOT / 'tests/ccl-manifests/fixtures/ccl-vm-legacy.c',
                        '-o', legacy], check=True, capture_output=True)
        result = self.compile()
        self.assertEqual(result.returncode, 0, result.stderr)
        sections = self.sections(result.stdout)
        self.assertEqual(sections, self.extract(legacy))
        self.assertEqual(sections['.cubit.caps'],
                         struct.pack('<IHH', 0x43424954, 1, 2) +
                         struct.pack('<BBHIQ', 2, 3, 24, 18, 0) +
                         struct.pack('<BBHIQ', 2, 3, 25, 19, 0))
        native = ROOT / 'userspace/ccl/apps/ccl-vm/build/ccl-vm.app'
        self.assertEqual(sections, self.extract(native))

    def test_real_ccl_expressions_and_comments(self):
        source = SOURCE.replace('"com.cubit.ccl-vm"',
                                '(concat "com.cubit." "ccl-vm")')
        catalog = CATALOG.replace('application-slots 24',
                                  'application-slots (let ((base 20)) (+ base 4))')
        result = self.compile(source, catalog)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, self.compile().stdout)

    def test_catalog_is_data_not_compiler_builtin(self):
        result = self.compile(SOURCE.replace('clock', 'my-new-service'),
                              CATALOG.replace('(service clock 19', '(service my-new-service 9001'))
        self.assertEqual(result.returncode, 0, result.stderr)
        caps = self.sections(result.stdout)['.cubit.caps']
        self.assertEqual(struct.unpack_from('<I', caps, 28)[0], 9001)
        self.reject(catalog='(service-catalog 1 (application-slots 24 62))', diagnostic='UNKNOWN_SERVICE')

    def test_named_bindings_follow_layout_and_declaration_order(self):
        result = self.compile()
        self.assertEqual(result.returncode, 0, result.stderr)
        bindings = self.directory / 'ccl_manifest_bindings.ads'
        self.assertIn('Slot_test_host : constant Interfaces.Unsigned_64 := 24;', bindings.read_text())
        self.assertIn('Slot_clock : constant Interfaces.Unsigned_64 := 25;', bindings.read_text())
        result = self.compile(catalog=CATALOG.replace('application-slots 24 62', 'application-slots 30 31'))
        self.assertEqual(result.returncode, 0, result.stderr)
        caps = self.sections(result.stdout)['.cubit.caps']
        self.assertEqual(struct.unpack_from('<H', caps, 10)[0], 30)
        self.assertEqual(struct.unpack_from('<H', caps, 26)[0], 31)
        self.assertIn('Slot_test_host : constant Interfaces.Unsigned_64 := 30;', bindings.read_text())
        self.assertIn('Slot_clock : constant Interfaces.Unsigned_64 := 31;', bindings.read_text())
        requests = ['(request-service ccl-test-host read-write test-host)',
                    '(request-service clock read-write clock)']
        result = self.compile(SOURCE.replace(requests[0], 'PLACEHOLDER').replace(
            requests[1], requests[0]).replace('PLACEHOLDER', requests[1]))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('Slot_clock : constant Interfaces.Unsigned_64 := 24;', bindings.read_text())
        self.assertIn('Slot_test_host : constant Interfaces.Unsigned_64 := 25;', bindings.read_text())

    def test_binding_name_validation(self):
        for name in ['25', 'bad_name', 'Clock', 'a--b', 'a-', '-a', '"clock"', 'x;bad']:
            with self.subTest(name=name):
                self.reject(SOURCE.replace('read-write clock)', f'read-write {name})'),
                            diagnostic='INVALID_BINDING_NAME')
        self.reject(SOURCE.replace('read-write clock)', 'read-write test-host)'),
                    diagnostic='DUPLICATE_BINDING')

    def test_slot_layout_validation(self):
        for layout in ['0 62', '24 63', '30 29', '-1 62', 'true 62', '"24" 62']:
            self.reject(catalog=CATALOG.replace('24 62', layout), diagnostic='INVALID_SLOT')
        self.reject(catalog=CATALOG.replace('(application-slots 24 62)', ''), diagnostic='MISSING_FIELD')
        self.reject(catalog=CATALOG.replace('24 62', '24 24'), diagnostic='SLOTS_EXHAUSTED')
        self.reject(catalog=CATALOG.replace('(application-slots 24 62)',
                    '(application-slots 24 62) (application-slots 1 23)'), diagnostic='DUPLICATE_FIELD')

    def test_restricted_catalog_rights(self):
        catalog = CATALOG.replace('clock 19 read-write', 'clock 19 read')
        self.reject(catalog=catalog, diagnostic='RIGHTS_NOT_OFFERED')
        result = self.compile(SOURCE.replace('clock read-write', 'clock read'), catalog)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.sections(result.stdout)['.cubit.caps'][25], 1)

    def test_fail_closed(self):
        cases = [
            ('', 'UNEXPECTED_END'),
            (SOURCE[:-2], 'UNEXPECTED_END'),
            (SOURCE + ' junk', 'TRAILING_INPUT'),
            (SOURCE.replace('read-write', 'grant-all'), 'UNKNOWN_RIGHTS'),
            (SOURCE.replace('(version "0.1.0")', ''), 'MISSING_FIELD'),
            (SOURCE.replace('(version "0.1.0")', '(version "0.1.0") (version "x")'), 'DUPLICATE_FIELD'),
            (SOURCE.replace('"com.cubit.ccl-vm"', '15'), 'EXPECTED_TEXT'),
            (SOURCE.replace('"com.cubit.ccl-vm"', '"bad\\nidentity"'), 'INVALID_TEXT'),
            (SOURCE.replace('executable-manifest 1', 'executable-manifest 2'), 'UNSUPPORTED_VERSION'),
            (SOURCE.replace('"0.1.0"', '(/ 1 0)'), 'INVALID_EXPRESSION'),
            (SOURCE.replace('"0.1.0"', '(clock.monotonic-ms)'), 'INVALID_EXPRESSION'),
            (SOURCE.replace('(version', '(approved-authority'), 'UNKNOWN_DECLARATION'),
        ]
        for source, diagnostic in cases:
            with self.subTest(diagnostic=diagnostic, source=source):
                self.reject(source=source, diagnostic=diagnostic)

    def test_catalog_validation(self):
        for catalog, diagnostic in [
            ('(service-catalog 2)', 'UNSUPPORTED_VERSION'),
            ('(service-catalog 1 (service x 0 read))', 'INVALID_SERVICE_ID'),
            ('(service-catalog 1 (service x 4294967296 read))', 'INVALID_SERVICE_ID'),
            ('(service-catalog 1 (service x 1 read) (service x 2 read))', 'DUPLICATE_SERVICE'),
            ('(service-catalog 1 (service x 1 read) (service y 1 read))', 'DUPLICATE_SERVICE'),
        ]:
            with self.subTest(catalog=catalog):
                self.reject(catalog=catalog, diagnostic=diagnostic)

    def test_bounds(self):
        self.reject(source=' ' * 4097, diagnostic='4096-byte bound')
        self.reject(catalog=' ' * 4097, diagnostic='4096-byte bound')
        base = '(executable-manifest 1 (identity "a") (version "1") '
        self.reject(base + ''.join(f'(request-service clock read b{i})' for i in range(1, 34)) + ')',
                    diagnostic='TOO_MANY_REQUESTS')
        catalog = '(service-catalog 1 ' + ''.join(f'(service s{i} {i} read)' for i in range(1, 34)) + ')'
        self.reject(catalog=catalog, diagnostic='TOO_MANY_SERVICES')
        self.reject(SOURCE.replace('"0.1.0"', '(' * 33 + ')' * 33),
                    diagnostic='NESTING_TOO_DEEP')

    def test_deterministic_malformed_input_smoke(self):
        rng = random.Random(2026)
        for _ in range(100):
            text = ''.join(rng.choice('()#"\\ abc123\n') for _ in range(rng.randrange(160)))
            self.reject(source=text)


if __name__ == '__main__':
    unittest.main()
