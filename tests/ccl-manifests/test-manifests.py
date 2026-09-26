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
        headers = subprocess.check_output(['objdump', '-h', obj], text=True)
        names = [line.split()[1] for line in headers.splitlines()
                 if len(line.split()) > 2 and line.split()[1].startswith('.cubit.')]
        for name in names:
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

    def test_symbolic_format_versions(self):
        for token in ("1", "2", "v2", "V1", '"v1"', "(+ 0 1)"):
            with self.subTest(token=token):
                self.reject(source=SOURCE.replace("executable-manifest v1",
                                                   "executable-manifest " + token))
                self.reject(catalog=CATALOG.replace("service-catalog v1",
                                                     "service-catalog " + token))

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
        self.reject(catalog='(service-catalog v1 (application-slots 24 62))', diagnostic='UNKNOWN_SERVICE')

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

    def test_rust_bindings_share_the_validated_manifest(self):
        for base in (24, 30):
            with self.subTest(base=base):
                ada = self.compile(catalog=CATALOG.replace(
                    'application-slots 24 62', f'application-slots {base} 62'))
                output = self.directory / 'bindings.rs'
                result = subprocess.run([
                    TOOL, self.directory / 'catalog.ccl',
                    self.directory / 'manifest.ccl', '--rust-output', output],
                    capture_output=True, text=True, timeout=5)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, ada.stdout)
                self.assertIn(f'pub const SLOT_TEST_HOST : u64 = {base};', output.read_text())
                self.assertIn(f'pub const SLOT_CLOCK : u64 = {base + 1};', output.read_text())

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
            (SOURCE.replace('executable-manifest v1', 'executable-manifest v2'), 'UNSUPPORTED_VERSION'),
            (SOURCE.replace('"0.1.0"', '(/ 1 0)'), 'INVALID_EXPRESSION'),
            (SOURCE.replace('"0.1.0"', '(clock.monotonic-ms)'), 'INVALID_EXPRESSION'),
            (SOURCE.replace('(version', '(approved-authority'), 'UNKNOWN_DECLARATION'),
        ]
        for source, diagnostic in cases:
            with self.subTest(diagnostic=diagnostic, source=source):
                self.reject(source=source, diagnostic=diagnostic)

    def test_catalog_validation(self):
        for catalog, diagnostic in [
            ('(service-catalog v2)', 'UNSUPPORTED_VERSION'),
            ('(service-catalog v1 (service x 0 read))', 'INVALID_SERVICE_ID'),
            ('(service-catalog v1 (service x 4294967296 read))', 'INVALID_SERVICE_ID'),
            ('(service-catalog v1 (service x 1 read) (service x 2 read))', 'DUPLICATE_SERVICE'),
            ('(service-catalog v1 (service x 1 read) (service y 1 read))', 'DUPLICATE_SERVICE'),
        ]:
            with self.subTest(catalog=catalog):
                self.reject(catalog=catalog, diagnostic=diagnostic)

    def test_bounds(self):
        self.reject(source=' ' * 4097, diagnostic='4096-byte bound')
        self.reject(catalog=' ' * 4097, diagnostic='4096-byte bound')
        base = '(executable-manifest v1 (identity "a") (version "1") '
        self.reject(base + ''.join(f'(request-service clock read b{i})' for i in range(1, 34)) + ')',
                    diagnostic='TOO_MANY_REQUESTS')
        catalog = '(service-catalog v1 ' + ''.join(f'(service s{i} {i} read)' for i in range(1, 34)) + ')'
        self.reject(catalog=catalog, diagnostic='TOO_MANY_SERVICES')
        self.reject(SOURCE.replace('"0.1.0"', '(' * 33 + ')' * 33),
                    diagnostic='NESTING_TOO_DEEP')

    def test_deterministic_malformed_input_smoke(self):
        rng = random.Random(2026)
        for _ in range(100):
            text = ''.join(rng.choice('()#"\\ abc123\n') for _ in range(rng.randrange(160)))
            self.reject(source=text)

    def test_fixed_runtime_bindings(self):
        catalog = CATALOG[:-2] + ' (fixed-binding test-host 18) (fixed-binding clock 25))'
        result = self.compile(catalog=catalog)
        self.assertEqual(result.returncode, 0, result.stderr)
        caps = self.sections(result.stdout)['.cubit.caps']
        self.assertEqual(struct.unpack_from('<H', caps, 10)[0], 18)
        self.assertEqual(struct.unpack_from('<H', caps, 26)[0], 25)
        self.reject(catalog=catalog.replace('clock 25', 'clock 18'), diagnostic='DUPLICATE_SLOT')
        self.reject(catalog=catalog.replace('clock 25', 'clock 63'), diagnostic='INVALID_SLOT')
        self.reject(catalog=catalog.replace('fixed-binding clock', 'fixed-binding test-host'),
                    diagnostic='DUPLICATE_BINDING')
        # Even a later fixed binding reserves its slot before automatic allocation.
        result = self.compile(catalog=CATALOG[:-2] + ' (fixed-binding clock 24))')
        self.assertEqual(result.returncode, 0, result.stderr)
        caps = self.sections(result.stdout)['.cubit.caps']
        self.assertEqual(struct.unpack_from('<H', caps, 10)[0], 25)
        self.assertEqual(struct.unpack_from('<H', caps, 26)[0], 24)

    def test_identity_only_and_explicit_empty_requests(self):
        source = '(executable-manifest v1 (identity "a") (version "1"))'
        result = self.compile(source)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn('with Interfaces;',
                         (self.directory / 'ccl_manifest_bindings.ads').read_text())
        result = self.compile(source)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(set(self.sections(result.stdout)), {'.cubit.id'})
        result = self.compile(source[:-1] + ' (requests-none))')
        self.assertEqual(self.sections(result.stdout)['.cubit.caps'],
                         struct.pack('<IHH', 0x43424954, 1, 0))
        self.reject(SOURCE[:-2] + ' (requests-none))', diagnostic='DUPLICATE_FIELD')

    def test_scopes_exact_encoding(self):
        source = SOURCE[:-2] + ' (filesystem-scope (rights read write create) "@mem:0/work"))'
        result = self.compile(source)
        self.assertEqual(result.returncode, 0, result.stderr)
        data = self.sections(result.stdout)['.cubit.access']
        expected = struct.pack('<IHHQ', 0x43434143, 1, 1, 0)
        expected += struct.pack('<BB6x64s8x', 11, 11, b'@mem:0/work')
        self.assertEqual(data, expected)
        for path in ['', '../escape', '@mem:0/work/../other', 'a//b', '*', 'a\\nsecret', 'x' * 65]:
            with self.subTest(path=path):
                self.reject(source.replace('@mem:0/work', path), diagnostic='INVALID_PATH')
        for rights in ['', 'read read', 'read admin']:
            self.reject(source.replace('read write create', rights), diagnostic='INVALID_ACCESS_RIGHTS')
        self.reject(source[:-1] + ' (filesystem-scope (rights read) "@mem:0/work"))',
                    diagnostic='DUPLICATE_SCOPE')
        many = ''.join(f'(filesystem-scope (rights read) "p{i}")' for i in range(17))
        self.reject(SOURCE[:-2] + many + ')', diagnostic='TOO_MANY_SCOPES')

    def test_streams_exact_encoding(self):
        source = SOURCE[:-2] + ' (stream stdout text (* 2 2)) (stream log raw-bytes 1))'
        result = self.compile(source)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.sections(result.stdout)['.cubit.streams'],
                         struct.pack('<IHH', 0x54534243, 1, 2) +
                         struct.pack('<HHHH', 2, 4, 1, 0) + struct.pack('<HHHH', 4, 1, 0, 0))
        self.reject(source.replace('log raw-bytes', 'stdout raw-bytes'), diagnostic='DUPLICATE_STREAM')
        self.reject(source.replace('stdout text', 'unknown text'), diagnostic='UNKNOWN_STREAM')
        self.reject(source.replace('stdout text', 'stdout unknown'), diagnostic='UNKNOWN_STREAM')
        for pages in ['0', '-1', '257', 'true', '"4"']:
            self.reject(source.replace('(* 2 2)', pages), diagnostic='INVALID_STREAM_PAGES')


    def test_native_manifest_sources_are_ccl(self):
        for directory in ('userspace/apps', 'userspace/services',
                          'userspace/ccl/apps', 'userspace/ccl/services'):
            self.assertEqual(list((ROOT / directory).rglob('manifest*.c')), [],
                             f'{directory}: native executable manifests must be CCL')

    def test_notification_and_framebuffer_requests(self):
        catalog = '''(service-catalog v1 (application-slots 24 62)
          (notification keys 1 publish-and-manage)
          (service unrelated 1 read-write))'''
        source = '''(executable-manifest v1 (identity "test") (version "1")
          (request-notification keys publish events)
          (request-notification keys manage focus)
          (request-framebuffer read-write framebuffer))'''
        result = self.compile(source, catalog)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.sections(result.stdout)['.cubit.caps'],
                         struct.pack('<IHH', 0x43424954, 1, 3) +
                         struct.pack('<BBHIQ', 7, 1, 24, 1, 0) +
                         struct.pack('<BBHIQ', 7, 2, 25, 1, 0) +
                         struct.pack('<BBHIQ', 1, 3, 26, 0, 0))
        self.reject(source.replace('keys publish', 'unrelated publish'), catalog,
                    'UNKNOWN_NOTIFICATION')
        self.reject(source.replace('request-notification keys publish',
                                   'request-service keys read'), catalog, 'UNKNOWN_SERVICE')
        self.reject(source, catalog.replace('publish-and-manage', 'publish'), 'RIGHTS_NOT_OFFERED')
        self.reject(source.replace('keys publish', 'keys read'), catalog, 'UNKNOWN_RIGHTS')
        self.reject(source, catalog.replace('keys 1', 'keys 18'), 'INVALID_NOTIFICATION_ID')
        self.reject(source.replace('focus)', 'events)'), catalog, 'DUPLICATE_BINDING')
        self.reject(source.replace('framebuffer)', 'Bad_name)'), catalog, 'INVALID_BINDING_NAME')

    def test_explicit_broad_access_and_config_domain(self):
        source = '''(executable-manifest v1 (identity "test") (version "1")
          (filesystem-scope (rights read) all)
          (config-scope (rights read write) all))'''
        result = self.compile(source)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.sections(result.stdout)['.cubit.access'],
                         struct.pack('<IHHQ', 0x43434143, 1, 2, 0) +
                         bytes([1, 0, 0]) + bytes(77) + bytes([3, 0, 1]) + bytes(77))
        self.reject(source.replace('config-scope (rights read write)',
                                   'filesystem-scope (rights read write)'), diagnostic='DUPLICATE_SCOPE')
        self.reject(source.replace('all)', '"")'), diagnostic='INVALID_PATH')
        self.reject(source.replace('all)', 'all-folders)'), diagnostic='INVALID_PATH')
        for right in ('execute', 'create'):
            self.reject(source.replace('read write', right), diagnostic='INVALID_ACCESS_RIGHTS')

    def test_network_scope_encoding_and_validation(self):
        template = '''(executable-manifest v1 (identity "test") (version "1")
          (request-network tcp-connect (ipv4 "10.0.2.0" 24)
            (ports 80 443) (dns allow) network))'''
        result = self.compile(template)
        self.assertEqual(result.returncode, 0, result.stderr)
        descriptor = 80 | (443 << 16) | (24 << 32) | (1 << 40) | (1 << 48)
        self.assertEqual(self.sections(result.stdout)['.cubit.caps'],
                         struct.pack('<IHH', 0x43424954, 1, 1) +
                         struct.pack('<BBHIQ', 10, 3, 24, 0x0a000200, descriptor))
        listener = template.replace('tcp-connect', 'tcp-listen').replace(
            '10.0.2.0" 24', '10.0.2.15" 32').replace('80 443', '8080 8080').replace('dns allow', 'dns deny')
        result = self.compile(listener)
        self.assertEqual(result.returncode, 0, result.stderr)
        descriptor = 8080 | (8080 << 16) | (32 << 32) | (2 << 40)
        self.assertEqual(self.sections(result.stdout)['.cubit.caps'][8:],
                         struct.pack('<BBHIQ', 10, 3, 24, 0x0a00020f, descriptor))
        broad = template.replace('10.0.2.0" 24', '0.0.0.0" 0').replace('80 443', '1 65535')
        self.assertEqual(self.compile(broad).returncode, 0)
        datagram = template.replace('tcp-connect', 'udp-connect').replace(
            '10.0.2.0" 24', '10.0.2.2" 32').replace('80 443', '123 123')
        result = self.compile(datagram)
        self.assertEqual(result.returncode, 0, result.stderr)
        descriptor = 123 | (123 << 16) | (32 << 32) | (3 << 40) | (1 << 48)
        self.assertEqual(self.sections(result.stdout)['.cubit.caps'][8:],
                         struct.pack('<BBHIQ', 10, 3, 24, 0x0a000202, descriptor))
        for old, bad in [
                ('tcp-connect', 'udp-listen'), ('tcp-connect', 'udp'),
                ('"10.0.2.0"', '123'),
                ('"10.0.2.0"', '"10.0.2.1"'), ('"10.0.2.0"', '"010.0.2.0"'),
                ('"10.0.2.0"', '"256.0.2.0"'), ('"10.0.2.0"', '"10.0.2"'),
                ('"10.0.2.0"', '"10..2.0"'), ('"10.0.2.0"', '"10.0.2.0."'),
                ('"10.0.2.0"', '"10.0.2.0/24"'), ('"10.0.2.0"', '"1.2.3.4.5"'),
                ('"10.0.2.0"', '"10.0.2.-1"'), ('"10.0.2.0"', '"10.0.2.0 "'),
                ('24)', '33)'), ('24)', '-1)'), ('24)', 'true)'),
                ('80 443', '0 443'), ('80 443', '444 443'), ('80 443', '80 65536'),
                ('80 443', '"80" 443'), ('dns allow', 'dns maybe')]:
            with self.subTest(old=old, bad=bad):
                self.reject(template.replace(old, bad), diagnostic='INVALID_NETWORK_SCOPE')
        for old, bad in [('dns deny', 'dns allow'), ('8080 8080', '8080 8081'),
                         ('32)', '24)'), ('10.0.2.15', '0.0.0.0'),
                         ('10.0.2.15', '224.0.0.1')]:
            self.reject(listener.replace(old, bad), diagnostic='INVALID_NETWORK_SCOPE')
        self.reject(template.replace('network)', 'Network)'), diagnostic='INVALID_BINDING_NAME')

    def test_tls_scope_encoding_and_validation(self):
        template = '''(executable-manifest v1 (identity "test") (version "1")
          (tls-scope "tls-test.cubit.internal:18460-18463"))'''
        result = self.compile(template)
        self.assertEqual(result.returncode, 0, result.stderr)
        pattern = b"tls-test.cubit.internal:18460-18463"
        entry = bytes([1, len(pattern), 2]) + bytes(5) + pattern + bytes(64 - len(pattern)) + bytes(8)
        self.assertEqual(self.sections(result.stdout)['.cubit.access'][16:], entry)
        for good in ('*:443', '*.example.com:443', 'a.example:1-65535'):
            with self.subTest(good=good):
                self.assertEqual(self.compile(template.replace(
                    'tls-test.cubit.internal:18460-18463', good)).returncode, 0)
        for bad in ('tls-test.cubit.internal', '*.com:443', 'Upper.example:443',
                    '10.0.2.2:443', 'a.example:0', 'a.example:500-400', 'a_b.example:443'):
            with self.subTest(bad=bad):
                self.reject(template.replace('tls-test.cubit.internal:18460-18463', bad),
                            diagnostic='INVALID_PATH')
        self.reject(template.replace('))', ')\n          (tls-scope "tls-test.cubit.internal:18460-18463"))'),
                    diagnostic='DUPLICATE_SCOPE')
        # Truncation must fail without exceptions or partial ELF output.
        for end in range(len(template) - 1):
            self.reject(template[:end])


if __name__ == '__main__':
    unittest.main()
