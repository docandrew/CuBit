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
DECODER = ROOT / 'tests/ccl-manifests/build/resources/resource_decode'
SOURCE = (ROOT / 'userspace/ccl/apps/ccl-vm/manifest.ccl').read_text()
SCHEMA = ROOT / 'userspace/ccl/interfaces/executable-manifest.ccl'


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
                                 self.directory / 'ccl_manifest_bindings.ads', '--schema', SCHEMA],
                                capture_output=True,
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

    TLS_TYPED = """(Executable_Manifest
  identity => "com.cubit.tls"
  version => "0.1.0"
  requests => [
    (service "config" "config")
    (service "clock" "clock")
    (network Network_Action.TCP_Connect "0.0.0.0" 0 1 65535 true 8 "tcp")]
  scopes => [
    (filesystem [File_Right.Read] "@nvme:0/tls/")
    (config [File_Right.Read] "tls.")])
"""
    TLS_KEYWORDS = """(executable-manifest v1
  (identity "com.cubit.tls")
  (version "0.1.0")
  (request-service config read-write config)
  (request-service clock read-write clock)
  (request-network tcp-connect (ipv4 "0.0.0.0" 0) (ports 1 65535) (dns allow) (connections 8) tcp)
  (filesystem-scope (rights read) "@nvme:0/tls/")
  (config-scope (rights read) "tls."))
"""

    def test_typed_manifest_matches_keywords(self):
        catalog = (ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl').read_text()
        typed = self.compile(self.TLS_TYPED, catalog)
        self.assertEqual(typed.returncode, 0, typed.stderr)
        typed_bindings = (self.directory / 'ccl_manifest_bindings.ads').read_text()
        keywords = self.compile(self.TLS_KEYWORDS, catalog)
        self.assertEqual(keywords.returncode, 0, keywords.stderr)
        self.assertEqual(self.sections(typed.stdout), self.sections(keywords.stdout))
        self.assertEqual(typed_bindings, (self.directory / 'ccl_manifest_bindings.ads').read_text())

    def test_typed_manifest_named_fields_and_defaults(self):
        catalog = (ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl').read_text()
        named = self.TLS_TYPED.replace(
            '(service "clock" "clock")',
            '(Request.Service (Service_Request binding => "clock" service => "clock"))')
        self.assertEqual(self.sections(self.compile(named, catalog).stdout),
                         self.sections(self.compile(self.TLS_TYPED, catalog).stdout))

    def test_typed_manifest_explains_failures(self):
        catalog = (ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl').read_text()
        for change, message in [
                (('"clock" "clock"', '"clok" "clock"'),
                 'requests entry 2: the catalog has no service named "clok"; did you mean "clock"? Fix: write "clock".'),
                (('[File_Right.Read] "@nvme', '[File_Right.Reed] "@nvme'),
                 '"File_Right.Reed": no such name. Fix: use one of: File_Right.Read, File_Right.Write, '
                 'File_Right.Execute, File_Right.Create.'),
                (('"tcp")', '"Tcp")'), 'requests entry 3: binding "Tcp" is not a lowercase kebab name.'),
                (('"@nvme:0/tls/"', '"@nvme:0/../tls"'), 'scopes entry 1: INVALID_PATH'),
                (('identity => "com.cubit.tls"\n', ''), 'line 1, column 2: field identity: '
                 'This field has no default, so the construction must give it'),
                (('version =>', 'verison =>'), 'field verison: This record has no field by that name')]:
            with self.subTest(change=change):
                result = self.compile(self.TLS_TYPED.replace(*change), catalog)
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, '')
                self.assertIn(message, result.stderr)

    def test_typed_may_launch_matches_launch_authority_layout(self):
        catalog = (ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl').read_text()
        names = ['logstore.svc', 'spawn-child.app']
        source = self.TLS_TYPED.replace(
            '  scopes =>', '  may_launch => [%s]\n  scopes =>' % ' '.join('(Launch "%s")' % n for n in names))
        result = self.compile(source, catalog)
        self.assertEqual(result.returncode, 0, result.stderr)
        fixture = subprocess.run(['python3', ROOT / 'tests/process-spawn/launch-table.py', *names],
                                 capture_output=True, text=True, check=True).stdout
        expected = b'LNCH' + struct.pack('<HH', 1, len(names)) + b''.join(
            bytes([len(n)]) + n.encode() for n in names)
        self.assertEqual(self.sections(result.stdout)['.cubit.launch'], expected)
        self.assertEqual(self.sections(fixture)['.cubit.launch'], expected)
        for change, message in [
                ('(Launch "logstore.svc")', 'may_launch entry 2: program "logstore.svc" is listed twice'),
                ('(Launch "a b")', 'may_launch entry 2: program "a b" is not 1 to 64 printable'),
                ('(Launch "as" ["as"])', 'may_launch entry 2: invoked_as aliases need the next launch table')]:
            with self.subTest(change=change):
                bad = source.replace('(Launch "spawn-child.app")', change)
                failed = self.compile(bad, catalog)
                self.assertEqual(failed.returncode, 1)
                self.assertIn(message, failed.stderr)

    def test_typed_parameters_match_program_parameters_layout(self):
        # The binutils ld manifest: an independent encoding of the
        # .cubit.description descriptor (CuBit.Program_Descriptions).
        catalog = (ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl').read_text()
        source = (ROOT / 'userspace/ports/binutils/ld.ccl').read_text()
        result = self.compile(source, catalog)
        self.assertEqual(result.returncode, 0, result.stderr)
        INPUT_FILE, OUTPUT_FILE, TEXT, FLAG = 1, 2, 6, 5
        MANY, OPTIONAL = 1, 2
        LITERAL, VALUE, WHEN_SET = 1, 2, 3
        parameters = [(OUTPUT_FILE, 0, 'output'), (INPUT_FILE, MANY, 'inputs'),
                      (INPUT_FILE, OPTIONAL, 'script'), (TEXT, OPTIONAL, 'entry'),
                      (FLAG, OPTIONAL, 'static')]
        pieces = [(WHEN_SET, 4, '-static'), (WHEN_SET, 3, '-e'), (VALUE, 3, ''),
                  (WHEN_SET, 2, '-T'), (VALUE, 2, ''), (LITERAL, 0, '-o'), (VALUE, 0, ''),
                  (VALUE, 1, '')]
        OUTLET, TEXT, STREAM = 2, 1, 1
        outlets = [(OUTLET, TEXT, STREAM, 4, 'unix.stdout'), (OUTLET, TEXT, STREAM, 4, 'unix.stderr')]
        expected = b'PDSC' + struct.pack('<HBBBBH', 1, len(parameters), len(pieces), len(outlets), 2, 0)
        expected += b''.join(bytes([k, f, len(n)]) + n.encode() for k, f, n in parameters)
        expected += b''.join(bytes([k, p, len(t)]) + t.encode() for k, p, t in pieces)
        expected += b''.join(bytes([d, e, m, g, len(n)]) + n.encode() for d, e, m, g, n in outlets)
        expected += bytes([1, 0, 2, 1])
        self.assertEqual(self.sections(result.stdout)['.cubit.description'], expected)
        for old, new, message in [
                ('(Argument_Piece.Value "inputs")', '(Argument_Piece.Value "input")',
                 'no parameter "input"'),
                ('(Argument_Piece.Value "inputs")', '(Argument_Piece.Value "static")',
                 'flag "static" has no value'),
                ('(Argument_Piece.Value "inputs")', '(Argument_Piece.Literal "-v")',
                 'parameter "inputs" is never rendered'),
                ('"entry" Parameter_Kind.Text', '"output" Parameter_Kind.Text',
                 'parameter "output" is declared twice'),
                ('"entry" Parameter_Kind.Text', '"Entry" Parameter_Kind.Text',
                 'name "Entry" is not 1 to 32'),
                ('(Parameter "static" Parameter_Kind.Flag)',
                 '(Parameter "static" Parameter_Kind.Flag many => true)',
                 'flag "static" cannot take many values'),
                ('(Argument_Piece.Literal "-o")', '(Argument_Piece.Literal "%s")' % ('x' * 49),
                 'literal is not 1 to 48 printable')]:
            with self.subTest(new=new):
                failed = self.compile(source.replace(old, new, 1), catalog)
                self.assertEqual(failed.returncode, 1)
                self.assertIn(message, failed.stderr)

    def test_existing_elf_bytes_unchanged(self):
        legacy = {'.' + path.name[:-len('.bin')]: path.read_bytes() for path in
                  sorted((ROOT / 'tests/ccl-manifests/fixtures/ccl-vm-legacy').glob('*.bin'))}
        result = self.compile()
        self.assertEqual(result.returncode, 0, result.stderr)
        sections = self.sections(result.stdout)
        self.assertEqual(sections, legacy)
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

    def test_keyword_stream_form_is_gone(self):
        # Outlets replace (stream stdout ...): CuBit has no stdout.
        self.reject(SOURCE[:-2] + ' (stream stdout text 4))')

    CONNECTORS_TYPED = TLS_TYPED.replace('  scopes =>', """  outlets => [
    (Outlet "unix.stderr" Element.Text pages => 4)
    (Outlet "com.example.tool.progress" Element.Integers signal => Signal.Level)
    (Outlet "com.cubit.stdlog" Element.Log_Records)]
  inlets => [(Inlet "unix.stdin" Element.Text)]
  descriptors => [(Descriptor 2 "unix.stderr") (Descriptor 0 "unix.stdin")]
  scopes =>""")

    def test_inlets_and_outlets_exact_encoding(self):
        catalog = (ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl').read_text()
        result = self.compile(self.CONNECTORS_TYPED, catalog)
        self.assertEqual(result.returncode, 0, result.stderr)
        INLET, OUTLET = 1, 2
        TEXT, BYTES, INTEGERS, LOGS = 1, 2, 3, 4
        STREAM, ONE_SHOT, LEVEL, EDGE = 1, 2, 3, 4
        # Outlets first, then inlets.
        connectors = [(OUTLET, TEXT, STREAM, 4, 'unix.stderr'),
                      (OUTLET, INTEGERS, LEVEL, 1, 'com.example.tool.progress'),
                      (OUTLET, LOGS, STREAM, 1, 'com.cubit.stdlog'),
                      (INLET, TEXT, STREAM, 1, 'unix.stdin')]
        expected = b'PDSC' + struct.pack('<HBBBBH', 1, 0, 0, len(connectors), 2, 0)
        expected += b''.join(bytes([d, e, m, p, len(n)]) + n.encode() for d, e, m, p, n in connectors)
        expected += bytes([2, 0, 0, 3])
        sections = self.sections(result.stdout)
        self.assertEqual(sections['.cubit.description'], expected)
        self.assertNotIn('.cubit.streams', sections)
        for old, new, message in [
                ('(Outlet "unix.stderr"', '(Outlet "stderr"', 'outlet "stderr" is not a qualified name'),
                ('(Outlet "unix.stderr"', '(Outlet "unix.tty"', 'unix.* names only stdin, stdout and stderr'),
                ('(Inlet "unix.stdin"', '(Inlet "unix.stdout"', 'unix.stdout is an outlet'),
                ('(Outlet "unix.stderr"', '(Outlet "unix.stdin"', 'unix.stdin is an inlet'),
                ('(Outlet "com.cubit.stdlog" Element.Log_Records)', '(Outlet "com.cubit.stdlog" Element.Text)',
                 'com.cubit.stdlog is a CuBit contract'),
                ('"com.cubit.stdlog"', '"com.cubit.exit"', 'com.cubit.exit is supplied by the system'),
                ('"com.cubit.stdlog"', '"unix.stderr"', '"unix.stderr" is declared twice'),
                ('pages => 4', 'pages => 256', 'not 1 to 255'),
                ('(Descriptor 2 "unix.stderr")', '(Descriptor 2 "unix.stdout")', 'no outlet or inlet "unix.stdout"'),
                ('(Descriptor 2 "unix.stderr")', '(Descriptor 2 "unix.stdin")',
                 'descriptor 2 writes, so it must name an outlet'),
                ('(Descriptor 0 "unix.stdin")', '(Descriptor 2 "unix.stderr")',
                 'descriptor 2 is mapped twice')]:
            with self.subTest(new=new):
                failed = self.compile(self.CONNECTORS_TYPED.replace(old, new, 1), catalog)
                self.assertEqual(failed.returncode, 1)
                self.assertIn(message, failed.stderr)

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

    def test_render_request_is_distinct_from_scanout_service(self):
        catalog = '''(service-catalog v1 (application-slots 24 62)
          (service gpu 17 read-write))'''
        source = '''(executable-manifest v1 (identity "test") (version "1")
          (request-service gpu read-write scanout)
          (request-render read-write render))'''
        result = self.compile(source, catalog)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.sections(result.stdout)['.cubit.caps'],
                         struct.pack('<IHH', 0x43424954, 1, 2) +
                         struct.pack('<BBHIQ', 2, 3, 24, 17, 0) +
                         struct.pack('<BBHIQ', 11, 3, 25, 0, 0))
        for rights in ('read', 'write', 'publish'):
            self.reject(source.replace('request-render read-write',
                                       'request-render ' + rights),
                        catalog, 'UNKNOWN_RIGHTS')
        self.reject(source.replace('render))', 'scanout))'), catalog,
                    'DUPLICATE_BINDING')
        self.reject(source.replace('(request-service gpu read-write scanout)',
                                   '(request-render read-write second)'),
                    catalog, 'DUPLICATE_FIELD')

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
            (ports 80 443) (dns allow) (connections 6) network))'''
        result = self.compile(template)
        self.assertEqual(result.returncode, 0, result.stderr)
        descriptor = 80 | (443 << 16) | (24 << 32) | (1 << 40) | (1 << 48) | (6 << 49)
        self.assertEqual(self.sections(result.stdout)['.cubit.caps'],
                         struct.pack('<IHH', 0x43424954, 1, 1) +
                         struct.pack('<BBHIQ', 10, 3, 24, 0x0a000200, descriptor))
        listener = template.replace('tcp-connect', 'tcp-listen').replace(
            '10.0.2.0" 24', '10.0.2.15" 32').replace('80 443', '8080 8080').replace('dns allow', 'dns deny')
        result = self.compile(listener)
        self.assertEqual(result.returncode, 0, result.stderr)
        descriptor = 8080 | (8080 << 16) | (32 << 32) | (2 << 40) | (6 << 49)
        self.assertEqual(self.sections(result.stdout)['.cubit.caps'][8:],
                         struct.pack('<BBHIQ', 10, 3, 24, 0x0a00020f, descriptor))
        broad = template.replace('10.0.2.0" 24', '0.0.0.0" 0').replace('80 443', '1 65535')
        self.assertEqual(self.compile(broad).returncode, 0)
        most = template.replace('connections 6', 'connections 32767')
        self.assertEqual(self.compile(most).returncode, 0)
        datagram = template.replace('tcp-connect', 'udp-connect').replace(
            '10.0.2.0" 24', '10.0.2.2" 32').replace('80 443', '123 123')
        result = self.compile(datagram)
        self.assertEqual(result.returncode, 0, result.stderr)
        descriptor = 123 | (123 << 16) | (32 << 32) | (3 << 40) | (1 << 48) | (6 << 49)
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
                ('80 443', '"80" 443'), ('dns allow', 'dns maybe'),
                ('connections 6', 'connections 0'),
                ('connections 6', 'connections 32768'),
                ('connections 6', 'connections -1'),
                ('connections 6', 'connections "6"'),
                ('connections 6', 'channels 6')]:
            with self.subTest(old=old, bad=bad):
                self.reject(template.replace(old, bad), diagnostic='INVALID_NETWORK_SCOPE')
        for old, bad in [('dns deny', 'dns allow'), ('8080 8080', '8080 8081'),
                         ('32)', '24)'), ('10.0.2.15', '0.0.0.0'),
                         ('10.0.2.15', '224.0.0.1')]:
            self.reject(listener.replace(old, bad), diagnostic='INVALID_NETWORK_SCOPE')
        self.reject(template.replace('network)', 'Network)'), diagnostic='INVALID_BINDING_NAME')
        # Every network request declares its channel count up front.
        self.reject(template.replace('(connections 6) ', ''), diagnostic='EXPECTED_FORM')

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

    def test_device_resources_encoding(self):
        # An NVMe-style PCI driver: resources relative to the matched device.
        template = '''(executable-manifest v1 (identity "nvme") (version "1")
          (match-pci-class 1 8 2)
          (device-memory registers (bar 0) (max-bytes 16384) read-write)
          (interrupt completion msix (vectors 1))
          (dma queues (bytes 1048576)))'''
        result = self.compile(template)
        self.assertEqual(result.returncode, 0, result.stderr)
        sections = self.sections(result.stdout)
        # Device resources never appear as endpoint capability requests.
        self.assertNotIn('.cubit.caps', sections)
        header = struct.pack('<IHH', 0x53524243, 1, 3) + struct.pack('<BBHHHQ', 1, 0, 1, 8, 2, 0)
        entries = (struct.pack('<BBHIQQ', 16, 3, 24, 0, 16384, 0) +
                   struct.pack('<BBHIQQ', 18, 1, 25, 0, 1, 1) +
                   struct.pack('<BBHIQQ', 19, 3, 26, 0, 1048576, 0))
        self.assertEqual(sections['.cubit.resources'], header + entries)
        bindings = (self.directory / 'ccl_manifest_bindings.ads').read_text()
        for name, slot in (('registers', 24), ('completion', 25), ('queues', 26)):
            self.assertIn(f'Slot_{name} : constant Interfaces.Unsigned_64 := {slot};', bindings)
        # Truncation must fail without exceptions or partial ELF output.
        for end in range(len(template) - 1):
            self.reject(template[:end])

    def test_platform_device_and_ports(self):
        template = '''(executable-manifest v1 (identity "ps2") (version "1")
          (platform-device ps2-controller)
          (io-ports data (resource 0) (count 1))
          (io-ports command (resource 1) (count 1))
          (interrupt keyboard (resource 2))
          (interrupt mouse (resource 3)))'''
        result = self.compile(template)
        self.assertEqual(result.returncode, 0, result.stderr)
        resources = self.sections(result.stdout)['.cubit.resources']
        self.assertEqual(resources[:24], struct.pack('<IHH', 0x53524243, 1, 4) +
                         struct.pack('<BBHHHQ', 3, 0, 1, 0, 0, 0))
        self.assertEqual(resources[24:], struct.pack('<BBHIQQ', 17, 3, 24, 0, 1, 0) +
                         struct.pack('<BBHIQQ', 17, 3, 25, 1, 1, 0) +
                         struct.pack('<BBHIQQ', 18, 1, 26, 2, 1, 4) +
                         struct.pack('<BBHIQQ', 18, 1, 27, 3, 1, 4))
        # PCI spelling on a platform device, and the reverse.
        self.reject(template.replace('(resource 0)', '(bar 0)'),
                    diagnostic='INVALID_DEVICE_RESOURCE')
        pci = template.replace('(platform-device ps2-controller)', '(match-pci-id 4358 4096)')
        self.reject(pci, diagnostic='INVALID_DEVICE_RESOURCE')

    def test_device_resource_rejections(self):
        base = '''(executable-manifest v1 (identity "d") (version "1")
          (match-pci-id 6900 4096)
          (device-memory regs (bar 0) (max-bytes 4096) read-write))'''
        self.assertEqual(self.compile(base).returncode, 0)
        for old, bad, diagnostic in [
                ('(match-pci-id 6900 4096)', '', 'MISSING_DEVICE_MATCH'),
                ('(match-pci-id 6900 4096)', '(match-pci-id 6900 4096) (match-pci-class 1 8 2)',
                 'DUPLICATE_DEVICE_MATCH'),
                ('6900 4096', '65535 4096', 'INVALID_DEVICE_MATCH'),
                ('6900 4096', '65536 4096', 'INVALID_DEVICE_MATCH'),
                ('6900 4096', '-1 4096', 'INVALID_DEVICE_MATCH'),
                ('(match-pci-id 6900 4096)', '(match-pci-class 256 0 0)', 'INVALID_DEVICE_MATCH'),
                ('(match-pci-id 6900 4096)', '(platform-device serial)', 'INVALID_DEVICE_MATCH'),
                ('(bar 0)', '(bar 6)', 'INVALID_DEVICE_RESOURCE'),
                ('(bar 0)', '(bar -1)', 'INVALID_DEVICE_RESOURCE'),
                ('(max-bytes 4096)', '(max-bytes 4095)', 'INVALID_DEVICE_RESOURCE'),
                ('(max-bytes 4096)', '(max-bytes 6000)', 'INVALID_DEVICE_RESOURCE'),
                ('(max-bytes 4096)', '(max-bytes 268439552)', 'INVALID_DEVICE_RESOURCE'),
                ('read-write)', 'write)', 'INVALID_DEVICE_RESOURCE'),
                ('read-write)', 'everything)', 'UNKNOWN_RIGHTS'),
                ('regs', 'Regs', 'INVALID_BINDING_NAME')]:
            with self.subTest(bad=bad):
                self.reject(base.replace(old, bad), diagnostic=diagnostic)
        # A match that asks for no resources grants nothing and is a mistake.
        self.reject(base.replace('(device-memory regs (bar 0) (max-bytes 4096) read-write)', ''),
                    diagnostic='MISSING_DEVICE_MATCH')
        extra = base.replace('read-write))', 'read-write) {})')
        for form, diagnostic in [
                ('(interrupt irq msix (vectors 0))', 'INVALID_DEVICE_RESOURCE'),
                ('(interrupt irq msix (vectors 33))', 'INVALID_DEVICE_RESOURCE'),
                ('(interrupt irq edge)', 'INVALID_DEVICE_RESOURCE'),
                ('(dma buf (bytes 0))', 'INVALID_DEVICE_RESOURCE'),
                ('(dma buf (bytes 67112960))', 'INVALID_DEVICE_RESOURCE'),
                ('(io-ports p (bar 0) (count 65537))', 'INVALID_DEVICE_RESOURCE'),
                ('(dma regs (bytes 4096))', 'DUPLICATE_BINDING')]:
            with self.subTest(form=form):
                self.reject(extra.format(form), diagnostic=diagnostic)
        for form in ('(interrupt irq line)', '(interrupt irq msi (vectors 32))',
                     '(io-ports p (bar 5) (count 65536))', '(dma buf (bytes 67108864))'):
            with self.subTest(form=form):
                self.assertEqual(self.compile(extra.format(form)).returncode, 0)

    def test_scheduling_request(self):
        template = '''(executable-manifest v1 (identity "mixer") (version "1")
          (request-scheduling realtime-cpu realtime (budget-us 1500) (period-us 5000)))'''
        result = self.compile(template)
        self.assertEqual(result.returncode, 0, result.stderr)
        sections = self.sections(result.stdout)
        self.assertNotIn('.cubit.caps', sections)
        self.assertEqual(sections['.cubit.resources'],
                         struct.pack('<IHH', 0x53524243, 1, 1) + bytes(16) +
                         struct.pack('<BBHIQQ', 20, 1, 24, 0, 1500, 5000))
        # The kernel admits at most 70% of a CPU and budget <= period <= 2^30 us.
        for budget, period in [('5000', '7143'), ('1', '1073741824'), ('700', '1000')]:
            with self.subTest(budget=budget, period=period):
                self.assertEqual(self.compile(template.replace(
                    '(budget-us 1500) (period-us 5000)',
                    f'(budget-us {budget}) (period-us {period})')).returncode, 0)
        for budget, period in [('0', '5000'), ('1500', '0'), ('5000', '4999'),
                               ('3501', '5000'), ('1', '1073741825'), ('"1500"', '5000')]:
            with self.subTest(budget=budget, period=period):
                self.reject(template.replace('(budget-us 1500) (period-us 5000)',
                                             f'(budget-us {budget}) (period-us {period})'),
                            diagnostic='INVALID_SCHEDULING')
        self.reject(template.replace(' realtime (', ' batch ('), diagnostic='INVALID_SCHEDULING')
        # Existing capability requests are unaffected by a resource request.
        both = template.replace('(request-scheduling', '(request-service filesystem read-write fs)\n  (request-scheduling')
        sections = self.sections(self.compile(both).stdout)
        self.assertEqual(sections['.cubit.caps'],
                         struct.pack('<IHH', 0x43424954, 1, 1) + struct.pack('<BBHIQ', 2, 3, 24, 6, 0))
        self.assertEqual(sections['.cubit.resources'][24:],
                         struct.pack('<BBHIQQ', 20, 1, 25, 0, 1500, 5000))
        self.assertEqual(self.compile(template.replace(
            '(request-scheduling', '(requests-none)\n  (request-scheduling')).returncode, 0)

    def test_driver_draft_manifests(self):
        # Draft driver manifests (not yet attached) against the slots devmgr
        # hand-mints today. Differences are listed so a migration handles them
        # deliberately: ps2 mouse notification (devmgr 9, now reserved for
        # scheduling), hda and mixer registration (devmgr 8).
        catalog = (ROOT / 'userspace/ccl/catalogs/driver-services.ccl').read_text()
        expected = {
            'nvme': {'registers': 4, 'completion': 5, 'queues': 6, 'registration': 7, 'ready': 15},
            'ata': {'command-block': 4, 'control': 5, 'channel': 6, 'registration': 7, 'ready': 15},
            'virtio-net': {'registers': 4, 'device': 5, 'rings': 6, 'ready': 15},
            'xhci': {'registers': 4, 'events': 5, 'rings': 6, 'mouse-events': 7,
                     'keyboard-events': 8, 'ready': 15},
            'ps2': {'data': 4, 'command': 5, 'keyboard-line': 6, 'mouse-line': 7,
                    'keyboard-events': 8, 'mouse-events': 10, 'ready': 15},
            'hda': {'registers': 4, 'controller': 5, 'streams': 6, 'registration': 7, 'ready': 15},
            'mixer': {'scheduling': 9, 'registration': 4, 'ready': 15},
        }
        for name, slots in expected.items():
            with self.subTest(driver=name):
                source = (ROOT / f'userspace/services/{name}/manifest.ccl').read_text()
                result = self.compile(source, catalog)
                self.assertEqual(result.returncode, 0, result.stderr)
                bindings = (self.directory / 'ccl_manifest_bindings.ads').read_text()
                for binding, slot in slots.items():
                    ada = binding.replace('-', '_')
                    self.assertIn(f'Slot_{ada} : constant Interfaces.Unsigned_64 := {slot};',
                                  bindings)
                self.assertIn('.cubit.resources', self.sections(result.stdout))

    def decode(self, section):
        path = self.directory / 'resources.bin'
        path.write_bytes(section)
        result = subprocess.run([DECODER, path], capture_output=True, text=True, timeout=5)
        self.assertNotIn('raised ', result.stderr, result.stderr)
        self.assertIn(result.returncode, (0, 1), result.stderr)
        return result

    def test_resource_sections_round_trip_through_decoder(self):
        # Every draft decodes with the proved startup decoder, and any single
        # corrupted byte is either refused or still a valid plan: never a crash.
        catalog = (ROOT / 'userspace/ccl/catalogs/driver-services.ccl').read_text()
        rng = random.Random(4242)
        for name in ('nvme', 'ata', 'ps2', 'virtio-net', 'hda', 'xhci', 'mixer'):
            with self.subTest(driver=name):
                source = (ROOT / f'userspace/services/{name}/manifest.ccl').read_text()
                result = self.compile(source, catalog)
                self.assertEqual(result.returncode, 0, result.stderr)
                section = self.sections(result.stdout)['.cubit.resources']
                decoded = self.decode(section)
                self.assertEqual(decoded.returncode, 0, decoded.stdout)
                for _ in range(200):
                    corrupt = bytearray(section)
                    corrupt[rng.randrange(len(corrupt))] ^= 1 << rng.randrange(8)
                    self.decode(bytes(corrupt))
                for end in range(len(section)):
                    self.assertEqual(self.decode(section[:end]).returncode, 1)
                self.assertEqual(self.decode(section + b'\0').returncode, 1)
        nvme = self.sections(self.compile((ROOT / 'userspace/services/nvme/manifest.ccl').read_text(),
                                          catalog).stdout)['.cubit.resources']
        self.assertEqual(self.decode(nvme).stdout.splitlines(), [
            'match PCI_CLASS_MATCH 1 8 2',
            'DEVICE_MEMORY READ_WRITE slot=4 index=0 amount=16384 extra=0',
            'INTERRUPT READ_ONLY slot=5 index=0 amount=1 extra=1',
            'DMA READ_WRITE slot=6 index=0 amount=1048576 extra=0'])
        # Hand-made sections the compiler never emits are refused.
        header = struct.pack('<IHH', 0x53524243, 1, 1)
        pci = struct.pack('<BBHHHQ', 1, 0, 1, 8, 2, 0)
        memory = lambda **f: struct.pack('<BBHIQQ', f.get('kind', 16), f.get('rights', 3),
                                         f.get('slot', 4), f.get('index', 0),
                                         f.get('amount', 4096), f.get('extra', 0))
        self.assertEqual(self.decode(header + pci + memory()).returncode, 0)
        for bad, status in [
                (header + pci + memory(slot=62), 'INVALID_ENTRY'),
                (header + pci + memory(slot=0), 'INVALID_ENTRY'),
                (header + pci + memory(amount=4097), 'INVALID_ENTRY'),
                (header + pci + memory(rights=2), 'INVALID_ENTRY'),
                (header + pci + memory(index=6), 'INVALID_ENTRY'),
                (header + pci + memory(extra=1), 'INVALID_ENTRY'),
                (header + pci + memory(kind=21), 'INVALID_ENTRY'),
                (header + struct.pack('<BBHHHQ', 0, 0, 0, 0, 0, 0) + memory(), 'DEVICE_WITHOUT_MATCH'),
                (header + struct.pack('<BBHHHQ', 1, 1, 1, 8, 2, 0) + memory(), 'INVALID_MATCH'),
                (header + struct.pack('<BBHHHQ', 1, 0, 1, 8, 2, 1) + memory(), 'INVALID_MATCH'),
                (header + struct.pack('<BBHHHQ', 2, 0, 0xFFFF, 1, 0, 0) + memory(), 'INVALID_MATCH'),
                (header + pci + struct.pack('<BBHIQQ', 20, 1, 4, 0, 1500, 5000),
                 'MATCH_WITHOUT_DEVICE'),
                (struct.pack('<IHH', 0x53524243, 1, 2) + pci + memory() + memory(),
                 'DUPLICATE_SLOT'),
                (struct.pack('<IHH', 0x53524243, 2, 1) + pci + memory(), 'INVALID_HEADER'),
                (struct.pack('<IHH', 0x53524243, 1, 0) + pci, 'INVALID_HEADER')]:
            with self.subTest(status=status, bad=bad.hex()):
                result = self.decode(bad)
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout.strip(), status)
        # Scheduling beyond the kernel's real-time share is refused.
        scheduling = struct.pack('<IHH', 0x53524243, 1, 1) + bytes(16)
        self.assertEqual(self.decode(scheduling + struct.pack('<BBHIQQ', 20, 1, 4, 0, 3500, 5000)).returncode, 0)
        self.assertEqual(self.decode(scheduling + struct.pack('<BBHIQQ', 20, 1, 4, 0, 3501, 5000)).stdout.strip(),
                         'INVALID_ENTRY')

    def test_resources_never_take_saved_reply_slot(self):
        catalog = '(service-catalog v1 (application-slots 61 62))'
        two = '''(executable-manifest v1 (identity "m") (version "1")
          (request-scheduling a realtime (budget-us 1) (period-us 2))
          (request-scheduling b realtime (budget-us 1) (period-us 2)))'''
        self.reject(two, catalog, diagnostic='INVALID_SLOT')
        one = two.replace('\n          (request-scheduling b realtime (budget-us 1) (period-us 2))', '')
        self.assertEqual(self.compile(one, catalog).returncode, 0)


if __name__ == '__main__':
    unittest.main()
