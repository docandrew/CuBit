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
# .cubit.resources version 2: the header names the resource basis
# (CCL.Resource_Sections.Basis_Kind: 0 none, 1 PCI function, 3 platform).
RESOURCES_VERSION = 2
BASIS_PCI, BASIS_PLATFORM = 1, 3
# .cubit.devices (CCL.Device_Sections): "CBDV", version 1, 16-byte entries.
DEVICES_MAGIC, DEVICES_VERSION = 0x56444243, 1
MATCH_PCI_IDS, MATCH_PCI_CLASS, MATCH_ACPI_ID, MATCH_USB_CLASS, MATCH_PLATFORM = 1, 2, 3, 4, 5


def devices_section(*entries):
    """The .cubit.devices bytes for (kind, payload) entries."""
    out = struct.pack('<IHH', DEVICES_MAGIC, DEVICES_VERSION, len(entries))
    for kind, payload in entries:
        out += struct.pack('<BBH', kind, len(payload), 0) + payload + bytes(12 - len(payload))
    return out


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
        result = subprocess.run([TOOL, definitions, manifest, '--startup-output',
                                 self.directory / 'startup.ads', '--schema', SCHEMA],
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

    def startup(self, section):
        """Decode .cubit.startup (CuBit.Startup_Tables) independently:
        ((limit kind, limit count), [(name, kind, need, state, handle)])."""
        magic, version, count, limit_kind, limit_count = struct.unpack_from('<IHHB3xI', section, 0)
        self.assertEqual((magic, version), (0x54534243, 1))
        self.assertEqual(len(section), 16 + 80 * count)
        entries = []
        for i in range(count):
            kind, need, state, length, reserved, handle = struct.unpack_from('<BBBBIQ', section, 16 + 80 * i)
            name = section[16 + 80 * i + 16:16 + 80 * i + 80]
            self.assertEqual(reserved, 0)
            self.assertEqual(name[length:], bytes(64 - length))
            entries.append((name[:length].decode(), kind, need, state, handle))
        return (limit_kind, limit_count), entries

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
        typed_bindings = (self.directory / 'startup.ads').read_text()
        keywords = self.compile(self.TLS_KEYWORDS, catalog)
        self.assertEqual(keywords.returncode, 0, keywords.stderr)
        self.assertEqual(self.sections(typed.stdout), self.sections(keywords.stdout))
        self.assertEqual(typed_bindings, (self.directory / 'startup.ads').read_text())

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
        # CAP-002 added .cubit.startup and made the caps entries' slot field
        # a reserved zero; every other byte is identical.
        self.assertEqual({k: v for k, v in sections.items() if k not in ('.cubit.startup', '.cubit.caps')},
                         {k: v for k, v in legacy.items() if k != '.cubit.caps'})
        self.assertEqual(self.startup(sections['.cubit.startup']),
                         ((1, 0), [('test-host', 1, 1, 1, 0), ('clock', 1, 1, 1, 0)]))
        self.assertEqual(sections['.cubit.caps'],
                         struct.pack('<IHH', 0x43424954, 1, 2) +
                         struct.pack('<BBHIQ', 2, 3, 0, 18, 0) +
                         struct.pack('<BBHIQ', 2, 3, 0, 19, 0))
        old = bytearray(legacy['.cubit.caps'])
        old[10:12] = old[26:28] = bytes(2)
        self.assertEqual(sections['.cubit.caps'], bytes(old))
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
        catalog = CATALOG.replace('(service clock 19',
                                  '(service clock (let ((base 15)) (+ base 4))')
        result = self.compile(source, catalog)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, self.compile().stdout)

    def test_catalog_is_data_not_compiler_builtin(self):
        result = self.compile(SOURCE.replace('clock', 'my-new-service'),
                              CATALOG.replace('(service clock 19', '(service my-new-service 9001'))
        self.assertEqual(result.returncode, 0, result.stderr)
        caps = self.sections(result.stdout)['.cubit.caps']
        self.assertEqual(struct.unpack_from('<I', caps, 28)[0], 9001)
        self.reject(catalog='(service-catalog v1)', diagnostic='UNKNOWN_SERVICE')

    def test_named_bindings_follow_declaration_order(self):
        # No slots: the caps entries carry a reserved zero, and the startup
        # table and Startup package name the requests in declaration order.
        result = self.compile()
        self.assertEqual(result.returncode, 0, result.stderr)
        caps = self.sections(result.stdout)['.cubit.caps']
        self.assertEqual(struct.unpack_from('<H', caps, 10)[0], 0)
        self.assertEqual(struct.unpack_from('<H', caps, 26)[0], 0)
        startup = (self.directory / 'startup.ads').read_text()
        self.assertLess(startup.index('function Test_Host'), startup.index('function Clock'))
        _, entries = self.startup(self.sections(result.stdout)['.cubit.startup'])
        self.assertEqual([e[0] for e in entries], ['test-host', 'clock'])
        requests = ['(request-service ccl-test-host read-write test-host)',
                    '(request-service clock read-write clock)']
        result = self.compile(SOURCE.replace(requests[0], 'PLACEHOLDER').replace(
            requests[1], requests[0]).replace('PLACEHOLDER', requests[1]))
        self.assertEqual(result.returncode, 0, result.stderr)
        _, entries = self.startup(self.sections(result.stdout)['.cubit.startup'])
        self.assertEqual([e[0] for e in entries], ['clock', 'test-host'])

    def test_rust_bindings_share_the_validated_manifest(self):
        ada = self.compile()
        output = self.directory / 'bindings.rs'
        result = subprocess.run([
            TOOL, self.directory / 'catalog.ccl',
            self.directory / 'manifest.ccl', '--rust-output', output],
            capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, ada.stdout)
        text = output.read_text()
        # Typed accessors (CAP-003), not name constants.
        self.assertIn('pub mod startup {', text)
        self.assertIn('    pub fn test_host(table: &Table) -> Option<Endpoint> {\n'
                      '        table.endpoint("test-host")\n    }', text)
        self.assertIn('    pub fn clock(table: &Table) -> Option<Endpoint> {', text)
        self.assertNotIn('BINDING_', text)
        self.assertNotIn('SLOT', text)
        # The declared table and the startup check the shared start calls.
        self.assertIn('cubit::startup::Expectation { name: "clock", kind: cubit::startup::Kind::Endpoint, '
                      'need: cubit::startup::Need::Required },', text)
        self.assertIn('    pub extern "C" fn __cubit_startup_check() {\n'
                      '        cubit::startup::require(EXPECTED, ', text)

    def test_c_header_shares_the_validated_manifest(self):
        ada = self.compile()
        output = self.directory / 'startup.h'
        result = subprocess.run([
            TOOL, self.directory / 'catalog.ccl',
            self.directory / 'manifest.ccl', '--c-output', output],
            capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, ada.stdout)
        text = output.read_text()
        self.assertIn('static inline cubit_endpoint cubit_startup_test_host(void)\n{\n'
                      '    return cubit_startup_endpoint("test-host");\n}', text)
        self.assertIn('cubit_endpoint cubit_startup_endpoint(const char *name);', text)
        self.assertTrue(text.rstrip().endswith('#endif'))
        self.assertIn('    {"test-host", 1, 1},  /* Endpoint, Required */', text)
        self.assertIn('__attribute__((weak)) void __cubit_startup_check(void)', text)
        self.assertIn('    cubit_startup_require(cubit_startup_expected, ', text)

    def test_rust_accessor_names(self):
        self.compile()
        manifest = self.directory / 'manifest.ccl'
        manifest.write_text(SOURCE.replace('read-write clock)', 'read-write type)'))
        result = subprocess.run([TOOL, self.directory / 'catalog.ccl', manifest,
                                 '--rust-output', self.directory / 'bindings.rs'],
                                capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, 1)
        self.assertIn('Rust keyword', result.stderr)

    def test_binding_name_validation(self):
        for name in ['25', 'bad_name', 'Clock', 'a--b', 'a-', '-a', '"clock"', 'x;bad']:
            with self.subTest(name=name):
                self.reject(SOURCE.replace('read-write clock)', f'read-write {name})'),
                            diagnostic='INVALID_BINDING_NAME')
        self.reject(SOURCE.replace('read-write clock)', 'read-write test-host)'),
                    diagnostic='DUPLICATE_BINDING')

    def test_retired_slot_declarations_are_refused(self):
        # CAP-002: catalogs no longer lay out slots.
        for retired in ['(application-slots 24 62)', '(fixed-binding clock 25)']:
            with self.subTest(retired=retired):
                self.reject(catalog=CATALOG[:-2] + ' ' + retired + ')',
                            diagnostic='UNKNOWN_DECLARATION')

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

    def test_identity_only_and_explicit_empty_requests(self):
        source = '(executable-manifest v1 (identity "a") (version "1"))'
        result = self.compile(source)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn('function ', (self.directory / 'startup.ads').read_text())
        result = self.compile(source)
        self.assertEqual(result.returncode, 0, result.stderr)
        sections = self.sections(result.stdout)
        self.assertEqual(set(sections), {'.cubit.id', '.cubit.startup'})
        self.assertEqual(sections['.cubit.startup'], struct.pack('<IHHB3xI', 0x54534243, 1, 0, 1, 0))
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
        STREAM, LEVEL, EDGE = 1, 2, 3
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
                ('"com.cubit.stdlog"', '"unix.stderr"', '"unix.stderr" is declared twice'),
                ('pages => 4', 'pages => 256', 'not 1 to 255'),
                # A single value is a Task, not a connector: no one-shot signal.
                ('signal => Signal.Level', 'signal => Signal.One_Shot', 'One_Shot'),
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
        catalog = '''(service-catalog v1
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
                         struct.pack('<BBHIQ', 7, 1, 0, 1, 0) +
                         struct.pack('<BBHIQ', 7, 2, 0, 1, 0) +
                         struct.pack('<BBHIQ', 1, 3, 0, 0, 0))
        self.reject(source.replace('keys publish', 'unrelated publish'), catalog,
                    'UNKNOWN_NOTIFICATION')
        self.reject(source.replace('request-notification keys publish',
                                   'request-service keys read'), catalog, 'UNKNOWN_SERVICE')
        self.reject(source, catalog.replace('publish-and-manage', 'publish'), 'RIGHTS_NOT_OFFERED')
        self.reject(source.replace('keys publish', 'keys read'), catalog, 'UNKNOWN_RIGHTS')
        self.reject(source, catalog.replace('keys 1', 'keys 128'), 'INVALID_NOTIFICATION_ID')
        self.reject(source.replace('focus)', 'events)'), catalog, 'DUPLICATE_BINDING')
        self.reject(source.replace('framebuffer)', 'Bad_name)'), catalog, 'INVALID_BINDING_NAME')

    def test_render_request_is_distinct_from_scanout_service(self):
        catalog = '''(service-catalog v1
          (service gpu 17 read-write))'''
        source = '''(executable-manifest v1 (identity "test") (version "1")
          (request-service gpu read-write scanout)
          (request-render read-write render))'''
        result = self.compile(source, catalog)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.sections(result.stdout)['.cubit.caps'],
                         struct.pack('<IHH', 0x43424954, 1, 2) +
                         struct.pack('<BBHIQ', 2, 3, 0, 17, 0) +
                         struct.pack('<BBHIQ', 11, 3, 0, 0, 0))
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
          (request-service config read-write config)
          (filesystem-scope (rights read) all)
          (config-scope (rights read write) all))'''
        native = (ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl').read_text()
        result = self.compile(source, native)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.sections(result.stdout)['.cubit.access'],
                         struct.pack('<IHHQ', 0x43434143, 1, 2, 0) +
                         bytes([1, 0, 0]) + bytes(77) + bytes([3, 0, 1]) + bytes(77))
        self.reject(source.replace('config-scope (rights read write)',
                                   'filesystem-scope (rights read write)'), catalog=native, diagnostic='DUPLICATE_SCOPE')
        self.reject(source.replace('all)', '"")'), catalog=native, diagnostic='INVALID_PATH')
        self.reject(source.replace('all)', 'all-folders)'), catalog=native, diagnostic='INVALID_PATH')
        for right in ('execute', 'create'):
            self.reject(source.replace('read write', right), catalog=native, diagnostic='INVALID_ACCESS_RIGHTS')

    def test_network_scope_encoding_and_validation(self):
        template = '''(executable-manifest v1 (identity "test") (version "1")
          (request-network tcp-connect (ipv4 "10.0.2.0" 24)
            (ports 80 443) (dns allow) (connections 6) network))'''
        result = self.compile(template)
        self.assertEqual(result.returncode, 0, result.stderr)
        descriptor = 80 | (443 << 16) | (24 << 32) | (1 << 40) | (1 << 48) | (6 << 49)
        self.assertEqual(self.sections(result.stdout)['.cubit.caps'],
                         struct.pack('<IHH', 0x43424954, 1, 1) +
                         struct.pack('<BBHIQ', 10, 3, 0, 0x0a000200, descriptor))
        listener = template.replace('tcp-connect', 'tcp-listen').replace(
            '10.0.2.0" 24', '10.0.2.15" 32').replace('80 443', '8080 8080').replace('dns allow', 'dns deny')
        result = self.compile(listener)
        self.assertEqual(result.returncode, 0, result.stderr)
        descriptor = 8080 | (8080 << 16) | (32 << 32) | (2 << 40) | (6 << 49)
        self.assertEqual(self.sections(result.stdout)['.cubit.caps'][8:],
                         struct.pack('<BBHIQ', 10, 3, 0, 0x0a00020f, descriptor))
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
                         struct.pack('<BBHIQ', 10, 3, 0, 0x0a000202, descriptor))
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
        header = struct.pack('<IHH', 0x53524243, RESOURCES_VERSION, 3) + struct.pack('<BBHHHQ', BASIS_PCI, 0, 0, 0, 0, 0)
        entries = (struct.pack('<BBHIQQ', 16, 3, 0, 0, 16384, 0) +
                   struct.pack('<BBHIQQ', 18, 1, 0, 0, 1, 1) +
                   struct.pack('<BBHIQQ', 19, 3, 0, 0, 1048576, 0))
        self.assertEqual(sections['.cubit.resources'], header + entries)
        self.assertEqual(sections['.cubit.devices'], devices_section((MATCH_PCI_CLASS, bytes([1, 8, 2]))))
        _, startup = self.startup(sections['.cubit.startup'])
        self.assertEqual([e[0] for e in startup], ['registers', 'completion', 'queues'])
        # Truncation must fail without exceptions or partial ELF output.
        for end in range(len(template) - 1):
            self.reject(template[:end])

    def test_typed_drives(self):
        # HW-002: a driver lists every device it drives (docs/device-matching.md).
        base = """(Executable_Manifest identity => "drv" version => "1"
          drives => [(Device_Match.PCI_IDs (PCI_IDs 0x8086 0x46D0 0x46D4))
                     (Device_Match.PCI_Class (PCI_Class 0x0C 0x03 0x30))
                     (Device_Match.ACPI_ID "PNP0C50")
                     (Device_Match.USB_Class (USB_Class 0x03 0x01 0x02))]
          requests => [(device-memory "registers" 0 16384 Access.Read_Write)])"""
        result = self.compile(base, self.DRIVER)
        self.assertEqual(result.returncode, 0, result.stderr)
        sections = self.sections(result.stdout)
        self.assertEqual(sections['.cubit.devices'], devices_section(
            (MATCH_PCI_IDS, struct.pack('<HHH', 0x8086, 0x46D0, 0x46D4)),
            (MATCH_PCI_CLASS, bytes([0x0C, 0x03, 0x30])),
            (MATCH_ACPI_ID, b'PNP0C50'),
            (MATCH_USB_CLASS, bytes([3, 1, 2]))))
        self.assertEqual(sections['.cubit.resources'][8:24],
                         struct.pack('<BBHHHQ', BASIS_PCI, 0, 0, 0, 0, 0))
        # Hexadecimal and decimal spell the same value.
        decimal = base.replace('0x8086 0x46D0 0x46D4', '32902 18128 18132')
        self.assertEqual(self.sections(self.compile(decimal, self.DRIVER).stdout)['.cubit.devices'],
                         sections['.cubit.devices'])
        pci_ids = '(Device_Match.PCI_IDs (PCI_IDs 0x8086 0x46D0 0x46D4))'
        for old, bad, diagnostic in [
                ('0x8086 0x46D0', '0xFFFF 0x46D0', 'INVALID_DEVICE_MATCH'),
                ('0x46D0 0x46D4', '0x46D4 0x46D0', 'INVALID_DEVICE_MATCH'),
                ('0x46D0 0x46D4', '0x46D0 0x10000', 'INVALID_DEVICE_MATCH'),
                ('0x0C 0x03 0x30', '0x0C 0x03 0x130', 'INVALID_DEVICE_MATCH'),
                ('"PNP0C50"', '"pnp0c50"', 'INVALID_DEVICE_MATCH'),
                ('"PNP0C50"', '"PNP0C5"', 'INVALID_DEVICE_MATCH'),
                ('"PNP0C50"', '"PNP0C50AB"', 'INVALID_DEVICE_MATCH'),
                (pci_ids, pci_ids + ' ' + pci_ids, 'DUPLICATE_DEVICE_MATCH'),
                (pci_ids, pci_ids + ' (Device_Match.Platform Platform_Device.PS2_Controller)',
                 'INVALID_DEVICE_MATCH'),
                ('(Device_Match.PCI_IDs', '(Device_Match.PCI_Range', 'INVALID_EXPRESSION')]:
            with self.subTest(bad=bad):
                result = self.compile(base.replace(old, bad), self.DRIVER)
                self.assertEqual(result.returncode, 1, result.stderr)
                self.assertEqual(result.stdout, '')
        # Seventeen entries exceed CCL.Device_Sections.MAX_ENTRIES.
        many = ' '.join('(Device_Match.PCI_IDs (PCI_IDs 0x8086 %d %d))' % (n, n) for n in range(17))
        result = self.compile(base.replace(pci_ids, many), self.DRIVER)
        self.assertEqual(result.returncode, 1)
        # Resources need a device devmgr gives resources for.
        acpi_only = base.replace(pci_ids, '').replace(
            '(Device_Match.PCI_Class (PCI_Class 0x0C 0x03 0x30))', '')
        self.assertEqual(self.compile(acpi_only, self.DRIVER).returncode, 1)

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
        self.assertEqual(resources[:24], struct.pack('<IHH', 0x53524243, RESOURCES_VERSION, 4) +
                         struct.pack('<BBHHHQ', BASIS_PLATFORM, 0, 1, 0, 0, 0))
        self.assertEqual(self.sections(result.stdout)['.cubit.devices'],
                         devices_section((MATCH_PLATFORM, bytes([1]))))
        self.assertEqual(resources[24:], struct.pack('<BBHIQQ', 17, 3, 0, 0, 1, 0) +
                         struct.pack('<BBHIQQ', 17, 3, 0, 1, 1, 0) +
                         struct.pack('<BBHIQQ', 18, 1, 0, 2, 1, 4) +
                         struct.pack('<BBHIQQ', 18, 1, 0, 3, 1, 4))
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
        # A driver may declare the devices it drives without asking for
        # resources (they come at run time); it still gets .cubit.devices.
        bare = self.compile(base.replace('(device-memory regs (bar 0) (max-bytes 4096) read-write)', ''))
        self.assertEqual(bare.returncode, 0, bare.stderr)
        bare_sections = self.sections(bare.stdout)
        self.assertNotIn('.cubit.resources', bare_sections)
        self.assertEqual(bare_sections['.cubit.devices'],
                         devices_section((MATCH_PCI_IDS, struct.pack('<HHH', 6900, 4096, 4096))))
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
                         struct.pack('<IHH', 0x53524243, RESOURCES_VERSION, 1) + bytes(16) +
                         struct.pack('<BBHIQQ', 20, 1, 0, 0, 1500, 5000))
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
                         struct.pack('<IHH', 0x43424954, 1, 1) + struct.pack('<BBHIQ', 2, 3, 0, 6, 0))
        self.assertEqual(sections['.cubit.resources'][24:],
                         struct.pack('<BBHIQQ', 20, 1, 0, 0, 1500, 5000))
        self.assertEqual(self.compile(template.replace(
            '(request-scheduling', '(requests-none)\n  (request-scheduling')).returncode, 0)

    def test_driver_draft_manifests(self):
        # Every driver manifest compiles against the driver catalog, with a
        # startup table naming each request (devmgr grants them by name).
        catalog = (ROOT / 'userspace/ccl/catalogs/driver-services.ccl').read_text()
        for name in ('nvme', 'ata', 'virtio-net', 'xhci', 'ps2', 'hda', 'mixer'):
            with self.subTest(driver=name):
                source = (ROOT / f'userspace/services/{name}/manifest.ccl').read_text()
                result = self.compile(source, catalog)
                self.assertEqual(result.returncode, 0, result.stderr)
                sections = self.sections(result.stdout)
                self.assertIn('.cubit.resources', sections)
                _, startup = self.startup(sections['.cubit.startup'])
                self.assertTrue(startup)
                package = (self.directory / 'startup.ads').read_text()
                for entry in startup:
                    self.assertIn(f'("{entry[0]}"', package)

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
            'basis PCI_BASIS 0 0 0',
            'DEVICE_MEMORY READ_WRITE index=0 amount=16384 extra=0',
            'INTERRUPT READ_ONLY index=0 amount=1 extra=1',
            'DMA READ_WRITE index=0 amount=1048576 extra=0'])
        # Hand-made sections the compiler never emits are refused.
        header = struct.pack('<IHH', 0x53524243, RESOURCES_VERSION, 1)
        pci = struct.pack('<BBHHHQ', BASIS_PCI, 0, 0, 0, 0, 0)
        memory = lambda **f: struct.pack('<BBHIQQ', f.get('kind', 16), f.get('rights', 3),
                                         f.get('reserved', 0), f.get('index', 0),
                                         f.get('amount', 4096), f.get('extra', 0))
        self.assertEqual(self.decode(header + pci + memory()).returncode, 0)
        for bad, status in [
                (header + pci + memory(reserved=4), 'INVALID_ENTRY'),
                (header + pci + memory(reserved=0x8000), 'INVALID_ENTRY'),
                (header + pci + memory(amount=4097), 'INVALID_ENTRY'),
                (header + pci + memory(rights=2), 'INVALID_ENTRY'),
                (header + pci + memory(index=6), 'INVALID_ENTRY'),
                (header + pci + memory(extra=1), 'INVALID_ENTRY'),
                (header + pci + memory(kind=21), 'INVALID_ENTRY'),
                (header + struct.pack('<BBHHHQ', 0, 0, 0, 0, 0, 0) + memory(), 'DEVICE_WITHOUT_BASIS'),
                (header + struct.pack('<BBHHHQ', BASIS_PCI, 1, 0, 0, 0, 0) + memory(), 'INVALID_BASIS'),
                (header + struct.pack('<BBHHHQ', BASIS_PCI, 0, 0, 0, 0, 1) + memory(), 'INVALID_BASIS'),
                (header + struct.pack('<BBHHHQ', BASIS_PCI, 0, 1, 8, 2, 0) + memory(), 'INVALID_BASIS'),
                (header + struct.pack('<BBHHHQ', 2, 0, 0, 0, 0, 0) + memory(), 'INVALID_BASIS'),
                (header + struct.pack('<BBHHHQ', BASIS_PLATFORM, 0, 9, 0, 0, 0) + memory(), 'INVALID_BASIS'),
                (header + pci + struct.pack('<BBHIQQ', 20, 1, 0, 0, 1500, 5000),
                 'BASIS_WITHOUT_DEVICE'),
                (struct.pack('<IHH', 0x53524243, 1, 1) + pci + memory(), 'INVALID_HEADER'),
                (struct.pack('<IHH', 0x53524243, RESOURCES_VERSION, 0) + pci, 'INVALID_HEADER')]:
            with self.subTest(status=status, bad=bad.hex()):
                result = self.decode(bad)
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout.strip(), status)
        # Scheduling beyond the kernel's real-time share is refused.
        scheduling = struct.pack('<IHH', 0x53524243, RESOURCES_VERSION, 1) + bytes(16)
        self.assertEqual(self.decode(scheduling + struct.pack('<BBHIQQ', 20, 1, 0, 0, 3500, 5000)).returncode, 0)
        self.assertEqual(self.decode(scheduling + struct.pack('<BBHIQQ', 20, 1, 0, 0, 3501, 5000)).stdout.strip(),
                         'INVALID_ENTRY')

    # ------------------------------------------------------------------
    # CAP-002 step 2: startup capabilities by name.
    # ------------------------------------------------------------------
    NATIVE = (ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl').read_text()
    DRIVER = (ROOT / 'userspace/ccl/catalogs/driver-services.ccl').read_text()
    FIXTURE = (ROOT / 'tests/ccl-manifests/fixtures/startup/manifest.ccl').read_text()
    GOLDEN = ROOT / 'tests/startup-tables/generated/startup.ads'
    GOLDEN_BODY = ROOT / 'tests/startup-tables/generated/startup.adb'

    def generate(self, source, catalog):
        manifest = self.directory / 'manifest.ccl'
        definitions = self.directory / 'catalog.ccl'
        manifest.write_text(source)
        definitions.write_text(catalog)
        output = self.directory / 'startup.ads'
        result = subprocess.run([TOOL, definitions, manifest, '--startup-output', output,
                                 '--schema', SCHEMA], capture_output=True, text=True, timeout=5)
        self.assertNotIn('raised ', result.stderr, result.stderr)
        return result, output

    def test_startup_section_declares_every_request(self):
        result = self.compile(self.FIXTURE, self.DRIVER)
        self.assertEqual(result.returncode, 0, result.stderr)
        limit, entries = self.startup(self.sections(result.stdout)['.cubit.startup'])
        self.assertEqual(limit, (2, 512))
        self.assertEqual(entries, [('registers', 6, 1, 1, 0), ('device', 8, 1, 1, 0),
                                   ('devmgr', 1, 1, 1, 0), ('pointer-events', 2, 2, 1, 0),
                                   ('real-time', 10, 1, 1, 0)])

    def test_startup_handle_limits(self):
        for spelling, expected in [('', (1, 0)), ('handles => Handle_Limit.Standard_Limit', (1, 0)),
                                   ('handles => Handle_Limit.Kernel_Limit', (3, 0)),
                                   ('handles => (Handle_Limit.At_Most 1)', (2, 1)),
                                   ('handles => (Handle_Limit.At_Most 32768)', (2, 32768))]:
            with self.subTest(spelling=spelling):
                result = self.compile(self.TLS_TYPED.replace(
                    '  scopes =>', f'  {spelling}\n  scopes =>'), self.NATIVE)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(self.startup(self.sections(result.stdout)['.cubit.startup'])[0], expected)
        for count in (0, -1, 32769):
            with self.subTest(count=count):
                result = self.compile(self.TLS_TYPED.replace(
                    '  scopes =>', f'  handles => (Handle_Limit.At_Most {count})\n  scopes =>'), self.NATIVE)
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, '')
                self.assertIn('handles: At_Most', result.stderr)
                self.assertIn('Handle_Limit.Kernel_Limit', result.stderr)

    def test_startup_optional_names(self):
        typed = self.TLS_TYPED.replace('  scopes =>', '  optional => ["clock" "tcp"]\n  scopes =>')
        result = self.compile(typed, self.NATIVE)
        self.assertEqual(result.returncode, 0, result.stderr)
        entries = self.startup(self.sections(result.stdout)['.cubit.startup'])[1]
        self.assertEqual([(e[0], e[1], e[2]) for e in entries],
                         [('config', 1, 1), ('clock', 1, 2), ('tcp', 5, 2)])
        # Optional does not change any older section.
        plain = self.sections(self.compile(self.TLS_TYPED, self.NATIVE).stdout)
        optional = self.sections(result.stdout)
        self.assertEqual({k: v for k, v in plain.items() if k != '.cubit.startup'},
                         {k: v for k, v in optional.items() if k != '.cubit.startup'})
        for names, message in [('["clok"]', 'optional entry 1: "clok" is not the binding of a request'),
                               ('["clock" "clock"]', 'optional entry 2: "clock" is listed twice')]:
            with self.subTest(names=names):
                result = self.compile(self.TLS_TYPED.replace(
                    '  scopes =>', f'  optional => {names}\n  scopes =>'), self.NATIVE)
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, '')
                self.assertIn(message, result.stderr)

    def test_startup_optional_render_both_frontends(self):
        keyword = ('(executable-manifest v1 (identity "a") (version "1") '
                   '(request-render-optional read-write render))')
        result = self.compile(keyword, self.NATIVE)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.startup(self.sections(result.stdout)['.cubit.startup'])[1],
                         [('render', 4, 2, 1, 0)])
        typed = '(Executable_Manifest identity => "a" version => "1" requests => [(Request.Optional_Render "render")])'
        self.assertEqual(self.sections(self.compile(typed, self.NATIVE).stdout),
                         self.sections(result.stdout))
        typed = '(Executable_Manifest identity => "a" version => "1" requests => [(render "render")])'
        result = self.compile(typed, self.NATIVE)
        self.assertEqual(self.startup(self.sections(result.stdout)['.cubit.startup'])[1],
                         [('render', 4, 1, 1, 0)])

    def test_startup_channel_limits(self):
        def channels(text):
            return self.TLS_TYPED.replace('  scopes =>', f'  channels => [{text}]\n  scopes =>')
        at = lambda n: f'(Channel_Limit.At_Most {n})'
        server = 'Channel_Limit.Server_Limit'
        for text in [f'(Channel_Offer "gpu.display" {at(1)} {at(1)} Channel_Class.Urgent)',
                     f'(Channel_Offer "gpu.command" {at(1)} {at(1)} Channel_Class.Interactive)',
                     f'(Channel_Offer "gpu.render" {server} {server})',
                     f'(Channel_Offer "fs" {at(4)} {server})',
                     f'(Channel_Offer contract => "a" total => {at(65536)} per_client => {at(65536)})',
                     f'(Channel_Offer "a" {at(1)} {at(2)}) (Channel_Offer "b" {server} {at(2)})']:
            with self.subTest(text=text):
                result = self.compile(channels(text), self.NATIVE)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn('.cubit.channels', self.sections(result.stdout))
        # The section's bytes: magic, version, count, then one offer.
        result = self.compile(channels(f'(Channel_Offer "gpu.render" {at(2)} {server} Channel_Class.Normal)'),
                              self.NATIVE)
        raw = self.sections(result.stdout)['.cubit.channels']
        self.assertEqual(raw[0:4], b'CBCH')
        self.assertEqual(int.from_bytes(raw[4:6], 'little'), 1)
        self.assertEqual(int.from_bytes(raw[6:8], 'little'), 1)
        offer = raw[8:8 + 80]
        self.assertEqual(offer[0], len('gpu.render'))
        self.assertEqual(offer[1], 3)                    # Normal
        self.assertEqual((offer[2], offer[3]), (1, 2))   # At_Most, Server_Limit
        self.assertEqual(int.from_bytes(offer[4:8], 'little'), 2)
        self.assertEqual(int.from_bytes(offer[8:12], 'little'), 0)
        self.assertEqual(offer[16:16 + 10], b'gpu.render')
        many = ' '.join(f'(Channel_Offer "c{i}" {at(1)} {at(1)})' for i in range(9))
        for text, message in [
                (f'(Channel_Offer "a" {at(0)} {at(1)})', 'per_client At_Most 0 is not 1 to 65536'),
                (f'(Channel_Offer "a" {at(3)} {at(2)})', 'per_client 3 is more than the total 2'),
                (f'(Channel_Offer "a" {at(1)} {at(0)})', 'total At_Most 0 is not 1 to 65536'),
                (f'(Channel_Offer "a" {at(1)} {at(65537)})', 'total At_Most 65537 is not 1 to 65536'),
                (f'(Channel_Offer "a b" {at(1)} {at(1)})', 'channels entry 1: contract "a b" is not 1 to 64'),
                (f'(Channel_Offer "" {at(1)} {at(1)})', 'channels entry 1: contract "" is not 1 to 64'),
                (f'(Channel_Offer "a" {at(1)} {at(1)}) (Channel_Offer "a" {at(1)} {at(1)})',
                 'channels entry 2: contract "a" is listed twice'),
                (many, 'channels entry 9: at most 8 contracts may be served'),
                ('(Channel_Offer "a" 1 1)', 'field per_client'),
                (f'(Channel_Offer "a" {at(1)})', 'field total')]:
            with self.subTest(text=text):
                result = self.compile(channels(text), self.NATIVE)
                self.assertEqual(result.returncode, 1, result.stderr)
                self.assertEqual(result.stdout, '')
                self.assertIn(message, result.stderr)

    def test_startup_connections_reserved_for_the_catalog(self):
        for item in ['(Connection.Offers "net.socket")', '(Connection.Uses "@my_network")',
                     '(Connection.Alias "@storage")']:
            with self.subTest(item=item):
                result = self.compile(self.TLS_TYPED.replace(
                    '  scopes =>', f'  connections => [{item}]\n  scopes =>'), self.NATIVE)
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, '')
                self.assertIn('connections: offers, uses and aliases come with the catalog and Connect', result.stderr)

    def test_startup_reserved_self_names(self):
        for name in ('self', 'self-process'):
            with self.subTest(name=name):
                self.reject(SOURCE.replace('read-write clock)', f'read-write {name})'),
                            diagnostic='RESERVED_BINDING_NAME')
                result = self.compile(self.TLS_TYPED.replace('"clock" "clock"', f'"clock" "{name}"'), self.NATIVE)
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, '')

    def test_startup_binding_golden(self):
        result, output = self.generate(self.FIXTURE, self.DRIVER)
        self.assertEqual(result.returncode, 0, result.stderr)
        text = output.read_text()
        self.assertEqual(text, self.GOLDEN.read_text())
        self.assertEqual(output.with_suffix('.adb').read_text(), self.GOLDEN_BODY.read_text())
        # Names, never numbers: no slot constants and no integer literals
        # outside comments. The Channels package carries the manifest's
        # declared channel limits (At_Most N), which are counts, not slots.
        capabilities = text.split('   package Channels is')[0]
        code = '\n'.join(line.split('--')[0] for line in capabilities.splitlines())
        self.assertNotIn('Slot', code)
        self.assertNotRegex(code, r'(?<![A-Za-z0-9_])[0-9]')
        self.assertIn('Gpu_Display_Per_Client : constant Standard.CuBit.Channel_Limits.Channel_Limit :=\n'
                      '        (Kind => Standard.CuBit.Channel_Limits.At_Most, Count => 1);', text)
        self.assertIn('Gpu_Render_Total : constant Standard.CuBit.Channel_Limits.Channel_Limit :=\n'
                      '        (Kind => Standard.CuBit.Channel_Limits.Server_Limit);', text)
        self.assertIn('Gpu_Command_Class : constant Standard.CuBit.Channel_Limits.Channel_Class :=\n'
                      '        Standard.CuBit.Channel_Limits.Interactive;', text)
        # The sections are the same with or without the binding.
        self.assertEqual(self.sections(result.stdout),
                         self.sections(self.compile(self.FIXTURE, self.DRIVER).stdout))

    def test_startup_binding_for_every_tree_manifest_kind(self):
        result, output = self.generate(SOURCE, CATALOG)
        self.assertEqual(result.returncode, 0, result.stderr)
        text = output.read_text()
        self.assertIn('function Test_Host return Standard.CuBit.Handles.Endpoint is', text)
        self.assertIn('function Clock return Standard.CuBit.Handles.Endpoint is', text)
        self.assertIn('--  Handle limit: standard (4096).', text)
        empty, output = self.generate('(executable-manifest v1 (identity "a") (version "1"))', CATALOG)
        self.assertEqual(empty.returncode, 0, empty.stderr)
        # Nothing declared, nothing to check (CAP-003): no table, no Require.
        self.assertNotIn('Expected', output.read_text())
        self.assertNotIn('Require', output.with_suffix('.adb').read_text())

    # ------------------------------------------------------------------
    # CAP-003: typed service endpoints and the startup check.
    # ------------------------------------------------------------------
    TYPED_CLIENTS = '''(executable-manifest v1 (identity "com.cubit.typed") (version "1")
  (request-service logstore read-write log)
  (request-service-optional log-observer read-write observer)
  (request-service process-manager read-write launcher)
  (request-service clock read-write clock))'''

    def test_startup_settings_accessors(self):
        """Startup.Settings: one instance per setting a scope covers, its
        generic chosen by shape and right; config-all adds the Inspector;
        settings need a required config request."""
        source = '''(executable-manifest v1 (identity "com.cubit.settings") (version "1")
  (request-service config read-write config)
  (config-scope (rights read) "time")
  (config-scope (rights read write) "desktop.display"))'''
        result, output = self.generate(source, self.NATIVE)
        self.assertEqual(result.returncode, 0, result.stderr)
        text = output.read_text()
        for line in ('package Time_Servers is new Standard.CuBit.Settings.Readable',
                     'package Time_Poll_Seconds is new Standard.CuBit.Settings.Readable\n'
                     '        (Standard.CuBit.Settings.Codecs.Count_Codec.Value, "time.poll-seconds", Config,',
                     'package Desktop_Display_Scale is new Standard.CuBit.Settings.Writable\n'
                     '        (Standard.Desktop_Display_Scale_Settings.Value,',
                     'package Desktop_Display_Mode is new Standard.CuBit.Settings.Writable_Family'):
            self.assertIn(line, text)
        self.assertNotIn('Inspector', text)
        self.assertNotIn('Desktop_Appearance', text)
        result, output = self.generate(source.replace('"time")', 'all)'), self.NATIVE)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('function Inspector return Standard.CuBit.Settings.Inspection.Inspector is\n'
                      '        (Standard.CuBit.Settings.Inspection.Using (Config));', output.read_text())
        for changed, message in [
                (source.replace('  (request-service config read-write config)\n', ''), 'requests no config service'),
                (source.replace('request-service config', 'request-service-optional config'),
                 'need a required config service')]:
            with self.subTest(message=message):
                result, _ = self.generate(changed, self.NATIVE)
                self.assertEqual(result.returncode, 1)
                self.assertIn(message, result.stderr)

    def test_startup_typed_clients(self):
        result, output = self.generate(self.TYPED_CLIENTS, self.NATIVE)
        self.assertEqual(result.returncode, 0, result.stderr)
        text = output.read_text()
        self.assertIn('function Log return Standard.CuBit.Handles.Services.Logstore.Endpoint is\n'
                      '     (Standard.CuBit.Handles.Services.Logstore.Named ("log"));', text)
        self.assertIn('function Observer return Standard.CuBit.Handles.Services.Log_Observer.Optional_Endpoint is',
                      text)
        self.assertIn('function Launcher return Standard.CuBit.Handles.Services.Process_Manager.Endpoint is', text)
        # A service without a client stays a plain endpoint.
        self.assertIn('function Clock return Standard.CuBit.Handles.Endpoint is', text)
        self.assertIn('package Startup with SPARK_Mode, Elaborate_Body is', text)
        body = output.with_suffix('.adb').read_text()
        self.assertIn('Standard.CuBit.Startup_Check.Logged.Require\n     (Expected, "com.cubit.typed",\n'
                      '      Standard.CuBit.Handles.Services.Logstore.Optional_Named ("log"));', body)
        # The sections do not change with the clients.
        self.assertEqual(self.sections(result.stdout),
                         self.sections(self.compile(self.TYPED_CLIENTS, self.NATIVE).stdout))

    def test_startup_check_without_logstore(self):
        result, output = self.generate(SOURCE, CATALOG)
        self.assertEqual(result.returncode, 0, result.stderr)
        body = output.with_suffix('.adb').read_text()
        self.assertIn('   Standard.CuBit.Startup_Check.Require (Expected, ', body)
        self.assertNotIn('Logged', body)

    def test_catalog_client_column(self):
        for column, accepted in [('(client Logstore)', True), ('(client Display)', False),
                                 ('(clients Logstore)', False)]:
            with self.subTest(column=column):
                catalog = self.NATIVE.replace('(service logstore 13 read-write (client Logstore))',
                                              f'(service logstore 13 read-write {column})')
                result = self.compile(self.TYPED_CLIENTS, catalog)
                self.assertEqual(result.returncode == 0, accepted, result.stderr)
        # Only a service has a client, not a notification.
        catalog = self.NATIVE.replace('(notification logstore 13 publish-and-manage)',
                                      '(notification logstore 13 publish-and-manage (client Logstore))')
        self.assertNotEqual(self.compile(self.TYPED_CLIENTS, catalog).returncode, 0)

    def test_config_scopes_name_settings(self):
        """CAP-003: a Config scope must name settings in the catalog."""
        base = '(executable-manifest v1 (identity "com.cubit.scope") (version "1") ' \
               '(request-service config read-write config) {})'
        for scope, accepted in [('(config-scope (rights read) "desktop.display.scale")', True),
                                ('(config-scope (rights read) "desktop.display.")', True),
                                ('(config-scope (rights read) "desktop.launch.10-workbench")', True),
                                ('(config-scope (rights read) all)', True),
                                ('(config-scope (rights read) "desktop.dispaly.scale")', False),
                                ('(config-scope (rights read) "no.such.setting")', False)]:
            with self.subTest(scope=scope):
                result = self.compile(base.format(scope), self.NATIVE)
                self.assertEqual(result.returncode == 0, accepted, result.stderr)
                if not accepted:
                    self.assertIn('names no setting in the setting catalog', result.stderr)

    def test_setting_catalog_is_checked(self):
        """A malformed catalog file stops every manifest with Config scopes."""
        settings = self.directory / 'settings'
        settings.mkdir()
        (self.directory / 'setting-catalog.ccl').write_text(
            (ROOT / 'userspace/ccl/interfaces/setting-catalog.ccl').read_text())
        manifest = self.directory / 'manifest.ccl'
        manifest.write_text('(executable-manifest v1 (identity "a") (version "1") '
                            '(request-service config read-write config) (config-scope (rights read) "a.b"))')
        def run():
            return subprocess.run([TOOL, ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl', manifest,
                                   '--schema', SCHEMA, '--settings', settings],
                                  capture_output=True, text=True, timeout=5)
        (settings / 'a.ccl').write_text('(Setting_Catalog settings => [(Setting "a.b" Shape.Key Value.Text '
                                        'Class.Application_State "")])')
        self.assertEqual(run().returncode, 0, run().stderr)
        for bad, why in [('(Setting "a.b" Shape.Key Value.Txt Class.Application_State "")', 'not a Setting_Catalog'),
                         ('(Setting "a.b." Shape.Key Value.Text Class.Application_State "")', "ends with '.'"),
                         ('(Setting "A.b" Shape.Key Value.Text Class.Application_State "")', 'is not 1 to 64'),
                         ('(Setting "a._b" Shape.Key Value.Text Class.Application_State "")', 'is not 1 to 64'),
                         ('(Setting "a.b" Shape.Key Value.Text Class.Application_State "Codec")', 'built-in codec'),
                         ('(Setting "a.b" Shape.Key Value.Text Class.Application_State "") '
                          '(Setting "a." Shape.Family Value.Text Class.Application_State "")', 'overlaps')]:
            with self.subTest(bad=bad):
                (settings / 'a.ccl').write_text(f'(Setting_Catalog settings => [{bad}])')
                result = run()
                self.assertEqual(result.returncode, 1)
                self.assertIn(why, result.stderr)

    def test_setting_kinds_are_generated_from_the_catalog(self):
        """CCL.Setting_Kinds is the catalog's: regenerate it to compare."""
        output = self.directory / 'ccl-setting_kinds.ads'
        result = subprocess.run([TOOL, '--setting-kinds', output, '--schema', SCHEMA],
                                capture_output=True, text=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(output.read_text(), (ROOT / 'userspace/ccl/src/ccl-setting_kinds.ads').read_text(),
                         'run make -C kernel setting-kinds after changing userspace/ccl/interfaces/settings/')
        self.assertIn('Starts (Key, "desktop.launch.") then Launch_Entry_Setting', output.read_text())

    def test_setting_codecs_are_generated_from_the_catalog(self):
        """CCL.Setting_Codecs (the readers' decoders) is the catalog's types':
        regenerate it to compare, file by file."""
        output = self.directory / 'codecs'
        output.mkdir()
        result = subprocess.run([TOOL, '--setting-codecs', output, '--schema', SCHEMA],
                                capture_output=True, text=True, timeout=60)
        self.assertEqual(result.returncode, 0, result.stderr)
        generated = sorted(path.name for path in output.iterdir())
        source = ROOT / 'userspace/ccl/src'
        self.assertEqual(generated, sorted(path.name for path in source.glob('ccl-setting_codecs*.ad?')))
        for name in generated:
            self.assertEqual((output / name).read_text(), (source / name).read_text(),
                             'run make -C kernel setting-kinds after changing a typed setting or its type')
        self.assertIn('procedure Decode (Source : String; Item : out Net_Interface_Value; Success : out Boolean)',
                      (output / 'ccl-setting_codecs-net_interface.ads').read_text())

    def test_system_configurations_name_catalog_settings(self):
        """Every key a system configuration seeds is a catalog setting, and
        an Address setting is IPv6 text (IPv4 mapped as ::ffff:a.b.c.d)."""
        import ipaddress
        import re
        declared = {}
        for catalog in sorted((ROOT / 'userspace/ccl/interfaces/settings').glob('*.ccl')):
            for key, shape, value in re.findall(r'\(Setting "([^"]+)" Shape\.(\w+)\s+\(?Value\.(\w+)',
                                                catalog.read_text()):
                declared[key] = (shape, value)
        self.assertIn('net.interface.', declared)
        self.assertNotIn('net.if0.address', declared)

        def setting_of(key):
            for name, (shape, value) in declared.items():
                if name == key or (shape == 'Family' and key.startswith(name)):
                    return value
            return None
        for configuration in ('system.ccl', 'tests/hardware/system-live.ccl',
                              'tests/timesync/system-timesync-test.ccl', 'tests/tls/system-tls-test.ccl',
                              'tests/config-turso/native/system-storage-test.ccl'):
            for key, rest in re.findall(r'\(setting "([^"]+)"\s+("[^"]*")?', (ROOT / configuration).read_text()):
                with self.subTest(configuration=configuration, key=key):
                    value = setting_of(key)
                    self.assertIsNotNone(value, f'{configuration}: "{key}" is in no userspace/ccl/interfaces/settings/ file')
                    if value == 'Address':
                        text = rest.strip('"')
                        # Raises for anything but IPv6 text, a bare dotted IPv4 included.
                        ipaddress.IPv6Address(text)

    def test_startup_output_must_be_a_spec(self):
        manifest = self.directory / 'manifest.ccl'
        manifest.write_text(self.TYPED_CLIENTS)
        result = subprocess.run([TOOL, ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl', manifest,
                                 '--startup-output', self.directory / 'startup.txt'],
                                capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, 1)
        self.assertIn('.ads', result.stderr)

    def test_startup_binding_refuses_unusable_names(self):
        for name in ('range', 'body', 'standard', 'expected', 'xor'):
            with self.subTest(name=name):
                result, output = self.generate(SOURCE.replace('read-write clock)', f'read-write {name})'), CATALOG)
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, '')
                self.assertIn(f'binding "{name}" cannot name a startup function', result.stderr)
                self.assertFalse(output.exists())
        # As a part of a longer name they are fine.
        result, _ = self.generate(SOURCE.replace('read-write clock)', 'read-write range-limit)'), CATALOG)
        self.assertEqual(result.returncode, 0, result.stderr)

    LINK_SCRIPTS = ('userspace/libc/link.ld', 'userspace/c/link.ld')

    def test_link_scripts_keep_every_manifest_section(self):
        """A program linked with --gc-sections (Mesa, Rust) keeps every
        section ccl-manifest emits, byte for byte: nothing references them,
        and a lost .cubit.startup gets the program refused at launch."""
        result = self.compile(self.FIXTURE, self.DRIVER)
        self.assertEqual(result.returncode, 0, result.stderr)
        manifest_source = self.directory / 'manifest.S'
        manifest = self.directory / 'manifest.o'
        manifest_source.write_text(result.stdout)
        subprocess.run(['gcc', '-c', manifest_source, '-o', manifest], check=True,
                       capture_output=True)
        emitted = self.extract(manifest)
        self.assertIn('.cubit.startup', emitted)
        self.assertIn('.cubit.resources', emitted)
        start_source = self.directory / 'start.S'
        start = self.directory / 'start.o'
        start_source.write_text('.text\n.globl _start\n_start:\n  hlt\n')
        subprocess.run(['gcc', '-c', start_source, '-o', start], check=True, capture_output=True)
        for script in self.LINK_SCRIPTS:
            with self.subTest(script=script):
                linked = self.directory / ('linked-' + pathlib.Path(script).parent.name)
                subprocess.run(['ld', '-static', '--gc-sections', '-e', '_start',
                                '-T', ROOT / script, start, manifest, '-o', linked],
                               check=True, capture_output=True)
                self.assertEqual(self.extract(linked), emitted)

    def test_mesa_window_manifest_declares_its_desktop(self):
        catalog = (ROOT / 'userspace/ccl/catalogs/native-runtime-services.ccl').read_text()
        source = (ROOT / 'tests/mesa-software/window-manifest.ccl').read_text()
        result = self.compile(source, catalog)
        self.assertEqual(result.returncode, 0, result.stderr)
        _, entries = self.startup(self.sections(result.stdout)['.cubit.startup'])
        self.assertEqual([name for name, *_ in entries], ['desktop'])

if __name__ == '__main__':
    unittest.main()
