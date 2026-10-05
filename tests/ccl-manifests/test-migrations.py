#!/usr/bin/env python3
"""Compile every migrated declaration and compare all .cubit sections with
the recorded bytes of the hand-written manifests it replaced.

fixtures/<name>/<section>.bin hold those bytes (extracted once from the old
C byte arrays, docs/c-removal.md): an independent regression oracle, never
an app input.
"""
import argparse
import json
import pathlib
import struct
import subprocess
import tempfile

root = pathlib.Path(__file__).resolve().parents[2]
tool = root / 'userspace/ccl/build/manifest/ccl-manifest'
catalog = root / 'userspace/ccl/catalogs/native-runtime-services.ccl'
migrations = json.loads(pathlib.Path(__file__).with_name('migrations.json').read_text())
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--linked', action='store_true',
                    help='also compare the already-built native ELF metadata')
args = parser.parse_args()


def recorded(name):
    """The recorded sections of fixture `name`: .cubit.<x> from <x>.bin."""
    return {'.' + path.name[:-len('.bin')]: path.read_bytes()
            for path in sorted((root / 'tests/ccl-manifests/fixtures' / name).glob('*.bin'))}


def identity_bytes(identity, version):
    result = struct.pack('<IHH', 0x44494243, 1, 2)
    for key, value in ((b'id', identity.encode('ascii')), (b'version', version.encode('ascii'))):
        result += struct.pack('<BH', len(key), len(value)) + key + value
    return result


def extract(obj, directory):
    result = {}
    headers = subprocess.check_output(['objdump', '-h', obj], text=True)
    for line in headers.splitlines():
        words = line.split()
        if len(words) > 2 and words[1].startswith('.cubit.'):
            section = words[1]
            output = directory / (obj.name + section + '.bin')
            subprocess.run(['objcopy', '--dump-section', f'{section}={output}', obj],
                           check=True, capture_output=True)
            result[section] = output.read_bytes()
    return result


with tempfile.TemporaryDirectory(prefix='ccl-migrations.') as temporary:
    directory = pathlib.Path(temporary)
    for index, entry in enumerate(migrations):
        source = root / entry['directory'] / 'manifest.ccl'
        new = directory / f'{index}-new.o'
        asm = directory / f'{index}.S'
        with asm.open('w') as output:
            subprocess.run([tool, catalog, source], stdout=output, check=True)
        subprocess.run(['gcc', '-c', asm, '-o', new], check=True)
        expected, actual = recorded(entry['fixture']), extract(new, directory)
        if 'extra_fixture' in entry:
            extra_sections = recorded(entry['extra_fixture'])
            assert not expected.keys() & extra_sections.keys(), entry
            expected.update(extra_sections)
        if 'identity_update' in entry:
            update = entry['identity_update']
            corrected = identity_bytes(update['id'], update['version'])
            original = expected.get('.cubit.id')
            # Assert the precise old defect/omission, not an arbitrary exception
            # to equivalence. Every authority section still compares unchanged.
            if entry['directory'].endswith('/devices'):
                malformed = bytearray(corrected)
                malformed[9] = 19
                assert original == malformed, 'unexpected Devices identity change'
            elif entry['directory'].endswith('/network-check'):
                assert original is None, 'network-check unexpectedly has an identity'
            elif entry['directory'].endswith('/ccl-control'):
                value = update['id'].encode('ascii')
                assert original == (struct.pack('<IHHBH', 0x44494243, 1, 1, 2, len(value)) +
                                    b'id' + value), 'unexpected ccl-control identity change'
            else:
                raise AssertionError('unreviewed identity-update exception')
            expected['.cubit.id'] = corrected
            print(f'IDENTITY UPDATE {entry["directory"]}: {update["reason"]}', flush=True)
        # Intentional post-migration additions for the native taskbar. Keep the
        # recorded fixtures unchanged and specify the exact added authority
        # bytes independently of the compiler under test.
        if entry['directory'] == 'userspace/services/clock':
            assert '.cubit.caps' not in expected and '.cubit.access' not in expected
            expected['.cubit.caps'] = (struct.pack('<IHH', 0x43424954, 1, 1) +
                                      struct.pack('<BBHIQ', 2, 3, 20, 11, 0))
            expected['.cubit.access'] = (struct.pack('<IHHQ', 0x43434143, 1, 1, 0) +
                                        struct.pack('<BBB5x64s8x', 1, 6, 1, b'clock.'))
            print('AUTHORITY UPDATE clock: Config endpoint; read-only clock. scope', flush=True)
        elif entry['directory'] == 'userspace/services/desktop':
            old_caps = expected['.cubit.caps']
            assert old_caps[:8] == struct.pack('<IHH', 0x43424954, 1, 4)
            expected['.cubit.caps'] = (struct.pack('<IHH', 0x43424954, 1, 6) +
                                      struct.pack('<BBHIQ', 2, 3, 26, 20, 0) +
                                      struct.pack('<BBHIQ', 2, 3, 25, 19, 0) + old_caps[8:])
            print('AUTHORITY UPDATE desktop: master-audio and read-clock endpoints', flush=True)
        # Outlets replaced streams (docs/ccl-launch-parameters.md, "Inlets and
        # outlets, not stdio"): the recorded stdout stream becomes one text outlet named
        # under the program's identity, in .cubit.description.
        renamed_outlets = {'userspace/apps/sleep': b'com.cubit.sleep.status',
                         'userspace/apps/wget': b'com.cubit.wget.progress'}
        if entry['directory'] in renamed_outlets:
            old_streams = expected.pop('.cubit.streams')
            assert old_streams == (struct.pack('<IHH', 0x54534243, 1, 1) +
                                   struct.pack('<HHHH', 2, 4, 1, 0)), 'unexpected stream fixture'
            name = renamed_outlets[entry['directory']]
            expected['.cubit.description'] = (b'PDSC' + struct.pack('<HBBBBH', 1, 0, 0, 1, 0, 0) +
                                              bytes([2, 1, 1, 4, len(name)]) + name)
            print(f'OUTLET UPDATE {entry["directory"]}: stdout stream -> {name.decode()}', flush=True)
        assert expected.keys() == actual.keys(), (entry, expected.keys(), actual.keys())
        for section in expected:
            assert expected[section] == actual[section], (entry, section, expected[section].hex(), actual[section].hex())
        if args.linked:
            app_dir = root / entry['directory']
            suffix = '.svc' if '/services/' in entry['directory'] else '.app'
            name = 'ccl-test-host' if app_dir.name == 'test-host' else app_dir.name
            linked = app_dir / 'build' / (name + suffix)
            assert linked.is_file(), f'build {name} before --linked'
            assert extract(linked, directory) == expected, f'linked ELF metadata differs: {linked}'
        print(f'PASS {entry["directory"]}: {", ".join(actual)}', flush=True)
print(f'PASS: {len(migrations)} manifests; exact bytes including documented identity/authority updates')
