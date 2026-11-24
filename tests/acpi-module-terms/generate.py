"""Own synthetic module terms. No captured firmware is read or embedded."""
from pathlib import Path
import argparse
import hashlib
import json
import subprocess


def package_length(data, start):
    follow = data[start] >> 6
    value = data[start] & (0x0F if follow else 0x3F)
    for index in range(follow):
        value |= data[start + index + 1] << (4 + 8 * index)
    return value, follow + 1


def specifications():
    cases = []
    def add(name, expression='Zero', checks='', raw=None, copies=1, expected='PASS', execution=None, truncate=False, declarations=''):
        cases.append(dict(name=name, expression=expression, checks=checks, raw=raw,
                          copies=copies, expected=expected, execution=execution, truncate=truncate, declarations=declarations))
    add('control', raw='')
    add('noop', raw='a3')
    for name, data in [('zero', '00'), ('one', '01'), ('ones', 'ff'), ('byte', '0a02'),
                       ('word', '0b3412'), ('dword', '0c78563412'),
                       ('qword', '0e8877665544332211')]:
        add(name, raw=data)
    add('string', '"discarded"')
    add('empty_string', '""')
    add('buffer', 'Buffer (4) {0xA5, 0x5A}')
    add('empty_buffer', 'Buffer (Zero) {}')
    add('package', 'Package (2) {One, "discarded"}')
    add('variable_buffer', 'Buffer (VAL0 ()) {0xA5}', 'If (SIDE != One) { Increment (ERRS) }')
    add('variable_package', 'Package (SIDE) {}', 'If (SIDE != Zero) { Increment (ERRS) }')
    for name, expression in [('named_integer', 'SINK'), ('named_string', 'STR0'),
                             ('named_buffer', 'BUF0'), ('named_package', 'PKG0')]:
        add(name, expression)
    add('reference', 'RefOf (SINK)')
    add('dereference', 'DerefOf (RefOf (SINK))')
    add('add', 'Add (2, 3, SINK)', 'If (SINK != 5) { Increment (ERRS) }')
    add('subtract', 'Subtract (8, 3, SINK)', 'If (SINK != 5) { Increment (ERRS) }')
    add('multiply', 'Multiply (2, 3, SINK)', 'If (SINK != 6) { Increment (ERRS) }')
    add('divide', 'Divide (7, 2, REM0, SINK)', 'If (SINK != 3) { Increment (ERRS) } If (REM0 != One) { Increment (ERRS) }')
    add('xor', 'Xor (3, One, SINK)', 'If (SINK != 2) { Increment (ERRS) }')
    add('logical', 'LEqual (One, One)')
    add('object_type', 'ObjectType (BUF0)')
    add('size_of', 'SizeOf (BUF0)')
    add('to_integer', 'ToInteger ("5", SINK)', 'If (SINK != 5) { Increment (ERRS) }')
    add('concatenate', 'Concatenate ("a", "b", STR0)', 'If (STR0 != "ab") { Increment (ERRS) }')
    add('index_reference', 'Index (BUF0, One)', 'If (DerefOf (Index (BUF0, One)) != 0x5A) { Increment (ERRS) }')
    add('one_call', 'VAL0 ()', 'If (SIDE != One) { Increment (ERRS) }')
    add('nested_call', 'Add (VAL0 (), One, SINK)', 'If (SIDE != One) { Increment (ERRS) } If (SINK != 5) { Increment (ERRS) }')
    add('void_call', checks='If (SIDE != One) { Increment (ERRS) }', raw=b'VOD0'.hex())
    add('revision', raw='5b30')
    add('timer', raw='5b33')
    add('buffer_discard_stress', 'Buffer (VAL0 ()) {0xA5}',
        'If (SIDE != 128) { Increment (ERRS) } If (DerefOf (Index (BUF0, One)) != 0x5A) { Increment (ERRS) }', copies=128)
    add('unsupported_opcode', raw='fe', expected='UNSUPPORTED', execution=['UNSUPPORTED'])
    add('truncated_byte', raw='0a', expected='UNSUPPORTED', execution=['TRUNCATED'], truncate=True)
    add('truncated_word', raw='0b01', expected='UNSUPPORTED', execution=['TRUNCATED'], truncate=True)
    add('truncated_dword', raw='0c0102', expected='UNSUPPORTED', execution=['TRUNCATED'], truncate=True)
    add('truncated_qword', raw='0e010203', expected='UNSUPPORTED', execution=['TRUNCATED'], truncate=True)
    add('unterminated_string', raw='0d414243', expected='UNSUPPORTED', execution=['TRUNCATED'], truncate=True)
    add('oversized_buffer', raw='11060c00000400', expected='UNSUPPORTED', execution=['VALUE_LIMIT'])
    add('hardware_denied', raw=b'F000'.hex(), expected='UNSUPPORTED',
        execution=['UNSUPPORTED', 'UNSUPPORTED_VALUE', 'FIELD_INDETERMINATE'],
        declarations='OperationRegion (R000, EmbeddedControl, Zero, One) Field (R000, ByteAcc, NoLock, Preserve) { F000, 8 }')
    return cases


def generate(output, iasl, only=None):
    output.mkdir(parents=True, exist_ok=False)
    rows = []
    for case in specifications():
        if only and case['name'] not in only:
            continue
        folder = output / case['name']
        folder.mkdir()
        source = folder / 'SOURCE.asl'
        source.write_text('''DefinitionBlock ("", "DSDT", 2, "CUBIT", "MODTERMS", 1) {
 Name (ERRS, Zero) Name (RMRC, One)
 Name (RP0P, Package () {":STST:synthetic:module-terms:PASS:"})
 Name (SINK, Zero) Name (SIDE, Zero) Name (REM0, Zero) Name (REF0, Zero)
 Name (BUF0, Buffer (4) {0xA5, 0x5A, Zero, Zero})
 Name (STR0, "keep") Name (PKG0, Package (2) {One, "keep"})
''' + case['declarations'] + '''
 Method (VAL0, 0, NotSerialized) { Increment (SIDE) Return (4) }
 Method (VOD0, 0, NotSerialized) { Increment (SIDE) }
 Name (ANCH, One)
 Method (SNIP, 0, NotSerialized) { Return (''' + case['expression'] + ''') }
 Name (AFTR, One)
 Method (MN00, 0, NotSerialized) {
   If (AFTR != One) { Increment (ERRS) }
''' + case['checks'] + '''
   Return (ERRS)
 }
}
''')
        argv = ['timeout', '--kill-after=2s', '20s', str(iasl), '-oa', '-p', str(folder / 'SOURCE'), str(source)]
        process = subprocess.run(argv, cwd=folder, capture_output=True, text=True, timeout=25)
        log = folder / 'compile.log'
        log.write_text(process.stdout + process.stderr)
        assert process.returncode == 0, (case['name'], str(log))
        data = (folder / 'SOURCE.aml').read_bytes()
        marker = b'SNIP\x00\xA4'
        assert data.count(marker) == 1, case['name']
        name = data.index(marker)
        matches = []
        for encoded in range(1, 5):
            start = name - encoded - 1
            if start < 36 or data[start] != 0x14:
                continue
            extent, used = package_length(data, start + 1)
            if used == encoded and start + 1 + used == name:
                matches.append((start, start + 1 + extent))
        assert len(matches) == 1, case['name']
        start, end = matches[0]
        term = data[name + len(marker):end] if case['raw'] is None else bytes.fromhex(case['raw'])
        if case['truncate']:
            result = bytearray(data[:start] + term)
        else:
            result = bytearray(data[:start] + term * case['copies'] + data[end:])
        result[4:8] = len(result).to_bytes(4, 'little')
        result[9] = 0
        result[9] = (-sum(result)) % 256
        assert sum(result) % 256 == 0
        aml = folder / 'MAIN.aml'
        aml.write_bytes(result)
        rows.append(dict(case, aml=str(aml), aml_sha256=hashlib.sha256(result).hexdigest(),
                         source=str(source), source_sha256=hashlib.sha256(source.read_bytes()).hexdigest(),
                         compile_argv=argv, compile_exit=process.returncode,
                         compile_log=str(log), inserted_term_hex=term.hex()))
    (output / 'FIXTURES.json').write_text(json.dumps(dict(cases=rows,
        provenance='Own IASL control with one Return operand moved to module TermList; explicit public-encoding raw controls, fresh header length/checksum. No captured firmware.'), indent=2) + '\n')
    print('GENERATED', len(rows), 'own module-term fixtures', flush=True)
    return rows


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--iasl', required=True)
    parser.add_argument('--only', nargs='+')
    args = parser.parse_args()
    generate(args.output, args.iasl, args.only)
