#!/usr/bin/env python3
"""Compare SDT admission with pinned FWTS checksum regression fixtures.

This uses upstream bytes and golden diagnoses; it does not run FWTS itself or
claim table-body conformance. Run inside nix develop. No hardware is accessed.
"""
import argparse
import collections
import hashlib
import io
import json
from pathlib import Path
import re
import subprocess
import tarfile
import tempfile
import urllib.request

COMMIT = 'f06eeafe26509961bfdffa60feb855623d79224e'
SHA256 = 'eb18a529ca5deaf76cd81812feea8f64535f6132e5a98d6ab3f8e57ca90bd6db'
CASES = ('0001', '0003', '0004')
ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / 'tests/aml-core/build/fwts'


def run(command, cwd):
    result = subprocess.run(command, cwd=cwd, text=True, capture_output=True)
    if result.returncode:
        raise RuntimeError(f'{command!r} failed:\n{result.stdout}\n{result.stderr}')
    return result.stdout.strip()


def tables(text):
    result = []
    current = None
    for line in text.splitlines():
        header = re.fullmatch(r'(.+?) @ 0x[0-9a-fA-F]+', line)
        if header:
            current = (header[1], bytearray())
            result.append(current)
            continue
        if not line.strip():
            continue
        row = re.fullmatch(r'\s+([0-9a-fA-F]+):((?: [0-9a-fA-F]{2}){1,16})\s{2,}.*', line)
        if row is None or current is None:
            raise ValueError(f'Unexpected dump line: {line!r}')
        if int(row[1], 16) != len(current[1]):
            raise ValueError('Noncontiguous table dump')
        current[1].extend(bytes.fromhex(row[2]))
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--archive', type=Path, help='Use a local archive; the same SHA256 is mandatory')
    args = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    archive = args.archive or BUILD / 'source.tar.gz'
    if not archive.exists():
        if args.archive:
            raise FileNotFoundError(archive)
        archive.write_bytes(urllib.request.urlopen(
            f'https://codeload.github.com/fwts/fwts/tar.gz/{COMMIT}', timeout=60).read())
    raw = archive.read_bytes()
    if hashlib.sha256(raw).hexdigest() != SHA256:
        raise ValueError('FWTS source archive SHA256 mismatch')
    fixtures = []
    with tarfile.open(fileobj=io.BytesIO(raw), mode='r:gz') as source:
        def read(name):
            member = source.extractfile(f'fwts-{COMMIT}/fwts-test/checksum-0001/{name}')
            if member is None:
                raise ValueError(f'Missing fixture {name}')
            return member.read().decode('utf-8')
        for case in CASES:
            expected = collections.defaultdict(collections.deque)
            for signature, correctness in re.findall(
                    r'Table (\S+) has (correct|incorrect) checksum', read(f'checksum-{case}.log')):
                expected[signature].append('ACCEPTED' if correctness == 'correct' else 'BAD_CHECKSUM')
            selected = []
            omitted = []
            for signature, data in tables(read(f'acpidump-{case}.log')):
                if signature in ('FACS', 'RSD PTR'):
                    omitted.append({'signature': signature, 'reason': 'Not a standard SDT checksum case'})
                    continue
                if len(signature) != 4 or not expected[signature]:
                    raise ValueError(f'No upstream diagnosis for {case}/{signature}')
                selected.append((signature, bytes(data), expected[signature].popleft()))
            remaining = {sig: list(values) for sig, values in expected.items() if values}
            # FWTS synthesizes an RSDT; the fixture has no bytes for that table.
            if remaining != {'RSDT': ['ACCEPTED']} or len(selected) != 16:
                raise ValueError(f'Unexpected fixture coverage: {case}: {remaining}, {len(selected)}')
            omitted.append({'signature': 'RSDT', 'reason': 'Golden output only; no input bytes'})
            fixtures.append((case, selected, omitted))
    report = {'commit': COMMIT, 'sha256': SHA256, 'scope': 'FWTS SDT checksum fixture comparisons',
              'fwts_executed': False, 'cases': [], 'omitted': []}
    with tempfile.TemporaryDirectory(prefix='probe-', dir=BUILD) as tmp:
        work = Path(tmp)
        (work / 'probe.adb').write_text('''with Ada.Command_Line; with Ada.Text_IO;
with Ada.Streams.Stream_IO; with Interfaces; with Firmware_Tables;
procedure Probe is
 package IO renames Ada.Streams.Stream_IO;
 F : IO.File_Type;
 Name : constant String := Ada.Command_Line.Argument (2);
begin
 IO.Open (F, IO.In_File, Ada.Command_Line.Argument (1));
 declare
  Data : Firmware_Tables.Bytes (1 .. Natural (IO.Size (F)));
  R : Firmware_Tables.Table_Result;
 begin
  for I in Data'Range loop Interfaces.Unsigned_8'Read (IO.Stream (F), Data (I)); end loop;
  IO.Close (F);
  R := Firmware_Tables.Read_Table (Data, Name);
  Ada.Text_IO.Put_Line (R.Status'Image);
 end;
end Probe;
''')
        (work / 'probe.gpr').write_text('''project Probe is
 for Source_Dirs use (".", "FIRMWARE");
 for Object_Dir use "obj"; for Exec_Dir use "."; for Main use ("probe.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
 end Compiler;
end Probe;
'''.replace('FIRMWARE', str(ROOT / 'shared/firmware')))
        run(['alr', 'exec', '--', 'gprbuild', '-p', '-P', str(work / 'probe.gpr')], ROOT / 'kernel')
        for case, selected, omitted in fixtures:
            report['omitted'].append({'fixture': case, 'tables': omitted})
            for index, (signature, data, expected) in enumerate(selected):
                binary = work / f'{case}-{index}.dat'
                binary.write_bytes(data)
                actual = run([str(work / 'probe'), str(binary), signature], work)
                report['cases'].append({'fixture': case, 'index': index, 'signature': signature,
                                        'expected': expected, 'actual': actual,
                                        'passed': actual == expected})
    failed = sum(not row['passed'] for row in report['cases'])
    report['summary'] = {'passed': len(report['cases']) - failed, 'failed': failed,
                         'fixtures_selected': len(CASES)}
    (BUILD / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print('FWTS-SDT-CHECKSUM: ' + json.dumps(report['summary']))
    if failed:
        raise SystemExit(1)


if __name__ == '__main__':
    main()
