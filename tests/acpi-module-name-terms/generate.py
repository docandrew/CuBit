"""Generate own small AML tables using public encodings and two own ASL seeds."""
from pathlib import Path
import hashlib
import json
import struct

sha = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()


def package_length(size):
    assert 0 <= size < 62
    return bytes([size+1])


def method(name, arguments, body):
    contents = name.encode()+bytes([arguments])+body
    return b'\x14'+package_length(len(contents))+contents


def scope(name, body):
    contents = name+body
    return b'\x10'+package_length(len(contents))+contents


def table(body, signature=b'SSDT', identifier=b'NAMETERM'):
    header = struct.pack('<4sIBB6s8sI4sI', signature, 36+len(body), 2, 0,
                         b'CUBIT ', identifier, 1, b'CBIT', 1)
    result = bytearray(header+body)
    result[9] = (-sum(result)) % 256
    assert len(result) == 36+len(body) and sum(result) % 256 == 0
    return bytes(result)


def decode_package(data, start):
    count = data[start] >> 6
    value = data[start] & (0x0f if count else 0x3f)
    for index in range(count):
        value |= data[start+1+index] << (4+8*index)
    return value, count+1


def generate(base, provider, output):
    base, provider, output = Path(base), Path(provider), Path(output)
    output.mkdir(parents=True, exist_ok=False)
    for path in [base, provider]:
        data = path.read_bytes()
        assert len(data) >= 36 and len(data) == int.from_bytes(data[4:8], 'little')
        assert sum(data) % 256 == 0
    raw = base.read_bytes()
    marker = b'_LID\x00'
    assert raw.count(marker) == 1
    position = raw.index(marker)
    spans = []
    for encoded in range(1, 5):
        start = position-encoded-1
        if raw[start] != 0x14:
            continue
        extent, used = decode_package(raw, start+1)
        if used == encoded and start+1+used == position:
            spans.append((start, start+1+extent))
    assert len(spans) == 1
    start, stop = spans[0]
    probe_base = output/'METHOD.aml'
    probe_base.write_bytes(table(raw[36:start]+method('_LID', 0, b'\xa4\\MISS')+raw[stop:],
                                b'DSDT', b'NAMEPROB'))
    prefix = b'\\SIDF'
    suffix = b'\\SIDF\x70\x01CONS'
    declaration = b'\x15\\MISS\x00\x00'
    # This External is deliberately unexecuted. An unconditional External would
    # create an ACPICA placeholder and test Uninitialized rather than absence.
    external = b'\xa0'+package_length(1+len(declaration))+b'\x00'+declaration
    cases = []

    def add(name, term=b'', definitions=b'', base_path=base):
        body = external+definitions+prefix+term+suffix
        path = output/(name+'.aml')
        path.write_bytes(table(body))
        paths = [base_path, path, provider]
        cases.append(dict(name=name, tables=[str(p) for p in paths],
                          table_sha256=[sha(p) for p in paths],
                          consumer_body_hex=body.hex(), term_hex=term.hex()))

    add('control')
    add('existing-root-name', b'\\CONS')
    add('missing-root-name', b'\\MISS')
    add('missing-nested-name', scope(b'\\DEV0', b'\\MISS'))
    add('missing-store-source', b'\x70\\MISS\\SINK')
    add('missing-nested-stores', b'\x70\x70\\MISS\\CONS\\SINK')
    add('missing-zero-arg-body', b'CAL0', method('CAL0', 0, b'\xa4\\MISS'))
    add('missing-one-arg-body', b'CAL1\\SIDF', method('CAL1', 1, b'\xa4\\MISS'))
    add('missing-last-argument', b'CAL1\\MISS', method('CAL1', 1, b'\xa4\x68'))
    add('missing-first-of-two-arguments', b'CAL2\\MISS\\SIDF', method('CAL2', 2, b'\xa4\x00'))
    add('method-probe-unknown', base_path=probe_base)
    add('missing-store-method-target', b'\x70\\MISS\\SIDF')
    add('missing-arithmetic-left', b'\x72\\MISS\\SIDF\\SINK')
    result = dict(cases=cases, count=len(cases), base_sha256=sha(base),
                  provider_sha256=sha(provider),
                  provenance='Own ASL seeds and public AML encodings; rebuilt lengths/checksums; no captured firmware or upstream test edits')
    (output/'FIXTURES.json').write_text(json.dumps(result, indent=2)+'\n')
    return cases
