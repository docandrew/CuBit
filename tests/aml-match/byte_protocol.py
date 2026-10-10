"""Strict bounded ASCII framing for byte-valued ToString observations.

This module does not classify ACPICA errors or infer AML integer width.
"""
from dataclasses import dataclass

UINT64_MAX = 2**64 - 1
MAX_BYTES = 65536
MAX_ELEMENTS = 8192
MAX_DEPTH = 64
MAX_LINES = 16384
MAX_OUTPUT_BYTES = 1024 * 1024
STATUSES = frozenset(('RETURNED', 'OBJECT_RETURNED', 'UNSUPPORTED_VALUE',
                      'EMPTY_BUFFER', 'PACKAGE_LIMIT', 'VALUE_LIMIT', 'BUDGET_EXCEEDED'))


def unsigned(text, maximum):
    if not text or not text.isascii() or not text.isdecimal():
        raise ValueError('Invalid unsigned decimal')
    # Avoid arbitrary integer conversion cost before checking the bound.
    if len(text) > len(str(maximum)):
        raise ValueError('Oversized decimal')
    number = int(text)
    if number > maximum:
        raise ValueError('Out of range')
    return number


@dataclass
class Cursor:
    lines: list
    position: int = 0
    nodes: int = 0

    def take(self):
        if self.position >= len(self.lines):
            raise ValueError('Truncated result')
        line = self.lines[self.position]
        self.position += 1
        return line

    def value(self, depth=0):
        if depth > MAX_DEPTH or self.nodes >= MAX_ELEMENTS:
            raise ValueError('Result tree limit')
        self.nodes += 1
        line = self.take()
        if line.startswith('INTEGER '):
            return {'kind': 'integer', 'value': unsigned(line[8:].strip(), UINT64_MAX)}
        if line == 'BUFFER' or line.startswith('BUFFER '):
            fields = line.split()
            if fields[0] != 'BUFFER' or len(fields) - 1 > MAX_BYTES:
                raise ValueError('Buffer limit')
            return {'kind': 'buffer', 'hex': bytes(unsigned(x, 255) for x in fields[1:]).hex()}
        if line.startswith('PACKAGE '):
            count = unsigned(line[8:].strip(), MAX_ELEMENTS)
            if count > MAX_ELEMENTS - self.nodes:
                raise ValueError('Package node limit')
            return {'kind': 'package', 'items': [self.value(depth+1) for _ in range(count)]}
        raise ValueError('Unsupported result kind')


def parse(output):
    """Parse exact STATUS, optional tree, MARK; output must be ASCII bytes."""
    if not isinstance(output, bytes) or len(output) > MAX_OUTPUT_BYTES:
        raise ValueError('Invalid output envelope')
    text = output.decode('ascii')
    # Only LF is framing; no Unicode splitlines or CR normalization.
    if '\r' in text or '\x00' in text:
        raise ValueError('Invalid framing byte')
    lines = text.split('\n')
    if lines and lines[-1] == '':
        lines.pop()
    if not lines or len(lines) > MAX_LINES:
        raise ValueError('Invalid line count')
    cursor = Cursor(lines)
    first = cursor.take()
    if not first.startswith('STATUS ') or first[7:] not in STATUSES:
        raise ValueError('Invalid status')
    status = first[7:]
    result = cursor.value() if status in ('RETURNED', 'OBJECT_RETURNED') else None
    if status == 'RETURNED' and result['kind'] != 'integer':
        raise ValueError('Scalar status mismatch')
    if status == 'OBJECT_RETURNED' and result['kind'] == 'integer':
        raise ValueError('Object status mismatch')
    marker = cursor.take()
    if not marker.startswith('MARK '):
        raise ValueError('Missing marker')
    mark = unsigned(marker[5:], UINT64_MAX)
    if cursor.position != len(lines):
        raise ValueError('Trailing output')
    return {'status': status, 'result': result, 'marker': mark}


def matches(expected, returncode, output):
    if returncode != 0:
        return False
    try:
        return parse(output) == expected
    except (ValueError, UnicodeError):
        return False
