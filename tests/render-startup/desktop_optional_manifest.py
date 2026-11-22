"""Test-only canonical optional-render request; not a CCL compiler replacement."""
import struct


def append_optional(data, slot=62):
    if len(data) < 8:
        raise ValueError("truncated header")
    magic, version, count = struct.unpack_from('<IHH', data)
    if magic != 0x43424954 or version != 1 or len(data) != 8 + 16 * count:
        raise ValueError("invalid capability metadata")
    if not 1 <= slot <= 62 or count == 65535:
        raise ValueError("invalid render slot or full metadata")
    entries = [struct.unpack_from('<BBHIQ', data, 8 + 16 * i) for i in range(count)]
    if any(kind == 11 or existing == slot for kind, _, existing, _, _ in entries):
        raise ValueError("render request or slot already present")
    # Exactly the procmgr v1 optional request; preserve every existing entry.
    return (struct.pack('<IHH', magic, version, count + 1) + data[8:] +
            struct.pack('<BBHIQ', 11, 3, slot, 1, 0))
