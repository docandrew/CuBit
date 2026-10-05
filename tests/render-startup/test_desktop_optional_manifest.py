import struct
import unittest
from desktop_optional_manifest import append_optional


class OptionalDesktopManifest(unittest.TestCase):
    def test_preserves_existing_requests(self):
        entries = [struct.pack('<BBHIQ', 1, 3, slot, slot + 50, 0) for slot in (20, 22, 25)]
        before = struct.pack('<IHH', 0x43424954, 1, len(entries)) + b''.join(entries)
        after = append_optional(before)
        self.assertEqual(after[8:-16], before[8:])
        self.assertEqual(struct.unpack_from('<IHH', after), (0x43424954, 1, 4))
        self.assertEqual(struct.unpack('<BBHIQ', after[-16:]), (11, 3, 62, 1, 0))

    def test_rejects_invalid_or_conflicting_metadata(self):
        header = struct.pack('<IHH', 0x43424954, 1, 1)
        for bad in (b'', header, header + b'\0' * 17,
                    header + struct.pack('<BBHIQ', 11, 3, 25, 0, 0),
                    header + struct.pack('<BBHIQ', 1, 3, 62, 0, 0),
                    struct.pack('<IHH', 0x43424954, 2, 0)):
            with self.subTest(bad=bad), self.assertRaises(ValueError):
                append_optional(bad)


if __name__ == '__main__':
    unittest.main()
