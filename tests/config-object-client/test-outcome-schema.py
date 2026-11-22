#!/usr/bin/env python3
"""Keep the approved Config outcome identity tied to its reviewed declaration."""
import hashlib
from pathlib import Path
import re
import unittest


class OutcomeSchema(unittest.TestCase):
    def test_identity(self):
        root = Path(__file__).resolve().parents[2] / "userspace/lib/config"
        declaration = (root / "config-write-outcome.schema").read_bytes()
        spec = (root / "config_object_outcomes.ads").read_text()
        key = spec.split("Key : constant", 1)[1].split(";", 1)[0]
        words = re.findall(r"16#([0-9A-F]+)#", key)
        self.assertEqual(len(words), 4)
        encoded = b"".join(int(word, 16).to_bytes(8, "big") for word in words)
        self.assertEqual(encoded, hashlib.sha256(declaration).digest())


if __name__ == "__main__":
    unittest.main()
