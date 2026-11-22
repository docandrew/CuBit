"""Nix-hosted integration checks for atomic, content-verified ext2 builds."""
import importlib.util
from pathlib import Path
import shlex
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    'disk_builder', Path(__file__).resolve().parents[2] / 'tools/build_development_disk.py')
builder = importlib.util.module_from_spec(spec)
spec.loader.exec_module(builder)


class DevelopmentDiskTests(unittest.TestCase):
    def test_valid_and_corrupt_readback(self):
        with tempfile.TemporaryDirectory(prefix='cubit-disk-test-') as tmp:
            root = Path(tmp)
            source = root / 'source'
            source.write_bytes(bytes(range(256)) * 4097 + b'final file bytes')
            output = root / 'disk.img'
            builder.build(output, [('nested/app.bin', source)])
            original = builder.sha256(output)
            self.assertGreaterEqual(output.stat().st_size, 128 * 1024 * 1024)
            run = builder.subprocess.run

            def corrupt_dump(command, **kwargs):
                result = run(command, **kwargs)
                if command[0] == 'debugfs':
                    dumped = Path(shlex.split(command[2])[2])
                    # Same length, successful tool exit, wrong bytes: this is
                    # precisely what a size-only/debugfs-exit check misses.
                    with dumped.open('r+b') as stream:
                        stream.write(b'BAD')
                return result

            with patch.object(builder.subprocess, 'run', side_effect=corrupt_dump):
                with self.assertRaisesRegex(RuntimeError, 'payload mismatch'):
                    builder.build(output, [('nested/app.bin', source)])
            self.assertEqual(builder.sha256(output), original)
            self.assertFalse(list(root.glob('.cubit-disk-*')))

    def test_invalid_inputs_leave_existing_image(self):
        with tempfile.TemporaryDirectory(prefix='cubit-disk-input-') as tmp:
            root = Path(tmp)
            output, source = root / 'disk.img', root / 'source'
            output.write_bytes(b'previous valid image')
            source.write_bytes(b'payload')
            for files in [[('../escape', source)], [('a', root / 'missing')],
                          [('a', source), ('a', source)], [('a//b', source)],
                          [('a/./b', source)], [('a/', source)]]:
                with self.assertRaises(ValueError):
                    builder.build(output, files)
                self.assertEqual(output.read_bytes(), b'previous valid image')


if __name__ == '__main__':
    unittest.main()
