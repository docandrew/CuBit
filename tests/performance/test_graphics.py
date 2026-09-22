import unittest
from graphics import graphics_report, STAGES


def record(stage, size=10, regions=1, overflow=0):
    return f'GRAPHICS: stage={stage} bytes= {size} regions= {regions} overflow={overflow}\n'


class GraphicsTests(unittest.TestCase):
    def test_no_measurement_is_not_zero_copy(self):
        self.assertIsNone(graphics_report('desktop: ready\n'))

    def test_cumulative_not_sum_of_snapshots(self):
        text = ''.join(record(s) for s in STAGES)
        text += record('desktop_staging', 20, 2)
        result = graphics_report(text)
        self.assertTrue(result['valid'])
        self.assertEqual(result['observed_cpu_copy_bytes'], 50)

    def test_firmware_without_gpu(self):
        text = ''.join(record(s, 0, 0) for s in STAGES[:3])
        self.assertTrue(graphics_report(text)['valid'])
        self.assertFalse(graphics_report('virtio-gpu: ready\n' + text)['valid'])

    def test_fail_closed(self):
        good = ''.join(record(s) for s in STAGES)
        for bad in (record('desktop_staging', 9),
                    record('desktop_staging', 12, 1, 1),
                    record('desktop_staging', 2**64),
                    record('desktop_staging', 20, 0),
                    record('unknown'), 'mixed ' + record('display_backend'),
                    'GRAPHICS: stage=display_backend bytes=12\n'):
            with self.subTest(bad=bad):
                result = graphics_report(good + bad)
                self.assertFalse(result['valid'])
                self.assertIsNone(result['observed_cpu_copy_bytes'])
        self.assertFalse(graphics_report(record('desktop_staging'))['valid'])
