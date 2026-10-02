import unittest
from check_pixels import check


class PixelOracle(unittest.TestCase):
    @staticmethod
    def capture(width=80, height=64, shift=0):
        pixels = bytearray(width * height * 3)
        for y in range(10, 42):
            for x in range(12 + shift, 44 + shift):
                offset = 3 * (y * width + x)
                pixels[offset:offset+3] = b"\xff\x00\xff"
        return f"P6\n{width} {height}\n255\n".encode() + pixels

    def test_valid_positions(self):
        for shift in range(20):
            check(self.capture(shift=shift))

    def test_malformed_capture(self):
        image = self.capture()
        for bad in (b"", image[:-1], image + b"x", image.replace(b"P6", b"P3", 1),
                    image.replace(b"255", b"256", 1), b"P6\n0 64\n255\n"):
            with self.assertRaises(ValueError):
                check(bad)

    def test_missing_or_extra_pixel(self):
        image = self.capture()
        with self.assertRaises(ValueError):
            check(image.replace(b"\xff\x00\xff", b"\x00\x00\x00", 1))
        with self.assertRaises(ValueError):
            check(image[:-3] + b"\xff\x00\xff")

    def test_same_count_wrong_geometry(self):
        image = self.capture()
        image = image.replace(b"\xff\x00\xff", b"\x00\x00\x00", 1)
        with self.assertRaises(ValueError):
            check(image[:-3] + b"\xff\x00\xff")


if __name__ == "__main__":
    unittest.main()
