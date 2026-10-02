"""Check the native Mesa four-region window in a QEMU framebuffer capture."""
import sys
from PIL import Image


def check(image):
    image = image.convert("RGB")
    pixels = image.load()
    # Locate the first pure red pixel; the deterministic test desktop has no
    # other pure-red surface. Do not assume a fixed window origin or screen size.
    origin = next(((x, y) for y in range(image.height) for x in range(image.width)
                   if pixels[x, y] == (255, 0, 0)), None)
    if origin is None:
        raise ValueError("no red Mesa region")
    x0, y0 = origin
    if x0 + 512 > image.width or y0 + 384 > image.height:
        raise ValueError("Mesa image clipped")
    colors = ((255, 0, 0), (0, 255, 0), (0, 0, 255), (255, 255, 255))
    for y in range(384):
        for x in range(512):
            expected = colors[(y // 192) * 2 + x // 256]
            if pixels[x0 + x, y0 + y] != expected:
                raise ValueError(f"wrong composed pixel at {x},{y}")
    return origin


if __name__ == "__main__":
    try:
        with Image.open(sys.argv[1]) as image:
            origin = check(image)
    except (OSError, ValueError, IndexError) as error:
        sys.exit(f"Mesa window: FAIL {error}")
    print(f"Mesa window: PASS 196608 composed pixels at {origin}")
