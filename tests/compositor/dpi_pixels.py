"""Exact solid rectangle oracle for native DPI screenshots."""


def coverage(origin, length, numerator, denominator):
    """Half-open logical interval sampled at output pixel centers.

    Buffer storage rounds its own extent upward; display coverage also depends
    on the logical origin's fractional phase. Do not equate the two extents.
    """
    assert origin >= 0 and length > 0 and numerator > 0 and denominator > 0
    def edge(value):
        return (2 * value * numerator + denominator - 1) // (2 * denominator)
    first, last = edge(origin), edge(origin + length)
    return first, last - first


def find_rectangle(data, image_width, image_height, rgb, width, height):
    if (len(data) != image_width * image_height * 3 or width <= 0 or height <= 0
            or width > image_width or height > image_height):
        return None
    pixel = bytes(rgb)
    row = pixel * width
    stride = image_width * 3
    for y in range(image_height - height + 1):
        start = y * stride
        end = start + stride
        while True:
            start = data.find(row, start, end)
            if start < 0:
                break
            x = (start - y * stride) // 3
            aligned = start % 3 == 0
            if aligned and (x == 0 or data[start-3:start] != pixel) and (
                    x + width == image_width or data[start+len(row):start+len(row)+3] != pixel):
                if all(data[start+j*stride:start+j*stride+len(row)] == row and
                       (x == 0 or data[start+j*stride-3:start+j*stride] != pixel) and
                       (x+width == image_width or
                        data[start+j*stride+len(row):start+j*stride+len(row)+3] != pixel)
                       for j in range(height)) and (
                           y == 0 or data[start-stride:start-stride+len(row)] != row) and (
                           y+height == image_height or
                           data[start+height*stride:start+height*stride+len(row)] != row):
                    return x, y
            start += 1
    return None


if __name__ == "__main__":
    assert coverage(102, 320, 5, 4) == (127, 400)
    assert coverage(112, 234, 5, 4) == (140, 292)
    assert coverage(112, 234, 3, 2) == (168, 351)
    # Independent enumeration uses the center inequality, not rounded edges.
    for n in range(1, 17):
        for d in range(1, 17):
            for origin in range(10):
                for length in range(1, 9):
                    first, count = coverage(origin, length, n, d)
                    expected = [p for p in range((origin+length)*n//d+2)
                                if 2*origin*n <= (2*p+1)*d < 2*(origin+length)*n]
                    assert list(range(first, first+count)) == expected
    color = (32, 176, 64)
    for w, h in ((320, 234), (400, 293), (480, 351)):
        iw, ih, x, y = w+30, h+40, 11, 13
        canvas = bytearray(iw*ih*3)
        for yy in range(y, y+h):
            canvas[(yy*iw+x)*3:(yy*iw+x+w)*3] = bytes(color)*w
        assert find_rectangle(canvas, iw, ih, color, w, h) == (x, y)
        assert find_rectangle(canvas, iw, ih, color, w-1, h) is None
        assert find_rectangle(canvas, iw, ih, color, w, h-1) is None
        assert find_rectangle(canvas, iw, ih, (176,32,64), w, h) is None
        assert find_rectangle(canvas[:-1], iw, ih, color, w, h) is None
        canvas[((y+h//2)*iw+x+w//2)*3] = 0
        assert find_rectangle(canvas, iw, ih, color, w, h) is None
    print("DPI pixels: PASS 20480 independent center-coverage cases; exact geometry and image rejection controls")
