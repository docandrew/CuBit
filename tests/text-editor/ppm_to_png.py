"""Convert the hosted screenshots (binary PPM) to PNG with the standard
library only. Usage: ppm_to_png.py FILE.ppm... --into FOLDER
Names lose their "screen-" prefix: build/screen-find.ppm -> FOLDER/find.png."""
import os
import struct
import sys
import zlib


def read_ppm(path):
    with open(path, "rb") as f:
        data = f.read()
    fields = []
    i = 0
    while len(fields) < 4:
        while data[i:i + 1].isspace():
            i += 1
        j = i
        while not data[j:j + 1].isspace():
            j += 1
        fields.append(data[i:j])
        i = j
    if fields[0] != b"P6" or fields[3] != b"255":
        raise ValueError(path + ": not an 8-bit P6 PPM")
    return int(fields[1]), int(fields[2]), data[i + 1:]


def chunk(kind, body):
    return struct.pack(">I", len(body)) + kind + body + struct.pack(">I", zlib.crc32(kind + body) & 0xFFFFFFFF)


def write_png(path, width, height, rgb):
    stride = width * 3
    raw = b"".join(b"\x00" + rgb[y * stride:(y + 1) * stride] for y in range(height))
    with open(path, "wb") as f:
        f.write(b"\x89PNG\r\n\x1a\n")
        f.write(chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0)))
        f.write(chunk(b"IDAT", zlib.compress(raw, 9)))
        f.write(chunk(b"IEND", b""))


def main():
    args = sys.argv[1:]
    into = args[args.index("--into") + 1]
    os.makedirs(into, exist_ok=True)
    for path in args[:args.index("--into")]:
        name = os.path.basename(path)[:-4]
        if name.startswith("screen-"):
            name = name[len("screen-"):]
        width, height, rgb = read_ppm(path)
        out = os.path.join(into, name + ".png")
        write_png(out, width, height, rgb)
        print(out)


if __name__ == "__main__":
    main()
