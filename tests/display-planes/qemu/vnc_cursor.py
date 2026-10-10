#!/usr/bin/env python3
"""Fetch the cursor QEMU's VNC server publishes (RichCursor, encoding -239).

QEMU sends the cursor image and hotspot that the guest defined through the
virtio-gpu cursor queue; it is not part of the guest framebuffer. Writes
JSON {width, height, hot_x, hot_y, pixels: [0xAARRGGBB...]} (alpha from the
RichCursor mask: 0xFF where set, else 0)."""
import json, socket, struct, sys

def recv(s, n):
    data = b""
    while len(data) < n:
        chunk = s.recv(n - len(data))
        if not chunk:
            raise EOFError
        data += chunk
    return data

def main(path, out):
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.settimeout(5)
    s.connect(path)
    recv(s, 12)
    s.sendall(b"RFB 003.008\n")
    n = recv(s, 1)[0]
    types = recv(s, n)
    if 1 not in types:
        raise SystemExit("no None security")
    s.sendall(b"\x01")
    if struct.unpack(">I", recv(s, 4))[0] != 0:
        raise SystemExit("security failed")
    s.sendall(b"\x01")  # shared
    width, height = struct.unpack(">HH", recv(s, 4))
    recv(s, 16)  # pixel format: 32bpp little endian from QEMU by default
    name_len = struct.unpack(">I", recv(s, 4))[0]
    recv(s, name_len)
    # Ask for 32bpp true colour BGRX little endian explicitly.
    s.sendall(struct.pack(">BxxxBBBBHHHBBBxxx", 0, 32, 24, 0, 1, 255, 255, 255, 16, 8, 0))
    encodings = [-239, 0]  # RichCursor, Raw
    s.sendall(struct.pack(">BxH", 2, len(encodings)) + b"".join(struct.pack(">i", e) for e in encodings))
    s.sendall(struct.pack(">BBHHHH", 3, 0, 0, 0, width, height))
    while True:
        kind = recv(s, 1)[0]
        if kind != 0:
            raise SystemExit("unexpected server message %d" % kind)
        recv(s, 1)
        count = struct.unpack(">H", recv(s, 2))[0]
        for _ in range(count):
            x, y, w, h, enc = struct.unpack(">HHHHi", recv(s, 12))
            if enc == 0:
                recv(s, w * h * 4)
            elif enc == -239:
                pixels = recv(s, w * h * 4)
                mask = recv(s, (w + 7) // 8 * h)
                values = []
                for row in range(h):
                    for col in range(w):
                        b, g, r, _ = pixels[(row * w + col) * 4:(row * w + col) * 4 + 4]
                        bit = mask[row * ((w + 7) // 8) + col // 8] >> (7 - col % 8) & 1
                        values.append((0xFF000000 if bit else 0) | r << 16 | g << 8 | b)
                json.dump({"width": w, "height": h, "hot_x": x, "hot_y": y,
                           "pixels": values}, open(out, "w"))
                return
            else:
                raise SystemExit("unexpected encoding %d" % enc)

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
