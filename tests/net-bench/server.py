#!/usr/bin/env python3
"""Host side of the network benchmark (tests/net-bench/net-bench.c).

Ports (on 127.0.0.1; QEMU user networking maps the guest's 10.0.2.2 here):
  18480 download: read an 8-byte little-endian size, send that many bytes
  18481 upload:   read an 8-byte size, then that many bytes; reply 8 bytes
  18482 rr:       echo each byte
  18483 connect:  send one byte and close
  18484 serve:    read the guest's counts (8-byte connections, 8-byte bytes),
                  then connect in SERVE_FORWARD (QEMU's forward to the guest's
                  listener) that many times reading one byte each, then once
                  more reading the bytes to the end

Usage: server.py SECONDS  (exits after SECONDS)
"""
import socket
import socketserver
import struct
import sys
import threading

CHUNK = memoryview(bytes(range(256)) * 4096)  # 1 MiB


def read_exact(sock, n):
    data = bytearray()
    while len(data) < n:
        part = sock.recv(n - len(data))
        if not part:
            raise ConnectionError("closed early")
        data += part
    return bytes(data)


class Download(socketserver.BaseRequestHandler):
    def handle(self):
        (size,) = struct.unpack("<Q", read_exact(self.request, 8))
        while size > 0:
            n = min(size, len(CHUNK))
            self.request.sendall(CHUNK[:n])
            size -= n


class Upload(socketserver.BaseRequestHandler):
    def handle(self):
        (size,) = struct.unpack("<Q", read_exact(self.request, 8))
        buf = bytearray(1 << 20)
        got = 0
        while got < size:
            n = self.request.recv_into(buf, min(len(buf), size - got))
            if n == 0:
                return
            got += n
        self.request.sendall(struct.pack("<Q", got))


class Echo(socketserver.BaseRequestHandler):
    def handle(self):
        self.request.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        while True:
            b = self.request.recv(1)
            if not b:
                return
            self.request.sendall(b)


class Connect(socketserver.BaseRequestHandler):
    def handle(self):
        self.request.sendall(b"c")


SERVE_FORWARD = ("127.0.0.1", 18486)


class Serve(socketserver.BaseRequestHandler):
    """The guest listens; connect in through QEMU's forward."""

    def handle(self):
        connections, size = struct.unpack("<QQ", read_exact(self.request, 16))
        for _ in range(connections):
            with socket.create_connection(SERVE_FORWARD, timeout=30) as c:
                if c.recv(1) != b"x":
                    raise ConnectionError("serve: no byte from the guest")
        with socket.create_connection(SERVE_FORWARD, timeout=30) as c:
            buf = bytearray(1 << 20)
            got = 0
            while True:
                n = c.recv_into(buf)
                if n == 0:
                    break
                got += n
            if got != size:
                raise ConnectionError(f"serve: {got} of {size} bytes")


class Server(socketserver.ThreadingTCPServer):
    allow_reuse_address = True
    daemon_threads = True
    request_queue_size = 256


def main():
    seconds = float(sys.argv[1]) if len(sys.argv) > 1 else 300
    for port, handler in ((18480, Download), (18481, Upload), (18482, Echo),
                          (18483, Connect), (18484, Serve)):
        server = Server(("127.0.0.1", port), handler)
        threading.Thread(target=server.serve_forever, daemon=True).start()
    threading.Event().wait(seconds)


if __name__ == "__main__":
    main()
