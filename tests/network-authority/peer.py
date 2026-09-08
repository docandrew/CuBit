"""Loopback-only peer for the real CuBit outbound TCP policy regression."""
import socket

with socket.socket() as listener:
    listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    listener.bind(("127.0.0.1", 18443))
    listener.listen(1)
    listener.settimeout(120)
    with listener.accept()[0] as connection:
        connection.settimeout(15)
        request = bytearray()
        while len(request) < 4:
            chunk = connection.recv(4 - len(request))
            if not chunk:
                raise RuntimeError("guest closed before completing PING")
            request.extend(chunk)
        if request != b"PING":
            raise RuntimeError("unexpected guest payload")
        connection.sendall(b"PONG")
        connection.shutdown(socket.SHUT_WR)
        print("network peer: PASS", flush=True)
