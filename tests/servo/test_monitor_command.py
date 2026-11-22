#!/usr/bin/env python3
"""Actual fixture command waits through fragmented QEMU prompts and echoes."""
import ast
from pathlib import Path
import socket
import tempfile
import threading
import time

source = ast.parse(Path(__file__).with_name('browser_input.py').read_text())
command = next(n for n in source.body if isinstance(n, ast.FunctionDef) and n.name == 'command')
module = ast.Module(body=[command], type_ignores=[])
with tempfile.TemporaryDirectory(prefix='servo-monitor-') as directory:
    monitor = Path(directory) / 'monitor.sock'
    capture = Path(directory) / 'capture'
    errors = []
    with socket.socket(socket.AF_UNIX) as server:
        server.bind(str(monitor))
        server.listen(1)
        def serve():
            try:
                with server.accept()[0] as peer:
                    for chunk in (b'QEMU monitor\r\n', b'(qe', b'mu) '):
                        peer.sendall(chunk)
                        time.sleep(.02)
                    data = b''
                    while not data.endswith(b'\n'):
                        data += peer.recv(4096)
                    assert data == b'screendump test\n'
                    for chunk in (b's', b'\x1b[K', b'creendump test\r\n'):
                        peer.sendall(chunk)
                        time.sleep(.02)
                    capture.write_bytes(b'complete screenshot')
                    for chunk in (b'(qe', b'mu) '):
                        peer.sendall(chunk)
                        time.sleep(.02)
            except Exception as error:
                errors.append(error)
        worker = threading.Thread(target=serve)
        worker.start()
        namespace = {'socket': socket, 'monitor': monitor}
        exec(compile(module, 'browser_input.py', 'exec'), namespace)
        namespace['command']('screendump test')
        assert capture.read_bytes() == b'complete screenshot'
        worker.join(timeout=2)
        assert not worker.is_alive() and not errors, errors
print('SERVO-MONITOR: PASS fragmented banner, echo, completion prompt and capture ordering')
