"""A minimal QMP client for the net-link fixtures (QEMU's machine protocol)."""
import json
import socket
import time


class QMP:
    def __init__(self, path, wait_seconds=30):
        deadline = time.monotonic() + wait_seconds
        while True:
            try:
                self.sock = socket.socket(socket.AF_UNIX)
                self.sock.connect(path)
                break
            except OSError:
                self.sock.close()
                if time.monotonic() >= deadline:
                    raise
                time.sleep(0.2)
        self.stream = self.sock.makefile("rwb")
        json.loads(self.stream.readline())  # greeting
        self.execute("qmp_capabilities")

    def execute(self, command, **arguments):
        message = {"execute": command}
        if arguments:
            message["arguments"] = arguments
        self.stream.write(json.dumps(message).encode() + b"\n")
        self.stream.flush()
        while True:  # skip asynchronous events
            reply = json.loads(self.stream.readline())
            if "return" in reply or "error" in reply:
                if "error" in reply:
                    raise RuntimeError(f"QMP {command}: {reply['error']}")
                return reply["return"]

    def set_link(self, netdev, up):
        """Pull (up=False) or plug back (True) the virtual cable of netdev."""
        self.execute("set_link", name=netdev, up=up)

    def quit(self):
        try:
            self.execute("quit")
        except (OSError, ValueError, RuntimeError):
            pass
