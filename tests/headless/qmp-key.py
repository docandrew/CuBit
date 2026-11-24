#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""One key edge into a headless run, through QMP input-send-event.

Usage: qmp-key.py QMP_SOCKET down|up QCODE

A held key is "down", the hold (the caller sleeps), then "up". Unlike
QEMU's sendkey hold time (HMP "sendkey KEY MS", QMP send-key "hold-time"),
which queues the release after MS and another MS of delay behind it, so
keys sent meanwhile reach the guest late (BUG-007), each edge here goes to
the guest at once and nothing is queued behind it.
"""
import json
import socket
import sys

TIMEOUT_S = 10.0
EDGES = {'down': True, 'up': False}


def main():
    path, edge, qcode = sys.argv[1:4]
    if edge not in EDGES:
        print('qmp-key: the edge is down or up, not %r' % edge, file=sys.stderr)
        return 2
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
        s.connect(path)
        s.settimeout(TIMEOUT_S)
        f = s.makefile('rb')

        def reply():
            while True:
                line = f.readline()
                if not line:
                    raise RuntimeError('QMP closed')
                message = json.loads(line)
                if 'event' not in message:
                    return message

        def execute(command, arguments=None):
            request = {'execute': command}
            if arguments is not None:
                request['arguments'] = arguments
            s.sendall((json.dumps(request) + '\n').encode())
            answer = reply()
            if 'error' in answer:
                raise RuntimeError('QMP %s: %s' % (command, answer['error']))

        reply()                           # the greeting
        execute('qmp_capabilities')
        execute('input-send-event', {'events': [
            {'type': 'key', 'data': {'down': EDGES[edge], 'key': {'type': 'qcode', 'data': qcode}}}]})
    return 0


if __name__ == '__main__':
    sys.exit(main())
