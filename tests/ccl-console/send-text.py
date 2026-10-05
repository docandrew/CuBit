#!/usr/bin/env python3
"""Type text into a CuBit guest through the QEMU monitor socket (sendkey),
then Enter: send-text.py SOCKET TEXT [SECONDS_PER_KEY]. US layout."""
import socket
import sys
import time

SHIFTED = {'(': '9', ')': '0', '"': 'apostrophe', '>': 'dot', '<': 'comma', '_': 'minus',
           ':': 'semicolon', '@': '2', '+': 'equal', '*': '8', '!': '1', '?': 'slash',
           '{': 'bracket_left', '}': 'bracket_right', '|': 'backslash', '~': 'grave_accent',
           '#': '3', '$': '4', '%': '5', '^': '6', '&': '7'}
PLAIN = {' ': 'spc', '.': 'dot', ',': 'comma', '-': 'minus', '=': 'equal', '/': 'slash',
         ';': 'semicolon', "'": 'apostrophe', '[': 'bracket_left', ']': 'bracket_right',
         '\\': 'backslash', '`': 'grave_accent'}


def key(c):
    if c.isalpha():
        return ('shift-' + c.lower()) if c.isupper() else c
    if c.isdigit():
        return c
    if c in SHIFTED:
        return 'shift-' + SHIFTED[c]
    if c in PLAIN:
        return PLAIN[c]
    raise SystemExit('no key for %r' % c)


def main():
    path, text = sys.argv[1], sys.argv[2]
    delay = float(sys.argv[3]) if len(sys.argv) > 3 else 0.06
    monitor = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    monitor.connect(path)
    for c in text:
        monitor.sendall(('sendkey %s\n' % key(c)).encode())
        time.sleep(delay)
    monitor.sendall(b'sendkey ret\n')
    time.sleep(0.2)
    monitor.close()


if __name__ == '__main__':
    main()
