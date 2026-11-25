#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Copy QEMU's serial output from a FIFO into the serial log.

With -serial file:LOG, QEMU writes each byte the guest sends from the vCPU
thread, holding its big lock; a write that waits on the host file system
(ext4 commits every 5 s) then holds back QEMU's main loop too, and with it
device timers such as the HDA model's DMA (tests/headless/run.sh, audio
tests). With -serial pipe:BASE QEMU writes into the FIFO BASE.out instead,
which never waits while this relay drains it into LOG; the relay's own
file-system waits are absorbed by the pipe buffer. The FIFO must itself be
in RAM (tmpfs): on ext4 even a FIFO write updates the inode's times, which
waits out a journal commit like a file write.

Usage: serial_relay.py FIFO LOG
Ends when QEMU closes the FIFO (its exit). The log is flushed per read, so
readers of LOG see the guest's lines as they arrive.
"""
import fcntl
import os
import sys

# Linux's F_SETPIPE_SZ, and the largest pipe an unprivileged process may ask
# for by default (/proc/sys/fs/pipe-max-size): about a second of a busy
# guest's console, far more than one host stall's worth.
F_SETPIPE_SZ = 1031
PIPE_BYTES = 1 << 20
READ_BYTES = 1 << 16


def main():
    fifo, log = sys.argv[1], sys.argv[2]
    with open(log, 'wb', buffering=0) as out:
        source = os.open(fifo, os.O_RDONLY)
        try:
            fcntl.fcntl(source, F_SETPIPE_SZ, PIPE_BYTES)
        except OSError:
            pass  # A smaller pipe still works; only its slack is less.
        while True:
            chunk = os.read(source, READ_BYTES)
            if not chunk:
                break
            out.write(chunk)
        os.close(source)
    return 0


if __name__ == '__main__':
    sys.exit(main())
