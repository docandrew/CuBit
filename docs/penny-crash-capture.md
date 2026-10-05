# Penny crash capture in the fast desktop launcher

`make -C kernel run-desktop-fast` now captures QEMU serial output on the host via
`tools/run_logged_qemu.py`. No guest filesystem permission is added. It works for
process faults and aborts even when Penny cannot write a file itself.

The launcher prints `kernel/desktop-logs/run-…` at startup and exit. Each run holds:

- `serial.log` and, after rotation, `serial.previous.log`: at most 8 MiB each.
- `crash.log`: the first recognized fatal marker, up to 512 KiB of preceding
  context and 256 KiB from that point onward. Later spam cannot overwrite it.
- `run.json`: actual QEMU command, staged Penny/Desktop and boot ISO hashes,
  host logger PID, and exit code when the launcher exits normally.

The current run and two older completed runs are retained; live runs are excluded
from pruning. `kernel/serial_output.log` links to the current serial segment.
An existing regular serial log is saved as `previous-launch.log` (last 8 MiB)
before replacing that compatibility path. Logs are ignored by Git. Copy a relevant
run elsewhere before starting enough additional sessions to age it out.

This is diagnostic capture, not a browser-crash fix or a core dump. Fatal markers
currently cover Penny abort, Rust panic, user memory fault and kernel panic. A
hang without such a marker remains in the rolling serial logs. Guest output can
already be interleaved; capture preserves those bytes rather than inventing a
stack trace. Hashes identify staged artifacts; they do not include debug symbols.

Validation: Nix-hosted `tests/servo/test_crash_capture.py` checks split markers,
rotation, first-crash preservation, exit status, artifact hashing and run retention.

Penny now routes stderr through its existing kernel diagnostics channel instead
of the unread guest stderr stream. This includes native library diagnostics that
can explain an abort. Its regular logger avoids sending the same message twice.
Other file descriptors keep their libc behavior. The native fixture requires a
stderr marker to prove the link interception works in the guest.
