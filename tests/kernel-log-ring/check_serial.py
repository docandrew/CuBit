#!/usr/bin/env python3
"""LOG-004: check a CCL_CONSOLE_DEMO=kernel-log serial log.

check_serial.py SERIAL_LOG [--overflow]

- logstore's catch-up summary accounts for every number the kernel gave:
  records taken + records dropped = the last sequence number read.
- (logs.recent "crash-probe") holds the runtime's last words,
  "unhandled exception PROGRAM_ERROR: main.adb:25 explicit raise" and its
  traceback (EXC-001), at Severity.Error under
  crash-probe's own source (its log channel: the kernel holds no program
  text).
- (logs.recent "fault-check") and (logs.recent "stack-probe") hold the
  summary of the kernel's typed report, "stopped by a page fault (write);
  details restricted", Severity.Error, under each program's own source,
  with no address, offset or register in it; never "segmentation fault".
- Their details (where: the null page, a stack overflow) are withheld from
  the store: logstore says on the console that it kept each one out (its
  catch-up summary counts those withheld by then, usually none: the
  probes fault after it).
- (first 4 (logs.recent "kernel")) holds kernel records (source
  Kernel_Source = 2**24 - 1). Without --overflow: none were dropped, and
  the oldest is from before logstore started. With --overflow (a kernel
  built with KERNEL_LOG_RECORDS=64): the oldest is the dropped marker,
  "kernel log: N records dropped (sequence 1..N)", with N as logstore's
  summary counted it, N > 0.
"""
import re
import sys

KERNEL_SOURCE = 2 ** 24 - 1
SUMMARY = re.compile(
    r"logstore: kernel log caught up: (\d+) records taken, (\d+) dropped, "
    r"through sequence (\d+); earliest t=(\d+)ms, logstore started t=(\d+)ms; "
    r"(\d+) restricted withheld")
ENTRY = re.compile(r'\(LogEntry (\d+) Severity\.(\w+) (\d+) "((?:[^"\\]|\\.)*)"\)')
REPL = "ccl-console: REPL completed: List<LogEntry>: "
# EXC-001: the exception's name and message, then "; traceback 0x..."
LAST_WORDS = "unhandled exception PROGRAM_ERROR: main.adb:25 explicit raise"
SUMMARY_TEXT = "{}: stopped by a page fault (write); details restricted"
WITHHELD = "logstore: kernel log: withheld the restricted fault detail of {} ({})"



def fail(message):
    print(f"kernel-log-ring: FAIL {message}", file=sys.stderr)
    sys.exit(1)


def find(lists, predicate, what):
    for entries in lists:
        for entry in entries:
            if predicate(entry):
                return entry
    fail(f"no record: {what}")


def main():
    if len(sys.argv) not in (2, 3) or (len(sys.argv) == 3 and sys.argv[2] != "--overflow"):
        fail("usage: check_serial.py SERIAL_LOG [--overflow]")
    overflow = len(sys.argv) == 3
    text = open(sys.argv[1], "rb").read().decode("latin-1")
    if "segmentation fault" in text.lower():
        fail('"segmentation fault" on the console')

    found = SUMMARY.search(text)
    if not found:
        fail("no logstore catch-up summary")
    taken, dropped, through, earliest, started, _ = map(int, found.groups())
    if taken + dropped != through:
        fail(f"inexact account: {taken} taken + {dropped} dropped != sequence {through}")
    print(f"kernel-log-ring: catch-up {taken} taken + {dropped} dropped = sequence {through}; "
          f"earliest t={earliest}ms, logstore started t={started}ms")

    results = [line[line.index(REPL) + len(REPL):] for line in text.splitlines() if REPL in line]
    lists = [[(int(t), level, int(source), message) for t, level, source, message in ENTRY.findall(r)]
             for r in results]

    fault_summaries = {name: SUMMARY_TEXT.format(f"com.cubit.{name}")
                       for name in ("fault-check", "stack-probe")}
    for label, predicate in (
            ("crash-probe's last words", lambda e: e[3].startswith(LAST_WORDS + "; traceback 0x")),
            ("fault-check's fault summary", lambda e: e[3] == fault_summaries["fault-check"]),
            ("stack-probe's fault summary", lambda e: e[3] == fault_summaries["stack-probe"])):
        _, level, source, message = find(lists, predicate, label)
        if level != "Error":
            fail(f"{label} is Severity.{level}, not Error")
        if source == KERNEL_SOURCE:
            fail(f"{label} is under the kernel's source, not the program's")
        print(f"kernel-log-ring: {label} under source {source}: {message}")

    # Nothing a probe's log reader gets names an address, offset or register.
    for name in ("fault-check", "stack-probe"):
        for entries in lists:
            for _, _, _, message in entries:
                if message.startswith(f"com.cubit.{name}:") and re.search(r"0x|rip|rsp", message):
                    fail(f"{name}'s log holds a detail: {message}")
    # The details exist, and stayed out of the store.
    for name, where in (("fault-check", "null page"), ("stack-probe", "stack overflow")):
        line = WITHHELD.format(f"com.cubit.{name}", where)
        if line not in text:
            fail(f'no "{line}"')
        print(f"kernel-log-ring: {line}")

    kernel = next((entries for entries in lists
                   if entries and all(source == KERNEL_SOURCE for _, _, source, _ in entries)), None)
    if kernel is None:
        fail('(first 4 (logs.recent "kernel")) returned no kernel records')
    oldest_time, _, _, oldest = kernel[0]
    if overflow:
        marker = f"kernel log: {dropped} records dropped (sequence 1..{dropped})"
        if dropped == 0 or oldest != marker:
            fail(f'oldest kernel record is "{oldest}", expected "{marker}"')
        print(f"kernel-log-ring: overflow marker: {marker}")
    else:
        if dropped != 0:
            fail(f"{dropped} records dropped with the full-size ring")
        if oldest_time >= started:
            fail(f'oldest kernel record "{oldest}" at t={oldest_time}ms is not before '
                 f"logstore started (t={started}ms)")
        print(f'kernel-log-ring: kernel record from before logstore: t={oldest_time}ms "{oldest}"')
    print("kernel-log-ring: PASS")


if __name__ == "__main__":
    main()
