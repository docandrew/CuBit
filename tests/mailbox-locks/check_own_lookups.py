#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""No own-table lookup while another process's mailbox lock is held.

The system call handlers look up the caller's capabilities with helpers
(findOwn, ownCap, schedulingAuthority, and whatever calls them) that take
the caller's mailbox lock. Called while the handler holds a target
process's mailbox lock, that is a second mailbox lock out of order
(IDIOT-001): with the caller as target the spinlock refuses the re-entry
and the kernel halts; two processes naming each other deadlock, two CPUs
spinning with interrupts off. Look the caller's authority up first, then
lock the target and re-check what can change (its generation).

Checks the system call units (syscall*.adb, process-ipc.adb,
kernel_object_calls.adb): within each subprogram, between
enterCriticalSection (mailtab(X).lock) for a process X other than the
caller and its exit, no call to a lock-taking lookup, directly or through
a subprogram of the same file.

Usage: check_own_lookups.py [KERNEL_SRC_DIR]. Exit 1 on a violation.
"""
import re
import sys
from pathlib import Path

src = Path(sys.argv[1] if len(sys.argv) > 1 else
           Path(__file__).resolve().parents[2] / "kernel/src")
header = re.compile(r"^\s*(?:overriding\s+)?(?:procedure|function)\s+(\w+)\b", re.I)
enter = re.compile(r"enterCriticalSection\s*\(\s*(?:Process\.)?mailtab\s*\(\s*(\w+)\s*\)\s*\.lock", re.I)
leave = re.compile(r"exitCriticalSection\s*\(\s*(?:Process\.)?mailtab\s*\(\s*(\w+)\s*\)\s*\.lock", re.I)
pair_lock = re.compile(r"(?<![\w.])lockMailboxes\s*\(", re.I)
pair_unlock = re.compile(r"(?<![\w.])unlockMailboxes\s*\(", re.I)
# The caller's own mailbox, as the handlers name it.
CALLER = ("callerpid", "caller", "mypid")
# Lookups that take a mailbox lock of the process they are given.
BASE = {"findown", "owncap", "schedulingauthority", "resolveendpointhandle"}
# Where system calls are handled.
UNITS = ("syscall*.adb", "process-ipc.adb", "kernel_object_calls.adb")


def code_lines(path):
    return [line.split("--", 1)[0] for line in path.read_text(errors="replace").splitlines()]


def bodies(lines):
    """name -> list of (start, end) line ranges of its body (overloads and
    same-named nested subprograms in different handlers included)."""
    found = {}
    for i, line in enumerate(lines):
        m = header.match(line)
        if not m:
            continue
        name = m.group(1)
        # Outside the parameter list: ';' ends a declaration, 'is' starts
        # a body (or an expression function, renaming or instance: skipped).
        j, depth, kind = i, 0, None
        while j < len(lines) and kind is None:
            line_j = lines[j] if j > i else lines[j][m.end():]
            for token in re.finditer(r"\(|\)|;|\bis\b", line_j, re.I):
                t = token.group().lower()
                if t == "(":
                    depth += 1
                elif t == ")":
                    depth -= 1
                elif depth == 0:
                    if t == ";":
                        kind = "declaration"
                    else:
                        rest = line_j[token.end():] + " " + " ".join(lines[j + 1:j + 2])
                        kind = ("other" if re.match(r"\s*(new\b|\(|separate\b)", rest, re.I)
                                else "body")
                    break
            if kind is None:
                j += 1
        if kind != "body":
            continue
        end = re.compile(r"^\s*end\s+" + re.escape(name) + r"\s*;", re.I)
        for k in range(j, len(lines)):
            if end.match(lines[k]):
                found.setdefault(name, []).append((i, k))
                break
    return found


def calls(text, name):
    # A call with arguments, or a parameterless call statement.
    return re.search(r"(?<![\w.])" + re.escape(name) + r"\s*(\(|;)", text, re.I) is not None


def resolve(subs, name, line):
    """The bodies a call of name at line reaches: the ones nested in the
    smallest body around line that declares one (handlers reuse local names
    such as performLocked), else every body of that name in the file."""
    spans = subs.get(name, [])
    around = [(s, e) for spans_ in subs.values() for s, e in spans_ if s <= line <= e]
    for s, e in sorted(around, key=lambda r: r[1] - r[0]):
        inner = [(a, b) for a, b in spans if s < a and b < e]
        if inner:
            return inner
    return spans


failed = False
for unit in sorted({u for pattern in UNITS for u in src.glob(pattern)}):
    lines = code_lines(unit)
    subs = bodies(lines)
    names = list(subs)
    # Lock-taking spans: the own-table lookups, then any body that calls
    # one (resolved by scope), to a fixed point.
    base = BASE
    taking = {(n, s, e) for n, spans in subs.items() if n.lower() in base for s, e in spans}

    def call_takes(name, k):
        if name.lower() in base:
            return True
        return any((name, a, b) in taking for a, b in resolve(subs, name, k))

    changed = True
    while changed:
        changed = False
        for n, spans in subs.items():
            for s, e in spans:
                if (n, s, e) in taking:
                    continue
                for k in range(s + 1, e):
                    if any(calls(lines[k], m) and call_takes(m, k) for m in names + list(base)
                           if m != n):
                        taking.add((n, s, e))
                        changed = True
                        break
    for n, spans in subs.items():
        for s, e in spans:
            held = []
            for k in range(s + 1, e):
                line = lines[k]
                m = enter.search(line)
                if m:
                    held.append(m.group(1))
                    continue
                m = leave.search(line)
                if m and held:
                    held.pop()
                    continue
                # Two mailboxes in order (lockMailboxes): a lookup in between
                # takes one of them again.
                if pair_lock.search(line) and n != "lockMailboxes":
                    held.append("<two mailboxes>")
                    continue
                if pair_unlock.search(line) and n != "unlockMailboxes" and held:
                    held.pop()
                    continue
                others = [h for h in held if h.lower() not in CALLER]
                if not others:
                    continue
                for t in sorted(set(names) | {"findOwn", "ownCap", "schedulingAuthority",
                                              "resolveEndpointHandle"}):
                    if t != n and calls(line, t) and call_takes(t, k):
                        print(f"MAILBOX-LOCKS: FAIL {unit.name}:{k + 1} ({n}): {t} while "
                              f"holding mailtab({others[-1]}).lock: {line.strip()}")
                        failed = True
if failed:
    sys.exit(1)
print("MAILBOX-LOCKS: PASS no own-table lookup while another process's mailbox lock is held")
