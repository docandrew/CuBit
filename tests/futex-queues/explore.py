#!/usr/bin/env python3
"""Exhaustive interleaving explorer for CuBit futex protocols (docs/threads.md).

Models the kernel's FUTEX_WAIT and FUTEX_WAKE at the granularity of the real
implementation (Process.Futex): WAIT takes the bucket lock, loads the user
word, and either enqueues (sleeping) or returns, releasing the lock; WAKE
takes the same lock and removes up to N waiters oldest-first. User code runs
atomic instructions (CAS, XCHG, load, store) that interleave freely with the
kernel steps; user stores never take the bucket lock.

Every interleaving of small thread programs is explored. Checked:
  * mutual exclusion (the futex mutex Rust's std uses);
  * no deadlock or lost wakeup: every final state has all threads finished;
    a state where no thread can move but some are unfinished is a failure;
  * join (the THREAD_EXIT protocol: the kernel clears the exit word, then
    wakes it) never strands the joiner.

Mutants that must be caught (the explorer is only trusted if it finds them):
  * a kernel WAIT that compares and enqueues without holding the bucket lock
    across both (a wake can slip in between);
  * an unlock that forgets to wake;
  * a join word cleared after the wake instead of before.
"""
import sys
from collections import deque

# --- program representation -------------------------------------------------
# Instructions (thread-local register file r0..r3):
#   ('cas', addr, expected, new, reg)
#   ('xchg', addr, value, reg)
#   ('load', addr, reg)
#   ('store', addr, value)
#   ('jeq', reg, value, target) / ('jne', reg, value, target) / ('jmp', target)
#   ('wait', addr, expected_or_reg)     kernel FUTEX_WAIT, expanded below
#   ('wake', addr, count)               kernel FUTEX_WAKE, expanded below
#   ('enter',) ('leave',)               critical section markers
#   ('done',)

def expand(program, locked_wait=True):
    """Expand kernel calls into their atomic steps, relinking jump targets."""
    out, where = [], []
    for ins in program:
        where.append(len(out))
        if ins[0] == 'wait':
            if locked_wait:
                out += [('bucket_lock',), ('wait_check_enqueue_unlock',) + ins[1:]]
            else:
                # Mutant: compare, then enqueue in a separate step, no lock.
                out += [('wait_check',) + ins[1:], ('wait_enqueue',) + ins[1:]]
        elif ins[0] == 'wake':
            out += [('bucket_lock',), ('wake_unlock',) + ins[1:]]
        else:
            out.append(ins)
    where.append(len(out))
    fixed = []
    for ins in out:
        if ins[0] in ('jeq', 'jne'):
            ins = ins[:3] + (where[ins[3]],)
        elif ins[0] == 'jmp':
            ins = (ins[0], where[ins[1]])
        fixed.append(ins)
    return fixed

# --- state -------------------------------------------------------------------
# state = (mem tuple, lock_owner or -1, queue tuple of (tid, addr),
#          threads tuple of (pc, regs tuple, sleeping bool), in_cs tuple)

def explore(programs, memory, check_mutex=False, limit=5_000_000):
    progs = programs
    n = len(progs)
    start = (tuple(memory), -1, (), tuple((0, (0, 0, 0, 0), False) for _ in progs))
    seen = {start}
    work = deque([start])
    explored = 0
    while work:
        state = work.popleft()
        explored += 1
        if explored > limit:
            raise RuntimeError("state space limit")
        mem, owner, queue, threads = state
        moved = False
        inside = [i for i in range(n)
                  if threads[i][0] < len(progs[i]) and progs[i][threads[i][0]][0] == 'leave']
        if check_mutex and len(inside) > 1:
            return False, explored, f"mutual exclusion violated by threads {inside}"
        for i in range(n):
            pc, regs, sleeping = threads[i]
            if sleeping or pc >= len(progs[i]) or progs[i][pc][0] == 'done':
                continue
            nxt = step(progs[i][pc], i, mem, owner, queue, threads)
            if nxt is None:
                continue          # blocked on the bucket lock
            moved = True
            if nxt not in seen:
                seen.add(nxt)
                work.append(nxt)
        if not moved:
            unfinished = [i for i in range(n)
                          if threads[i][0] < len(progs[i]) and progs[i][threads[i][0]][0] != 'done']
            if unfinished:
                return False, explored, (f"stuck: threads {unfinished} cannot finish "
                                         f"(sleeping={[t[2] for t in threads]}, mem={mem})")
    return True, explored, "ok"

def step(ins, i, mem, owner, queue, threads):
    pc, regs, sleeping = threads[i]
    mem = list(mem); regs = list(regs); threads = list(threads)
    op = ins[0]
    new_pc = pc + 1

    def val(x):
        return regs[int(x[1:])] if isinstance(x, str) else x

    if op == 'cas':
        _, a, e, v, r = ins
        regs[r] = mem[a]
        if mem[a] == e:
            mem[a] = v
    elif op == 'xchg':
        _, a, v, r = ins
        regs[r], mem[a] = mem[a], v
    elif op == 'load':
        _, a, r = ins
        regs[r] = mem[a]
    elif op == 'store':
        _, a, v = ins
        mem[a] = v
    elif op in ('jeq', 'jne'):
        _, r, v, t = ins
        if (regs[r] == v) == (op == 'jeq'):
            new_pc = t
    elif op == 'jmp':
        new_pc = ins[1]
    elif op in ('enter', 'leave'):
        pass
    elif op == 'bucket_lock':
        if owner != -1:
            return None
        owner = i
    elif op == 'wait_check_enqueue_unlock':
        _, a, e = ins
        assert owner == i
        owner = -1
        if mem[a] == val(e):
            queue = queue + ((i, a),)
            threads[i] = (new_pc, tuple(regs), True)
            return (tuple(mem), owner, queue, tuple(threads))
    elif op == 'wait_check':            # mutant, unlocked
        _, a, e = ins
        if mem[a] != val(e):
            new_pc = pc + 2             # skip the enqueue: EAGAIN
    elif op == 'wait_enqueue':          # mutant, unlocked
        _, a, e = ins
        queue = queue + ((i, a),)
        threads[i] = (new_pc, tuple(regs), True)
        return (tuple(mem), owner, queue, tuple(threads))
    elif op == 'wake_unlock':
        _, a, count = ins
        assert owner == i
        owner = -1
        kept, woken = [], 0
        for (t, qa) in queue:
            if qa == a and woken < count:
                tp, tr, _ = threads[t]
                threads[t] = (tp, tr, False)
                woken += 1
            else:
                kept.append((t, qa))
        queue = tuple(kept)
    else:
        raise ValueError(op)
    threads[i] = (new_pc, tuple(regs), False)
    return (tuple(mem), owner, queue, tuple(threads))

# --- protocols ---------------------------------------------------------------
M = 0          # mutex word
W = 1          # join (exit) word

def mutex_thread(rounds, unlock_wakes=True):
    """Rust std futex mutex: 0 unlocked, 1 locked, 2 contended."""
    p = []
    for _ in range(rounds):
        base = len(p)
        L_WAIT, L_CS = base + 5, base + 8
        p += [
            ('cas', M, 0, 1, 0),        # base+0
            ('jeq', 0, 0, L_CS),        # +1 uncontended
            ('jeq', 0, 2, L_WAIT),      # +2 already contended: wait
            ('xchg', M, 2, 0),          # +3
            ('jeq', 0, 0, L_CS),        # +4 got it
            ('wait', M, 2),             # +5 L_WAIT
            ('xchg', M, 2, 0),          # +6
            ('jne', 0, 0, L_WAIT),      # +7
            ('enter',),                 # +8 L_CS
            ('leave',),                 # +9
            ('xchg', M, 0, 1),          # +10 unlock
            ('jne', 1, 2, base + 13),   # +11 no waiters
            ('wake', M, 1) if unlock_wakes else ('jmp', base + 13),  # +12
        ]
    p.append(('done',))
    return p

def joiner():
    # loop { v = load W; if v == 0 break; wait(W, v) }
    return [('load', W, 0), ('jeq', 0, 0, 4), ('wait', W, 'r0'), ('jmp', 0), ('done',)]

def exiting_thread(clear_first=True):
    # THREAD_EXIT: the kernel clears the exit word, then wakes it.
    if clear_first:
        return [('store', W, 0), ('wake', W, 1 << 30), ('done',)]
    return [('wake', W, 1 << 30), ('store', W, 0), ('done',)]

def run(name, programs, memory, expect_ok, locked_wait=True, check_mutex=False):
    progs = [expand(p, locked_wait) for p in programs]
    ok, explored, why = explore(progs, memory, check_mutex)
    verdict = "PASS" if ok == expect_ok else "FAIL"
    kind = "holds" if expect_ok else "caught"
    detail = why if not ok else ""
    print(f"explore: {name}: {verdict} ({explored} states; property "
          f"{'holds' if ok else 'violated'}{': ' + detail if detail and not expect_ok else ''})")
    return verdict == "PASS"

def main():
    results = [
        run("mutex 2 threads x 2 rounds", [mutex_thread(2), mutex_thread(2)],
            [0, 1], True, check_mutex=True),
        run("mutex 3 threads x 1 round", [mutex_thread(1)] * 3,
            [0, 1], True, check_mutex=True),
        run("mutex 3 threads x 2 rounds", [mutex_thread(2)] * 3,
            [0, 1], True, check_mutex=True),
        run("join, 1 joiner", [joiner(), exiting_thread()], [0, 1], True),
        run("join, 2 joiners", [joiner(), joiner(), exiting_thread()], [0, 1], True),
        # Mutants: each must be caught.
        run("mutant: unlocked kernel wait (join)", [joiner(), exiting_thread()],
            [0, 1], False, locked_wait=False),
        run("mutant: unlocked kernel wait (mutex)", [mutex_thread(1)] * 3,
            [0, 1], False, locked_wait=False, check_mutex=True),
        run("mutant: unlock without wake", [mutex_thread(1, unlock_wakes=False)] * 2,
            [0, 1], False, check_mutex=True),
        run("mutant: exit word cleared after wake", [joiner(), exiting_thread(False)],
            [0, 1], False),
    ]
    if all(results):
        print("explore: PASS")
        return 0
    print("explore: FAIL")
    return 1

if __name__ == "__main__":
    sys.exit(main())
