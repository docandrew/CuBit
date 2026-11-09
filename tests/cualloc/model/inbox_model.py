#!/usr/bin/env python3
"""Exhaustive interleaving check of CuAlloc's remote-free protocol
(userspace/allocator/process/cualloc.adb: Push_Node, Take_All, Drain,
Thread_Exit, Adopt_Heap). SPARK does not analyse threads outside Jorvik,
so this is the evidence for the protocol; it is a bounded model check,
not a proof for every size.

Model (one heap H, sequentially consistent atomics: on x86-64 the CAS and
the exchange are locked instructions, full barriers, and plain stores are
not reordered with each other, TSO):
  * Pushers: each pushes its nodes to H's inbox, one at a time, as
    Push_Node does: load head; write node.next := head; CAS(head, node),
    retrying from the load on failure.
  * Holders: O starts holding H (it made it). A holder may take the whole
    inbox (exchange with 0) and walk the taken list node by node, reading
    each node's next word, at any time; at exit it takes and walks once
    more, then, under the directory lock, marks H abandoned. Thread A may,
    under the lock, adopt H when it is abandoned, then do the same.
Checked in every reachable state:
  * at most one thread holds H; only the holder takes;
  * no node is delivered (walked) twice;
  * a node's next word never changes between its take and its walk;
  * at the end: every pushed node was delivered exactly once, or is still
    in the inbox list exactly once (H then abandoned: the next adopter
    takes it), and the inbox list is acyclic.
Mutants (--mutant NAME) break one step; each must be caught by a
property violation (reaching a bound does not count): blind-store (a plain
store instead of the CAS), split-take (take as a load, then a store of 0),
adopt-held (adopt without checking the heap is abandoned), drop-on-exit
(take at exit, abandon without releasing what was taken).
Bounds: at most MAX_STATES states and MAX_DEPTH steps per path; a run that
reaches one reports "bound reached" and fails. Configurations used:
(pushers, nodes per pusher, holders, takes before exit) = (2,2,2,2),
(3,1,2,2), (2,3,2,1); the largest explores about 0.47 M states.
Not modelled: the batching of frees into nodes and the node's return
(sequential code in one thread; tests/cualloc/remote_twice.adb and the
thread stress cover it), weak memory beyond x86-64 TSO.
"""
import argparse
import sys

NIL = 0

def initial(pushers, nodes_each, holders):
    # Shared: head, next words, state ('held'/'abandoned'), holder.
    nexts = {}
    node = 1
    plan = []
    for _ in range(pushers):
        mine = []
        for _ in range(nodes_each):
            nexts[node] = NIL
            mine.append(node)
            node += 1
        plan.append(tuple(mine))
    # Pusher local: (index of node being pushed, pc, loaded head)
    push = tuple((0, 'load', NIL) for _ in range(pushers))
    # Holder local: (phase, takes done, list being walked as tuple of
    # (node, next seen at take time), ...)
    hold = tuple(('holding' if i == 0 else 'waiting', 0, ()) for i in range(holders))
    return (NIL, tuple(sorted(nexts.items())), 'held', 0, push, hold, frozenset(), tuple(plan))


def list_from(head, nexts):
    seen, out = set(), []
    while head != NIL:
        if head in seen:
            return None  # cycle
        seen.add(head)
        out.append(head)
        head = nexts[head]
    return out


def successors(state, mutant, max_takes):
    head, nexts_t, hstate, holder, push, hold, delivered, plan = state
    nexts = dict(nexts_t)
    out = []

    def mk(**kw):
        d = dict(head=head, nexts=nexts, hstate=hstate, holder=holder, push=push, hold=hold,
                 delivered=delivered)
        d.update(kw)
        return (d['head'], tuple(sorted(d['nexts'].items())), d['hstate'], d['holder'], d['push'],
                d['hold'], d['delivered'], plan)

    # Pushers.
    for i, (k, pc, seen) in enumerate(push):
        if k >= len(plan[i]):
            continue
        node = plan[i][k]
        if pc == 'load':
            out.append(('push%d load' % i, mk(push=push[:i] + ((k, 'link', head),) + push[i + 1:])))
        elif pc == 'link':
            n2 = dict(nexts)
            n2[node] = seen
            out.append(('push%d link' % i, mk(nexts=n2, push=push[:i] + ((k, 'cas', seen),) + push[i + 1:])))
        elif pc == 'cas':
            if mutant == 'blind-store':
                # Store instead of CAS: a concurrent push or take is lost.
                out.append(('push%d store' % i, mk(head=node, push=push[:i] + ((k + 1, 'load', NIL),) + push[i + 1:])))
            elif head == seen:
                out.append(('push%d cas ok' % i, mk(head=node, push=push[:i] + ((k + 1, 'load', NIL),) + push[i + 1:])))
            else:
                out.append(('push%d cas retry' % i, mk(push=push[:i] + ((k, 'load', NIL),) + push[i + 1:])))

    # Holders.
    for t, (phase, takes, walking) in enumerate(hold):
        def setme(p, tk, w):
            return hold[:t] + ((p, tk, w),) + hold[t + 1:]
        if phase == 'waiting':
            # Adopt, under the lock: only an abandoned heap.
            if hstate == 'abandoned' or mutant == 'adopt-held':
                out.append(('h%d adopt' % t, mk(hstate='held', holder=t, hold=setme('holding', 0, ()))))
        elif phase in ('holding', 'exiting'):
            if walking:
                node, next_at_take = walking[0]
                if nexts[node] != next_at_take:
                    return None, 'node %d next changed after its take (%d -> %d)' % (node, next_at_take, nexts[node])
                if node in delivered:
                    return None, 'node %d delivered twice' % node
                out.append(('h%d walk %d' % (t, node), mk(delivered=delivered | {node}, hold=setme(phase, takes, walking[1:]))))
                continue
            if takes < max_takes or phase == 'exiting':
                if holder != t and mutant != 'adopt-held':
                    return None, 'thread %d takes without holding' % t
                if mutant == 'split-take':
                    # Load, then (later) store 0: two steps.
                    lst = list_from(head, nexts) or []
                    after = 'storing-exit' if phase == 'exiting' else 'storing'
                    out.append(('h%d take(load)' % t, mk(hold=setme(after, takes, tuple((n, nexts[n]) for n in lst)))))
                else:
                    lst = list_from(head, nexts)
                    if lst is None:
                        return None, 'inbox list has a cycle'
                    walk = tuple((n, nexts[n]) for n in lst)
                    nxt_phase = 'abandoning' if phase == 'exiting' else phase
                    out.append(('h%d take' % t, mk(head=NIL, hold=setme(nxt_phase, takes + 1, walk))))
            if phase == 'holding':
                out.append(('h%d exit' % t, mk(hold=setme('exiting', takes, ()))))
        elif phase in ('storing', 'storing-exit'):
            after = 'abandoning' if phase == 'storing-exit' else 'holding'
            out.append(('h%d take(store 0)' % t, mk(head=NIL, hold=setme(after, takes + 1, walking))))
        elif phase == 'abandoning':
            if walking and mutant == 'drop-on-exit':
                # Take at exit, then abandon without releasing what was taken.
                out.append(('h%d abandon, dropping' % t, mk(hstate='abandoned', holder=-1, hold=setme('done', takes, ()))))
            elif walking:
                node, next_at_take = walking[0]
                if nexts[node] != next_at_take:
                    return None, 'node %d next changed after its take' % node
                if node in delivered:
                    return None, 'node %d delivered twice' % node
                out.append(('h%d walk %d' % (t, node), mk(delivered=delivered | {node}, hold=setme(phase, takes, walking[1:]))))
            else:
                out.append(('h%d abandon' % t, mk(hstate='abandoned', holder=-1, hold=setme('done', takes, ()))))
    return out, None


def check_final(state):
    head, nexts_t, hstate, holder, push, hold, delivered, plan = state
    nexts = dict(nexts_t)
    pushed = {n for i, mine in enumerate(plan) for n in mine[:push[i][0]]}
    lst = list_from(head, nexts)
    if lst is None:
        return 'final inbox list has a cycle'
    if len(set(lst)) != len(lst):
        return 'node twice in the inbox'
    if set(lst) & delivered:
        return 'node both delivered and in the inbox'
    if set(lst) | delivered != pushed:
        return 'lost nodes: %s' % sorted(pushed - set(lst) - delivered)
    if lst and hstate != 'abandoned' and all(h[0] == 'done' for h in hold):
        return 'nodes left with no one to drain them'
    return None


# Hard bounds: the search never grows past these (run it under a memory
# cap too: systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0).
MAX_STATES = 1_000_000
MAX_DEPTH = 2_000


def explore(pushers, nodes_each, holders, max_takes, mutant):
    start = initial(pushers, nodes_each, holders)
    seen = {start}
    stack = [(start, 0)]
    finals = 0
    while stack:
        s, depth = stack.pop()
        if len(seen) >= MAX_STATES:
            return False, 'bound reached at %d states' % len(seen), len(seen)
        if depth >= MAX_DEPTH:
            return False, 'bound reached at depth %d (%d states)' % (depth, len(seen)), len(seen)
        holders_now = [t for t, h in enumerate(s[5]) if h[0] in ('holding', 'exiting', 'abandoning', 'storing', 'storing-exit')]
        if len(holders_now) > 1:
            return False, 'two holders at once', len(seen)
        succ, err = successors(s, mutant, max_takes)
        if err:
            return False, err, len(seen)
        if not succ:
            finals += 1
            err = check_final(s)
            if err:
                return False, err, len(seen)
        for _, n in succ:
            if n not in seen:
                seen.add(n)
                stack.append((n, depth + 1))
    return True, '%d final states' % finals, len(seen)


def main():
    p = argparse.ArgumentParser()
    p.add_argument('--mutant', default=None)
    a = p.parse_args()
    # Bounds (pushers, nodes per pusher, holders, takes before exit); each
    # finishes well inside MAX_STATES (the largest, ~0.5 M states).
    configs = [(2, 2, 2, 2), (3, 1, 2, 2), (2, 3, 2, 1)]
    if a.mutant:
        caught = []
        for c in configs[:2]:
            ok, why, n = explore(*c, a.mutant)
            caught.append(not ok and not why.startswith('bound reached'))
            print('mutant %s, config %s: %s (%s, %d states)' % (a.mutant, c, 'CAUGHT' if caught[-1] else 'missed', why, n))
        sys.exit(0 if any(caught) else 1)
    # A mutant counts as caught only by a property violation, not a bound.
    for c in configs:
        ok, why, n = explore(*c, None)
        print('pushers=%d nodes each=%d holders=%d takes=%d: %s, %s, %d states' % (c + ('PASS' if ok else 'FAIL', why, n)))
        if not ok:
            sys.exit(1)


if __name__ == '__main__':
    main()
