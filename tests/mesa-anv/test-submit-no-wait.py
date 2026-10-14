#!/usr/bin/env python3
"""GPU-001 step 3 source check: Mesa's submit path makes no blocking call.

From the queue callbacks (anv_cubit_queue_exec_locked/_async) through
everything they reach in anv_cubit_memory.c and anv_cubit_sync.c, and from
Native_GPU_Queue.Submit and the event loop's wake (Arm_Idle_Wake,
Collect_Wake) through everything they reach in their own body,
CuBit.GPU_Sessions.Submit, CuBit.GPU_Queue_Clients.Submit and
CuBit.Channels.Kick: no device IPC (cubit_intel_*), no blocking call
(capCall), no wait (queue waits, vk_sync waits, condition waits, sleeps
other than the queue's own bounded backpressure look), no lifetime_mutex,
no unbounded loop in the Ada path.

Two named exceptions on the internal-batch callback, both before its
descriptor: the one-time context preparation at device creation
(prepare_submission) and the pre-lock dependency hook
(anv_cubit_wait_dependencies), which waits only for a signal not submitted
yet and never under the device mutex.

Then every mutation below (one injected wait or call each) must be caught.
Linux-hosted text analysis; it proves nothing about the kernel or driver.
"""
from pathlib import Path
import re
import shutil
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[2]
C_FILES = ('userspace/mesa/anv/anv_cubit_memory.c', 'userspace/mesa/anv/anv_cubit_sync.c')
C_ROOTS = ('anv_cubit_queue_exec_locked', 'anv_cubit_queue_exec_async')
C_EXEMPT = {'prepare_submission', 'anv_cubit_wait_dependencies'}
C_FORBIDDEN = re.compile(
    r'\b(cubit_intel_\w+|cubit_gpu_queue_wait|cubit_gpu_queue_open|vk_sync_wait\w*|'
    r'pthread_cond_\w*wait|usleep|nanosleep|sleep|wait_values|wait_cpu_retirement)\s*\(|'
    r'&lifetime_mutex')
ADA = {
    'userspace/mesa/anv/native_gpu_queue.adb': ('Submit', 'Arm_Idle_Wake', 'Collect_Wake'),
    'userspace/runtime/gnat/cubit-gpu_sessions.adb': ('Submit', 'Arm_Wake', 'Complete_Wake'),
    'userspace/runtime/gnat/cubit-gpu_queue_clients.adb': ('Submit',),
    'userspace/runtime/gnat/cubit-channels.adb': ('Kick',),
}
ADA_FORBIDDEN = re.compile(
    r'\b(capCall|Native_GPU_Calls|Wait_Reached|Poll_Reached|Wait_For_Activity\w*|'
    r'Wait_Event|Receive\w*|SYSCALL_WAIT\w*|SLEEP_UNTIL\w*)\b')
# The one sleep allowed: Native_GPU_Queue.Submit's bounded backpressure look.
ADA_SLEEP = re.compile(r'SYSCALL_SLEEP\b')
BOUNDED_LOOK = 'for Look in 0 .. Room_Budget_Ms / Room_Pause_Ms loop'


def c_functions(text):
    """name -> body of every function defined in a C file."""
    result = {}
    for match in re.finditer(r'^(?:static\s+)?[A-Za-z_][\w \t\*\n]*?\b(\w+)\s*\(([^;{}]*)\)\s*\{',
                             text, re.M):
        name, depth, i = match.group(1), 1, match.end()
        while depth and i < len(text):
            depth += {'{': 1, '}': -1}.get(text[i], 0)
            i += 1
        result[name] = text[match.end():i]
    return result


def check_c(root):
    functions = {}
    for name in C_FILES:
        functions.update(c_functions((root / name).read_text()))
    problems, seen, todo = [], set(), list(C_ROOTS)
    while todo:
        name = todo.pop()
        if name in seen or name in C_EXEMPT:
            continue
        seen.add(name)
        body = functions.get(name)
        if body is None:
            problems.append('C root/callee not found: ' + name)
            continue
        for hit in C_FORBIDDEN.finditer(body):
            problems.append(f'{name}: {hit.group(0).strip()}')
        for callee in re.findall(r'\b(\w+)\s*\(', body):
            if callee in functions:
                todo.append(callee)
    for root_name in C_ROOTS:
        if root_name not in seen:
            problems.append('C root missing: ' + root_name)
    return problems, len(seen)


def ada_subprograms(text):
    """name -> list of bodies (subprogram bodies in a package body)."""
    result = {}
    for match in re.finditer(r'^\s*(?:procedure|function)\s+(\w+)\s*(?:\([^()]*\))?\s*'
                             r'(?:return\s+[\w.]+\s*)?is\b(?!\s*\()', text, re.M | re.S):
        name = match.group(1)
        end = re.search(r'^\s*end\s+' + name + r'\s*;', text[match.end():], re.M)
        if end:
            result.setdefault(name, []).append(text[match.end():match.end() + end.start()])
    return result


def check_ada(root):
    problems, count = [], 0
    for name, roots in ADA.items():
        bodies = ada_subprograms((root / name).read_text())
        seen, todo = set(), list(roots)
        while todo:
            sub = todo.pop()
            if sub in seen:
                continue
            seen.add(sub)
            if sub not in bodies:
                problems.append(f'{name}: {sub} not found')
                continue
            for body in bodies[sub]:
                count += 1
                for hit in ADA_FORBIDDEN.finditer(body):
                    problems.append(f'{name}: {sub}: {hit.group(0)}')
                sleeps = ADA_SLEEP.findall(body)
                if sleeps and not (name.endswith('native_gpu_queue.adb') and sub == 'Submit'
                                   and BOUNDED_LOOK in body and len(sleeps) == 1):
                    problems.append(f'{name}: {sub}: sleep outside the bounded backpressure look')
                if re.search(r'^\s*loop\b', body, re.M) or re.search(r'\bwhile\b', body):
                    problems.append(f'{name}: {sub}: unbounded loop')
                for callee in re.findall(r'\b([A-Z]\w*)\s*\(', body):
                    if callee in bodies and callee not in ('Submit',) + tuple(roots):
                        todo.append(callee)
    return problems, count


def check(root):
    c, c_count = check_c(root)
    ada, ada_count = check_ada(root)
    return c + ada, c_count, ada_count


MUTATIONS = [
    ('C: device IPC in the locked submit', 'userspace/mesa/anv/anv_cubit_memory.c',
     '   const cubit_endpoint endpoint = record->tracker.endpoint;\n   if (!record->queue_admitting',
     '   const cubit_endpoint endpoint = record->tracker.endpoint;\n   (void)cubit_intel_session_status(endpoint);\n   if (!record->queue_admitting'),
    ('C: lifetime_mutex in exec_locked', 'userspace/mesa/anv/anv_cubit_memory.c',
     '   (void)perf_query_pass;\n',
     '   (void)perf_query_pass;\n   pthread_mutex_lock(&lifetime_mutex); pthread_mutex_unlock(&lifetime_mutex);\n'),
    ('C: queue wait in queue_submit', 'userspace/mesa/anv/anv_cubit_memory.c',
     '   const char *why = NULL;\n',
     '   const char *why = NULL;\n   (void)cubit_gpu_queue_wait(record->tracker.endpoint, 0, 1, 0);\n'),
    ('C: completion wait before signalling', 'userspace/mesa/anv/anv_cubit_memory.c',
     '   for (uint32_t l = 0; l < list_count; l++) {\n',
     '   (void)vk_sync_wait_many(&device->vk, wait_count, waits, 0, UINT64_MAX);\n   for (uint32_t l = 0; l < list_count; l++) {\n'),
    ('C: sleep in batch_reference', 'userspace/mesa/anv/anv_cubit_memory.c',
     '   uint64_t parent_offset = offset;\n',
     '   uint64_t parent_offset = offset;\n   usleep(1000);\n'),
    ('C: condition wait in add_point', 'userspace/mesa/anv/anv_cubit_sync.c',
     '   if (t->bound && !cubit_endpoint_same(t->endpoint, endpoint)) {',
     '   (void)pthread_cond_wait(&sync_changed, &sync_mutex);\n   if (t->bound && !cubit_endpoint_same(t->endpoint, endpoint)) {'),
    ('C: a wait in resolve', 'userspace/mesa/anv/anv_cubit_sync.c',
     '   native_gpu_timeline_resolve(&t->state, &completed);',
     '   native_gpu_timeline_resolve(&t->state, &completed);\n   (void)cubit_gpu_queue_wait(t->endpoint, 0, 1, 0);'),
    ('C: the dependency hook inside exec_locked', 'userspace/mesa/anv/anv_cubit_memory.c',
     '   struct batch_ref batch;\n   if (cmd_buffer_count) {',
     '   struct batch_ref batch;\n   (void)vk_sync_wait_many(&device->vk, wait_count, waits, 0, 0);\n   if (cmd_buffer_count) {'),
    ('Ada: a call in Submit', 'userspace/mesa/anv/native_gpu_queue.adb',
     '         Decode (Item.all, Decoded, Valid);',
     '         Decode (Item.all, Decoded, Valid);\n         Ignore := CuBit.Messages.capCall (0, Request, 0);'),
    ('Ada: a wake in Reap_All', 'userspace/mesa/anv/native_gpu_queue.adb',
     '         exit when not Got;',
     '         exit when not Got;\n         Sessions.Wait_Reached (State.Session, 0, 1, 0, Result);'),
    ('Ada: unbounded backpressure', 'userspace/mesa/anv/native_gpu_queue.adb',
     BOUNDED_LOOK, 'loop'),
    ('Ada: a second sleep in Submit', 'userspace/mesa/anv/native_gpu_queue.adb',
     '         State.Last (Decoded.Context) := Value;',
     '         Ignore := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_SLEEP, 1);\n'
     '         State.Last (Decoded.Context) := Value;'),
    ('Ada: blocking kick', 'userspace/runtime/gnat/cubit-channels.adb',
     'Ignore := CuBit.Messages.capSubmit (C.Endpoint, M, CuBit.Messages.NO_COMPLETION_TOKEN);',
     'Ignore := Boolean (CuBit.Messages.capCall (C.Endpoint, M, CuBit.Messages.Wait_Forever).label = 0);'),
    ('Ada: a wait in the session submit', 'userspace/runtime/gnat/cubit-gpu_sessions.adb',
     '      Clients.Submit (S.Client, Item, Tag, Signal, Submitted, Kick);',
     '      Clients.Submit (S.Client, Item, Tag, Signal, Submitted, Kick);\n'
     '      Poll_Reached (S, 0, 1, 0, Result);'),
    ('Ada: a loop in the client submit', 'userspace/runtime/gnat/cubit-gpu_queue_clients.adb',
     '      if not Can_Submit (C) then\n         return;\n      end if;',
     '      while not Can_Submit (C) loop\n         null;\n      end loop;'),
]


def main():
    problems, c_count, ada_count = check(ROOT)
    if problems:
        print('FAIL:\n  ' + '\n  '.join(problems))
        return 1
    caught = 0
    for label, name, old, new in MUTATIONS:
        work = Path(tempfile.mkdtemp(prefix='no-wait.'))
        try:
            for relative in set(C_FILES) | set(ADA):
                (work / relative).parent.mkdir(parents=True, exist_ok=True)
                shutil.copy(ROOT / relative, work / relative)
            text = (work / name).read_text()
            if text.count(old) != 1:
                print('STALE mutation: ' + label)
                return 1
            (work / name).write_text(text.replace(old, new))
            found, _, _ = check(work)
            if not found:
                print('SURVIVED mutation: ' + label)
                return 1
            caught += 1
        finally:
            shutil.rmtree(work)
    print(f'Submit no-wait PASS: {c_count} C functions and {ada_count} Ada bodies reached, '
          f'no IPC, wait, sleep or lifetime_mutex; {caught}/{len(MUTATIONS)} injected waits caught')
    return 0


if __name__ == '__main__':
    sys.exit(main())
