#!/usr/bin/env python3
"""No submit path or queue path waits (GPU-001 steps 1 and 2).

Fragment harness over the exact production sources. It checks that the
synchronous wrapper's handler (Handle_Application_Submission), the queue's
channel and wake handlers, every callback main.adb gives the queue service,
the queue service's Turn and Submit_Call with everything they run, the
ledger's observation and the native ring writer contain no unbounded loop
and no waiting primitive; that reply capabilities are saved before any
publication; that the reply slots are distinct; and that the loop paces GPU
work in flight with a 1 ms monotonic sleep. Mutation controls inject a wait
into copies of each fragment and must be caught.

Bounded 'for' loops (copying a segment, visiting the sessions) are not
waits; a wait is a 'while' loop, a bare 'loop', or one of the calls below.
This is a source-structure check, not a proof of timing or hardware behaviour.
"""
from pathlib import Path
import re

root = Path(__file__).resolve().parents[2]
service = root / 'userspace/services/intel-gpu'
main = (service / 'main.adb').read_text()
queue = (service / 'intel_gpu_queue_service.adb').read_text()
ledger = (service / 'intel_gpu_context_ledger.adb').read_text()
native = (service / 'intel_gpu_native_queue_ring.adb').read_text()

# Calls that wait for the GPU, the GuC or time.
WAITS = [
    r'\bContext_Wait\s*\.\s*Execute\b', r'\bInitial_Wait\b', r'\bApp_Wait\b',
    r'\.\s*Wait\s*\(', r'\bWait_For_Activity_Until\b', r'\breceiveUntil\b',
    r'\bSYSCALL_SLEEP\w*', r'\bGuC_Pause\b', r'\bPause\s*;', r'\bService_Initial_Events\b',
    r'\bSleep_While_GPU_Busy\b', r'\bcapCall\b', r'\bPark_Resident_Contexts\b',
]
FOR_LOOP = re.compile(r'\bfor\b[^;]*?\bloop\b', re.IGNORECASE | re.S)
LOOP = re.compile(r'\b(loop|while)\b', re.IGNORECASE)


def strip_comments(text: str) -> str:
    return '\n'.join(line.split('--', 1)[0] for line in text.splitlines())


def body(text: str, kind: str, name: str) -> str:
    """The first completion (not a forward declaration) of kind name."""
    for start in re.finditer(rf'^\s*{kind} {name}\b', text, re.M):
        depth, i = 0, start.end()
        while i < len(text):
            c = text[i]
            if c == '(':
                depth += 1
            elif c == ')':
                depth -= 1
            elif depth == 0 and c == ';':
                break  # a declaration without a body
            elif depth == 0 and re.match(r'\bis\b', text[i:i + 3]) and text[i - 1].isspace():
                if re.match(r'is\s*\(', text[i:]):
                    break  # an expression function
                end = text.index(f'end {name};', i)
                return text[start.start():end + len(f'end {name};')]
            i += 1
    raise SystemExit(f'FAIL: {kind} {name} body not found')


def violations(fragment: str) -> list[str]:
    code = FOR_LOOP.sub('for-bounded', strip_comments(fragment))
    found = [m.group(0) for m in LOOP.finditer(code)
             # 'end loop' closes a loop already reported (or a bounded one)
             if not code[max(0, m.start() - 4):m.start()].endswith('end ')]
    for pattern in WAITS:
        found += re.findall(pattern, code)
    return found


checks = 0


def require_no_wait(label: str, fragment: str) -> None:
    global checks
    bad = violations(fragment)
    if bad:
        raise SystemExit(f'FAIL: {label} waits or loops: {bad}')
    checks += 1


def require_caught(label: str, fragment: str, injected: str) -> None:
    global checks
    # Inject just before the fragment's final 'end <name>;'.
    cut = fragment.rindex('   end ')
    mutated = fragment[:cut] + injected + fragment[cut:]
    if not violations(mutated):
        raise SystemExit(f'FAIL: mutation not caught in {label}: {injected.strip()}')
    checks += 1


handler = body(main, 'procedure', 'Handle_Application_Submission')
fragments = {
    # main.adb: handlers and every callback the queue service calls.
    'Handle_Application_Submission': handler,
    'Handle_Channel_Open': body(main, 'procedure', 'Handle_Channel_Open'),
    'Handle_Channel_Close': body(main, 'procedure', 'Handle_Channel_Close'),
    'Handle_GPU_Wake': body(main, 'procedure', 'Handle_GPU_Wake'),
    'Select_Session': body(main, 'function', 'Select_Session'),
    'Write_Segment': body(main, 'procedure', 'Write_Segment'),
    'Kick': body(main, 'procedure', 'Kick'),
    'Quarantine_Session': body(main, 'procedure', 'Quarantine_Session'),
    'Call_Finished': body(main, 'procedure', 'Call_Finished'),
    'Answer_Wake': body(main, 'procedure', 'Answer_Wake'),
    'Ensure_Context': body(main, 'procedure', 'Ensure_Context'),
    'Read_Submission_Marker': body(main, 'procedure', 'Read_Submission_Marker'),
    # The queue service: everything Turn and Submit_Call run.
    'Turn': body(queue, 'procedure', 'Turn'),
    'Serve_Session': body(queue, 'procedure', 'Serve_Session'),
    'Admit': body(queue, 'procedure', 'Admit'),
    'Pop_Ready': body(queue, 'procedure', 'Pop_Ready'),
    'Send_Kicks': body(queue, 'procedure', 'Send_Kicks'),
    'Kick_One': body(queue, 'procedure', 'Kick_One'),
    'Fail_Core': body(queue, 'procedure', 'Fail_Core'),
    'Answer_Held_Wake': body(queue, 'procedure', 'Answer_Held_Wake'),
    'Publish': body(queue, 'procedure', 'Publish'),
    'Write_Status': body(queue, 'procedure', 'Write_Status'),
    'Submit_Call': body(queue, 'procedure', 'Submit_Call'),
    'Wake_Request': body(queue, 'procedure', 'Wake_Request'),
    'Ledger.Observe': body(ledger, 'procedure', 'Observe'),
    'Native_Queue_Ring.Write': body(native, 'procedure', 'Write'),
}
for label, fragment in fragments.items():
    require_no_wait(label, fragment)

# The handler publishes through Submit_Call and returns; the loop answers.
if 'Queue_Service.Submit_Call' not in handler or 'Queue_Service.Turn' in handler:
    raise SystemExit('FAIL: handler must Submit_Call and leave completion to the loop')
if 'saveReplyCap (Unsigned_64 (Submission_Reply_Slot))' not in handler:
    raise SystemExit('FAIL: handler must save the reply capability before publishing')
if handler.index('saveReplyCap') > handler.index('Queue_Service.Submit_Call'):
    raise SystemExit('FAIL: reply capability saved after publication')
wake = fragments['Handle_GPU_Wake']
if 'saveReplyCap (Unsigned_64 (Wake_Reply_Slot))' not in wake:
    raise SystemExit('FAIL: a held wake must save its reply capability')
checks += 4

# Reply slots are distinct.
slots = dict(re.findall(r'^\s*(\w+_Reply_Slot) : constant CapabilitySlot := (\d+);', main, re.M))
if len(set(slots.values())) != len(slots) or not {'Submission_Reply_Slot', 'Wake_Reply_Slot'} <= set(slots):
    raise SystemExit(f'FAIL: reply slots collide or are missing: {slots}')
checks += 1

# GPU work in flight is paced by a monotonic sleep of at most 1 ms per turn,
# never Wait_For_Activity_Until (which returns at once while a deferred
# request stays queued).
pace = body(main, 'procedure', 'Sleep_While_GPU_Busy')
if 'SYSCALL_SLEEP_UNTIL_MONOTONIC_MICROSECOND' not in pace or 'Wait_For_Activity' in pace:
    raise SystemExit('FAIL: GPU work in flight must sleep on the monotonic clock')
poll_us = re.search(r'Submission_Poll_Us : constant Unsigned_64 := ([\d_]+);', main)
poll_ms = re.search(r'Submission_Poll_Ms : constant Unsigned_64 := ([\d_]+);', main)
if not poll_us or int(poll_us.group(1).replace('_', '')) > 1000 or \
   not poll_ms or int(poll_ms.group(1).replace('_', '')) > 1:
    raise SystemExit('FAIL: in-flight sleep exceeds 1 ms')
loop_start = main.index('   Submit_Budget_Query;\n   loop')
loop_text = main[loop_start:]
for step in ('Service_Context_Events;', 'Process_Retained_Events;', 'Queue_Service.Turn (Queue_State);',
             'Process_Pending_Quarantines;', 'Report_Submit_Latency;', 'Sleep_While_GPU_Busy;',
             'Queue_Service.Arm_Wake_Words (Queue_State)'):
    if step not in loop_text:
        raise SystemExit(f'FAIL: service loop lacks {step}')
# Parking is refused while GPU work is in flight.
park = body(main, 'procedure', 'Park_Resident_Contexts')
if 'if not GPU_Park_Allowed then' not in park:
    raise SystemExit('FAIL: Park_Resident_Contexts must check GPU_Park_Allowed first')
checks += 4

# The application ring writer acts on the selected application context:
# its ownership and coherence predicates are Submission_Work_Owner, as step
# 1's application ring. The boot render context's Live_* predicates need
# that context runnable and refused every application write on the NUC
# (2026-10-10).
inst = re.search(r'package Queue_Ring is new Intel_GPU_Native_Queue_Ring\s*\(([^;]*)\);', main)
if not inst:
    raise SystemExit('FAIL: Queue_Ring instantiation not found')
actuals = [a.strip() for a in inst.group(1).split(',')]
if actuals != ['Selected_CPU_Base', 'Selected_Backing_Bytes', 'Submission_Work_Owner',
               'Submission_Work_Owner']:
    raise SystemExit(f'FAIL: Queue_Ring must use the application context predicates: {actuals}')
checks += 1
for bad in ('Live_Coherent_Ready', 'Live_Backing_Owner'):
    mutated = main.replace(inst.group(0), inst.group(0).replace(
        'Submission_Work_Owner);', bad + ');'), 1)
    m = re.search(r'package Queue_Ring is new Intel_GPU_Native_Queue_Ring\s*\(([^;]*)\);', mutated)
    if [a.strip() for a in m.group(1).split(',')] == actuals:
        raise SystemExit(f'FAIL: wiring mutation not caught: {bad}')
    checks += 1

# Mutation controls: each injected wait must be caught.
injections = ('      while not Done loop null; end loop;\n',
              '      loop exit when Done; end loop;\n',
              '      Context_Wait.Execute (Contexts, ID, Context_Life.Enable, 1, Status);\n',
              '      Application_Completion.Wait (Attempt, 1, Status);\n',
              '      Ignored := syscall (SYSCALL_SLEEP, 1);\n',
              '      GuC_Pause;\n')
mutated_fragments = ('Handle_Application_Submission', 'Turn', 'Admit', 'Submit_Call',
                     'Handle_GPU_Wake', 'Native_Queue_Ring.Write')
for label in mutated_fragments:
    for injected in injections:
        require_caught(label, fragments[label], injected)

print(f'Submit and queue no-wait PASS: {checks} checks; {len(fragments)} fragments, reply slots, '
      f'1 ms monotonic pacing, park refused in flight, '
      f'{len(mutated_fragments) * len(injections)} mutations caught')
