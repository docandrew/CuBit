# Bounded EC backend receive loop

Set TMPDIR before Nix, then run:

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-receive/run.sh
    nix develop --command bash tests/acpi-ec-receive/semantic.sh

The behavioral runner supports --source-root DIR and --validate-only. Its exact
36-source/22-compiled-unit closure includes the actual EC engine, catalog,
saved-reply, publication, wait, controller, receive child and native queue
adapters. Kernel queues, hardware/clock, reply callbacks and admission hooks are
modeled. Both checked/release profiles pass 544 checks in 36 runtime processes:
18 cases/profile, including Field/Serve, foreign query source, event fairness,
stop with a retained saved reply, unexpected completion, clock refusal, idle
and spurious wake, trusted control handling, hook exception/reentry, unknown
hardware result, normal/unknown publication and owner conflict. Tests are not
SPARK proofs or native execution. No AML/parser behavior changes or new ACPICA
execution comparison are claimed.

Instantiate Controller.With_Receive once per exclusively serialized backend.
Turn checks completions, handles at most one event, advances the existing
controller, then handles one typed service request. It never polls mixed IPC.
Runnable work and hardware waits prevent idle sleep; active waits use the
existing bounded microsecond pause and per-transaction budgets. Idle uses the
explicit Wait_Forever sentinel without converting clock epochs. Default event
and control hooks admit nothing. Trusted event hooks may submit only an
independently admitted source through Controller.Start_Query. Control hooks
may claim receipts/control requests; Handled=False must preserve the current
reply authority for Begin_Call and must not perform another receive. All hooks
must stay bounded, return normally and avoid reentry/blocking peer calls.

Serving faults/stops latch before another turn consumes queues. Stop requests
do not cancel already submitted hardware work. The process-lifetime controller,
backing, saved replies, reservations/Global Lock and unknown publication or
hardware recovery must remain owned. Returning from Serve does not authorize
process exit, lease release, reset or replay. A trusted lifecycle controller
must supervise drain/recovery separately. Counters reporting no pending reply
or no hardware recovery do not establish global quiescence. This library does
not register/launch a process, provision named-register capabilities, admit
interrupts, install AML providers or select native hardware startup.

The semantic runner instantiates default adapters/hooks against 222 actual
runtime specs/252 sources in both strict -gnatc profiles, with zero object or
binder output. native-semantics.json preserves that snapshot, not hardware
behavior or physical clock correctness. Full service/interpreter verification,
actual backend provisioning and real-device acceptance remain unfinished.
