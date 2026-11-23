# ACPI power inventory watch

Run the closed hosted policy/native-adapter suite:

```
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c env TMPDIR=/home/doc/cubit-build-tmp bash tests/acpi-power-watch/run.sh
```

The observer-badged operation is `ACPI_Power_Watch_Protocol.Watch` (label29).
Request `[nonzero ID,seen generation,0,0]`; reply `[ID,status,generation,1]`.
Seen0 obtains the initial generation without parking. Reissue using the returned
watch generation after reading power pages21/22. Auxiliary page reads still use
those pages' own cache generations. These counters are independent. IDs and
sender identities retain their full width. The generation fits signed CCL and
never wraps; exhaustion is explicit. Reconnect after service replacement.

Eight independent actual kernel senders can park a request. A replacement affects
only that sender, after acquiring its new saved reply. Denied/malformed/full/save
refusals retain earlier authorities. Save and Answer are trusted, serialized
transport boundaries; Answer consumes once even if its caller disappeared.
A watch neither runs AML nor creates a timer. The dispatcher resets its immediate
reply decision on each request. Stop drains replies at a quiet boundary; an
unsafe retained reply prevents successful native owner retirement.

Discovery records copied descriptors for batteries and every identified ACPI0003
node under the native scan's existing shared fuel/time/topology guards. Page21's
measurement remains the first selected AC source. A retained current-target
Notify80/81 is matched against the same Core's current device binding and routed
once before broker ACK. Node/path/Notify data is never invocation authority.
Relevant notices, completed initial/invalidated publications, observed power
value changes and isolated family failure advance the watch generation. A cache
may be temporarily unready immediately after a notice; the completed publication
advances it again. Unrelated targets/numbers and unchanged samples do not.

The old unserved query-label protocol is removed. Battery-cache codec metadata
is local to its existing generic observer; native reads remain Read_Metrics.

The standalone suite uses actual watch policy and native reply-adapter bodies,
with explicit bounded saved-reply/opaque-sender models. It covers authentication,
full-width inputs, spare/flag validation, independent holders, replacement/save
failure, capacity, unsafe boundaries, coalescing, caller loss, stop and exhaustion
in checked and release profiles. Passing runtime assertions are not a SPARK
proof, native IPC acceptance or hardware correctness claim.

The `events.asl` and `joined/` helpers additionally witness a private composition
run against the existing startup/shutdown actual-root harness. Seven fixed AML
producers cover battery80/81, both AC targets, unrelated/wrong notices and a PSR
change, in both AML widths. Eight saved watches are drained on normal stop and
through cached-only runtime-failure recovery. The kernel-call helper is a model;
no real handle or hardware grant is created. The packet receipt names the full
joined source/input/result hashes and retained platform overrides. These helpers
are supplemental inputs, not standalone-suite mains. Coordinator native and
power.svc client integration remain separate gates.
