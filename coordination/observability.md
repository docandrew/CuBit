# Observability architecture subtask

2026-10-01: authorized by user via compositor parent. Owns only
`docs/observability-streams.md` and this note. Read root AGENTS.md,
coordination/README.md and compositor/filesystem/networking/Servo notes.
Read-only audit of kernel Trace/scheduler/time, runtime Streams/Logging,
Log_Fanout, typed stream admission/lifetime and filesystem protocol.
No shared source edits, builds, staging, native tests, or build lock held.
Delivering architecture and phased implementation handoff; no claim a new
metrics service, transport, profiler or serial-free exporter already exists.

2026-10-01 deliverable complete: docs/observability-streams.md (architecture,
5 phased gates, explicit ownership handoffs). Local links/phase structure checked;
no code/proof/native-test claims. Important audit findings: kernel Trace already
has512events/CPU+histograms but serial export; current Streams writer services
subscription requests and reader lacks overwrite-consistency/gap validation.
Recommend new fixed-word atomic snapshot transport with SPARK sequential policy,
one collector attachment, overwrite-oldest event history, latest-value counters,
serial-free archive/export vertical slice before sampled stack profiler.
Kernel trace control authorization/global reset and SMP synchronization require
review; do not expose existing diagnostic syscall semantics as production API.
No shared source modifications or builds. Parent notified with document/handoffs.

## Observability implementation agent (owner of this note since 2026-10-01)

2026-10-01 15:40 ownership transferred by user via CCL agent: this note now
belongs to the observability implementation agent (metrics service, typed
logging catch-up, later tracing). History above is the architecture subtask's
and is kept verbatim. Read README.md and all notes before shared edits.

Owned (new files only unless stated):
- `docs/metrics-service.md` (new); `docs/observability-streams.md` (append an
  "Implementation status" section; architecture text kept intact)
- `userspace/services/metricsvc/` (new service `metrics.svc`)
- `userspace/runtime/gnat/cubit-metric_*.ad?` and `cubit-metrics.ad?` (new
  runtime units; native user_runtime auto-discovers them, so each is
  compile-checked natively in a private build workspace before relying on it)
- `tests/metrics/` (new hosted tests/proofs)
Not touching: procmgr, devmgr, kernel, catalogs' existing entries,
`userspace/ccl/src`, compositor/desktop sources, logstore (until planned
logsvc slice is announced here), sparktls/sparkentropy.
No live jobs, no lock held.

2026-10-01 16:40 metrics slice 1 DONE (hosted+proof), slice 2 code ready.
Hosted `nix develop -c bash tests/metrics/run.sh`: 24340 checks PASS; SPARK
level 1 161 checks, 0 unproved (records codec, two-page producer batcher,
histograms, store incl. ghost isolation post: a publication changes only the
caller's own (PID, issued tag) source). Isolation mutant killed, control ok.
New runtime units cubit-metric_{records,protocol,batches}.ad?, cubit-metrics.ad?
compiled privately with user_runtime flags (-gnatpg) in scratch: clean. They
are auto-discovered by the next user_runtime build. metricsvc + metrics-check
app compile natively in scratch (no link, no shared outputs touched).
Catalog: ADDITIVE lines only, `(service metrics 24 read-write)` and
`(service metrics-observer 25 read-write)`; manifest compiler verified
(app slots 24/29). Design: docs/metrics-service.md.

REQUEST to graphics agent (procmgr owner this round), needed for any native
metrics test: apply tests/metrics/procmgr-metrics-issuance.patch (101 lines,
procmgr/main.adb + cubit-authority_policy.ads; base sha256 main.adb 6b0a0733,
policy 46e0d584). Mirrors log tag issuance: per-launch non-wrapping
publisher/observer tags for roles 24/25, observer startup-only, observer
endpoint routed to the role-24 registered service. gcc -gnatc on patched
procmgr PASSES. I will not edit procmgr; reply here or via the user.
Pending (needs build lock, additive): kernel/Makefile targets `metricsvc`,
`metrics-check`; headless case `metrics` + tests/headless/init-metrics.ccl.

2026-10-01 17:20 logsvc slice (hosted) DONE: LogRecord v2 typed fields
(cubit-log_records, max wire 800 B; records without fields stay v1
byte-identical) + severity-filtered subscriptions (log_fanout, logstore
main, cubit-log_protocol comment, cubit-logging Subscribe Minimum default
Trace: source compatible). tests/log-fields 29923 PASS + level-1 proof 104/0;
tests/typed-logging PASS; tests/log-fanout PASS incl. new filter cases, all
6 level-2 proofs pass. gcc -gnatc of every Log_Records/Logging user (clock,
intel-gpu diagnostics, xhci, boot-logs, shell, log-check, log-retire, mesa
probe/log) PASS. NOTE for log-authority owners: existing log-check expects
Gap=3 after 19 records, i.e. a 16-record queue, but committed Log_Fanout
Capacity is 512, so that headless case likely already fails (not changed by me).
Now claiming additionally: userspace/services/logstore/{log_fanout,main}.adb/ads,
cubit-log_records.ad?, cubit-logging.ad? (Subscribe only), tests/log-fanout
(additive filter test), tests/log-fields, userspace/apps/{log-fields-check,
metrics-check}, tests/headless/init-{log-fields,metrics}.ccl.
ACTIVE: background retry (every 60 s, nonblocking) for ONE locked window:
additive kernel/Makefile targets + run.sh cases log-fields/metrics, then
make user_runtime logstore log-fields-check metricsvc metrics-check and
headless log-fields (TCG 4 CPU, 120 s). Logs /tmp/cubit-observability-*.

2026-10-01 16:06 native window TERMINAL, lock released. Under the lock:
added kernel/Makefile targets metricsvc, metrics-check, log-fields-check
(additive, after logstore) and run.sh cases log-fields/metrics (help line,
case list, init profile, image install block, markers; additive). Built
user_runtime logstore log-fields-check metricsvc metrics-check: PASS (all
linked; metrics.svc staged in isodir/boot only, not in world/images).
Headless log-fields TCG 4 CPU: PASS (serial
/tmp/cubit-observability-log-fields.serial): structured Warning record with
4 typed fields delivered intact to a Warning-filtered startup observer; the
Information record was filtered in logstore. metrics headless NOT run (blocked
on procmgr request above). No live jobs, no lock held.

REQUEST to graphics agent (after procmgr patch lands), first real producers:
desktop.svc owns one CuBit.Metrics.Publisher (manifest: request-service
metrics). At startup Put Describe records, e.g. key1 desktop.frame.render
(Latency, us), key2 desktop.input.to.present (Span, us, correlation = input
serial), key3 desktop.frames (Counter), key4 desktop.input.queue (Gauge).
Per frame: Put the values (no IPC); call Flush once per frame (or every N
frames) with a unique token; forward its CQE to CuBit.Metrics.Complete from
the existing completion loop. Never wait; Has_Room=false means flush or drop
(counted). Source timestamps must be producer time, not collector arrival.
No compositor file is edited by me; I can review a patch.
