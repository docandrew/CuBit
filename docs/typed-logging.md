# Typed diagnostic records and text-to-log adapter

Status: portable SPARK codec/adapter, hosted routing demonstration, and native
grant-backed logstore integration. `CuBit.Log_Protocol.Event` wraps this record
with authenticated immediate-peer identity, issued publisher tag, and collector
monotonic time. The old numeric event experiment and query/clear API are removed.
Kernel logging and CCL live routing are not migrated by this work.

## Diagnostic record contract

`CuBit.Log_Records.Contract` declares LogRecord schema version 2: bounded wire
size 800 bytes, with at most 512 UTF-8 text bytes and at most eight typed
fields (see [structured fields](#structured-fields-format-version-2)). A record
without fields still encodes as format version 1, byte for byte as before. Schema identity is the current local metadata
identifier `16#4355_424C_4F47_0001#`, not a provider identity or authority.

The producer supplies severity (trace, debug, information, warning, error,
critical), text, and one of three timestamp variants:

- Unspecified: no clock/domain/value.
- Monotonic milliseconds: positive clock-domain identifier and unsigned ticks.
- Unix milliseconds: unsigned milliseconds since the Unix epoch. Pre-epoch
  timestamps are outside this initial format; display/calendar range validation
  remains necessary before conversion to a calendar representation.

These are producer claims. No PID, signer, authority tag or trusted audit category
is accepted as an authenticated field in this record. A collector separately
records the actual authenticated immediate peer and its own observed time.
After text conversion that peer is the adapter. Preserving the original source
as authenticated provenance requires an additional attested/delegated provenance
mechanism; copying a source field is not enough. This record cannot be used as a
substitute for a trusted security-audit event.

### Wire layout

Offsets below are one-based. Multi-byte integers are little-endian. There is no
pointer, padding-dependent Ada record overlay, allocation, or host-endian cast.

| Bytes | Contents |
|---|---|
| 1–4 | ASCII `CLOG` |
| 5 | Format version 1 |
| 6 | Severity: 0–5 in the order above |
| 7 | Clock: 0 unspecified, 1 monotonic, 2 Unix |
| 8 | Reserved flags, zero |
| 9–10 | UTF-8 text byte length, 0–512 |
| 11–16 | Reserved, zero |
| 17–24 | Timestamp value; zero if unspecified |
| 25–32 | Monotonic clock domain; zero otherwise |
| 33 onward | Exactly the declared text bytes |

### Structured fields (format version 2)

A record may carry up to eight typed fields alongside its text, so viewers and
CCL can filter and chart values without re-parsing prose. Each field has a
1–16 byte name (lowercase ASCII letters, digits, `.`, `_`, `-`) and one kind:
signed integer (two's complement), unsigned integer, duration in microseconds,
or truth (0/1). Kinds map onto CCL integer and boolean values. Build with
`With_Field`; read with `Field_Total`, `Field_At`, `Name`, `Kind`, `Value` and
`Signed_Value`.

Version 2 differs from version 1 only as follows: byte 5 is 2; byte 11 is the
field count (1–8; version 1 requires 0, so the encoding stays canonical); then
`count × 32` bytes of fields precede the text.

| Field bytes | Contents |
|---|---|
| 1–16 | Name, zero padded; no zero byte inside the name |
| 17 | Kind: 1 signed, 2 unsigned, 3 duration µs, 4 truth |
| 18–24 | Reserved, zero |
| 25–32 | Value, little-endian (truth: 0 or 1) |

Field values are producer claims, like the text. Tests: `nix develop -c bash
tests/log-fields/run.sh` (wire vectors, limits, every malformed field form and
a canonical re-encoding fuzz); GNATprove level 1 proves absence of run-time
errors in the codec (104 checks). Round-trip behaviour is tested, not proved.

### Validation

The decoder rejects unsupported headers, reserved bits, invalid enum values,
inconsistent lengths, invalid clock/domain combinations, and malformed UTF-8.
UTF-8 validation rejects overlong encodings, surrogates, out-of-range scalars,
incomplete sequences, ASCII controls other than TAB, and DEL. Unicode content
remains untrusted presentation data: renderers must escape for their context and
must not treat log text as trusted identity or markup.

An encoder clears its whole output buffer; the transport sends only `Used` bytes.
A decoder ignores storage after `Used`, not bytes within the declared message.
A native adapter must bound an incoming length before conversion to `Wire_Count`
and provide stable copied or ownership-protected bytes during validation.
Failed decoding returns an error variant with no partially decoded record.

## Explicit text conversion

`CuBit.Text_To_Log.Input_Contract` describes bounded raw byte chunks of a UTF-8
text stream (maximum 512 bytes per chunk), not independently valid UTF-8 strings.
Chunk boundaries may split a scalar or CRLF pair. The pure adapter's byte-feed
API is an in-process loop over admitted chunks, not IPC per character.

- LF terminates a line; the immediately preceding CR is stripped for CRLF.
- Bare/internal CR and other forbidden controls produce an invalid-line report.
- Empty LF-terminated lines produce empty records.
- A line over 512 bytes is discarded through its terminator and reported once.
  It is not truncated and presented as a complete message.
- EOF flushes a final nonempty unterminated line. Incomplete UTF-8 or a trailing
  bare CR is reported; repeated EOF creates no phantom empty records.
- Severity is configured when constructing the adapter. Timestamp is supplied at
  line completion by the adapter host; it is not reconstructed producer time.

The host must process every emitted record or explicitly account for downstream
loss. This first codec/adapter does not implement transport backpressure, durable
audit storage, subscriber fan-out, sequence numbers, rate limits, or persistence.
A lossy upstream text stream must call `Report_Gap` before resuming. It returns
an `Upstream_Gap` loss report, clears partial data, and discards through the next
LF (or EOF) to resynchronize. It may conservatively lose a full line because
framing inside the missing bytes is unknown; it never joins surviving fragments
across the gap. The transport still must reliably report gaps and account for
lost chunks/bytes. An unnoticed upstream drop cannot be repaired by this adapter.

The adapter result is a definite private container around its internal variant.
This prevents callers passing a discriminant-constrained out parameter that
would fail when a different kind of emission occurs.

## Hosted flow demonstration

Run `nix develop -c bash tests/typed-logging/run.sh` from the repository root.
It builds/runs tests, runs the demo, then invokes GNATprove on the codec/adapter.

The demo uses the real admission and binding-lifecycle units to establish
text source → adapter and adapter → collector A. It transfers encoded records,
checks collector decoding, prepares collector B while A stays active, then
commits B. It rejects stale binding references, the wrong immediate producer,
and malformed records. The old transport cannot retire without acknowledgement.

This is **Linux-hosted**, using in-process fixture peers and bounded collector
arrays. Approvals, peer identities and time are supplied by the test harness;
they are not authenticated kernel IPC. It does not start logstore or establish
shared-memory transport. "Ready" and "quiescent" remain trusted fixture inputs.

SPARK checks initialization and absence of runtime errors in the selected units.
Codec round-trip behavior, wire compatibility and UTF-8 correctness are regression
tested, not yet universally proved. No end-to-end security or cryptographic claim
follows from those proofs. See [tests](../tests/typed-logging/README.md).

## Native service and client boundary

See [log fan-out tests](../tests/log-fanout/README.md) for native test commands and
the exact authorization boundary. Publishing uses an endpoint-bound, read-only
grant and asynchronous request. Observing requires a distinct approved endpoint,
a caller/tag-bound subscription, and a caller-owned writable output grant.
Only the native adapter supplies source identity, issuance tag and observed time.

Client objects own page-aligned buffers. Readers remain process-lived;
publishers must remain alive until explicit Disconnect reports Done (or exit).
Their event-loop owner serializes access. One publication may be outstanding;
additional emits drop locally and increment a saturating counter. No completion
queue is secretly drained by Emit: the application forwards matching completions.
A stalled collector leaves one pending page and subsequent emits drop. An explicit
`Rate_Limited` completion counts one drop and leaves the publisher ready for a
future emit; it never retries automatically. Other failed or ambiguous completions
disable that publisher; it does not reuse potentially
acquired memory. Independent collector restart/reconnection is not implemented.
Interactive readers block on RPC and are unsuitable for latency-sensitive paths.

Subscriptions take a minimum severity (request word 0, `Severity'Pos`; zero
means everything). Logstore filters before queueing, including the replay of
retained history, so a narrow observer does not copy or lose its queue to
low-severity floods: filtered records are not loss and never produce `Gap`.
A repeated Subscribe keeps the queue and updates the filter for later records.
Hosted tests: `tests/log-fanout` (filtered replay, filter-is-not-loss, retry).

### Explicit publisher disconnect and grant retirement

`Disconnect(Publisher, Done)` permanently stops new submissions from that object,
requests revocation and checks retirement. It does not wait for a service RPC.
While Done is false, retain the object and forward its original completion to
`Complete`, then call Disconnect again. Once Done is true, its page may leave
scope: its grant has retired and no publication completion remains outstanding.
The operation is idempotent and does not automatically retry a lost record,
discover another collector, reconnect, or revoke the caller's endpoint authority.
An unresponsive collector can leave Done false indefinitely; a timeout is not
permission to free the object. Keep it alive or let process teardown retire it.
Cancellation/rebinding of such outstanding work is a separate future protocol.
Kernel revocation still performs its normal locks/TLB synchronization; this is
not a wait-free operation or a hard real-time guarantee.

Two facts are deliberately separate. A `TARGET_DIED` completion is delivered
during mailbox retirement, before received grant mappings are fully torn down.
Likewise, successful Revoke may leave an acquisition pending. Neither alone
authorizes buffer reuse. Failed query results never count as completed retirement.

The existing owned-grant generation syscall now returns:

- `0`: caller-owned slot inactive, mapping retirement completed;
- a nonzero generation: active **or revocation pending**;
- `U64'Last`: invalid/foreign slot or query failure.

`CuBit.Memory_Grants.Retirement_Confirmed` accepts zero or a strictly newer valid
generation (grant generations do not wrap). Same/older generations and failures
are not confirmation. The query holds the existing grant lock; inactive state
is published only after mapping retirement and acknowledged TLB invalidation.
This concerns the specified owned reference, not arbitrary aliases or grants to
the same memory. Publishers keep their backing page private to their one grant.

SPARK ghost checks prove the pure result classification, not kernel concurrency
or TLB behavior. Native tests cover pending revocation with two acquisitions,
final return, reused slots, foreign-query denial, disconnect during publication,
and a test collector exiting while retaining an acquisition and unanswered call.
Producer death while a collector retains a page, automatic restart/rebinding,
issuer epochs and persistent quota state remain further lifecycle work.

Procmgr issues monotonically distinct tags without wrapping; exhaustion denies
issuance. Bootstrap observer approval requires trusted startup, in addition to
the requested manifest entry. An ordinary launch cannot request startup status.
Restarting procmgr or logstore independently needs an epoch/invalidation design
before it can be supported safely. It is not equivalent to an ordinary app restart.

These are bounded copied diagnostics, not the future zero-copy streaming data
plane or durable security audit. The ring holds 16 records, with at most eight
observers and explicit overflow counts. Producer flooding can evict other logs;
per-producer fairness, durable storage and richer viewer UX remain work.

### Shared publication budgets: deliberately small first implementation

Procmgr assigns every publication issuance to the **same bootstrap diagnostic
pool** today. This is a conservative system-wide development allocation, not an
inferred package identity or a finished per-deployment policy. Copies, restarts
of producer apps, and additional handles share the pool instead of multiplying
the allowance. The pool survives producer exits; independent logstore restart
remains unsupported, as above.

The kernel-stamped publisher tag contains a pool ID (1–15) and a nonzero 28-bit
issuance identity. Observer tags retain their format. Procmgr fails issuance
closed at the publication identity limit, without wrapping. Apps cannot choose
their pool in a request or by changing the authorityTag message field. The pure
limiter supports independent pools for future trusted policy assignments; the
current issuer always selects pool 1. No pool IDs are hashed from app-supplied
names or self-asserted package identities.

Each pool starts with 64 record credits and refills one credit per 100 ms, capped
at 64. A record is at most 800 wire bytes, so one record-credit also bounds bytes
without a second accounting dimension. These are tunable development constants,
not performance targets. Trusted monotonic time drives refill; backwards time
does not create credits, and long idle intervals cannot accumulate above burst.

After authority and IPC-header validation, admission spends one credit **before**
grant acquisition or payload decoding. Malformed payloads/grants do not refund
credits. When empty, logstore returns `Rate_Limited` with zero words, touches no
grant, and inserts no record. There is no waiting queue or retry timer. The shared
native client treats this as best-effort loss; a lower-level caller may surface
the explicit error instead. Durable audit must not use this discard contract.

The limiter keeps saturating rejected-attempt counters per pool; they are not yet
exposed through an observer IPC operation or UI. Client `Dropped` counts local
busy/failure and rate-limited losses. Subscriber `Gap` is distinct: it reports
accepted records subsequently evicted from that subscriber's bounded queue.

This limits admitted decode/copy work and publication volume, **not** the cost of
receiving rejected IPC or per-producer fairness. One producer can still spend the
shared allowance and evict another producer's diagnostics. Separate approved
allocations, fair scheduling, and transport abuse limits remain later work; no
hierarchical quotas, runtime policy language, or per-process registry is added.

`Log_Budgets` is a portable SPARK core. Proof covers runtime safety and exact
credit spending for Admit. Sustained-rate behavior, tag mapping, pool isolation,
and clock edge cases are regression tested, not claimed universally proved.
Native QEMU tests exercise rate-limit replies, forged pool tags, client drop
accounting, and successful publication after refill.

## Viewing logs from the CCL REPL (2026-10-01)

`(logs.recent "netstack")` in the CCL Workbench returns a typed `List<LogEntry>`:
- `LogEntry` is `(time, severity, source, message)`, and `severity` is a `Severity` variant (`userspace/ccl/interfaces/logs.schema`).
- The result is ordinary CCL data, for example `(where (fn ((e LogEntry)) (= (field e severity) Severity.Error)) (logs.recent "netstack"))`.
- The argument is a service-catalog name (`CuBit.Service_Names`) or a decimal process number.

**How it works:**
- **Subscribe** carries a source filter in word 1 (`Every_Source` = 0). A new subscription replays only that process's retained records, so one query is subscribe, drain, close.
- **Result size:** one result carries up to `CCL.Interfaces.Logs.MAX_ENTRIES` (42) entries; when they don't all fit, the newest are kept.
- **Host boundary:** lists of records cross it as typed images (`CCL.Objects` sequences), which the interpreter and the VM copy into their own regions.

**Status:**
- **Hosted:** the interpreter path is tested (`tests/ccl-type-discovery/log_view_tests.adb`), as is logstore's source filter (`tests/log-fanout`).
- **Native:** the Workbench builds.
- **Not yet live:** procmgr approves log observation only for `boot-logs.app`. The Workbench needs the same transitional approval; I've asked the graphics agent, who owns procmgr.
- **Later:** typed log fields in entries, live tailing (a subscription left open), a table view in the Workbench and Observatory, and persistence to disk.
