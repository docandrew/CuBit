# Authorized log fan-out core

Run from the repository root:

```sh
nix develop -c bash tests/log-fanout/run.sh
```

The runner builds a Linux-hosted assertion-enabled test and runs GNATprove on
`Log_Fanout`, `Log_Budgets`, tag helpers, the pure authority-policy evaluator,
and ghost policy properties.
The live CuBit log service uses this core. This runner tests the core on Linux;
it does not boot the native adapter or build an ISO.

The regression test covers publisher-only subscription denial, zero caller
denial, stolen subscription handles, wrong-authority reads and closes,
independent subscriber queues, overflow reporting, retained history, stale
handles after close/reopen, distinct launch tags at the same PID, idempotent
subscription retries, lease renewal/expiration, and subscriber capacity exhaustion. Denied reads
return no event contents or loss counts and do not consume the victim's queue.

Budget tests cover distinct issuance tags sharing one pool, independent pools,
burst depletion, exact refill boundaries, repeated/backward/maximum time, and
a dense arrival trace bounded by burst plus elapsed refill credits. Proof covers
runtime safety and the exact one-credit Admit postcondition; the full temporal
rate bound remains regression tested. See the [budget contract](../../docs/typed-logging.md#shared-publication-budgets-deliberately-small-first-implementation).

GNATprove checks initialization and runtime safety of the isolated broker.
Separate ghost checks establish the Boolean policy intersection and bootstrap
rule equivalence. The assertion-enabled test exercises all 16 combinations of
request, installation approval, session approval, and issuer allowance.
These do not prove authentication of those inputs, resource-scope derivation,
noninterference, IPC caller authentication, concurrency safety, or the live log
service implementation. Read each proof invocation's output for its own summary.

## Security and integration boundary

- `Publish` is a trusted internal API. A native adapter must check publishing
  authority and stamp source identity/time; application-provided details remain
  untrusted claims.
- Subscription operations require an observer tag and matching owner/tag/handle.
  These arguments must come from authenticated IPC metadata. Guessing the
  numeric tag is not possession of an endpoint authority.
- Observer access currently means the whole diagnostic feed, including the
  retained snapshot. There are no topic scopes or per-event audience filters.
  A narrower observer must not be attached until filtering is implemented
  before queue insertion or recipient-readable memory mapping.
- Each subscriber has a bounded private queue. Overflow drops the oldest event
  and explicitly reports loss. This is diagnostic telemetry, not a lossless
  security audit trail. Counts saturate at the maximum representable value.
- Handles are not reused within a broker lifetime. Observer tags must be unique
  per issuance so PID reuse cannot inherit a subscription; procmgr now issues
  distinct publisher/observer tags. One subscription is allowed per owner/tag pair.
  Trusted monotonic-time advancement reclaims subscribers idle for 30 seconds;
  successful subscribe/read refreshes the lease, denied operations do not.
  The native loop advances time on requests and at one-second idle deadlines.
  Independent issuer/broker restart is not supported: reboot the system instead.
- The broker has one serialized owner. It is not a concurrent shared object.
  It copies small records and does not implement zero-copy grant streams or a
  negotiated stream-policy handshake.

## Native integration

`logstore.svc` validates kernel-stamped publishing/observing authority and exact
IPC headers, then acquires generation-checked, caller-owned grants. It copies
producer data before decoding and returns acquisitions before replying. Readers
provide their own writable pages; there is no shared service query buffer.

`logstore` requests publication only. `log-observer` requires both a manifest
request and trusted startup-plan approval. Ordinary OP_SPAWN cannot select that
approval. This is the development-image bootstrap policy, not a finished
installation policy or interactive permission broker. Observer authority covers
the whole feed. The old query/clear API and disabled auto-subscription experiment
have been removed rather than preserved as compatibility paths.

The shared `CuBit.Logging` client publishes asynchronously with one outstanding
page per publisher. The caller forwards its completion to release that page for
reuse. Busy/unavailable publication counts a local drop without waiting. Reader
operations are synchronous and intended for interactive observers, not hot service
paths. The shell's `logs` command uses that reader; clock publishes startup status.

Native regression commands:

```sh
nix develop -c make -C kernel logstore procmgr clock shell log-check
nix develop -c bash tests/headless/run.sh --test log-authority --accel kvm --timeout 25
```

The native test checks real grant-backed records, authenticated clock and test
publisher identities, local busy drops, collector overflow, forged tags,
publisher-only observation denial, observer-only publication denial, invalid
headers/records/grant generations, read-only output grants, stale handles, and
ordinary-launch denial of observer authority, rate limiting, forged budget tags,
and client recovery/drop accounting. Cross-owner handles, PID reuse,
lease expiry, and exhaustion are currently hosted core regressions, not native
lifecycle tests. Service restart, persistence, producer fairness, kernel boot-log
capture and broad service instrumentation remain follow-up work.
