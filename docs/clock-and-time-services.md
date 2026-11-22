# Native time: UTC, local presentation and future synchronization

## Current boundary

`clock.svc` keeps the existing monotonic-millisecond operation unchanged. The
new `CuBit.Clocks.Read` runtime client requests a wall-clock snapshot through
the separately declared clock endpoint (slot 25). It returns UTC seconds,
local civil fields, the UTC offset in seconds, and an explicit quality enum.
Calling the runtime client does not grant clock authority.

During boot, devmgr reads the CMOS RTC through a narrowly minted 0x70/0x71 I/O
capability. It checks update-in-progress, two matching snapshots, battery-valid,
BCD/binary and 12/24-hour encodings, and calendar validity. Retries are bounded.
This initial implementation assumes the RTC contains **UTC, in 2000..2099**;
it does not infer local time or use an ACPI century register. An invalid sample
leaves wall time unavailable rather than inventing a date or blocking the boot.

After loading persisted Config data, devmgr atomically replaces
`clock.boot-sample` with this boot's UTC/monotonic pair (or `unavailable`). Config
denies writes to `clock.boot-*` by ordinary clients even when their normal ACL
allows the broader prefix. The existing Config administrators remain trusted.
Clock reads this seed once and advances it using monotonic elapsed time. RTC
precision, oscillator drift and suspend/resume are not solved by this anchor.

The taskbar renders HH:MM. It queries at most once per ten seconds, aligns a
query with the next minute, and only repaints when the displayed value changes.
`--:--` means unavailable; `TZ?` means the configured zone could not be used.
Current valid time has quality `RTC_Only`, **not network-synchronized**.

## Timezone configuration

The system CCL configuration supplies, for example:

```lisp
(setting "clock.time-zone" "America/Denver")
```

Both shipped defaults use `UTC`. The external setting is an IANA name so it is
readable and portable; the service resolves it to a generated Ada `Time_Zone`
enum. Unknown names fail explicitly. Changing display timezone never changes
UTC or monotonic time. Clock needs only read access to the `clock.` Config scope.

The Nix-pinned tzdata is compiled on Linux into enum names and shared, bounded
transition tables for 2000..2099, including daylight-saving transitions and
fractional-hour offsets. Nothing parses TZif files on the native service path.
Aliases retain distinct names but share identical rule tables. Lookup is binary
search within the chosen zone. The generator checks every explicit TZif
transition against its extracted table; future POSIX-tail rules are expanded
with the pinned build-host ZoneInfo implementation. Updates to civil timezone
law require rebuilding the bundled data; an enum is not a promise that political
rules never change. Dates outside the table's range report unsupported.

## Network time synchronization (SNTP)

Status: implemented 2026-09-24. Unauthenticated SNTP only; NTS is future work
(see the [secure networking roadmap](secure-networking-roadmap.md)).

`timesync.svc` owns outbound NTP authority; `clock.svc` still has no network
access. The declared scope is peer-bound UDP to any IPv4 address on port 123,
with DNS. It is approved by `(network approve-declared)` in `init.ccl`.

### Adjustment authority

Reading time never implies changing it. A separate `clock-control` service
role (22) is minted by procmgr only for a declared request approved by the
trusted startup plan, the same rule as master audio control. The endpoint
reaches clock.svc with the kernel-stamped tag
`CuBit.Clock_Control.Authority_Tag`, and clock.svc refuses `Submit_Sample`
from any other tag. `timesync-check` confirms natively that an ordinary clock
client's attempt is refused.

A sample is UTC in milliseconds, the local monotonic millisecond at which it
held, an uncertainty, the number of agreeing sources and an authentication
flag. The reply reports an `Outcome` and the resulting quality.

### Clock policy

`Clock_Discipline` (pure SPARK, proved) decides each sample:

| Rule | Outcome |
|---|---|
| Observed after the current monotonic time | `Rejected_Future_Observation` |
| Older than 10 s | `Rejected_Stale` |
| Uncertainty above 1 s | `Rejected_Uncertainty` |
| Beyond 2099, the end of the time-zone tables | `Rejected_Out_Of_Range` |
| Below the time floor | `Rejected_Below_Floor` |
| Unauthenticated with fewer than 2 agreeing sources | `Rejected_Sources` |
| Unauthenticated and more than 15 minutes from this boot's RTC reading | `Rejected_Conflict` |
| Unauthenticated after authenticated time was set | `Rejected_Conflict` |
| Otherwise | `Stepped` |

The 15-minute bound is measured from the RTC anchor, not from the current
network time, so repeated small spoofed steps cannot walk the clock away. If
the RTC is unavailable, or is below the floor (for example after CMOS battery
loss), unauthenticated time above the floor is accepted.

The **time floor** is the source commit time, generated at build time
(`Clock_Floor`). It is never earlier than 2026-01-01, because the Nix shell
exports a 1980 `SOURCE_DATE_EPOCH`. An RTC reading below the floor is
ignored. A floor that also persists the last authenticated time needs NTS and
Config persistence first.

Samples are applied as steps; monotonic time is never changed. Gradual
slewing is not implemented. `Time_Quality` gains `Network_Unauthenticated`
and `Network_Authenticated`. `CuBit.Clocks.Is_Valid_Wall_Time` accepts both,
together with `RTC_Only`, and the taskbar uses it.

### SNTP client

- **Configuration.** `time.servers` holds up to four `host` or `host:port`
  entries; the default is `time.cloudflare.com` and three `pool.ntp.org`
  names. `time.poll-seconds` accepts 64 to 86400 (default 1024). Nothing is
  hardcoded in the binary. Without the setting, timesync does not synchronize.
- **Requests.** Each server gets a fresh peer-bound UDP channel. The request
  is SNTPv4 mode 3, with a 64-bit transmit-timestamp nonce from RDRAND when
  the CPU reports it (RFC 9109); otherwise a TSC-derived value, which is
  logged as weaker.
- **Validating replies.** A reply must be exactly 48 bytes: mode 4, version 3
  or 4, stratum 1 to 15, not unsynchronized, with the nonce echoed as origin
  and nonzero timestamps. Server times must be in order, the round trip no
  more than 2 s, and the root distance no more than 1 s. A Kiss-o'-Death reply
  stops queries to that server until the setting changes or the service
  restarts.
- **Estimate.** UTC at reception is the transmit time plus half the network
  path delay. The uncertainty is half the path delay, plus the root delay and
  dispersion, plus 1 ms.
- **Agreement.** Estimates are combined with Marzullo's algorithm. A strict
  majority of at least two must share a point, and the midpoint of their
  intersection is submitted.
- **Scheduling.** After success, the next poll is in `time.poll-seconds` plus
  up to 63 s of jitter. After a failure it backs off from 16 s up to the poll
  interval.

The SNTP, server-list, clock-discipline and control-encoding cores are SPARK
proved (`make -C kernel prove-timesync`). The control round trip needs level 4.
Their hosted tests are in `make -C kernel test-timesync`. IPC adapters, RDRAND
and the service loop are tested, not proved.

### Headless regression

`tests/headless/run.sh --test timesync` passes (2026-09-24, QEMU TCG). It
boots `timesync-test.svc`: `timesync.svc` with only its `.cubit.caps`
section replaced. The build checks identical bindings, a non-empty section of
the same size, and that the installed section matches. Comparing the two ELFs
confirmed that every differing byte lies inside `.cubit.caps`.
The variant's scope names loopback fixture ports 18123 to 18125, because an
unprivileged host fixture cannot bind port 123. `tests/timesync/fixture.py`
runs two honest servers at host time + 300 s and a falseticker one hour
further. It checks request format and nonce freshness, then requires the
guest's reported UTC within 5 s of host + 300 s. In the passing run, the
guest adopted the two agreeing servers (uncertainty 5 ms) and stepped by
+301 s. `timesync-check` also confirmed that an ordinary clock client's
adjustment is refused. Linux-hosted fixtures show behaviour against the
fixture, not against internet NTP servers. Separately, a default
`boot-shell-nvme` run under KVM on 2026-09-24 synchronized against the four
default public servers through QEMU user networking: all four agreed, with
7 ms uncertainty. That is one observation, not a regression lane.

### Remaining work

- NTS (RFC 8915) with SPARKTLS, for `Network_Authenticated`, plus a persisted
  authenticated time floor.
- Slewing for small corrections; drift estimation.
- Suspend/resume handling, and a UI to show conflicts and time quality.

## GNAT runtime integration

`CuBit.Clocks` is a native runtime client, **not yet `Ada.Calendar` support**.
The runtime adapter should eventually implement:

- `Ada.Calendar.Clock` from UTC wall time, raising the standard failure when
  unavailable rather than silently substituting uptime.
- `Ada.Calendar.Time_Zones` from the same configured zone/rule source, not a
  separate environment variable or host libc implementation.
- `Ada.Real_Time` from the kernel monotonic clock, unaffected by timezone or NTP.

Before adding these standard names, test the standard year range (1901..2399),
arithmetic and Duration precision, local Split/Time_Of inversion across ambiguous
and nonexistent DST times, explicit-offset formatting, leap-second policy and
exception behavior. The current 1970..2399 civil helper and 2000..2099 zone table
do not cover that complete contract. Do not copy Linux GNAT OS primitives into
CuBit or turn a second-stack workaround into the calendar implementation.

## Verification status

The native code builds with Nix. Linux-hosted executable tests enumerate every
date in 1970..2399, both endpoints of each day, leap-century behavior, all enum
name round trips, selected DST boundaries and fractional offsets. Pure helpers
are SPARK-declared, but this increment does not claim a completed GNATprove run
or proof of RTC hardware, IPC, Config policy or time synchronization.

The USB-only KVM integration run also booted a fixed 2026-07-01 12:34 UTC RTC,
displayed 12:34 and advanced to 12:35 while running apps. A separate no-HDA run
booted clock/desktop normally and ran SameBoy silently. These are emulator
results; laptop RTC convention, media-key mapping and drift still need testing.
