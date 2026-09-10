# Development control wire profile v1

The transport-independent `CCL.Control` dispatcher exchanges typed Ada records.
`Control_Wire` validates and encodes the CBOR boundary; `Control_HTTP` handles
the bounded development HTTP envelope. The native `Control_Transport` owns
grant-backed networking. The browser is only another client, not an evaluator.

Operations 1–3 use one definite CBOR array `[1, requestId, operation, source]`.
Operations 4–6 use `[1, requestId, operation, source, targetGeneration]`.
Request IDs are nonzero uint64 values, echoed for correlation, **not authority
or replay protection**. Source is at most 1024 ASCII bytes (printable plus tab,
CR, LF); it must be empty except for evaluation or starting a monitor.
The target is nonzero for stop and zero for start/inspect. No maps, floats, byte strings,
tags, references, indefinite lengths, noncanonical integers, or trailing items.

| Operation | Response array |
| --- | --- |
| 1 Inspect bindings | `[1,id,1,pid,networkPid,clockPid,clockAvailable,monotonicMs,digest0,digest1,digest2,digest3]` |
| 2 Evaluate expression | `[1,id,2,ok,displayText,typeCode,diagnosticPosition,fuelRemaining]` |
| 3 Read clock | `[1,id,3,clockAvailable,monotonicMs]` |
| 4 Start monitor | `[1,id,4,accepted,state,generation,runs,intervalMs,source,ok,displayText,typeCode,diagnosticPosition,fuelRemaining,nextDeadline]` |
| 5 Stop monitor | Same 15-field shape, operation 5 |
| 6 Inspect monitor | Same 15-field shape, operation 6 |

Monitor state codes: Empty 0, Waiting 1, Executing 2, Stopping 3, Stopped 4,
Faulted 5. There is one shared lab-owned slot; starting an active slot is
rejected without replacing its source. Stop requires its current generation.
Generation protects against stale actions within this host lifetime, not
forgery, replay across restarts, or unauthorized clients. The host fixes the
interval at 1000 ms and fuel at 4096. Inspect does not create or reload a
program. Source/result remain inspectable after stop; reboot clears the slot.

Integers are unsigned uint64 on the wire; display text carries the existing CCL
result image (including signed values). Type codes: invalid/no scalar 0,
Integer 1, Boolean 2, String 3, Character 4. Diagnostic positions are 1-based,
zero if absent. Fuel is at most 4096. Responses are at most 8192 bytes, text at
most 4096 ASCII bytes. CBOR is not a trust boundary substitute: the browser
validates shape, field types, limits, version, operation and request ID again.

The HTTP lab profile accepts only POST/OPTIONS `/ccl` with HTTP/1.1, Host
`127.0.0.1:18445` and Origin `http://127.0.0.1:8787`. POST requires
`application/cbor` and Content-Length 1..1100. Headers are limited to 2048
bytes; input storage is 4096 bytes. Duplicate critical headers, transfer
encoding, Expect, encoding/upgrade/trailer headers, folded/malformed headers,
and already-buffered excess bytes are rejected. One request per connection;
no keep-alive/pipelining. A fixed five-second deadline covers all request reads.

Origin checks are browser cross-origin defenses, not authentication. Trusted
local clients may forge them. TLS, admission and authenticated discovery remain
future work; this endpoint must remain in an isolated loopback-only lab.

CBOR implementation: unmodified `cbor_ada` 0.3.0, Copyright Baris Erdem, at the revision pinned by
`flake.nix` / `flake.lock`, Apache-2.0. Its license is in the pinned source's
`LICENSE` file, reproduced as `CBOR_LICENSE.txt` here. The native app build also
stages it at `/licenses/cbor_ada.txt` in the ISO tree. It is linked into this
**userspace** app, never the kernel.
The profile excludes floats; upstream float-proof gaps remain recorded in
`docs/ccl-cbor-evaluation.md` and are not claimed resolved here.
