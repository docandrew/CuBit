# Portable stream delivery policies

```sh
nix develop -c make -C kernel test-stream-policies prove-stream-policies
```

The hosted test enumerates the 6-by-6 delivery-policy compatibility matrix,
checks 1,024 exact/undersized payload budgets, and exercises malformed schemas,
in-flight limits, close mismatch and maximum-width sizes. The test executable
uses assertions and overflow checks; production builds do not gain runtime
assertions from this harness.

`Proof_Cases.Check` is Ghost and takes arbitrary policies. Its ten assertions
prove compatibility symmetry, valid self-matching, rejection of lossless/lossy
substitution, exact agreement of schema/delivery/capacity/close behavior, and
sufficient payload/in-flight bounds on an accepted match. GNATprove discharges
14 checks: these ten assertions and four termination checks, with no unproved
checks, assumptions or SPARK-Off regions. This is not a queue, transport,
concurrency, or authority-enforcement proof.

Payload accounting uses a non-modular `Payload_Count`; both factors originate
from 32-bit fields and their full product fits the declared 64-bit value range.
The upper-bound test checks `0xFFFFFFFF * 0xFFFFFFFF` exactly. Accounting covers
payload slots (including held elements), not allocator or control-record overhead.

The package also compiles directly against CuBit's freestanding runtime:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -P ../userspace/runtime/user_runtime.gpr -c -u cubit-protocols-stream_policies.ads'
```

This unit supplies **policy metadata only**. No existing stream API or wire
handshake uses it yet. Integrating it requires a versioned descriptor/handshake
and live enforcement, plus ownership, cancellation, resource admission and
authority checks. Exact policy matching must never be treated as a grant.
