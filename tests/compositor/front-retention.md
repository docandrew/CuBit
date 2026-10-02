# Retained scanout front in the compositor pool

`Compositor_Pool` still manages exactly three allocation slots. It now separates
`Displayed` (a submitted/pending ticket) from `Front` (a retained visible ticket).
The copied-source Desktop path continues to use `Retire_Display`; it does not
create a front role. The new direct-front operations are currently exercised by
hosted tests and the native Mesa oracle, not a production direct-scanout adapter.

`Latch_Display` requires the exact pending ticket, the exact prior front ticket
(or `None` for the first frame), and authoritative evidence that the new target
latched and the previous front retired. The new front stays held. If the driver
signals these independently, its adapter must retain both allocations until it
has both facts. A matching rendering completion alone is insufficient.
`Retire_Front` handles the final target after confirmed quiescent output disable.

All four roles—writer, ready, pending and front—are distinct by slot. At most
three can be occupied with three allocations. `Acquire` returns `None` without
changing state if front, pending and ready occupy all three. This means defer
rendering/coalesce damage, not allocate a fourth target or write the front.
After confirmed replacement, only the retired old front becomes eligible for
acquisition; its next ticket has a new serial. Existing newest-ready replacement,
render-failure behavior and copied-source release remain supported.

Wrong generation, wrong serial, wrong prior front, duplicate latch, unconfirmed
retirement and invalid teardown fault the pool while retaining tracked roles.
Later apparently valid events cannot reopen a faulted pool. `Open` is only for
a fresh allocation epoch after separately established ownership/retirement; it
must not be used to forget uncertain old readers.

## Proof and evidence

The policy has 27 SPARK analysis results (18 flow, 9 prover), zero unproved or
justified. Contracts establish distinct valid tickets, write exclusion of the
front, front preservation across existing operations, exact latch/retire effects,
unchanged state under allocation backpressure and sticky failure.

Hosted tests perform 15,001 successful modeled latches, independently checking
visible allocation pixels while other targets are written. They exercise
front+pending+ready exhaustion and rendering while another target is pending,
nine stale/unknown retirement cases and duplicate latch. Existing 3,000
copied-source cycles, 2,000 actual Display helper frames and 1,000 actual cursor
repair frames also pass, including their repair-defect negative controls.

The native Mesa oracle adds 64 simulated latches with real Mesa draws into three
imported CuBit buffers. It verifies held front and pending pixels after rendering
the third allocation, bounded backpressure, replacement and final-front release.
The latch/retirement events are supplied by the test; this does not exercise
physical scanout, i915, a Display direct-target wire protocol or hardware fences.

Reproduce inside Nix:

```sh
cd kernel
alr exec -- gprbuild -q -p -P ../tests/compositor/pool.gpr
../tests/compositor/build/pool/pool_tests
alr exec -- gnatprove -P ../tests/compositor/pool.gpr -u compositor_pool.adb \
  --level=2 --report=all --checks-as-errors=on -j1
```

Build the native oracle with `tests/compositor/build-native.py` and the existing
musl Mesa archive directory, under `coordination/build.lock`. Run its
`compositor-probe.app` as `SOFTPIPE_IMAGE` with headless `--test softpipe` and
require `COMPOSITOR-FRONT`, `COMPOSITOR-POOL` and `COMPOSITOR-NATIVE` success
markers. The direct-target transport and driver contract are still pending;
see [the integration proposal](../../docs/compositor-shared-targets.md).

Checkpoint: native run `/tmp/cubit-front-native.log` completed the new front
marker, original pool marker and COMPOSITOR-NATIVE marker, but the full
100-second harness timed out after the baseline SOFTPIPE-NATIVE phase started.
The whole gate therefore did not pass, and its dependent Desktop overload run
was not reached. Both Desktop variants compiled. Rerunning the complete gate
with sufficient time remains required. This state is included in the user’s
safekeeping snapshot; it is not a release-completion claim.
