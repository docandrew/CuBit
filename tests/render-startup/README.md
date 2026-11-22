# Render startup decisions

`CuBit.Render_Startup` separates the decision to resume a suspended application
from the foreign operations that create it, request GPU authority, inspect its
capabilities and stop it. Production procmgr uses it for required rendering and
explicit optional-render wire requests. Optional requests use param0 1 in the
existing type-11 request; param0 0 remains required. The CCL compiler spelling
is pending integration by its owner. Desktop's manifest is unchanged.

For optional requests, an unapproved launch starts software-only without broker
contact. An approved launch that fails admission is stopped before one fresh
software-only attempt. Both require successful empty-slot inspection. The
wrapper makes at most two attempts; neither a required request nor the software
attempt can request another retry. Optional requests are limited to the normal
application role, before any Config-storage attachment.

## Decisions and proof boundary

| Attempt | Conditions for resuming | Otherwise |
| --- | --- | --- |
| Render | Approved request, authenticated admitted result, inspected render endpoint | Discard; an optional request may request one software retry |
| Software only | Optional request, no admission request for this child, successfully inspected empty render slot | Discard without another retry |

Pending, rejected and uncertain admission results never permit the existing
child to resume. A required request never selects software or requests a retry.
The retry predicate additionally requires an accepted stop request. The software
attempt cannot request another retry, so a caller following these transitions
makes at most two attempts.

These are proved decision properties, not proof of the foreign observations.
The caller must establish all of the following:

- Approval comes from trusted policy, independently of executable metadata.
- Admission replies belong to the captured child incarnation and request.
- Capability inspection succeeds. An inspection error is `Unknown`, not `Empty`.
- A retry creates a fresh child incarnation. It must not resume or reuse the
  failed GPU child, even if a late success arrives.
- The software attempt makes no GPU admission request and has an empty render
  destination after all other manifest capabilities have been installed.
- Failed-attempt broker slots, tokens and uncertain GPU resources remain
  retained until their independent lifetime protocol allows retirement.

A successful stop syscall acknowledges the stop request; it does not establish
GPU quiescence, display retirement, or permission to reclaim a broker resource.
The decision package allocates no buffers and performs no IPC or pixel copying.

## Hosted checks

Run from the repository root:

```sh
nix develop -c bash -c '
  set -e
  gprbuild -p -P tests/render-startup/startup.gpr
  tests/render-startup/build/startup_tests
  cd kernel
  alr exec -- gnatprove -P ../tests/render-startup/startup.gpr \
    -u cubit-render_startup.ads --mode=all --level=2 --report=all \
    --checks-as-errors=on -j1
'
```

The test enumerates 320 combinations of requirement, attempt, approval,
admission, capability observation and stop acceptance, plus 49 incarnation
pairs including invalid identities and PID reuse with a new generation. On
2026-10-02 all passed; GNATprove reported 11 results (four flow/termination,
seven prover), with zero unproved or justified checks. Eleven faulty synthetic
traces also validate the native observer's rejection checks. This does not
prove procmgr as a whole.

## Native gate

The existing [native launch-policy fixture](../mesa-anv/native-launch-policy/README.md)
checks production procmgr's required-request path: an unapproved child does not
submit admission, an approved child without a provider cannot resume, both
children are stopped, and ordinary Devices startup still succeeds. It does not
test optional software retry, successful Intel admission or physical latency.
The [optional fixture](native/README.md) adds direct software startup, fresh-child
retry, occupied-slot and malformed-flag cases. Its first run was rejected by
the ELF loader because the fixture manifest object implied an executable stack;
the observer correctly failed, but its new runner branch did not propagate the
failure. That first run is failed evidence despite its wrapper's exit 0. The
fixture stack flags and runner propagation were corrected. The rerun passed
under four-CPU TCG, 1 GiB, for 90 seconds: two software children executed with
verified empty slots, four render attempts were denied, five failed children
were stopped, two admissions were submitted, and exactly one software retry
occurred. The retry's running incarnation differed from its failed GPU child.
Devices also opened its native window. Evidence is `optional-corrected.log`
and `optional-corrected.serial.log`; `optional-final.sha256` records sources and
staged binaries. Procmgr, the policy and fixture main matched the hashes taken
during the first run; only fixture packaging and failure propagation changed.
The oracle's 11 negative controls pass, and the actual headless shell guard
was separately exercised with successful and deliberately failing traces.

Current evidence is kept under `build/evidence/` (ignored build output). The
2026-10-02 native run before optional integration passed `render-launch-policy` under four-CPU TCG with
1 GiB and a 70-second run. The serial log records exactly two denied launches,
one submitted admission, two successful stop requests and the Devices native
window. No sentinel ran. This validates the required-request integration only.

`native-verified.log` and `native-verified.serial.log` retain that result. The
wrapper exited 1 **after** the native test passed because its post-test source
hash check detected a concurrent change to the CCL owner's `ccl-language.adb`.
Procmgr and `CuBit.Render_Startup` matched their pre-build hashes. The before
and after hash files retain the difference; the after file also records the
staged kernel, procmgr and devmgr binaries. This is not a completely frozen-
source reproducibility record, and no CCL correctness claim follows from it.

Earlier failed builds are also retained: runtime formatting (corrected and
re-proved), devmgr's missing `ccl-streams.ads` source-list entry, and a missing
`Type_Source` helper in the CCL image compiler. The latter two were corrected
by the shared-source owner before the successful native run; this work did not
edit those sources.
