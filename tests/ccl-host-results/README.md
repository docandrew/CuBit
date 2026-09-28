# Owned CCL host results

Run with Nix:

```sh
nix develop -c bash -c 'cd kernel && \
  alr exec -- gprbuild -p -P ../tests/ccl-host-results/results.gpr && \
  ../tests/ccl-host-results/build/result_tests && \
  alr exec -- gnatprove -P ../tests/ccl-host-results/results.gpr \
    -u result_fixture.adb --level=2 -j2 --report=all --checks-as-errors=on'
```

The previous callback profile returned a discriminated `Host_Values.Value`
through an `out` parameter. That signature permits a caller to supply an object
constrained to one kind, while the callback changes it. Four such assignments
were unproved in the direct and periodic scalar-wrapper instantiations.

Callbacks now return one `Host_Values.Call_Result`: a **non-discriminated** record
containing a mutable discriminated value and the existing success flag. A caller
cannot constrain this envelope to one returned kind. The actual value remains
strongly discriminated; it has not been flattened into unrelated always-present
fields. No heap, pointers, exception handlers, constrainedness guards, `Assume`,
or SPARK exclusions are needed. A false success flag means the caller must not
consume the returned value; it is the existing host-call status, not a new
source-language error/Result model or an authorization token.

The fixture exercises every previous/new kind pair, including integer, boolean,
text, handler-shaped and owned object values, for ordinary, aliased, nested and
array storage. 175 hosted checks pass. Ten focused SPARK obligations discharge,
including all five kind-changing assignments and the functional returned-kind/
success contract. Latest focused proof: `/tmp/cubit-source-host-proof.log`.
The handler fixture checks representation only; an empty handler reference does
not become an executable callback or a legal source result.

Production consumers use the same envelope: interpreter, sessions, retained
handlers, buttons, periodic programs, Workbench, remote control and Config's CCL
binding. The old two-output host callback profile was removed, not kept as a
compatibility overload. The scalar VM host callback is a separate interface
returning the VM's non-discriminated value record and is unchanged. CCLB and
on-wire Config protocols have not changed.

Fixture logs: `/tmp/cubit-host-result-fixture.log` and
`/tmp/cubit-host-result-fixture-final.log`. Hosted consumer results:
`/tmp/cubit-host-result-consumers.log`; initial discovery/view/remote results:
`/tmp/cubit-host-result-regressions.log` (that initial command completed those
suites, then stopped on a nonexistent Make target; the remaining suites were
run explicitly afterward).

Proofs of the actual instantiated wrapper and native integration are separate
from this small fixture; do not use its nine checks to claim that the entire
interpreter or the Config/Turso durability model is proved.

The actual direct and periodic wrapper instantiations now discharge all four
previously failing discriminant checks, plus both scalar-conversion preconditions.
The focused report has 309 total checks: 303 flow/termination, four runtime
checks and two functional contracts, none unproved. Log:
`/tmp/cubit-host-result-instantiated-proof.log`; project
`tests/ccl-remote/remote.gpr`, subdirectory `owned-result-proof`, unit
`interpreter_host.adb`, region `ccl-language.adb:1898:1920`. This is deliberately
not a whole-interpreter result; a broader run is tracked separately.

Native compilation passes for Workbench, `ccl-control` and `config-check`.
KVM `config-inspection` passes, executing a real CCL `config.get` through the
native binding alongside scope-denied and backend-nomination-denied fixtures.
The native Workbench startup/first-frame smoke test also passes; the normal ISO
is restored. Log: `/tmp/cubit-owned-result-native.log`. This does not mean the
new general aggregate Config source imports are implemented: the native CCL
fixture uses the existing string Config query, and the broader typed-object
client remains a separate integration surface.
