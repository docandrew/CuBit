# Reviewed Config activation

Linux-hosted tests of the shared SPARK `Config_Activation` controller, using the
real CCL config evaluator and scoped Config authority state. No synthesized
approval Boolean, alternate ACL engine or ordinary-write bypass. These tests do
not execute native IPC, Turso commits or Desktop changes.

```sh
nix develop -c bash tests/config-activation/run.sh --prove
```

The host harness enables assertions and overflow checks; production/native
builds keep their existing options. Ghost predicates describe source
correspondence, current review authority and exact consumer acknowledgements.
GNATprove focuses on the controller; it does not prove the compiler, database or
IPC transport by association.

2026-09-26: 126 checks pass. Focused proof43 obligations (five functional), none
unproved/justified; report `build/obj/gnatprove/gnatprove.out`. Native Config links
and the activation unit compiles against CuBit's runtime; it is not linked into
a native activation endpoint yet. No ISO rebuilt or VM activation tested here.

Tests cover legacy/wildcard read/write denial, wrong scopes and subjects,
readback revocation, grant replacement/regrant, stale bases and proposal IDs,
invalid edits consuming reviews, owned source versus mutable UI copies,
multi-setting/startup rejection, arbitrary String lower bounds, duplicate/delayed
completions, definite rejection versus uncertain commit, malformed success
revisions, pending/failed application, consumer replacement and revision overflow
boundaries. The ordinary catalog and authority-wire suites separately ensure
activation cannot sneak through normal handles or the old grant mask.
Revocation after Begin_Commit blocks future admission/readback, but does not
retroactively cancel already accepted work; the fixture tests that boundary too.

The next step is a trusted managed-collection storage adapter, not exposing
this package as an unrestricted endpoint. See
[the design and trust boundary](../../docs/config-declarative-state.md#source-bound-activation-controller-2026-09-26).
