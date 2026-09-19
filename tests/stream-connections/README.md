# Typed connection admission tests

```sh
nix develop -c bash tests/stream-connections/run.sh
```

The Linux-hosted runner enables assertions and overflow checks, then proves the
portable admission unit and its ghost properties with GNATprove. It uses no live
service, kernel capability issuer, network, or image mutation.

The suite contains 55 admission cases and seven ghost assertions about allowed
connections, plus the generated proof obligations for the admission functions.

A second executable exercises binding prepare/commit/abort/retire: no early route
switch, denied/competing preparation, wrong actors/tickets, absent resource
readiness, withdrawn approval at commit, stale generations, aborted-resource
cleanup, old-resource retention and delayed/duplicate cleanup acknowledgements.
SPARK checks the lifecycle's runtime safety and postconditions: failed operations
leave the state unchanged; prepare/abort/retire preserve the active route and its
generation; successful commit advances the generation and installs an active route.
These are model properties, not proof of actual DMA/grant quiescence or authenticated
approval inputs. No runtime registry or live IPC endpoint is installed by this test.

The test matrix covers all combinations of the three approvals, all policy
denial outcomes, mismatched recipients, stale binding/port generations, another
controller, invalid references, direction errors, invalid delivery budgets,
incompatible delivery and element schemas, and selected-profile changes.

The text/log example uses **fixture** schemas: direct text-to-log is rejected;
the two separately approved adapter legs succeed. This checks admission only,
not text conversion, provenance forwarding, transport, or actual log ingestion.

Ghost assertions establish that every allowed result has all three exact-bound
approvals, valid references, output-to-input direction and compatible profiles.
Approval authenticity and issuer authority are trusted inputs, not something these
proofs establish. The model does not protect live binding state or perform atomic
generation validation. Runtime schema validation and complete transport/ownership
contracts remain necessary. See [design](../../docs/stream-wiring.md).
