# Region notification result lifetime

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-region-cleanup/run.sh

The runner also accepts `--source-root DIR` and `--validate-only`. Sources and
test inputs are hash-pinned; normal runs copy them into a private directory.
Builds use one worker and a180-second bound; each runtime has a30-second bound.

The actual region initializer and retained-result implementation run with a
modeled evaluator report/release policy. Both AML integer widths and checked/
release builds cover ordinary two-region connection/disconnection, excessive
charge with successful/refused cleanup, refusal without clearing a handle,
claimed success without clearing it, failure after clearing it, lost provider
readiness, failed access status, missing handles and disconnect cleanup failure.
The suite checks retained slot counts, no later notification after failure,
foreign-context refusal, cleanup-only retry and absence of AML replay.

There are948checks/40runtime processes. The predecessor implementation fails a
separate retained-result regression after an excessive callback charge; the
original source hashes, compiled test and failure log are preserved in the
milestone's private NEGATIVE.json evidence. The new suite needs the cleanup API.

ACPICA20260408 compares explicit local aliases, package values and ordinary
method effects at revisions1/2. It emits the retained cache-cleanup diagnostic,
which is recorded separately from method success. This does not establish
equivalence of provider fault handling or notification/lifecycle policy.

No new SPARK proof or native/hardware validation is claimed. Providers must
preserve Core/result ownership and satisfy the callback/release contracts.
The owner must retain the session, Core and backing while Pending is true.
Close authenticates the original Core lifetime and never reconnects a failed
session. Hardware/provider withdrawal is a separate lifetime decision.
