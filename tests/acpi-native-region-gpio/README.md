# Native region wrapper with GPIO startup

This hosted suite selects the actual native Regions body, Core, device walker,
scoped region initialization, initializer history, GPIO observation/discovery,
payload builder, preparation owner and admission transport. It exercises the
production prepared-plan classifier and ancestor-triggered REG entrypoints,
announcement and disconnect state, and copied diagnostic entrypoints.

Cases cover AML 32/64 modes, renamed and deep ancestors, known/unknown replies,
one confirmation, outstanding grant retirement, expired original deadlines and
known refusal. A different Core cannot use the classifier. Unknown completion
or an unretired grant blocks native disconnect before any REG0. Successful
cleanup disconnects known groups exactly once. Diagnostic entrypoints are
one-shot and do not perform EC physical callbacks in these scalar fixtures.

Physical preparation, Fields observations, lock readiness, memory profiles,
kernel replies, grants and clocks are modeled. The EC command observation
uses the actual local command generic in its idle state. The EC-related AML
fields here are scalar substitutes; this is not the laptop's ERAX geometry.
Neither native startup/registration nor physical Field effects are executed.

The full Core is compiled with `-O0 -gnatp` and explicit fixture checks. No new
SPARK proof, native/hardware result or full-service correctness is claimed by
this suite. Export `TMPDIR=/home/doc/cubit-build-tmp` before Nix and run the
Node `run.mjs` entrypoint. Results and frozen source pins go to a unique
temporary directory. The hosted Core uses a 128-node alias; the native
component uses a 16,384-node limit.
