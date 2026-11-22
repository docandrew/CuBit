# Serving policy contracts

Export `TMPDIR=/home/doc/cubit-build-tmp` before entering the Nix environment.
Run `run.sh` for the existing exhaustive fifteen-fact behavioral fixture in
checked and release profiles. Run `run.sh --prove` for SPARK level 1 and 2
proofs of the actual service policy and the ghost clients in `fixtures`.
The proof runner pins the specified toolchain, solvers and local Why3 config.
Every invocation freezes its sources in a unique temporary output directory.

The contracts describe the initial declaration, all eligibility conditions,
retirement acknowledgment, refusal consuming the first choice, mode/reason
selection and immutable subsequent choices. Ghost clients check that consumer
ownership and absent retirement block observer selection, and that neither a
failed declaration nor an observer declaration can later become runtime mode.

These are properties of supplied facts and the pure policy. They do not prove
that native physical-owner observations are truthful, that retirement really
occurred, or that kernel registration, queues, native startup or hardware works.
The body is unchanged; the production addition is its public SPARK contracts.
