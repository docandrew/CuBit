Hosted local lifecycle policy tests

In the Nix environment run `bash tests/acpi-local-lifecycle/run.sh`.
`--source-root DIR` chooses a review tree; `--validate-only` validates the
pinned source/helper catalog without compiling. Output and RESULT.json stay
in a fresh private TMPDIR directory. No shared lock, native build or hardware.

Runs the actual pure shutdown controller and pool capability owner in checked
and release profiles: twelve shutdown cases and 33 pool cases per profile.
These test modeled callbacks, transport and lifecycle evidence; they do not
prove hardware, kernel transport, root integration or whole-service quiescence.
No proof is run by this entry point. ACPICA is covered by separate AML suites.

One compiler worker, build timeout180s, execution30s, address space24GiB,
captured output1MiB, stack64MiB, no core dumps. Inputs and generated projects
are hashed and revalidated around every command. Permanent program build
definitions remain component.ccl; GPR projects here are temporary test inputs.
