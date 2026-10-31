# Module contexts and retained requests

Run bash tests/aml-module-execution-context/run.sh inside pinned Nix with TMPDIR=/home/doc/cubit-build-tmp set before entry. --validate-only checks exact inputs without compiling.

The checked/release, 32/64-bit hosted matrix has 8780 checks across 100 fresh runtime processes and 116 commands. All existing cases remain. Each request case now also checks malformed, absent and newly declared parameter paths before loading, preserving the namespace audit, caller pins and retained source state without module effects.

The real Core pipeline selects existing paths, appends an owned retained block, applies ordinary Store and closes serving after post-effect failure. Tests cover foreign identities, lookup ordering, held pins, omitted parameters, duplicates, type changes, module/fatal failure and measured remaining budget. All fixtures use normal pinned iASL.

Run bash tests/aml-module-execution-context/reference-ordering.sh inside pinned Nix for the independent ordering controls. The runner checks exact tool hashes and retains sources, compiler/runtime logs, expected refusals and input hashes.

Independent ACPICA controls (12 cases across both widths) accept an existing target and reject missing/newly declared targets before initialization effects. ACPICA resolves the parameter before loading and stores afterward. The ACPI 6.6 store-timing wording does not establish a requirement for late lookup. A privately tested late-lookup alternative was retained as evidence and not installed as a correctness fix. See /home/doc/git/.acpi-proof-work/preload-parameter-policy-work/../postload-parameter-work/REVIEW.md for the observations and decision.

This is idle-owner composition of already evaluated arguments, not actual LoadTable bytecode or complete parameter policy. Active callers, table ownership/copied pools, cleanup/readiness and whole verification remain open. No new proof or native/hardware validation is claimed. Prior unchanged-source regression results in VALIDATED.json are explicitly labeled as reused evidence.


## Definition provenance and retained-attempt identity

Namespace declarations now retain an opaque definition identity issued while
executing each DSDT, SSDT or retained append. Persistent alias and Field objects
belong to their declaring definition, independently of their target or backing
region. Method-local nodes follow their existing lifetime owner. Value kind
changes preserve declaration identity. Authenticated queries expose provenance
without exporting the arena. Legacy seeded/root nodes may have no definition.

Each limited append attempt retains the issued identity exactly when consumed.
Core's load model includes it, and load reports and direct-slot diagnostics retain
it through successful and failed consumed attempts. Full-byte duplicates report
the canonical definition without consuming the alternate slot or replaying
effects; identical headers with different bodies remain independent. An identity
is metadata, not hardware authority or permission to delete a namespace object.
No_Definition must never be interpreted as a cleanup wildcard.

The independent foundation matrix has9676checks across104runtime processes and
124commands, with matching checked/release results and both AML integer widths.
Independent normal ACPICA controls compare visible values and declaration-owner
groups, including aliases and Fields whose backing belongs to another table.
Numeric tokens are not compared. Exact regression and source linkage is recorded
in tests/aml-module-execution-context/VALIDATED.json.

The unfinished private VM loader and recovery prototypes are excluded. The shared
executor is unchanged. Current Core records cover idle-owner appends; active
loaders still need one authoritative reservation/consumption registry below the
Core/namespace boundary, nested context restoration, reference/method-byte
lifetime, definition retirement, copied Load pools, full parameter/error policy
and provider/readiness integration. Provenance alone does not establish any of
those properties. No new SPARK proof, native execution or hardware validation is
claimed. The complete verified interpreter/service objective remains unfinished.
