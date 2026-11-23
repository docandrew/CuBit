# GPIO pad diagnostic sampler

Run in the pinned Nix development shell with TMPDIR exported first:
`bash tests/gpio-pad-diagnostics/run.sh`.

Actual typed read-only sampler, checked/release. Tests invalid/active guards,
exact admitted plan/register IDs, all refusal positions and statuses, initialized
unattempted samples, and no further reads after refusal. Hardware is modeled.
Actual Main lifecycle/reset-wait wiring is covered separately by gpio-native.
No native execution, physical GPIO read or whole-driver proof claim.
