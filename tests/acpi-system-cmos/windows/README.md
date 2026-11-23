# CMOS named ACPI hardware window regression

Inside the pinned Nix development shell, with TMPDIR exported first:
`python3 tests/acpi-system-cmos/windows/run.py`.

The standalone runner checks exact source hashes and uses private outputs.
It compiles the actual CCL manifest frontend and native ACPI manifest, then
runs148admission/refusal checks in checked and release profiles. The emitted
canonical sections are assembled into a real hosted ELF. Exact native Main
`findSection`, `readManifest` and resource-dispatch fragments are generated
from the checked source and tested against6valid/malformed ELF cases. The
capability-minter callback is modeled. An original zero-map refusal branch
must compile and fail the positive test, demonstrating the regression.

No native execution, physical CMOS access or whole-service proof is claimed.
The independent proof receipt covers only the new pure manifest-window
policy:13checks closed,0unproved,0Assume. Existing unchanged proofs are reused.
