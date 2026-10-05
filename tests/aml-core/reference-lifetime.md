# AML Index-reference lifetime oracle

Run inside the pinned Nix development environment:

```sh
python3 tests/aml-core/acpica_references.py \
  --tools "$(dirname "$(command -v acpiexec)")" \
  --output /tmp/acpi-reference-lifetime --reference-only
```

Use a fresh output directory for each invocation. To compare a CuBit runner,
replace `--reference-only` with `--runner /absolute/path/to/runner`. The runner
receives the AML file, method name, and `--result-object`; it must return exactly
`INTEGER n` and exit successfully. Reference-only success establishes ACPICA
fixtures, not CuBit conformance. The interpreter does not yet support these
mutable reference cases; this suite is not registered as a passing CuBit test.

The fixture runs each method in a fresh ACPICA process at both 32-bit and 64-bit
AML integer widths (14 observations):

| Method | Expected | Property exercised |
| --- | --- | --- |
| RIDX | 3 | Index into a method-local buffer survives return. |
| RPKG | 7 | Index into a method-local package survives return. |
| RGLB | 9 | Returned global-buffer reference observes a later write. |
| RCHN | 3 | Returned buffer reference survives another allocating call. |
| RIWR | 3 | Store to an argument holding an Index reference does not blindly write through it. |
| RSTR | 97 | Index into a method-local string survives return and reads its first byte. |
| RSMU | 99 | A global string reference observes a later indexed write. |

These observations require backing-object retention beyond the creating call.
They do not establish all Name, Local, Arg, RefOf, package replacement, or
reclamation semantics. A preliminary returned RefOf(Local0) probe produced
AE_AML_NO_RETURN_VALUE and is deliberately excluded pending further analysis.

Each width has an independent control evaluation. Unexpected ACPICA diagnostics,
missing or multiple results, nonzero exits, mismatches, and changed input hashes
fail the run. Only the exact ACPICA 20260408 shutdown cache diagnostic observed
in the control may be accepted after the result. A report is written only after
all checks succeed; any old report is removed before starting. Reports include
source, tool, runner (when used), ASL, and AML hashes.
