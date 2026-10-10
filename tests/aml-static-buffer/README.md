# Static named Buffer count checks

From repository root:

```sh
tests/acpi-hosted/run.sh --group static-buffer --mode release
tests/acpi-hosted/run.sh --group static-buffer --mode checked
tests/acpi-hosted/run.sh --group static-buffer-oracle --mode release
tests/acpi-hosted/run.sh --group static-buffer-oracle --mode checked
```

Pure fixture mains expect306+158 checks per profile. They cover canonical Integer/String/nonempty Buffer sizes, scopes, implicit hexadecimal/width conversion, package consumption, raw bytes and rollback. This does not admit arbitrary TermArg evaluation or change VarPackage count policy.

Oracle group builds a distinctly named Static_Buffer_Oracle_Runner selecting TEST, then explicitly runs compare.py. It never replaces the original ASLTS MN00 runner. Cached reference manifest verifies38 normal iasl cases:36 compiled inputs executed by CuBit;33 integer comparisons and3 explicit bounded-rejection policies per profile, plus2 recorded compiler rejections that are not runtime parity.32bit oversized String size and empty Buffer are policy rejections, not successful parity. This replays preserved ACPICA observations; it does not launch fresh ACPICA or require network. Original ASL, AML, stdout/stderr and tool/source provenance are retained. Source archive and Nix executable provenance are distinct. Cache-allocation warnings remain in raw logs.

Each oracle child is limited to1GiB address space,64MiB stack and30s; per-run directories preserve return codes/timeouts/failures. Build/result directories are local and disjoint by profile/run. The oracle remains separately selectable; all includes the two pure groups but not the cached oracle. Fixture origin: aml-static-buffer-coercion-c1s30dy9; reference origin: static-buffer-coercion-oracle-siuokc0j. No proof or native-stack claim.
