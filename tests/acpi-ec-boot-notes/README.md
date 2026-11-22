# EC boot metadata observations

This opt-in Linux-hosted suite exercises the actual request/bootstrap immutable
catalog, ECDT decoder, bounded observation/formatter and native logging hook. The
logger is an explicit hosted adapter; native runtime/logstore delivery is separate.

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nice -n 19 nix develop -c bash tests/acpi-ec-boot-notes/run.sh
bash tests/acpi-ec-boot-notes/run.sh --validate-only
```

Use Node on PATH for validation-only. Fresh execution needs the pinned Nix GNAT
and ACPICA tools. Outputs are private under TMPDIR, with one build worker,
180-second compilation, 30-second runtime, 5-second ACPICA limits, a 64MiB stack,
disabled core dumps and bounded logs. Generated hosted project filenames can
produce GPR's cosmetic project-name warning; Ada warnings remain errors.

The 168 workspace input files are the union of compiler-read ALI D dependencies
from checked/release profiles, including generic bodies. The fixture/main/logger
are pinned separately. Cases cover hidden idle/receiving catalogs, missing and
duplicate ECDTs, valid metadata, inspection-limit refusals, malformed fixed data,
unsupported IO range, register aliasing, invalid namepath, full 32-bit UID, and
metadata beside unavailable device providers. The ACPICA ECDT fixture must
compile/disassemble/recompile to identical bytes before catalog observation.

Both profiles pass 376 checks in 24 processes with identical stdout. The native
hook emits one information/warning record per call; test output also prints the
formatted value directly for comparison. The production hook emits one record.

The default 4096-byte copy budget is local diagnostic policy, not a format or
grant-size limit. Larger tables report the required extent and Inspection_Limit,
without a partial decoded description. GPE_Bit is not an IRQ/GSI. Copied GAS
addresses and UID do not establish namespace identity, reservation, Global Lock,
provider availability or hardware authority. No AML or hardware IO is performed
by these observations, and no wire operation/capability is added.

No new proof, native execution or hardware acceptance is claimed. The separate
native API/startup harnesses check the integration call site. Early provider
installation before module AML/root _INI remains unfinished. This suite is not
yet registered in global test groups; run it explicitly.
