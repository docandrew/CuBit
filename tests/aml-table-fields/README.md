# Immutable table-field hosted milestone

Run inside the repository Nix shell:

```sh
python3 tests/acpi-hosted/run.py --group table-fields --mode release
python3 tests/acpi-hosted/run.py --group table-fields --mode checked
```

Both profiles passed 662,835 focused checks: 657,557 span/window checks,
4,126 initializer/quota checks, 981 owned field execution checks, 116 Alias/GC
checks and 55 literal static loader checks. Twelve cached ACPICA observations
(six field shapes under two integer revisions) match returned endpoint/type/length;
1,862 protocol and 13 evidence-guard checks also pass. This does not establish
general module execution, timing or error equivalence. The original evidence,
including raw tool output, remains in `../aml-table-field-reference/reference`.

The group runs five mains and the cached replay, with strict release/checked
switches, 180-second builds, 30-second mains, a 64 MiB stack, one build job and
1 MiB log bounds. Default and `all` selection remain unchanged. Full private
validation receipts are identified in `VALIDATION.json`; build products and full
validation logs are not part of the integration delta.

Table-backed extraction uses validated inventory/range spans and direct owned
Buffer initialization within arena quotas. Windows are limited to 1,024 bytes
per request; legacy inspection remains bounded. Literal-string DataTableRegion
selectors and the supported read-only module-level Field subset are implemented.
General module-level TermArg evaluation, side effects, timing and forward-resolution
semantics remain incomplete. No native service deployment is claimed.

The original table-backing proof discharged 111 of 113 checks, including all 91
runtime checks, leaving byte-content and window-content postconditions. A later
experimental proof discharged all 31 Read_Window checks with prefix/tail loop
invariants, but its scalar ghost branch retained unresolved obligations and is
not shipped. The selected body retains only those window invariants and the
original Read_Byte; that exact reduced whole unit has not been re-proved. The
original Read_Byte functional residual remains. Its focused span tests and the
combined public group pass with assertions enabled. Concrete initializer
instantiation passed SPARK admission only; allocator correctness and the full
interpreter remain unproved. No assumptions were added to discharge these checks.
