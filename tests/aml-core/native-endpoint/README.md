# Native endpoint envelope regression

From the repository root:

```sh
nix develop -c bash -c 'python3 tests/aml-core/native-endpoint/prepare.py && cd kernel && alr exec -- gprbuild -p -P ../tests/aml-core/native-endpoint/endpoint.gpr && ../tests/aml-core/build/native-endpoint/endpoint_tests'
```

This hosted test executes the production ACPI_Backend_Endpoint adapter and core
with Region_Mock as the hardware callback. prepare.py extracts the current
runtime's message declarations, including representation clauses and the size
assertion, into the ignored build directory. It does not copy syscall bodies,
emulate kernel stamping, or establish kernel/process isolation. Compiler switch
-gnatwJ permits the existing runtime declarations' parenthesized array syntax.

Coverage includes forged authority in payload/reserved fields, mismatched and
zero stamps, every nonzero reserved/flags value, every incorrect envelope length,
unknown register IDs (including values formerly interpreted as offsets), write-mask violations,
stale epochs, read-value encoding, write arguments, cleared reply authority,
and uncertain completion retaining an internal receipt without exposing it or
allowing replay. Replies are prefilled with nonzero data to detect stale output.

2026-10-01: 330280 checks passed. The standard run.sh also prepares and executes this target.
