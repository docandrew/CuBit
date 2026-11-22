# Native endpoint envelope regression

From the repository root:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command nice -n 19 bash -c 'set -e; ulimit -S -s 65536; python3 tests/aml-core/native-endpoint/prepare.py; gprbuild -p -j1 -P tests/aml-core/native-endpoint/endpoint_checked.gpr; tests/aml-core/build/native-endpoint-checked/endpoint_tests'
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

The ABI generator now follows the current opaque Process_ID declaration and
copies the actual runtime parent/type spec/body unchanged, alongside the exact
message/completion layout and hashed input manifest. No synthetic integer PID
alias is introduced. Original endpoint_tests.adb and region/backend sources are
unchanged. Separate release/checked projects use isolated outputs; checked has
-gnata and -gnato. Hosted mocks do not establish native message stamping, actual
hardware access, launcher authority or runtime stack sufficiency.
