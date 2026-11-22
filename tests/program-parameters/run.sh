#!/usr/bin/env bash
#  Hosted tests of typed program parameters (docs/ccl-launch-parameters.md):
#  ccl-manifest generates the binding (package Parameters) and the
#  .cubit.parameters descriptor for acpi-sample.ccl; the descriptor's
#  identity is checked with an independent SHA-256 (Python hashlib), then
#  parameter_tests checks the binding against the launcher's CCL check.
#  --prove also runs gnatprove (level 2) over the binding's Decode, the
#  token readers and the descriptor and section codecs (under a memory cap).
#  Needs a built ccl-manifest (make -C kernel ccl-manifest).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
tool="$root/userspace/ccl/build/manifest/ccl-manifest"
generated="$here/build/generated"
rm -rf "$generated"
mkdir -p "$generated"
"$tool" "$root/userspace/ccl/catalogs/native-runtime-services.ccl" "$here/acpi-sample.ccl" \
    --schema "$root/userspace/ccl/interfaces/executable-manifest.ccl" \
    --startup-output "$generated/startup.ads" > "$generated/manifest.S"
gcc -c "$generated/manifest.S" -o "$generated/manifest.o"
objcopy --dump-section .cubit.parameters="$generated/descriptor.bin" "$generated/manifest.o" "$generated/manifest-copy.o"
python3 - "$generated/descriptor.bin" <<'EOF'
import hashlib, struct, sys
data = open(sys.argv[1], 'rb').read()
magic, version, length = struct.unpack_from('<4sHH', data, 0)
identity = data[8:40]
root_length = data[40]
root = data[41:41 + root_length].decode()
signature = data[41 + root_length:]
assert magic == b'PRMS' and version == 1, (magic, version)
assert len(signature) == length and root == 'ACPI_Parameters', (len(signature), length, root)
expected = ('(type Deadline_Milliseconds (range 1 9223372036854775807)) '
            '(type ACPI_Deadline (variant (Within Deadline_Milliseconds) (Forever))) '
            '(type Namespace_Node_Count (range 1 2147483647)) '
            '(type Method_Byte_Count (range 1 2147483647)) '
            '(type ACPI_Parameters (record (deferred_initialization ACPI_Deadline) '
            '(namespace_nodes Namespace_Node_Count) (method_bytes Method_Byte_Count)))')
assert signature.decode() == expected, signature
assert hashlib.sha256(signature).digest() == identity, 'identity is not the SHA-256 of the signature'
print('program-parameters: descriptor PASS (canonical signature, SHA-256 identity)')
EOF
cd "$root/kernel"
alr exec -- gprbuild -p -q -j8 -P "$here/parameter_tests.gpr"
#  The CCL reference path needs a few MiB of stack per check.
(cd "$generated" && ulimit -s unlimited && "$here/build/tests/parameter_tests")
#  The image build's check of startup-plan arguments (ccl-config --programs).
python3 "$here/test-startup-arguments.py"
if [[ ${1:-} == --prove ]]; then
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
        alr exec -- gnatprove -P "$here/prove.gpr" -j2 --level=2 --checks-as-errors=on
fi
