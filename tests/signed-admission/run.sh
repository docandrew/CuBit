#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Hosted tests of signed admission (SIGN-001, userspace/lib/signing):
#   tests/signed-admission/run.sh            build and run the tests
#   tests/signed-admission/run.sh --quick    the same without the bit sweep
#   tests/signed-admission/run.sh --prove    then gnatprove the library (level 2)
#   tests/signed-admission/run.sh --mutate   mutation checks (mutations.sh)
# Run in the Nix shell; the caller bounds gnatprove's memory. Fixture keys
# are generated at test time in a temporary directory and removed after.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
if [[ ${1:-} == --mutate ]]; then
    exec bash "$here/mutations.sh"
fi
fixtures=$(mktemp -d "${TMPDIR:-/tmp}/signed-admission.XXXXXX")
trap 'rm -rf "$fixtures"' EXIT
bash "$here/fixtures.sh" "$fixtures/certificates"
# The payload: a CuBit program when this checkout has built one (its
# .cubit.* sections are then covered by the bit sweep), else this test.
payload=${SIGNED_ADMISSION_PAYLOAD:-}
if [[ -z $payload ]]; then
    for candidate in "$root/kernel/isodir/boot/sleep.app" "$root/kernel/isodir/boot/hello.app"; do
        [[ -f $candidate ]] && { payload=$candidate; break; }
    done
fi
cd "$root/kernel"
alr exec -- gprbuild -p -q -P "$here/signed_admission_tests.gpr"
alr exec -- gprbuild -p -q -P "$root/userspace/ccl/ccl_sign.gpr"
objects=${SIGNED_ADMISSION_OBJ:-$here/build}
"$objects/main" "$fixtures/certificates" "${payload:-$objects/main}" ${1:+$1}
bash "$here/cli.sh"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/signed_admission_tests.gpr" \
        -u cubit-signed_admission-envelopes.adb cubit-signed_admission-certificates.adb \
           cubit-signed_admission-revocations.adb cubit-signed_admission-scopes.adb \
           cubit-signed_admission-decisions.adb cubit-signing_keys.adb \
           cubit-certificate_encoder.adb cubit-program_signing.adb \
        --level=2 -j2 --checks-as-errors=on --report=fail
    grep -E "^(Run-time|Assertions|Functional|Total)" "$objects/gnatprove/gnatprove.out" || true
fi
