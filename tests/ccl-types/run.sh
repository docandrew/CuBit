#!/usr/bin/env bash
# Run inside nix develop. Hosted assertions are intentional; no native checks.
set -euo pipefail
cd "$(dirname "$0")/../../kernel"
alr exec -- gprbuild -p -P ../tests/ccl-types/types_tests.gpr
../tests/ccl-types/build/registry_tests
../tests/ccl-types/build/enum_tests
../tests/ccl-types/build/variant_tests
../tests/ccl-types/build/variant_rejection_tests
../tests/ccl-types/build/resource_tests
