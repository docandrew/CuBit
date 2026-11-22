#!/usr/bin/env bash
# Native fixture scripts must resolve the gprbuild configuration before their
# first cd. Resolving it later from a relative $0 makes the make call fail
# inside an unquoted expansion, gprbuild runs with no configuration, and alr's
# default GNAT (v16) gets mixed with the v15 runtime objects.
set -euo pipefail
repo=$(git -C "$(dirname "$0")" rev-parse --show-toplevel)
cd "$repo"
status=0
checked=0
while IFS= read -r script; do
    [ "$script" = tests/headless/run.sh ] && continue
    [ "$script" = tests/native-gprbuild-config/check.sh ] && continue
    checked=$((checked + 1))
    assign=$(grep -n '^gprbuild_config=' "$script" | head -1 | cut -d: -f1 || true)
    first_cd=$(grep -nE '(^|[^[:alnum:]_])cd ' "$script" | head -1 | cut -d: -f1 || true)
    if [ -z "$assign" ]; then
        echo "FAIL $script: no top-level gprbuild_config assignment"; status=1
    elif [ -n "$first_cd" ] && [ "$first_cd" -lt "$assign" ]; then
        echo "FAIL $script: cd on line $first_cd before gprbuild_config on line $assign"; status=1
    fi
    uses=$(grep -c 'print-native-gprbuild-config' "$script")
    if [ "$uses" -ne 1 ]; then
        echo "FAIL $script: resolves the configuration $uses times (only the assignment may)"; status=1
    fi
done < <(git grep -l 'print-native-gprbuild-config' -- '*.sh')
[ "$checked" -gt 0 ] || { echo "FAIL no native fixture scripts found"; exit 1; }
[ "$status" -eq 0 ] && echo "PASS $checked native fixture scripts resolve gprbuild configuration before cd"
exit $status
