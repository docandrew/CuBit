#!/usr/bin/env bash
# Desktop_Icon_Store loads once per process: one run per case (README.md).
set -euo pipefail
build=$1
atlas=$build/Assets/cubit-icons/3/applications.qoi
oracle=$build/reference/applications.rgba
for chunk in 0 1 7 4096; do "$build/icon_store_tests" "$atlas" "$oracle" good "$chunk"; done
for mode in truncated corrupt read-failed; do "$build/icon_store_tests" "$atlas" "$oracle" "$mode"; done
"$build/icon_store_tests" "$build/fixtures/icons-wrong-size.qoi" "$oracle" wrong-size
