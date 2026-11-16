#!/usr/bin/env bash
# UI-020: Desktop composes into scanout buffers Display lends it (grants
# derived from the GPU driver's), never with authority over the GPU: its
# manifest requests no endpoint to the GPU service, so it can neither
# present nor clear there. (At run time Desktop also refuses to compose into
# lent scanout if it ever holds one: "desktop: holds a GPU endpoint".)
# Display's manifest requests no endpoint to Desktop: the owner offers a
# read-only one with its first loan request.
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
fail=0
strip() { sed -e 's/#.*$//' "$1"; }
if strip "$root/userspace/services/desktop/manifest.ccl" | grep -Eq '\(service "gpu"|request-service(-optional)? gpu'; then
    echo "DESKTOP-GPU-AUTHORITY: FAIL desktop requests the GPU service"; fail=1
fi
if strip "$root/userspace/services/display/manifest.ccl" | grep -Eq 'desktop'; then
    echo "DESKTOP-GPU-AUTHORITY: FAIL display requests an endpoint to Desktop"; fail=1
fi
[ $fail = 0 ] && echo "DESKTOP-GPU-AUTHORITY: PASS desktop holds no GPU endpoint; display none to Desktop"
exit $fail
