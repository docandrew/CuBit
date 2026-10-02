#!/usr/bin/env bash
# Copy the pinned Mesa source before applying our small platform adaptation.
# Never patch the Nix store or a caller's source checkout in place.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
source_tree=${1:?pinned Mesa source tree required}
destination=${2:?new destination directory required}
test -f "$source_tree/src/util/detect_os.h"
if [ -e "$destination" ]; then
  echo "Destination already exists: $destination" >&2
  exit 1
fi
cp -R "$source_tree" "$destination"
chmod u+w "$destination/src/util" "$destination/src/util/detect_os.h" "$destination/src/util/os_misc.c"
chmod u+w "$destination/src/c11/impl" "$destination/src/c11/impl/threads_posix.c"
patch --batch --fuzz=0 -d "$destination" -p1 < "$root/tests/mesa-software/cubit-platform.patch"
