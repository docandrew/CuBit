#!/usr/bin/env bash
#  Library-only system calls (IPC-013, user 2026-11-26): services, drivers,
#  apps and guest test programs reach the kernel only through the library
#  units (the runtime: CuBit.Kernel_ABI/Messages/Handles/Channels/Waits and
#  their kin; the libc; the Rust std port), never through a system-call
#  binding of their own. Then a mechanism change touches only the library.
#
#  library-only.sh          list the offenders (file: names), exit 0
#  library-only.sh --check  fail when a file not in library-only-allowed.txt
#                           offends, or a listed file no longer does (the
#                           list only shrinks; at zero, every file is clean)
#
#  A reference is a system-call number name (SYSCALL_*), a CuBit.Kernel_ABI
#  call number, the runtime's system-call instruction (CuBit.Kernel_Calls's
#  Call and Submit, which a renamed Kernel_ABI would otherwise hide), or a
#  raw syscall entry (syscall/syscallN, cubit_syscall, "syscall" in inline
#  assembly) in Ada, C or Rust source.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
cd "$root"
#  The runtime's system-call numbers (CuBit.Kernel_ABI's System_Call
#  constants), as Kernel_ABI.<Name>.
calls=$(grep -oE '^ *[A-Z][A-Za-z_0-9]* *: constant System_Call' \
          userspace/runtime/gnat/cubit-kernel_abi.ads | awk '{print $1}' | paste -sd'|')
pattern="\\bSYSCALL_[A-Z0-9_]+\\b|Kernel_ABI\\.($calls)\\b|\\bKernel_Calls\\.(Call|Submit)\\b|\\bsyscall[0-6]? *\\(|\\bcubit_syscall[0-6]?\\b|\"syscall\""
library='^userspace/(runtime|libc|rust|c)/'
offenders=$(grep -rlE --include='*.ad[sb]' --include='*.c' --include='*.h' --include='*.rs' \
              "$pattern" userspace tests 2>/dev/null \
            | grep -vE '/(build|obj|target)/' | grep -vE "$library" | sort || true)
if [[ ${1:-} != --check ]]; then
    for f in $offenders; do
        names=$(grep -ohE "$pattern" "$f" | sort -u | tr '\n' ' ')
        echo "$f: $names"
    done
    echo "library-only: $(wc -w <<<"$offenders") files outside the library units"
    exit 0
fi
allowed="$here/library-only-allowed.txt"
fail=0
for f in $offenders; do
    if ! grep -qxF "$f" "$allowed" 2>/dev/null; then
        echo "library-only: $f calls the kernel directly (use the library units)"
        fail=1
    fi
done
#  The allowed list only shrinks: an entry that no longer offends must go.
while IFS= read -r f; do
    [[ -z $f || $f == \#* ]] && continue
    if ! grep -qxF "$f" <<<"$offenders"; then
        echo "library-only: $f no longer calls the kernel directly: remove it from library-only-allowed.txt"
        fail=1
    fi
done < "$allowed"
(( fail == 0 )) && echo "library-only: PASS ($(wc -w <<<"$offenders") files still to migrate)"
exit $fail
