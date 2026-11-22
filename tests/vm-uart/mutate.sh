#!/usr/bin/env bash
#  Mutation check of vmm's guest UART, serial keys and console line: each
#  mutant breaks one rule; the level-2 proof or the hosted test's literal
#  oracles must fail for each. Run in the Nix shell under the memory cap
#  (run.sh).
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
apps=$here/../../userspace/apps/vmm
files=("$apps/core/guest_uart.ads" "$apps/core/guest_uart.adb" "$apps/serial_keys.adb" "$apps/core/console_line.adb")
saved=$(mktemp -d)
for f in "${files[@]}"; do cp "$f" "$saved/$(basename "$f")"; done
restore() { for f in "${files[@]}"; do cp "$saved/$(basename "$f")" "$f"; done; }
trap 'restore; rm -rf "$saved"' EXIT
survivors=0
check() {   # old new name
    restore
    python3 - "$1" "$2" "${files[@]}" <<'PY' || { echo "mutate: pattern missing: $3"; exit 2; }
import sys
old, new, *paths = sys.argv[1:]
for p in paths:
    s = open(p).read()
    if old in s:
        open(p, "w").write(s.replace(old, new, 1)); break
else:
    sys.exit(1)
PY
    if (cd "$here/../../kernel" && alr exec -- gprbuild -p -q -P "$here/vm_uart_tests.gpr" >/dev/null 2>&1 \
          && "$here/build/main" >/dev/null 2>&1 \
          && alr exec -- gnatprove -P "$here/vm_uart_tests.gpr" -u guest_uart.adb -u serial_keys.adb \
               -u console_line.adb --level=2 -j2 --checks-as-errors=on --report=fail >/dev/null 2>&1); then
        echo "mutant SURVIVED: $3"; survivors=$((survivors + 1))
    else
        echo "mutant killed: $3"
    fi
}
check "package Guest_UART with" "package Guest_UART with" "baseline (must SURVIVE)"
[ "$survivors" = 1 ] || { echo "vm-uart: baseline fails" >&2; exit 1; }
survivors=0
check "               U.Head := (U.Head + 1) mod FIFO_Depth;" "               null;" \
      "reading RBR does not advance the FIFO"
check "      U.Store ((U.Head + U.Count) mod FIFO_Depth) := B;" "      U.Store (U.Head) := B;" \
      "a received byte overwrites the oldest"
check "      Raise_IRQ := Line (U) and then not Before;" "      Raise_IRQ := U.Count = 1;" \
      "the first byte raises even while OUT2 gates the line"
check "      Raise_IRQ := Line (U) and then not Before;" "      Raise_IRQ := False;" \
      "received data never raises"
check "      Effect.Raise_IRQ := Line (U) and then (not Before or else Retrigger);" \
      "      Effect.Raise_IRQ := Line (U) and then not Before;" "a THR write while THRE is pending raises nothing"
check "               Effect.Discarded := U.Count;" "               Effect.Discarded := 0;" \
      "a FIFO reset discards silently"
check "              or (if U.Count > 0 then LSR_Data_Ready else 0);" ";" "LSR never shows data ready"
check "     (if (U.IER and IER_Received_Data) /= 0 and then U.Count > 0 and then U.Count >= Trigger_Level (U)
      then IIR_Received_Data
      elsif (U.IER and IER_Received_Data) /= 0 and then U.Count > 0 then IIR_Character_Timeout" \
      "     (if (U.IER and IER_Received_Data) /= 0 and then U.Count > 0 then IIR_Character_Timeout" \
      "IIR reports the timeout at the trigger level"
check "     (Identity (U) /= IIR_No_Interrupt and then (U.MCR and MCR_OUT2) /= 0);" \
      "     (Identity (U) /= IIR_No_Interrupt);" "OUT2 does not gate the line"
check "(if FIFO_Enabled (U) then FIFO_Depth else 1)" "FIFO_Depth" "sixteen bytes held without a FIFO"
check "         when Scan_Up        => return Cursor_Key ('A');" "         when Scan_Up        => return Cursor_Key ('B');" \
      "Up sends Down's sequence"
check "            return One (L - Control_Offset);" "            return One (L);" "Ctrl+letter sends the letter"
check "         when BS => L.Cursor := L.Cursor - Natural'Min (1, L.Cursor);" "         when BS => null;" \
      "BS does not move the cursor"
[ "$survivors" = 0 ] && echo "vm-uart: mutation OK" || { echo "vm-uart: $survivors survived" >&2; exit 1; }
