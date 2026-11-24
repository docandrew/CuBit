#!/usr/bin/env bash
set -euo pipefail
serial=${1:?serial log required}
status=${2:?QEMU exit status required}
if [ "$status" != 0 ]; then
    echo "acpi-shutdown: QEMU did not power off by itself (status $status)" >&2
    exit 1
fi
if grep -aqE 'shutdown-standin:|acpi-backend:|power: halted|STILL_RUNNING|STARTUP_REFUSED|Ada heap: cannot allocate|acpi: SCI unavailable|acpi: SCI acknowledgement refused|power: acpi.svc did not answer Prepare_To_Sleep|power: acpi.svc could not keep the power-button watch|com\.cubit\.acpi pid[0-9]+ exited' "$serial"; then
    echo "acpi-shutdown: stand-in, failed native service, or unsuccessful shutdown" >&2
    exit 1
fi
awk '
BEGIN {
 n=split("acpi: stage runtime ok LOCAL_IDLE|acpi: stage runtime-selection ok one-service mode=LOCAL_RUNTIME|acpi: SCI armed|acpi: SCI 1 fixed=[PWRBTN ]|power: power button pressed (FIXED_EVENT, count 1)|power: POWER_OFF requested by POWER_BUTTON|power: procmgr stopped the apps|power: asking acpi.svc to prepare to sleep (S5)|power: prepare to sleep PREPARED|procmgr: shutdown: stopping com.cubit.test.shutdown-stubborn pid|procmgr: shutdown: stopping com.cubit.test.shutdown-cooperative pid|shutdown-cooperative: quiesced; exiting 0|logstore: storage stopping:|procmgr: shutdown: logstore detached its storage:|procmgr: shutdown: the filesystem commits|FS: stop: storage COMMITTED|procmgr: shutdown: storage COMMITTED|devmgr: shutdown: drivers stopped|procmgr: shutdown: stopped in|power: procmgr stopped the system:|power: asking acpi.svc to enter S5", need, "[|]")
 # logstore remains alive for the final diagnostics. It detaches its
 # storage after services stop and before filesystem commit, then drivers.
 # S5 may truncate this final console tail (LOG-009): require an ordered
 # prefix, never require logstore itself to stop. QEMU exit 0 is the S5
 # evidence; run.sh checks the disk and tests/driver-stop checks drivers.
 tail_first=13
}
{ for(i=1;i<=n;i++) if(index($0,need[i])) { count[i]++; if(!at[i]) at[i]=NR }
  if(index($0,"procmgr: shutdown: keeping com.cubit.acpi pid")) kept=1
  if(index($0,"procmgr: shutdown: keeping com.cubit.logstore pid")) kept_logs=1
  if(index($0,"procmgr: shutdown: stopping com.cubit.logstore pid")) stopped_logs=1
  if(index($0,"did not end within its grace of 500 ms; ending it")) forced=1
}
END {
 for(i=1;i<=n;i++) if(count[i]>1 && (i==4 || i==5 || i==6 || i==8 || i==9 || i==n)) {
   print "acpi-shutdown: repeated transition: " need[i] > "/dev/stderr"; exit 1
 }
 last=0; cut=0
 for(i=1;i<=n;i++) {
   if(!at[i]) {
     if(i>=tail_first) { if(!cut) cut=i; continue }
     print "acpi-shutdown: missing: " need[i] > "/dev/stderr"; exit 1
   }
   if(cut) { print "acpi-shutdown: \"" need[i] "\" without \"" need[cut] "\" before it" > "/dev/stderr"; exit 1 }
   # The power.svc apps-stopped/prepare/PREPARED records (7..9) come from a
   # different producer than the procmgr service stops (10..): the logstore
   # mirror may print one producer records a few lines late. procmgr only
   # stops services after the power.svc prepare reply (causally), so order
   # 7..9 among themselves and after 6, not against the procmgr lines.
   if(i>=7 && i<=9) continue
   if(at[i]<=last) { print "acpi-shutdown: misordered: " need[i] > "/dev/stderr"; exit 1 }
   last=at[i]
 }
 # power.svc logs PREPARED before asking procmgr to stop services, but
 # the mirror can print that record after procmgr starts. Preserve the
 # power producer request/reply/entry order, not a cross-producer order.
 if(at[7]<=at[6] || at[8]<=at[7]) { print "acpi-shutdown: misordered apps stop or prepare request" > "/dev/stderr"; exit 1 }
 if(at[9]<=at[8] || (at[n] && at[9]>=at[n])) { print "acpi-shutdown: misordered preparation reply" > "/dev/stderr"; exit 1 }
 if(at[n] && !at[n-1]) { print "acpi-shutdown: S5 asked without the stop report" > "/dev/stderr"; exit 1 }
 if(at[n-1] && !at[n-2]) { print "acpi-shutdown: power.svc stop report without procmgr report" > "/dev/stderr"; exit 1 }
 if(stopped_logs) { print "acpi-shutdown: logstore stopped before the final power tail" > "/dev/stderr"; exit 1 }
 if(!kept || !kept_logs || !forced) { print "acpi-shutdown: missing power-tail or forced-stop evidence" > "/dev/stderr"; exit 1 }
}' "$serial"
echo 'acpi-shutdown: real service SCI/watch/prepare/stop/S5 evidence PASS'
