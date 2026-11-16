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
 n=split("acpi: stage runtime ok LOCAL_IDLE|acpi: stage runtime-selection ok one-service mode=LOCAL_RUNTIME|acpi: SCI armed|acpi: SCI 1 fixed=[PWRBTN ]|power: power button pressed (FIXED_EVENT, count 1)|power: POWER_OFF requested by POWER_BUTTON|power: procmgr stopped the apps|power: asking acpi.svc to prepare to sleep (S5)|power: prepare to sleep PREPARED|procmgr: shutdown: stopping com.cubit.test.shutdown-stubborn pid|procmgr: shutdown: stopping com.cubit.test.shutdown-cooperative pid|shutdown-cooperative: quiesced; exiting 0|procmgr: shutdown: stopping com.cubit.logstore pid|FS: stop: storage COMMITTED|devmgr: shutdown: drivers stopped|procmgr: shutdown: stopped in|power: procmgr stopped the system:|power: asking acpi.svc to enter S5", need, "[|]")
 # From logstore'"'"'s stop on (logstore, the storage commit, the drivers,
 # the procmgr report, then the power.svc lines), everything comes within
 # milliseconds of S5 now that services and drivers end on Stop at once,
 # and S5 can stop the kernel console ring before it drains them (seen cut
 # at "stopped i", and inside devmgr'"'"'s lines: LOG-009). So the log may end
 # anywhere in that tail: each is optional, but the log is a prefix, so
 # once one is missing every later one must be too (a later line without
 # an earlier one is a misordering). The QEMU exit 0 above is the S5
 # evidence: a halt keeps QEMU running to the timeout; the disk checks in
 # run.sh are the storage evidence; tests/driver-stop checks the drivers.
 tail_first=13
}
{ for(i=1;i<=n;i++) if(index($0,need[i])) { count[i]++; if(!at[i]) at[i]=NR }
  if(index($0,"procmgr: shutdown: keeping com.cubit.acpi pid")) kept=1
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
   if(at[i]<=last) { print "acpi-shutdown: misordered: " need[i] > "/dev/stderr"; exit 1 }
   last=at[i]
 }
 if(at[n] && !at[n-1]) { print "acpi-shutdown: S5 asked without the stop report" > "/dev/stderr"; exit 1 }
 if(at[n-1] && !at[n-2]) { print "acpi-shutdown: power.svc stop report without procmgr report" > "/dev/stderr"; exit 1 }
 if(!kept || !forced) { print "acpi-shutdown: missing power-tail or forced-stop evidence" > "/dev/stderr"; exit 1 }
}' "$serial"
echo 'acpi-shutdown: real service SCI/watch/prepare/stop/S5 evidence PASS'
