# Startup timing and Global Lock diagnostics

Run in Nix with TMPDIR=/home/doc/cubit-build-tmp: bash tests/acpi-startup-diagnostics/run.sh. Checked and release hosted tests exercise nested timing, clock degradation, bounded full-identity trace authorization and one-shot dumps, actual FACS atomic operations on allocated hosted RAM, and the native reporter with modeled transport/logger. No physical hardware, kernel or image execution, no new SPARK proof. Native semantic evidence is recorded separately.

The 12-record trace copies existing callback observations; zero placeholders are only meaningful when their presence/availability flags say so. CAS records retain the last attempt plus the attempt count, not every intermediate attempt. The dump request has a fresh 250 ms diagnostic-only deadline after startup closes; it cannot renew AML/hardware deadlines. Dumps remain pending while the physical owner is held or uncertain.
