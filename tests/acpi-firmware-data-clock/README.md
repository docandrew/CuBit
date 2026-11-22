# Firmware-data IPC clock domain

With TMPDIR=/home/doc/cubit-build-tmp exported before entering Nix: bash tests/acpi-firmware-data-clock/run.sh.

The actual firmware-data native frontend is exercised with modeled clocks, frame decoding, endpoint and timing. A frozen before-fix body must fail at the different available HPET epoch (clock regression2); corrected checked/release bodies each pass79 checks. Coverage includes zero/high-bit/ceiling/sentinel kernel milliseconds, unavailable high-resolution clock, expired/invalid IPC deadlines, valid exchange and preservation/disposal of carried handles on a kernel error.

IPC deadlines use GETTIME milliseconds. The fallback high-resolution microsecond clock may have a different HPET epoch; dividing it by1000 does not convert the epoch. Runtime Monotonic/Published_Clock sources are pinned for that audited contract. These are hosted regressions, not a SPARK proof, real kernel IPC test or hardware validation.
