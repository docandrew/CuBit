# DesignWare controller stack regressions

Run in the pinned Nix environment with TMPDIR=/home/doc/cubit-build-tmp:

    bash tests/i2c-dw-stack/run.sh --output-root /absolute/private/output

Five hosted mains run checked and release: transfer61056, registers196742,
configuration5148, LPSS3995445 and owner331768:4590159checks per profile. The
runner pins inputs, snapshots sources, records commands and checks the exact
six production plus five test compiled-unit names and profile flags. Each build
uses j1/nice19 with180s limit; test execution has a30s limit.

The composed owner has work-bounded startup with zero real clock callbacks;
actual runtime transactions retain clock continuity and deadline checks. Work
counts are independent of16bit byte counts. A maximum65535-byte read is checked
byte-for-byte with sufficient work allowance, including separate command and
receive actions. Per-channel target/speed reconfiguration occurs only while
serialized and disabled/idle; unknown access effects freeze rather than replay.

These are Linux-hosted models, not live CuBit service or electrical validation.
Existing scoped planner/timing proofs do not prove the stateful owner, volatile
accesses or whole driver. New stateful source remains tested, not newly proved.
The retired saved-configure-reply wrapper is absent. Native binding, generic
loop, client grant ownership and kernel-event policy have separate suites in
 tests/i2c-dw-native; common wire tests are in tests/i2c-admission.
