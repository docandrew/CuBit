# Boot-to-buddy admission boundaries

This round fixes two production off-by-one errors and makes free-list count
initialization explicit. It does not claim to prove the entire boot allocator.

1. The boot allocator's high-water mark is inclusive. A buddy block starting
   exactly there must consult the boot bitmap for that first frame; it cannot
   be admitted wholesale. Previously the test used `highest > first` instead
   of including equality.
2. A bitmap containing N bits owns PFNs `0 .. N - 1`, not `0 .. N`. Previously
   `MAX_BOOT_PFN` equaled N, allowing the setup/search paths to access the word
   immediately beyond the bitmap. The configured 64 MiB bitmap has 16,384 bits;
   its inclusive last PFN is 16,383. This was a real unchecked access risk with
   kernel runtime bounds checks disabled.

`Buddy_Boot_Admission` supplies the production source-selection and word/bit
index operations. `Last_Frame` is an Ada 2022 static expression function, so the
kernel's actual `MAX_BOOT_PFN` constant comes directly from the proved formula,
evaluated at compile time. Allocation-search postconditions now consistently
use inclusive last-frame arithmetic.

When a block crosses the boot allocator's range, frames through its inclusive
high-water mark consult the bitmap; frames above it do not. This avoids an
out-of-range bitmap query while preserving the firmware-region admission check.
Buddy list setup explicitly initializes each sentinel's count to zero.

## Verification

Run from the repository root, through Nix:

```sh
nix develop -c bash tests/buddy-boot-admission/prove.sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/buddy-boot-admission/admission_tests.gpr && ../tests/buddy-boot-admission/build/main'
nix develop -c make -C kernel cubit_kernel
nix develop -c python3 tests/buddy-boot-admission/check-codegen.py
nix develop -c python3 tests/intrusive-list-splices/check-codegen.py
nix develop -c tests/headless/run.sh --test capability-security --disk kernel/capability_security_disk.img --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-boot-admission-final-native-serial.log --keep-logs
nix develop -c env CUBIT_DOOM_MULTIAPP=1 tests/headless/run.sh --test desktop-doom --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-boot-admission-desktop-serial.log --keep-logs
```

The focused core passes **13 checks, none unproved or justified**. Properties
cover the inclusive high-water boundary, whole-tail admission, bitmap lookup
bounds and lossless word/bit decomposition. The proof reuses the existing
`tests/buddy-bitmap/why3.conf` CVC5 integer encoding to bridge unsigned machine
arithmetic and integer index bounds. No assumptions or suppressed obligations.

Host tests exhaust 133,120 small-arena intervals/high-water combinations and
2,105,344 bitmap indices, plus full 40-bit boundary cases. Explicit regressions
cover `first = highest`, crossing beyond the tracked range, and the first
unrepresented bitmap frame.

Pinned-toolchain inspection verifies that the kernel's getters each remain
three instructions (move, shift/mask, return), that the compiled bitmap guard
matches its actual storage capacity, and that helpers/Ghost proofs add no calls.
The regular buddy list insertion is unchanged. These are codegen checks, not
allocator latency measurements.

The final optimized build passed the four-vCPU KVM multi-app desktop/DOOM
fixture, including game pixels and responsive Apps-menu input. The security
fixture also passed on the final optimized build with eight partial-load
rollbacks and successful PID reuse.

## Remaining obligations

The later [boot reservation ADT](../boot-frame-allocator/README.md) replaces the
hand-written bitmap getters/mutations, removes the redundant free counter and
proves reservation coverage by the high-water mark. This directory's codegen
entry point now checks that actual packed representation through the new suite.

Firmware area validity, page alignment and conflicting overlapping-region
handling remain adapter obligations. These fixes do not establish the
full physical-address/metadata mapping or arena-wide allocation non-overlap.
The high-water rule applies to firmware-validated usable regions after boot
metadata has been reserved; it is not permission to admit arbitrary addresses.
