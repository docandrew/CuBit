# Owned values initialized during execution

Run from the repository root, using the Nix environment:

```sh
nix develop -c bash tests/ccl-owned-locals/run.sh
nix develop -c bash tests/ccl-owned-locals/run.sh --prove
```

These are Linux-hosted tests of the shared, freestanding CCL VM and ownership
verifier. Assertions/checks are enabled in this test binary, not native CuBit
release builds. They do not call Config, mint OS handles or grant authority.

Coverage:

- Move into a dynamic local, inspect its sole available owner, and move out to
  the host; all 32 ownership tags and all three ownership modes, each executed
  after a CCLB encode/decode/verification round trip.
- Reject copied owned values, literal-to-owned initialization (including tag
  zero), wrong nominal tags, source reuse, destination redeclaration, and moves
  during read/write borrows.
- Preserve tags across copy-stack/drop-under-top and distinguish tags at branch
  joins. Execute both branches of a valid ownership transfer.
- Reject a moved value abandoned below the returned top-of-stack value.
- Account for completion operands of all four local-argument import transfer
  modes; execute completion-to-local initialization and reject full-stack calls.

The two verifier passes have separate responsibilities. `CCL.VM.Verify`
validates operand shape, nominal ownership tag and move/copy eligibility.
`CCL.Ownership.Bytecode.Verify` receives that lowered control flow and tracks
declaration, availability, borrows, dispositions and scope completion. The
latter alone is not an operand/type admission boundary.

The host still owns returned resources and cleanup on stop, fuel exhaustion or
failed calls. These checks do not prove external resource cleanup or connection
to the CCL resource registry. Factory returns and source ownership lowering are
separate remaining integration work; no integer here represents a Config handle.

Validation (2026-09-25): 1,182 hosted checks pass. Focused GNATprove level 2
discharges 212 checks for `ccl-vm.adb` and `ccl-ownership-bytecode.adb`, with no
unproved or justified checks and no added assumptions/SPARK exclusions. This
is the selected units' runtime-safety and existing-contract proof, not a full
formal proof of the verifier's semantic soundness or the complete CCLB codec.
