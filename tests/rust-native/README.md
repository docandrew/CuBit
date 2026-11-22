# Rust native bootstrap regression

Run `nix develop -c bash tests/rust-native/run.sh` from the repository root.
Requires the normal development NVMe image (`kernel/nvme_disk.img`). The runner
uses a temporary copy and the standard headless harness; it does not modify
the contents of the user's base disk. Test apps are not added to normal desktop
or LiveCD startup plans.

Three distinct checks:

1. Linux-hosted ELF inspection: no dynamic loader or undefined symbols, correct
   stack contract, BSS, no writable/executable LOAD segment, expected manifest
   bytes, identical loaded bytes for both authority variants, syscall-number
   agreement with the kernel enum.
2. Linux-hosted Rust unit tests: slot bounds and rejection of malformed local
   messages before any syscall. These are not guest tests.
3. Native CuBit under QEMU: BSS initialization, all four IPC words (including
   high-bit values), successful Clock replies, absent-authority failure, saved
   reply capability lifetime, and two separate processes completing the fixture.

Native markers in `/tmp/cubit-rust-native-serial.log`:

```
rust-probe: Hello from Rust! (IPC)
TEST: PASS rust-clock-authorized
TEST: PASS rust-clock-denied
TEST: PASS rust-native
```

The first marker occurs for each probe. The existing headless fault scan rejects
`TEST: FAIL` and kernel exception signatures. The Clock rejection is established
by the paired fixture and inspected manifests, not a detailed denial code from
the syscall (which currently returns a null tag for multiple failure causes).

Related checks:

```sh
nix develop -c python3 tests/ccl-manifests/test-manifests.py -v
nix develop -c bash -c 'cd userspace/rust && CUBIT_BINDINGS_DIR="$PWD/build" cargo clippy --locked --offline --release --workspace -- -D warnings'
```

No formal proof is claimed. This is executable cross-language ABI and authority
regression coverage, not proof of the kernel or Rust's entire memory-safety story.
