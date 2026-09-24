# Canonical grant-reference IPC encoding

The pure production `CuBit.Grant_References` package encodes the complete
32-bit generation and bounded global slot into one 64-bit IPC word. Callers
must validate with `Valid_Wire` before `Decode`: generation zero and out-of-range
low fields are invalid, never masked into valid identities. References are identities,
not authority: the kernel's existing acquisition checks remain mandatory.

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/grant-references/references.gpr && ../tests/grant-references/build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/grant-references/references.gpr -u cubit-grant_references.ads codec_proof.adb --level=2 --report=all -j4'
```

The Linux-hosted test checks all 4096 slots at four generation boundaries,
including the maximum generation, plus malformed encodings. Assertions are
enabled in this hosted test only, not in the kernel or native runtime.

GNATprove on 2026-09-23: **20 checks, zero unproved, zero justified**. The Ghost
proof harness establishes both round-trip directions for arbitrary valid
inputs. No `Assume` or SPARK-Off escape hatch. This proves codec safety and
identity preservation, not filesystem correctness, authorization or DMA safety.

Native integration: `storage-grants` now requires `POSITIONED-IO-CHECK: PASS`.
It exercises the codec through real read/write-at FS IPC, including malformed
words, stale identities, buffer rights and cursor preservation.
