# Grouped keyboard retention regressions

From the repository root, use the pinned Nix environment:

```
nix-shell tests/compositor/vulkan-affine-shell.nix --run 'python3 tests/keyboard-pending/run.py --source-root . --toolchain-root .'
```

The runner copies actual source into a unique /tmp workspace, checks input
hashes before/after execution, uses Alire GNAT16 and runs eight actual PS2 Main
cases with mocked ports/IPC, plus grouped queue tests and level-2 SPARK proof.
Result JSON, logs and proof diagnostics remain in the printed directory.
It does not modify shared build outputs or require the shared build lock.

Coverage: existing pointer normal/overflow/replacement; refused keyboard
prefix; refused suffix after prefix delivery; grouped keyboard overflow;
consumer replacement with queued input; consumer replacement between prefix
and suffix. Policy tests cover all queue occupancies, E0/E1 grouping and loss.

Proof covers the pure Input_Pending/Keyboard_Pending policy, not controller
I/O, kernel transport or Desktop. The driver fixture exercises actual PS2 Main;
it does not model xHCI. Native PS2/USB delivery, real and injected refusal,
consumer replacement, negative controls and the Desktop prefix-recovery fix
are recorded in docs/compositor-evidence/keyboard-driver-integration/.
E1 grouping is not new Pause-key decoding or a full held-key snapshot protocol.
