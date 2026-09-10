# Desktop protocol checks

From the repository root, using the Nix environment:

```sh
nix develop -c make -C kernel test-desktop-protocol prove-desktop-protocol
```

The test uses the production portable protocol and grant-reference sources,
staged under `build/` to avoid importing CuBit's freestanding Ada runtime into
the hosted test.
Assertions and overflow checks are enabled only in this hosted executable.

See [the protocol specification](../../docs/desktop-protocol.md) for the exact
proven properties, unproved round-trip properties covered by tests, and the
native `desktop-protocol` QEMU adversarial test. None of these proves the
whole compositor, grant lifetime, kernel IPC authentication or latency.
