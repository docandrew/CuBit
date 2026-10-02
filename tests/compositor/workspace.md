# Logical workspace admission

The native compositor has no logical scene image. `Compositor_Workspace.Valid`
therefore bounds coordinates and integer stride arithmetic independently of
pixel allocation. Each real output still uses the existing physical-buffer
validation and allocation ledger. The legacy compositor retains its scene
capacity checks and allocations.

Run in Nix:

```sh
cd kernel
alr exec -- gprbuild -p -P ../tests/compositor/workspace.gpr
../tests/compositor/build/workspace/workspace_tests
alr exec -- gnatprove -P ../tests/compositor/workspace.gpr -u compositor_workspace.ads --level=2 -j1 --report=all
```

The boundary fixture exercises every admitted height, its maximum width and
the first rejected width. It also rejects empty/oversized extents and admits
logical dimensions exceeding the former 16MiB scene cap. This is not evidence
that a particular large physical scanout is supported: per-output buffers still
have their existing size limit.

After a native headless run with the configured tightly packed virtual modes,
check the actual root-owned pixel allocation ledger:

```sh
python3 tests/compositor/check-native-storage.py SERIAL_LOG 1024x768 1280x720
```

Use `--retired` for a fixture that requests Desktop teardown and verifies all
target readers have retired. Stopping QEMU alone is not a teardown test.
The checker requires exactly three allocations per output, correct cumulative
charges and the explicit zero-scene marker. With `--retired`, it also requires
each allocation to be released and the final charge to reach zero. It does not
count Mesa, font, application or other process memory.
