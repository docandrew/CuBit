# RAM Block.Device.V1 provider

Run Linux-hosted tests in Nix:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/ram-block/ram_block_tests.gpr && ../tests/ram-block/build/main'
```

This compiles the production RAM block dispatcher. Only the kernel grant API
is replaced by an instrumented fixture. It checks description/volatile flags,
bounded initialization, rejected rebinding, 36 rejected requests (including
unsupported flush), both access directions, stale/foreign grants, buffer bounds,
multi-sector and last-sector transfers, unchanged bytes outside the transfer,
and uncertain completion after a failed grant return. These are regression
tests, not SPARK proofs or evidence that the stub proves kernel enforcement.

The native counterpart uses real CuBit IPC and kernel grants:

```sh
nix develop -c make -C kernel ramdisk filesystem devmgr storage-check
nix develop -c bash tests/headless/run.sh --test storage-grants --accel tcg,thread=multi --timeout 60 --serial /tmp/ram-storage.serial --keep-logs
```

The storage fixture adds a fresh live RAM-volume seed to the normal development
bootstrap; it does not modify a user's disk. It requires both the driver's ready
marker and `RAM-VOLUME-CHECK: PASS`: scoped create, positioned write across a
sector boundary, zero-filled prefix, reopen/readback, volatile flush rejection,
truncate and EOF. Existing NVMe/grant/authority regressions run alongside it.
The volume-list integration also keeps RAM and NVMe handles open together and
checks that truncating a RAM alias cannot change the NVMe file.
The diagnostic now requires the RAM seed as well as its NVMe fixture.

Native RAM and NVMe storage checks passed under four-vCPU QEMU TCG. This does not
measure hardware latency or establish zero-copy I/O: filesystem staging remains,
and separating RAM storage introduces IPC. Laptop/USB hardware was not tested.
Run headless fixtures sequentially because they share image staging.
