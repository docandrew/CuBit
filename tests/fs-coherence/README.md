# Cross-process filesystem coherence (FS-022)

One case per program (`cases.txt`), built from `fs-coherence.c`: the
program prepares files, starts its other process (CuBit: itself again with
`posix_spawn`; Linux: `fork`), which changes them and `_exit`s without
closing anything. Once `waitpid` returns, and 500 ms later (so the service
has finished the other process's teardown), the program checks what the
names hold. Table and history: docs/filesystem-data-plane.md, "Cross-process
coherence tests".

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c tests/fs-coherence/host.sh              # Linux semantics, on the host
# CuBit, after make -C kernel world, under the build lock:
flock coordination/build.lock nix develop -c bash -c \
  'tests/fs-coherence/build-cubit.sh && tests/fs-coherence/run-cubit.sh 3'
tests/headless/run.sh --test fs-coherence-exit-left-open --accel kvm --timeout 60   # one case
```

Markers: `fs-coherence: start CASE`, `fs-coherence: CASE PASS` (or `FAIL`
and what was wrong), `fs-coherence: end` (QEMU stops there). Files live
under `@nvme:0/fs-coherence` (made by the headless runner on its disk copy).
The guest gets 512 MiB, as bench-fs: each process lends the service about
9 MiB of queue arenas, and at the default 128 MiB the desktop's services
leave too little for two.
tests/filesystem-hosted/run.sh runs host.sh with the other hosted suites.
