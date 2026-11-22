# Server-side copy slices

`Copy_Slices` (userspace/services/filesystem): how filesystem.svc cuts a
`Queue_Copy` into bounded slices and how a copy ends
(docs/filesystem-protocol-v2.md step 5). Hosted, Linux:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/filesystem-copy/copy.gpr && ../tests/filesystem-copy/build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/filesystem-copy/copy.gpr -u copy_slices.adb --level=2 --report=all -j4'
```

The tests run 20,000 copies against a source that grows and shrinks while
they run, with cancels, deadlines and failed slices: every slice stays
within its limits, the bytes copied are a prefix, and every ending occurs.
The proof (level 2) covers no run-time errors and the contracts. The guest
test is storage-check's `QUEUE-COPY-CHECK` (headless storage-grants).
