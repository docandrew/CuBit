# Volume descriptions

`CuBit.Volume_Descriptions` (userspace/runtime/gnat): the
`Volume.Description.V1` record `Queue_Describe_Volume` answers
(docs/filesystem-protocol-v2.md step 7). Hosted, Linux:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/volume-descriptions/volumes.gpr && ../tests/volume-descriptions/build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/volume-descriptions/volumes.gpr -u cubit-volume_descriptions.adb --level=2 --report=all -j4'
```

The tests check round trips and that every record Decode accepts, damaged
or random, re-encodes to the same bytes. The proof (level 2) covers no
run-time errors and Decode accepting only Valid records. The guest test is
storage-check's `QUEUE-VOLUME-CHECK` (headless storage-grants).
