# Filesystem queue wakes

`Queue_Wakes` (userspace/services/filesystem) decides when filesystem.svc
answers a client's held wake request (`OP_FS_WAKE`,
docs/filesystem-protocol-v2.md step 1). Hosted, Linux:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/filesystem-queue-wakes/wakes.gpr && ../tests/filesystem-queue-wakes/build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/filesystem-queue-wakes/wakes.gpr -u queue_wakes.adb --level=2 --report=all -j4'
```

The tests run the fixed cases (answered at once with answers waiting, held
otherwise, superseded, woken by an answer, answered at the queue's end, a
failed hold) and 1,000 random sequences of 200 events against a counting
reference: wakes held = held wakes answered + (1 while one is held).

2026-10-08: `QUEUE-WAKES: 80036 checks PASS`; GNATprove level 2: 11 checks,
0 unproved (postconditions: a held wake is answered on supersede, answer and
end; a request with answers waiting is answered at once).
