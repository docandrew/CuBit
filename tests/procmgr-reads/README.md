# Exact executable reads

Run through Nix:

```
nix develop -c make -C kernel test-procmgr-reads
```

The hosted test executes the **same generic sequencing implementation** used
by procmgr, with an injected transfer boundary. It checks empty inputs,
page/window boundaries, >16 MiB images, `Natural'Last`, and first/middle/last
short replies, oversized replies, read errors, and retirement errors. No next
window is issued after a failed transfer. These are tests, not a proof of the
IPC/grant adapter or file immutability.

The separate `Read_Proof.Check` instantiation supplies arbitrary reply length
and status. GNATprove proves the shared loop's arithmetic checks,
initialization, progress, and termination (ten proved diagnostics):

```
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/procmgr-reads/read_tests.gpr -u read_proof.adb --level=2 --report=all --checks-as-errors=on -j2'
```

This does not prove that the actual IPC callback returns or retires its grant;
those remain trusted integration boundaries and native regression targets.

Native desktop startup additionally exercises an executable larger than the
16 MiB maximum single grant. Every transfer is directly into its final buffer
offset via a <=1 MiB grant; no extra payload copy is introduced. The adapter
rejects incomplete replies, closes the file, and quarantines the buffer on
unconfirmed retirement. A regular file changing during a multi-window read is
not a cryptographic snapshot; signed executable admission remains separate.
