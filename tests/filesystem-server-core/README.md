# Filesystem service pure core

The units filesystem.svc's request handlers (`Fs_Server` and its children,
userspace/services/filesystem) use for their pure logic, proved at level 2
and tested hosted on Linux against the formulas main.adb used before they
were extracted (kept in main.adb here as references):

- `Fs_Handle_Codes`: wire handles (one-based slot, generation), dirty-entry
  tags, object identities, the open-object and inode-version hashes, the
  version-table probe, directory checks and resume tokens. Slots,
  generations, wire handles, tags, checks and tokens are distinct types.
- `Fs_Path_Checks`: `..` traversal detection and the canonical relative
  path watches and change events match.
- `Fs_Reply_Labels`: the reply label each Ext2 outcome is answered with
  (proof only: success and only success is `REPLY_OK`).

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c bash -c 'cd tests/filesystem-server-core && gprbuild -p -P server_core.gpr && build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/filesystem-server-core/server_core.gpr -u fs_handle_codes.adb fs_path_checks.adb --level=2 --report=all -j2'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/filesystem-server-core/labels_proof.gpr -u fs_reply_labels.ads --level=2 --report=all -j2'
```

The test checks every slot with nine generations at the edges (round
trips, the old formulas, tags), 200,000 random words against the validity
rule, 20,000 random identities per volume against the old hash, check and
token formulas (and the zero-mix case), the version probe over the whole
table, and every path over `./a` up to eight characters at two lower bounds
against the old traversal and canonical code: about 3 million checks
(`SERVER-CORE-CHECK: PASS`). Mutants of the trailing-separator trim, the
traversal end rule, the tag width and the nonzero check are each caught.

`labels_proof.gpr` lists the Ext2 spec and the specs it names (the
filesystem-interop fixture's CuBit.Messages); keep it in step if those
specs' dependencies change.
