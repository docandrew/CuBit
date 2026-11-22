# The pure ext2 core: format, geometry, bitmaps

`Ext2_Format` (numbers, on-disk constants, directory-record and inode-field
codecs), `Ext2_Geometry` (superblock admission and where inodes, group
descriptors and bitmap bits are) and `Ext2_Bitmaps` (block/inode bitmap
reservation and release) are the SPARK core of the filesystem service's ext2
driver (CODE-002). Each proves at level 2 with nothing unproved:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/filesystem-format/format.gpr -u ext2_format.adb ext2_geometry.adb ext2_bitmaps.adb --level=2 -j2'
```

`main.adb` (Linux-hosted) checks them against the arithmetic the driver used
before the split, kept here as the oracle: 300,000 random superblocks
(`Supported`), 300,000 random valid geometries (every location function),
200,000 random directory records, every i_mode, name length and file type,
and 5,000 random bitmaps (`Take_Free` against the old scan, then release).

```sh
nix develop -c bash -c 'cd tests/filesystem-format && gprbuild -p -P format.gpr && build/main'
```

It prints `FORMAT-CORE-CHECK: PASS` and the number of checks. The units also
prove inside tests/filesystem-truncate/accounting_proof.gpr (level 1), where
tests/filesystem-truncate/mutations.sh plants six mutants in them.
