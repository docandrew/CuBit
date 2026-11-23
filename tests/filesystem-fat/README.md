# FAT32 (FS-030)

filesystem.svc's FAT32 backend: the SPARK core (`fat_format`, `fat_names`,
`fat_chains` in userspace/services/filesystem), the driver over it
(`fat32`), and the dispatch beside ext2 (`fs_volumes`). The write order and
mount policy are in `fat32.ads`.

## Proof (core)

```sh
export TMPDIR=/home/doc/cubit-build-tmp
cd tests/filesystem-fat
systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 nix develop ../.. -c \
  gnatprove -P fat_core.gpr -u fat_format.adb fat_names.adb fat_chains.adb fat_chain_instances.ads \
  --level=1 -j2 --checks-as-errors=on
```

Level 1, nothing unproved: no run-time error for any bytes (boot sector,
FSInfo, FAT entries, directory entries, long-name parts, names, dates), and
the contracts: an admitted geometry's regions fit the volume and every FAT
holds every cluster; FAT entries keep their top four bits; a chain walk
never returns a cluster outside the data region and stops (Brent's cycle
check plus the bound of one link per cluster); the free-cluster ledger
never counts more free clusters than exist. `fat_chain_instances` proves
the walk and free-search generics for any FAT reader (the driver's reads
the device).

## Hosted tests

```sh
nix develop ../.. -c bash -c 'gprbuild -p -P fat_core.gpr && build/core/main'    # FAT-CORE-CHECK: PASS
nix develop ../.. -c bash -c 'gprbuild -p -P fat_driver.gpr && python3 run.py'   # FAT-DRIVER-CHECK: PASS
```

`main.adb` checks the core against oracles (FATGEN's checksum as its C
reads, Ada.Calendar for dates, random boot sectors and names). `run.py`
runs the production driver on images made at test time with mkfs.fat and
mtools (or bytes written from FATGEN's layout): no stored images. Only the
block IPC is replaced (tests/filesystem-interop/fixture), so this is not
native CuBit integration. It checks:

- geometries (512 B to 64 KiB clusters, 4 KiB sectors, one FAT, more
  reserved sectors): the driver reads a tree Linux tools wrote, then writes
  a workload (directories, long and Unicode names, odd write slices, sparse
  growth, truncation both ways, renames across directories, a replacing
  rename, removals) onto free space full of old bytes; `fsck.fat -n` must
  find nothing and mtools must read back exactly the model;
- names: aliases with numeric tails (`LONGFI~1`, `LONGF~10`), plain aliases,
  case-insensitive lookup, Unicode (an astral character too), refusals
  (`:`, device names, a trailing dot or space, control characters, `"`);
- mount policy: the first write clears FAT[1]'s clean bit, a volume left
  dirty or with the hard-error bit or the boot sector's dirty bit mounts
  read-only, a flush sets the bit again, FSInfo nonsense is ignored and an
  exact count is written;
- 31 adversarial images: cyclic, broken, out-of-range and bad-marked
  chains, a size past the chain, bad cluster fields, long names with a bad
  checksum or ordinal, orphaned parts, directory cycles and bad clusters,
  and fourteen bad boot sectors: no hang, no crash, refusals as expected;
- a partition window (an MBR partition at sector 2048);
- power cuts after every write of seven workloads (append, mkdir and
  create, unlink and rmdir, rename, replacing rename, truncate, directory
  growth), with a volatile write cache losing nothing, everything, all but
  the newest request, or random subsets of what was not yet flushed
  (`CUBIT_TEST_CUT_LOSES`): `fsck.fat -n` may report only what `fat32.ads`
  lists (lost clusters, a chain longer than the size, FATs that differ,
  orphaned long-name parts, shared clusters after a cut rename), and after
  `fsck.fat -a` no committed data is lost and nothing new shows bytes it
  was not given. `FAT_CUT_FINDINGS=1` prints what fsck reported.

`--quick` runs smaller images and fewer cut modes.

## Mutations

```sh
nix develop ../.. -c bash mutate.sh     # MUTANTS: killed N of N
```

Proof mutants in the core, hosted-test mutants for what its contracts do
not state, and driver mutants (write order and barriers, both FATs,
allocation, dirty marking, long-name checksums, chain checks, aliases,
`..`), each killed by the run.py section named in the script.
