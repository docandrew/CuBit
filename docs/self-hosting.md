# CuBit on CuBit: self-hosting plan

Status: plan, 2026-10-03. Goal: develop CuBit on CuBit. That means version
control (jj preferred over git), GNU binutils, gcc and GNAT/gprbuild running
natively, driven from the CCL console (no Unix shell or coreutils: see
docs/process-arguments.md and the no-Unix-userland decision).

## Where things stand

From a repository survey, 2026-10-03.

- **C:** musl 1.2.6 with CuBit system calls (`userspace/libc`). Files can be
  read and written; pthreads and futexes work; TCP works; `posix_spawn` and
  `waitpid` go through procmgr's OP_LAUNCH. Programs are static and non-PIE.
- **Rust:** std on the Unix-family target `x86_64-unknown-cubit` works
  (threads, time, args/env, files, TCP). Servo (85 MB static) runs, with
  rustls over libc sockets.
- **Storage:** ext2 is writable, with ext3/JBD2 journaling (data=ordered) and
  multi-GiB files.
- **Memory:** a 1 TiB owned-memory aperture per process, and up to 128 threads.
- **Not ported yet:** binutils, gcc, GNAT, gprbuild, git, jj.

## What blocks every tool

These come first, in this order:

1. **File metadata the tools rely on.** Done 2026-10-04; see "Item 1:
   what landed" below.
   - Real mtime: written on modification and returned by `stat`/`fstat`.
   - `ftruncate` (mapped to OP_RESIZE_FILE).
   - `readlink` answering `EINVAL` (this fixes `realpath`).
   - `access(W_OK)` telling the truth.
   - `select`.
2. **Working directory.** A per-process cwd (`chdir`/`fchdir`/`getcwd`),
   directory-relative `*at()` calls, and a cwd in OP_LAUNCH. Done
   2026-10-04; see "Item 2: what landed" below.
3. **Directory growth.** Create/rename/unlink in directories beyond direct
   blocks, and in htree directories. Needed by `.git/objects`, build trees and
   jj's store.
   - **3a, growth.** Reads already follow single and double indirect blocks.
     Writes were limited to the 12 direct blocks and to inodes with no flags.
     A directory with no room first grows by one empty block (a single unused
     record spanning it, as Linux ext2 does), written through the same
     ordered allocation path as file appends (`appendRun`) but as journaled
     directory data. The parent's size is published, and the name is then
     inserted as usual. A crash in between leaves only a valid empty block,
     and ENOSPC writes nothing. Create, unlink and rmdir work on every
     block.
   - **htree.** Linux-written `dir_index` directories read correctly as
     linear ones, since index blocks look like unused records. Before CuBit
     changes one, it clears `EXT2_INDEX_FL` in the inode, first and in the
     same transaction, so Linux treats it as a linear directory and never
     reads a stale index. `e2fsck -fD` can rebuild the index. Building
     indexes ourselves (the half-MD4 hash) comes later, if large-directory
     lookups need it.
   - **3b, POSIX rename.** Rename was limited to one directory and one block,
     and refused an existing destination. Git and jj need renames across
     directories (temporary files moved into `objects/xx/`) and atomic
     replacement (`index.lock` to `index`). Order without a journal, as Linux
     ext2 does it: raise the inode's link count, insert the new name
     (replacing the old target's record), drop the old name, lower the link
     count, then release a replaced target with no links left. A moved
     directory gets its ".." updated and its parents' link counts adjusted.
     Moving a directory into its own subtree is refused. With the journal,
     all of it is one transaction.
4. **Child processes that can work.** CuBit has no Unix pipes or
   descriptor inheritance. Processes exchange typed streams over IPC, and
   redirection is authorized stream wiring (docs/ccl-streams.md).
   - **Standard streams as declared streams.** A ported program's stdin,
     stdout and stderr are declared in its CCL manifest as plain byte streams
     (`Stream<Bytes>`) unless the program is known to produce something more
     specific. Today stdout and stderr exist, declared as text; stdin is
     missing.
   - **A launcher reads its children's streams:** the CCL launch built-in
     (item 5), or a C parent through the libc.
   - **Place delegation:** a child gets a declared subset of its launcher's
     filesystem places, e.g. the build directory and a temporary place.
   - **Not needed for the build tools** (2026-10-04 audit):
     - binutils open and write their files directly.
     - The gcc driver without `-pipe` passes temporary files between cc1,
       as and ld. libiberty adds `dup2`/`close` file actions only for
       `-pipe`, stderr-to-stdout or output files.
     - gprbuild captures compiler output by redirecting inherited
       descriptors, so the CCL build tool (item 6) drives gcc, gnatbind and
       gnatlink directly instead. It uses `.ali` files for dependencies, and
       each tool's stderr arrives as a stream.
     - `posix_spawn` file actions stay `ENOTSUP`. If a port ever needs one,
       the libc can map it onto stream wiring.
   - **Arguments are typed** (user decision, 2026-10-04). Programs never
     take a raw argv or envp. A legacy Unix tool is wrapped by its CCL
     manifest, which declares typed parameters (docs/ccl-launch-parameters.md)
     and how they map onto the argv the tool expects. A file name is its own
     type, passed as such. An input-file parameter can carry read access to
     exactly that file, and an output-file parameter create and write access:
     place delegation follows from the arguments.
   - **Open point, for item 5:** the launch block from item 2 carries raw
     argv and environment strings, and posix_spawn passes them through (gcc
     starting cc1). Under typed arguments, procmgr would check a C
     launcher's argv against the child's declared argument mapping and turn
     it into the typed value, refusing undeclared flags.
5. **Running programs from CCL.** In the CCL console, a launched program's
   stdout and stderr each show up as a new card, kept to inspect at leisure
   (user, 2026-10-04); binutils are the first programs to test it with.
   - **Progress (2026-10-04):** typed parameters in manifests, the proved
     `CuBit.Program_Parameters`, procmgr's `OP_PROGRAM_PARAMETERS`, and
     `CuBit.Launching.Describe`/`Wait` are in place, and binutils runs this
     way on CuBit (docs/ccl-launch-parameters.md, Status). The CCL side
     comes next.
   - A launch built-in: arguments, a cwd and a place.
   - stdout/stderr come back as `Stream<String>`, and the exit status as a
     value (docs/ccl-streams.md, phase 5).
   - A program's manifest is checked before it runs.
   - Output streams go into console-owned buffers that can be redirected and
     un-redirected while the program runs (docs/development-backlog.md,
     CCL-001).

### Item 1: what landed

- **Wall clock in the kernel.** clock.svc publishes
  `SYSINFO_WALL_CLOCK_OFFSET` (1403): UTC ms at monotonic 0, set only while
  its time is current. Only the registered clock driver may set it; device
  keys may now be set only by the registered devmgr. Before this change any
  process could set them. libc's `CLOCK_REALTIME` reads this offset with a
  single system call instead of making an IPC call to clock.svc. The old code
  marked wall time unavailable forever after one failed call.
- **ext2 times.** The following set mtime and ctime (ext2's i_ctime field):
  writes, size changes, creates (the new inode's atime too), and directory
  entry changes in the parent. Unlink sets the target's ctime. A time
  changes only when the second changes, so repeated writes cost at most one
  inode write a second. While wall time is unknown, times are left alone. Not
  done yet: the nanosecond `_extra` fields of 256-byte inodes keep their old
  values.
- **Describe.** `Queue_Describe` (15) returns a handle's
  `Directory.Inspection.V1`: size, the three times, mode, links, owner and
  `objectId` (volume << 32 | inode). Before answering, it takes in the
  pages a write-delegated handle has buffered. The record's `createdMs` was
  renamed `changedMs`, because it always held ctime. libc fills
  `st_mtim`/`st_ctim`/`st_atim`, `st_mode`, `st_nlink`, `st_ino`/`st_dev`
  and `st_uid`/`st_gid` from it.
- **ftruncate/truncate** go through `Queue_Resize` (16). The resize is
  queued, so it runs after the client's earlier writes.
- **access** opens the name itself. The open answer's new
  `Rights_Policy_Write` bit says whether this process's policy lets it write
  the file or create in the directory. `X_OK` uses the volume's mode bits.
- **readlink/readlinkat** return `EINVAL` for a name that exists (there are
  no symbolic links) and `ENOENT` for one that doesn't. `realpath` works.
- **select/pselect6** are built on poll. Signal masks are ignored, because no
  signals are delivered.

Tested on Linux hosts: the ext2 interop matrix runs its 256-byte-inode cases
with a known wall clock. They check the written, created and parent
directory times and leave every other field unchanged. A mutation that drops
the write stamp fails the matrix. Tested on QEMU: libc-check checks
mtime/ctime against `CLOCK_REALTIME`, `stat` against `fstat`, directory
mtime, readlink/realpath, access by scope, ftruncate/truncate zero-fill, and
select. None of this is proved: the time logic is regression-tested only.

### Item 2: what landed

- **Launch block version 2** (docs/process-arguments.md): the header's
  reserved field is now a directory count, and the working directory, a
  qualified CuBit name, follows the environment strings. It counts toward
  the 4096-string limit. A new rejection reason, `Too_Many_Directories`,
  covers a count above one. The validator and builder are still proved to
  accept exactly the well-formed blocks (gnatprove level 1, no unproved
  checks). Version 1 is removed, not kept beside it.
- **Names are resolved by proved Ada.** `CuBit.Path_Names` (SPARK, gnatprove
  level 2, nothing unproved) is called from libc through
  `__cubit_name_resolve` and `__cubit_name_display`
  (`CuBit.Path_Names_C`, `Export, Convention => C`). Names that don't start
  with `/` or `@` start from the working directory, and `.` is dropped.
  `..` removes the component before it but never the volume (`/..` is `/`),
  which is exact because there are no symbolic links. Proved: the result
  starts with `@`, keeps the selected volume, fits the filesystem service's
  256-byte limit (a compile-time check ties the two), and has no `..`
  component, whatever the input. The service still rejects `..` as a second
  check. The C copy of this logic is deleted; libc keeps only the
  directory's storage and lock.
- **Strict working directory** (user decision: assume any software may be
  compromised).
  - chdir succeeds only into a directory the process may read: ENOENT,
    ENOTDIR, or EACCES when its scopes hide it.
  - A process starts with no directory chosen. Relative names then resolve
    against the system volume's root, exactly as absolute ones do, and its
    children get no directory.
  - A directory chosen by chdir, or by a launcher that procmgr checked, is
    passed to children by `posix_spawn`. procmgr refuses the launch
    (`Not_Granted`, EACCES, with a `CuBit.Failures` explanation) unless the
    child's own filesystem scopes let it read that directory, so a launcher
    cannot place a child in a directory the child can't see.
  - getcwd gives POSIX paths on the system volume (`/src`) and CuBit names
    elsewhere (`@usb:0/x`).
- **The `*at()` calls** (`openat`, `mkdirat`, `unlinkat`,
  `renameat`/`renameat2`, `fstatat`, `faccessat`/`faccessat2`,
  `readlinkat`) start from an open directory: its name is the resolver's
  base. Directory descriptors keep their name for this.

Tested on QEMU:
- libc-check: chdir/getcwd, relative and `..` names, refusals (including
  EACCES for the hidden root), every `*at()` call, fchdir, `..` stopping
  at the root.
- processes: a child starts at `/` while no directory is chosen, and in
  `/tls` after the parent's chdir. A child that can't read `/tls` is
  refused with EACCES, and chdir into the hidden root is refused. A
  mutation that makes the child ignore the directory fails this.
- Hosted: the launch-argument suite (68 checks plus 400,000 random
  blocks) and tests/path-names (34 checks plus 300,000 random paths), each
  compared against an independent reference.

Not done yet: Ada and the Moto-based Rust runtime ignore the directory, and
`posix_spawn_file_actions_addchdir_np` is unsupported (item 4).

6. **A CCL build tool instead of make** (user decision, 2026-10-04: "I'd
   prefer to create a CCL replacement for Make"). GNU make is not ported.
   - Targets, sources, tools and outputs are typed CCL values, checked by the
     type system like manifests and startup plans, not a Makefile dialect.
   - Rules run tools through item 5's launch built-in, each with only the
     places and manifest authority it declares. Outputs land in a declared
     build place.
   - Staleness comes from file metadata (item 1's mtimes) and content hashes.
     Independent targets run as parallel children (item 4).
   - The build graph can be inspected from the console, like any other CCL
     value.
   - First user: building CuBit's own pieces on CuBit, replacing
     `kernel/Makefile` rule by rule.

### Item 3: what landed

2026-10-04, as designed under item 3 above.

- **Growth.** `findRoom` scans every block (single and double indirect),
  and `growDirectory` adds an empty block through `appendRun` as journaled
  `Directory_Data`. Create, unlink, the same-directory in-place rename and
  rmdir's parent work on every block. rmdir still releases only directories
  whose blocks are all direct.
- **htree.** `unindexDirectory` clears `EXT2_INDEX_FL` before any change.
  Other inode flags (extents, for one) are still refused, unchanged.
- **Rename** (`renamePath`, `renameMove`): across directories, replacing a
  file or an empty directory, moving directories (".." retargeted, both
  parents' link counts adjusted), refusing a directory into its own
  subtree. A replaced file still held by handles becomes an orphan, freed
  at its last close, as with unlink. New block operations, proved at level
  2 with the rest of `Directory_Blocks`: `Empty_Block`, `Retarget` and
  `Retarget_Parent`.
- **Protocol.** New replies `IS_DIRECTORY` (EISDIR), `INVALID_MOVE` (EINVAL)
  and `CROSS_VOLUME` (EXDEV, which `mv`-style tools answer by copying). A
  directory onto a non-directory is `WRONG_OBJECT_TYPE` (ENOTDIR); onto a
  directory with entries, `NOT_EMPTY`.
- **Tests.**
  - tests/filesystem-interop/run-directories.py: Linux-made ext2 and ext3
    images, 1 KiB and 4 KiB blocks. The production driver grows a directory
    to double-indirect blocks, changes an htree directory and runs every
    rename case. `e2fsck -fn` must come back clean, and debugfs checks the
    names, contents and "..". Mutating the htree clearing, the growth write,
    a parent link count or ".." makes it fail.
  - The 27-image round trip, journal replay, namespace power cuts, crash
    and pressure checks still pass. The truncate suite's creation faults
    now cover growth at 17 boundaries in three modes.
  - Guest: bench-fs, libc, processes and storage-grants pass.
    storage-grants' rename check now moves a file across directories, and
    its fixtures expect htree directories to be writable and extents
    refused.

### Item 4: what landed so far

2026-10-04.

- **Delegated places.** A launcher hands the child it starts a list of
  places, each with read, write and create rights (`CuBit.Launch_Grants`,
  proved at level 2), carried beside the launch block in the request.
  procmgr checks each one against what the launcher holds, refuses the
  whole launch otherwise, installs the rest with the child's own manifest
  scopes, and records them as held, so the child may delegate them in turn.
  Ada launchers use `CuBit.Launching`; the CCL launch built-in (item 5) will
  too, with typed file arguments producing the list.
- **Longer names.**
  - Scopes and delegated places hold up to 256 bytes: `File_Access`'s wire
    entries carry a two-byte length, and procmgr's held scopes went from 64
    to 256 bytes. Manifest `.cubit.access` entries keep 64 for now.
  - Path names may be up to 4096 bytes (Linux's PATH_MAX;
    `Directory_Paths.Maximum_Bytes`, which `Path_Names` follows), and
    components 255. Every name is still resolved to a full name. Resolving
    `*at()` against directory handles is not planned: CuBit prefers short
    paths (user decision, 2026-10-04). The libc parks handles only for names
    up to 256 bytes.
- **Tests.** tests/launch-grants; tests/path-names (34 checks, 300,000 random
  cases). The processes guest test gains `delegate-check`:
  - a child cannot read a place outside its manifest;
  - it can once that place is delegated;
  - a place longer than 64 bytes delegates;
  - delegating rights or places the launcher lacks is refused.
- **Remaining:** stdin declared as a text stream, and a launcher reading its
  children's streams (with item 5).

## Then the tools

| Order | Tool | Needs | Notes |
| --- | --- | --- | --- |
| 1 | **binutils** (as, ld, ar, objcopy, nm) | 1-2 | Single-process file I/O; the lightest. Cross-built static with `cubit-cc` and a manifest. No CuBit target triple (user decision, 2026-10-04): they stay x86-64 Linux (musl) tools producing ordinary static ELF with the flags CuBit needs (non-PIE ET_EXEC, `-z stack-size`, `cubit.ld`), and the manifest sections are linked in a separate step, as `cubit-cc` does. |
| 2 | **jj** (git backend through gix) | 1-3, 5 | Rust std already works. Local operations need no subprocess. Fetch/push go in-process (gix plus rustls over libc TCP, like Servo), because children cannot receive network authority. No pager or editor (no terminal); the CCL console shows output. |
| 3 | **gcc** (≥ 14, for libiberty's `posix_spawn`), configured as an x86-64 Linux musl compiler, not a CuBit triple | binutils, 1-5 | The driver launches cc1/as/collect2/ld by exact name through `may-launch`. Temp files live in a writable place. Always without `-pipe`: temp files, not descriptor plumbing. |
| 4 | **GNAT** (gprbuild optional) | gcc, 4, 6 | A full GNAT runtime built for the musl ABI (CuBit's own runtime is ZFP). The CCL build tool drives gcc/gnatbind/gnatlink directly from `.ali` dependencies. gprbuild, which captures output through inherited descriptors, is not needed for it. |
| later | git | 1-4 | `run-command.c` is fork/exec plus pipes; it would need patching. jj covers the version-control need first. |

## Tools: progress

- **binutils 2.46** (2026-10-04, `userspace/ports/binutils`): `as`, `ld`,
  `ar`, `nm`, `objcopy`, `objdump`, `readelf`, `strip`, `size`, `strings`,
  `ranlib`, `addr2line`. Cross-built static with `cubit-cc` as an x86-64
  Linux (musl) toolchain; the CCL manifest is added afterwards with
  `objcopy --add-section`. Guest test `binutils`: `as` and `ld` on CuBit
  assemble and link a small program, byte-identical to the same binutils on
  Linux. `as` and `ld` now have typed parameters (as.ccl, ld.ccl) and no
  file scopes: the Ada test launcher asks procmgr for each tool's
  descriptor, renders typed values into argv and delegated places, and each
  tool touches exactly the files its call names. Raw argv without places
  fails, and a file the launcher does not hold is refused (guest test, all
  PASS, 2026-10-04). The other tools still share one work place
  (`@nvme:0/work`) until their manifests are written. Its search of default
  library paths is refused, harmlessly.

- **GCC 15.3.0** (2026-10-05, `userspace/ports/gcc`): `cc1` and the `gcc`
  driver, cross-built static with the CuBit libc. Guest test `gcc`
  (`tests/gcc`), all PASS on CuBit, each compared with the same GCC and
  binutils on Linux:
  - stage 2: `cc1` compiles `hello.c` and `as.app` assembles it, both
    byte-identical;
  - stage 3: `gcc -O2 -fno-pie -c hello.c`: the driver starts `cc1` and `as`
    itself through the libc's `posix_spawn`, and the object file is
    byte-identical.
  - **stage 3, linked and run (2026-10-05):** `gcc -O2 -fno-pie hello.c
    hello-manifest.s -o hello` on CuBit. The driver runs cc1, as twice (the
    program and its CCL manifest) and ld. The program is byte-identical to
    the same link on Linux, and it runs on CuBit, exiting with 42. All 10
    checks PASS, in 117 s under TCG.

  How it runs:
  - **Install layout (D6):** everything under `@nvme:0/toolchain`
    (`--prefix=/toolchain`): `bin/gcc`, `libexec/gcc/x86_64-linux-musl/15.3.0/cc1`,
    `x86_64-linux-musl/bin/{as,ld}`, and the CuBit libc's headers in
    `include`. The default include directories are configured inside it
    (`--with-local-prefix`, `--with-native-system-header-dir`). cc1 treats a
    refused `/usr/include` as an error, unlike a missing one.
  - **Names:** the driver finds its programs from `argv[0]`
    (`/toolchain/bin/gcc`). The libc's `posix_spawn` resolves a name with a
    `/` like a path, so `/toolchain/bin/../lib/gcc/.../../../../x86_64-linux-musl/bin/as`
    matches the driver's `may_launch` entry `toolchain/x86_64-linux-musl/bin/as`
    exactly.
  - **Authority (D2, implemented):** cc1 and the driver have no file scopes.
    The launcher delegates `work` (read, write, create) and `toolchain`
    (read). procmgr records which held places were delegated and answers
    `OP_DELEGATED_PLACES` (`16#010B#`, `CuBit.Launch_Grants`), and
    `posix_spawn` passes those on whole to cc1 and as. Per-file delegation
    is CCL-004.
  - Temporary files go in the work place (`TMPDIR=/work`).
  - **Linking:** a specs file (`userspace/ports/gcc/cubit.specs`, installed
    as `lib/gcc/x86_64-linux-musl/15.3.0/specs`) reproduces `cubit-cc`:
    `-mno-red-zone`, `cubit-crt1.o`, `-static -T cubit.ld`, a 1 MiB stack,
    libc and libgcc in a group. ld runs directly, with no collect2 and no
    LTO (D4). libgcc, crtbeginT and crtend come from the musl cross
    compiler of the same version.
  - **Diagnostics (D3, implemented):** the libc's `posix_spawn` lends each
    child a ring for its `unix.stderr` (`CuBit.Libc_Child_Outlets`) and
    copies it to the caller's descriptor 2 while it waits. ld's errors reach
    the test through the driver.
  - **Found and fixed on the way:** the libc page cache zero-filled a page
    the filesystem answered short (allowed) and served the zeros as file
    bytes; on the write path it would have written them back. It now caches
    a partial page only at the end of the file. `archive-walk` (in the
    test) checks that `libc.a` reads correctly through `pread`, `lseek` +
    `read` and `mmap`.
  - **Still open:** the driver's harmless probes of `/lib` and `/usr/lib`;
    D8 (`@` response files); why the filesystem service answers short there;
    and an intermittent hang, seen twice, where the driver did not continue
    after a child exited. The move to `Applications/gcc/15.3.0/` (FS-020)
    comes next.

## jj: feasibility (audit, 2026-10-05)

Verified on the Linux host (build and link); nothing has run on CuBit yet.

- **jj 0.44.0** (pinned nixpkgs) builds and links for `x86_64-unknown-cubit`
  the way Servo does, with no unresolved symbols, using
  `--no-default-features --features git`.
  - Beyond Servo's crate fixes, it needs the CuBit target in libc 0.2.189,
    socket2, getrandom, nix 0.29/0.31, termios, mac_address and errno.
  - It has no C crypto or compression: zlib-rs and RustCrypto.
- **The plan's premise was wrong: jj has no in-process fetch or push.**
  Since 0.30 every network operation runs `git` (`GitSubprocessContext`:
  fetch, remote show, branch prune, push). gix 0.85 can fetch, but has no
  push.
- **Local operations are blocked by four libc and filesystem gaps:**
  - **`flock`:** ENOSYS. jj locks its op heads and working copy, and treats
    the error as fatal.
  - **`chmod`/`fchmod`:** ENOSYS. jj sets exec bits on every checkout.
  - **Symlinks:** none. jj's symlink probe must return false for CuBit, so
    links are checked out as files, like git's `core.symlinks=false`.
  - **`mmap` of files:** a private copy of at most 256 MiB. CuBit's own
    packs are 430 and 302 MB.
- **Smaller points:**
  - mtime has one-second resolution, so more files get rehashed.
  - jj must not snapshot before the wall clock is set.
  - There is no `HOME`, so the user's name and email go in the repository
    config.
  - The pager, editor and diff tools are unusable without a terminal: use
    `-m` and filesets.
- **Network:** DNS and TCP already work through the netstack for Rust std.
  For TLS, either rustls with aws-lc in-process (it compiles), or gix's HTTP
  transport over tls.svc (SPARKTLS), which is recommended.
- **Plan:**
  0. A `userspace/ports/jj` build.
  1. Local jj on CuBit: handle-owned `flock`, `fchmod`, and a way past the
     256 MiB pack limit.
  2. A typed manifest and launch from CCL.
  3. In-process fetch and clone through gix, over tls.svc.
  4. In-process push: a send-pack. This is the hard part; consider
     upstreaming it.
- **Decisions needed:**
  - fork or upstream;
  - the TLS route;
  - real `flock` or lock files;
  - large packs;
  - exec bits;
  - a raw argument list for the interim manifest;
  - whether to start fetch-only, pushing from Linux meanwhile.

## GCC and GNAT: feasibility (audit, 2026-10-05)

Verified on the Linux host only; nothing below has run on CuBit yet.

- **Version: GCC 15.3.0**, the one the pinned nixpkgs ships for gcc, gcc15
  and gnat15. It is also the musl cross compiler `cubit-c++` links with, so
  its `libgcc*.a`, crt files and `libstdc++` are reused. libiberty selects
  `posix_spawn` (`HAVE_POSIX_SPAWN`).
- **Already builds against the CuBit libc.** Configured as a cross-native
  compiler (build x86_64-pc-linux-gnu, host = target x86_64-linux-musl),
  with `CC=cubit-cc` and `CXX=cubit-c++`, `cc1`, `cc1plus`, `xgcc`,
  `collect2` and `lto-wrapper` link static with no missing libc symbols.
  - GMP, MPFR and MPC build in-tree unchanged.
  - GCC itself builds with `-fno-exceptions -fno-rtti`. `libstdc++` is
    already linked by `cubit-c++`, and C++ exceptions work on CuBit (the
    `cxx-check` guest test).
  - Sizes, static and stripped: `cc1` 40 MB, `cc1plus` 42 MB, `gnat1`
    44 MB, `xgcc` 2.4 MB.
- **How the driver runs its tools:**
  - Without `-pipe` it runs one command at a time and passes no file
    actions, which the libc accepts.
  - It passes `COLLECT_GCC`, `COMPILER_PATH` and the rest in the
    environment, which travels in the launch block.
  - Temporary files go under `TMPDIR`.
  - With `--disable-lto` and no `collect2`, it runs only `cc1`, `as` and
    `ld`.
  - A fixed `--prefix=/toolchain` with `--with-as`/`--with-ld` makes it
    find its pieces whatever `argv[0]` is.
- **Arguments and authority.** procmgr does not check argv, by design: it
  checks the places delegated and the child's manifest scopes
  (docs/ccl-launch-parameters.md). That settles the open point in item 4:
  `cc1`'s argv is open-ended (temporary names, many `-I`), so a typed
  mapping cannot cover it. Today, the libc's `posix_spawn` delegates no
  places and lends no rings.
- **GNAT:**
  - `gnat1` needs no tasking runtime, and `gnatbind` is pure file I/O.
  - `gnatlink` and `gnatmake` spawn through `fork`/`exec`, which CuBit
    lacks.
  - Building GNAT needs a GNAT with a musl `libgnat`: the dev shell's is
    glibc. `nixpkgs#pkgsCross.musl64.buildPackages.gnat15` provides one
    (one GCC build, everything else cached).
  - The target `libgnat` (what user programs link) is the x86_64-linux one,
    built against the CuBit libc. Small musl fixes are expected.
- **Memory:**
  - The 1 TiB owned aperture and the 256 MiB per mapping are enough.
  - The compilers need a large stack: they link with `CUBIT_STACK_SIZE`
    64 MiB.
  - **Gap:** `munmap` accepts only a whole original allocation, while
    GCC's garbage collector unmaps parts of 2 MiB blocks. The failure is
    ignored, so the memory leaks; runs don't fail. Precompiled headers need
    fixed-address `mmap`, so they stay off.
- **Files at run time:**
  - Everything goes under one read-only place, e.g. `@nvme:0/toolchain`:
    `bin`, `libexec/gcc/x86_64-linux-musl/15.3.0/cc1`, `lib/gcc/…`, and the
    CuBit sysroot.
  - A specs file reproduces what `cubit-cc` passes: `cubit-crt1.o`,
    `-static -no-pie -T cubit.ld`, `-z stack-size`.

### Plan

1. `userspace/ports/gcc/build.sh` cross-builds `cc1` and the driver
   (started 2026-10-05). It uses an interim manifest: work and toolchain
   scopes, no typed parameters.
2. Run `cc1 hello.c -o hello.s` on CuBit from a test launcher. Assemble and
   link it with `as` and `ld`, and compare with the same `cc1` on Linux.
3. The driver end to end (`gcc hello.c`). This needs its `may_launch`
   table, its children's access (D2), stderr (D3) and a `TMPDIR` place.
4. A musl `libgnat`, then `gnat1 -c hello.adb`.
5. `gnatbind`, then link through `gcc`. Skip `gnatlink`, or patch its spawn
   (D5).
6. Hardening: partial `munmap` (D7), response files for long links, and
   building CuBit units on CuBit byte for byte against Linux.

### Decisions needed

- **D1, bootstrap GNAT (decided, user 2026-10-05):** nix
  `pkgsCross.musl64.buildPackages.gnat15`.
  Built 2026-10-05: it supplies the musl cross Ada compiler (`gnat1`,
  `gnatbind`, `gnatmake` for x86_64-linux-musl), but no musl Ada runtime
  (nixpkgs builds no target libada). Our GCC build, with
  `--enable-languages=c,ada` and this compiler for Ada, builds the musl
  `libgnat` as a target library (stage 4).
- **D2, authority for driver-launched tools (decided, user 2026-10-05;
  implemented 2026-10-05):** the libc's `posix_spawn` re-delegates the
  places the driver was given (`OP_DELEGATED_PLACES`), never its own
  manifest scopes. Never widening. No fixed-scope interim was needed.
- **D3, children's stderr from a C launcher (decided, user 2026-10-05;
  implemented 2026-10-05):** compiler diagnostics reach the console. The
  libc lends each child its own stderr ring and copies it to the launcher's
  descriptor 2 while waiting (`CuBit.Libc_Child_Outlets`). This replaces
  sharing the launcher's ring: no grant forwarding, and never two writers on
  one ring.
- **D4:** drop `collect2` and LTO (done 2026-10-05: the specs run ld
  directly; GCC is configured `--disable-lto`).
- **D5:** `gnatlink` patched to use `posix_spawn`, or the CCL build tool
  binds and links directly.
- **D6:** install place and layout (FS-020), e.g. `@nvme:0/toolchain` with
  `--prefix=/toolchain`.
- **D7:** partial `munmap` in the kernel or libc (recommended), or a GCC
  configure workaround.
- **D8, CuBit names in a GNU tool's argv (found 2026-10-05):** GCC and
  binutils read any argument starting with `@` as a response file
  (libiberty's `expandargv`). A CuBit name such as `@nvme:0/work/hello.s` is
  therefore first opened as the response file `nvme:0/work/hello.s`,
  relative to the working directory. When that fails, the argument is kept
  as written, so the binutils and gcc tests pass despite the refusals. When
  a readable file has that name, its contents become arguments. This applies
  to as.app's typed parameters too, which render CuBit names. Options:
  - Render names on the system volume as POSIX paths (`/work/hello.s`), and
    others as CuBit names. Generic, but other volumes still collide.
  - A per-manifest argv form for file parameters (`posix` or `cubit`),
    declared in the program's description: generic, no per-tool code.
    Recommended, together with CCL-004's argv grammar.
  - Until then, launchers pass relative names from a working directory
    (the gcc test does).

## Ownership

- **Mine (CCL, networking, logging, filesystem):** 1 (filesystem side), 2,
  3, place delegation, 5 and 6.
- **libc process code** (`process.c`, `posix_spawn`): claimed by the processes
  agent, coordination/processes.md. Standard-stream declarations and stream reading for children come to me with item 4.
- Kernel changes, such as a cwd in the launch block, are coordinated in
  coordination/.

## Verification

Each step is proved where it is a protocol or invariant: the filesystem
service, the launch block, place delegation. Each is tested hosted and on
QEMU. The tools themselves are checked by building CuBit pieces with them on
CuBit, and comparing against the Linux-built output.
