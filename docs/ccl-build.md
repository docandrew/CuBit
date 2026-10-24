# CCL build and the Storehouse

Status: design, 2026-10-19, with the user's decisions of the same day.
Implemented hosted on Linux: the Ada program rule family, GNAT 15.3, a
`.gpr` converter, and programs migrated from make byte for byte (see
"First slice" and "Second slice"). Backlog BLD-001, with SELF-001 item 3
and FS-020's Storehouse.

## Decisions (user, 2026-10-19)

- **CCL build is THE build system.** `.gpr` files are translated once to
  CCL by a converter for the GPR subset CuBit uses (`tools/gpr_to_ccl.py`),
  then deleted; CCL is the source of truth. gprbuild is never ported to
  CuBit. An opaque gprbuild step is allowed only on the Linux host, for an
  awkward third-party crate. ccl-build may emit a `.gpr` for the Ada
  Language Server as an output, never read one as an input.
- **GCC 15.3 everywhere.** Native CuBit Ada moves off Alire's
  `gnat_native` 16.1 to nixpkgs' GCC/GNAT 15.3 (`pkgsCross.musl64`
  gnat15), the compiler that runs on CuBit (branch `gnat`), so host and
  CuBit builds can be compared byte for byte.
- **Rule families stay in Ada** in the tool until the CCL capacity work
  (`docs/ccl-capacity.md` in the `cubit-ccl-limits` worktree) reaches its
  milestone M1; nothing works around the caps in CCL.
- **D5:** no gnatlink or gnatmake.

## Why

`kernel/Makefile` (2,300 lines), `userspace/Makefile`, gprbuild projects
and about thirty `tools/*.py` scripts build CuBit today. Staleness is by
mtime, outputs land in the source tree, and nothing records which tool,
flags and inputs made a file. CuBit is going to build itself (SELF-001),
and GNU make is not being ported (self-hosting item 6).

The user, 2026-10-19: "Nix is very complicated and I'm hoping we can
simplify quite a bit with CCL Build." Earlier decisions (BLD-001): a full
content-addressed system like Nix; every step named by the hash of all its
inputs (SHA-256 through SPARKTLSCrypto); an immutable Storehouse; nothing
re-runs if its identity is present; no mtime staleness; several outputs per
entry (`out`, `debug`, `assets`); steps are typed tool calls whose sandbox is
the delegated places. And (D5, 2026-10-19) no gnatlink or gnatmake: CCL
build takes gprbuild's role.

## What we keep from Nix, and what we drop

| Nix | CCL build | Why the Nix part is unnecessary here |
|---|---|---|
| Input-addressed derivations | **Kept**: a step's identity is the hash of everything it runs with | |
| Immutable store | **Kept**: the Storehouse, read-only entries named by hash | |
| Binary substitution | **Kept, later**: signed entries fetched by identity through `tls.svc` | |
| Generations, GC roots | **Kept**: a generation is a root set; GC deletes what no root reaches | |
| Nix language (lazy, untyped, dynamically scoped) | Typed CCL values, checked by the type checker | Errors are type errors at the declaration, not deep in an evaluation trace |
| bash builders, stdenv phases, setup hooks | A step is one typed tool call: program, arguments, inputs, outputs | No shell exists on CuBit; a phase is just another step |
| chroot, user namespaces, `nixbld` users | Capability-delegated places | On CuBit a process can name only what it was delegated |
| Store-path hash scanning for runtime references | Nothing | Static bundles, no shared libraries: the runtime closure is the bundle |
| RPATH rewriting, `patchelf`, wrapper scripts | Nothing | Same reason; programs find their files relative to their own bundle |
| `.drv` files | The canonical encoding of the step value | The step value is the derivation; no second file format |
| Input-addressed *outputs* (`/nix/store/<input hash>-name`) | Content-addressed outputs | Nix needs input addressing because outputs embed their own store path. CuBit outputs do not, so the output can be named by its content, which gives early cutoff for free |
| Fixed-output derivations for fetches | Imports with a pinned content hash | Same idea, one kind |
| Channels, flakes, overlays | Ordinary CCL values and functions; pins are values in Git | Composition is function application in a typed language |

What remains is small: **hash inputs, look up, run if missing, store
outputs by content, record the mapping.**

## Two identities

- **Output identity** = SHA-256 of the content. A file's identity is the
  plain SHA-256 of its bytes (so `sha256sum` agrees). A tree's identity is
  the SHA-256 of its canonical listing (below), a Merkle hash.
- **Step identity** = SHA-256 of the step's canonical encoding, in which
  every input appears as its output identity, never as the step that made
  it.

The **action cache** maps step identity → the output identities it
produced. A build is: compute a step's identity; if the action cache has
it and the outputs are present, the step is done; otherwise run it, store
each output under its content hash, record the mapping.

**Early cutoff follows.** If an edit to `a.adb` rebuilds `a.o` to the same
bytes (a comment change), every step that reads `a.o` has the same
identity as before and is a cache hit. No separate "content-addressed
derivations" mode is needed. It relies on reproducible tools, which CuBit
already requires: the GCC, binutils and GNAT ports produce byte-identical
output on CuBit and on Linux.

**Non-reproducible steps** are a bug to fix, not a mode. When a step runs
again (its outputs were collected) and gives different outputs than its
action record, the tool reports it and fails the build. A `--check` that
reruns a step on purpose, like `nix build --check`, is planned.

## Steps

A step is a typed tool call:
- **program**: a file in an imported toolchain, or an output of another
  step;
- **arguments**: typed pieces, each one argv entry `prefix & what &
  suffix`: literal text; an input or output, as its path in the step
  directory or relative to the working directory; a directory in the step
  (a runtime); the step directory itself (for `-ffile-prefix-map`);
- **inputs**: each placed at a relative path in the step's directory;
- **outputs**: named, each at a relative path, read back after the run;
- **captured outlets**: `unix.stdout` captured as an output, for tools that
  write their result there (`ccl-manifest`);
- **working directory** (relative). The **environment is empty**; a
  declared environment (values as argument pieces) comes when a tool needs
  one.

Literal arguments cannot contain a `/` at all: a file enters a step only
as a typed input or output, so it is hashed. The rendered argv is not
hashed; the step value is. Step directories and store locations never reach
an identity.

**Step lists** are produced by **rule families** (an Ada program, a C
object, a manifest, an image). Three typed documents feed them (schema:
`userspace/ccl/interfaces/build.ccl`):
- `build-params.ccl`, one `Parameters` value: toolchain pins, the
  runtime, the manifest tool and the two launchers' catalogs, the target
  switches and what every native link shares;
- `build-catalog.ccl`, one `Build_Catalog` value: named code profiles
  (`proved`, `pixel-loop`) and libraries (`ui-toolkit`);
- each component's `component.ccl`, one `Component` value naming only what
  differs. Its directory is where the file is.

```lisp
# userspace/apps/sleep/component.ccl
(Component name => "sleep" kind => Kind.Application
  source => (Source.Ada (Ada_Source)))

# userspace/apps/dictionary/component.ccl
(Component name => "dictionary" kind => Kind.Application
  source => (Source.Ada (Ada_Source
    main => (Main.File "dictionary_main.adb")
    sources => ["userspace/lib/lexicon" "userspace/lib/lexicon/channel"]
    libraries => ["ui-toolkit"]
    language => Ada_Language.Ada_2022
    debug_info => false
    units => [(Unit_Code ["lexicon_wire.adb"] "proved")])))
```

**Derived, not repeated:** the executable is the name with the kind's
suffix (`.app`, `.svc`, `.drv`; a `Tool` has none); the manifest's catalog
follows from who launches the component (applications and tools: the
process manager; drivers: the device manager; a service says which,
`(Kind.Service Launcher.Device_Manager)`, and one the kernel starts has no
manifest); switches follow from the code's intent (`language`,
`optimization`, `checks`, `frame_pointer`, `value_range_propagation`,
`debug_info`), in one fixed order; the binder unit's switches are the
program's without GNAT's own, as gprbuild does. A component cannot state a
catalog or an executable name, so it cannot contradict its kind.

**Decision:** rule families are native (Ada) code in the build tool for
now, like Bazel's native rules, and become CCL functions returning step
values later. CCL's current limits (8 KiB of source including the schema,
256 value cells, 16 functions) cannot hold the expanded steps of even one
program, while a declaration fits easily. The executor and identities are
defined on steps, so moving a rule into CCL changes no identity semantics.
This follows the no-special-cases rule: declarations are ordinary typed
values (`interfaces/build.ccl`) checked by the type checker, read by field
name; there is no keyword reader.

## Granularity and dynamic dependencies

**Per compilation unit**, like Bazel and Shake: one step per `gcc -c`, one
for `gnatbind`, one for the binder unit, one for the link. Editing one body
recompiles one unit and relinks.

Ada steps follow D5 (user, 2026-10-19), as `tests/gnat` does on CuBit:
`gcc -c` (gnat1) per unit, `gnatbind -x` on the main `.ali`, `gcc -c -gnatA
-gnatWb -gnatiw -gnatws b__main.adb`, and a `gcc` link with the runtime
library. No gnatlink, gnatmake or gprbuild.

**Which units, and which files each reads, are discovered:**

1. **Units: a step that yields further steps** (Shake's monadic `need`,
   ninja's dyndep). The Ada rule compiles the main unit, reads the `W`
   lines of its `.ali` (an output, so a store entry), and adds a compile
   step for each withed unit found in the program's source directories,
   recursively. The binder step's inputs are then all those `.ali` files.
   The plan is extended only from outputs, which are content-addressed, so
   the expansion is as reproducible as the steps.
2. **Files a compile reads: recorded traces** (Shake's verifying traces,
   ccache's direct mode). A compile runs with a declared over-approximation
   (the source directories and the runtime). Afterwards the `.ali` `D`
   lines (GNAT) or the `-MD` depfile (C) list the files actually read. The
   trace records, under a *pre-key* (tool, arguments, and the **names**
   present in the candidate directories), the used set. On the next build
   the step's precise identity is the pre-key plus the contents of only the
   used files. Hashing names as well as contents keeps it sound when a new
   file would shadow an old one on the search path. A miss still runs with
   the full candidate set and re-records the trace, so a trace can make a
   build faster but never wrong, provided the compiler reports every file
   it read (assumed on Linux; on CuBit, delegate only the used set and a
   missed file fails loudly).

The first slice uses step 1 and declares whole directories as inputs;
step 2 is the next optimisation once the kernel's 300-unit builds are in.

## Canonical encoding

One encoding for every value that is hashed: **deterministic CBOR**
(RFC 8949 §4.2.1 core deterministic encoding), a subset of no floats, no
maps, no tags, no indefinite lengths: unsigned integers, byte strings, text
strings and definite arrays, each head in its shortest form (negative
integers join when a hashed value needs them). The launch-parameter design (docs/ccl-launch-parameters.md) already
chose canonical CBOR for program arguments, so a build step and a launch
are encoded the same way.

- Steps, trees and action records are encoded field by field (below).
  CCL values, when they are hashed (launch arguments, steps written by
  CCL rules), map the same way: a record is an array of its fields in
  declaration order, a variant `[alternative, payload]`, a list an array,
  an enum its position.
- Each hashed kind starts with a version tag: `["cubit.step", 1, ...]`,
  `["cubit.tree", 1, ...]`, `["cubit.action", 1, ...]`, so a step can never
  collide with a tree.
- A **tree** is `["cubit.tree", 1, [[name, kind, identity-or-target],
  ...]]`, entries sorted by name bytes; kinds are file, executable file,
  symbolic link (target text, relative only) and directory (its tree
  identity). Owners, times and other mode bits are not part of a tree.
- **Only the step is hashed.** The declaration, its schema and the rule
  family are not part of a step's identity: the step value alone says what
  runs, so two declarations (or two rule versions) that produce the same
  step share its outputs. A rule change that changes what runs changes the
  step, and so its identity.

The encoder and decoder of heads are proved; see "First slice".

## The Storehouse

```
<store>/files/<sha256>        one immutable file, mode 0444
<store>/executables/<sha256>  one immutable executable file, mode 0555
<store>/trees/<sha256>/       one immutable tree, read-only
<store>/actions/<sha256>      step identity -> outputs (canonical CBOR record)
<store>/roots/<name>          GC roots: a list of identities
<store>/work/                 staging and step directories, owned by a live
                              process or garbage
```

- **Linux host:** `${XDG_CACHE_HOME:-$HOME/.cache}/cubit/storehouse`,
  outside every checkout, so worktrees (`cubit`, `cubit-build`,
  `cubit-gnat`) share outputs. The tool takes it as an explicit `--store`
  argument; nothing defaults silently.
- **CuBit:** `@system/Storehouse/` (FS-020), written only by the software
  manager. A step's outputs become entries; `Applications/<name>/<version>`
  views name entries. Entry metadata (FS-020's extended attribute) holds the
  identity, the step identity that made it and, for programs, the GNU
  build-id (BLD-001).
- **Multiple outputs per entry:** a step declares named outputs (`out`,
  `debug`, `assets`, or `main.o`/`main.ali`); each is stored separately by
  content, and the action record names them. Installing `out` never pulls
  `debug`.
- **Atomic, crash-safe writes:** write into `work/` with a fresh name,
  `fsync`, make read-only, set mtime to the epoch, `rename` into place,
  `fsync` the directory. `rename` either places a complete entry or does
  not; if the name exists (a concurrent build made the same content) the
  copy is discarded. The action record is written last, after all its
  outputs, by the same rename. A crash leaves only garbage in `work/`,
  which the next run deletes; never a partial entry or a record naming a
  missing output.
- **Integrity:** an entry's name is its hash, so `verify` re-hashes and
  reports any entry whose content no longer matches (tampering, disk
  corruption). On Linux, sandboxes hard-link store files read-only; a tool
  that `chmod`s an input and writes through it would corrupt the store,
  which `verify` detects but does not prevent. On CuBit the entries are
  read-only places, so it cannot happen.
- **GC:** roots are generations (FS-020), the current checkout's last
  build (`roots/<checkout>`), and explicit pins. Mark from roots through
  action records; delete the rest. Action records are cheap and can be
  kept longer than their outputs.

## Running a step

**Linux (hosted, first):** the executor claims the step's directory,
`<store>/work/step-<shape>`, hard-links each input from the store at its
declared path, sets the working directory, runs the program with an empty
environment and `argv` rendered from the pieces, reads back exactly the
declared outputs (missing → failure), stores them, records the action, and
deletes the directory.

The directory is named by the step's **shape** (its encoding without the
inputs' contents), not by a fresh name: GNAT records its switches in the
`.ali`, including `-ffile-prefix-map=<step directory>=.`, so a fresh name
per run would make every `.ali` differ and stop early cutoff at the binder.
With the shape, editing a body leaves the path the same. A claim file
placed by `link` (never partial) names the owning process; a live owner
makes a concurrent identical step fail rather than share the directory.
On CuBit the places a step sees have fixed names, so such recorded paths
are identical on every machine; on Linux they are identical within one
store (the store's path appears in `.ali` files, not in programs).

What this enforces on Linux, honestly:
- *Enforced:* inputs are snapshots of hashed bytes (the tool reads the
  store copy, not the live source); relative inputs not declared are absent
  from the directory; literal arguments name no files (no `/`); the
  environment is empty; inputs are read-only; outputs are exactly the
  declared ones.
- *Assumed:* that the tool does not open absolute paths outside the
  directory (`/usr/include`, `$HOME`). The toolchain is called by absolute
  path inside the store and can read anything the user can. Linux
  namespaces (bubblewrap-style) could close this; not planned, since CuBit
  is where enforcement matters.

**CuBit:** the same executor launches the step through procmgr with
exactly its places: each input read-only, the output directory
create/write, the toolchain bundle read-only (D2's delegated places,
per-file delegation CCL-004). A tool that reaches for an undeclared file is
refused by the filesystem service. This is real enforcement, not hygiene.

## Toolchains and bootstrapping

- **Now:** the toolchains are imported as pinned trees. `build-params.ccl`
  names each toolchain and its expected tree identity; the first build on
  a machine is given the host directory (`--import NAME=PATH`), hashes it,
  and refuses a mismatch. Native CuBit Ada uses GNAT 15.3
  (`pkgsCross.musl64.buildPackages.gnat15.cc`, unwrapped) with nixpkgs'
  unwrapped binutils 2.46 for `ld` (`-B`); `flake.nix` exports both as
  `CUBIT_GNAT` and `CUBIT_BINUTILS`.
- **Nix store trees are recorded in place**, not copied:
  `<store>/imports/<identity>` names the store path. Nix store paths are
  immutable, and a copy would break their absolute references (the GNAT
  driver runs the cross `as` from its own store path; kernel headers are
  links to another store path). The tree identity covers the tree's bytes
  and its link texts; `verify` re-hashes it. The rest of the toolchain's
  run-time closure (glibc, GMP, MPFR, the cross `as`) is pinned by Nix's
  own input addressing, not by a CCL build identity: an assumption until
  the toolchain is built by CCL build itself. Other trees are copied into
  `trees/`.
- **make uses the same compiler** through
  `userspace/toolchain/gnat-15.cgpr`, a gprbuild configuration project
  (Makefile `NATIVE_GPRBUILD`), with `-gno-record-gcc-switches
  -ffile-prefix-map=<checkout>=.` for every native unit. The kernel and its
  ZFP runtime stay on Alire's toolchain for now; host tools (ccl-build,
  ccl-manifest, tests) also stay on Alire's/Nix's host GNAT.
- **Make-built inputs during migration** (the runtime library, `crt0.o`,
  `ccl-manifest`) are imported by content as ordinary files. Each rule
  family moved into CCL build turns one of them into a step output.
- **Dropping Nix:** once the GCC/GNAT port builds on CuBit, the toolchain
  itself is a CCL build output from source plus a pinned seed compiler (the
  imported tree), then a second build of it with itself (two-stage
  bootstrap, compared byte for byte). Nix then supplies nothing; the seed
  is one pinned Storehouse entry. Development hosts still need Linux
  binaries of the tools until CuBit self-hosts fully.

## Migration from make

Rule family by rule family, each checked byte for byte against make:

1. **Ada userspace apps** (first): one rule family covers about fifty
   `.app`, `.svc` and `.drv` targets with the same shape (typed manifest,
   gprbuild compile/bind/link, the user runtime). It exercises everything:
   imported toolchain, dynamic unit discovery, multiple outputs, a host tool
   step. First program: `sleep.app` (done, below).
2. The user runtime (`libgnat-user.a`, `art0.o`, CuAlloc objects): removes
   the largest make-built import.
3. CCL host tools (`ccl-manifest`, `ccl-config`, `ccl-image`): host Ada
   programs, so the manifest step's tool becomes a step output.
4. C programs and libc (`cubit-cc`, `-MD` traces), then Rust (cargo as one
   coarse step at first).
5. The kernel (`cubit.gpr`, the runtime under `kernel/runtime` imported as
   is, the generated `build.ads`).
6. Images and disks: `ccl-image`'s typed plan becomes the input of image
   steps.

**Coexistence:** make stays the entry point. A migrated make target calls
`ccl-build` and copies the outputs where make used to put them, so images,
headless tests and other agents are unaffected. Each migrated family gets a
comparison test (`tests/ccl-build/compare-*`) that builds with both and
compares bytes; make's recipe is deleted once the comparison has passed
and the CCL build is the one used, per AGENTS.md (no backup
implementations).

**Make's outputs must be reproducible first.** Today's userspace objects
embed the checkout's absolute path and gprbuild's temporary file names
(with process ids) in DWARF (`DW_AT_producer`, `DW_AT_comp_dir`), so two
make builds differ. Each migrated project gains `-gno-record-gcc-switches
-ffile-prefix-map=<checkout>=.`; that alone makes make's output identical
across checkouts and runs, and gives CCL build a target to match. Since the
second slice `userspace/toolchain/gnat-15.cgpr` adds them to every native
unit, so no project file needs them.

## Visibility (planned; the first slice logs each step as "ran" or "hit"
with its identity)

- `ccl-build explain STEP` prints the step value, its identity, each input
  with its identity and origin, and why it ran: no action record; outputs
  missing; or the input whose identity changed since the last recorded
  build of the same step name (the action cache keeps the last identity per
  step name in the root record).
- The console shows the build as a CCL value (the expanded step list is a
  `List Step`), and the stream graph inspector shows running steps as
  nodes with their outlets, like any other launch.
- Every run appends a record (step, identity, hit/miss, duration, outputs)
  that the capability graph (CAP graph plan) can show: what each step read
  and wrote.

## On CuBit (SELF-001)

The same Ada tool, compiled for CuBit, with the executor's Linux layer
replaced by procmgr launches with delegated places, and the store by the
Storehouse place. The core (encoding, identities, action records, rule
families) is portable SPARK, unchanged. The milestone in SELF-001 (edit,
build and install a changed component on CuBit) is a CCL build whose
output entry becomes the new generation's view; with LIVE-001 the same
entry feeds hot reload.

## Usability target (user, 2026-10-19)

The CCL migration is not finished until the build is far more usable than
Nix, not a transliteration of the Makefile. Today `sleep`'s `component.ccl`
repeats the toolchain pin, the switches and the linker lines; every app
file would repeat them again. The target below is design; the syntax is the
intended CCL, not what the checker accepts today (see "Language features
this needs").

### 1. An app is about three lines

Conventions fill in everything an app shares: its directory
(`userspace/apps/<name>`), `main.adb`, `manifest.ccl`, the object
directory, the user runtime, the native profile's switches and link
arguments, and the executable name (`<name>.app`, `.svc` for services,
`.drv` for drivers).

```lisp
# userspace/apps/sleep/component.ccl (target syntax; today it is
# (Component name => "sleep" kind => Kind.Application source => (Source.Ada (Ada_Source))))
(cubit.app "sleep")

# userspace/apps/ipctest-client/component.ccl: two programs, one extra source dir
(cubit.app "ipctest-client"
  (with sources => [@here @here/../ipctest-server/channel]
        mains => [(main "main.adb") (main "departing.adb" "ipctest-departing.app")]))

# userspace/services/nvme/component.ccl: a driver, one setting differs
(cubit.driver "nvme" (with defines => [(define "io_profile" Io_Profile.Latency)]))
```

`with` is record update: it overrides only the fields named and keeps the
rest of the convention. A **profile** is an ordinary value of type
`Profile` (switches, link arguments, runtime, toolchain); `cubit.app` uses
`params.native` unless given another:

```lisp
(define native-debug Profile (with params.native compiler => (append params.native.compiler ["-O0"])))
(cubit.app "files" (with profile => native-debug))
```

**Images and their variants are values too.** An image is a record of
placements and settings; a variant is one `with`:

```lisp
(define live-usb Image
  (cubit.image layout => Layout.Optical provider => "usb-optical"
               apps => [files text logs] settings => params.image-defaults))
(define live-usb-modeset Image (with live-usb settings => (with live-usb.settings modeset => true)))
```

### 2. One repository build-parameters file

`build-params.ccl` at the root is imported by every build: the GCC/GNAT
version and toolchain pins, the profiles, common switches and image
defaults.

```lisp
# build-params.ccl
(define gcc (Toolchain name => "gnat-15.3.0-x86_64-linux-musl"
                       identity => (sha256 "0f3c...")))
(define binutils (Toolchain name => "binutils-2.46" identity => (sha256 "9a41...")))
(define native Profile
  (Profile toolchain => gcc binutils => binutils runtime => @src/userspace/runtime
           compiler => ["-O2" "-g" "-gnatwa" "-gnatw.X" "-fstack-usage" "-fomit-frame-pointer"
                        "-mno-red-zone" "-fno-pic"]
           link => [(Link.Text "-nostdlib") (Link.Text "-static") (Link.Text "-no-pie")
                    (Link.Stack (mebibytes 16)) (Link.Text "-lgnat-user")
                    (Link.Script @src/userspace/c/link.ld) (Link.File @src/userspace/c/build/crt0.o)
                    Link.Manifest_Object Link.Runtime_Library (Link.Text "-Wl,--build-id=sha1")]))
(define image-defaults Image_Settings (Image_Settings modeset => false serial-console => true))
```

**Identity is per value, not per file.** A step's identity already covers
only what the step runs (the step value), not the files the values came
from. Changing `native.compiler` re-identifies every compile step that uses
it and nothing else; changing `image-defaults` re-runs only image steps.
Editing a comment in `build-params.ccl` re-runs nothing.

### 3. Fetchers are fixed-output steps

```lisp
(define sparknacl (fetch.git url => "https://github.com/rod-chapman/SPARKNaCl"
                             commit => (commit "49e3bddf092561ce2b74c134a35acff91a2da9a4")
                             sha256 => (sha256 "5d1f...")))
(define binutils-src (fetch.url url => "https://ftp.gnu.org/gnu/binutils/binutils-2.46.tar.xz"
                                sha256 => (sha256 "a3c2...")))
(define binutils-tree (unpack.tar-xz binutils-src))
```

- A fetch's identity is its declared `Sha256` (the output identity), not
  its URL, so a mirror change re-runs nothing.
- `fetch.git` takes a commit, never a branch or tag name: `Commit` is a
  type of 40 hex digits. `fetch.url` takes `https://` and `ftp://` URLs.
- **Only fetch steps get network authority**: on CuBit, a channel to the
  TLS service and netstack scoped to the URL's host; every other step has
  none. On Linux the executor's other steps already have no declared
  network (not enforced on the host, as for files).
- The bytes are hashed before they enter the store; a mismatch is refused
  and nothing is stored.
- A build refuses an unpinned fetch. `build.pin` runs the fetch once,
  prints the hash and offers to write it into the declaration (like
  `nix-prefetch-url`), visibly.
- Unpacking (`unpack.tar-xz`, `unpack.zip`) is a separate ordinary step:
  the archive is pinned, the tree is derived, and the unpacker never has
  network authority.

### 4. Proofs are cached steps

```lisp
(proof @src/userspace/lib/storehouse/storehouse-canonical.adb level => 1)
```

A gnatprove run is a step: its inputs are the unit's closure, the proof
switches and the prover toolchain; its outputs (the `.spark` files and the
summary) are store entries. A unit is compiled with `-gnatp` only if its
proof step exists in the build and succeeded: `Profile.suppress_checks` is
admitted per unit by a proof entry, so "proved, therefore -gnatp" is
checked by the build instead of by convention. Re-proving after an
unrelated edit is a cache hit. Proof steps run under the memory budget
the step declares (the 24 GB cap becomes a step limit).

### 5. Console tools

| Tool | What it shows |
|---|---|
| `(build.why "sleep.app")` | why each step ran: no record, output collected, or which input identity changed since the last build of that step (and the value or file it came from) |
| `(build.diff gen-41 gen-42)` | two builds or generations compared by identity: changed steps, changed outputs, changed parameters |
| `(build.graph "sleep.app")` | the step graph as a live node view (stream graph inspector): inputs, outputs, hit or ran, duration |
| `(build.run "sleep.app")` | build, then launch the output as a new generation entry (with LIVE-001, hot reload) |

### 6. Language features this needs

| Feature | Use | Status |
|---|---|---|
| Imports and modules | `build-params.ccl`, shared rule families (`cubit.app`) | not in CCL yet; not covered by ccl-capacity.md |
| Functions over records and lists, function values | rule families written in CCL; `map` over sources and units | functions exist (16 per program, 8 parameters); function values not yet; sizes lifted at ccl-capacity.md **M1** (slices 1 to 5) |
| Record update (`with`) | overrides, profiles, image variants | not yet |
| Typed file names `@here`, `@src` that cannot escape the tree | every path in a declaration; replaces strings checked by `Storehouse.Paths` | not yet; the path rules exist (proved) in `Storehouse.Paths` |
| Typed `Sha256`, `Commit`, `Url` | pins and fetches; malformed hashes are type errors | not yet (strings checked by the tool today) |
| Larger programs and values | a rule family's expansion | ccl-capacity.md **M1** |

Rule families stay in Ada until M1 lands (user, 2026-10-19); none of this
works around the caps in CCL.

**Acceptance test** for this target: the Ada program rule family rewritten
in CCL gives step identities identical to the Ada family's for every
migrated program (`tests/ccl-build` compares them), and every app's
`component.ccl` is about three lines.

### Nix feature mapping

| Nix | CCL build |
|---|---|
| `default.nix` / `mkDerivation` per package | `(cubit.app "name")`, conventions plus `with` |
| `stdenv`, setup hooks, phases | profiles (values) and rule families (functions returning steps) |
| `overrideAttrs`, `.override` | `with` (record update) |
| overlays | a different `Profile` or `build-params` value passed in |
| flake inputs, `flake.lock` | `build-params.ccl` pins (`Toolchain`, `fetch.git` with `Commit` and `Sha256`) |
| `fetchgit`, `fetchurl` (fixed-output derivations) | `fetch.git`, `fetch.url`, the only steps with network authority |
| `nix-prefetch-url` | `build.pin` |
| `nix why-depends`, `nix-diff`, `nix log` | `build.why`, `build.diff`, the step's captured outlets |
| `nix build --check` | rerun and compare (reported automatically when a rerun differs) |
| NixOS module system, system configurations | image and system declarations as values; variants by `with` |
| `nix run` | `build.run` (a new generation entry) |
| binary caches | signed Storehouse substitution through `tls.svc` (later) |
| `nix-store --gc`, profiles | generations as GC roots |
| — | proofs as cached steps admitting `-gnatp` (no Nix equivalent) |

## First slice (2026-10-19)

Hosted on Linux; nothing runs on CuBit yet.

- **Code.** `userspace/lib/storehouse/` is the portable SPARK core:
  `Storehouse.Hex` (identity names), `Storehouse.Canonical` (deterministic
  CBOR heads, encoder and refusing decoder), `Storehouse.Hashing` (streamed
  SHA-256 over SPARKTLSCrypto), `Storehouse.Paths` (relative paths, literal
  arguments). `userspace/ccl/tools/ccl-build/` is the Linux tool: the store
  (`Build_Store`), steps, identities and the executor (`Build_Steps`), the
  CCL declaration reader (`Build_Declarations`, through the CCL type
  checker and VM), the Ada program rule family (`Build_Ada_Programs`) and
  the system-call layer (`Build_POSIX`). Schema: `userspace/ccl/interfaces/
  component.ccl`; first declaration: `userspace/apps/sleep/component.ccl`.
- **Commands.** `ccl-build build --store DIR --source-root DIR [--import
  NAME=PATH] [--out DIR] COMPONENT.ccl`, `ccl-build verify --store DIR`,
  `ccl-build hash-tree DIR` (to pin an import).
- **sleep.app:** six steps (manifest, its object, `gcc -c main.adb`,
  `gnatbind -x`, the binder unit, the link). `manifest.S`, `startup.ads`,
  `manifest.o`, `main.o`, `b__main.o` and `sleep.app` are byte-identical to
  `make sleep`'s. A rebuild runs nothing (6 answered, about 50 ms). Another
  checkout sharing the store runs nothing. Touching `main.adb` runs nothing.
  A comment edit reruns `gcc -c main.adb` only: the object and `.ali` are
  unchanged, so bind, binder unit and link are answered (early cutoff). A
  code edit reruns compile, bind and link; the binder unit is still
  answered because `b__main.adb` did not change.
- **Make's side:** reproducibility switches
  (`-gno-record-gcc-switches -ffile-prefix-map=<checkout>=.`); before
  them, two make builds differed (gprbuild's temporary file names in
  `DW_AT_producer`). Superseded by the second slice: the switches now come
  from `userspace/toolchain/gnat-15.cgpr` for every native unit.
- **Toolchain:** first Alire's `gnat_native` 16.1.0, copied into the store
  (990 MB, about 20 s); replaced by GNAT 15.3 in the second slice. The
  runtime library, `crt0.o` and `ccl-manifest` are make-built inputs taken
  by content.

**Tested** (`tests/ccl-build/run.sh`: 31 checks plus the proof, all pass): core unit tests
(RFC 8949 appendix A vectors, refusal of non-shortest heads, negative
integers, maps, tags, floats, indefinite lengths and truncation, 200,000
random round trips, FIPS 180-4 SHA-256 answers including streamed pieces,
path and literal rules: 80 checks); store and executor tests (put and
lookup, read-only entries, kinds as separate namespaces, tree identities
including the executable bit and links, cache hits and misses, labels
outside the identity, early cutoff, an undeclared input being absent, a
path in a literal refused, a non-reproducible step reported, a tampered
entry found by `verify`, a damaged action record refused, simulated crashes:
staging of a dead process removed, a missing action record rerun to the same
outputs: 37 checks); the sleep.app comparisons and cache behaviour above;
and a **real crash**: SIGKILL three seconds into the toolchain import leaves
no tree entry, `verify` passes, and the next build removes the dead
process's staging and completes.

**Proved** (`run.sh --prove`: gnatprove level 1, `-j2`, under a 24 GB
memory cap; 159 checks, none unproved) for `userspace/lib/storehouse`:
- absence of run-time errors in all four units, so the core is safe to
  compile with `-gnatp` (its proof project does);
- `Head` writes exactly the shortest canonical head (`Is_Head`, with the
  RFC 8949 length rule), and `Read_Head` is valid only on bytes equal to
  `Head`'s output for the value it returns: one value, one encoding, and
  the decoder accepts nothing else;
- `Hex.Parse` inverts `Hex.Image` on every valid name (one spelling per
  identity);
- termination of the streamed hashing loop.
SHA-256 itself is SPARKTLSCrypto/SPARKNaCl's, proved upstream, not here.
The host tool (store, executor, rule, declaration reader) is ordinary Ada
with run-time checks and assertions on, and is tested, not proved.

**Assumed:** that tools read only their
inputs (Linux does not enforce it; the toolchain is called inside the store
by absolute path); that `.ali` `W` lines name every program unit a unit
depends on; that the host's `/bin/sh`, `cat`, `grep` and `od` exist (tests
only); that `rename` and `link` are atomic and `fsync` durable (ext4/ext3).

**Not yet:** the recorded-trace optimisation (dependency step 2), `-MD` for
C, `explain`, GC, environment variables in steps (always empty), parallel
steps, multi-unit programs beyond the code path (sleep has one unit; the
`W`-line discovery is exercised only with no further program units).

## Second slice (2026-10-19): GNAT 15.3, the converter, 46 programs

Hosted on Linux; nothing runs on CuBit yet.

**GNAT 15.3 for all native Ada.** `userspace/toolchain/gnat-15.cgpr`
configures gprbuild for the musl cross GNAT 15.3 with nixpkgs' binutils;
`kernel/Makefile`'s `NATIVE_GPRBUILD` (77 targets, the user runtime and
CuAlloc) and the build scripts that run gprbuild themselves
(`tools/build_desktop_vulkan_*.py`, `build_mesa_desktop.py`,
`build_teapots.py`, `config-storage/build.sh`,
`servo/build-cubitshell.sh`, through `CUBIT_NATIVE_GPRBUILD_CONFIG`) use
it, and manifest objects are assembled by the same driver (`NATIVE_CC`).
- All 77 native gprbuild targets, `config-storage` and `teapots` build
  with 15.3; **no source change was needed**. Servo's shell
  (`build-cubitshell.sh`) is switched but was not rebuilt (hours of Rust).
- Warnings: the same on sampled units (`bench-input`: 30 warnings with
  either compiler). Code differs, as expected between compiler versions
  (`sleep.app` 375,008 bytes with 16.1, 370,216 with 15.3); nothing has
  been run on CuBit or QEMU with the 15.3 build yet.
- The kernel and its ZFP runtime stay on Alire's 16.1 (not touched).

**The converter** (`tools/gpr_to_ccl.py survey|convert`), a one-time
migration tool (Python, like the other host build scripts; it is deleted
with the last `.gpr`). Of 115 non-test `.gpr` files, **47 fit** the subset
native programs use: `Languages ("Ada")`, `Object_Dir "build"`,
`Source_Dirs`, one `Main`, the user runtime, `Library_Dir ""`, and
`Default_Switches` of Builder, Compiler, Binder and Linker plus
`Builder'Executable`. Its output is typed CCL, and `ccl-build check`
type-checks every `component.ccl` against `build-params.ccl`. What does not fit,
by reason (apps and services only):

| Reason | Count | Examples | What CCL build needs |
|---|---|---|---|
| imports another project (`with`) | 17 | the UI toolkit apps, tls, ccl-console | a library component kind (fonts, toolkit flags, SPARKTLS) |
| reads an external (scenario variable) | 5 | nvme, config, ccl-vm | typed parameters in `build-params.ccl` or the component |
| case statement on an external | 4 | desktop, display, virtio-gpu | variants as values (usability target 1) |
| `Source_Files`, `Excluded_Source_Files` | 3 | ramdisk, devmgr, acpi | explicit unit lists in the schema |
| more than one main | 2 | ipctest-client, bench-load | `mains => [...]` |
| global configuration pragmas | 1 | netstack | a `configuration_pragmas` file field |
| make-generated sources | 1 | clock (`time_zones`, `clock_floor`) | generator steps |
| extends another project | 1 | config-denied | a variant of config-check's value |
| library projects | 1 | (services) | library kind |

Host tools, the kernel, libraries and tests are not native programs and
were not attempted.

**Migrated: 46 programs**, each built by gprbuild (15.3) through make and
by ccl-build, then compared byte for byte with `tests/ccl-build/compare.sh`
(executable, every unit object, the binder object, manifest assembly,
object and startup bindings; `.ali` and `b__main.ad[sb]` record paths and
are not compared): all identical. Their make targets now call
`$(CCL_BUILD) <dir>/component.ccl` (make stays the entry point; outputs are
copied back to where make put them), and their `.gpr` files are deleted,
except `config_check.gpr` (extended by config-denied) and `metricsvc.gpr`
(a compositor test fixture builds it): those two are translated, and their
make targets use ccl-build. The programs:

- apps: avx-check, bench-audio, bench-input, bench-ipc-client,
  bench-ipc-server, bench-spread, bench-storage, boot-timeline,
  capability-test, clock-check, config-check, devmem-check, fault-check,
  filesystem-scope-check, futex-check, gpu-viewer, input-stress,
  ipctest-server, log-check, log-fields-check, log-retire, metrics-check,
  network-check, oom-check, shell, sleep, stall-check, storage-check,
  thread-check, timesync-check, tls-check, wget;
- services and drivers: ata, hda, intel-gpu (337 steps), logstore,
  metricsvc, mixer, netmgr, procmgr (66 steps, no manifest), ps2,
  timesync, virtio-input, virtio-net, xhci, ccl-test-host.

**Blocked: filesystem.svc.** It compiles with `-flto=auto`. GCC's LTO
objects are not reproducible across directories even with
`-ffile-prefix-map` and `-frandom-seed` (tested on a one-unit package: the
same directory and seed give the same bytes, another directory does not),
so ccl-build's step directory gives different objects from make's. It
stays on gprbuild until LTO is made path-independent or dropped for it.

**What the rule family learned on the way** (each needed for byte
identity with gprbuild): sources in several directories (`-I` in search
order); program units in a directory the runtime also searches
(`shared/boot`) are compiled from the runtime's tree; gnatbind lists only
objects it finds beside their `.ali`, so the bind step gets the unit
objects; gprbuild names the object directory in gnatbind's object path
(`-aO`), which adds a comment line to `b__main.adb` and so moves line
numbers in `b__main.o`'s debug information; link order is main, binder,
then gnatbind's object list; `-L` runtime library, `-B` binutils and
`--build-id` close the link, as `gnat-15.cgpr` does. Programs without a
manifest (`procmgr`) are `catalog => Catalog.No_Manifest`.

**Tested:** `tests/ccl-build/run.sh` (31 checks: unit tests, every
`component.ccl` type-checks, sleep and the multi-unit boot-timeline build with
15.3, cache, another checkout, a fresh store giving the same `sleep.app`
bytes, mtime, early cutoff, code edit, store verification, SIGKILL during a
tree copy and recovery) plus the 46-program comparison above and `make`
building migrated targets through ccl-build (all answered by the cache,
outputs unchanged). **Proved:** the Storehouse core, as in the first
slice (159 checks, level 1, none unproved); the rule family and converter
are tested, not proved. **Assumed:** gprbuild's link order and binder
inputs as observed on these 46 programs; the Nix closure of the toolchain.

## Third slice (2026-10-20): GNAT 15.3 validated on CuBit, rebased onto micro

Hosted builds, then CuBit under QEMU (KVM) and the UEFI live image.

- **The whole world builds with GNAT 15.3** (`make world
  CUBIT_COMPOSITOR_TIMING=on`), with the 46 migrated programs built by
  ccl-build through make. Native test fixtures that still used Alire's
  16.1 (`tests/grant-forward`, `tests/mesa-anv`, `tests/ccl-cbor`
  native probe, and the fixture scripts the headless runner calls:
  gcc, binutils, control-events, process-spawn, render-startup,
  config-object-client, config-turso, plus seven ad hoc native-fixture
  scripts) now take the same configuration, from make
  (`print-native-gprbuild-config`) when not run by it; the headless runner
  exports it. gnatbind refuses to mix the 15.3 runtime with 16.1 objects,
  so a missed fixture fails loudly rather than silently.
- **Headless suite on the 15.3 build, all PASS** (24 cases, `--accel kvm`,
  under the shared build lock): desktop-display, desktop-dual-output, text,
  files, logs, launch-failure, async-ipc, capability-security, oom-reclaim,
  storage-grants, ccl-console (`--timeout 90`), boot-timeline, threads,
  futex, clock, avx, log-fields, devices, timesync, bench-spread,
  processes, control-events, tls-service, network-authority.
- **UEFI live image** (`usb-live-uefi-iso`, booted with
  `tests/usb-optical/run-live.py --uefi --cpus 4 --quiet-xhci`): USB LIVE
  BOOT PASS; the desktop, Files, Workbench and DOOM screenshots render. The
  Servo payload was the main checkout's existing `cubitshell.app` (Servo
  was not rebuilt with 15.3). `config-storage` exits with "missing/invalid
  bootstrap database path" on the live image; earlier live-boot logs from
  before the switch show the same line, so it is not a 15.3 issue.
- **No 15.3-specific failure was found**, so nothing needed fixing in the
  sources. Not run: the Servo test, the bench-* timing comparisons, hardware.
- **Found: Nix garbage collection removed the GNAT 15.3 store path**
  between sessions (the dev shell is not a garbage-collector root), so
  `nix develop` rebuilt the cross compiler from source (about 40 minutes;
  `pkgsCross` GNAT is not in the binary cache). The rebuilt tree had the
  same identity (`537d17c4...`): the Nix build of GCC 15.3 is
  reproducible. ccl-build now registers a Nix garbage-collector root
  (`<store>/imports/<identity>.root`, `nix-store --add-root`) whenever it
  records a Nix tree, so a pinned toolchain stays as long as the
  Storehouse names it.
- **Rebased onto micro** (`fe529c25`, apps: calculator, dictionary,
  lexicon). The calculator and dictionary use the UI toolkit (`with`) and
  stay on gprbuild through `NATIVE_GPRBUILD` (15.3). The lexicon service
  fits the subset: translated, compared byte for byte, its make target
  calls ccl-build and `lexicon.gpr` is deleted. **47 programs** now build
  with ccl-build; the comparison of all 47 against gprbuild on the rebased
  tree: 47 identical.

## Fourth slice (2026-10-20): libraries and the toolkit apps

**What the `with`-ed projects really are.** The UI toolkit is not a
library build: every toolkit app lists the toolkit's directories in its own
sources and compiles them itself. The `with`s bring three things:
- `fonts_native.gpr`: an externally built archive (`libcubit_fonts.a`,
  from `userspace/rust`), linked after the program's objects, with its
  directory in gnatbind's object path (so in `b__main.adb`'s listing);
- `toolkit_flags.gpr`: per-file switches for the pixel loops
  (`client_raster.adb`, `client_glyph_blend.adb`);
- `calc_flags.gpr`: only a variable (the proved units' switches) that
  programs use in their own per-file switches.

**Library component kind.** `build-catalog.ccl` holds the libraries:
each `Library` has a name, source directories compiled into each user,
units with a named code profile, and archives. A component names the
libraries it uses (`libraries => ["ui-toolkit"]`) and may add its own unit
groups (`units => [(Unit_Code [files] "proved")]`). A unit's switches are
those of the first group naming it (the component's, then its
libraries'), built from the code profile and the component's
`debug_info`. Archives are linked after the objects and named in
gnatbind's object path. (The first version of this slice spelt the
switches out per program and kept the libraries in a separate
`build-libraries.ccl`; the declarative model below replaced both.)

**Migrated: filesystem.svc** (after the LTO decision below; 59 programs in
all). **Migrated: 11 toolkit apps**, each compared byte for byte with gprbuild
15.3 (all identical): calculator (with its proved units), files, devices,
desktop-shell, boot-logs, clipboard-check, config-inspector, desktop-check,
dictionary (its lexicon wire unit), logs, trace-viewer. Their `.gpr` files
are deleted and make calls ccl-build. **58 programs** now build with
ccl-build.

**Still on gprbuild, and why:**
- **CCL capacity (ccl-capacity.md M1):** `text` (ten proved units in one
  group give `PROGRAM_FULL`), and the SPARKTLS library (eleven source
  directories in the Nix crate tree, ten excluded files, four per-file
  switch groups): its declaration is far over the cap, so tls, tls-probe,
  ccl-console and ccl-workbench wait for M1. The rule family needs no new
  idea for it: a library compiled in its own object directory with its own
  switches, sources referenced in place in the crates' Nix tree, its
  objects in gnatbind's list.
- **Not built by make through gprbuild alone:** teapots and the desktop
  (Python scripts that compile, then link with Mesa and C glue);
  observatory (no make target).
- **Scenario variables and case statements** (desktop, display,
  virtio-gpu, display-check, nvme, config, ccl-vm, ccl-control,
  config-storage's worker): need typed build variants (usability target 1).
- **Several mains** (ipctest-client, bench-load), **explicit source lists**
  (ramdisk, devmgr, acpi), **global configuration pragmas** (netstack),
  **generated sources** (clock), **extends** (config-denied), **LTO**
  (filesystem).

### filesystem.svc with and without LTO (measured 2026-10-20, for the decision)

Both GNAT 15.3, KVM, 4 vCPUs, `tests/headless/run.sh --test bench-fs`, three
runs each (nine rounds per workload; medians, with min and max):

| | with `-flto=auto` | without |
|---|---|---|
| filesystem.svc file size | 1,480,240 B | 1,544,040 B |
| text / data | 262,258 / 136 B | 285,682 / 45,736 B |
| seq-write MB/s | 339.8 (251–370) | 339.1 (220–367) |
| seq-read warm MB/s | 17,458 | 17,712 |
| seq-read cold MB/s | 22,114 | 21,398 |
| rand-read IOPS | 2,538,702 | 2,672,246 |
| rand-write IOPS | 313,262 (95k–1.03M) | 376,411 (89k–920k) |
| rand-write-fsync IOPS | 510 (p99 4.8 ms) | 521 (p99 5.1 ms) |
| create per s | 49,359 | 54,883 |
| open-read per s | 382,898 | 417,603 |
| list per s | 2,616,043 | 2,434,518 |
| unlink per s | 255,365 | 240,751 |

The differences are within run-to-run spread; reads are served mostly
from the client's page cache, so the server's code matters most in
create, unlink and fsync, which move a few percent either way.
storage-grants passes 3 of 3 both ways. bench-fs reports coherence
FAILs both ways (`other-exit-left-open`, `reopen-after-other-rename`,
sometimes `reopen-after-other-unlink-recreate`); they predate this work
(coordination/fs-split.md, 2026-10-11) and vary between runs. Without LTO,
filesystem.svc builds byte-identical with ccl-build (checked; not
migrated, its build is unchanged).

## Declarative model (2026-10-22/23)

The user's decision (backlog BLD-001): the type stays `Component` (later
nested in `Computer`/ISO, SYS-001, then `Datacenter`, DC-001); files are
`component.ccl`; a component has a `kind` and a `source` with the
language-specific details, and names only what differs.

- **Shape** (above, "Steps"): `(Component name kind source)`, `Kind` =
  `Application | (Service Launcher) | Driver | Tool`, `Source` = `Ada`
  today (room for C and Rust). `build.ccl` and the old
  `Component.Ada_Program` shape are refused (negative tests), never
  translated.
- **Intent instead of switch lists.** Every `Switches.Exactly` copy is
  gone. What differed between programs was always one of six facts:
  Ada 2022 or not, -O2 or -O3 (`Maximum`), checks on or suppressed
  (proved), frame pointer omitted or kept, value-range propagation, and
  debug information; plus switches that change no code (warnings,
  stack-usage reports). Proved units name the `proved` profile; the UI
  toolkit's pixel loops `pixel-loop`; the toolkit's five source
  directories and font archive are the `ui-toolkit` library.
- **Derived:** executable (name + kind's suffix), catalog (from the
  launcher), binder-unit switches (the program's without GNAT's), and the
  manifest object (left out for a kernel-started service).
- **Reported, not special-cased:** filesystem, mixer and netmgr are
  services whose manifests use the device manager's catalog: devmgr
  launches them. That is expressed as `(Kind.Service
  Launcher.Device_Manager)`, a fact about the component, not an override.
  procmgr is `(Kind.Service Launcher.Kernel)` (no manifest). Two
  components' names differ from their directories (`ccl-test-host` in
  `userspace/ccl/services/test-host`, `metrics` in
  `userspace/services/metricsvc`): the name is the program's, the
  directory is where `component.ccl` is.
- **Strict:** unknown fields are type errors that name the field and give
  line and column; a catalog cannot be stated (so cannot contradict the
  kind); unknown libraries and profiles list the known ones; names must
  be lowercase kebab; the manifest object cannot be linked twice.
- **Calculator and text** no longer need CCL capacity M1: naming the
  `proved` profile instead of spelling out switch lists makes their
  declarations small. Both migrated, byte-identical to gprbuild 15.3:
  **60 programs** build with ccl-build.
- **Command lines changed, programs did not.** The switches are now
  composed in one fixed order, so step identities of the compile steps
  changed (the store keeps both); every program binary is byte-identical
  (see "Identity report").

### Identity report (declarative model)

Before: the commit candidate's tree (old schema, `build.ccl`), built in a
scratch checkout; after: this tree (`component.ccl`); one Storehouse for
both. All 60 programs are byte-identical: the 58 previously migrated and
calculator and text, which this pass migrated (each also compared with
gprbuild 15.3: identical). The compile steps' identities changed (the
switches are now composed in one order); their outputs did not, so every executable's Storehouse identity is
unchanged.

One difference appeared first and is not the model's: the Rust font
archive (`userspace/rust/build/font-native/libcubit_fonts.a`) embedded the
checkout's absolute path, so the toolkit applications built in two
checkouts differed. With the same archive they were identical.

### Checkout-independent fonts archive

The archive embedded the path two ways, both fixed in
`userspace/rust/Makefile`:

- **rustc's panic and debug paths.** Cargo now passes
  `--remap-path-prefix=<checkout>=.`, as `gnat-15.cgpr` maps the path to
  `.` for Ada.
- **The allocator objects it bundles.** Its rules ran gprbuild on
  `allocator_native.gpr` without the native configuration, so gprbuild
  used alr's default GNAT (16.1) and its own switches: `cualloc_native.o`
  was recompiled by GNAT 16.1, without `-ffile-prefix-map`, and went into
  the archive and so into every toolkit program. The rules (and
  config-storage's `build.sh`, the other such call) now pass the native
  configuration.

`tests/reproducible-paths/check.sh` (after `make world`) fails when an
archive or a component program embeds the checkout path, or when an archive
holds Ada compiled by anything but GNAT 15.3. Built in two checkouts, every
component program and the fonts archive are now byte-identical. The
allocator library's members are identical too; the library itself differs
only in the timestamp ranlib writes into its symbol index (`gnat-15.cgpr`
indexes without `-D`), which no program links.

## Embedded blobs (2026-10-23, HW-002)

A component can link bytes into its program, such as a driver's firmware.
The firmware itself lives in the top-level `firmware/` directory, laid out as
linux-firmware lays it out (docs/device-matching.md, "Firmware"). The
declaration is `embedded`, a list of `Blob` values in `component.ccl`. The
types are in `interfaces/build.ccl`.

- **Source.** A blob comes from one of two places:
  - `(vendored path digest)`: a file under `firmware/`. It is hashed and
    stored like any input. A path outside `firmware/` is refused.
  - `(fetch url digest)`: an `http://` or `https://` download. It runs once,
    as its own logged action (`fetch <name>`), with an empty environment and
    the host's `curl`. The bytes are hashed before they are stored. After
    that the blob is answered from the store by its digest, with no network.

  The digest is a `SHA256_Digest`: eight 32-bit words of the hex digest, in
  order. The CCL type checker refuses a word wider than 32 bits and a digest
  with a word missing. A mismatch is a hard error naming the blob, both
  digests and the location, and a refused fetch stores nothing.
- **Steps.** Each blob has one `embed <name>` step. It runs the binutils
  `objcopy -I binary`, with the bytes placed as `blob` in the step's working
  directory, so the symbols do not depend on the file's name. It renames
  them `cubit_blob_<name>_start`, `_end` and `_size`, puts the bytes in
  `.rodata`, adds an empty `.note.GNU-stack`, and writes
  `build/embedded/<name>.o`. The program links that object after its own.
- **Ada.** ccl-build writes `build/generated/embedded.ads`, package
  `Embedded`. For each blob it declares `<Name>_Bytes` (the exact length),
  `<Name>_Data` and `<Name>`, a constant byte array imported from the
  `_start` symbol. Programs never write Import names themselves. Only
  programs that declare blobs get the package.
- **Licence.** Each blob's licence file becomes a product at
  `build/licenses/<file>`, so `--out` exports it. The bundle layout
  (`Drivers/<name>/<version>/licenses/`) takes it from
  there.
- **Tests** (`tests/ccl-build/run.sh`):
  - negative declarations: a digest word too wide, seven words, a misspelt
    Blob field, a vendored path outside `firmware/`;
  - the `fixtures/embedded` program: its blob's bytes in the linked ELF hash
    to the pinned digest (`check-embedded.py`), and its licence is exported;
  - a wrong vendored digest is refused;
  - a fetch from a local HTTP server: a wrong digest is refused and nothing
    is stored; the right digest builds; with the server stopped, a rebuild
    is answered from the store.

## Open questions for the user

1. ~~Read `.gpr` files or translate once?~~ Decided: translate once, then
   delete (user, 2026-10-19).
2. **Store location on the Linux host.** *Recommendation:*
   `~/.cache/cubit/storehouse`, shared by all worktrees, always passed
   explicitly by make.
3. ~~Rule families in Ada now?~~ Decided: in Ada until ccl-capacity M1.
8. ~~filesystem.svc and LTO~~ Decided (user, 2026-10-20): drop
   `-flto=auto` for now and reconsider in a performance pass. filesystem.svc
   is migrated (byte-identical to gprbuild 15.3 without LTO). LTO is not
   inherently non-reproducible: the cross-directory difference most likely
   comes from per-unit random seeds (`-frandom-seed`), LTO's temporary file
   names under `TMPDIR`, and possibly paths in LTO sections that
   `-ffile-prefix-map` does not map. A pass that wants LTO back should
   derive `-frandom-seed` from the step identity and keep LTO's temporaries
   inside the step directory, then check byte identity again.
9. **The kernel's toolchain:** move the kernel (and its ZFP runtime) to
   GCC 15.3 too, so the whole system has one compiler. *Recommendation:*
   yes, as the kernel's own migration step, with a QEMU boot test under
   the build lock.
4. **Fix make's reproducibility (prefix maps) as each family migrates?**
   *Recommendation: yes*; it changes debug info paths from absolute to
   checkout-relative (`./userspace/...`), which debuggers resolve from the
   checkout root.
5. **Binary substitution between machines:** signed entries
   (SIGN-001 keys) fetched by identity through `tls.svc`. *Recommendation:*
   after self-hosting; a local store is enough until then.
6. **Should CCL source files (packages, declarations) be Storehouse entries
   too?** *Recommendation: yes for installed packages* (a generation pins
   its declarations by identity), no for the working tree, which Git/jj
   holds.
7. **Relation to the filesystem journal:** the store needs only `rename`
   and `fsync` atomicity, which ext3 data=ordered provides; no new journal
   feature. *Recommendation:* no further work.
