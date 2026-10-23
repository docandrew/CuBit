# Declarations checked at build time (CAP-003)

Status: design, 2026-10-22. Nothing below is implemented yet. Backlog:
CAP-003, ahead of CFG-003.

## Why

The user: "I was hoping that the type-safety of manifest/endpoint
declarations would also help us identify these issues at build-time -
since we declare both inbound and outbound capabilities and their typed
endpoint messages."

After CAP-002, six regressions reached hardware silently:

| # | Regression | Manual step that was skipped |
|---|---|---|
| R1 | Desktop's log publisher never bound | `CuBit.Logging.Bind` |
| R2 | Files' log publisher never bound | `CuBit.Logging.Bind` |
| R3 | config-storage's Rust `_start` dropped the launch-block length | hand-copied entry glue |
| R4 | config-storage compared the message tag with Config's PID | hand-written badge check |
| R5 | the gcc test lacked `CuBit.Launching.Bind` | `CuBit.Launching.Bind` |
| R6 | Desktop's manifest lacked a Config scope for `desktop.display.*` | keeping string keys and scopes in step by hand |

The manifest already declared what each program needed. Nothing made the
code use the declaration, or checked that the image could satisfy it.

**The rule.** Anything a manifest declares is used through a type that a
program can only get from its generated bindings. Anything the system
composes is checked when the image is built. A runtime backstop catches
what types cannot see, and says which declaration was wrong.

Four mechanisms, from earliest to latest:
1. **Per program, at compile time:** generated typed objects (sections 1–3).
2. **Per image, at build time:** the declaration check (section 4).
3. **At first boot, in the pre-landing set:** the startup-health test (section 5).
4. **On every refusal, at run time:** one Warning naming the program and the declaration (section 5).

## 1. Per program: generated startup objects, not raw handles

Today `ccl-manifest --startup-output` generates `package Startup` with one
function per binding, each returning a plain `CuBit.Handles.Endpoint`, and
a `Verify` the program may or may not call. 46 of the 111 Ada programs with a
manifest never call `Startup.Verify`. The library objects (log `Publisher`
and `Reader`, `CuBit.Launching`'s package state, `CuBit.Config`'s package
state) start unbound and take any endpoint.

### Decisions

- **Typed endpoints per service.** The service catalog gives each service
  that has a runtime client a handle type:
  `Logstore_Endpoint`, `Log_Observer_Endpoint`, `Log_Control_Endpoint`,
  `Process_Manager_Endpoint` and `Config_Endpoint` first. They live in a new
  `CuBit.Handles.Services` and have no public constructor. The generated
  `Startup.Logstore` returns `Logstore_Endpoint`. Services without a typed
  client keep returning `Endpoint`. The catalog row names the type, so
  ccl-manifest has no list of special service names (no CCL special cases).
- **Objects that cannot exist unbound.** Each client type gets unknown
  discriminants, so a declaration must initialize it, and the only
  initializer takes the typed endpoint:

  ```ada
  type Publisher (<>) is limited private;
  function Bound_To (Target : CuBit.Handles.Services.Logstore_Endpoint) return Publisher;

  Writer : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
  ```

  `Writer : CuBit.Logging.Publisher;` alone is a compile error. A program
  whose manifest does not request logstore has no `Startup.Logstore`, which
  is also a compile error. Build-in-place keeps the publisher's aligned
  pages where they are now. The same applies to `Reader`
  (`Log_Observer_Endpoint`) and to a new `CuBit.Launching.Launcher`
  (`Process_Manager_Endpoint`), which `Launch`, `Describe`, `Lend_Ring` and
  `Launch_Table` take as their first parameter. `CuBit.Launching` loses its
  package-level binding.
- **Verify cannot be skipped.** The generated `Startup` package checks the
  table during its own elaboration, before `main`. If a required entry is
  missing or of the wrong kind, it logs a Critical naming the program and
  the binding (for example `startup table has no "config"`) and exits with
  a typed status. If `.cubit.startup` declares entries but the launch block
  is empty, it says so: "launch block not delivered; the entry glue
  dropped it". That is R3's signature. `Verify` stops being public.
- **Typed Config accessors for exactly the declared settings.** This needs
  the setting catalog (below). For each Config scope it declares, a program
  gets `Startup.Settings.<Name>` returning that setting's typed value, or a
  `Set_<Name>` for writable scopes. A key family gets an accessor that takes
  the family's typed parameter, for example
  `Display_Mode (Monitor : Monitor_Name)` for `desktop.display.mode.<monitor>`.
  Every result is `(Value | Absent | Denied | Invalid | Unavailable)` and
  must be examined. "Use the default" is only the outcome `Absent`; a
  `Denied` is never folded into a default the way Desktop does today. A key
  outside the declared scopes has no accessor, so using one is a compile
  error.

### The setting catalog (new)

`userspace/ccl/interfaces/settings.ccl` is a typed CCL `Setting_Catalog`.
Each entry gives:
- the key, or a key family with a typed parameter;
- the CCL type (from the existing `CCL.Interfaces.*` declarations);
- the class (`Declaration_Managed | Application_State`,
  docs/config-declarative-state.md);
- the Ada codec that reads and writes the value.

It replaces the hand-written prefix table in `CCL.Typed_Settings.Kind_Of`.
config.svc and `CCL.Configurations` check values against the same
catalog. Today's untyped settings get types when they enter the catalog:
- `net.if0.*` and `net.dns.*` become one 16-byte address (see the single
  address type rule);
- `time.servers`, `tls.hosts`, `system.assets.root` and
  `cubit.config.storage.database` get typed name and path values;
- `logs.minimum-level` becomes `Severity`;
- the theme strings become `Theme`.

Manifests then name catalog entries (`(config Setting.Display_Scale
[File_Right.Read File_Right.Write])`), not string prefixes. ccl-manifest
derives the exact `.cubit.access` bytes from the catalog, so procmgr and
config.svc are unchanged.

CFG-003 (persisting user preferences as typed collections) builds on this:
its preference collections are catalog entries of class
`Application_State`. Their typed accessors and their schema identity come
from the same declaration.

### Removals (no compatibility aliases)

| Removed | Replacement | Call sites to migrate |
|---|---|---|
| `CuBit.Logging.Bind` (Publisher and Reader) | `Bound_To` | 25 calls in 18 files: 17 publisher binds and 8 reader binds, including the log-check and xhci/intel-gpu cases below |
| `CuBit.Launching.Bind` and its package state | `Launcher` from `Startup.Process_Manager` | 5 calls, plus about 15 `Launch`/`Describe`/`Wait` calls in 6 programs gaining a parameter |
| `CuBit.Config.get/set/delete/list/Bind` (raw `System.Address`) | `Startup.Settings` | 56 calls in 15 files (17 of them in the config-check test) |
| `CuBit.Config_Reader.Query` with a string key | `Startup.Settings`, or `Inspector` (below) | 20 calls in 6 files |
| `CuBit.Log`'s lookup of `"logstore"` by name | `CuBit.Log` takes the bound publisher from `Startup` | 1 (runtime) |
| String lookups `Endpoint_Named ("…")` outside generated code | generated functions | 19 calls in 17 files (gcc, gnat and process-spawn tests, libc, ccl_launcher, doom, sameboy, Penny audio…) |
| public `Startup.Verify` | elaboration check | 49 calls |

Programs that really do need arbitrary keys are the shell, config-inspector,
the CCL `config` built-ins and the config-check test. They declare
`config-all`, and only then do they get `Startup.Config_Inspector`, a
string-keyed `Inspector` object. A program without `config-all` cannot
obtain that type.

Endpoints obtained at run time, rather than from the startup table, need a
checked conversion until CAP-002's `Connect` returns typed handles:
- intel-gpu and xhci get logstore's endpoint from devmgr;
- the log-check test deliberately binds to wrong endpoints.

Production code gets `CuBit.Handles.Services.Accept_Logstore (Accepted)`.
The test-only escape is `CuBit.Logging.Unchecked`, and the build check
(below) refuses it outside `tests/`.

## 2. Message contracts and badges

### Today

Service protocols are hand-written Ada packages shared by both ends
(`CuBit.TLS_Protocol`, `CuBit.Log_Protocol` and others): numeric labels and
four words. Sharing the package gives compile-time agreement only when both
ends are built from the same tree. Nothing on the wire carries a contract
identity. Nothing ties a client's `request-service` to the protocol it
speaks. The catalog's `connections` (Offers/Uses) field is refused while
non-empty, waiting for CAP-002 step 4.

Badges (the `authorityTag` a launcher mints on an endpoint) are raw
`Unsigned_64` constants, hand-chosen in nine packages. Each is checked by a
raw `=` at the receiver and minted by Ada literals in procmgr or devmgr:

| Receiver | Badge(s) | Minted by | Check |
|---|---|---|---|
| devmgr | `Intel_Ready_Tag`, `Intel_GPU_Broker_Request.Authority_Tag`, `GP.Viewer_Tag` | devmgr (self-badged copies) | 16 raw `=` |
| intel-gpu | `Intel_GPU_Boot.Broker_Tag`; render sessions keyed by (sender, tag) | devmgr | raw `/=`; tag as a session key |
| netstack | `Policy/Manager/Driver_Authority_Tag`; grant tags in a range | devmgr; netstack's grants | 3 raw `=`, range checks |
| tls | `TLS_Protocol.Policy_Tag` | procmgr | raw `=` |
| clock | `Clock_Control.Authority_Tag` | procmgr (role literal) | raw `=` |
| mixer | `Audio_Control.Authority_Tag` | procmgr (role literal) | passed into `Mixer_Control` |
| desktop | `CuBit.Input.Publisher_Tag` | devmgr_launches, procmgr | raw `=` in `CuBit.Input` |
| logstore | publisher, observer and control tag ranges (`CuBit.Log_Protocol`) | procmgr, devmgr | `May_Publish`/`May_Invoke` |
| acpi | `ACPI_Launch.Bootstrap_Tag` | its launcher | raw |
| config-storage | **PID compare** (R4; still on micro). The fix in the cfg002 worktree, uncommitted, adds `Worker_Request_Tag` | procmgr | raw `=` |
| config.svc | none: kernel-reported sender PID against the registry, per-PID scope table | n/a | `isAdmin`, `Config_Authority` |

Which catalog role gets which badge is hard-coded in procmgr
(`isAudioControl`, `isClockControl`, `isLogObserver`…, main.adb around line
2674) and in devmgr.

### Decisions

- **Contracts are declared on catalog services, and offers and uses name
  them.** A catalog row names the service's contract (an interface
  descriptor: identity, version and digest, as in docs/typed-ipc.md). A
  manifest's `uses` names a service and the contract version it was built
  against. The server's `offers` names the contract it serves. The
  generated `Startup` package `with`s the contract's Ada package and states
  the version statically
  (`pragma Compile_Time_Error (TLS_Protocol.Contract.Version /= 2, …)`), so
  manifest and code cannot drift apart. ccl-manifest records each offer
  and use with its contract identity in a new `.cubit.contracts` section,
  which the image check reads.
- **One codec per contract, used by both ends.** Short term, the
  hand-written protocol package is that codec, with a declared
  `CuBit.Protocols` contract constant. Long term, it is generated from the
  descriptor (typed-ipc.md "Remaining integration" 6). Both ends depend on
  the package named by the catalog. The image check verifies that the
  identities match.
- **Badges are part of the contract.** A contract declares its badge
  roles, for example `(badges Client Policy)` for TLS. The generator emits:
  - `type TLS.Badge is (Client, Policy)`;
  - `function Classify (Request : Message) return Badge_Result`, which has
    an `Unknown` alternative;
  - `Mint (Endpoint, TLS.Policy)` for brokers.

  Badge words are derived from the contract identity and the role's
  position: no hand-chosen constants, no magic numbers. Servers dispatch
  with an exhaustive `case Classify (Request)`, so adding a role is a
  compile error at every server, and a raw `authorityTag =` is refused by
  the build check outside the generated code.
- **Brokers mint from declarations.** A client's `uses` names the role it
  asks for (`(Uses "tls" role => Policy)`). procmgr and devmgr mint the
  declared role, after checking it against the launch policy, instead of
  using their role-literal tables. R4 becomes impossible: config-storage
  offers `config-worker` with one role `Config`, Config's manifest uses that
  role, and the only accepted request is `Classify = Config`.
- **Session keys stay separate.** intel-gpu and netstack use (sender, tag)
  as a session key. Those badges are per-session issuances from a range.
  The contract declares them as a `Session` role with an issuance range, so
  `Classify` returns `Session (N)`.

This lands together with CAP-002 steps 4–5 (`Connect`, catalog), which
needs the same offers, uses and contract identities. The badge part can go
first (slice 5).

## 3. Non-Ada entry points

### Inventory of `_start` today

| File | Used by | Launch block |
|---|---|---|
| `userspace/c/crt0.S` | every Ada program (122 build files name `crt0.o`) and freestanding C | stores the length; payload readers split the trailer |
| `userspace/libc/crt/crt1.S` | musl/libc C programs | stores the length and splits the trailer inline |
| `userspace/rust/std/moto-rt-cubit/src/cubit.rs` | Rust std programs | stores the length and splits the trailer inline (second copy of the split) |
| `userspace/services/config-storage/src/lib.rs` | config-storage | **dropped it (R3)**; the cfg002 fix moves it to a third copy, `entry.rs` |
| `tests/config-turso/native/src/lib.rs` | Turso probe | stores it (hand copy) |
| `userspace/rust/probe/src/lib.rs` | Rust probe | passes RDI into `rust_main(len)` instead (another convention) |
| `userspace/runtime/gnat/art0.asm` | nothing (built by `userspace/Makefile`, linked by no one) | ignores RDI; dead |
| `tests/servo/audio-output/start.S` | fake devmgr in a test image | none needed (boot root) |

### Decisions

- **One entry source.** `userspace/runtime/start/cubit_start.S`, in GNU `as`
  Intel syntax (BLD-002's direction), does three things:
  - stores `__cubit_launch_block_length`;
  - splits the `CUBITLB1` trailer once;
  - calls a per-language hook (`main` for C and Ada, `__libc_start` for
    musl, `rust_main` for Rust).

  crt0, crt1 and moto-rt-cubit include it: Rust through
  `global_asm!(include_str!(…))`. config-storage, the Turso probe and the
  Rust probe use the Rust std entry or the shared `cubit_start` crate
  module. No program writes `_start`.
- **Delete `art0.asm`** and its Makefile rule. It is unused, and it is one
  of BLD-002's eight yasm files.
- **Build check.** `tools/check_capability_numbers.py`, renamed
  `tools/check_declarations.py`, also refuses:
  - a `_start` symbol defined outside `cubit_start.S`;
  - a raw `authorityTag` comparison outside generated code;
  - `CuBit.Logging.Unchecked` outside `tests/`.
- **C and Rust bindings are generated too.** `--rust-output` already emits
  binding-name constants. It grows typed accessors (`startup::logstore() ->
  LogstoreEndpoint`), and a new `--c-output startup.h` replaces
  `cubit_startup_endpoint ("name")`. Rust and C use the same elaboration
  check, run from the shared start code before the language hook.

## 4. The whole-image check

### What it checks

For one image (the programs it contains, its startup plan and its catalogs):

1. **Every use resolves.** Each required service request, and later each
   `uses`, names a catalog service that some program in the image offers.
   Today that is a program whose manifest registers the role (its
   `registration` notification), or a boot root (devmgr or procmgr). The
   offered contract must be `Compatible` with the used one
   (`CuBit.Protocols.Compatible`).
2. **Every required startup entry is provided by the plan.** The provider of
   each required entry of each started program is itself started by the
   plan (`init.ccl`, devmgr's boot list, a launcher's `may_launch` and the
   desktop's launch entries). Optional entries whose provider is missing
   are reported as information.
3. **Every Config scope exists.** Each requested scope names a setting
   catalog entry. Each setting in the image's `system.ccl` is in the
   catalog with that type, and some program in the image may read it, so a
   setting nobody can read is reported (R6 seen from the other side).
4. **Unused offers** (a service started that nobody uses) are warnings.
5. **Undeclared grants.** A launcher grant with no declaration behind it is
   an error once the launcher's grants are declared. Today that means
   devmgr's `sendWildcardACLConfig (netmgrPID)`.

### Where it runs

- **Now:** `ccl-image-check`, a hosted Ada tool next to `ccl-image`. It
  reads the ELF sections with the proved decoders that already exist
  (`CuBit.Startup_Tables.Decode`, `CuBit.Launch_Authority`,
  `CuBit.Channel_Limits`, resource sections), plus the catalogs,
  `settings.ccl`, the startup profile and `system.ccl`. It runs in two
  places:
  - from `realize.py` for initrd and ISO images, which already holds every
    payload's bytes and already checks that startup executables are present;
  - from the `make world` step that stages the ext2 disk, which the CCL
    image profiles do not cover yet.

  A failed check fails the image.
- **Later:** a step of ccl-build over the System value (SYS-001), where the
  plan is one typed value rather than four files. The check is the same
  code. APP-003's import/export check for applications is the same
  analysis with application offers.

devmgr's boot list is Ada literals today (`spawnFromBootStorage
("netmgr.svc")`). Until it moves into CCL (the CCL-native startup rule), the
check reads a declared `images/boot-services.ccl`, and devmgr is changed
to refuse a spawn that is not in it, so the two cannot drift.

### Report

The report is a typed CCL value, `Declaration_Report`, printed in CCL
notation so the REPL and Observatory can load it. It is not JSON. Each
finding gives:
- the severity;
- the program, as image path and identity;
- the declaration, as kind, name, and manifest file and line;
- the problem, as an enum;
- the remedy, in the explained-failure style of ccl-manifest.

```
error  desktop.svc (com.cubit.desktop) manifest.ccl:22
       config scope "desktop.display.scale": no setting in settings.ccl
       Fix: add (Setting Display_Scale …) to settings.ccl, or name an existing setting.
error  config-storage.svc (com.cubit.config-storage) manifest.ccl:5
       offers config-worker v1 roles (Config); config.svc uses role Owner
       Fix: use a role the contract declares (Config).
warning clock-control: offered by clock.svc, used by no program in this image
```

## 5. Runtime backstop

### Startup-health headless test

A new headless test, `startup-health`, is added to the pre-landing set. No
such set exists in the repository yet; slice 1 creates
`tests/pre-landing.md` with the commands. The test boots the full desktop
image with `--accel kvm` and fails on any of:
- a capability refusal (`Capability_ABI.Refusal`) or the kernel's
  "slot is not CAP_ENDPOINT" class of message;
- a generated startup check failing (missing entry, launch block not
  delivered);
- a launch refused for a required startup entry;
- a service in the plan that never registers within a deadline;
- dropped log records: each started program's publisher reports
  `Dropped = 0`, and logstore's per-channel drop count is 0 at the end;
- an expected "started" record missing from logstore for any program that
  requests logstore. This is what R1 and R2 looked like from outside.
- any of the once-only refusal Warnings below.

The expected set (programs, roles) comes from the image's
`Declaration_Report`, not a hand-kept list.

### Each refusal logged once, by name

Today refusals say `FS: access denied for PID 41: @nvme:0/lib` or nothing
at all (Config's `AccessDenied` is silent, and an unbound `Emit` only
counts). Each refusing party logs one Warning per (program, declaration),
naming the program identity (from procmgr's process table, not a PID) and
the declaration involved:
- `config: com.cubit.desktop read gpu.power.waitboost denied: no config scope covers it`
- `logstore: com.cubit.files emitted 3 records on an unbound publisher`
  (until slice 1 makes that unrepresentable)
- `config-storage: request refused: badge is not a config-worker role (untagged)`

Repeats are counted, not logged.

## 6. What would have caught each regression

| # | Compile time (1–3) | Image check (4) | Startup health / refusal log (5) |
|---|---|---|---|
| R1 Desktop publisher | **Yes.** An unbound `Publisher` does not compile | — | "started" record missing; unbound-emit Warning |
| R2 Files publisher | **Yes**, same | — | same |
| R3 config-storage `_start` | **Yes.** No hand-written `_start` is allowed (build check) | — | elaboration check: "launch block not delivered" |
| R4 PID as tag | **Yes.** `Classify` has no PID case; broker mints the declared role | contract and role match between Config's use and config-storage's offer | badge-refusal Warning; Config persistence never ready |
| R5 gcc `Launching.Bind` | **Yes.** No `Launcher` without `Startup.Process_Manager` | — | launch refusal in the test |
| R6 Desktop Config scope | **Yes.** No `Startup.Settings.Display_Scale` without the scope | the setting in `system.ccl` is readable by no program | config denial Warning |

Slice 1 alone catches R1, R2 and R5 at compile time, and R3, R4 and R6 at
first boot (elaboration check, startup-health, refusal Warnings).

## 7. Live bugs with the same shape (found 2026-10-22)

These are on micro now and are fixed in slice 1:

1. **binutils test never binds the launcher** (R5's twin).
   `tests/binutils/check/main.adb` calls `CuBit.Launching.Describe` and
   `Launch` without `CuBit.Launching.Bind`. Every launch returns
   `No_Process_Manager`. The manifest does declare process-manager.
2. **Desktop reads `gpu.power.waitboost` with no Config scope for it**
   (R6's twin). `Desktop_GPU_Power_Settings.Config_Key` is outside every
   `config-scope` in `userspace/services/desktop/manifest.ccl`.
   - The read is denied, and Desktop folds `AccessDenied` into the default.
   - All 195 boot logs under `/home/doc/cubit-build-tmp` say
     `desktop: gpu waitboost on (default)`, never `(configured)`, although
     `system.ccl` sets it.
   - It is masked only because the default and the setting are both `true`.
     Setting it to false would be silently ignored.
3. **config-storage R3 and R4 are still on micro.** The fixes (`entry.rs`
   and `Worker_Request_Tag`) are uncommitted in `/home/doc/git/cubit-cfg002`.
   Land that, then fold it into section 3's shared entry and section 2's
   generated badge.
4. **netmgr has no Config scope at all** but reads `net.if0.*` and
   `net.dns.*`. It works only because devmgr sends it a wildcard,
   read-write Config ACL (`sendWildcardACLConfig (netmgrPID)`,
   devmgr/main.adb:3219). That grant is undeclared and wider than needed.
   - Fix: declare `(config [File_Right.Read] "net.")` and delete the
     wildcard call.
5. **Dormant fixture with an unbound Reader.**
   `tests/compositor/startup-denial/native_log_observer.adb` subscribes
   without `Bind`. It would fail "observer subscription" if rebuilt.
   - It also finds Desktop by polling `Registered_Driver`.
   - Fix it, or delete it with the recorded evidence that uses it.
6. **Undeclared runtime logstore grants.** intel-gpu and xhci get
   logstore's endpoint from devmgr at run time, by a hand-written exchange,
   with no manifest declaration. This is not broken, but it is invisible to
   any check.
   - Fix: declare logstore as an optional startup entry for drivers, minted
     by devmgr, and delete both exchanges.
7. **46 Ada programs never call `Startup.Verify`.** A missing entry becomes
   a null handle that fails later and quietly. The generated elaboration
   check (slice 1) fixes all of them at once.

Checked and clean:
- every other `Logging.Publisher` (clock, lexicon, display, desktop, files,
  xhci, intel-gpu, log-fields-check, log-check, `CuBit.Log`,
  deferred_warnings, mesa tests) is bound;
- the other Launching users (gcc, gnat, process-spawn, control-events,
  ccl_launcher) bind;
- every other program's Config keys fall inside its declared scopes: clock,
  timesync, tls, lexicon, logstore, calculator, Penny, config-storage, and
  the shell and inspectors with `all`.

## 8. Slices

| Slice | Content | Size | Catches |
|---|---|---|---|
| **1** | Typed service endpoints (catalog column); `Publisher (<>)`/`Reader (<>)` with `Bound_To`; `Launcher`; generated elaboration-time startup check; `startup-health` test plus `tests/pre-landing.md`; one Warning per refusal in config, logstore and procmgr; fix live bugs 1, 2, 4 and 5; land cfg002's config-storage fix | S–M (about 45 call sites, 2 runtime packages, ccl-manifest generator, 1 headless test) | R1, R2 and R5 at compile time; R3, R4 and R6 at first boot |
| 2 | Shared `cubit_start.S`; delete `art0.asm` and the three Rust copies; generated Rust and C typed startup accessors; `tools/check_declarations.py` | S | R3 at build time |
| 3 | `settings.ccl` setting catalog; manifests name settings; `Startup.Settings` accessors; `Inspector` for `config-all`; remove `CuBit.Config`'s string API and string `Config_Reader.Query`; `Typed_Settings.Kind_Of` generated | L (76 call sites in about 18 files; types for 12 untyped settings) | R6 at compile time; **CFG-003 builds on this** |
| 4 | `ccl-image-check` with `Declaration_Report`; catalog `offered-by`; `images/boot-services.ccl` and devmgr enforcing it; wired into `realize.py` and the disk staging; startup-health takes its expectations from the report | M | composition errors before boot (missing provider, orphan setting, undeclared grant) |
| 5 | Badge roles in contracts; generated `Classify` and `Mint`; procmgr and devmgr mint from `uses`; migrate the 11 services in the badge table; refuse raw tag compares | M–L | R4 at compile time |
| 6 | Contract identities on offers and uses (`.cubit.contracts`), `Compatible` in the image check; generated codecs from descriptors; merges into CAP-002 steps 4–5 (`Connect`, catalog) | L | protocol drift between client and server |

Slice 1 keeps CAP-002's direction. Typed endpoints are what `Connect` will
return, and the elaboration check is `Verify` made mandatory.

## Decisions on the open questions

The user accepted every recommendation below on 2026-10-22.

1. **Elaboration-time startup check: exit, or keep running degraded?**
   Recommendation: exit with a Critical record for a missing *required*
   entry. procmgr already refuses such a launch, so reaching this point
   means the table was lost (R3), and running on is what made R3 silent.
   Optional entries stay `Optional_*`.
2. **Where the typed-endpoint mapping lives.** Recommendation: a typed column
   in the service catalog (`(service logstore 13 read-write (client
   Logstore))`), naming an enum of client types rather than an Ada package
   string, so the catalog stays a typed CCL value.
3. **Badge words: derived or declared?** Recommendation: derived from the
   contract digest and role position, never written down. A log line prints
   the role name, never the number.
4. **Should the image check fail on unused offers?** Recommendation: a
   warning. Minimal profiles legitimately start services nobody uses yet,
   for example clock-control.
5. **Who owns `tests/pre-landing.md`?** There is no pre-landing set in the
   tree yet, although the backlog and memory refer to one. Recommendation:
   slice 1 creates it with startup-health first. Other agents add their
   regression tests to it.
6. **The netmgr wildcard.** Recommendation: replace it with a declared
   `net.` read scope in slice 1. Is there a reason netmgr needs write?

## Implementation notes (slice 1, 2026-10-22)

**Typed endpoints.**
- The service catalog's `(client X)` column (`native-runtime-services.ccl`:
  logstore, log-observer and process-manager) is read by `ccl-manifest`.
  `X` is a `CCL.Manifests.Service_Client`; an unknown one is
  `Unknown_Client`.
- The generated `Startup` function for such a binding returns
  `CuBit.Handles.Services.X.Endpoint`, or `Optional_Endpoint` when the entry
  is optional. Each is an instance of the generic
  `CuBit.Handles.Typed_Endpoints`, whose full type is private. The ELF
  sections do not change.

**Objects that exist only bound.**
- `CuBit.Logging.Publisher (<>)` and `Reader (<>)` are made with
  `Bound_To`, which takes the typed endpoint or its optional form; `Bind`
  is gone.
- A program that is handed logstore at run time asks for that explicitly:
  `Awaiting_Grant`, then `Attach`. That covers the xhci and intel-gpu
  grants from devmgr and the deferred warnings.
- `Services.Logstore.Granted` is the one conversion from a plain endpoint.
  It is used for those grants and for log-check's deliberate stand-in.
- `CuBit.Launching.Launcher (<>)` is made with `Using`. `Launch`,
  `Lend_Ring`, `Launch_Table`, `Delegated_Places` and `Describe` take it;
  `Bind` and the package state are gone.
- The gcc, gnat, binutils and process-spawn launchers now generate their
  `Startup` package instead of looking `"process-manager"` up by string.

**The startup check.**
- `ccl-manifest --startup-output startup.ads` also writes `startup.adb`.
- The spec is `Elaborate_Body`. `Verify` is gone; `Expected` is the declared
  table.
- The body calls `CuBit.Startup_Check.Require`. On failure it says which
  entry and why, or that the launch block carried no table (R3's
  signature). It sends the same as a Critical record when logstore is
  reachable, then exits with `Startup_Refused` (EX_CONFIG).
- The 49 hand-written `Startup.Verify` checks are removed. Five programs
  that used nothing else from `Startup` keep `with Startup` (with
  `pragma Unreferenced`) for its elaboration.

**Refusals once.**
- `CuBit.Refusal_Notes.Warn_Once` logs
  `<service>: Warning: refused … (CAP-003)` once per distinct refusal and
  counts repeats.
- It is called from three places:
  - config.svc, for a key outside the caller's scopes or a system key;
  - logstore, for a channel or operation without its badge;
  - procmgr, for a required startup entry it cannot grant.
- The PID names the process: config.svc and logstore have no
  PID-to-program table yet, so the doc's "program identity" is still open
  there. procmgr names the program.

**Live bugs fixed.**
1. The binutils launcher now launches through `Startup.Process_Manager`.
2. Desktop declares `gpu.power.waitboost`.
3. Not fixed here: config-storage waits for cfg002's merge.
4. netmgr declares `(config [File_Right.Read] "net.")`. devmgr reads each
   child's `.cubit.access` (`Driver_Manifests`, proved) and installs exactly
   those scopes (`sendDeclaredConfigScopes`). The wildcard for netmgr is
   gone.
5. The startup-denial observer is bound.

**Tests.**
- `tests/headless/run.sh --test startup-health`, with
  `init-startup-health.ccl` and `check-refusals.py`.
- `tests/pre-landing.md`.
- Hosted: the ccl-manifests suite (typed clients, the catalog column,
  `.ads` only, the body), startup-tables (with a stub body) and
  driver-manifests (netmgr's scope).
