# Program parameters (design, 2026-09-27)

Status: design. Nothing here is implemented yet.

A program declares its parameters in its manifest, the way a function
declares its signature. A launcher supplies arguments as CCL values. The
arguments are type-checked, and defaults filled in, before the child runs.
The child receives one canonical, read-only value. Because the signature
sits in the manifest, the REPL can complete a program's parameter names
and show their types and defaults, just as it does for service operations.

First user: netstack limits (docs/netstack-redesign.md, "netstack limits as
typed startup parameters").

## What exists today

- `define` takes positional, typed parameters only, at most 8. There are no
  named arguments or defaults, no integer range types, and no lists.
- `(start ...)` in startup profiles has `priority`, `network` and `role`,
  but no arguments.
- Spawn carries no arguments. procmgr's `OP_SPAWN` takes a filename, cwd,
  priority and sandbox flags. The kernel's spawn takes an ELF and a name.
  The runtime sets `Command_Line_Args` False, and the libc fakes `argv`.
- Completion covers qualified service operation names only (from the
  interface catalog). It does not cover programs, `define`d functions or
  argument names.

## Language additions (general, not launch-specific)

These are useful to ordinary CCL functions too. Launch parameters reuse
them, so a program signature is written exactly like a function signature.

1. **Defaults.** A parameter or record field may end with a default:
   `(port Port 443)`. The default must be a constant expression of the
   parameter's type, checked when the definition compiles. Parameters with
   defaults come after those without.
2. **Named arguments.** A call gives positional arguments first, then
   `:name value` pairs, in any order:
   `(connect "example.org" :timeout-ms 2000)`. It is an error to name an
   unknown parameter, to give one twice (positionally and by name), or to
   leave out one that has no default. Record constructors accept the same
   form, so `(Netstack_Limits :connections 4096)` fills the other fields
   from their defaults.
   - Landed for record constructors on 2026-10-02 with Ada's spelling
     instead: `(Netstack_Limits connections => 4096)`. `=>` follows a field
     name and is valid only in argument position. See docs/ccl-typed-manifests.md.
3. **Range types.** `(type Port (range 1 65535))` declares a bounded
   integer type. Arguments are checked against the bounds at compile time
   when they are constant, and at the call otherwise. This is the CCL form
   of the tight subtypes used in the Ada code.

Evaluation stays total and bounded: defaults are constants, so filling
them cannot run code.

## Manifest declaration

```lisp
(executable-manifest v1 (identity "netstack") (version "1")
  (type Connection_Count (range 16 65536))
  (parameters
    (connections Connection_Count 1024)
    (channels (range 16 16384) 256)
    (time-wait (range 0 65536) 1024)))
```

- `parameters` uses the `define` parameter syntax. Each entry is a name, a
  type, and an optional default. Types may be built-ins, range types, or
  record/enum/variant types declared in the same manifest.
- The manifest tool emits the signature as a canonical descriptor in a
  non-loadable `.cubit.parameters` section. This follows the form of
  `.cubit.interfaces`, with SHA-256 identity.
- It also generates bindings, as it does for capabilities (for example
  `CCL_Manifest_Bindings`):
  - Ada: a record type plus a proved, total `Decode`, so the program reads
    `Parameters.Connections`.
  - C: a struct plus a decode function.
- A program with no `parameters` form accepts no arguments. Supplying any
  is an error, not silently ignored.

## Supplying arguments

- **Startup profiles:**
  `(start "netstack.svc" (arguments :connections 4096 :channels 1024))`.
- **REPL:** a program is called like a function, for example
  `(run netsurf.app :homepage "http://10.0.2.2/")`. The exact head form
  needs agreement with the shell/REPL owner.
- **devmgr and other programmatic launchers:** they build the same
  canonical value through the runtime encoder.

## Checking and delivery

1. The launcher compiles the argument expression against the child's
   declared signature, read from its `.cubit.parameters` section. It
   reports errors with the same diagnostics as a function call.
2. Defaults are filled in, so the value is complete. It is encoded
   canonically; the encoding is one record, with fields in declaration
   order, in the CBOR subset of docs/ccl-cbor-evaluation.md.
3. procmgr spawns with the encoded bytes (a grant, bounded size), and
   re-checks them against the signature. procmgr is the authority; the
   launcher's check exists to give good diagnostics.
4. The kernel maps the bytes read-only into the child at a fixed,
   documented address (an auxiliary page next to the stack) and passes its
   length in the entry registers.
5. The child's generated `Decode` validates the bytes again. It is proved
   total, so malformed input yields an error, never undefined behaviour.
6. libc: `crt1` builds `argv` from the value for ported programs
   (`argv[0]` is the name, then `--name=value` per field), so ported C
   code keeps working. Native programs use the typed record.

## Completion

- The package catalog gains each installed program's signature, taken from
  its `.cubit.parameters` section.
- After `(run ` the REPL completes program names. After a program name it
  completes `:parameter` names not yet given, and the signature popup shows
  each parameter's type, bounds and default.
- The same applies to `define`d functions and record constructors, once
  named arguments exist.

## Order of work

1. Language: range types, defaults and named arguments in `define` and in
   record constructors. This includes the evaluator, type checker,
   diagnostics and tests. It is a shared CCL change, so it needs agreement
   with the other agent.
2. Manifest `parameters` form, `.cubit.parameters` descriptor, and the
   generated Ada/C bindings with a proved decoder.
3. Spawn path: procmgr `OP_SPAWN` argument grant, kernel mapping, runtime
   and crt1 entry.
4. Launchers: the `start (arguments ...)` form, devmgr's netstack spawn,
   and the REPL `run` form with completion.
5. netstack sizes its tables from its parameters.

## Legacy tools: typed parameters over argv (design, 2026-10-04)

User decisions (docs/self-hosting.md, items 4 and 5): no raw argv or envp. A
ported Unix tool is wrapped by its CCL manifest, which declares typed
parameters and how they map onto the argv the tool expects. A file name is
its own type. In the CCL console every outlet a launched program
declares comes back as a card to inspect.

**Authority comes from places, not argv.** Arguments stay data
(docs/process-arguments.md). A file parameter's value turns into a
delegated place (`CuBit.Launch_Grants`), which procmgr checks against what
the launcher holds and installs for the child, exactly as for any
delegation. A launcher that lies in argv gains nothing: the child can touch
only its own manifest scopes and the places it was given. So procmgr checks
delegation, not argv, and the typing lives in the launcher, where it gives
correct, discoverable calls.

**Manifest forms** (in interfaces/executable-manifest.ccl, typed like every
other field; userspace/ports/binutils/ld.ccl is the first real one):

```lisp
parameters => [
  (Parameter "output" Parameter_Kind.Output_File)
  (Parameter "inputs" Parameter_Kind.Input_File many => true)
  (Parameter "script" Parameter_Kind.Input_File required => false)
  (Parameter "static" Parameter_Kind.Flag)]
arguments => [
  (Argument_Piece.When_Set (Conditional_Literal "static" "-static"))
  (Argument_Piece.Literal "-o") (Argument_Piece.Value "output")
  (Argument_Piece.When_Set (Conditional_Literal "script" "-T"))
  (Argument_Piece.Value "script")
  (Argument_Piece.Value "inputs")]
```

The pieces use the variant's constructors directly: short helpers
(`literal`, `value`, `when-set`) would take the schema past the CCL
program's 16-function limit. They can come back when that limit is raised
(interpreter and VM together).

- Kinds: `Input_File` (read access to exactly that file), `Output_File`
  (create and write; its directory must be one the launcher holds),
  `Input_Directory` and `Output_Directory` (the place itself), `Flag` (a
  Boolean, rendering nothing itself), and `Text` (an ordinary string: no
  authority, for symbol names, options and the like).
- `many` takes a list. A parameter that is not required may be absent; then
  its pieces render nothing.
- Argument pieces: `Literal`, `Value` (a parameter's value; a list renders
  each element in turn), and `When_Set` (a literal rendered only when a flag
  is true or an optional parameter is present). A flag is always optional
  and has no `Value`.
- The manifest tool also checks that names are 1 to 32 of `[a-z0-9_-]` and
  unique, that every piece names a declared parameter, that literals are 1
  to 48 printable characters, and that every parameter is rendered by some
  piece, so no file is delegated without being passed. Limits: 16
  parameters and 24 pieces, so a descriptor fits a 2 KiB manifest section.
- The manifest tool emits both as a `.cubit.parameters` section: a versioned
  descriptor that the proved Ada unit `CuBit.Program_Parameters` validates
  and renders. Rendering a value yields the launch block (argv[0] then the
  pieces in order) and the delegated places (one per file parameter value).

**The CCL side.**
- procmgr answers `OP_PROGRAM_PARAMETERS` with the descriptor of a program
  the requester may launch (its may_launch names it), so a launcher needs no
  read access to program files.
- The console turns the descriptor into a record type for the program
  (`ld.Parameters`), with `Input_File` and `Output_File` as distinct
  nominal types made by `(input-file "@nvme:0/work/a.o")` and `(output-file ...)`.
  So the ordinary type checker checks a call, and completion and
  signatures work as for any function.
- `(ld.run (ld.Parameters output => ... inputs => [...] static => true))`
  renders the value, starts the program through `CuBit.Launching`, and
  returns a process: an identifier for that incarnation, not a holder of
  streams. Its inlets and outlets are reached through the program's accessors (below),
  and its exit status comes from its own operation once it ends.
- As a CCL feature, it lands in the interpreter and in the bytecode
  compiler, verifier and VM together (docs: bytecode parity).

## Inlets and outlets, not stdio (user decisions, 2026-10-04)

CuBit has no stdin, stdout or stderr. A program declares any number of
**outlets** (values it emits) and **inlets** (values it takes) in its
manifest, each with a name, an element type and a signal kind. (They were
called ports at first; the user chose the dataflow terms, which say the
direction and match the stream node graph. "Connector" is the internal word
for either, in the Ada units.) Every inlet and outlet is identifiable: the
process incarnation plus its name, with its type known when code is
checked.

**Names are fully qualified, like application identities (user,
2026-10-04):** `unix.stdout`, `unix.stderr`, `com.cubit.stdlog`,
`com.cubit.audit`. The qualifier says whose convention it follows. `unix.*`
names are legacy glue for ported programs (what their descriptors 0, 1 and 2
meant), not a CuBit concept. A `com.cubit.*` name is a CuBit contract with a
defined element type (`com.cubit.stdlog` is an outlet of `Log_Records`), so
a manifest that declares it otherwise is rejected. Names are at least two
dot-separated components of `[a-z0-9-]`, at most 48 bytes.

```lisp
outlets => [
  (Outlet "unix.stdout" Element.Text pages => 4)
  (Outlet "unix.stderr" Element.Text pages => 4)
  (Outlet "org.gnu.ld.progress" Element.Integers signal => Signal.Level)]
inlets => [(Inlet "unix.stdin" Element.Text)]
descriptors => [(Descriptor 1 "unix.stdout") (Descriptor 2 "unix.stderr")]  # porting glue only
```

- **Signal kinds (user, 2026-10-04; the details are a proposal for
  review).** Besides its element type, each declares how values arrive:
  - `Stream`: a sequence of elements, each delivered in order (with any gaps
    counted), such as a transcript.
  - `One_Shot`: exactly one value, ever, then done (a future). Examples: a
    tool's result, or the process's exit status.
  - `Level`: a current state. Readers see the latest value, and
    intermediate values may be coalesced (e.g. progress, "ready", a volume).
    A card shows the value, not a history.
  - `Edge`: discrete transitions, each one an event that is never
    coalesced (e.g. "file written", "phase changed").

  The signal kind is part of the type (a `Level` outlet cannot be wired to
  something that wants every element), maps onto the existing delivery
  policies (`Stream_Policies`: lossless, ordered with gaps, latest value),
  and decides what a card and a graph node draw. Every process has a
  system-supplied one-shot outlet, `com.cubit.exit`, carrying its exit
  status, so the exit status is just another outlet.
- **They replace** `Output_Stream`, `Stream_Kind` (`Standard_Output`,
  `Standard_Error`, `Log`), the `.cubit.streams` section, procmgr's stream
  bitmask, and the fixed ring IDs 2 and 3 with their four-ring limit. The
  producer-owned rings of `CuBit.Streams` stay as the transport: a ring's ID
  is its outlet's position in the description plus one (outlets come first,
  then inlets).
- **Descriptors are porting glue.** A ported Unix program reads and writes
  file descriptors; its manifest maps each one it uses onto an inlet (0) or
  an outlet (others), and only its libc shim reads that map. An unmapped
  descriptor is refused (EBADF). The system never sees descriptor numbers.
- **One program description.** What procmgr returns for a program covers
  its parameters, its inlets and its outlets, so a launcher knows them all
  before the program starts.
- **Per-program accessors (user's choice).** The console publishes each
  program it may launch as a CCL interface generated from its description:
  `Ld_Parameters`, `ld.run`, and one accessor per outlet named by its
  qualified name, so `(ld.unix.stderr r)` is a `Stream<String>`. Anything the
  program does not declare is a type error, and completion lists the real
  ones. They are host operations, so the interpreter, compiler, verifier and
  VM check and run them like any other. `(outlets r)` lists a run's outlets
  (name, element, signal, arrived, lost) for discovery and the node graph.
- **Every outlet gets a card (user's choice).** When a program starts from
  the console, each declared outlet appears as its own card and as a node in
  the stream graph, live and kept for inspection. Wiring an outlet elsewhere
  moves the edge; the card shows where it goes.

**Launcher-owned outlet rings (design, 2026-10-04).** A launcher that
reads a child's outlets (the console) owns their rings. Before
launching, it allocates one ring per outlet in its own memory, with the
outlet's declared pages and element type. It writes the ring header with
itself as the one subscriber, and lends each ring to procmgr as a
forwardable grant in the OP_LAUNCH request. procmgr derives a grant of each
for the new child and lists them, with their owner's PID (procmgr's: derived
grants live in its grant namespace), in the child's
launch block, between the strings and the description. The child's libc (or
`CuBit.Streams.Open_Outlet`) maps its lent ring and produces into it. The
launcher reads its own memory: no subscription message, no endpoint to the
child, and nothing lost to a race or to the child's exit, since the ring
outlives the child. An outlet with no lent ring (an init-started program, a C
parent) keeps the producer-owned ring created on first write. This is the
"console-owned buffers" that docs/development-backlog.md CCL-001
describes, which redirection will re-point.

**Status, 2026-10-04.**
- Done (step 1): the schema forms; the `.cubit.parameters` encoder
  (ccl-manifests-typed.adb and encoding.adb, re-checked with `Decode` before
  it is emitted; tests/ccl-manifests); and `CuBit.Program_Parameters`
  (`Decode`, `Find`, `Add`, `Render`), proved at level 2 with no unproved
  checks, plus a hosted test (tests/program-parameters, which includes
  every one-byte corruption of a descriptor).
- Done (step 2): `OP_PROGRAM_PARAMETERS` (label 16#0109#) in procmgr.
  `CuBit.Launching.Describe` is its client, and `CuBit.Launching.Wait`
  returns a child's exit report.
- as.ccl and ld.ccl carry real parameters and no file scopes. The binutils
  guest test (tests/binutils/check) is an Ada launcher that describes,
  renders, launches and waits. This is a live CuBit run, though not yet
  from CCL. Guest test `binutils`: all nine checks pass, including a
  refusal for a missing parameter, a Not_Granted refusal for a file outside
  the launcher's places, and failure of raw argv without places.
- Found on the way: streams cannot be record fields in CCL (Stream_Not_Data),
  so a process cannot hold its inlets and outlets; the per-program accessors reach them.
  A host operation takes one data argument, so a call's values travel as
  one record.
- ccl-manifest runs its compilation on a 256 MiB task: interpreter frames
  are about 240 KiB, and typed manifests were close to overflowing a
  default 8 MiB stack.

**Inlets and outlets, status 2026-10-04.**
- `CuBit.Program_Descriptions` (it replaces `CuBit.Program_Parameters`) is
  one description per program: parameters, pieces, inlets and outlets (direction,
  element, mode, ring pages, qualified name) and the descriptor map. Its
  manifest section is `.cubit.description` (magic `PDSC`), and procmgr's
  request is `OP_PROGRAM_DESCRIPTION` (16#0109#). Proved at level 2 (288
  checks); the hosted test covers names, duplicates, descriptor
  directions and every one-byte corruption.
- The manifest schema has `Port`, `Port_Direction`, `Port_Element`,
  `Port_Mode` and `Descriptor`. `Output_Stream`, `Stream_Kind`,
  `.cubit.streams`, the keyword `(stream ...)` form, procmgr's stream
  parsing and the dead `REQ_STREAM` caps entry are removed. The manifest
  tool enforces the `unix.*` and `com.cubit.*` rules, and `com.cubit.exit`
  is reserved for the system.
- The launch block is format 3. procmgr attaches the program's validated
  description after the strings (`Attach_Description`, re-validated), and
  builds a name-only block when the launcher gave none, so every program
  learns its inlets and outlets at start. A malformed description refuses the launch.
  Proved at level 2 (318 checks); the 400,000 random blocks include ones
  with descriptions.
- The libc maps a descriptor to an outlet only through the manifest's
  `descriptors`: the ring is the outlet's position plus one, with the
  outlet's pages and element type. An unmapped descriptor, 1 and 2
  included, is EBADF. Native Ada programs call `CuBit.Streams.Open_Outlet`
  with the outlet's qualified name. sleep and wget declare
  `com.cubit.sleep.status` and `com.cubit.wget.progress`.
- Input inlets and outlets (descriptor 0) are declared and mapped but not delivered yet.

**The CCL side, status 2026-10-04 (hosted results; the guest run follows).**
- `CCL.Interfaces.Programs` generates a program's interface from its
  description: `Ld_Parameters` plus shared `Input_File`, `Output_File`,
  `Input_Directory`, `Output_Directory` and `Run` types; `ld.run`; one
  accessor per outlet by qualified name; and `ld.com.cubit.exit`.
  Keys are SHA-256 through SPARKTLSCrypto (the whole SPARKTLS project is a
  dependency of the CCL front ends: `sparktls_cubit.gpr` natively,
  `sparktls_host.gpr` hosted). tests/ccl-programs: typed calls check and
  compile and verify for the VM, file kinds are distinct, undeclared inlets and outlets
  and parameters are type errors (19 checks).
- The catalog accepts dotted operation names and refuses a qualified name
  two interfaces would share (`Ambiguous_Name`; tests/ccl-type-discovery).
- The session's stream table has outlet streams: text lines (64 kept, 200
  characters each) or Integers, fed by the host, ended when the program
  ends, pinned until the run's slot is reused (tests/ccl-streams).
- `CCL_Program_Bindings` over the platform's `CCL_Launcher` (native: the
  console's launch table through procmgr's new OP_LAUNCH_TABLE, each
  program's description, launcher-owned rings and `CuBit.Launching`; Linux
  preview: no programs). It sits in the shared host environment, so the
  console and the Workbench both get it.
- Launcher-owned rings are built: `CuBit.Outlet_Rings` (proved at level 2),
  `CuBit.Launching.Lend_Ring`, procmgr's derivation for the child, the
  trailer (ring table, then description) in the launch block, and adoption
  in the libc and `CuBit.Streams.Open_Outlet`.
- Done since (guest run, 2026-10-04): every outlet of a program an entry
  starts gets its own live card automatically, re-run when elements arrive
  (no `:watch`), named by the run when the entry defined one, as in
  `(window 64 (as.unix.stderr bad))`. `(ld.outlets r)` lists each outlet's
  signal, arrived and lost counts and whether it ended. Released runs'
  rings are revoked and their pages reused once the grant's retirement is
  confirmed. "Ports" were renamed inlets and outlets throughout (user).
- A type mismatch names what wanted which type and what it was given, for
  every record (`field output takes Output_File, not Input_File`, pointing
  at the value) and every host operation (`ld.run takes Ld_Parameters, not
  Input_File`): new diagnostics `Field_Type_Mismatch` and
  `Argument_Type_Mismatch`, with the expected and found type names carried
  to the console. `(ld.outlets r)` also shows each outlet's stream type
  (`Stream<String>`).
- Still to do: input to inlets; console-owned rings that can be re-pointed
  (redirection, CCL-001).

**Order of work.**
1. Schema forms, the `.cubit.parameters` descriptor, and
   `CuBit.Program_Parameters` (validate, render), proved at level 2 with
   hosted tests.
2. `OP_PROGRAM_PARAMETERS` in procmgr.
3. Inlets and outlets in manifests and in the program description (replacing
   `.cubit.streams`), and the libc's descriptor map.
4. Per-program CCL interfaces generated from descriptions (`ld.Parameters`,
   `ld.run`, outlet accessors, `outlets`), Text streams in the session's stream
   table, a card per outlet, and the exit status.
5. binutils manifests with real parameters, replacing the work place; a
   guest test drives `as` and `ld` from CCL.
