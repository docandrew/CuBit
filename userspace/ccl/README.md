# CuBit Control Language

The CuBit Control Language (CCL) is CuBit's strongly typed,
capability-aware language for interactive control, automation, and bounded
event-driven services.

CCL is not a Unix shell and does not provide a TTY, process environment, text
pipeline, or ambient access to system resources. Programs can affect the system
only through typed authority explicitly supplied to them.

The current design proposal is in
[`docs/control-language.md`](../../docs/control-language.md).

## Names

The initial naming conventions are:

| Name | Meaning |
|---|---|
| CuBit Control Language | Full language name |
| CCL | Language and subsystem abbreviation |
| CCL VM | Bytecode verifier and interpreter |
| CCL Workbench | Shared graphical editor, compiler, and debugger for CuBit and Linux |
| `cclc` | Workstation source compiler |
| `ccl-run` | Hosted runner using deterministic or emulated CuBit imports |
| `ccl-debug` | Planned AST and bytecode step debugger |
| `ccl-disasm` | Bytecode inspection tool |
| `ccl-vm.app` | Freestanding CCL VM process running on CuBit |
| `.ccl` | CCL source module |
| `.cclb` | CCL bytecode module |

The first `ccl-run` Workbench executable is implemented. It evaluates a source
expression supplied as command-line arguments, or starts an interactive REPL
when invoked without arguments. Both paths use the same checked, fuel-bounded
`CCL.Language` interpreter as the native and CuBit tests.

The BASIC-like interactive syntax and Lisp-like structured syntax are both CCL.
They elaborate into the same typed core and do not identify separate languages.
`CuBASIC` is the existing desktop prototype and historical predecessor of the
CCL console.

The target ownership vocabulary is unrestricted, move-only (affine),
must-handle (linear), borrowed-ro, borrowed-rw, and explicitly shared.
Must-handle types declare their valid disposition verbs, such as “sent,
cancelled, or returned” and “committed, rolled back, or returned.” This system
is designed but not yet implemented: today's checker handles scalar types and
typed host-import metadata. Bounded heterogeneous tuples, closed variants, and
ownership-aware bytecode verification precede the general source compiler so
unrestricted copying does not become part of the module ABI accidentally.

## Trust boundary

The source compiler is not part of the runtime security boundary. Every module,
including one produced by `cclc`, must be validated by the CCL VM before it is
instantiated.

```text
source
  |
  | potentially fallible compiler
  v
unresolved bytecode + descriptor-pinned linkage
  |
  | trusted host intersects linkage with granted bindings
  v
linked bytecode module
  |
  | trusted verifier
  v
validated module
  |
  | bounded interpreter and typed host imports
  v
observable effects
```

Interface discovery and invocation authority are separate. Analysis receives an
explicit bounded catalog view; the default view is empty. Catalog operations
contain immutable type/effect metadata and no runtime binding. Compilation
records their descriptor digest, version, and operation ordinal while emitting
an unresolved import. A trusted host links that import only when an exact
operation contract has an already-authorized local binding. Failed admission
does not partially link a program.

The verifier, interpreter, bytecode decoder, bounded value representation, and
host-import boundary are security-critical. Parsers, source compilers,
formatters, disassemblers, and presentation clients are not trusted to establish
runtime safety.

`CCL.Secondary_Stacks` now provides the first variable-sized value foundation.
It is a bounded, allocation-free region with Ada-like value-carried string
bounds, length-checked sliding copies, scoped marks, generation-checked
descriptors, selective scrubbing of sensitive temporaries, and full scrubbing
at execution teardown. It does not use the GNAT secondary stack or expose raw
pointers. CCLB v3 remains scalar; string literals, function-result lowering,
and a constant-pool encoding will enter together in a later bytecode version.
GNATprove discharges every flow, run-time-check, functional-contract, and
termination obligation for the representative 64-KiB/256-value SPARK proof
instantiation, with no assumptions or justified checks.

## Planned layout

```text
userspace/ccl/
    README.md
    src/                    freestanding SPARK VM library
        format/             bytecode encoding and decoding
        verifier/           structural, type, effect, and bound checks
        runtime/            bounded interpreter and value representation
        host/               abstract typed host interface
    tests/                  native and CuBit-shared fixtures
        valid/              accepted bytecode modules
        invalid/            malformed or statically invalid modules
        behavior/           deterministic execution cases
    tools/                  workstation tools
        cclc/               source parser, elaborator, and compiler
        ccl-run/            hosted runner and CuBit-import emulator
        ccl-debug/          source and bytecode step debugger
        ccl-disasm/         module inspection and diagnostics
    apps/
        ccl-vm/             CuBit host adapter and module runner
        ccl-workbench/      native desktop adapter and application manifest
```

Directories should be introduced with their first implementation rather than
populated with placeholder packages.

## Implementation rule

The VM library must remain freestanding. The same verifier and interpreter
sources will be built as:

1. a native workstation executable using a deterministic test host; and
2. a CuBit userspace component using IPC, streams, descriptors, and session
   authority.

Platform-specific code belongs behind the host interface. The core VM must not
depend on a filesystem, sockets, threads, a desktop, environment variables, or
the hosted Ada runtime.

## First milestone

The first milestone is not a source-language REPL. It is a specified bytecode
format plus a test module builder, verifier, and bounded interpreter supporting:

* scalar constants;
* immutable records and variants;
* structured branches;
* bounded collections;
* statically typed functions;
* fuel accounting;
* typed failures; and
* one mocked host operation, with the same import exercised through real CuBit
  IPC in the guest.

The same fixtures must execute identically under the native test host and in a
CuBit process under QEMU.

## Current implementation

The first VM slice now lives in `src/ccl-vm.ads` and `src/ccl-vm.adb`. It
contains no allocation, exceptions, I/O, tasking, or platform-specific code.
The current instruction set supports:

* signed 64-bit integer and Boolean constants;
* checked integer addition, multiplication, division, modulo, and equality;
* Boolean negation and stack discard;
* forward conditional and unconditional branches; and
* program termination with an optional typed result; and
* statically declared, typed host imports that suspend and resume the VM.

The verifier rejects empty programs, fallthrough, invalid or backward jumps,
unreachable instructions, stack underflow and overflow, operand type errors,
and inconsistent types or depth at control-flow joins. Backward branches are
deliberately excluded from this first format so every accepted control-flow
graph is acyclic.

The interpreter charges one unit of fuel per instruction and reports typed
outcomes for completion, fuel exhaustion, arithmetic overflow, and defensive
detection of invalid bytecode. It performs defensive checks even after
verification; the private validated-program type prevents normal callers from
bypassing verification.

The first canonical source interpreter is also implemented in
`src/ccl-language.ads` and `src/ccl-language.adb`. It currently supports:

```lisp
(+ 20 22)
(= (+ 20 22) 42)
(not false)
(if false 10 20)
(let ((answer (+ 20 22))) (= answer 42))
(concat "elapsed: " "01:01:01")
(at "clock" 2)
(to-string (mod 3661 60))
```

The reader builds into a fixed 128-node AST with bounded names, lexical
bindings, decoded string-literal storage, and nesting depth. A separate static
pass rejects unbound names, incorrect operands, non-Boolean conditions, and
conditional branches with different result types before deterministic
evaluation begins. Evaluation is fuel-bounded and checks arithmetic overflow,
division by zero, actual string bounds, and secondary-region exhaustion.
Strings and Characters currently execute only in direct interpretation; their
compiler forms fail explicitly as unsupported because scalar CCLB v3 has no
constant pool. Arithmetic lowers through the normal compiler and VM.

The canonical syntax is intentionally tiny. BASIC-like interactive syntax will
later desugar to the same checked AST rather than introducing another evaluator.
The current reader treats `#` through the end of a line as a comment, including
after an expression. This marker remains provisional until the source syntax is
declared stable.

Unknown list operators are resolved only through a caller-supplied
`CCL.Catalog.Interface_Catalog`. This keeps service names out of the parser,
type checker, compiler, and VM. The Linux Workbench currently supplies the
canonical `clock` descriptor as its first host interface, then separately maps
that pinned operation to SDL's monotonic clock through a
`Granted_Bindings` value. Merely receiving the catalog permits type checking and
completion; it does not permit execution.

`CCL.Interfaces.Clock` is the first shared, generated-style descriptor binding.
The Linux Workbench and native CuBit VM host publish the same immutable Clock
contract. In CuBit, the test now analyzes and compiles
`(clock.monotonic-ms)`, proves that the resulting import is still unresolved,
links it only after installing the already-authorized Clock endpoint binding,
and then invokes `clock.svc` through native asynchronous capability IPC. The
remaining step toward genuine runtime discovery is a privileged catalog service
that accepts service-published descriptors and returns authority-filtered
views.

`clock.svc` now carries the canonical text in a non-loadable
`.cubit.interfaces` ELF section. The native host binding is an opaque local
index mapped to its already-granted capability slot; it is no longer Clock's
temporary global driver number. Thus even the linked CCLB artifact is insulated
from driver registration IDs while descriptor admission is being built.

The version 3 `.cclb` encoder and loader live in `src/ccl-format.ads` and
`src/ccl-format.adb`; the byte layout is specified in
[`docs/ccl-bytecode-format.md`](../../docs/ccl-bytecode-format.md). The format
contains bounded resource requests, ownership types and dispositions, initial
and dynamic-local declarations, descriptor-pinned portable imports, and
fixed-width instructions. Its canonical decoder rejects trailing data,
reserved bits, duplicate verbs, out-of-range type references, alternate operand
encodings, invalid enum representations, and malformed bytecode. Imported
modules return an unlinked program plus linkage and require explicit admission.
The same implementation is tested natively and in a freestanding CuBit process.

The first executable ownership model lives in `src/ccl-ownership.ads` and
`src/ccl-ownership.adb`. It implements unrestricted, move-only, must-handle,
borrowed-ro, and borrowed-rw state transitions; type-declared disposition
verbs; ownership-compatible branch joins; explicit scope completion; and
aggregate ownership-mode propagation. Native fixtures cover moves, copies,
drops, borrows, protocol transitions, and mismatched branches. The package
builds against both hosted and freestanding runtimes, and its current flow and
runtime-safety checks pass GNATprove at level 1.

`src/ccl-ownership-bytecode.ads` and `.adb` provide the ownership control-flow
verification layer. Modules declare injected local types; instructions
copy, move, explicitly discard, borrow, return borrows, or apply a declared
disposition. The verifier propagates the complete ownership environment through
forward control flow and rejects incompatible branch joins or invalid scope
exit. Its native fixtures and GNATprove level-1 pass are clean, and it builds
against the freestanding runtime. `CCL.VM.Verify` now invokes this layer as a
mandatory part of admission, and the executing VM defensively mirrors each
accepted ownership transition. Native and in-guest tests show a valid
disposition executing and an illegal must-handle drop being rejected.

The in-memory VM ABI carries ownership types, injected-local declarations,
local/verb instruction operands, and ownership opcodes. Canonical `.cclb` v3
serializes fixed-size type, disposition, local-declaration, and portable-import
tables and sends
the decoded program through both the ordinary and ownership bytecode verifiers.
The current locals are abstract verifier/runtime state only and cannot perform a
host operation. Nevertheless, instantiation already follows the future security
contract: the ordinary initializer rejects modules requiring locals, while
`Initialize_With_Locals` requires an exact host-supplied count and matching
value-kind and ownership-type tags. A module declaration never instantiates an
owned value by itself.

Native behavior tests can be built and run from the repository root with:

```text
nix develop --command bash -lc \
  'cd kernel && alr exec -- gprbuild -P ../userspace/ccl/ccl_native.gpr -p'
userspace/ccl/build/native/main
```

The hosted Workbench runner can be built and used with:

```text
nix develop --command make -C kernel ccl-run
userspace/ccl/build/ccl-run/ccl-run "(+ 20 22)"
userspace/ccl/build/ccl-run/ccl-run
```

The **Linux Workbench** UI preview renders a source editor and execution panel
in an SDL window, through the same **shared** CuBit widget toolkit used by
native desktop applications:

```text
nix develop --command make -C kernel ccl-ui-preview
```

The source pane uses the shared bounded editor core, including multicursor
editing, selection, word movement, pointer dragging, and scrolling. Its bounded
undo history restores the document and complete cursor/selection set
transactionally. Use Ctrl+Z to undo and
Ctrl+Y or Ctrl+Shift+Z to redo; adjacent typing and deletion operations are
coalesced until cursor movement or another non-edit action breaks the sequence.
Press F5 or Ctrl+Enter in the source editor to parse, type-check, and run the
buffer with a fixed fuel budget. Results include remaining fuel; failures report
a one-based source location and move the editor cursor to the erroneous form.
The blue Compile toolbar button instead analyzes the source, lowers it to CCLB,
and admits the artifact through the independent VM verifier. A successful
artifact enables the green VM Run button and populates the right-hand pane with
program counters, opcode bytes, and assembly-style instructions. Any source
edit immediately invalidates that artifact and disables VM Run; direct
interpretation and verified bytecode execution remain visibly separate modes.
VM Run retains a resumable machine state. Pause freezes it, Stop makes that
state terminal, and Step Into or Step Over advances exactly one instruction
(the two step modes are equivalent until CCLB gains call frames). The bytecode
table highlights the machine snapshot's current program counter while running
or stepping. The execution pane takes a bounded read-only VM inspection
snapshot after every state transition. It shows the operand stack top-first
and each visible local's value, availability, and ownership mode; the snapshot
also carries type tags, borrow counts, pending-import state, and the remaining
fuel for later inspector views. Inspection copies the private runtime state and
cannot mutate it. Compiler-emitted, independently validated debug ranges also
highlight the innermost active source expression and scroll it into view. Debug
metadata remains optional and non-authoritative: rejecting it disables the
source highlight without affecting VM admission.
Clicking a disassembly row toggles a breakpoint marker. Continuous execution
checks the current PC before dispatch, pauses before a marked instruction, and
skips that same breakpoint once when resumed. Step Over uses the active debug
range and runs until that source expression exits, another breakpoint is hit,
the VM waits, or execution terminates.
Close the window with Escape or the window close button. The hosted window is
structurally resizable: additional space expands the source editor and adds
visible source and disassembly rows. Drag either dotted vertical splitter to
resize the execution or disassembly pane, and drag the disassembly header
boundaries to resize its columns. The hosted framebuffer remains explicitly
bounded at 1280x720 logical pixels; larger drawable areas are letterboxed.
SDL and its normal hosted event loop belong only to the Linux adapter. The
**native CuBit Workbench** builds the same source, editor, compiler, verifier,
VM, and renderer behind a small platform boundary. It presents through
`desktop.svc`, receives composed text and pointer events from the desktop input
protocol, and resolves CCL Clock requests through an explicitly manifested
capability to `clock.svc`; it does not link SDL or use ambient hosted file I/O.
Build it with `make -C kernel ccl-workbench`, or launch it from the desktop's
Launch menu after `make -C kernel run-desktop`.

The core can also be compiled against CuBit's freestanding userspace runtime:

```text
nix develop --command bash -lc \
  'cd kernel && alr exec -- gprbuild -P ../userspace/ccl/ccl_cubit.gpr -p'
```

GNATprove analysis is enabled through `ccl_core.gpr`. The core is entirely in
`SPARK_Mode => On`; proof is still in progress, and outstanding verification
conditions must be discharged rather than suppressed before this milestone is
considered formally verified.

The bounded interface catalog, granted-binding table, and transactional linker
currently discharge all level-1 GNATprove checks. The catalog-aware language
frontend and generic AST-to-CCLB lowering do as well. None of these units uses
`pragma Assume` or disables SPARK. This does not erase the separately tracked
obligations in the scheduler, VM execution loop, or v3 bytecode formatter.

### In-guest runtime

`apps/ccl-vm/ccl-vm.app` builds the same source and bytecode runtime against
CuBit's freestanding Ada runtime. Its package manifest requests one narrowly
scoped service endpoint in slot 24. The verifier fixes that import's argument
and result types, authority class, and host binding before execution. When the
VM reaches the import it returns `Waiting_For_Host`; the adapter performs IPC,
validates completion, supplies a typed result, and resumes the same fuel state.

`services/test-host/ccl-test-host.svc` is a deliberately tiny regression-only
endpoint. Its package identity is granted authority to register driver 18, and
its sole operation maps integer 41 to 42. The Workbench uses the same binding
with a deterministic mock. The headless regression boots both processes and
requires source, bytecode, service invocation, and resumed-result markers:

```text
nix develop --command tests/headless/run.sh --test ccl-vm --timeout 25
```

Current fixed verifier state consumes approximately 24 KiB of stack and source
interpretation approximately 13 KiB in optimized freestanding builds. This is
bounded and works with CuBit's user-stack growth, but future multi-isolate hosts
should place AST, verifier-state, and value arenas in explicitly quota-managed
isolate memory rather than retaining them as large call-stack objects.

The guest adapter uses `capSubmit` and a token-checked completion. The current
self-test also exercises the bounded scheduler in `src/ccl-scheduler.ads`: one
isolate submits an import and suspends, a second isolate runs to completion,
and the first resumes only after its generation-tagged token is returned. The
scheduler has four fixed slots and at most one outstanding import per isolate;
it performs no allocation. CCL bytecode receives no raw syscall access.

Because the current verifier rejects backward branches, every dispatch is
already bounded by the 256-instruction program limit and remaining fuel. When
bounded iteration is added, dispatch quanta must become explicit so a runnable
isolate cannot monopolize its host between imports.
