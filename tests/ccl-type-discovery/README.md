# Discovered CCL data types

## Native completion preflight (2026-09-25)

`Native_Objects.Pending_Call` and `Accepts_Object_Result` let an authorized host
inspect a suspended copy-value call and check its nominal result type/capacity
without executing it. `Complete_Object` also accepts primitive/scalar-variant
native images and unboxes them through the shared value adapter; aggregate
images retain owned snapshots. This supports the shared asynchronous Config
bridge without exposing object indices or collection handles to scripts.
Preflight is not IPC authentication or a program/run identity check: the host
must retain the correct machine and approved binding for each pending request.

The suite now passes 240 checks. New cases exercise integer/Boolean images,
wrong-type preflight, exact unchanged pending-call snapshots, stopped/completed
calls, and more scalar completions than aggregate storage slots in one run.
The Config bridge separately passes 234 hosted lifecycle/dispatch checks; native
compiled Config reads resume through it before and after reboot. Focused
VM/native-wrapper/host-values SPARK checks321 pass (72 runtime,26 assertions,
23 contracts,158 initialization,4 non-aliasing,37 termination,1 dependency),
none unproved/justified. The capacity predicate is expressed directly rather
than duplicated as another runtime guard. Full codec proof remains unfinished.

## Native field projection and nested matches (2026-09-25)

CCLB v6 now includes `Project_Field` and admits general persistable sum types
in `Switch_Variant`. The compiler emits both from ordinary `field`/`match`
expressions. Operand verification tracks the exact product, sum and payload
types; malformed field indices, mismatched roots and reserved operands are
rejected. The private native execution instance resolves projections against
owned snapshots. Subtrees retain cursors into their owner's snapshot instead
of allocating another object or copying the image for every field access.
Exports still validate the receiver's independent nominal binding.

The hosted native-object suite passes 124 checks, including exact single-step
fuel accounting, both nested alternatives, 8 KiB subtree export and more field
accesses than snapshot slots. The Config source suite passes 378 checks,
including compiled handling of every Config read status, nested boolean access,
read-to-write subtree forwarding, no write on Missing or malformed data, and
missing-grant rejection before execution (including read allowed, write denied).
These are Linux-hosted regressions; the native writer and independent read-only
reboot separately pass Config IPC and SQLite/WAL/ext2 validation. The reboot
executes both whole-object returns and field/match against recovered objects.
Native Workbench builds; the normal desktop ISO is restored after testing.

Focused SPARK analysis of `ccl-vm`, `ccl-vm-native_objects` and `ccl-host_values`
discharges 318 obligations: 72 runtime checks, 26 assertions, 21 functional
contracts, 159 initialization, 4 non-aliasing, 35 termination and 1 dependency.
No unproved or justified checks and no `Assume`. This covers the implemented
contracts/checks, not a proof of full compiler semantics or IPC authentication.
Explicit `Global => null` on the actual native storage callback avoids a
GNATprove frontend assertion during generic inlining and checks its intended
no-global-side-effects boundary; it does not suppress any verification.

```sh
nix develop -c bash -c 'ulimit -v 8388608; cd kernel && \
  alr exec -- gnatprove -P ../tests/ccl-type-discovery/native_objects.gpr \
    -u ccl-vm.adb ccl-vm-native_objects.adb ccl-host_values.adb \
    --subdirs=object-projection-proof --level=2 -j2'
```

The complete codec proof remains unfinished (the earlier run was deliberately
stopped for excessive proof-generation memory use). Aggregate constructors and
string operations in bytecode, public Config lifecycle/Workbench dispatch,
durable defaults and performance work remain outstanding.

## Native object transport (CCLB v6, 2026-09-25)

`native_objects.gpr` exercises the shared compiler, encoder/decoder, pinned
linker and optional `CCL.VM.Native_Objects` machine. Whole records (including
8 KiB strings) pass through suspended host imports and locals without a client
codec. The wrapper owns its snapshots and exports only its current argument or
result under approved nominal metadata. The scalar VM stays small and rejects
forged object references through its public completion and initial-local APIs.
55 checks pass: missing grants, full roundtrip, input-buffer independence,
wrong schema, malformed reply, failed call, stop cleanup and capacity admission
before a seventeenth host effect, forged initial locals, duplicate completions
and rejection of obsolete v5 modules. Core VM, catalog, portable import and Config
regressions also pass. VM+wrapper SPARK discharges195 checks; this is not a
whole-system authorization proof. Logs `/tmp/cubit-object-vm-*.log`.
Projection and general matches are implemented in the newer checkpoint above;
general aggregate constructors remain pending.
Version6 deliberately replaces5; no legacy decoder is retained.

## Portable typed imports (introduced in CCLB v5, 2026-09-25)

The compiler and codec now retain nominal argument/result references and schema
keys. Admission requires an explicitly supplied authorized catalog, compares
complete nominal definitions rather than local type numbers, and installs no
bindings until all imports pass. Schema identity does not confer authority.
An object-wrapped Integer remains distinct from an ordinary scalar contract.

`portable_objects.gpr` passes 1023 checks covering source compilation, canonical encode/decode,
authorized linking, shifted registry IDs and stepwise VM execution, as well as
missing grants/schemas, changed definitions, forged keys, malformed metadata,
all truncated prefixes and atomic rejection when a later import fails.
The former v4 restrictions below describe earlier milestones; v5 supersedes
them. Strings and general product/nested payload execution remain unsupported.

The host-value/catalog units pass 133 SPARK checks (64 runtime, 2 functional,
67 flow/termination). The codec proof was interrupted because proof expansion
exceeded a useful resource budget; do not treat codec regression tests as a
completed proof. See `build/portable-objects/contracts-proof/gnatprove/gnatprove.out`.

## Nominal VM import completion (2026-09-25)

The internal VM import descriptor now carries argument/result nominal type
references. The verifier tracks those types on the operand stack and rejects
unsupported layouts and mismatched arguments. Execution suspends at the import;
repeated Continue calls do not consume fuel or replay an effect while waiting.
Completion accepts only a correctly typed, unrestricted value of the declared
result type, including a valid variant alternative. A host result is not an
authority injection mechanism.

The earlier completion path checked only `Kind`. The new regression reproduced
acceptance of a tagged integer before the fix (`/tmp/cubit-config-vm-resume-before.log`,
check 8). Checking type/ownership at this external input boundary fixes that
hole; this is not evidence of an exploitable kernel capability bypass.

`import_results.gpr` builds `import_result_tests.adb`. Tests exercise scalar and
variant responses, distinct nominal types with identical shapes, invalid tags,
copyability/alternatives, suspension, verifier rejection, and exact linkage-local
matching. The existing complete hosted VM/CCLB/ownership suite also passes.
The VM unit's 174 SPARK checks discharge, none unproved (29 runtime, 14 assertions,
11 functional contracts, 120 other checks). This proves its implemented
obligations, not complete language or host-service functional soundness.

```sh
nix develop -c bash -c 'cd kernel && \
  alr exec -- gprbuild -p -P ../tests/ccl-type-discovery/import_results.gpr && \
  ../tests/ccl-type-discovery/build/import-results/import_result_tests && \
  alr exec -- gnatprove -P ../tests/ccl-type-discovery/import_results.gpr \
    --subdirs=proof -u ccl-vm.adb --level=2 -j2 --report=all --checks-as-errors=on'
```

This was the internal execution prerequisite. The v5 implementation above now
adds portable schema-pinned imports without dropping their nominal fields.

## Typed source host objects (2026-09-25)

Host import descriptors now declare independently approved argument/result schema
keys. The shared catalog retains those bindings with its type registry, and the
checker assigns the actual nominal type to each object call. A missing schema,
unsupported executable shape or wrong argument type fails before host invocation.
Grants match the complete import declaration, including schema keys: publishing
an interface or changing its result schema cannot grant access by itself.

`host_objects.gpr` / `host_object_tests.adb` exercise 109 hosted checks: discovered
variant echo/get, integer/Boolean/Unit alternatives, typed function arguments,
shifted registry IDs, missing schemas, wrong nominal types, missing/stale grants,
malformed host images, wrong result kinds and host failure; empty/maximum/oversize
string results and explicit rejection of record execution. Portable simple
variant imports now compile through CCLB v5.
Run in Nix:

```sh
nix develop -c bash -c 'cd kernel && \
  alr exec -- gprbuild -p -P ../tests/ccl-type-discovery/host_objects.gpr && \
  ../tests/ccl-type-discovery/build/host-objects/host_object_tests'
```

The native Config test now executes source imports too; see its README. This is
not general aggregate source execution: records/nested payloads remain
unsupported by execution. Simple variant imports work in CCLB v5. The host ABI owns complete native objects, but
the interpreter currently executes only its supported scalar/string/simple-sum
shapes; strings retain its existing 1,024-character limit. No pointer, handler or
authority is persisted as data. The Config adapter is nonblocking; the dedicated
native test host waits for IPC, and must not be copied into a GUI event loop.

Focused SPARK runs pass for `CCL.Objects.Values` (29 checks) and the expanded
schema catalog (22 checks), with none unproved. Logs:
`/tmp/cubit-source-host-proof.log`, `/tmp/cubit-source-host-catalog-proof.log`.
These prove the specified conversion/catalog obligations, not the whole
interpreter, policy system or database crash consistency.

The actual direct/periodic interpreter host instantiations also pass the selected
object-call region: 66 runtime checks and 324 flow/initialization/termination
checks, none unproved. Reproduce with:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove \
  -P ../tests/ccl-remote/remote.gpr --subdirs=typed-object-host-proof \
  -u interpreter_host.adb --limit-region=ccl-language.adb:1660:1798 \
  --level=2 -j2 --report=all --checks-as-errors=on'
```

Log: `/tmp/cubit-source-host-interpreter-proof.log`. This selected-region proof
does not supersede the unresolved broader interpreter work described below.

Run in Nix:

```sh
nix develop -c bash tests/ccl-type-discovery/run.sh --prove
```

`CCL.Types.Import_Definition` copies only the requested root's dependency
closure. Source IDs are translated into the receiving registry; identical
existing nominal definitions are reused, not duplicated. A conflicting name
or exhausted registry rejects the whole staged import, leaving the target
unchanged. Backward-only type references allow bounded reverse/forward passes
without recursion, heap allocation, native pointers or evaluating constructors.

`CCL.Catalog.Publish_Type` applies that operation to a trusted discovery view.
The frontend takes one type-registry snapshot before parsing; AST nodes/import
descriptors do not each contain a registry. Discovered variants can be used
without repeating their declarations in source. Local declarations append to
the snapshot and cannot replace advertised names. An empty view reveals none
of these definitions. This API does not acquire discovery authority, authenticate
a publisher, compute schema digests, grant host operations or install bindings:
the host must obtain an authorized, validated descriptor before publishing it.
Conflicting unqualified names fail; implicit renaming/namespace aliasing is not
implemented.

50,162 hosted import checks cover all dependency depths and target capacities,
atomic capacity failure, stable prior IDs/definitions/layouts, repeated imports,
primitive/unknown roots, shared dependencies, unreachable conflicting types,
shape/count/name/payload/order conflicts and rollback after staging dependencies.
58 catalog checks exercise interpretation, analysis, compilation, CCLB encode/decode,
VM execution, source declarations and typed function parameters; the existing
compiler still rejects function definitions as unsupported. Type/operation
visibility without grants still fails link admission.

36 additional checks distinguish visible type metadata from executable
values. A snapshot containing unused record/nested-sum descriptions must not
prevent a scalar program from running or round-tripping through CCLB. Attempts
to use those unsupported shapes through constructors, comparisons, locals,
match tables or inappropriate instruction metadata still fail verification.
The native shifted-registry reader caught the original blanket-rejection bug.
Shared type analysis rejects construction of unsupported variant payloads for
both execution modes. A string-payload constructor regression reproduced an
incorrect interpreter success before this restriction: the scalar variant
representation could not preserve the string. Description visibility must not
be mistaken for implementation support. The current supported variant payloads
remain Integer, Boolean and Unit.

The focused proof of `ccl-types.adb` discharges 50 checks, including absence of
runtime errors and unchanged-target behavior on rejected imports. This is not
a proof of complete graph correspondence, issuer identity, policy or kernel IPC.
Full graph/identity-preservation properties are regression-tested here and by
the independent `CCL.Types.Correspondence` tests.

The corrected VM also discharges all 171 focused SPARK checks (including 11
functional contracts); this proves the implemented obligations, not a complete
semantic soundness theorem. Reproduce separately with:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove \
  -P ../tests/ccl-type-discovery/discovery.gpr --subdirs=vm-proof \
  -u ccl-vm.adb --level=2 --checks-as-errors=on -j2'
```

The broader `prove-ccl-interpreter-host` run reported unresolved obligations in
function bookkeeping, host conversion and analyzed-root handling. That run was
stopped after failures to revise the source admission boundary; its log is
`/tmp/cubit-type-discovery-language-proof.log`. An isolated HEAD comparison
reproduced the function-count increment failure (`/tmp/cubit-type-discovery-baseline-823.log`);
that property predates type discovery. This comparison does not establish the
origin of every other failure. The older whole-interpreter proof counts must
not be presented as current evidence; the 50/171 focused results above are narrower.

Function-table hardening removes the unused out-of-table sentinel: definition,
resolved-call and handler indices now have the actual table index subtype.
Failed resolution is represented by a diagnostic, not an invalid index. The
parser reserves a slot before parsing a body and publishes its successor count;
the checker publishes that same slot only after checking the body. No added
guards, assumptions, SPARK exclusions or frame contracts are used.
156 hosted checks exercise every table capacity, highest-slot calls, overflow
rejection, self/forward calls, duplicates, arity/type errors, handler restrictions,
caller/callee scope isolation and every truncated prefix of a function program.
Retained handlers execute the last table slot after the source buffer changes;
an invalid main expression prevents callback preparation and execution.
These test semantics and rejection behavior; bounded indices alone are not a
proof that a name resolves to the semantically correct function.

Focused SPARK validation on this revision: the parser's reserved-slot count
publication no longer generates a range VC (the subtype makes it statically
safe); the checker region discharges 14 runtime checks plus 128 flow/termination
checks. Reports: `/tmp/cubit-function-proof-825.log`,
`/tmp/cubit-function-proof-checker.log` and
`/tmp/cubit-function-proof-checker-report.txt`. Both evaluator table accesses
also pass their focused runs (`/tmp/cubit-function-proof-evaluator.log`).
Native Workbench compilation,
a four-vCPU KVM startup/first-frame smoke test and normal ISO restoration pass
(`/tmp/cubit-function-native-smoke.log`, `/tmp/cubit-function-workbench.serial`).
That smoke test is not native execution of every hosted function fixture.

The [native Config fixture](../config-object-client/native-app/README.md) also
uses a discovered `Reading` type: no declaration text appears in its snippets.
Its reader uses a shifted type index; the shared VM adapter translates values
against the retained approved Config binding. The source-host extension above
now exercises these simple variants through actual CCL Config calls as well.

## Scalar boundary hardening (2026-09-25)

Interpreter payloads now use a private scalar record with an actual two-value
kind subtype, integer and boolean fields. They cannot carry VM ownership tags,
copyability flags, nominal variant identity or alternative indices. Those
concepts must use their appropriate runtime representation, rather than being
silently embedded in an interpreter scalar. Export constructs a fresh plain VM
value; no unchecked conversion, predicate assumptions or defensive internal
guards are used to make this representation work.

The existing `From_Scalar (Left.Scalar)` precondition failure was reproduced by
the focused pre-change proof (`/tmp/cubit-scalar-proof-before.log`). Narrowing
the representation removes that precondition; the replacement host conversion's
discriminant check discharges. Its limited report has one runtime check plus
132 flow/termination checks, **not a whole-interpreter proof**. Helper analysis
also passes. `/tmp/cubit-scalar-proof-after.log` records those results. The final
generic-boundary selection in that log contains only flow checks and must not
be claimed to prove the instantiated host wrapper.

An actual-instantiation run through `Interpreter_Host.Evaluate` and the periodic
pump (`/tmp/cubit-scalar-instantiated-proof.log`) proves both guarded scalar
conversion preconditions, but leaves **four discriminant checks unproved**:
the default result assignment and the converted result assignment, in each of
the two instantiations. The callback's `out CCL.Host_Values.Value` formal permits
a constrained actual even though it changes the discriminant. Do not report
this wrapper or whole interpreter as fully proved. Next: revise the owned
callback-result API so its shape guarantees a writable result kind, rather
than adding exception handlers or guard branches around assignments. The
normal interpreter uses unconstrained result locals; current hosted/native
regressions have not reproduced a runtime exception on that execution path.

Follow-up: the shared callback profile now uses an owned
`CCL.Host_Values.Call_Result` envelope. Its value remains discriminated, but
the envelope prevents the caller from constraining that component to one kind.
The four previously unproved assignments now discharge in both actual wrapper
instantiations, along with the guarded scalar preconditions. The fixture also
proves safe transitions and its returned-kind contract. See
[owned host results](../ccl-host-results/README.md). No exception handlers,
constrainedness guards or compatibility callback overloads were added.

Separately, the scalar-copy host adapter used to accept a VM value carrying an
ownership tag and project it into an ordinary integer/boolean. A hosted test
reproduced this (`/tmp/cubit-scalar-ownership-before.log`); the adapter now rejects
noncopyable/tagged results along with variants. Failed callbacks are not projected.
This is a host/VM ownership-model boundary bug, not evidence that an arbitrary
unprivileged process could mint kernel authority. Explicit transfer operations
remain separate from scalar-copy imports.

Hosted view tests cover those rejections, wrong-primitive result diagnostics,
and true/false argument/return round trips through a typed function and host
call. Discovery/frontend checks now total 79, checking integer/boolean variant
payloads and ownership-free result metadata as well as nominal identities.
Import/metadata/function suites remain 50,162 / 36 / 156 checks. Hosted remote
interpreter, periodic widget and Config VM adapter tests pass. Native Workbench
compilation and KVM startup/first-frame smoke test pass; the normal desktop ISO
is restored. That smoke test does not execute every hosted boundary fixture.
Logs: `/tmp/cubit-scalar-{regressions,discovery-final,native}.log`.

### Root-result admission (2026-09-25)

The broader interpreter proof reported an unresolved index at the top-level
handler-export diagnostic: `Check_Node` returned a type, but the caller then
indexed the original possibly-sentinel root reference. No runtime failure was
reproduced. Root export admission now lives at the end of `Check_Node`, where
the existing node bounds validation already applies. Its ordinary diagnostic
path supplies the location. No extra bounds guard, assumption, or unproved
postcondition was added; nested handlers remain legal as callback arguments.

The function suite passes 174 checks, including direct/let/if handler exports
with exact root diagnostic positions through analysis and interpretation.
Callback, session, remote and periodic host regressions also pass. The focused
no-host instantiation proof discharges four runtime checks and 130 flow/
termination checks. Reproduce the diagnostic region with:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove \
  -P ../tests/ccl-remote/remote.gpr --subdirs=root-export-proof \
  -u ccl-language.adb --limit-region=ccl-language.adb:1247:1266 \
  --level=2 --report=all --checks-as-errors=on -j2'
```

The broad proof was intentionally stopped after finding that obligation, before
editing its inputs. This focused result is not a completed whole-interpreter
proof. Logs: `/tmp/cubit-root-export-{proof,regressions,hosts}.log`.
Native Workbench/ccl-control/config-check builds and the 90-second KVM
Workbench startup/first-frame test also pass; normal desktop ISO restored.
That smoke test is not an interactive exercise of handler errors. Log:
`/tmp/cubit-root-export-native.log`.
The actual direct/periodic host-instantiation run also completes: six runtime
checks (two in each of three instances) and 303 flow/termination/non-aliasing
checks, none unproved. Command: use `-u interpreter_host.adb` and
`--limit-region=ccl-language.adb:1247:1255` with the project above. Log:
`/tmp/cubit-root-export-host-proof.log`.
