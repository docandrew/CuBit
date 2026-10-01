# CCL Bytecode Module Format

Status: version 8 (CBOR, 2026-09-30); native objects plus opaque live VM resource values.
Version 8 replaces version 7's fixed little-endian layout with CBOR and adds
the function table. The history below describes what versions 6 and 7 added
to the program model, which version 8 keeps.

Version 7 adds the resource value kind (`4`) and retains the layout of v6. It
intentionally replaces v6; there are no deployed binaries requiring a
compatibility decoder. Live resource references are never module payload data.
Portable resource-valued imports are rejected until discovery can pin their
complete ownership contracts; `Result_Type_Tag` currently belongs only to the
in-memory import declaration, not a formerly reserved wire byte.

Version 6 added the native-object value kind to schema-pinned imports/locals.
Native strings, products and nested variants can flow from host calls through
locals and further calls or be returned intact. Projection and general variant
dispatch are implemented; compiled object construction remains future work.

Version 7 serializes scalar/variant/object programs, descriptor-pinned portable imports,
ownership type and disposition definitions, typed initial-local declarations,
and compiler-created dynamic locals. Values for initial locals are never
embedded in a module: the host must bind exact value-kind and ownership-type
matches when it instantiates the validated program. Imports support scalars and
simple nominal variants and persistable native objects; a zero-parameter call
still uses the integer-zero sentinel. Object bytes and runtime references are
never encoded in a module.

`CCL.VM.Native_Objects` owns the optional bounded snapshot storage; the ordinary
scalar machine remains small. Copying a VM value copies only its local reference,
not a full image. The native wrapper copies and validates replies before resume,
and exports only the currently waiting argument or completed result under an
independently approved schema. The scalar completion API rejects object
references, including apparently well-typed references forged by a host caller.
Initial external locals cannot inject them either. Hosts must use the native
wrapper for object programs and stop/reinitialize it to release retained data.
At most 16 object replies are retained per run; capacity is checked before
exposing another object-producing request to the host. This fixed pool is not
a substitute for future per-isolate accounting of the module's memory budget.

Owned imports carry their local, transfer mode, cancellation policy, and
success/failure/cancellation disposition verbs without erasing the ownership
contract. Runtime bindings are categorically absent from the representation.

`CCL.VM.Resource_Values` admits a live, nominally matching opaque reference from
the host registry to a pending factory call. The result is noncopyable and uses
the import's declared ownership tag; resources cannot use an unrestricted
ownership type. Ordinary scalar completion and external-local injection reject
resources. The host authenticates/correlates the acquisition and validates every
outgoing use against the registry. Static ownership cannot by itself prove
external handle cleanup or authenticate IPC. The source interpreter and public
factory/compiler linkage are not yet connected to this boundary.

CCL bytecode modules use the `.cclb` extension. The core payload has a single
canonical little-endian representation so it can later be hashed and signed
without normalization ambiguity. The decoder is bounded, allocation-free,
implemented in SPARK, and always invokes the ordinary CCL bytecode verifier
before returning a `Validated_Program`.

## Version 8 plan: canonical CBOR and interpreter parity (2026-09-30)

**Why.** The interpreter now has:
- a value arena (records, payload variants);
- lists of records and list fields, `(list-of T)`;
- recursive types (a list of self) and range types.

CCLB still compiles none of them, has no strings or lists, and keeps
records in a 16-snapshot pool. Parity comes before any further language
feature (docs/ccl-repl.md).

**Module encoding: CBOR, format v8, replacing v7 with no compatibility
decoder.**
- **Built on what exists.** It uses the Nix-pinned `cbor_ada` (a SPARK
  single-head decoder that already enforces shortest-form heads and rejects
  reserved additional-info values) and the restricted profile of
  `CCL.Objects.Persistence`: definite lengths, shortest integers, no tags,
  maps or floats, no trailing data.
- **Text is byte strings.** CCL text is carried as CBOR byte strings, as in
  persistence: CCL strings can hold NUL and bytes above 127.
- **One encoding per module.** No maps (so no duplicate keys) and
  shortest-form heads, so hashing and signing (COSE_Sign1 later) need no
  normalization.
- **Top level** is a fixed-position array:
  `[8, types, constants, imports, locals, functions, code]`.
  - Types carry every interpreter shape: products, sums, sequences,
    completed self-lists, and range bounds.
  - Constants carry text.
- **Placement: outside the core.** The codec lives in its own directory,
  `userspace/ccl/modules/`, like `persistence/`, so embedding the CCL core
  doesn't pull in CBOR. The core VM keeps accepting a decoded `Program`,
  which always goes through the ordinary verifier. Only loaders of `.cclb`
  bytes opt in (the module loader, `ccl-run`, the Observatory's "install
  module" request, tests). `ccl-format`'s v7 reader is removed from `src/`.

**The VM gains the interpreter's value model:**
- per-run text and list regions, and the value arena (nodes and slots,
  components referring only to older nodes);
- a `Value` refers to a node instead of an object-snapshot position;
- host images are copied in and out at import boundaries, as in the
  interpreter.

**Steps, each with compiler lowering, verifier rules, VM execution, proofs
and a differential test** (every interpreter test program is also compiled,
verified and run, and the literals must match):

1. **Done 2026-09-30.** The v8 module codec in `userspace/ccl/modules/` on
   `cbor_ada`, for today's v7 content only. It is proved at level 1, except
   one 64-bit conversion proved at level 3 (`make prove-ccl-format`). All existing CCLB tests pass
   unchanged; v7 is removed. Round-trip and hostile-input tests.
2. Strings: text constants, the text region, string builtins. **First
   slice done 2026-09-30:**
   - **Values.** `Text_Value` (kind 5) holds a descriptor into the run's
     text region (`Text_Regions`, 64 KiB, 512 strings). Strings hold up to
     8 KiB and results carry up to 1 KiB, the interpreter's bounds, so both
     fail at the same points (differential tests).
   - **Opcodes:** `Push_Text`, `Concat_Text`, `Length_Text` and
     `Equal_Text`, plus string `=`/`/=`, which the language gained at the
     same time.
   - **Constant pool.** The verifier checks constant references and pool
     bounds.
   - **Locals.** Text may live in compiler-created locals, never in
     host-supplied ones.
   - **Built-ins** (second slice, same day). One opcode, `Text_Builtin`
     (38), whose immediate names a `CCL.Text_Operations.Operation`:
     - the 13 operations `upper`, `lower`, `trim`, `reverse`, `first`,
       `last`, `skip`, `contains`, `index-of`, `starts-with`, `ends-with`,
       `replace` and `parse-int`;
     - the interpreter and the VM call the same Ada package. It is proved at
       level 1, so the engines differ only in how they copy operands in and
       store results;
     - the verifier types the operands and the result from the package's
       signature table;
     - differential tests cover every operation, the clamping edges,
       `parse-int` junk and overflow, and a pattern over 1 KiB.
   - **Next:** characters (`at`), `to-string`, then `split`/`join` (with
     lists, step 3).
3. Lists and list builtins.
4. The value arena: record and payload-variant construction, field and
   payload access, lists of records, list fields, `(list-of T)`, recursive
   types.
5. Range checks: a verified `Check_Range` on values entering range-typed
   positions.
6. Function values and captures (the former "parity 4").

**Limits to revisit:** 256 instructions and a 64-slot stack per module are
small for a full startup profile.

## Version 8 layout

A module is **one CBOR item** in the restricted profile that
`CCL.Objects.Persistence` also uses:
- definite lengths and shortest-form heads (`cbor_ada` rejects anything
  else);
- no maps, tags or floats, and no trailing data.

Every value therefore has exactly one encoding, and the bytes can be hashed
and signed as they are. The codec (`userspace/ccl/modules/ccl-format.ad?`)
lives outside the CCL core, so embedding the core doesn't pull in CBOR.

```text
["CCLB" (bytes), 8, [fuel, memory, in_flight],
 ownership_types, data_types, matches,
 [dynamic_locals, locals], imports, functions, constants, code]

ownership_types [[mode, [[verb, effect, next_type] ...]] ...]
data_types      [[shape, name, [[part_name, type] ...], low, high] ...]
matches         [[type, [target x 16]] ...]
locals          [[kind, ownership_type, data_type] ...]
imports         [[argument, result, authority, ownership_argument, local,
                  transfer, cancellation, parameters, success_verb,
                  failure_verb, cancel_verb, major, minor, operation,
                  argument_type, result_type,
                  digest, argument_schema, result_schema] ...]
functions       [[entry, [[kind, type] ...], result_kind, result_type] ...]
constants       [text ...]      (byte strings: Push_Text's pool)
code            [[op, local, verb, type, alternative, immediate, target,
                  import] ...]
```

**Field notes:**
- **Integers.** Enumerations and indexes are unsigned integers; an
  instruction's `immediate` and a range's `low`/`high` are signed.
- **Names** are byte strings of at most 32 bytes.
- **Digests and schema identities** are 32-byte byte strings, words most
  significant first.
- **Data types** are the declared types in registry order. The shape codes
  are:
  - product 1, sum 2, resource 3, sequence 4, callable 5;
  - bounded 6, a range type with `low`/`high`; the bounds are 0 for every
    other shape.

  A part may name a later type only as the list of its own type
  (`CCL.Types.Complete_Self_List`). The decoder defines it with a
  placeholder and completes it once every definition exists.
- **Match tables** always carry the full 16 targets.
- **Constants** are packed into the program's pool in order: at most 32
  constants and 4 KiB of text.
- **Unused operand fields** of an instruction must be zero, as the
  canonical-instruction rule requires.

**Tests.** Corruption tests locate a field by its one encoding
(`tests/ccl-module-support/module_patches.ads`). A byte sweep replaces
every byte of a module with 18 values and requires each result to be
rejected, or to be a valid module whose re-encoding is exactly those
bytes.

## Functions

Named functions (`define`) compile to CCLB functions (in-memory programs today;
the v7 module format refuses them with `Unsupported_Functions` until v8 adds a
function table).

* **Layout.** The main body comes first and ends in `Halt`. Each function's
  code follows as one contiguous region, in declaration order. The function
  table gives each function's entry, parameter kinds and result kind.
* **Calls.** `Call_Function` (`32`) takes the function index as its immediate.
  The arguments, evaluated left to right, stay on the stack as the callee's
  parameters. `Return_Function` (`33`) keeps the result and drops the
  parameters beneath it.
* **No recursion.** A function may call only functions declared before it
  (lower indexes); the main body may call any. The call graph is acyclic, so
  every program still terminates, and at most 16 frames are live.
* **Data only.** Parameters and results are Integer, Boolean or scalar-variant
  values. Function bodies may not use program locals, ownership imports,
  resource results or `Halt`; `let` inside a function binds on the operand
  stack. Owned resources therefore never cross a call.
* **Checked per region.** Each function is verified from its entry, starting
  with its parameters on the stack. Jumps and fall-through stay inside the
  region, and `Return_Function` must find exactly the parameters plus one
  result.
* **Whole-program stack bound.** The verifier records each region's maximum
  depth and, per call site, the depth beneath the callee's frame. It then
  combines them in index order, callees first, and rejects the program
  (`Stack_Overflow`) unless the deepest call chain fits the 64-slot stack. At
  run time the stack cannot overflow.
* **Ownership.** Only the main body can hold locals, so the ownership verifier
  checks the main region; calls are ownership-neutral.
* **Serializable state.** The machine state gains a bounded frame stack: return
  PC and callee per live call. It has no native pointers.

## Validation order

The loader fails closed in this order:

1. The profile, as each head is read: major type, shortest form, definite
   length, and each count's bound.
2. Magic, version and resource ceilings.
3. Ownership modes and dispositions, nominal definitions (including
   completed self-lists and range bounds), match tables and locals.
4. Import enumeration values, ownership contracts and descriptor linkage.
5. Functions.
6. Opcodes, operand bounds and canonical instructions.
7. Exactly one item: nothing may follow it.
8. Control-flow, stack, type, import and ownership verification.

The portable decoder never directly produces an executable linked program.
Execution requires descriptor admission, transactional binding, and ordinary
VM verification after linking. The convenience decoder returns a
`Validated_Program` only for authority-free modules with no imports.

## Identity and signature envelope

Package identity, provenance, content digest, signer identity, and signature
belong in a versioned envelope around the canonical core payload. They are not
yet implemented. Keeping the envelope separate permits the same payload to be
carried by an installed package, an interactive session, or a mutually
authenticated remote node without changing bytecode semantics.

The envelope must bind at least:

* the exact `.cclb` payload bytes;
* stable module and publisher identities;
* requested authority/effect declarations;
* resource ceilings;
* format and policy versions; and
* optional expiry or deployment constraints.

Signature validation establishes provenance, not authority. Installation or
session policy must still decide which declared imports are resolved.

## Future: a verified JIT (not planned soon)

The interpreter and this VM cover current needs. The format should not rule
out a later native-code compiler, for the same reason eBPF pairs a verifier with
a JIT: verification once makes checks at run time unnecessary.

What makes CCLB a good JIT input, and must be preserved as opcodes are added:

* **Forward jumps only.** The control-flow graph is acyclic, so every program
  terminates and each basic block's fuel cost is known when it is compiled.
  Fuel can be charged per block rather than per instruction. The exceptions
  are bounded builtins, which charge fuel per element.
* **Known stack shape.** The verifier fixes the depth and type of every stack
  slot at every instruction, so slots can map to registers with no run-time
  type tags.
* **Static call targets.** Function values name entries of the module's own
  finite function table, so an indirect call is a bounds-checked switch.
* **Checked arithmetic** lowers to the operation plus an overflow branch.
* **Resumable host calls.** Each `Invoke_Import` becomes a return point, so the
  host's suspend/complete protocol is unchanged.

Trust: the JIT itself joins the trusted base; a verified program run by a wrong
JIT is a sandbox escape. The intended shape is a template JIT in SPARK: fixed,
audited instruction templates per opcode, with proofs that only templates are
emitted and that jumps land on block starts. Its output is cached by module
digest, or produced at installation beside the signed module. Pages are never
both writable and executable, and mapping code executable is a capability,
held by one JIT service rather than by every process.
