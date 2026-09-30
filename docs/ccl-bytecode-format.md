# CCL Bytecode Module Format

Status: version 7; native objects plus opaque live VM resource values

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

## Version 7 layout

```text
header             32 bytes
ownership table     type_count × 36 bytes
nominal type table  data_type_count × 580 bytes
match table         match_count × 34 bytes
local table         local_count × 4 bytes
import table        import_count × 120 bytes
instruction table   instruction_count × 16 bytes
```

No trailing bytes are permitted. All reserved fields must be zero.

### Header

| Offset | Size | Field |
|---:|---:|---|
| 0 | 4 | ASCII `CCLB` |
| 4 | 2 | format version, currently 7 |
| 6 | 2 | header size, currently 32 |
| 8 | 4 | exact total payload length |
| 12 | 2 | instruction count, maximum 256 |
| 14 | 2 | import count, maximum 16 |
| 16 | 4 | requested instruction fuel |
| 20 | 4 | requested isolate memory bytes |
| 24 | 2 | maximum outstanding host operations |
| 26 | 1 | total local count (initial plus dynamic), maximum 32 |
| 27 | 1 | ownership-type count, maximum 32 |
| 28 | 1 | compiler-created dynamic-local count, at most the local count |
| 29 | 1 | nominal type count, maximum 32 |
| 30 | 1 | match table count, maximum 16 |
| 31 | 1 | reserved, zero |

Current loader policy permits at most 1,000,000 fuel, 16 MiB of declared
memory, and one outstanding operation per isolate. A module must request
nonzero fuel. Loading does not itself grant these resources: the host may
reduce limits according to installation and session policy.

### Ownership type definition

| Offset | Size | Field |
|---:|---:|---|
| 0 | 1 | mode: unrestricted `0`, move-only `1`, must-handle `2` |
| 1 | 1 | active disposition count, maximum 8 |
| 2 | 2 | reserved, zero |
| 4 | 32 | eight fixed disposition slots, 4 bytes each |

Each disposition slot contains a verb byte, effect byte (`consume` `0`,
`transfer` `1`, or `transition` `2`), next-type byte, and one zero reserved
byte. Active verbs must be unique. Every next-type value is range checked;
active transitions may refer only to a declared type. Fixed slots keep the
representation canonical and the decoder bounded.

### Nominal type definitions and match tables

Each 580-byte type definition has a 33-byte name (one length byte, then 32
zero-padded bytes), shape byte at offset 33 (sum = 2), count byte at 34, and
zero reserved byte at 35. Offset 36 contains sixteen 34-byte alternatives:
the same name encoding followed by a payload type reference. Unused alternative
slots must be entirely zero. Names and alternative names must be valid and
unique in their respective scope. Reference 1 means Integer, 2 Boolean, and
6 Unit (no payload); executable variants reject other payload kinds. Definitions receive
snapshot-local references 7 through 38 in table order. The registry codec can
describe products too; these and non-scalar variants use native-object values,
not scalar variant opcodes. Scalar match/constructor instructions still reject
non-scalar payloads.

A 34-byte match entry contains its nominal type reference, a zero reserved
byte, then sixteen little-endian 16-bit code targets, ordered by alternative.
Every declared alternative must have a forward, in-range target when used by
`Switch_Variant`; remaining entries must be zero. Tables contain no authority,
service identity, or runtime bindings. These type references are local to this
module, never authenticated IPC schema identities.

### Local declaration

| Offset | Size | Field |
|---:|---:|---|
| 0 | 1 | representation: integer `0`, Boolean `1`, scalar variant `2`, native object `3`, opaque resource `4` |
| 1 | 1 | declared ownership type |
| 2 | 1 | type reference for variant/object/resource, otherwise zero |
| 3 | 1 | reserved, zero |

Initial locals precede compiler-created dynamic locals. Initial entries form
an instantiation contract, not module-owned data; the host must supply exact
value-kind, ownership-type, and nominal-type matches with valid alternatives.
Dynamic locals begin uninitialized and are checked by the ownership verifier.

### Portable import declaration and linkage

| Offset | Size | Field |
|---:|---:|---|
| 0 | 1 | argument value kind |
| 1 | 1 | result value kind |
| 2 | 1 | authority class |
| 3 | 1 | ownership-argument Boolean (`0` or `1`) |
| 4 | 1 | owned local index |
| 5 | 1 | transfer mode |
| 6 | 1 | cancellation mode |
| 7 | 1 | source parameter count |
| 8 | 1 | success disposition verb |
| 9 | 1 | failure disposition verb |
| 10 | 1 | cancellation disposition verb |
| 11 | 1 | reserved, zero |
| 12 | 2 | interface major version |
| 14 | 2 | interface minor version |
| 16 | 1 | operation ordinal |
| 17 | 1 | argument nominal type reference (zero for scalar representation) |
| 18 | 1 | result nominal type reference (zero for scalar representation) |
| 19 | 5 | reserved, zero |
| 24 | 32 | descriptor SHA-256 digest as four little-endian 64-bit words |
| 56 | 32 | argument schema identity (zero for ordinary scalar contract) |
| 88 | 32 | result schema identity (zero for ordinary scalar contract) |

Value kinds, authority classes, transfer modes, and cancellation modes use the
explicit enum representations declared by the CCL core. A decoder range-checks
each byte before converting it to the corresponding enum. The digest, version,
and ordinal pin operation identity; the remaining fields pin its full type,
effect, and ownership contract.

The decoder returns an unlinked `Program` and a separate `Linkage_Table` after
structural and bytecode verification. Trusted admission must match that linkage
against already-granted operations, install opaque host-local bindings, and
verify the linked program before execution. No capability slot, endpoint,
driver ID, process ID, or host binding can be represented in a CCLB v7 payload.
Import kinds are Integer (`0`), Boolean (`1`), scalar Variant (`2`), or native
Object (`3`). Objects require a nonzero type reference, a persistable shape
outside the scalar representations, and a pinned nonzero schema identity.
A variant requires
a declared nominal reference and nonzero schema identity. Scalar representations
require nominal reference zero; a nonzero schema identity distinguishes an
object-wrapped Integer/Boolean from an ordinary scalar contract.

Resource (`4`) is not admitted in the portable import table yet. It must not be
disguised as Object, assigned a persistence binding, or encoded as an integer
handle to bypass the missing resource-signature linkage.

The compiler lowers the checked source type, but does not grant anything. Linking
matches the complete descriptor contract against existing grants, then resolves
each nonzero schema identity against an explicitly supplied authorized catalog.
It compares the full nominal type definition, including names, alternatives,
payloads and ordering, using type correspondence rather than numeric-ID equality.
Shifted local IDs are allowed; changed definitions or absent schemas fail. The
default empty schema view only admits ordinary scalar imports. All checks finish
before any runtime binding is installed. Schema identity is not issuer identity
or authority, and this step does not implement signature verification.

Host completion independently checks the expected nominal type, valid alternative
and unrestricted ownership before resuming. Module deserialization is separate
from Config value transport: Config clients still exchange native typed objects,
not serialized CCLB/CBOR payloads.

### Instruction

| Offset | Size | Field |
|---:|---:|---|
| 0 | 1 | opcode |
| 1 | 1 | local index, or zero when unused |
| 2 | 1 | disposition verb, or zero when unused |
| 3 | 1 | nominal type for Make_Variant/Equal_Variant, otherwise zero |
| 4 | 8 | signed immediate, two's-complement little-endian |
| 12 | 2 | jump target |
| 14 | 1 | import index |
| 15 | 1 | one-based alternative for Make_Variant, otherwise zero |

Unused operands must be zero. Boolean immediates must be exactly zero or one.
These rules reject semantically equivalent alternate encodings.

The scalar arithmetic opcodes are `Add_Integer` (`3`), `Multiply_Integer`
(`19`), `Divide_Integer` (`20`), and `Modulo_Integer` (`21`). Each consumes two
integers and produces one integer. Division by zero and the sole signed-division
overflow case terminate execution with a typed status. These additive opcodes
retain their prior opcode numbers. Strings remain excluded pending a future
canonical constant pool and variable-sized value representation.

| Opcode | Instruction | Effect |
|---:|---|---|
| 22 | Make_Variant | Consume the declared scalar payload (none for Unit), produce a nominal variant. |
| 23 | Equal_Variant | Compare two values of the declared enum type; payload sums are rejected. |
| 24 | Switch_Variant | Immediate indexes a match table; consume a variant, jump and expose only that alternative's payload. |
| 25 | Copy_Stack | Immediate is depth from top, zero-based; copy only unrestricted data. |
| 26 | Drop_Under_Top | Preserve result while removing the unrestricted lexical payload underneath. |
| 27 | Project_Field | Consume an object of the declared product type; produce the immediate's field. |
| 28 | Subtract_Integer | Consume two integers, produce their difference; overflow terminates with a typed status. |
| 29 | Less_Integer | Consume two integers, produce `left < right`. |
| 30 | Less_Equal_Integer | Consume two integers, produce `left <= right`. |
| 31 | Equal_Boolean | Consume two Booleans, produce their equality. |
| 32 | Call_Function | Immediate is an earlier function's index; its parameters are on the stack. |
| 33 | Return_Function | Keep the result, drop the parameters, continue after the call. |

The compiler adds no opcode for the other operators:

* `>` is `Less_Equal_Integer` followed by `Not_Boolean`, and `>=` is
  `Less_Integer` followed by `Not_Boolean`. Both are exact on integers.
* `/=` is the matching equality followed by `Not_Boolean`.
* `and` and `or` lower to the conditional's forward jumps, so the right
  operand runs only when it decides the result.

Operands are always evaluated left to right.

The verifier propagates a distinct payload stack type into every dispatch arm,
and enforces nominal identity, ownership-copy restrictions, and stack equality
at joins. Moved locals are opaque transferred values on the operand stack:
they can be returned but cannot be copied, boxed, discarded, laundered through
arithmetic/scalar imports, or reinitialized as unrestricted locals. Resource
operations remain governed by the existing ownership-import/disposition path.

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

1. minimum buffer size, magic, version, and header size;
2. bounded counts and exact total length;
3. reserved fields and resource ceilings;
4. ownership modes, dispositions, nominal definitions, match tables, and locals;
5. import enum values, ownership contracts, and descriptor linkage;
6. opcode and operand encodings;
7. canonical-encoding rules; and
8. control-flow, stack, type, import, and ownership verification.

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
