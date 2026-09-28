# Typed resource signatures

Run the Linux-hosted tests through Nix:

```sh
nix develop -c bash tests/ccl-resource-signatures/run.sh
nix develop -c bash tests/ccl-resource-signatures/run.sh --prove
```

Host signatures now distinguish ordinary data from opaque live resources.
Argument/result resource names are canonical nominal identifiers resolved through
the visible catalog, whose resource ownership policy must also be published.
They are not persistence schema keys, local numeric type IDs or authority grants.

The test interface is deliberately generic, not a Config parser intrinsic:

```lisp
(collections.open)                       # IntegerCollection
(collections.get (collections.open))    # Integer
(collections.close (collections.open))  # Integer sentinel
```

These examples are **test interfaces, not runnable Workbench examples**.
The compiler now lowers resource factories, owned locals, borrowing and moves
and invokes the VM ownership verifier before accepting the result. In particular
`get(open())` fails ownership checking: its temporary must-handle collection is
never closed or returned. Binding it explicitly permits read, close, then return
of the ordinary copied value:

```lisp
(let ((collection (collections.open)))
  (let ((value (collections.get collection)))
    (let ((closed (collections.close collection))) value)))
```

Linking reconstructs the approved ownership layout from the independently
supplied catalog, matches complete nominal definitions despite different local
type numbers, and rejects weakened ownership rules or altered local associations
before installing any bindings. Import reuse distinguishes calls on different
locals without changing service operation identity or grants.

Source calls may now carry a separately typed resource receiver and ordinary
data argument, e.g. `(collections.set collection 42)`. `Receiver_Resource` pins
the receiver's nominal identity; `Parameters` counts only the ordinary data
arguments (zero or one), not the receiver. A zero-data call such as
`(collections.read collection)` uses the existing scalar sentinel while borrowing
its separate owned local. The receiver is checked/evaluated first, then the data;
ownership is applied when submitting the call, not when starting argument
evaluation. Closing the receiver while evaluating the data therefore fails the
ordinary VM ownership verifier. BASIC uses `collections.set(collection, 42)`;
both views round-trip through the same checked tree.

Generic factory type-argument specialization and portable resource encoding
remain pending. Direct interpreter
admission still rejects resource imports before any host effects. CCLB encoding
also rejects these imports rather than dropping ownership metadata.

75 checks cover nominal mismatch between otherwise identical resource shapes,
integer-as-handle rejection, visible-but-unapproved resource types, canonical
signature fields, exact grant matching, no execution before interpreter admission,
rejected lowering without a resource tag, and refusal to persist or unbox a
genuine live reference.

277 additional checks compile/link/run resource calls on the Linux-hosted native
object VM, including two simultaneous handles, expression receivers, explicit
moves, and copied data after close. Negative tests cover abandoned temporaries,
leaks, double-close, use-after-move, inconsistent branch ownership, weaker modes,
altered dispositions/tags/locals, receiver/data contract tampering and unsupported
portable encoding. Two writable handles retain independent values; closing a
receiver in its own data expression is rejected. The host
simulates resource operations; this suite itself does not issue CuBit IPC.

The standard `--prove` run analyzes `CCL.Host_Values`, `CCL.Objects.Values`,
`CCL.Catalog`, and `CCL.Compiler`: 340 checks discharged, no unproved or
justified checks. In particular `From_Host` proves it never accepts a resource
as a persistence image. This does not prove the source type checker or
authenticate an IPC sender. The catalog still has one flow warning for an
intentionally unused publication target reference.

Compiler AST reads use one checked accessor, including field,
match-scrutinee and equality operands; invalid references report a malformed
typed tree rather than converting the absent-node sentinel to an array index.
No assumptions, suppressed checks or SPARK-Off sections were introduced.

This proves the generated safety checks and existing contracts of these units,
not semantic equivalence of source and bytecode or end-to-end authorization.
Compiler flow warnings about unused final state remain. Ownership rejection and
linkage rejection atomicity are additionally regression-tested above, not newly
proved functional postconditions.

Ordinary Config data stays copyable. Only the live collection reference is a
resource; its value type will determine the successful Get result and Set input.
