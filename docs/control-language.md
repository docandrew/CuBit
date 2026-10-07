# CuBit Control Language (CCL)

Status: design proposal

The CuBit Control Language is a strongly typed, capability-aware language for
interactive system control, automation, and small event-driven services. It is
intended to be a first-class CuBit interface rather than an emulation of a Unix
shell, terminal, or process environment.

The provisional user interface is BASIC-like for concise interactive use. A
Lisp-like form provides the canonical structured representation. Both forms
elaborate into the same small, typed core language.

The language and implementation subsystem are abbreviated **CCL**. Source files
use `.ccl`, bytecode modules use `.cclb`, and the bytecode verifier and
interpreter are collectively called the **CCL VM**. Implementation sources live
under [`userspace/ccl`](../userspace/ccl/README.md).

Linux-hosted development tools are collectively the **CCL Workbench**. The
hosted runner, `ccl-run`, executes the same semantics with deterministic mocks
or emulated CuBit host imports; `ccl-debug` will add source and bytecode stepping.
The freestanding runtime inside CuBit is `ccl-vm.app`.

The old `CuBASIC` prototype embedded in `desktop.svc` has been removed. The
native CCL Workbench is an ordinary desktop application with the supported
editor and REPL; command evaluation and management authority do not belong in
the compositor. This does not remove CCL's BASIC-style syntax or the Desktop's
restricted declarative theme loader. The typed desktop protocol, retained widget
model, and bounded canvas are specified separately in
[`ccl-ui.md`](ccl-ui.md).

CCL's package-description and system-configuration model is specified in
[`ccl-packages.md`](ccl-packages.md).
Remote provisioning, management sessions, activation separation, and recovery
are explored in [`remote-management.md`](remote-management.md).
Its role in constructing bounded AI-agent missions and typed tool adapters is
specified in [`agent-security.md`](agent-security.md).

## Goals

The language should:

* make authority visible in program types;
* make external effects explicit and statically checkable;
* safely evaluate untrusted or downloaded code with explicitly supplied
  authority;
* support local IPC, typed streams, configuration, secrets, and asynchronous
  operations directly;
* provide bounded execution for interactive commands and event handlers;
* support useful automated proofs of runtime safety and non-amplification of
  authority;
* serve desktop, serial recovery, and remote management sessions without VTs,
  TTYs, or terminal emulation;
* compile small scripts into independently isolated CuBit services; and
* extend across mutually authenticated CuBit nodes without making the network
  appear local or failure-free.

## Non-goals

The language is not intended to provide:

* POSIX shell compatibility;
* Unix-style byte/text pipes as the primary composition mechanism (typed value
  and stream composition, including a proposed `|>`, is a separate goal);
* ambient filesystem, process, environment, or network access;
* arbitrary native-code loading or a foreign-function interface;
* implicit conversion of secrets, descriptors, or authority into serializable
  data;
* unrestricted computation in contexts that promise bounded execution; or
* transparent distributed objects that hide latency and partial failure.

## Architecture

```text
BASIC-like interactive syntax
             |
             | desugars
             v
Lisp-like structured syntax
             |
             | elaborates and type-checks
             v
Typed core calculus
             |
             | evaluates with explicit authority and resource budgets
             v
Management broker and capability-mediated CuBit IPC
```

Presentation and transport are separate from evaluation:

```text
desktop console --+
serial recovery --+--> management session --> language evaluator
TLS management ---+                              |
                                                  v
                                           management broker
                                                  |
                                                  v
                                           CuBit services
```

The desktop console owns its window, editor, history, completion, and value
rendering. It does not own framebuffer, raw input, process-launch, or broad
system-management authority.

The management broker exposes typed operations such as service inspection,
log subscription, narrowly scoped configuration changes, and package launch.
It is the enforcement boundary even when the evaluator has already type-checked
a program.

## Surface syntax

The [interactive composition design](ccl-interactive-composition.md) connects
authorized discovery, completion/type pictures, typed `|>` pipelines, and
eventual local/distributed graph editing to one CCL semantic system. Those
features remain proposed; the extracted `CCL.Sessions` REPL foundation is now
implemented and shared by native and hosted Workbench clients.

The interactive syntax should be terse and BASIC-like:

```basic
LET failed = SERVICES WHERE STATE = FAILED

FOR service IN failed
    AWAIT RESTART service
NEXT

ON CONFIG "network/routes" CHANGED AS route
    LOG INFO, "route changed", route
END
```

The corresponding structured syntax may resemble:

```lisp
(let ((failed
       (filter services
         (lambda ((service ServiceSummary))
           (= service.state ServiceState.Failed)))))
  (for service in failed
    (wait (service.restart restart-authority service))))

(on (config.changed "network/routes")
  (lambda ((route Route))
    (log Info "route changed" route)))
```

Exact syntax is deliberately left open. The important invariant is that both
forms produce the same typed abstract syntax tree and have identical security
semantics. Interactive shorthand is syntax sugar, not a second interpreter.

### Round-trip source views (implemented for current expressions)

The two surfaces are intended to be isomorphic over the canonical CCL syntax
tree, not over source bytes. Converting Lisp to BASIC and back must preserve
the program's structure, types, authority requirements, and evaluation order.
Whitespace and aliases may normalize. Every future core expression needs a
representation in both surfaces; BASIC is not intended to be a restricted mode.
This promise is about the expression language, not yet package/system declarations.

`CCL.Language.Views` now provides the shared bounded converter/formatter.
Both input surfaces pass the existing `CCL.Language` analyzer against the
caller's explicitly visible interface catalog. Conversion does not invoke
services, install bindings, or grant authority. The resulting compact Lisp
form feeds the existing interpreter or AST-to-CCLB compiler; there is no second
evaluator. Existing bytecode limitations (including strings) still apply.

The initial BASIC notation is expression-oriented, with comma-separated calls
and immutable local bindings:

```basic
#!ccl basic
LET name = concat("Cub", "ie") IN
  concat("Hello, ", name)
END
```

The corresponding complete Lisp expression is:

```lisp
(let ((name (concat "Cub" "ie")))
  (concat "Hello, " name))
```

The BASIC view now uses infix `+`, `*`, `/`, `MOD`, and `=` and an
expression-valued conditional:

```basic
LET text = to-string(7) IN
  IF length(text) = 1 THEN
    concat("0", text)
  ELSE
    text
  END
END
```

There is no implicit mutable result or required `RETURN`: the selected branch
supplies the conditional's value, and both branches must have the same type.
The analyzer checks both branches, and host admission still checks the whole
program before any effects; evaluation executes only the chosen branch.

Multiplication, division, and modulo bind more tightly than addition;
equality binds least tightly. Operators at the same precedence associate left.
Parentheses preserve different grouping, including right-associated addition
and multiplication: the formatter must not reassociate even mathematically
associative operations because overflow, evaluation order, and fuel are
observable. Negative integer literals are supported; subtraction and new
comparison operators await corresponding core-language operations.

Earlier call spellings such as `add(20, 22)`, `mod(7, 3)`, and
`if(condition, yes, no)` remain accepted and normalize to the new notation.
Service calls retain qualified names, e.g. `clock.monotonic-ms()`.
Names are case-sensitive; uppercase `LET`, `IN`, `END`, `IF`, `THEN`, `ELSE`,
`MOD`, `FUNCTION`, `AS`, and `RETURN` are keywords. Backticks escape
keyword/operator-bearing identifiers.
Hyphens remain identifier characters (`elapsed-ms`, `to-string`), not an infix
minus sign. Quoting a name cannot embed code. Multiple simultaneous let
bindings and first-class function values remain future work. Typed top-level
`define` declarations are implemented as described below.

In the shared native/Linux Workbench source editor, **F8** (or the syntax
button beside REPL) switches views. **Shift+F8** formats the current view.
Rendering uses two-space indentation, separate let bodies and conditional
branches, and an 80-column wrapping target without splitting literals.
Formatting is idempotent. Comments retain their text/order but currently move
above the expression rather than remaining attached to individual nodes.
The `#!ccl basic` header selects BASIC when reopening a saved file.

Switching or formatting never recompiles/restarts a running VM or reloads a
watch snapshot. Source highlights remap by AST node identity while the PC,
operand stack, locals, fuel, and breakpoints remain untouched. Ordinary edits
still invalidate compiled state. Failed parsing/type checking or insufficient
output capacity leaves the original document, cursors, and undo history intact.
Successful conversion is an undoable document edit. The REPL currently remains
Lisp, explicitly labeled as such; its completion/history views need separate
integration.

Bounds are explicit: views hold 4,096 bytes; the lowered executable source
still has the core's 1,024-byte limit and existing node/nesting limits. A
conversion that cannot fit reports failure, never truncates code. Full
round-trip equivalence is the design invariant within supported resource
bounds, not a claim of unlimited source sizes or a completed formal proof.
Tests live in `tests/ccl-views`, including formatter idempotence, node-range
mapping, escaped literals/names, rejection cases, and the shared SDL Workbench.

The current structured reader uses `#` for line comments:

```lisp
# Restart only after the new certificate is available.
(on certificate.rotated
  (service.restart gateway)) # The comment continues to end of line.
```

The comment marker is provisional and may change before CCL source syntax is
versioned. Comments are source trivia: they do not enter the typed tree or CCLB,
but their characters still count toward source locations used by diagnostics
and debugger mappings.

## Value and type model

CCLB v3 currently has only `Integer` and `Boolean` runtime values. The shared
source frontend and direct interpreter additionally implement immutable
one-based `String` and `Character` values as the first variable-sized slice,
and nominal enums as the first closed-sum slice.
Host imports declare argument/result types and an authority class, but
authority is not yet a first-class value and the source checker does not yet
enforce ownership. Runtime bindings are deliberately absent from discoverable
interface descriptors and are installed only during host admission. The
following is the target type system, not a claim about the current
implementation, except where explicitly marked implemented below.

The core should remain deliberately small. Expected value classes include:

* booleans, bounded integers, text, byte strings, and durations;
* options and typed results;
* immutable records, variants, and bounded collections;
* functions and closures with explicit effects;
* tasks and typed streams;
* opaque local and remote resource references; and
* opaque authority values.

There is no universal string conversion. In particular, secrets and authority
values do not support formatting, generic equality, logging, or serialization.
The overloaded `to-string` family contains only explicitly declared, safe
conversions. A type receives no printable representation merely because it is
a CCL value; in particular, secrets and authorities have no such overload.

The initial structured string and arithmetic forms are:

```lisp
(* 6 7)
(/ milliseconds 1000)
(mod seconds 60)
(length "clock")
(at "clock" 2)                  # Character 'l'; indexes start at one
(concat "elapsed: " "01:01:01")
(to-string 42)
```

Division truncates toward zero. `mod` follows Ada's modulo semantics: a
nonzero result has the sign of the divisor. Division by zero, the
`Integer'First / -1` overflow case, multiplication overflow, invalid indexes,
and exhausted text storage are typed evaluation outcomes rather than unchecked
host exceptions.

### Implemented enums, scalar variants, and bounded type descriptions

Enums use a declaration whose members form a syntactic list:

```lisp
(type Color (enum Red Blue Green))

(define (caption (color Color)) String
  (to-string color))

(caption Color.Blue)
```

The equivalent BASIC view is:

```basic
TYPE Color = (Red, Blue, Green)

FUNCTION caption(color AS Color) AS String RETURN
  to-string(color)
END

caption(Color.Blue)
```

`(type Color (variant (Red) (Blue) (Green)))` means the same thing; the
formatter normalizes this nullary sum to `enum`. Each alternative carries
`Unit`, the empty product. `Color.Red` is a `Color`, not an Integer or a
string. A separately declared `Other.Red` has a different nominal type.
Equality accepts members of the same enum; `to-string` returns the member
name. The Workbench displays a directly returned member as `Color.Red`.
There are no ordinal casts, implicit conversions, or unqualified-member
inference yet.

Declarations belong to one source snapshot, precede their uses, and may be
interleaved with function declarations. Enums work in immutable bindings,
function parameters/results, and `if` branches. The shared interpreter is
used by both the Linux preview and native CuBit Workbench. CCLB v5 also compiles
enums, scalar-payload variants, and exhaustive matches with their nominal
identities intact. Functions and strings remain interpreter-only. Typed host
imports can carry these simple variants under schema-pinned contracts; native
Config fixtures exercise that bridge. Existing text-only UI hooks still need
explicit conversion to text. Manifest/config scalar readers do not accept
enums as integer fields.

`CCL.Types` supplies a bounded nominal registry beneath the frontend. It can
describe named products and closed sums, with references only to previously
published types. This makes layouts acyclic and calculable at declaration
time. There are at most 32 declarations, 16 components per declaration, and
256 cells in a described layout; existing source/nesting bounds also apply.
Names are at most 32 characters; a frontend enum's qualified `Type.Member`
must fit that bound. Publication is atomic: any rejected definition leaves
the registry unchanged. A registry reference is local to its source snapshot,
**not** an authenticated schema identity or a transferable authority.

The interpreter accepts persistable payload types, including strings, records
and nested variants. The bytecode VM currently supports only scalar payloads
(`Integer`, `Boolean`, or no payload):

```lisp
(type Reading (variant (Value Integer) (Unavailable) (Flag Boolean)))
(match (Reading.Value 41)
  ((Reading.Value n) (+ n 1))
  ((Reading.Unavailable) 0)
  ((Reading.Flag enabled) (if enabled 100 0)))
```

```basic
TYPE Reading = VARIANT (Value AS Integer, Unavailable, Flag AS Boolean)
MATCH Reading.Value(41)
  CASE Reading.Value(n) THEN n + 1
  CASE Reading.Unavailable THEN 0
  CASE Reading.Flag(enabled) THEN IF enabled THEN 100 ELSE 0 END
END
```

Every alternative must appear exactly once, with its declared payload bound
in that arm only. All arms return the same type; only the selected arm is
evaluated. There are no wildcard arms, implicit casts, tag-only equality for
payload sums, or partial matches. Nullary alternatives remain atoms, such as
`Reading.Unavailable`. `samples/variants.ccl` exercises both interpreter and VM.

CCLB v5 carries a bounded nominal schema snapshot and complete match tables,
plus stable schema identities for typed imports. The linker compares their
complete definitions with an explicitly authorized catalog before binding them.
`Switch_Variant` checks the nominal type and dispatches to the selected arm,
exposing only that arm's scalar payload. The verifier checks every arm and
requires identical stack types at joins. Compiler-created payload bindings
are lexical stack slots, not mutable branch-local locals. They are visible in
the debugger operand stack; named local inspection still covers ordinary lets.
General lets inside branches, function calls, strings, nested aggregates,
record construction, generics, and authority-bearing payloads are not yet
supported by this bytecode slice. Unsupported forms fail explicitly.

Interpreted records use nominal positional constructors in declaration order:

```lisp
(type Note (variant (Text String) (Absent)))
(type Settings (record (title String) (note Note) (enabled Boolean)))
(let ((settings (Settings "Dashboard" (Note.Text "Ready") true)))
  (length (field settings title)))
```

The corresponding BASIC declarations are `TYPE Note = VARIANT (Text AS String,
Absent)` and `TYPE Settings = RECORD (title AS String, note AS Note, enabled AS
Boolean)`. Construction uses `Settings("Dashboard", Note.Text("Ready"), true)`;
projection uses `field(settings, title)`. Both surfaces share the checked tree.
Empty records are permitted; omitted, extra, or mistyped constructor fields are
rejected before evaluation. Records can flow through ordinary functions and
bindings. `match` binds the actual nested payload without flattening it.

Local construction retains nominal metadata without inventing an advertised
schema key. Sending a value to a host operation requires complete nominal
correspondence with its independently approved schema and the operation's grant.
Construction/inspection use a bounded pool of 16 owned aggregate snapshots per
evaluation, shared with aggregate host results. Each native value is bounded by
256 cells and 8 KiB text. Native string views retain that full text bound;
length and indexing inspect the owned snapshot without copying its bytes.
Concatenation can construct strings up to 8 KiB, using an owned snapshot when
the result exceeds the small text region's 1 KiB bound. Literal parsing and the
scalar UI result buffer retain their existing limits.
Constructor arguments evaluate left-to-right once, after reserving the snapshot
slot. Builders that exceed a bound cannot publish a partial object.

Embedding hosts can receive whole typed values through
`Interpret_Object_With_Values`; `Interpret_Object` is the pure, grant-free form.
Both return a separate `Object_Interpretation_Result` containing an owned native
image. They require an independently approved expected binding and check its
complete nominal definition before effects, not just its digest or local ID.
The final image is validated against that binding before publication and remains
owned after temporary evaluation storage is cleared. There is no client codec.
Failure returns Has_Value=False and empty output, never a partial aggregate.

CCLB v6 can transport whole native objects across suspended imports, retain them
in locals and return them through the optional CCL.VM.Native_Objects wrapper.
This wrapper owns the bounded snapshots and exports only its waiting argument
or completed result under approved type metadata. Scalar host completion cannot
inject a native reference; normal scalar machine state stays small. Aggregate
construction, projection and general match instructions, and display through
the scalar-oriented Workbench result UI, are still pending. Ordinary scalar evaluations do not
carry an extra 16 KiB image; callers opt into the native result API explicitly.
Standalone strings can be returned through the native object API and passed to
Object_Value endpoints without first materializing in the small text region.
Text_Value endpoints enforce their own declared limits before invocation;
the scalar UI result API explicitly rejects strings exceeding 1 KiB, never
silently truncating them. Extracting a string field just retains a view into
its owning snapshot rather than consuming another text-region allocation.
No copyability, ownership, or exportability is inferred just because a layout
fits. Resource-bearing definitions remain inadmissible in this data path. The scalar
variant instructions reject moved ownership values; stack copies cannot turn a
transferred resource into unrestricted data, including through arithmetic or
reinitializing an unrestricted local.

The next slices are products/nested payloads and bounded generic instantiation. `Option<T>` and
`Result<T, E>` should be ordinary declared sums, not privileged compiler
cases. An explicit must-use obligation must prevent discarding a result;
that is distinct from authority ownership and is not enforced by this scalar
slice. Propagation shorthand follows those semantics, not the reverse.

The member declaration is syntax, not an implicitly allocated runtime list.
Planned `(members Color)` should produce a bounded homogeneous collection of
`Color` values in declaration order, not strings or integer ordinals. It
will follow the collection implementation; it is not available yet.

### Implemented typed functions (interpreter)

A program can contain typed function declarations followed by one result
expression. Declarations belong to that source snapshot, not to a persistent
global REPL namespace:

```lisp
(define (seconds (ms Integer)) Integer
  (/ ms 1000))

(define (caption (s Integer)) String
  (concat "Uptime seconds: " (to-string s)))

(ui.label-text (caption (seconds (clock.monotonic-ms))))
```

The equivalent BASIC view uses `FUNCTION seconds(ms AS Integer) AS Integer
RETURN ms / 1000 END`. `RETURN` here introduces the function's single body
expression; it is not an early-exit statement. Both views share the same typed
AST and interpreter, and their formatter/converter preserves canonical code.

The initial slice supports up to 16 uniquely named functions with 0–8 typed
parameters each. `Integer`, `Boolean`, `String`, and `Character` are available.
The body must exactly match its declared return type; there are no implicit
coercions. Functions may call earlier definitions, but not themselves or later
definitions. Nested declarations, closures, general function values, and overloads are
not implemented yet. Builtin names and qualified service names cannot be
redefined. Parameters cannot be duplicated or named as literals.

Arguments evaluate exactly once, left to right, in the caller's environment.
The body sees only its parameters and local `let` bindings, not the caller's
locals. Calls share the invocation's fuel and bounded text arena, and the
combined call/expression depth is bounded by 32. Text results remain owned by
that invocation's arena; no pointer to a returned host stack frame escapes.
Temporary text is not yet reclaimed at each function return, so text-heavy
programs can exhaust the arena even when their final result is small.

All function bodies are checked, and all referenced host operations (even in
unused definitions) are admitted against the runtime's existing grants before
any execution. Functions create no authority. Earlier effects are not rolled
back if a later call fails. Watch retains the source snapshot and starts each
evaluation with fresh invocation storage/fuel; registrations and callback
lifetimes are managed separately by the owning callback runtime.

Use F5, REPL, or F7 Watch. CCLB compilation explicitly rejects definitions/calls
until bytecode call frames and function metadata are implemented. See
`userspace/ccl/samples/function-clock-label.ccl` for a complete example.

### Typed non-capturing handlers

`(handler clicked)` references an earlier named zero-argument Boolean function.
It has a distinct handler type: it may be let-bound, selected by an `if`, and
passed to a typed handler argument such as `ui.button-on-click`. It is not a
native address or scalar ID. It cannot be a top-level result, host return value,
or an ordinary function parameter/result in this initial slice. The BASIC view
uses `handler(clicked)`; ordinary definitions still use `FUNCTION ... RETURN ... END`.

At the host boundary the reference owns bounded source and entry-name storage.
The UI owner analyzes/adopts this code once using its current catalog/grants,
then retains checked code for subsequent clicks. Code does not convey authority.
Each dispatch checks current binding identities and receives fresh fuel/storage;
ordinary IPC authority checks still apply. No captures or reentrant evaluator
calls are introduced. Closing/replacing a registration is explicit, and source
edits do not change registered code. See [CCL UI](ccl-ui.md) and
`userspace/ccl/samples/button-clock.ccl`.

### Planned overload resolution

The intended model allows CCL functions to be overloaded by their complete profile: name, parameter
types, and result type. Resolution is bidirectional. The checker first collects
candidates with the right name and arity, checks argument types, then uses an
expected result type supplied by the surrounding expression when necessary.

Expected result types can come from an annotated binding, a function return,
an enclosing operation parameter, a constrained aggregate component, or
another otherwise-resolved overload. Overloading solely on result type is
therefore legal when such a context selects exactly one candidate:

```text
parse : String -> Integer
parse : String -> Duration

LET count : Integer = parse("42")       # resolved
LET delay : Duration = parse("42ms")    # resolved
parse("42")                             # ambiguous at the REPL
```

Resolution succeeds only when exactly one complete profile remains. No match
and multiple matches are distinct static diagnostics; declaration order never
breaks a tie, and the runtime never performs overload dispatch. Interface
descriptors and CCLB linkage identify the already-resolved complete profile, so
a service cannot substitute a same-named operation with a different result
type during admission.

### Constrained and unconstrained values

CCL adopts Ada's separation between an unconstrained type and the definite
bounds of each value of that type. `String` denotes the family of immutable
strings rather than one maximum-sized buffer type. Every constructed string
nevertheless has definite, value-carried bounds:

This model follows Ada 2022's object, object-declaration, array-conversion, and
return-object semantics (RM 3.3, 3.3.1, 4.6, and 6.5). GNAT's primary and
secondary stack strategy informs the implementation, but is not observable CCL
semantics.

```text
String                    unconstrained string type
String(1 .. 32)           constrained string subtype
```

CCL source defaults strings to a lower bound of one. General array and slice
types may later preserve another positive lower bound, but bounds are never
implicit native addresses and never permit unchecked indexing.

An immutable binding with an unconstrained nominal type is constrained by its
initial value:

```text
LET label : String = Format_Time(now)
```

`label` acquires the actual bounds returned by `Format_Time`; it does not denote
a resizable buffer. A binding with an explicit constraint is accepted only when
the compiler can prove the value has the required length or the expression has
an explicit typed failure path:

```text
LET status : String(1 .. 3) = "200"
```

Verified CCL does not insert an unexpected Ada-style `Constraint_Error` for a
length that could not be established. It proves the constraint, requires the
program to handle a typed mismatch, or rejects the program.

Array assignment follows Ada's useful sliding rule: source and destination
lower bounds may differ, but each dimension must have the same length. The
source elements are placed into the destination's bounds. Rebinding constructs
a new immutable value; it never secretly resizes an existing constrained
object.

An unconstrained function result is an anonymous result object whose bounds are
determined by the returned expression. Result semantics are independent of
storage placement. The compiler may:

* construct directly into caller-provided storage when its destination and
  capacity are known;
* place a short, statically bounded result in primary VM-stack storage;
* place a variable-sized temporary in the CCL secondary region; or
* require an explicit persistent region when the value escapes its execution
  scope.

Every variable-sized operation has a static, schema-provided, or caller-supplied
maximum. “Unconstrained” therefore means that a particular length is determined
when the value is constructed, not that allocation is unbounded.

The CCL secondary region is a bounded VM facility, not the GNAT secondary
stack. Values contain checked region descriptors and bounds rather than native
pointers. Marks delimit temporary lifetimes; releasing a mark invalidates all
newer descriptors, and allocation generations prevent stale descriptors from
becoming valid when storage is reused. Secret-bearing allocations are scrubbed
on release, and the entire used region is scrubbed when an execution is torn
down.

### Products, sums, and bounded symbolic data

CCL should retain useful Lisp data semantics without making an unbounded linked
list the universal representation. Its foundational aggregate types are:

```text
Tuple<(T1, T2, ... Tn)>            fixed heterogeneous product
Record<(name: T, ...)>             named product
Variant<T1, T2, ... Tn>            closed tagged sum
List<T, Capacity>                  bounded homogeneous sequence
SExpr<Atoms, Depth, Nodes, Bytes>  bounded symbolic tree
```

A quoted heterogeneous form may elaborate to a tuple:

```lisp
'(443 true "public")
```

```text
Tuple<(Integer, Boolean, Text<6>)>
```

`first`, `rest`, and `cons` have statically determined tuple result types. A
compile-time tuple index returns its precise element type. Dynamic indexing
returns a closed variant rather than an untyped value. Pattern matching must
exhaust every variant.

Bounded `SExpr` values permit symbolic data and eventually hygienic macros, but
transformed syntax must pass the normal elaborator, type and ownership checker,
resource analysis, module decoder, and bytecode verifier before execution.

Aggregate traits are conservative. A tuple is serializable, displayable, or
copyable only when every element has that property. A tuple containing a
must-handle (linear) authority is itself must-handle. The runtime should use bounded contiguous arenas
and indexes, not cons-cell pointers or an unrestricted heap.

## Authority types

Authority is represented by opaque, unforgeable values. Operations require
authority in their type signatures rather than consulting an ambient process
identity.

Example types:

```text
Inspect<Service>
Restart<Service>
Read<Config<Network>>
Write<Config<Network>>
Use<Secret<TLSPrivateKey>>
Connect<TLSGateway>
Listen<HTTPS>
```

An operation might have the type:

```text
restart : Restart<Service> * ServiceRef
       -> Task<Result<Unit, RestartError>>
```

Code without a `Restart<Service>` value cannot type-check a call to `restart`.
Names, integers, byte strings, and network identities cannot be converted into
authority.

Owned authority values are **must-handle** (linear) by default: ownership must
be moved, returned, consumed by a declared terminal operation, or explicitly
finalized exactly once. They
cannot be implicitly copied or silently dropped. Selected observation authority
may be wrapped in an explicit `Shared<T>` type, while one-shot grants and reply
authority remain non-shareable. **Move-only** (affine) types are reserved for
values that may be abandoned safely but must never be duplicated.

Selected authority may support explicit delegation or attenuation. Attenuation
can remove rights or narrow the referenced object, but no language operation may
widen authority.

```text
admin   : Manage<Service>
monitor : Observe<Service> = restrict admin to { status, logs }
```

Whether attenuation consumes the source authority is determined by the source
type and operation, not by a universal rule.

### Locators and authority kinds

Locators (security model, "Names and locators") are typed in CCL. Each
authority kind (filesystem, network, configuration, ...) is a type family:
its locator grammar is a CCL type, declared with the service's interface
descriptor, and every name that denotes an instance of that kind has a
static type.

```text
kind Net        locator = variant { tcp (Host, Port), udp (Host, Port),
                                    tcp-listen (Address, Port) }
kind Filesystem locator = Path
kind Config     locator = record { namespace : Name, key : Name }

@net     : Authority<Net>
@system  : Authority<Filesystem>     -- a registered alias of that kind
@config  : Authority<Config>
```

- **Literals are checked at compile time.** `@net:tcp:example.org:443`
  parses by the network grammar into a value of type `Locator<Net>`;
  `@net:tcp:example.org:99999` or `@system:../etc` is a compile error, not a
  run-time string failure. The same proved splitter runs in the compiler and
  the runtime.
- **A locator is an instance of its kind's type, with typed fields.** The
  fields are ordinary CCL types with their own literal syntax and checks, so
  a locator is structured data, not a string with pieces:

  ```text
  type Address = Bytes<16>           -- IPv6; IPv4 as ::ffff:a.b.c.d
  type Host    = variant { name (Domain_Name), address (Address) }
  type Port    = range 1 .. 65535
  type Segment = Name                -- no '/', never '..' or empty
  type Path    = list<Segment>
  ```

  `@net:tcp:[2001:db8::1]:443` is `tcp (address (2001:db8::1), 443)`, and
  `@net:tcp:10.0.2.2:443` holds the same `Address` type as
  `::ffff:10.0.2.2` (the single address type: no family tag anywhere).
  Fields are read with their types (`(field target port)` is a `Port`), a
  function can take a bare `Address` or `Port`, and building a locator from
  typed parts needs no text at all. An `Address` field is checked as an
  address when compiled: `10.0.2.256` or `1::2::3` is a type error.
  Scopes use the same types: a network scope is an `Address` prefix and a
  `Port` range, so a locator is checked against a scope field by field.
- **A locator is data, never authority.** Using one needs the matching
  authority value from the program's view:

  ```text
  open  : Connect<Net> * Locator<Net> -> Task<Result<Channel, OpenError>>
  read  : Read<Filesystem> * Locator<Filesystem> -> Task<Result<Bytes, ReadError>>
  ```

  A bare `@system:fonts/a.ttf` in a call resolves the `system` authority from
  the view; the compiler requires the manifest to declare it, so an
  undeclared authority is a compile error rather than a run-time denial.
- **Aliases are typed.** A registration binds a name to one kind; passing
  `@system:...` where a `Locator<Net>` is expected does not type-check. This
  is the security model's "a filesystem alias cannot be bound to netstack",
  enforced statically where CCL is used and by the registrar everywhere.
- **Kinds compose with existing parameters.** A program's launch parameter
  may have type `Locator<Net>` (for example a server's listen address), so
  launchers get checked values and the REPL completes authorities from the
  view, then fields from the kind's grammar.
- **No string round trip.** A `Locator<K>` prints as its canonical text, but
  text becomes a locator only through the kind's parser, and never becomes
  authority.

### Ownership and borrowing

The checker maintains separate unrestricted and must-handle environments:

```text
Γ ; Δ ⊢ expression : T ! Effects ⊣ Δ'
```

`Γ` contains unrestricted bindings. `Δ` contains owned
must-handle (linear) and move-only (affine) values. Checking an expression
transforms `Δ`; every must-handle binding must have one valid disposition along
every exit path.

CCL distinguishes:

* **unrestricted** values, which may be copied or discarded;
* **move-only** (affine) values, which may be moved or explicitly discarded but
  not copied;
* **must-handle** (linear) values, which must be moved, consumed, returned, or
  finalized exactly once;
* **borrowed-ro** (shared borrow) values, which permit temporary read-only
  access without ownership;
* **borrowed-rw** (exclusive borrow) values, which permit temporary exclusive
  read/write access; and
* explicitly **shared** authority, available only when its type and host policy
  support safe duplication.

Persistent authority does not require copying. An operation can borrow it:

```text
restart : borrowed-ro<Control<Service.Restart>> * ServiceRef
       -> Task<Result<Unit, RestartError>>
```

or consume ownership while changing protocol state:

```text
commit : Transaction<Open> -> Task<Transaction<Committed>>
reply  : Reply<Pending, Response> * Response -> Reply<Sent, Response>
```

A one-shot grant is consumed without replacement:

```text
activate : ActivateOnce<Certificate> * ValidatedCertificate
        -> Task<ActiveCertificate>
```

Moves and borrows are explicit in the typed core even when surface syntax can
infer an unambiguous move. Borrowed values cannot escape their scope, be stored
in a longer-lived object, cross an asynchronous suspension unless their
lifetime permits it, or be converted into ownership.

Control-flow joins compare ownership state as well as ordinary result type. A
must-handle value cannot be consumed in only one branch:

```lisp
; invalid: grant remains owned only on the false path
(if condition (activate (move grant) certificate) skipped)
```

Tuples, variants, closures, tasks, and results preserve ownership. Destructuring
moves must-handle fields; closures become must-handle when they capture one;
and a task holding authority remains must-handle until waited on or cancelled
through an operation that accounts for that authority.

The bytecode verifier must mirror these rules. Stack and local states at every
control-flow join include ownership state, `Drop` is illegal for a must-handle
value, copy-like operations require an unrestricted type, and host imports specify
whether arguments are borrowed, moved, consumed, or returned. Verification
must establish that bytecode cannot forge, duplicate, leak, or accidentally
destroy authority even if the source compiler is faulty.

#### Disposition verbs

Every must-handle type declares the verbs that validly account for ownership.
These are typed protocol transitions, not documentation strings. `return` is
the universal ownership transfer back to a caller; other verbs are defined by
the capability, endpoint, descriptor, stream, task, or protocol type.

```text
Transaction<Open>
    ownership: must-handle (linear)
    dispositions:
        commit   -> Transaction<Committed>
        rollback -> Transaction<RolledBack>
        return   -> Transaction<Open> owned by caller

Reply<Pending<Response>>
    ownership: must-handle (linear)
    dispositions:
        send Response -> Reply<Sent>
        cancel Error  -> Reply<Cancelled>
        return        -> Reply<Pending<Response>> owned by caller

StreamWriter<Open<Item>>
    ownership: must-handle (linear)
    dispositions:
        close  -> StreamWriter<Closed>
        abort  -> StreamWriter<Aborted>
        return -> StreamWriter<Open<Item>> owned by caller

ActivateOnce<Certificate>
    ownership: must-handle (linear)
    dispositions:
        activate ValidatedCertificate -> ActiveCertificate
        return                        -> ActivateOnce<Certificate> owned by caller
```

Ordinary borrowed operations do not discharge ownership:

```text
status  : borrowed-ro<Transaction<Open>> -> TransactionStatus
append  : borrowed-rw<StreamWriter<Open<Item>>> * Item -> Result<Unit, Error>
```

Disposition metadata belongs in the same typed interface definition that
generates client bindings, service stubs, bytecode import declarations,
protocol validators, and documentation. It therefore drives:

* type checking and bytecode ownership-state transitions;
* type-specific diagnostics and suggested fixes;
* console completion showing the currently valid verbs;
* security review showing whether an operation borrows, moves, or consumes;
* generated endpoint and stream state machines; and
* UI recovery actions for abandoned interactive work.

Diagnostics should name the concrete missing disposition:

```text
Pending reply must be sent, cancelled, or returned.
Open transaction must be committed, rolled back, or returned.
Open stream writer must be closed, aborted, or returned.
```

A generic `drop` cannot satisfy a must-handle obligation. Emergency cleanup is
a declared verb such as `cancel`, `rollback`, `abort`, or `close`, with semantics
implemented and enforced by the owning service.

Kernel capabilities remain the runtime enforcement boundary:

```text
ELF manifest and installation policy  maximum process authority
session policy                        authority supplied to an isolate
CCL ownership checker and verifier    ownership-safe use within checked code
service and kernel                    runtime authorization and object validity
```

Must-handle typing does not make revocation unnecessary. Expiring or revoked
runtime objects produce typed operation failures; linearity controls aliases
and use, not the continued validity of an external resource.

### Session authority

The next native Workbench, recovery REPL, and widget-host integration steps are
tracked in [CCL workspace milestones](ccl-workspace.md). Saving Workbench source
does not introduce ambient filesystem bindings into the evaluated program.

Starting a REPL does not give evaluated code all authority held by the console,
evaluator, or authenticated user. A session receives an explicit environment
containing only the bindings granted by local policy.

Discovery is separately scoped. The parser and compiler receive an explicit,
bounded catalog snapshot. An empty snapshot reveals no service operations;
there is no ambient global catalog. A discovery grant may reveal a descriptor
without supplying a usable endpoint, while a granted-binding view contains only
the exact descriptor operations admitted for invocation. Consequently all of
these states are representable and visible:

```text
not visible       operation cannot be enumerated or named
visible           type/effect contract may be inspected and compiled against
requested         module records an unresolved descriptor-pinned import
granted           trusted host has mapped that import to a local handle
exercised         VM has submitted an invocation through that handle
```

The linkage identity is the descriptor SHA-256 digest, major/minor version, and
operation ordinal. Linking also compares the complete import contract and is
transactional: any missing grant or mismatch leaves all runtime bindings
uninstalled. Runtime-local binding numbers, capability slots, endpoints,
driver IDs, and process IDs never contribute to descriptor identity. The
canonical encoding is specified in
[`ccl-interface-descriptors.md`](ccl-interface-descriptors.md).

A downloaded script declares required types and effects, but receives no
authority merely because it was downloaded, parsed, or invoked. Authority is
provided explicitly:

```basic
RUN "watch-web.cubit" WITH LOG.READ("web"), SERVICE.RESTART("web")
```

The compiled authority and effect requirements should be recorded in the
script or service ELF manifest. The process manager compares those requirements
with installation and session policy before minting capabilities.

## Effect types

Effects describe what evaluation may do independently of whether the caller
possesses the required authority. Examples include:

```text
Clock
Entropy
Log<Info>
Connect<Service:web>
Read<Config:network>
Use<Secret:web-identity>
Spawn<Package>
```

A function signature may include an inferred effect set:

```text
checkWeb : Unit -> Task<Health>
    effects { Clock, Connect<Service:web>, Log<Info> }
```

Pure expressions have an empty effect set. IPC, time, entropy, secret use,
configuration mutation, process creation, and network access are never
implicit. Effect inference should make ordinary interactive use concise while
allowing manifests and review tools to display the complete inferred set.

## Secrets

A secret is a use-restricted object, not text:

```text
Secret<TLSPrivateKey>
```

Secret types do not implement display, logging, serialization, ordering, or
general equality. They may be passed only to operations that explicitly accept
their use:

```text
configureTLS : TLSIdentity * Use<Secret<TLSPrivateKey>>
            -> Task<Result<TLSListener, TLSError>>
```

This permits a script to configure or rotate a key without observing its key
material. The secret service remains responsible for enforcing policy at
runtime; the type system prevents accidental misuse in checked programs.

## Asynchronous execution and streams

This section describes the target language semantics. What exists today
(2026-10-04, hosted tests in `tests/ccl-streams`; no live host operation
returns a task yet):

- `Stream<T>` over session streams ([ccl-streams.md](ccl-streams.md)).
- `Task<T>`, written `(Task T)` in types. A task is a session handle like a
  stream, but it completes once, with a single result. A host operation
  declares `Result_Task` to return one, and `(task T n)` names one in
  source.
- **Semantics follow Erlang, not JavaScript or Tokio** (user decision,
  2026-10-05). There are no `async` functions, no function colouring and no
  executor:
  - A run is like a Pid, and its outcome task is like a monitor reference.
  - `(wait t)` is like `receive` for that one message: it blocks only the
    evaluation that contains it, never the console, another entry or a
    thread.
- **Looking at a task never waits.** A task value shows its state when the
  entry ran: `Task<Run_Outcome>: Running`, or `Task<Run_Outcome>: Done
  (Run_Outcome.Finished (Unix_Exit 0))`. A live card over a task re-runs
  when it completes. A run that has already ended shows `Done` at once.
- **One engine** (user decision, 2026-10-05). Every entry is analysed,
  compiled to CCLB, linked against the host's grants, verified and run on
  the VM (`CCL.Evaluation`); the tree-walking interpreter is gone. Compiling
  and verifying an entry costs about 25–30 µs on the host.
- **`(wait t) : T`:**
  - The VM suspends on the task until the host answers its `Wait_View`.
  - A REPL entry does not keep its machine between entries yet. On a pending
    task it stops with `Waiting_On_Task`. Once the task completes, the
    session runs the entry again (`CCL.Sessions.Resume_With_Values`) and
    answers the host calls of the first run from a log (`CCL.Host_Replay`),
    so no call is made twice. A definition that waits is bound when it
    resumes. Keeping the suspended machine instead is the next step.
  - The console keeps up to 4 waiting entries. An entry that made more than
    8 service calls before waiting fails rather than waiting for ever.
- **Soundness:**
  - `wait` is checked: the checker and the VM verifier both require a
    `Task<T>` operand and give `T`.
  - A result crosses as an image without its producer's schema key, and the
    reader checks its structure against `T`. A mismatch is an error
    (`Stream_Element_Mismatch`), never a wrongly typed value.
  - A handle names a stream or a task, never both. A `(task T n)` written
    over a stream's handle, or a `(stream T n)` over a task's, names
    nothing (`Stream_Unavailable`); it is neither another kind's elements
    nor a wait that cannot end.
  - Tasks are not data: a task cannot be a record field, has no stream
    views, and a stream cannot be waited on. A handle is local to its
    session, so it must not be persisted or sent.

Cancellation, must-handle tasks and `Task` results of non-host code are not
built yet. The callback dispatcher is still synchronous; its bounded
queue/lifetime model is a foundation for the later async runtime.

Asynchronous operations are part of the language rather than an external I/O
convention:

```text
Task<T>
Stream<T>
```

`wait` blocks only the evaluation that contains it, never an OS thread or a service event loop.
Streams contain typed values rather than lines of text:

```text
Stream<LogEntry>
Stream<ConfigChange<Route>>
Stream<NetworkEvent>
Stream<AudioFrame<Stereo, Hz48000>>
```

Composition preserves types. Text formatting occurs only when a presentation
client renders a value.

Every buffered stream has an explicit capacity and overflow policy. Producers
and consumers must select backpressure, cancellation, or a loss policy such as
`DropOldest`; unbounded queues are not part of the language semantics.

Calls, subscriptions and bulk streams belong to the same interface-definition
and authority model. The full [typed stream contract](typed-ipc.md#calls-and-streams-share-one-interface-model)
also specifies delivery/ordering, ownership, close/drain, cancellation, resource
budgets and transport compatibility. Element type equality alone is insufficient.
The [embedded/published exposure model](ccl-interactive-composition.md#one-interface-definition-explicit-exposure)
applies equally to calls and streams: one definition may have local and IPC
adapters, but neither publication nor in-process execution grants ambient access.

## Event-driven services

Long-lived behavior should normally be expressed as bounded event handlers,
not infinite loops:

```basic
EVERY 30 SECONDS
    LET health = AWAIT SERVICE "web".HEALTH
    IF NOT health.OK THEN
        AWAIT SERVICE "web".RESTART
    END IF
END
```

Each handler invocation is finite and independently budgeted, while the host
service remains reactive indefinitely. A script compiled as a shim service runs
in its own process with a manifest derived from its checked authority and effect
requirements. Failure or budget exhaustion is therefore isolated from both the
managed service and the language host.

## Verification and bounded execution

The language should distinguish guarantees enforced by different mechanisms.

### Type-system guarantees

The core type system should enforce:

* memory and initialization safety;
* exhaustive handling of variant values;
* separation of ordinary data, secrets, resource references, and authority;
* absence of authority forgery and implicit authority amplification;
* must-handle (linear) or move-only (affine) use of authority according to its
  ownership mode;
* ownership-compatible state at every control-flow join;
* non-escaping, lifetime-correct borrows; and
* explicit effects for externally observable operations.

### Automated proof obligations

Refinement checking and SMT-backed proofs should target:

* integer and collection bounds;
* division and other partial-operation preconditions;
* protocol-state transitions;
* user-specified preconditions, postconditions, and invariants;
* termination measures for pure functions and bounded loops; and
* preservation of selected information-flow properties.

The implementation should initially target a small decidable refinement
language rather than exposing arbitrary solver formulas as normal syntax.
SPARK, Why3, and GNATprove are natural implementation tools, but the language
semantics must not depend on one solver accepting a particular proof.

### Runtime resource bounds

Static proof cannot make unrestricted general computation both terminating and
expressive. Runtime evaluation therefore also receives explicit budgets:

* instruction or reduction fuel;
* wall-clock deadlines where a clock is available;
* heap and collection limits;
* call-depth limits;
* IPC and outstanding-task limits;
* stream-buffer and output limits; and
* cancellation authority.

Loops must iterate over statically bounded ranges, structurally finite values,
or explicitly consume fuel. Budget exhaustion produces a typed failure and
cannot grant additional authority.

Debugger execution uses the same VM state and resource budget as normal
execution. A bounded instruction slice may return `Paused` while preserving the
program counter, operand stack, ownership environment, import lifecycle, and
remaining fuel. `Stopped` is an explicit terminal state and consumes no further
instruction or fuel. Step Into and Step Over are identical until the bytecode
gains call-frame semantics; the UI must not imply otherwise. Read-only machine
snapshots expose only bounded debugger metadata and cannot mutate execution.
The richer inspection snapshot copies the operand stack in top-first order and
copies declared locals with their scalar value, value kind, ownership type,
ownership mode, availability state, and active read/write borrows. It also
reports the pending import lifecycle without exposing the private lifecycle or
ownership environments themselves. Consumers receive values, not references
or mutable handles into VM state. Inspection is therefore observational and
cannot become an alternate execution or authority path.

CCL debug maps are optional, bounded, non-authoritative metadata. Each entry
maps a half-open bytecode range to an AST node and half-open source range;
nested entries are expected, and debugger lookup chooses the smallest matching
PC range. Debug-map validation is independent of VM verification. Invalid or
stripped metadata disables source-level debugging but cannot make executable
bytecode valid or invalid, alter execution, or grant authority. The initial
representation travels in memory beside the compiled program. A later CCLB
revision may encode it as an optional, separately validated section that
production packages can strip.

Workbench breakpoints are debugger state, not bytecode mutations. They are
checked against the snapshot PC before instruction dispatch. Resuming from a
breakpoint ignores that one current-PC match exactly once, preventing an
immediate retrigger without suppressing later visits. Source-level Step Over
advances in bounded instruction slices until the active debug range is exited;
terminal states, host waits, and intervening breakpoints always take priority.

Lexical locals are distinct from host-injected locals. `Initialize_Local`
consumes one checked operand-stack value, initializes exactly one previously
undeclared dynamic local, and makes that binding available to subsequent
copy/move/borrow operations. The stack verifier checks its scalar kind, the
ownership verifier checks declaration order and branch joins, and the runtime
defensively repeats both checks. The initial implementation permits only
unrestricted lexical values; authority-bearing move-only and must-handle locals
need explicit scope-exit semantics before admission. CCLB v3 records the exact
dynamic-local count so compiler-created locals survive serialization.

## Network IPC

SPARKTLS gateways will allow typed CuBit IPC between mutually authenticated
nodes. Mutual TLS establishes peer identity and channel security; it does not
itself grant application authority.

Local descriptors and capabilities are never serialized directly. Export is an
explicit, policy-controlled operation:

```text
local authority
      |
      | explicit export and attenuation
      v
gateway-managed remote reference
      |
      | bound to an authenticated TLS session
      v
remote gateway proxy
      |
      | local policy mints session authority
      v
target service
```

The type system distinguishes local and remote references:

```text
Local<ServiceRef>
Remote<Node, ServiceRef>
Exportable<ServiceSummary>
```

Only explicitly exportable data may cross a node boundary. Secrets, raw memory
grants, local descriptors, framebuffer surfaces, and reply capabilities are
non-exportable unless a separate narrowly defined protocol provides safe use
semantics.

Remote calls expose latency, cancellation, authentication state, and partial
failure in their types:

```text
call : Remote<Node, Endpoint<Request, Response>> * Request
    -> Task<Result<Response, NetworkCallError>>
```

Interface definitions should eventually generate local IPC bindings, gateway
codecs, client and server stubs, contracts, protocol-state validators, and fuzz
targets from the same source description.

## Near-term implementation plan

Completed foundations include the bounded scalar interpreter, Lisp-like reader,
typed host imports, canonical `.cclb` format, resumable VM, asynchronous CuBit
IPC adapter, fixed multi-isolate scheduler, and a SPARK ownership-state engine
with native fixtures. An ownership-aware bytecode verifier now exercises typed
locals, moves, borrows, dispositions, scope exit, and control-flow joins as a
proved layer. The primary VM verifier now requires that ownership pass, and the
runtime defensively mirrors accepted transitions. `.cclb` version 3 now carries
the bounded type, disposition, local, and portable-import tables, while host injection
must exactly satisfy those declarations. The VM now also supports bounded
paused execution, read-only debugger snapshots, explicit stop, and unrestricted
lexical-local initialization. The shared frontend now produces one opaque,
bounded typed AST consumed by direct interpretation and CCLB compilation;
callers cannot forge a successful analysis around a caller-built tree. The
initial compiler lowers scalar literals, integer addition and equality, and
boolean negation, forward-only conditional control flow, and unrestricted
lexical locals with shadowing. Its output must still pass the independent VM
verifier before execution. A local introduced inside only one conditional
branch is currently rejected because bytecode has no scope-exit operation that
can make the ownership states at that join identical.

The source frontend now resolves otherwise-unknown operators through an
explicit bounded interface catalog. The language core contains no clock,
service, driver, or host-binding identifiers. Compilation leaves imports
unresolved and emits an in-memory linkage sidecar pinned to descriptor digest,
version, operation ordinal, and type/effect contract. A separate granted-binding
view performs all-or-nothing host admission. The Linux Workbench's monotonic
clock is the first adapter using this path; catalog visibility alone is covered
by a negative test proving that it cannot become invocation authority.

The direct interpreter now also exposes a generic, statically instantiated
`Interpret_With_Host` adapter. It analyzes the complete expression and admits
every host operation against the same exact granted-binding lookup used by
VM linkage before any host call. Evaluation invokes only reached branches;
it does not rewrite source, substitute clock constants, eagerly evaluate all
imports, or replay the program to service a request. Arguments/results remain
typed scalars, and a failed call or wrong result kind produces an explicit
evaluation error. The host-free interpreter remains host-free.

This first path supports only synchronous scalar-copy contracts; moves,
borrows, cancellation and disposition protocols are rejected, not silently
treated as ordinary callbacks. Fuel bounds interpreter computation, not a
native IPC service's response time. Async suspension and deadlines remain a
separate runtime task. The native Observatory control app installs only its
manifest-granted clock endpoint and reads it afresh when execution reaches
`(clock.monotonic-ms)`. Descriptor metadata remains bundled, not authenticated
live discovery. `userspace/ccl/samples/monotonic-clock.ccl` samples once and
formats that result as HH:MM:SS; each Evaluate performs a new invocation.

The next implementation order is:

After the current kernel IPC hardening tranche, CCL resumes with debugger stack
inspection in the Workbench. The next authority-bearing runtime milestone is
then descriptor-pinned service discovery and binding through kernel-enforced
handles; CCL must not grow a private PID-addressed compatibility path.

The ownership bytecode verifier also models an asynchronous local import as one
abstract operation. Copy requires an unrestricted value. Move checks both the
success and failure disposition verbs and requires their resulting ownership
states to join. Borrowed-ro and borrowed-rw open their respective borrow during
the operation and return it on both terminal paths before continuation. This
layer is deliberately ahead of the executable VM: the VM still needs an
explicit submission-accepted transition so local enqueue failure cannot be
confused with a remote failure completion.

That runtime lifecycle is now implemented as a standalone SPARK state machine:
idle, offered, accepted, cancellation-requested, and completed. Operations
declare one of three cancellation policies: not-cancellable, best-effort, or a
guaranteed cancellation request. “Guaranteed” means the service promises to
produce a terminal cancellation outcome; it does not mean the caller may
release ownership immediately. In every policy, a moved value or active borrow
remains unavailable until exactly one terminal completion. Local submission
rejection returns to idle without changing ownership, and duplicate completion
is rejected.

The lifecycle is now embedded in `CCL.VM.Machine_State` for non-cancellable
owned imports. `Invoke_Import` produces an offer without transferring early;
the host adapter must explicitly acknowledge enqueue acceptance before the VM
activates the move or borrow. Completion applies the success/failure outcome and
only then resumes bytecode. Cancellable imports remain rejected by VM admission
until the cancellation branch is included in the ownership join. `.cclb` v3
preserves the complete owned-import contract even when current VM admission
rejects a cancellation policy it cannot yet enforce.

1. Validate descriptor hashes before publication and bind admitted imports to
   kernel-enforced handles rather than numeric host test bindings.
2. Add bounded heterogeneous tuples and closed variants, carrying ownership
   modes compositionally.
3. Add typed host-import parameter modes and enforce move/borrow transfer at
   the host boundary.
4. Add explicit lexical scope exit to bytecode ownership state so branch-local
   unrestricted bindings can converge safely, then extend source lowering to
   typed host imports and ownership-aware values.
5. Add a minimal BASIC-like syntax that desugars to the checked core.
6. Retired: the embedded CuBASIC prototype was removed in favor of the native
   CCL Workbench editor and REPL.
7. Introduce a management broker with typed, narrowly scoped operations and
   derive manifest requirements from checked authority and effects.
8. Add bounded collections, cancellation, and typed streams.
9. Add serial-recovery and SPARKTLS management front ends using the same session
   protocol.
10. Compile event-driven scripts into isolated shim services.
11. Add refinement types and SMT-backed proof obligations incrementally after
    the core semantics and interpreter are stable.

## Open questions

* Final language and product names.
* The smallest useful core type system and refinement language.
* Which authorities, if any, qualify for explicit `Shared<T>` and which policy
  may construct such values.
* Whether explicit disposition verbs are sufficient for emergency abandonment
  of must-handle resources, or selected resource types must remain move-only.
* How polymorphism is restricted to keep inference and verification tractable.
* Whether macros are permitted, and at which checked representation they run.
* The exact relationship between source scripts, checked intermediate forms,
  proof artifacts, and ELF manifests.
* Persistence and upgrade semantics for long-running event handlers.
* Revocation and expiry semantics for local sessions and exported remote
  references.
* Which information-flow guarantees can be enforced automatically without
  making routine console use burdensome.
