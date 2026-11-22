# The CCL type system: formal specification

Status: **normative target**, version 0.1 (2026-10-02). This document defines the
type system CCL is meant to have. The implementation is measured against it.
Section 13 records where today's code conforms, where it falls short and where
it diverges. Where code and this document disagree, the code has a bug or a
missing feature, unless the conformance table records an accepted divergence.

The words MUST, MUST NOT, SHOULD and MAY are used as in RFC 2119.

## 1. Goals

CCL is the glue language of CuBit: startup profiles, manifests, the console, live
cells, typed IPC and agent workflows are all CCL. The type system serves
**power, performance, protection**:

- **Protection:**
  - Authority is never ambient. Every effect on the outside world is named in a type, and admitted against grants before anything runs.
  - Owned resources are neither duplicated nor silently lost.
  - Values crossing a process boundary are checked against a nominal contract.
- **Performance:**
  - Checking is decidable and bounded (linear in program size up to fixed limits).
  - Evaluation is total and fuel-bounded.
  - Types erase to a compact verified bytecode that a small VM runs without re-deriving them.
- **Power:**
  - Records, variants, lists, streams and first-class functions.
  - Named arguments and defaults.
  - Typed host interfaces.
  - A staged path to refinement and dependent types over decidable logics (section 10).

The type system has three interlocking parts:

1. A **nominal, first-order core** with bounded parametric built-ins (sections 3–6).
2. A **substructural layer** with unrestricted, affine and linear modes, borrows and typestate (section 7).
3. An **effect layer** that makes host authority explicit (section 8).

On top of these sits a staged **refinement and dependent layer** (section 10).
Every well-typed source program compiles to bytecode that the CCLB verifier
accepts, and the two semantics agree (section 11).

## 2. Notation

| Symbol | Meaning |
| --- | --- |
| `Σ` | The **signature**: declared types, field defaults, resource policies, visible functions. Built in program order; a declaration sees only earlier ones. |
| `Θ` | The **interface catalog**: host operations visible to the program (discovery, not authority). |
| `Γ` | The **unrestricted context**: `x : τ` for bindings of mode `U`. |
| `Δ` | The **substructural context**: `x : τ @ s` for bindings of mode `A` or `L`, where `s` is a state (section 7). |
| `ε` | An **effect**: a finite set of host operations `iface.op`. |
| `G` | **Grants**: the operations the running program's manifest and system grant. |
| `τ, σ` | Types. `ρ` ranges over range types, `R` over records, `V` over variants, `K` over resource types. |
| `m` | A **mode**, `m ∈ {U, A, L}`, ordered `U < A < L`. |

The principal judgment is

```
Σ; Θ; Γ; Δ ⊢ e : τ ! ε ⊣ Δ'
```

It reads: under signature `Σ` and catalog `Θ`, with unrestricted bindings `Γ` and
substructural bindings in state `Δ`, expression `e` has type `τ`, may perform
the host operations `ε`, and leaves the substructural bindings in state `Δ'`.
When `Δ` is irrelevant, it is written `Σ; Θ; Γ ⊢ e : τ ! ε`. When `ε` is empty,
`! ∅` is omitted.

Rules are written

```
premise₁    premise₂
──────────────────── (Rule-Name)
conclusion
```

## 3. Types

### 3.1 Grammar

```
τ ::= Integer | Boolean | String | Character | Unit           base types
    | ρ                                                       range type      (type ρ (range lo hi))
    | R                                                       record          (type R (record (f τ [d]) ...))
    | V                                                       variant / enum  (type V (variant (A [τ]) ...))
    | List τ                                                  finite list
    | Stream τ                                                live stream handle
    | (τ₁ ... τₙ) → τ                                        function, n ≤ 8
    | K⟨σ⟩                                                    resource family instance (e.g. ConfigCollection⟨Preferences⟩)
    | K@S                                                     resource in typestate S (section 7.4)
    | Handler                                                 second-class callback reference
```

`Unit` is the empty record and has exactly one value. An enumeration is a
variant all of whose alternatives have payload `Unit`.

### 3.2 Identity and equality

Type equality `τ ≡ σ` is:

- **Nominal** for ranges, records, variants and resource families. Two declarations are the same type only if they are the same declaration. Across registries, two declarations correspond (`Σ₁ ⊢ τ ≅ σ ⊣ Σ₂`) iff all of the following hold:
  - the same name;
  - the same shape;
  - the same ordered component names;
  - pairwise-corresponding component types;
  - for ranges, **equal bounds**;
  - for records, **equal field defaults**.
- **Structural** for `List`, `Stream` and function types: `List τ ≡ List σ` iff `τ ≡ σ`, and likewise for the others.
- Generated spellings (`List-Integer`, `Fn12`, `Stream-Integer`) are not type names. A program MUST NOT write them. They exist only inside registries.

A **schema key** identifies a persistable type for typed IPC and storage. It
MUST be a cryptographic digest (SHA-256) of the type's canonical descriptor,
which includes names, shapes, bounds and defaults. Two endpoints agree on a
value's meaning iff their keys are equal and their roots correspond.

### 3.3 Well-formed signatures

`⊢ Σ ok` holds when every declaration in `Σ` meets these rules:

- Its name is fresh in `Σ` (builtins, catalog-imported types and generated specializations included) and is not a reserved word or a visible function name.
- Every component type is earlier in `Σ`, with one exception: a record field or variant payload MAY be `List Self`, the only recursion allowed.
- Every component type is **Storable** (section 4) and not a `Stream`.
- A range has `lo ≤ hi`. A variant has 1 to 16 alternatives, and a record has 0 to 16 fields.
- Every field default `d` is a **constant of its field's type** (section 5.4).
- The layout of every declared type fits the value-cell bound (256 cells).

## 4. Kinds: what a value may do

Each type is classified by a set of kinds. Kinds are predicates on types, and
each kind is closed under the type formers that preserve it.

| Kind | Meaning | Definition |
| --- | --- | --- |
| `Data` | May be a record field, payload or list element. | Base types except `Handler`, ranges, records and variants whose components are `Data`, and `List τ` for `τ` `Data` and not a list. |
| `Persistable` | May be stored, serialized or sent over IPC under a schema key. | `Data` and not containing `Stream`, functions, resources or `Handler`. Equal to `Data` today; they diverge once records may hold resources (7.6). |
| `Storable` | May be held by one evaluation. | `Persistable`, plus `Stream τ` for persistable `τ`. |
| `Comparable` | Has decidable equality `=`. | Integer, ranges, Boolean, String, Character, enumerations, and records/variants/lists of comparable types (structural equality). |
| `Ordered` | Supports `< <= > >=` and `sort`. | Integer, ranges, String, Character, enumerations (declaration order). |
| `Printable` | Supports `to-string`. | Every `Data` type. Records print in canonical CCL syntax. |
| `Capturable` | May be captured by a closure. | Every type of mode `U` (7.1). A closure capturing an `A`/`L` value takes that mode (7.5). |
| `Exportable` | May be the root result of a program. | `Storable`, or a resource type (moved out to the host). Not `Handler`, not `Unit` unless the host asks for it. |

Today's `List-element` restriction (no lists of ranges) is a quirk to fix:
lists of ranges are `Data` (Q-6).

## 5. Static semantics of the core

### 5.1 Subsumption and coercion

There is exactly one implicit relationship, between a range and its base type.

**Widening is free and silent.** A range value is an Integer value:

```
Γ ⊢ e : ρ      ρ = range lo hi
────────────────────────────── (T-Widen)
Γ ⊢ e : Integer
```

**Narrowing is checked.** An Integer may fill a position declared `ρ`. The
position elaborates to a checked coercion `⌈e⌉ρ`, which is static when `e` is a
constant and a run-time `Range_Error` otherwise:

```
Γ ⊢ e : Integer      position expects ρ
──────────────────────────────────────── (T-Narrow)
Γ ⊢ ⌈e⌉ρ : ρ
        constant(e) ⇒ lo ≤ ⟦e⟧ ≤ hi      (else static error Value_Out_Of_Range)
```

**Positions.** The positions that may narrow are:

- record fields and variant payloads;
- function arguments, both named calls and calls through function values, uniformly;
- function results;
- host arguments typed with a range;
- list elements of `List ρ`;
- the branches of `if` and `match` when the expected type is `ρ`.

**Bidirectional checking.** The checker works bidirectionally. Expected types
flow inward (into branches, list elements and lambda parameters). Synthesized
types flow outward, so a range-typed variable read in an Integer context widens
by T-Widen. Every binding form treats a parameter of type `ρ` as `ρ`, so range
parameters of `define` and `fn` behave the same (Q-1, Q-2). Arithmetic
yields `Integer` and never `ρ`.

No other implicit conversions exist. Character, enumerations and Boolean do
not convert to Integer.

### 5.2 Expressions

```
───────────────── (T-Int)       ───────────────── (T-Bool)      ───────────────── (T-Str)
Γ ⊢ n : Integer                 Γ ⊢ b : Boolean                 Γ ⊢ "s" : String

(x : τ) ∈ Γ                                    V ∈ Σ   A ∈ alts(V)   payload(V, A) = Unit
──────────── (T-Var)                           ───────────────────────────────────────── (T-Member)
Γ ⊢ x : τ                                      Γ ⊢ V.A : V

Γ ⊢ e₁ : τ₁ ! ε₁ ⊣ Δ₁    Γ, x : τ₁ ⊢ e₂ : τ₂ ! ε₂ ⊣ Δ₂        (τ₁ of mode U; for A/L see 7.2)
──────────────────────────────────────────────────────────── (T-Let)
Γ ⊢ (let ((x e₁)) e₂) : τ₂ ! ε₁ ∪ ε₂ ⊣ Δ₂

Γ ⊢ c : Boolean ! ε₀ ⊣ Δ₀   Γ ⊢ e₁ ⇐ τ ! ε₁ ⊣ Δ₁   Γ ⊢ e₂ ⇐ τ ! ε₂ ⊣ Δ₂   Δ₁ ⊔ Δ₂ = Δ'
──────────────────────────────────────────────────────────────────────────────────── (T-If)
Γ ⊢ (if c e₁ e₂) : τ ! ε₀ ∪ ε₁ ∪ ε₂ ⊣ Δ'

Γ ⊢ e₁ : Integer   Γ ⊢ e₂ : Integer   ⊕ ∈ {+ - * / %}         Γ ⊢ e₁ : τ   Γ ⊢ e₂ : τ   Ordered(τ)
──────────────────────────────────────── (T-Arith)            ────────────────────────────────────── (T-Order)
Γ ⊢ (⊕ e₁ e₂) : Integer                                       Γ ⊢ (< e₁ e₂) : Boolean

Γ ⊢ e₁ : τ   Γ ⊢ e₂ : τ   Comparable(τ)
─────────────────────────────────────── (T-Eq)
Γ ⊢ (= e₁ e₂) : Boolean
```

Overflow and division by zero are run-time errors, not type errors (section 9).

### 5.3 Records, named arguments and defaults

Named association and defaults are **elaboration**. They are resolved before
typing into a positional construction:

```
R ∈ Σ   fields(R) = f₁:τ₁ … fₙ:τₙ   args = p₁ … pₖ, (g₁ => a₁) … (gⱼ => aⱼ)
  {g} ⊆ {f}, distinct, disjoint from f₁…fₖ    every fᵢ not given has default dᵢ
──────────────────────────────────────────────────────────────────────────── (E-Construct)
(R args) ⇝ (R e₁ … eₙ)    eᵢ = pᵢ (i ≤ k) | aₗ (fᵢ = gₗ) | dᵢ (otherwise)

Γ ⊢ eᵢ ⇐ τᵢ ! εᵢ (1 ≤ i ≤ n)
───────────────────────────── (T-Record)
Γ ⊢ (R e₁ … eₙ) : R ! ⋃ εᵢ

Γ ⊢ e : R    (f : τ) ∈ fields(R)
──────────────────────────────── (T-Field)
Γ ⊢ (field e f) : τ
```

**Elaboration errors** (static):

- `Unknown_Field_Argument` — a named field that `R` does not have.
- `Repeated_Field_Argument` — a field given twice, by position or by name.
- `Positional_After_Named` — a positional value after a named one.
- `Missing_Field_Argument` — a field that has no default and is not given.

Elaboration order is the declaration's field order. Evaluation order is
left-to-right in **source** order, which matters only for effects (section 8).

### 5.4 Constants

A default, and every argument to a type-level index (section 10), is a
**constant**, which needs no evaluation:

- an integer literal;
- `true` or `false`;
- a string literal;
- an enum member `V.A`;
- `[]` at a list type;
- a record whose components are all constants.

String and record defaults are part of the target; the implementation admits
integers, Booleans, members and `[]` (C-5).

### 5.5 Variants and match

```
V ∈ Σ   payload(V, A) = τ ≠ Unit   Γ ⊢ e ⇐ τ
───────────────────────────────────────────── (T-Variant)
Γ ⊢ (V.A e) : V

Γ ⊢ e : V ! ε₀ ⊣ Δ₀    {A₁ … Aₙ} = alts(V) (exhaustive, no duplicates)
Γ, xᵢ : payload(V, Aᵢ) ⊢ bᵢ ⇐ τ ! εᵢ ⊣ Δᵢ    Δ₁ ⊔ … ⊔ Δₙ = Δ'
──────────────────────────────────────────────────────────────── (T-Match)
Γ ⊢ (match e ((V.A₁ x₁) b₁) … ((V.Aₙ xₙ) bₙ)) : τ ! ε₀ ∪ ⋃ εᵢ ⊣ Δ'
```

Nullary alternatives bind nothing. A future extension adds a wildcard arm
`(_ b)` covering the remaining alternatives; exhaustiveness stays mandatory.

### 5.6 Lists and built-ins

`[e₁ … eₙ]` checks every element against the expected element type. Without an
expected type, it synthesizes the first element's type, and n = 0 needs an
expected type or `(list-of τ)`. Element types MUST be `Data` and not a list.

Built-ins are **typed constants with rank-1 polymorphic schemes**, instantiated
at each use. User-defined polymorphism is not part of this version. Schemes
quantify over type variables `α, β`, optionally with a kind bound.

```
each   : ∀α β. (α → β) × List α → List β          where   : ∀α. (α → Boolean) × List α → List α
any, all : ∀α. (α → Boolean) × List α → Boolean   count   : ∀α. (α → Boolean) × List α → Integer
fold   : ∀α β. (β α → β) × β × List α → β         sum, min, max : List Integer → Integer
sort   : ∀α:Ordered. List α → List α              sort-by : ∀α β:Ordered. (α → β) × List α → List α
first, last, skip : ∀α. Integer × List α → List α (also String → String)
contains : ∀α:Comparable. α × List α → Boolean    length : ∀α. List α → Integer (also String)
at     : ∀α. List α × Integer → α (String → Character)
range  : Integer × Integer → List Integer          … (the text built-ins are monomorphic)
```

Lambda parameters without annotations are inferred only from the scheme's
expected argument type (checking mode). Inference never generalizes.

### 5.7 Functions

```
fresh f    Σ ⊢ τᵢ type, τ type    xᵢ : τᵢ ⊢ b ⇐ τ ! ε      (no outer Γ: defines are closed)
──────────────────────────────────────────────────────── (T-Define)
Σ, f : (τ₁ … τₙ) →^ε τ ⊢ program-rest

Γ ⊢ f : (τ₁ … τₙ) →^ε τ    Γ ⊢ aᵢ ⇐ τᵢ ! εᵢ
───────────────────────────────────────────── (T-App)
Γ ⊢ (f a₁ … aₙ) : τ ! ε ∪ ⋃ εᵢ

Γ, xᵢ : τᵢ ⊢ b : τ ! ε ⊣ Δ_b    captures(b) ⊆ Γ ∪ Δ    m = max mode of captured values
──────────────────────────────────────────────────────────────────────────────── (T-Lambda)
Γ ⊢ (fn ((x₁ τ₁) …) b) : (τ₁ … τₙ) →^ε τ  @ m
```

Function types carry their **latent effect** `ε` (section 8).

**Termination.** A function is visible only after its own body is checked.
Recursion and mutual recursion are therefore impossible, and the call graph is
acyclic. Together with the bounded built-ins, every evaluation terminates.
Fuel bounds its cost.

**Bounds.** At most 8 parameters, 16 functions, 4 captures, 32 bindings and
nesting depth 32. These are part of the static semantics: exceeding one is a
static error, never a run-time one.

**Handler.** `(handler f)` with `f : () → Boolean` has type `Handler`, which is
**second-class**: it MAY be passed directly to a host operation that expects a
handler and MUST NOT be bound by `let` or stored, captured, returned or exported.
This rule applies at every depth (Q-4).

### 5.8 Streams

`Stream τ` (with `τ` `Persistable`, not `Unit`) is a **session-scoped handle**
to a live source. It is `Storable` but not `Data`: it MAY be bound and passed,
and MUST NOT be a field, payload, list element or capture.

```
(stream τ n)       : Stream τ          n a constant in 1..Maximum_Handle
(latest s)         : τ                 (may wait; may fail Stream_Empty)
(window n s)       : List τ            n : Integer; result length ≤ min(n, Maximum_Window)
(arrived s), (lost s) : Integer
```

A stream handle is unrestricted. Its safety is temporal, not substructural:
every read is checked against the handle's generation and the element type
(`Stream_Unavailable`, `Stream_Element_Mismatch`). Section 10 refines the type
of `window` to `List≤n τ`.

## 6. Interfaces and host values

The catalog `Θ` maps each operation name to a **contract**:

```
Θ(iface.op) = ⟨receiver: K? via t,  argument: τ_a?,  result: τ_r [stream],  dispositions⟩
t ∈ {copy, move, borrow-ro, borrow-rw}
```

- Argument and result types are `Persistable` types identified by schema key, scalars, or resource types.
- A result marked `stream` has type `Stream τ_r`.
- Catalog visibility is **discovery only**: it decides whether a name resolves and what it means, never whether it may run.

```
Θ(i.op) = ⟨r : K via t, a : τ_a, τ_r⟩    Δ ⊢ r : K @ Available ⇝_t Δ₁    Γ ⊢ e ⇐ τ_a ! ε
─────────────────────────────────────────────────────────────────────────────────────── (T-Host)
Γ; Δ ⊢ (i.op r e) : τ_r ! ε ∪ {i.op} ⊣ Δ₂
        where Δ₂ applies the contract's success and failure dispositions to r; both outcomes
        must reach the same Δ₂ (7.3)
```

## 7. The substructural layer

### 7.1 Modes

Every type has a mode `m(τ)`:

| Mode | Structural rules | Meaning |
| --- | --- | --- |
| `U`, unrestricted | weakening and contraction | Data, streams, functions capturing only `U` values. Copy freely, drop freely. |
| `A`, affine (move-only) | weakening, no contraction | Never duplicated. May be abandoned: dropping it is safe for the resource's owner. |
| `L`, linear (must-handle) | neither | Never duplicated, never silently lost. MUST be discharged exactly once, by a declared disposition or by being returned to the host. |

There is deliberately no *relevant* mode (copyable but must-use).

**Where modes come from.** A resource type's mode comes from its **approved
policy** in `Σ` and is never `U`. All other base and data types are `U`. A
compound type's mode is the maximum of its components' modes (records,
variants, closures), which is the `Combine` lattice. Resources inside records
are a staged extension (7.6).

### 7.2 Contexts and states

`Δ` binds each `A`/`L` variable to a type and a state:

```
s ::= Available | Moved | Handled | Discarded | Borrowed_RO(k) | Borrowed_RW
```

**Name use is a move.** The use of a variable `x : τ @ Available` with `m(τ) ≠ U`
moves it:

```
Δ(x) = τ @ Available    m(τ) ∈ {A, L}
──────────────────────────────────── (T-Move)
Γ; Δ ⊢ x : τ ⊣ Δ[x ↦ Moved]
```

**Using a moved value is an error.** Any use of `x` in state `Moved`, `Handled`
or `Discarded` is a static error (use after move).

**Weakening applies to `A` only.** At the end of `x`'s scope:

- `A` in `Available` is permitted. Elaboration inserts an explicit `drop x`, so the bytecode stays explicit.
- `L` in `Available` is a static error (`Outstanding_Must_Handle`).
- Any outstanding borrow is a static error.

**Joins.** `Δ₁ ⊔ Δ₂` is defined iff every binding is in the same state in both
branches, after inserting `drop` for `A` bindings that are `Available` in one
branch and `Discarded` in the other. Otherwise it is
`Branch_Ownership_Mismatch`. Linear obligations must therefore be discharged
uniformly on all paths.

### 7.3 Dispositions and host transfer

A resource policy declares **verbs**, each with an effect on the binding:

- `consume` (the resource is destroyed: `Handled`);
- `transfer` (ownership leaves the program: `Handled`, recorded as transferred for the effect log);
- `transition V'` (typestate: the binding stays `Available` at a new type, 7.4).

`consume` and `transfer` are distinct in the effect trace, though equal for
discharge (O-2). A generic `drop` never discharges a linear value.

**Host transfer modes:**

- `copy` is forbidden for `A`/`L` arguments.
- `move` takes `Available → Moved` at **acceptance**. Rejection leaves `Δ` unchanged, and completion applies the success or failure disposition.
- `borrow-ro` and `borrow-rw` open and close a borrow around the call.

**Static rule:** both outcomes of a `move` call must produce the same `Δ` (7.2's
join).

### 7.4 Typestate

`K@S` indexes a resource by a protocol state, for example
`Transaction@Open --commit--> Transaction@Committed`. A `transition` verb rebinds
`x : K@S` to `x : K@S'` in `Δ`, so operations requiring `K@S'` are well-typed only
after it. Typestate is a finite, policy-declared state machine. It is the first
and simplest dependent construction (section 10).

### 7.5 Borrows and closures

**Borrows are second-class and lexically scoped.** `(borrow x b)` (RO) and
`(borrow-mut x b)` (RW) make `x` usable inside `b` without moving it:

- The borrowed reference MUST NOT escape `b`: it may not be returned, stored or captured.
- RO borrows are shared, up to 8.
- An RW borrow is exclusive: no other borrow and no use of `x` while it is open.
- Host calls with `borrow` transfer are the built-in instance of this rule.

**Closures** capturing an `A`/`L` value take the maximum mode. A linear closure
must be called (discharging its captures) or returned. Today only `U` values
may be captured, which is the conservative subset.

### 7.6 Staged: resources in data

A record may contain resource fields. Its mode is the maximum of its fields'
modes. Destructuring (`match` or field projection on an `A`/`L` record)
**moves** the record and binds its owned fields. Such a record is `Storable` in
an evaluation but never `Persistable`.

## 8. The effect layer: authority as types

Every expression has a latent effect `ε ⊆ Ops`: the set of host operations it
may invoke. Function types carry the effect of their body. Effects compose by
union (rules in section 5).

```
⊢ program : τ ! ε      ε ⊆ G
───────────────────────────── (Admit)
program may run under grants G
```

**Admission** is a static judgment over the whole program, after typing and
before evaluation. A program whose effect is not covered by its grants is
refused **before anything runs**, with an explanation naming each missing
operation and the manifest request that would grant it (`CuBit.Failures`).

**Consequences:**

- No ambient authority: an operation not in `ε` cannot be reached.
- **Least privilege by construction:** `ε` is exactly what a program needs. Manifests SHOULD be checked against the effects of the programs they launch, and a tool can derive the minimal manifest from `ε`.
- **Effect-polymorphic built-ins:** `each : ∀α β ε. (α →^ε β) × List α → List β ! ε`.
- **Exact or over-approximated:** effects are exact for first-order code and a safe over-approximation through function values. The latent effect of a value is part of its type.

Grants are not types. Admission is the boundary between the static world (what
the program could do) and the system's decision (what it may do).

## 9. Dynamic semantics (summary)

Evaluation is a big-step, fuel-indexed relation:

```
⟨e, η, Δ, φ⟩ ⇓ ⟨v, Δ', φ'⟩
```

Here `η` maps variables to values and `φ` is the remaining fuel. Every step
costs fuel, built-ins cost per element, and `φ' ≤ φ`. The outcome is a value
or one of a closed set of **checked run-time errors**:

- `Overflow`, `Division_By_Zero`, `Index_Error`, `Range_Error`;
- `Stream_Empty`, `Stream_Unavailable`, `Stream_Element_Mismatch`;
- `Host_Failed(f)`, carrying a `CuBit.Failures.Failure`;
- `Fuel_Exhausted`.

Stuck states do not exist (section 12, T1).

## 10. Refinement and dependent types (staged)

Dependency is added in stages. At every stage the predicates belong to a
**decidable logic**, so type checking stays decidable and bounded. Proof
obligations are discharged by a decision procedure, by a run-time check
inserted at a narrowing position, or by an imported smart constructor whose
postcondition is proved in SPARK.

| Stage | Construct | Logic | Example |
| --- | --- | --- | --- |
| 0 (exists) | Interval refinements of Integer | intervals | `(type Port (range 1 65535))` |
| 1 | Refinement types `{x : τ \| φ}` on base types and records | quantifier-free linear integer arithmetic (QF-LIA) on Integer fields; regular languages on String; length bounds | `(type Identity (refine String (matches "[a-z0-9.-]+") (length 1 64)))`, `(type Budget (refine Scheduling (<= budget_us (* 7 (/ period_us 10)))))` |
| 2 | Size-indexed types | Presburger arithmetic over indices | `List≤n τ`, `Array τ n`, `window : (n : Nat) → Stream τ → List≤n τ` |
| 3 | Typestate and value-indexed families | finite state machines; equality on enum or constant indices | `K@S` (7.4); `Locator⟨Kind.File⟩`; `Request⟨Service.Filesystem⟩` |
| 4 (research) | Dependent records and Π over constants | the stage-2 logic plus constants | `(record (n Nat) (xs (Array Integer n)))` |

**Rules common to all stages:**

- **Erasure:** refinements and indices are erased in bytecode. A narrowing position compiles to the same `Check_Range`-style guard, generalized to `Check_Refinement` with a verified predicate program.
- **Subtyping:** `{x : τ | φ} <: τ`, and `{x : τ | φ} <: {x : τ | ψ}` iff `φ ⇒ ψ` is valid in the stage's logic.
- **Manifests are the first client.** Identity, path, IPv4 and scheduling rules become refinements. Manifest checking then *is* type checking, and the hand-written validators disappear.
- **SPARK correspondence:** stage 1–2 refinements map to Ada subtype predicates. A CCL type then has a proved Ada twin with the same predicate, as with `Tight types` in the Ada codebase.

## 11. Bytecode typing and compilation

The CCLB verifier assigns each program point an abstract state:

```
⟨S, L, Δ⟩
```

`S` is a stack of value types `⟨kind, type, copyable, tag⟩`, `L` holds the local
types, and `Δ` is the ownership environment. The bytecode typing relation is
`⊢_B P ok`, and it holds when:

- **Instruction rules:** each instruction maps an input state to an output state by its typing rule.
- **Merges:** abstract states at merge points must be equal. Ownership states merge with 7.2's join.
- **Termination:** jumps go forward only, and named calls go only to lower indices.
- **Bounded stack:** the stack bound is statically computed, at most 64 slots.
- **Linear operands:** a non-copyable stack value may not be duplicated or dropped, and only the top value may be non-copyable at `Halt`.

**Type erasure.** `|·|` maps source types to bytecode types:

- ranges erase to Integer, with `Check_Range` at narrowing positions;
- refinements erase to their base type plus a check;
- records and payload variants erase to `Object(R)`;
- enums erase to `Variant`;
- streams erase to `Integer` tagged with the stream type, so arithmetic on a handle is refused;
- resources erase to non-copyable `Resource` values tagged with their policy mode.

**Compilation obligations:**

- **(C1) Type preservation:** if `⊢ e : τ ! ε` then `⊢_B compile(e) ok`, with result type `|τ|` and imports exactly `ε`.
- **(C2) Ownership preservation:** the source `Δ`-derivation maps to an accepted bytecode ownership derivation, with elaborated drops explicit.
- **(C3) Adequacy:** for every fuel `φ`, interpreting `e` and executing `compile(e)` produce the same value, or the same error class.

The bytecode verifier is a **separate trust boundary**. It MUST reject every
ownership or typing violation even if the compiler is faulty. Modules are
canonical: `encode(decode(m)) = m` for every accepted `m`.

## 12. Metatheory targets

These are the theorems the system is designed to satisfy. Each is to be proved
(SPARK, or a mechanized model) or, until then, tested differentially.

| Id | Theorem |
| --- | --- |
| T1 | **Progress and preservation (source):** a closed well-typed program evaluates, under any fuel, to a value of its type, a checked run-time error, or `Fuel_Exhausted`. It never gets stuck. |
| T2 | **Termination:** evaluation of every well-typed program terminates. Its cost is bounded by a function of program size and input lengths, independent of fuel. |
| T3 | **Linearity:** no `A`/`L` value is used after a move or duplicated. Every `L` value is discharged exactly once on every terminating path, including error paths through the host-transfer lifecycle. |
| T4 | **Authority confinement:** every host operation invoked at run time is in `ε`, hence in `G` after Admit. |
| T5 | **Decidability:** typing, elaboration, effect inference and admission are decidable, with cost linear in program size up to the fixed bounds. Refinement checking is decidable per stage. |
| T6 | **Determinism:** evaluation is deterministic given host replies and stream contents. |
| T7 | **Contract fidelity:** a value accepted under schema key `k` validates against the type `k` names. `Persistable` values round-trip exactly through the object encoding. |
| B1 | **Verifier soundness:** `⊢_B P ok` implies that the VM never reaches `Invalid_Bytecode` on `P` and never violates ownership. |
| C1–C3 | The compilation obligations in section 11. |

## 13. Conformance (2026-10-02)

**Legend:**

- **✓** conforms
- **◐** partial
- **✗** missing
- **⇄** diverges (a decision is recorded below)
- Test evidence means hosted and differential tests. Proof means SPARK, at the level stated.

### 13.1 Rules

| Spec | Status | Today |
| --- | --- | --- |
| 3.2 nominal declarations | ✓ | `CCL.Types.Define`, de-duplication by name |
| 3.2 correspondence compares bounds and defaults | ⇄ | `Correspondence.Resolve` ignores range bounds and defaults; `Import_Definition` checks both (Q-3) |
| 3.2 generated names not writable | ✗ | `List-Integer`, `Fn12` resolve as type names (Q-5) |
| 3.2 schema key is a digest of the descriptor | ◐ | Interfaces use SHA-256 of schema text; `CCL.Objects` treats keys as opaque |
| 4 kinds | ◐ | `Persistable`/`Storable` as specified; `Comparable` lacks records and lists; `Printable` lacks Boolean, String and records; ranges are not list elements (Q-6) |
| 5.1 uniform narrowing and widening | ⇄ | Lambda parameters keep `ρ` (Q-1); named calls narrow but value calls compare bases (Q-2); `if`/`match`/list/host positions require exact types |
| 5.2 core expressions | ✓ | `Check_Node`; interpreter and VM differential (`Same`, about 145 cases) until the interpreter's removal (2026-10-05) |
| 5.3 named arguments and defaults | ✓ | landed 2026-10-02; `tests/ccl-types/record_default_tests.adb` covers all paths |
| 5.4 string and record defaults | ✗ | integer, Boolean, member and `[]` only (C-5) |
| 5.5 match | ✓ | exhaustive, no duplicates; wildcard is future |
| 5.6 built-in schemes | ◐ | ad hoc per-built-in checks implement the schemes; no explicit scheme table |
| 5.7 no recursion, bounds | ✓ | visibility after body; static limits |
| 5.7 Handler second-class at all depths | ◐ | declarations no longer nest, so the result expression is checked at the root (Q-4 fixed 2026-10-02); value-level rules still partial |
| 5.8 streams | ✓ | `Stream<T>`; handles generation-checked |
| 6 host contracts | ✓ | `Θ` from `CCL.Catalog`; persistable objects by schema key |
| 7.1 modes U/A/L | ◐ | `CCL.Ownership`: `Unrestricted`/`Move_Only`/`Must_Handle`, per resource type through approved policy |
| 7.2 source-level ownership judgment | ✗ | ownership is checked only on bytecode (O-1) |
| 7.2 affine weakening | ⇄ | `Move_Only` requires an explicit `Drop` at scope end (O-4); spec: elaborate the drop |
| 7.2 joins | ✓ | exact environment equality (`Join`) |
| 7.3 dispositions and transfer | ◐ | consume and transfer are indistinguishable (O-2); cancellable owned imports rejected (O-7) |
| 7.4 typestate | ◐ | `Transition` dispositions re-type a binding; no source syntax |
| 7.5 lexical borrows | ✗ | borrows are counters, used only by host imports (O-3) |
| 7.5 closures capture `A`/`L` | ✗ | only `U` data captured (conservative subset) |
| 7.6 resources in records | ✗ | resources cannot be fields |
| 8 effect types and Admit | ◐ | Admit is a whole-program grant check; effects are not in function types; diagnostics name the missing operation (`Explain_Refusal`) |
| 10 stage 0 intervals | ✓ | range types, static literal check, `Check_Range` |
| 10 stages 1–4 | ✗ | design only; manifests are the first client |
| 11 verifier | ◐ | abstract interpretation as specified; value calls bounded only at run time |
| C1–C3 | ◐ | tested (differential, tampered programs, byte sweep); not proved. With the interpreter removed (2026-10-05), C3 has no executable reference engine |
| T2 termination, fuel | ◐ | fuel bound proved (`Steps ≤ Fuel`); termination by construction, not proved |
| T3, B1 | ◐ | VM and ownership verifier proved free of run-time errors (level 2, 212 checks); semantic soundness not proved |
| canonical modules | ◐ | byte-sweep tested; codec proved AoRTE at level 1 only |

### 13.2 Decisions on observed quirks (Q) and ownership gaps (O)

| Id | Observation | Decision |
| --- | --- | --- |
| Q-1 | Lambda range parameters are not widened. | Fix: all binders follow 5.1. |
| Q-2 | Named calls narrow; value calls compare bases. | Fix: both narrow per 5.1. |
| Q-3 | Correspondence ignores bounds and defaults. | Fix: compare both (3.2); keys become descriptor digests. |
| Q-4 | Handler root check only at depth 0. | Fixed 2026-10-02: declarations are siblings, not nesting. |
| Q-5 | Generated type names are writable. | Fix: reject them in `Read_Type`. |
| Q-6 | Ranges are not list elements. | Fix: ranges are `Data`. |
| Q-7 | `let` binder names are not validated. | Fix: same rule as parameters (Referenceable, not reserved). |
| Q-8 | `(Unit)` is a record construction. | Keep: `Unit` is the empty record; its value is written `(Unit)`. |
| O-1 | No source ownership checker. | Add the 7.2 judgment in `CCL.Language`; the bytecode verifier stays the second boundary (11). |
| O-2 | Consume and transfer are equal. | Keep equal for discharge; distinguish in the effect trace. |
| O-3 | Borrows are counters. | Add lexical borrow forms (7.5); counters remain the bytecode representation. |
| O-4 | `Move_Only` needs an explicit drop. | Source permits weakening; elaboration emits `Drop_Local`, keeping bytecode explicit. |
| O-5 | Joins require exact equality. | Keep, after drop elaboration. |
| O-6 | Streams and handlers are outside ownership. | Keep: streams are temporal (generations), handlers are second-class. |
| O-7 | Cancel verbs are unchecked. | Keep rejecting cancellable owned imports until cancellation joins the 7.3 rule. |

## 14. Order of work

1. **Fix the quirks** Q-1 to Q-7 (small, local; in the checker, compiler, verifier and VM).
2. **Source ownership judgment** (O-1, O-4), with lexical borrows (O-3). Then typestate syntax.
3. **Effects in function types**, and admission as `ε ⊆ G`. Derive minimal manifests from effects.
4. **Refinement stage 1** with manifests as the first client. Typed manifests (docs/ccl-typed-manifests.md) then express their path, identity and network rules as types.
5. **Proofs:** functional contracts for `CCL.Ownership` (T3 on the bytecode model), verifier soundness (B1) in stages, and a mechanized model of the core for T1/T2.
