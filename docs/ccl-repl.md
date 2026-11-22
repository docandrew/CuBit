# The CCL REPL: typing commands, without the terminal

Status: design (2026-09-29). Nothing here is implemented beyond "Where CCL is
today". It builds on:
- [control-language.md](control-language.md): the language, with its Lisp
  and BASIC dialects mapping 1:1;
- [ccl-standard-library.md](ccl-standard-library.md);
- [ccl-interactive-composition.md](ccl-interactive-composition.md);
- [ccl-launch-parameters.md](ccl-launch-parameters.md);
- [stream-wiring.md](stream-wiring.md): typed ports;
- [agent-security.md](agent-security.md);
- [scheduler.md](scheduler.md) and [audio-graph.md](audio-graph.md): live
  metrics.

## Premise

**CuBit has no TTYs.** A terminal is a grid of characters, and every Unix
shell inherits that limit: output is text, so tools print text and other
tools re-parse it. In CuBit, a command is a typed operation on a typed
interface, and its result is a typed value. The REPL is free to show that
value in whatever form makes a person fastest.

**The remote REPL is not SSH-over-a-terminal.** It is the same typed message
exchange as IPC, coming from an outside authority. Its scope and permissions
are those of its grant, which is narrower than a local session's unless a
policy says otherwise.

**The goal:** typing commands and getting feedback should feel like the
future — live, visual, discoverable — and be measurably more productive
than a terminal. People like typing into a console, so the keyboard stays
primary: everything is reachable by typing, and the pointer is optional.

The CCL Workbench REPL is the primary console. The old framebuffer console,
`shell.app`, is retired once the REPL covers its commands; its command list is
the checklist in "Coverage".

## Principles

1. **Results are live objects, not printed text.**
   - A table of processes stays connected to its source. It can update in
     place, sort, filter, and be opened: a process row leads to its
     streams, endpoints, capabilities and threads.
   - Anything can be pinned to become a live widget.
2. **Effects decide what may run while you type.**
   - The effect checker knows whether a pipeline only observes, or also
     controls, writes or reaches the network.
   - Observe-only pipelines preview live, like a spreadsheet: the result
     updates with every keystroke, within its fuel and time bounds.
   - Anything with effects runs only on Enter, shows a plan first ("will
     restart 2 services"), and asks for approval when policy requires it.
3. **Intellisense comes from types, never hand-written tables.** Completion,
   signatures, docs, fields after `|`, enum members, units, paths, processes
   and the capability each command needs are all generated from interface
   descriptors and the checker.
4. **Everything is visible.** Every stream of a command has a home: output,
   errors, log, metrics, health and progress each go to a sink that fits
   (scheduler.md §8, visibility).
5. **It must be fast, and the speed is measured.**
   - A keystroke is echoed on the next frame, and completion shows within
     10 ms.
   - A live preview never blocks typing.
   - The input-latency budget of [input-latency.md](input-latency.md)
     applies to the REPL itself.
6. **Two dialects, one program.** Every line can be shown and edited as Lisp
   or BASIC, and the two map 1:1. The pipeline form (below) is part of the
   language and must map 1:1 into both.

## Typing: the command line

```text
ps | where cpu > 5 | sort-by mem --desc | first 10
logs audio | where level >= warn
open @nvme:0/report.json | get services | where state != running
```

- **Commands** are typed operations from interface descriptors. Flags are
  named arguments, checked against the descriptor.
- **Pipes.** `a | f x` elaborates to `(f x a)` in Lisp, and to the
  equivalent in BASIC. Values are tables, records, lists and scalars.
- **Blocks.** `{ |p| p.name }` is a lambda that closes over the session.
- **Sources are commands at the head of the pipeline:** `open`, `ls`, `ps`,
  `logs`, `fetch`, `job 3`. There is no `<` input redirect.
- **Saving:** `> path` and `>> path` mean `save` and `save --append`. Saving
  is an effect, checked against file-write authority, and it is atomic
  (written to a temporary file, then renamed).
- **Streams by name.** `build:log`, `mixer:metric` and `server:health`
  select a port other than the primary one. `tee` fans out within a
  pipeline, and `connect a:log -> logstore` wires a port durably under
  stream-wiring.md's approvals.

## Seeing: the result surface

| Result | Shown as | You can |
|---|---|---|
| table | a live grid with typed columns and units | sort, filter, open rows, pin, chart a column |
| record | a structured card | expand fields, follow references |
| stream | a live pane: log tail, sparkline, gauge, event list | pause, scrub time, filter as you type |
| graph (services, audio, stream wiring) | a node-link view | select a node to insert it into the command line |
| plan (effects about to happen) | a diff: before and after | approve, edit or cancel |
| error | inline, at the offending token, with the fix suggested by the checker | apply the fix |

- **The system map.** A persistent, navigable view of what is running:
  services, apps, endpoints, the stream wiring and the authority edges between
  them. Typing narrows it: `net` highlights netstack, its endpoints and its
  clients. Selecting from it inserts a typed reference into the command line.
  The same view is what `describe` returns for any object.
- **Pinning builds dashboards.** Pinning any live result (`sched top`,
  `mixer:metric | chart`, `fs cache-stats`) makes it a widget, so a dashboard
  is built by typing and saved as a CCL document.
- **Time.** Every result is timestamped, and history is a notebook of typed
  results, not scrollback.
  - `diff (ps @10m-ago) ps` compares states.
  - Metrics streams can be scrubbed back in time.
  - Results can be re-run, compared and exported.

## Intellisense

- **Completion is ranked by type and context:**
  - after `|`: operations that accept the value's type;
  - after `.`: the value's fields;
  - after `where x >`: values of `x`'s unit;
  - paths and processes, from live data.
- **Signature help:** parameters, types, defaults and ranges (from typed
  launch parameters), effects, and the capability needed. A command the
  session cannot run is shown, and says why.
- **Ghost preview.** For observe-only pipelines, the first rows of the result
  appear under the line as you type.
- **`help` and `describe`** render descriptor docs with examples, and every
  example is runnable.

## Remote

The web front end (apps/ccl-control and `userspace/ccl/remote`) is the
same REPL in a browser, speaking the same typed message exchange over the
network:
- **Authority:** a remote session holds exactly its grant. It is scoped by
  the outside authority's identity and by policy, never by a Unix account.
- **Accountability:** every command is typed, attributed and logged.
- **Streams and the system map render live**, which is where visualization
  is richest.
- **Infrastructure as code:** a system's desired state is a CCL document.
  Applying it works like plan/apply:
  1. type- and authority-check the document;
  2. show the difference from the live system;
  3. apply atomically, with rollback.

  Live state is queryable as the same kind of document, so drift is
  visible.

## Agents

An agent uses the same engine:
- **Natural language in the REPL** ("why is audio crackling?") produces a
  proposed CCL pipeline, shown inline with its effects.
- **Observe-only proposals preview live.** Anything else follows
  propose/approve/commit ([agent-security.md](agent-security.md)).
- **Agents can build apps.** A generated app or plugin is a CCL package
  whose ports, UI and capabilities are shown before install ("write me a
  guitar fuzz plugin with pedal knobs": [audio-graph.md](audio-graph.md)).

## Productivity: how we know it works

This is a benchmark, not a feeling. For a fixed set of daily tasks, we
measure keystrokes, time and errors, against bash and nushell doing the same
job on Linux. The tasks:
- find what is using CPU;
- tail and filter a service's log;
- restart a service;
- find large files;
- check network state;
- change a setting.

The REPL should win on every task, or we learn why. The latency targets in
principle 5 are checked by the same probes as the input-latency work.

## Where CCL is today (survey, 2026-09-29)

- **The core is small and mostly proved:** a reader, checker and tree
  interpreter (`ccl-language.adb`), a bytecode VM with a verifier, and an
  interface catalog with granted bindings.
- **Missing:**
  - subtraction, comparisons, `and`/`or`;
  - lists, maps, Option/Result;
  - lambdas, loops, recursion.
- **Strings:** four functions only.
- **Limits:** 1 KiB of source, 128 AST nodes.
- **The Workbench REPL:**
  - single-line input;
  - 16 entries of history;
  - completion of catalog operations only;
  - one-line `Type: value` results;
  - no persistence between entries.
- **Host calls:** each takes one argument; only clock, config and UI are
  exposed; descriptors are mirrored by hand in Ada.
- **The bytecode path** cannot run strings or functions.
- **Two consoles today:** the Linux `ccl-run` text REPL (development only)
  and `shell.app` (Ada, not CCL).

## Coverage: retiring shell.app

`shell.app`'s commands, each to become a typed operation or a generic
pipeline command:

`clear config echo head hexdump help ifconfig kill logs ls mem nslookup ping
volumes ps pwd route spawn streams sysinfo uptime wc write`

- `head` becomes `first`, `wc` becomes `length` or `stats`, and `hexdump`
  becomes `bytes | hex`.
- `ls`, `ps`, `volumes`, `route` and `sysinfo` return tables or records.
- `ping` returns a stream of replies, and `streams` lists a process's typed
  ports.

## Phases

Each phase ships something usable in both the Workbench and the remote web
front end, the Observatory (userspace/ccl/tools/ccl-observatory, with
userspace/ccl/remote/control_wire on the CuBit side). A new value kind means
a new wire result type in control_wire, and a decoder and renderer in
wire.js/app.js, each with its node tests. The two front ends never drift
apart.

1. **Language core,** keeping the level-1 proofs and the Lisp/BASIC 1:1
   mapping:
   - arithmetic and comparisons, `and`/`or`;
   - bounded lists and maps, Option/Result;
   - lambdas and closures;
   - fuel-bounded `each`/`fold`;
   - a builtin table (`ccl-builtins`);
   - string functions;
   - a persistent session environment;
   - configurable bounds.
2. **Pipelines and the result surface:**
   - the `|` form in both dialects;
   - the table builtins (`where`, `each`, `sort-by`, `group-by`, `first`,
     `get`, `select`, `length`);
   - the live table and card renderers;
   - `save`/`open`;
   - the `cmd:stream` syntax and default sinks;
   - ghost preview for observe-only pipelines.

   **Fuel for streams (decided 2026-09-30).** Today fuel is one total per
   evaluation (1M in a session, 4,096 for periodic programs). For a stream
   such as `open file.txt | filter "asdf"`, that can't tell a broken program
   from a large input. CCL programs always terminate, so fuel's job is
   catching runaway cost, not ensuring termination. Streams therefore split
   the two concerns:
   - **Fuel per input unit.** Each element (line, record or chunk) gets its
     own budget in each pipeline stage. A predicate that burns too much on
     one line is stopped as a bug; a large file of cheap lines is not.
   - **Total cost through the scheduler.** A streaming run executes in
     slices; the VM already pauses (`Paused`) and resumes for a bounded
     number of instructions. Each slice gets fresh fuel, and its CPU time
     is charged to the session through the scheduler's budget accounting.
     It runs as long as its input lasts, at a fair share of the CPU, always
     visible (Observatory) and cancellable.
   - **Explicit total ceilings.** An overall limit is opt-in, never a hidden
     default: something like `(with-budget 10s ...)`, or a policy limit,
     for example for agents.
   - **Non-streaming entries** keep today's single total budget.

   Slices also give natural checkpoints for pausing or migrating a run, and
   "fuel per unit × units" makes cost predictable (docs/ccl-cool-stuff.md).
3. **System interfaces:**
   - multi-argument, record-returning host calls;
   - generated descriptor bindings;
   - files, processes, logs, network, volumes and system information;
   - scheduler and audio metrics;
   - the system map.

   Then retire `shell.app`.
4. **Typed launch parameters,** on the same descriptors. The first users are
   netstack's TCP capacity and the filesystem block cache.
5. **Remote:** apps/ccl-control as a full front end, then plan/apply
   documents.
6. **Agents** on the same engine.

## Using the REPL today

The Workbench REPL (F6 in the CCL Workbench) and `ccl-run` on Linux share one
session engine (`CCL.Sessions`), so everything below works in both.

**Either dialect, no mode switch.** Input starting with `(` is Lisp. Anything
else is read as BASIC (falling back to Lisp for atoms like `42`).

```
20 + 22                                   (+ 20 22)
upper("hello")                            (upper "hello")
IF 3 > 2 THEN "yes" ELSE "no" END         (if (> 3 2) "yes" "no")
```

**The session remembers.** Definitions and named values persist across
entries; redefining a name replaces it, and dependents pick it up.

```
FUNCTION sq(n AS Integer) AS Integer RETURN n * n END
(define (cube (n Integer)) Integer (* n (sq n)))
LET words = split("", "the quick brown fox")
(define total (sum (each (fn ((w String)) (length w)) words)))
LET total = total * 2
:env        what the session keeps
:reset      forget everything
```

In the Workbench, the session's definitions can be kept as files in its
workspace (the only files it may touch):

```
:save tools        the definitions as tools.ccl (never replaces a file)
:files             the .ccl files in the workspace
:load tools        tools.ccl's definitions into the session
```

Kept values are literals: integers, Booleans, strings, enumeration members, and
complete lists of up to 16 integers, Booleans or strings. Anything else is
refused with a clear message (define a function instead). An entry changes the
session only if it succeeds.

**Pipelines.** `|` (BASIC) and `->>` (Lisp) pass a value through stages,
each receiving it as its last argument; a bare name is a one-argument stage.
Both print back exactly as written.

```
range(1, 20) | where(FUNCTION(n) n MOD 3 = 0) | sum           63
split("", "the quick brown fox") | sort-by(FUNCTION(w) length(w)) | first(2)
"hello world" | upper | reverse                                "DLROW OLLEH"
(->> (range 1 20) (where (fn (n) (= (mod n 3) 0))) sum)
```

**Functions without type noise.** A function passed to a builtin, or to a
parameter declared as a function type, takes its parameter types from there:
`FUNCTION(w) length(w)` / `(fn (w) (length w))`. A function bound with
`let`, or written anywhere else, still needs its types.

**Builtins.** The subject (list or string) always comes last, ready for
pipelines.

| Lists | Strings |
|---|---|
| `each f xs`, `where p xs`, `fold f init xs` | `upper s`, `lower s`, `trim s` |
| `any p xs`, `all p xs`, `count p xs` | `contains needle s`, `starts-with p s`, `ends-with p s` |
| `first n xs`, `last n xs`, `skip n xs` | `index-of needle s` (1-based, 0 if absent) |
| `sort xs`, `sort-by key xs`, `reverse xs` | `replace old new s` (every occurrence) |
| `sum xs`, `min xs`, `max xs`, `range a b` | `split sep s` (`""` splits on blanks), `join sep xs` |
| `contains x xs`, `length xs`, `at i xs` | `parse-int s`, `to-string n`, `concat a b` |

`first`, `last`, `skip`, `reverse` and `contains` also work on strings.
Functions are `(fn ((x T)) body)` / `FUNCTION(x AS T) body`; they may capture
enclosing values. A function you define shadows a builtin of the same name.

**Limits.** Fuel 1,000,000 per entry (every element and comparison costs fuel,
so every entry ends); 8 KiB programs; 512 syntax nodes; lists of up to 4,096
elements. Results longer than 64 elements show the first 64 and `... N more`.

## Phase 1 progress

- **Done (2026-09-29):**
  - Subtraction, `/=`, `<`, `<=`, `>`, `>=`, and short-circuit `and`/`or`.
    Lisp also accepts word spellings (`subtract`, `less`, …).
  - BASIC spells them `-`, `<>`, `<`, `<=`, `>`, `>=`, `AND` and `OR`, with
    precedence `OR` < `AND` < comparisons < `+ -` < `* / MOD`.
  - A binary minus in BASIC needs whitespace before it, because names may
    contain hyphens: `a-b` is a name, `a - b` subtracts.
  - `=` and `/=` accept Booleans.
  - Tests: native suite (18 new checks); BASIC view round trips (43
    fixtures).
  - Bytecode too (2026-09-29): CCLB opcodes `Subtract_Integer`,
    `Less_Integer`, `Less_Equal_Integer` and `Equal_Boolean`; the other
    operators lower to these plus `Not_Boolean` and forward jumps. A
    differential test runs 19 expressions both ways and compares results.
    From here on, each language feature lands in the interpreter and CCLB
    together. (Superseded 2026-10-05: the interpreter was removed; features
    land in the analyser, compiler, verifier and VM.)
  - Named functions in CCLB (2026-09-29): `Call_Function`/`Return_Function`,
    one region per function, no recursion, and a verifier-checked
    whole-program stack bound. A defined function now shadows a builtin of
    the same name, and `fn` and `list` are reserved words. Function values,
    lambdas, lists, strings and builtins are the remaining CCLB backlog.
- **Lists, first slice (2026-09-29):**
  - `[a b c]` / `(list a b c)` in Lisp and `[a, b, c]` in BASIC, the same
    node; at most 16 elements per literal. An empty `[]` is rejected until
    declared types are wired in.
  - `List<T>` for Integer, Boolean, String, Character and enumeration
    elements.
  - `length` and `at` (1-based, typed index errors) work on lists.
  - Elements live in `CCL.Secondary_Arrays`, a generic sibling of the
    string region. It is proved at level 1 (54 checks) and hosted-tested.
  - Results: the REPL shows `List<Integer>: [10, 20, 30]`. The remote wire
    carries type code 5 plus typed elements, and the Observatory decodes and
    shows them as a table.
  - Tests: native suite, 48 view round trips, remote CBOR, and web decoder.
  - Proof of the interpreter with lists: in progress (moot since the
    interpreter's removal on 2026-10-05).
- **Functions, slices 1–3 (2026-09-29):**
  - Function types `(Function (Integer) Integer)` / `FUNCTION(Integer) AS
    Integer`; named functions are values; `(f 3)` calls through a value.
  - Anonymous functions `(fn ((n Integer)) (* n n))` /
    `FUNCTION(n AS Integer) n * n`, lambda-lifted to generated `fn#`
    definitions. Captures are rejected for now
    (`Lambda_Capture_Unsupported`).
  - List builtins, collection last so that pipelines can supply it:
    `each`, `where`, `fold`, `any`, `all` (short-circuit), `first`, `sum`,
    `range`. Operands are evaluated before the builtin runs. Every element
    visited costs fuel, so a builtin always ends.
  - Type rules: the function's parameters must match the element type, and
    `where`/`any`/`all` need a Boolean result (`Function_Argument_Mismatch`).
    `each` may change the element type.
  - The REPL shows `Function: name`. The remote wire carries type code 6 with
    the function's name, and the Observatory shows it.
  - Tests: native suite (13 builtin checks), 61 view round trips, remote
    CBOR, web decoder.
- **Captures and list types, slice 4a (2026-09-29):**
  - An anonymous function may use enclosing `let` bindings and parameters:
    `(let ((k 3)) (each (fn ((n Integer)) (* n k)) xs))`. The checker records
    each enclosing name the body resolves, including through a nested
    function. The function value carries a copy of each captured value in
    the list region, and a call binds them beneath the parameters. That is
    lambda lifting: no closures on the heap, no recursion.
  - Captures are by value at creation. At most 4 are allowed
    (`Too_Many_Captures`), and only of scalar, String, Character or
    enumeration type. A list or function capture is reported as
    `Lambda_Capture_Unsupported`; pass those as parameters.
  - List types in declarations: `(List Integer)` / `LIST(Integer)`. For
    example, `FUNCTION scale(k AS Integer, xs AS LIST(Integer)) AS
    LIST(Integer)`.
  - List builtins now build their results in place in the list region
    (`Reserve`, `Write`, `Shrink` in `CCL.Secondary_Arrays`, proved at level
    1, 88 checks), with no stack buffer sized by the data.
- **Daily-driving round (2026-09-29, overnight):**
  - String builtins and list builtins round 2 (table above); results built
    in place, fuel per element and per comparison; heapsort.
  - The REPL reads BASIC as well as Lisp; BASIC is lowered without checking
    and the whole entry is checked in the session's context.
  - Persistent session environment: kept definitions and literal values,
    `:env`, `:reset` (CCL.Sessions).
  - Long list results are shortened (first 64 plus the total) instead of
    failing; the remote wire carries the total (11-field list response).
  - Limits raised: fuel 1,000,000 (was 4,096), source 8 KiB (was 1 KiB),
    512 nodes (was 128), 4,096 list elements, 32 KiB text region, 8 KiB of
    literal text per program.
  - The Workbench logs each REPL result (`ccl-workbench: REPL completed:
    <result>`), and the headless `ccl-workspace` test now types BASIC
    `40 + 2` and requires `Integer: 42` inside CuBit.
  - Parameter-type inference for anonymous functions passed to builtins or
    to function-typed parameters (`FUNCTION(w) ...`, `(fn (w) ...)`).
  - Pipelines: `x | f(a) | g` / `(->> x (f a) g)`, stages marked in the
    tree so both views print them as written (74 view round trips).
  - Workbench workspace commands `:save`, `:load`, `:files`.
  - Found natively: the Workbench's live label passed the new session fuel
    default into `CCL.Periodic_Programs` (budget 1 .. 4096), a range error
    only a native run exercised; periodic programs now use their own
    `Default_Fuel`.
- **Next:** system data (files, processes, services) as typed lists through
  a capability-gated interface; CCLB parity for strings and lists (v8).

### Lists (design)

- **Type.** `List<T>` is a one-parameter type, specialized in the registry
  as resource families are (`CCL.Types`). Elements are homogeneous and
  capacity is bounded; the session sets the bound, never unbounded.
- **Literals** (decided 2026-09-29):
  - Lisp: square brackets, as in Clojure: `[1 2 3]`, `[x (+ y 1)]`.
  - BASIC: `[1, 2, 3]`.
  - Both are exactly the canonical `(list 1 2 3)`: parentheses mean a call
    and brackets mean a collection.
  - There is no quote form; unevaluated symbol data does not fit static
    typing.
  - An empty `[]` takes its element type from context (a declared parameter
    or `let` type), or is written `(list-of Integer)`.
- **Storage** follows control-language.md's "Constrained and
  unconstrained values", the same scheme strings use today (Ada semantics,
  GNAT-style secondary stack):
  - `List<T>` is an unconstrained type; every list value carries definite
    bounds (lower bound 1).
  - Elements live in the bounded CCL secondary region
    (`CCL.Secondary_Stacks`, generalized from strings to element arrays).
    Values hold checked region descriptors, never native pointers.
  - Marks delimit temporary lifetimes. Releasing a mark invalidates newer
    descriptors, and generations stop stale descriptors from reviving when
    storage is reused.
  - Arrays slide on assignment (same length, bounds may differ). A binding
    with an explicit constraint is accepted only when it can be proved.
  - Every array has a static or session-supplied maximum. Exhausting the
    region is a typed failure, not a crash.
  - Scalar and string elements come first; records follow (see "Records
    and variants: the value arena" below).
- **Wire form.** IPC and remote sessions serialize with CBOR, using the
  restricted, float-free profile of apps/ccl-control
  ([ccl-cbor-evaluation.md](ccl-cbor-evaluation.md)).
  - A `List<T>` is a definite-length CBOR array (major type 4) of `T`'s
    encoding. Indefinite-length arrays are rejected.
  - Decoding checks the declared element type and capacity against the
    schema before placing the elements in the receiver's secondary region.
    A peer can therefore never grow a list past its declared bound or change
    its element type.
- **First builtins:**
  - `length`, `at` (Option result once Option exists; index error until
    then), `append`, `concat`, `reverse`, `sum`, `range`;
  - with function values: `each` (map), `where` (filter), `fold`, `sort-by`,
    `first n`, `any`, `all`.
- **Anonymous functions** (decided 2026-09-29). They map 1:1:

  | Lisp | BASIC |
  |---|---|
  | `(fn ((x Integer)) (* x 2))` | `FUNCTION(x AS Integer) x * 2` |
  | `(fn (x) (* x 2))`, typed from context | `FUNCTION(x) x * 2` |

  The pipeline form `{ |x| x * 2 }` is a third spelling of the same node.
  - Named functions are values by name: `(each double xs)` / `each(double, xs)`.
  - Calling a function value is an ordinary call: `(f 3)` / `f(3)`.
  - Function types: `(Function (Integer) Integer)` / `FUNCTION(Integer) AS Integer`.
  - Slices:
    1. function types, named functions as values, and calls through values;
    2. `fn` without captures;
    3. `each`, `where`, `fold`, `sort-by`, `first`, `any`, `all`;
    4. captures (lambda lifting) and parameter-type inference.
- **Functions as values.** `(handler f)` generalizes from zero-argument
  Boolean functions to any declared function, typed by its profile. Lambdas
  `{ |x| ... }` / `(fn (x Integer) ...)` are lambda-lifted into generated
  `define`s. Captured `let` values become extra parameters, so the
  evaluator still has no closures on the heap, and "no recursion" still
  holds.
- **Proof targets:**
  - list operations never index outside capacity;
  - element types match the list's parameter;
  - `each`, `where` and `fold` consume fuel for every element, so they
    always terminate.

### Records and variants: the value arena (2026-09-30)

Records and variants with payloads that an evaluation builds live in a
bounded **value arena** (`CCL.Language`). This design replaces one 16 KiB
object image per record.
- **Nodes and slots.**
  - A node is a record or payload variant. Its components are stored values
    (slots): scalars, text descriptors into the text region, list
    descriptors, or earlier nodes.
  - There are no pointers. Components always refer to *older* nodes, so
    values are acyclic, and the printer and exporter recurse on a strictly
    decreasing node index.
- **Limits.** 512 nodes and 2,048 slots per evaluation (`MAX_VALUE_NODES`,
  `MAX_VALUE_SLOTS`), about the memory the 16 images took. Exceeding them
  is `Evaluation_Object_Storage_Exhausted`, a typed failure.
- **Text is shared, not copied.** A record's text fields name strings in
  the text region, so 16 fields holding one 513-byte string store 513 bytes.
- **Object images** (`CCL.Objects`) remain the host/IPC boundary format:
  - arena values are copied into an image when passed to a host operation,
    and validated against its schema;
  - host results still arrive as image views (moving them into the arena is
    part of the next step).
- **Results.** A record or payload variant result is exported as its
  canonical literal, which reads back as the same value:
  - `Pair: (Pair 42 "hi")`
  - `V: (V.Some (C 7))`
  - `Note.Absent`

  The REPL shows it, a `LET` keeps it between entries, and BASIC
  constructor syntax works (`LET p = Pair(42, "hi")`).
- **Not yet:**
  - a record with a Character field has no literal (CCL has no character
    literal syntax), so it is refused;
  - the remote wire has no type code for literals yet;
  - the CCLB compiler/VM still rejects records (parity step).

**Lists of records (2026-09-30).**
- **What works:**
  - lists of records and variants: `[(C 1 "x") (C 2 "y")]`;
  - list-typed record fields: `(type R (record (xs (List Integer))))`;
  - `each`, `where`, `fold`, `sort-by`, `reverse`, `first` and `skip` over
    record elements.
- **Typed empty lists.** `(list-of T)`, and `list-of(T)` in BASIC, is the
  empty `List<T>` when nothing else gives the element type (a `Launch`
  with no dependencies needs it).
- **Long literals.** A literal may be longer than one syntax node's 16
  components: the parser chains 16-element chunks, invisibly to the
  language.
- **Results.** Lists with compound elements leave as literals
  (`List<C>: [(C 1 "x")]`).
- **Termination measure.** The printer's recursion measure is (node bound,
  list level): a record steps to strictly older nodes, and a list steps to
  its elements, which are never lists.
- **Rules.**
  - Local types use `CCL.Objects.Storable`: `Persistable`, plus list
    fields and lists of Persistable elements.
  - Only Persistable values cross a host boundary.
- **Not yet:**
  - lists of lists;
  - lists inside host images;
  - a character literal.
- **A limit that matters later:** a whole program has at most 512 syntax
  nodes, and a full startup profile needs roughly 350–400.

**Recursive types (2026-09-30).** A record or variant may hold a list of
itself, and nothing else recursive:

```lisp
(type Launch (record (name String) (after (List Launch))))
(type Tree (variant (Leaf Integer) (Node (List Tree))))
```

- **Why values stay finite.** A list may be empty, so every value is
  finite. A direct self field such as `(next T)` has no base case and
  remains an error, as does a list of lists.
- **How the declaration is read.** Inside its own declaration, a type's
  name is accepted only as `(List Self)`. The field is completed after the
  type is defined (`CCL.Types.Complete_Self_List`), the one forward
  reference the registry allows.
- **Host boundary.** Recursive types are `Storable`, never `Persistable`,
  so they don't cross it as images.
- **Values are acyclic.** Arena nodes refer only to older nodes, and two
  launches may share one dependency. The literal repeats shared values,
  since values are immutable.

**Range types (2026-09-30).** `(type Priority (range 1 10))`, written
`TYPE Priority = RANGE 1 TO 10` in BASIC, declares a subtype of Integer, as
in Ada.
- **Positions, not expressions.** A range type constrains the positions
  that hold values: record fields, variant payloads, function parameters
  and results. Reading such a position gives an Integer, so arithmetic,
  comparison and builtins are unchanged, and arithmetic results are
  Integers.
- **Checking.**
  - A literal flowing into a range position is checked when the program is
    checked: `(L "a" 11)` gives `Value_Out_Of_Range`.
  - A computed value is checked when it runs: `Evaluation_Range_Error`, a
    typed failure, never wrapping or clamping.
  - Function values given to builtins (`each`, `where`, ...) check their
    range parameters and results the same way.
- **Declaration rules.** Bounds are integer literals, and `Low > High` is
  refused.
- **Not yet:**
  - range types as list elements;
  - range types across a host boundary (they are `Storable`, not
    `Persistable`, and have no wire encoding yet).
- **Later.** The bounds are static facts a verifier can use; that is where
  formal verification of configurations starts.

**String equality (2026-09-30).** `=` and `/=` compare strings by
content, in the interpreter and in CCLB.

**Character equality (2026-10-01).** `=` and `/=` also compare characters
(from `at`), in both engines. There is still no character literal syntax:
write `(= (at s 1) (at "x" 1))`.

**Bytecode parity is next (decided 2026-09-30).** (Since 2026-10-05 the
interpreter is removed and every program runs on the VM, CCLB format
version 9; the rest of this paragraph is the 2026-09-30 plan.) Everything above runs
only in the interpreter today. The CCLB compiler still rejects records and
variants with payloads, lists of records, recursive and range types, and
CCLB is still format v7. Before any new language feature:
- CBOR module format v8, with a SPARK-proved reader;
- strings and lists in the VM;
- the value arena in the VM, whose verifier proves that node references
  point backwards;
- range checks as a verified operation.

After that: named-field construction and imports that carry types, landing
in the interpreter and the VM together.

## Open questions

- Does the VM gain strings and closures, or does the REPL stay on the
  interpreter? (Resolved 2026-10-05: the REPL runs on the VM; the
  interpreter was removed.)
- How is a live `Stream<T>` represented in the language, and how is it
  buffered?
- How are preview budgets set (fuel, time, memory), and how is a preview
  cancelled on each keystroke?
- How are units and quantity literals (`1mb`, `5ms`) typed?
- How should the pipeline form look in BASIC, keeping the mapping 1:1?
