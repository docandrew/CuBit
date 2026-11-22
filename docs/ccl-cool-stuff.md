# CCL cool stuff

What CCL's design makes possible, beyond a shell and a configuration language.
Most of this is **direction, not implementation**. Each section says what exists
today, what is planned, and what is only possible because of choices already
made. The constraints at the end keep these doors open.

Related: [CCL REPL](ccl-repl.md), [bytecode format](ccl-bytecode-format.md),
[agent security](agent-security.md), [threads](threads.md).

## The ingredients

CCL bytecode (CCLB) has an unusual combination of properties:

| Property | Status |
|---|---|
| A verifier checks stack types, jump targets and ownership before anything runs | implemented, proved |
| Forward jumps only: every program terminates | implemented |
| Fuel: every instruction is metered, so every run is bounded | implemented |
| Effects only through declared host calls (imports); no ambient clock, randomness or I/O | implemented |
| The VM pauses at each host call and resumes when the host answers | implemented |
| Bounded, allocation-free machine state with no native pointers | implemented |
| A canonical encoding, so a module has one byte form and one digest | implemented (v7 binary); moving to strict CBOR (v8, planned) |
| Signed modules (COSE_Sign1 envelope) | planned |
| One CBOR value encoding shared by code constants, arguments, results and storage | planned (v8) |

Each ingredient exists in some other system. Having all of them in one small,
proved VM is what enables the rest of this document.

## 1. Ship code to the data

Instead of many round trips (list the services, fetch each one's metrics, filter
locally), a client sends one small module that runs next to the data and returns
only the answer.

- It generalizes the "reduce IPC" rule from batching messages to batching
  computation.
- It is safe on the receiving side. The verifier checks the module, fuel bounds
  it, and the receiver's policy decides which host calls it may make. Foreign
  code gains no more authority than local code.
- It suits remote management and IaC, agents operating many machines, and
  queries against large local datasets (logs, metrics, filesystems).

*Status: possible once v8 modules and a "run module" request exist. Interactive
REPL and remote evaluation keep sending source, which the target's own catalog
checks.*

## 2. Content-addressed code

A module's canonical bytes give it a stable digest, and the digest is its
identity.

- Nodes cache verified modules by digest. A request sends the digest first and
  the module only on a miss.
- Verification can be cached by digest too, so a signed module seen before
  starts immediately. That is "execute directly" without trusting the sender's
  toolchain.
- Imports bind by interface-descriptor digest. Code built against one version
  of an interface cannot silently bind to another.

*Prior art: Unison (content-addressed functions), Nix (content-addressed
builds), IPFS.*

## 3. Deterministic results

Everything nondeterministic arrives through an explicit host call, so a
module's result is a function of its bytes, its arguments and the host replies
it received.

- **Caching:** memoize by (module digest, input digest).
- **Verification by re-execution:** run the same module on a second node and
  compare the results.
- **Safe retry:** a failed shard reruns and gives the same answer.
- **Replay and audit:** record the host replies, and a run can be replayed
  exactly for debugging or forensics. That fits the visibility motto.

## 4. Fuel as a cost model

Fuel is already the unit of work. It can also be the unit of admission and
accounting.

- A node can quote, cap or charge for foreign code before running it.
- Fuel budgets can compose: a job's budget is divided among the modules it
  fans out to.
- Programs always terminate, so a budget is a hard upper bound, not a timeout
  guess.

## 5. Fan-out and fold (map/reduce)

The list builtins (`each`, `where`, `fold`, `any`, `all`, `sum`) already take
functions and put the collection last.

- A coordinator ships one module to N nodes. Each node runs `each`/`where` next
  to its shard, and the typed CBOR results come back to be combined with `fold`.
- Lambda captures become the shipped arguments, and the function-type rules
  check them on both sides.

*Status: the builtins were implemented in the interpreter, which was removed
2026-10-05; bytecode support and remote execution are planned.*

## 6. Serializable machine state: pause, move, resume

This is potentially the biggest one. A CCL VM run is fully described by a small,
bounded state:

- the operand stack and locals;
- the instruction pointer;
- the remaining fuel;
- the pending host call, if the run is waiting for one.

It contains no native pointers, no host thread, no hidden heap. Objects are
indices into a bounded store, not addresses. If that state has a canonical
encoding (the same CBOR profile as modules), then a run can be:

- **checkpointed** and resumed after a crash, a reboot or an upgrade: durable
  jobs;
- **moved** to another machine mid-run, for example to the machine that holds
  the next shard of data, or away from one that is draining for maintenance;
- **handed off**: node A does part of the work, pauses at a host call, and node
  B finishes it;
- **suspended for a long time**: a workflow waiting days for a human approval is
  just a stored state and a pending host call.

It suits long-running agent missions, batch jobs, IaC rollouts that pause for
approval, and anything that must survive a restart.

**The hard part is resources.** Integers, Booleans, variants and object values
move freely. Live resources do not: open files, sockets, capabilities are
registry references meaningful on one machine only. CCL's ownership types
already make every resource explicit and linear, so the verifier knows exactly
which values are resources. Migration rules could therefore be precise:

- A run can move only at a point where it holds no live resources.
- Otherwise each resource must declare how it moves: closed and reacquired
  under the target's policy, or transferred by its owning service.
- The target's policy decides again what the moved run may do. Authority never
  travels inside the state.

The state must also be authenticated, since a forged state is a forged program
position. So it would be signed, and verified against the module's per-PC
stack types (which the verifier already computes) before resuming.

*Status: not implemented. The VM already keeps the state bounded and
pointer-free, which is the property that usually makes this impossible
elsewhere.*

### Has this been done?

Pieces of it, many times. Rarely all together, and rarely safely.

- **Operating-system process migration:** Sprite (Berkeley), MOSIX/openMosix,
  Condor checkpointing, and today CRIU for Linux containers. Machine-level:
  live VM migration (Xen, VMware vMotion). These move raw memory and fight
  native pointers, kernel state and open file descriptors. They are powerful
  but heavyweight, and they cannot reason about what the program means.
- **Mobile agents (1990s):** General Magic's **Telescript** had a `go`
  instruction that moved a running agent, with its execution state, to another
  "place". It is the closest ancestor of this idea. Also IBM Aglets and other
  Java agents (these moved code and data but not the running stack), and
  **Emerald** (1980s), which migrated live objects and threads. They faded
  mostly over security (running foreign code safely) and over resources that do
  not move.
- **Serializable continuations:** Stackless Python pickled running tasklets
  (used in EVE Online). Termite Scheme migrated processes as continuations.
  Seaside (Smalltalk) stored web-session continuations.
- **Durable execution, the modern form:** Temporal, Azure Durable Functions,
  Restate. These do not serialize a machine state; they record every effect
  and deterministically replay the workflow code to rebuild it. That works only
  because effects are explicit, which is the same discipline CCL has.
  Golem Cloud does durable WebAssembly by snapshotting and replay.
- **Code shipping without state:** Erlang sends functions and spawns processes
  remotely but does not migrate a running process. eBPF ships verified programs
  into the kernel but has no pause or resume.

What would be new in CCL is the combination:

- a proved verifier;
- termination by construction, and fuel;
- linear resource types, which make "what can move" decidable;
- deterministic effects;
- a bounded, pointer-free state with a canonical encoding;
- capability-based authority re-decided at every hop.

## 7. A verified JIT, eventually

Verification once can replace checks at run time. See "Future: a verified JIT"
in [the bytecode format](ccl-bytecode-format.md#future-a-verified-jit-not-planned-soon).
It is a long way off; the VM covers current needs.

## 8. One encoding for code and data

With v8, a module, its constants, its arguments, its results, a paused state and
a stored object would all be the same strict CBOR profile. The same proved
reader checks all of them, the Observatory can inspect all of them, and digests
mean the same thing everywhere.

## Constraints that keep these doors open

Check every new opcode and VM feature against these:

1. **Determinism:** no new source of nondeterminism except through a declared
   host call.
2. **Serializable state:** no native pointers, host handles or unbounded
   structures inside the VM state. Resources stay explicit, typed and linear.
3. **One canonical encoding** for modules, values and (eventually) states.
4. **Termination:** keep jumps forward-only. Bounded iteration goes through
   fuel-charged builtins.
5. **Static call targets:** function values index the module's own finite
   function table.
6. **Authority is never carried by code or state:** it is always granted by the
   executing node's policy.
