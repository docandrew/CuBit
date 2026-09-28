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
   - `:name` is a new token kind: a colon followed by a symbol. It is valid
     only in argument position, so it cannot be mistaken for a value.
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
