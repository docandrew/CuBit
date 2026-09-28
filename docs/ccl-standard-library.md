# CCL standard library

Status: design notes (2026-09-27). Nothing here is implemented. The survey
of today's CCL is summarized under "Where CCL is today".

CuBit does not follow Unix's one-binary-per-tool model. The CCL REPL is
the shell, and in the busybox spirit it carries its tools with it. It
therefore needs typed replacements for curl, jq, sort, grep, base64 and
their kind. Nushell is the model: commands take and return structured
values, and text appears only when the REPL renders a value.

## Principles

- **Values, not text.** `fetch` returns a record (status, headers, body),
  and `decode json` returns CCL lists, records and scalars. Filtering and
  projection are ordinary CCL, so there is no jq-style second language.
- **URLs in, locators behind.** People and scripts write ordinary URLs
  (`https://example.com/a?b=1`). The library parses them into a typed
  `Locator<Https>` (host, port, path, query), and the checker, the effect
  set and autocomplete all work on that typed value. The locator form
  (`@https:example.com:443/a`) remains the system's internal name
  (security-model.md, "Names and locators").
- **Effects are declared.** Pure functions (decode, strings, lists) need
  no authority. `fetch` has the effect `Connect<Net:host:port>`, so a
  program is rejected at check time, not partway through a run, if it
  lacks that authority. The REPL session's authority and a program's
  manifest decide what is granted; the library itself grants nothing.
- **Bounded.** Every function states its limits (response bytes, nesting
  depth, element counts), consistent with CCL's bounded values. A limit
  exceeded is a typed error (`Too_Large`), never a truncated value.
- **Proved parsers.** Anything that reads untrusted bytes (URL, HTTP/1.1
  response, JSON, CSV) is a SPARK unit proved free of run-time errors,
  with its acceptance rule stated in its contract. This follows the
  netstack's codecs.

## Where it lives

Recommended: **the library is CCL catalog interfaces, served by Ada
adapters, with pure functions as built-in forms in the shared frontend.**

- **Pure functions** (`decode`, `encode`, string and list functions) are
  built-in forms. Today these are hand-written parser keywords
  (`concat`, `length`, `at`). They should become a **table of built-in
  functions**: each entry gives a name, a signature, an effect set (empty)
  and an evaluator. The checker, the tree interpreter, the bytecode
  compiler and REPL completion all read the same table, so a function is
  added in one place. Source: `userspace/ccl/src/ccl-builtins*.ad[sb]`.
- **Effectful functions** (`fetch`, later `resolve`, `listen`, file and
  config access) are **catalog interfaces** (`userspace/ccl/interfaces/
  *.ccl-interface`) with authority class `Network` (or `Control`,
  `Observe`). The host installs the grant; an adapter serves the call.
  This reuses what already exists: descriptors, grants, `Link_Program`,
  host-call suspension in the VM, and completion. Completion already
  lists catalog operations, so the library shows up in the REPL with no
  extra work.
- **Adapters** live beside the hosts that grant them: the Workbench
  (`apps/ccl-workbench`), `ccl-control` and `ccl-run` on Linux. A shared
  adapter package (`userspace/ccl/stdlib/`) implements each operation once
  over the runtime (`CuBit.Net_Channels`, the TLS service). Hosts only
  choose which grants to install.
- **Not a separate service process at first.** An HTTP client service
  would add an IPC hop for every request and a second authority check.
  An in-process adapter, holding the network authority the host was
  granted, is simpler. A shared `fetch` service can come later if several
  programs need one connection pool.

## First functions

| Function | Signature (sketch) | Effect |
| --- | --- | --- |
| `url` | `String -> Result<Locator<Https>, Url_Error>` | none |
| `fetch` | `Locator<Https> * Request -> Task<Result<Response, Fetch_Error>>` | `Connect<Net>` |
| `decode` | `Format * Bytes -> Result<Value, Decode_Error>`; `Format` is `json`, `cbor` or `csv` | none |
| `encode` | `Format * Value -> Result<Bytes, Encode_Error>` | none |
| `get` | `Value * Path -> Option<Value>` | none |
| `where`, `select`, `sort-by`, `first`, `length` | Nushell-style, over lists of records | none |
| `lines`, `split`, `trim`, `matches` | strings | none |
| `base64`, `hex`, `sha256` | bytes | none |

`fetch` would accept a `String` URL and apply `url` implicitly, so
`fetch "https://example.com/x.json" | decode json | get items` reads as
it would in Nushell. `Response` holds a status (a variant of named
codes), headers (a bounded list of name/value records) and a body
(`Bytes` up to a stated limit, or a bounded `Stream<Bytes>` once streams
exist).

## Where CCL is today

(From a survey of `userspace/ccl`.)

- **Frontend:** one shared frontend (`ccl-language`) feeds a tree
  interpreter (strings, functions, records, variants) and a bytecode VM
  (integers, booleans, variants only; no text, no call frames).
- **Built-ins:** hand-written parser keywords, each its own node kind.
- **Host operations:**
  - They come through catalog descriptors plus separately installed
    grants, with the VM suspending on a host call.
  - Each takes at most one argument.
  - The runtime descriptors are hand-written Ada that mirrors the
    `.ccl-interface` text.
- **Limits:** 1,024 bytes of source, 128 AST nodes, a 1,024-byte text
  arena, 256 value cells.
- **Missing:**
  - value types: lists, maps, byte strings, Option/Result (except as
    user-defined variants);
  - JSON and modules;
  - `Task`/`await` (documented, not implemented).
- **CBOR:** the only codec is the external `cbor_ada` package, in
  `ccl-control`'s wire format.
- **Proofs:** the core is SPARK and much of it is proved. Obligations
  remain open in the VM execution loop, the scheduler and the v3
  bytecode format.

## What must come first

1. **Value model:**
   - bounded `List<T>`, `Bytes`, `Option`/`Result`;
   - a dynamic `Value` for decoded documents, i.e. a closed variant of
     null, boolean, integer, text, list and record;
   - larger, still bounded arenas, sized by typed launch parameters
     (ccl-launch-parameters.md) rather than fixed at 1 KB.
2. **The built-in function table,** replacing the keyword chain, used by
   the checker, both evaluators and completion.
3. **Bytecode VM:** text values and call frames, so library functions
   run in both evaluators; otherwise the library is interpreter-only.
4. **Host operations** with more than one argument (`fetch` takes a
   locator and a request).
5. **`Task` for effectful calls.** The VM's host-call suspension is most
   of it; the interpreter's host calls are synchronous today.
6. **Proved parsers** for URLs, HTTP/1.1 responses and JSON, in
   `userspace/net/src` style.

Steps 1–3 are general CCL work, needed for more than the library. Step 6
can start independently.

## Open questions

- Should `Value` (the dynamic document type) be one closed variant, or
  should `decode` require a target type (`decode json as Config`) so that
  most programs never see an untyped value? Nushell is dynamic; CCL
  leans typed. A reasonable middle: `decode` returns `Value`, and
  `as T` checks a `Value` against a declared type.
- HTTP scope: HTTP/1.1 over the TLS service first, with no HTTP/2,
  cookies or automatic redirects across origins. Is a same-origin
  redirect followed by default?
- Should Linux-hosted `ccl-run` get `fetch` (over sockets), or is `fetch`
  CuBit-only?
