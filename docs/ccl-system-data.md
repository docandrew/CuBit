# CCL system data: files, processes and services in the REPL

**Status: proposal for review (2026-09-30).** Nothing here is implemented.
The REPL can compute, keep definitions, and save them to its workspace; it
cannot yet see the system. This is the next step toward retiring `shell.app`
(see [the REPL design](ccl-repl.md), phase 3).

## What people will type

```
processes() | where(FUNCTION(p) p.memory > 10000000) | sort-by(FUNCTION(p) p.memory)
files("@nvme:0/work") | where(FUNCTION(f) ends-with(".ccl", f.name)) | length
services() | where(FUNCTION(s) s.state = State.Faulted)
read-text("@nvme:0/work/notes.txt") | split("\n") | count(FUNCTION(l) contains("TODO", l))
```

Each source is a host operation returning a typed list of records, so every
builtin and pipeline stage already works on it.

## Principles

1. **Authority is explicit.** Each source is a separate interface
   (`sys.processes`, `fs.list`, `fs.read-text`, `svc.list`), published only
   where the session holds the matching grant. The Workbench manifest
   declares which it requests. The REPL shows what it has (`:env` could list
   granted interfaces).
2. **Observe before control.** First interfaces are read-only. `kill`,
   `write`, `restart` come later as separate, individually granted
   operations, and use the propose/approve flow from
   [agent security](agent-security.md) when an agent issues them.
3. **Scoped paths.** File operations take a volume-qualified path and are
   checked by the filesystem service against the session's scopes (today the
   Workbench has `@nvme:0/work` and `@mem:0/work`). No ambient root.
4. **Typed records, not text.** A process is a record (`pid`, `name`,
   `state`, `memory`, `cpu-time`), a file is (`name`, `kind`, `size`,
   `modified`). This needs host operations that return lists of records.
   Today they return a single scalar, text or object, so this is the main
   language work.
5. **Bounded.** Results are bounded lists (4,096 elements); larger sources
   need paging arguments or a stream (phase 2, `cmd:stream`).

## Work, in order

1. **Host results that are lists of records.** Extend `CCL.Host_Values` and
   the VM's host-import path. Records already exist as typed objects;
   lists of them need an element representation in the list region.
2. **Multi-argument host operations** (`read-text path limit`), already
   listed in the standard-library doc.
3. **`sys.processes`**, backed by procmgr's process list (read-only, a new
   observe grant).
4. **`fs.list` / `fs.read-text`**, backed by the filesystem service, within
   the session's scopes.
5. **`svc.list`**, from devmgr's service table.
6. **The Observatory** renders lists of records as tables, since the wire
   already carries typed lists.

## Decisions needed

- Record field access syntax: `p.memory` (BASIC) / `(field p memory)` (Lisp)
  exists for records. Is that the spelling to use in pipelines?
- Whether read-only system observation is granted to the Workbench by
  default, or requested per session.
- Whether file reads return `String` (bounded, 8 KiB) now and `Bytes` later.
