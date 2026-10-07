# Typed manifests (design and plan, 2026-10-02)

Status: phase 1 done (2026-10-02). The typed frontend, converter and
whole-tree comparison have landed. Phase 2, rewriting every manifest and
deleting the keyword reader, comes next, in an announced window.

Each `manifest.ccl` becomes an ordinary CCL expression whose value is an
`Executable_Manifest`. `CCL.Manifests` was a hand-written keyword reader,
frozen on 2026-09-30 ("no more special cases"). Its replacement checks the
manifest with the type checker, then reads the checked value. The ELF
sections it writes stay byte for byte the same. The first new field is
`may_launch`, which `.cubit.launch` needs (docs/process-arguments.md).

## Language groundwork (landed)

- **Record field defaults:** `(type Limits (record (name String) (connections Count 1024)))`.
  - A default is a constant of its field's type: an Integer within its range, `true`/`false`, an enum member (`Mode.Fast`), or `[]`.
  - Defaults are kept beside the type (`CCL.Types.Set_Default`/`Default_Of`), not in its description. They are not part of its layout, encoding or identity. `Import_Definition` carries them, and an import whose defaults differ is a conflicting definition.
- **Named construction:** Ada's named association.
  - `(Limits name => "a" connections => 7)`. Positional fields come first, and a positional value after a named one is `Positional_After_Named`.
  - BASIC spells it `Limits(name => "a", connections => 7)` and declares a default as `connections AS Count := 1024`, as an Ada record does.
  - `=>` is free in both notations (lambdas are `fn`/`FUNCTION`), and it is recognized only after a field name in a record construction.
  - A field left out takes its default. Leaving out a field that has none is `Missing_Field_Argument`. An unknown name is `Unknown_Field_Argument`, and a field given twice is `Repeated_Field_Argument`.
- **Parity:** the parser rewrites a named construction into the positional one, filling in defaults as literals. The compiler, verifier and VM therefore see only what they already handle. The node records which fields were named and which were defaulted, so both views print the source's spelling.
- **Tested:** `tests/ccl-types/record_default_tests.adb` runs the BASIC round trip, compiler, verifier, VM and CCLB round trip.

## The manifest type (proposed)

Requests stay **one ordered list**. Capability slots are assigned in request order across kinds, so grouping by kind would change the bytes. Scopes and streams are also kept in order.

```
Executable_Manifest = (identity: String, version: String,
                       requests: List(Request) = [], scopes: List(Scope) = [],
                       streams: List(Stream) = [], device: Device_Match = Device_Match.None,
                       may_launch: List(Launch) = [])
Request = Service(Service_Request) | Notification(Notification_Request)
        | Network(Network_Request) | Framebuffer(String) | Render(String)
        | Device_Memory(...) | IO_Ports(...) | Interrupt(...) | DMA(...) | Scheduling(...)
Service_Request = (service: String, access: Access = Access.Read_Write, binding: String)
Scope = Filesystem(File_Scope) | Config(Config_Scope) | TLS(String)
File_Scope = (rights: List(File_Right), under: Scope_Place)     -- Scope_Place = Everything | Under(String)
Launch = (program: String, invoked_as: List(String) = [])
```

- **Catalog names:** a service or notification name is checked against the build's service catalog when the manifest is compiled. A typo is a build error that lists the catalog's names. It is a String, not an enum, because the catalog has more than 16 services.
- **Path and network rules:** the old reader's rules (path traversal, IPv4 dotted decimal, scheduling admissibility, device resource bounds) are kept as checks on the value. Failures are explained through `CuBit.Failures`.

`tls.svc` today:

```lisp
(executable-manifest v1
  (identity "com.cubit.tls")
  (version "0.1.0")
  (request-service config read-write config)
  (filesystem-scope (rights read) "@nvme:0/tls/"))
```

`tls.svc` typed (exact spelling to settle with the first conversions):

```lisp
(Executable_Manifest
  identity => "com.cubit.tls"
  version => "0.1.0"
  requests => [(service "config" "config")
               (service "clock" "clock")]
  scopes => [(filesystem [File_Right.Read] "@nvme:0/tls/")])
```

`service`, `filesystem` and friends are ordinary CCL functions in a small manifest prelude, smart constructors over the typed records. They are not keywords.

## Phase 1 (landed)

- **One declaration, one encoder.** `CCL.Manifests.Model` holds the checked declaration and catalog plus the shared rules (metadata text, binding names, scope paths). `CCL.Manifests.Encoding` assigns slots and writes every section. The keyword reader (`CCL.Manifests.Keywords`) and the typed frontend (`CCL.Manifests.Typed`) both fill the same declaration. `CCL.Manifests.Compile` sends sources that begin with `(Executable_Manifest` to the typed frontend and everything else to the keyword reader.
- **The schema is CCL:** `userspace/ccl/interfaces/executable-manifest.ccl` holds the 33 `(type …)` declarations and 16 constructor `define`s, and nothing else defines them. There is no Ada mirror and no separate `.schema` text.
  - The tool prepends the schema to each manifest, so the type checker checks the manifest against those declarations. The result is bound under an in-process key and read **by field and alternative name** (`Named`, `Alt`), so the CCL file alone decides order and spelling.
  - The tool finds the schema with `--schema FILE`, or in `interfaces/` beside the catalogs directory.
  - 33 types exceed the old 32, so `CCL.Types.Maximum_Declarations` rose to 48. The native schema image keeps its own 32-definition wire size (`Maximum_Image_Definitions`), so config IPC frames are unchanged.
- **Language fixes found on the way:**
  - Top-level declarations no longer count as nesting. A 49-declaration program hit the depth limit of 32; the parser, checker, evaluator and compiler now pass the same depth along the declaration chain.
  - That also fixes type-system quirk Q-4: the result expression after declarations is checked at the root.
  - Qualified names still obey the 32-character limit, so the enum is `Notify_Access`.
- **Explained failures:** each failure names the entry, says what is wrong, and gives a fix:
  - `requests entry 2: the catalog has no service named "clok"; did you mean "clock"? Fix: write "clock".`
  - `line 13, column 18: "File_Right.Reed": no such name. Fix: use one of: File_Right.Read, …`
  - `line 1, column 2: field identity: This field has no default, …`
  
  The field name comes from a new language result, `Diagnostic_Subject`, which the console also shows.
- **Verified:**
  - `tests/ccl-manifests/compare-tree.py` compiles all 83 manifests against all 3 catalogs (249 pairs): with HEAD's keyword tool, and with this tree's tool on each manifest's typed conversion. Result: **0 mismatches** in sections, bindings and accept/reject.
  - The manifest suite has 34 tests, adding byte equality with the keyword form, named fields with defaults, explained failures, and `may_launch`.
  - All 29 hosted CCL suites pass.
- **`may_launch`:** emits `.cubit.launch` in the `CuBit.Launch_Authority` v1 layout, byte-equal to `tests/process-spawn/launch-table.py`. v1 has no aliases, so a non-empty `invoked_as` is refused with an explanation until the table's next version maps aliases to programs.

Prelude vocabulary:

| Old form | Typed form |
| --- | --- |
| `(request-service config read-write config)` | `(service "config" "config")` |
| `(request-notification mixer manage registration)` | `(notification "mixer" Notify_Access.Manage "registration")` |
| `(request-network tcp-connect (ipv4 "0.0.0.0" 0) (ports 1 65535) (dns allow) (connections 8) tcp)` | `(network Network_Action.TCP_Connect "0.0.0.0" 0 1 65535 true 8 "tcp")` |
| `(filesystem-scope (rights read) "@nvme:0/tls/")` | `(filesystem [File_Right.Read] "@nvme:0/tls/")` |
| `(config-scope (rights read) all)` | `(config-all [File_Right.Read])` |
| `(match-pci-class 4 3 0)` | `device => (Device_Match.PCI_Class (PCI_Class 4 3 0))` |
| `(interrupt device msix (vectors 1))` | `(interrupt "device" Interrupt_Mode.MSI_X 1)` |
| `(stream stdout text 4)` | `(Stream Stream_Kind.Standard_Output Stream_Format.Text 4)` |
| — | `may_launch => [(Launch "logstore.svc")]` |

`tests/ccl-manifests/convert-manifest.py` produces the typed form and keeps every comment with its declaration.

## Plan

1. **Language:** record defaults and named construction (done). `Maximum_Declarations` raised to 48 (done).
2. **Split `CCL.Manifests`** into the declaration and the encoder (done).
3. **Manifest interface and typed frontend** (done).
4. **Converter and comparison** (done: 0 mismatches over 249 pairs).
5. **Switch-over:** a lock window announced in `coordination/networking.md`. Rewrite every manifest, switch the Makefile, delete the keyword reader.
6. **`may_launch`:** emit `.cubit.launch` (done, v1 layout). Then port `tests/process-spawn` to a typed manifest and delete `launch-table.py`. A v2 table then maps `invoked_as` aliases to programs, which needs a change to procmgr and `CuBit.Launch_Authority`.
7. **Every interface in CCL:** fs, console, timer, logs and images move their types from Ada `Define_Types` into CCL declaration files that the host loads and publishes. Each schema key becomes a SHA-256 of the CCL text, which needs a small SPARK SHA-256 in the CCL tree.
8. **Refinements:** move the rules the type cannot state yet (names, paths, IPv4, bounds) into refinement types (docs/ccl-type-system.md, stage 1).
