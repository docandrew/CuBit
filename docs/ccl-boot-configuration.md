# Native CCL boot configuration

For the profile/context direction and current read-only CCL inspection slice,
see [Configuration contexts and inspection](config-contexts-and-inspection.md).
For the proposed installed-system startup dependency split, see
[Bootstrap storage and Config](boot-storage-and-config.md). This is distinct from
the current authoritative seed mechanism described below.
The desired/active/application-state split and next activation steps are in
[declarative configuration and mutable state](config-declarative-state.md).

CuBit evaluates configuration source **inside userspace during boot**. GRUB
and the kernel do not contain the CCL interpreter. Linux is not required to
translate configuration into another language before an image can boot.

## Evaluation and effects

The shared `CCL.Configurations` frontend uses the real CCL interpreter to
evaluate declaration fields and returns an owned, bounded configuration plan.
It has no host adapter: expressions cannot access files, clocks, networking,
launch processes, or mint authority.

- `devmgr.svc` reads `system.ccl` from the bootstrap CPIO archive, validates
  the whole system plan, then seeds config.svc using native grant-backed IPC.
- `procmgr.svc` reads `init.ccl` through filesystem.svc, validates the whole
  startup plan, then launches its entries in order using existing launch policy.
  The plan owns its names before ELF loading reuses the source buffer.
- Images carry the CCL sources directly. The same pure frontend is available
  on Linux as `ccl-config` for preflight and plan inspection.

Invalid profiles produce diagnostics and no effects from that profile.
A missing, oversized, or invalid system profile stops devmgr boot progression;
a rejected startup profile launches none of its entries. There is no fallback
parser for legacy `.conf` syntax. This is validation-before-effects, **not**
transactional rollback of successful IPC operations or previously booted drivers.
Launch failures after validation retain the existing per-entry reporting.

## Current syntax

The header uses the symbolic format tag `v1`, not a numeric version expression.
The shared `Format_Version` enum selects the supported declaration grammar;
unknown tags and old numeric headers are rejected.

`setting` declares an entry in the resulting plan; it does not immediately
write Config. The old `set` spelling is rejected, not retained as an alias.
There is no unrestricted runtime `config.write` operation added by this syntax
change. A future runtime Config client must explicitly hold the relevant write
authority. Default-versus-managed semantics and service-advertised schemas
remain design work; do not infer them from the noun `setting` alone.

```lisp
(system-config v1
  (setting "example.title" "CuBit")
  (setting "example.name" (concat "Cu" "Bit"))
  (setting "example.count" (* 6 7))
  (setting "example.enabled" true))
```

Keys are generic, not hard-coded Desktop/Filesystem concepts. Values are CCL
strings, integers, or booleans; integers and booleans become canonical text
because config.svc currently stores bytes, not schema-tagged CCL values.

```lisp
(startup v1
  (start "clock.svc" (priority 5))
  (start "ccl-control.app" (priority (+ 2 3))
    (network approve-declared)))
```

Use actual packaged executable names. Priority is required and constrained to
1..10. Network approval is an enum: omitted or `deny` means No_Network;
`approve-declared` selects the existing Declared_Network launch ceiling.
It is a trusted boot-plan decision, not a capability request, source-capability
delegation, or permission for arbitrary CCL to grant authority. This migration
does not repair or broaden procmgr's existing trusted minting powers.

The live profile deliberately omits `config.store`, retaining its no-internal-
disk persistence behavior. Authenticating boot artifacts and introducing a
policy service remain separate security work.

## Bounds and validation

Source: 8192 bytes. System plan: 128 unique keys (128 bytes each), values up to
1024 printable ASCII bytes. Startup: 16 ordered launches, names up to 64 bytes.
Repeated launches are allowed; duplicate settings/fields, unknown declarations,
empty profiles, and trailing input are rejected. Filenames cannot contain
paths, device schemes, or embedded launch options.

Each field uses the existing language parser/type checker and a 1024-unit
execution budget; its existing source, AST, text, and nesting bounds also apply.
No unbounded evaluation, external imports, or effectful host calls are enabled.
The frontend is SPARK-mode code without Assume or SPARK-Off sections, but these
new units have **not yet completed a GNATprove proof gate**. Native adapters are
not thereby proved correct.

## Host checks

```sh
nix develop -c make -C kernel test-ccl-configurations
nix develop -c python3 tests/ccl-configurations/test-native-rejection.py
userspace/ccl/build/config/ccl-config system.ccl
userspace/ccl/build/config/ccl-config init.ccl --dump-plan
```

Normal success is silent. The optional dump is for people and regression
comparisons, not an artifact consumed by CuBit. Legacy profiles exist only in
`tests/ccl-configurations/fixtures` to verify migration equivalence.

This is the boot-configuration slice of the package plan. Declarative ISO
membership, build graph realization, installation approvals, signatures, and
runtime policy brokering remain follow-on work.
