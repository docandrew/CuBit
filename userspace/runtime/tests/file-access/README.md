# Bounded filesystem-policy tests

These run on Linux using the Nix toolchain. They are not CuBit applications.
Assertions are enabled in the host test harness, not in the native runtime.

```sh
nix develop -c make -C kernel test-filesystem-policy prove-filesystem-policy
```

The targets cover both `CuBit.Directory_Paths` and `CuBit.File_Access`.
The latter keeps rights as an enum-indexed Boolean set and scopes in a private,
bounded policy record. Tests cover every wire rights value, every supported
nonempty requested-rights combination, component-boundary matching, malformed
records, invalid names, rejected partial policies, and non-1/extreme String bounds.

GNATprove checks the actual pure implementation. The policy decoder's failed
result is proved empty; `Clear` is proved empty. `Allows` is defined directly
as a valid-path, nonempty-rights, matching-entry decision, without merging rights
from independent entries. Proof uses ordinary loops rather than expanding all
16 records into a large proof term. No assumptions, SPARK-Off sections, or
runtime-only substitutes for the proof are used in these two packages.

The native service's IPC, imported memory, policy publication, caller identity,
and media handling are **not** proved by these host projects. Exercise those with:

```sh
nix develop -c make -C kernel filesystem procmgr files storage-check
nix develop -c tests/headless/run.sh --test storage-grants --accel kvm --timeout 40
nix develop -c tests/headless/run.sh --test files --accel kvm --timeout 40
```

Tests operate on a temporary copy of the fixture disk. They do not grant the
Files application write access or touch the interactive desktop's data image.
