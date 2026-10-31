# Registered execution sessions

Inside pinned Nix with TMPDIR=/home/doc/cubit-build-tmp exported before entering, run bash tests/aml-registered-sessions/run-active.sh and run-idle.sh. Both use ACPICA-compiled fixtures, fresh checked/release builds, both AML widths and private output directories. Inputs and tools are hashed before and after every command.

Active cases cover direct/named invocation and direct/named/single/unbound Field paths. Idle cases cover nested selected-root loading, parent/child errors and recovery, self-load, provider refusal, duplicate handling, preserved caller pins and operation budgets. These are hosted tests. After terminal module failure, authenticated caller pins can be released while idle. Retained-result reads and AML execution remain closed; full owner disposal and recovery remain unfinished.

See docs/acpi-registered-loading.md for architecture, scoped proof and remaining compatibility/native gaps.

For six independent normal ACPICA controls, run bash tests/aml-registered-sessions/run-reference.sh /absolute/output/from/run-idle. These assert common visible values and declaration-owner groups for success and parent/child recovery at both widths. They do not equate the owner helper API/replay contract with AML LoadTable. Existing ACPICA cache diagnostics remain recorded in the logs.

Terminal module failure permits authenticated caller-pin release while retained reads and AML execution stay closed. The idle suite checks foreign/duplicate/empty handles and preserved failed phase/namespace/definition state. For a real synchronous Stall callback attempting cleanup during execution, run bash tests/aml-registered-sessions/run-busy.sh in pinned Nix. Both profiles and AML widths require Busy, unchanged handle and exact before/after pin count; the interpreter can hold a temporary pin during the call. This tests local library reentry, not IPC or native behavior.
