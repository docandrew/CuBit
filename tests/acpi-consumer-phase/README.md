# Provider prefix and consumer preparation

Export TMPDIR=/home/doc/cubit-build-tmp before Nix, then run `bash tests/acpi-consumer-phase/run.sh`. Builds and results use an isolated output directory.

The actual Core executes root/_SB initializers, selected provider prerequisites and ancestor closure, scoped REG activation, one-shot consumer preparation and deferred initializers. An earlier consumer must wait for confirmed registration and admission. The preparation callback copies three actual initializer invocations into the GPIO v2 codec before admitting deferred work.

The two prefix-error cases are deliberately different:

- `prefix-error` divides by zero before writing OSYS. The ordinary INI error permits the registration attempt, but REG then reads an uninitialized local because OSYS is unset. The outcome is Failed; no consumer admission occurs, ECAV's partial write remains observable, and failed registration is never forced disconnected.
- `prefix-ready-error` writes OSYS and then divides by zero. REG succeeds using that observed state; the outcome is Completed_With_Errors, with the error retained and all deferred consumers admitted exactly once.

Fatal prefix, unknown classification, refused/overcharged preparation, failed REG and replay controls remain strict. AML32/64 runs cover every case. Six ACPICA comparisons independently check successful ordering and both prefix/REG cases, including preserved partial effects and the two distinct failure statuses.

Prerequisite classification is trusted lifecycle input, not static analysis of arbitrary AML. Controller identity/resources, physical fields, quiet/transport and admission are models. This is not the complete native GPIO owner composition. Full Core runs at -O0 -gnatp; no new SPARK proof, native execution or hardware result is claimed here. The companion ordering matrix covers the disabled/default generic path.
