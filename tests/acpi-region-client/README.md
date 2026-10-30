# Hosted named-register client tests

In the pinned Nix shell with TMPDIR set as required by coordination/acpi.md:

```sh
nice -n19 python3 tests/acpi-region-client/run.py --mode release
nice -n19 python3 tests/acpi-region-client/run.py --mode checked
```

The saved runner checks exact sources/tools, fatal compiler warnings, compiled
ALI dependencies, 180-second single-worker builds, 30-second runtimes, 64 MiB
stack and bounded logs. `--validate-only` checks registration without compiling.
There is no native build or inline Python snippet.

The fixture uses the real Region_IO dispatcher and Region_Policy implementation
behind an explicit hosted transport. It checks full-width IDs/values, canonical
reply decoding, invalid envelopes, no-effect local rejections, endpoint lifetime,
finite deadlines, known denial, completed backend faults, lost/malformed replies,
backend unknown completion, confirmation/retirement without replay, and receipt
ownership between live contexts. Physical access, IPC provenance, native deadline
enforcement and authority provisioning remain modeled boundaries. This is hosted
runtime evidence, not proof of the client or a native driver acceptance result.
