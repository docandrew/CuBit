# Hosted buffer sizing

Run from the repository Nix shell:

```sh
python3 tests/acpi-hosted/run.py --group buffer-sizing --mode release
python3 tests/acpi-hosted/run.py --group buffer-sizing --mode checked
```

The explicit group preserves the existing default and `all` selection. It runs
five mains (layout, allocation, package data, runtime/static namespace, real-owner
execution), compiles thirteen adapters while verifying selected source paths,
and replays eighteen cached ACPICA cases after raw-evidence, protocol and guard
checks. Compile-only adapters are not runtime tests. Each build is bounded to
180 seconds, each main and replay child to 30 seconds, runtime stacks to 64 MiB,
and captured logs to 1 MiB. Input hashes include every project source directory
and the complete reference manifest. The reference retains fourteen integer
flows and four explicitly mapped error cases; it does not claim exact ACPI error
code equivalence for those projections.

The underlying larger-buffer paths have hosted regression evidence, not a SPARK
proof or native deployment claim. The public group passes all 28 stages in both strict profiles: 9,077 main
checks, 2,448 protocol checks, 15 guard checks and 18 ACPICA comparisons per
profile. These results establish hosted behavior for the covered cases.
Runtime operand count expressions have a separate [dynamic Buffer group](../aml-buffer-dynamic/REGISTRATION.md). Static-loader and nested-package count paths
remain unfinished. The independent FieldData 1024-byte limit remains; arena
capacity also bounds allocation.
