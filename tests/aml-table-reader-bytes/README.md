# Owned table selector and Field-byte policy

Run `TMPDIR=/home/doc/cubit-build-tmp nix develop -c bash tests/aml-table-reader-bytes/run.sh`; `--validate-only` checks pinned sources without compiling. Two pure hosted mains run in checked/release profiles:251732 Field-reader and4378 selector checks per profile,512220 total checks in4 processes,11 source ALIs/profile.

Each profile compares42068 boot/copied Field views and83424 logical bytes with an independent bit-by-bit oracle, including shifted starts, partial final bytes, malformed extents, missing identities, out-of-range reads and zeroed refusal outputs. Selection checks filters, stable boot/copy precedence, duplicates, missing tables and invalid inventories. This finite matrix is regression evidence, not full functional proof or an AML coverage percentage. No hardware, native execution or acpiexec is used.
