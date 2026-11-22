# Saved laptop AML replay

Inside the pinned Nix shell, with `TMPDIR=/home/doc/cubit-build-tmp`, run:

```
bash tests/aml-laptop-replay/run.sh /absolute/capture.json
```

The script holds the shared build lock for the timing gate, compiles an isolated
release closure (`-O2 -gnatp`, j1), and checks five executions. It verifies exact
full semantic counters and uses a capture-specific median execution-time budget.
The accepted Lenovo partial replay median was 0.624195207 seconds; its initial
budget is 0.95 seconds (about 52% margin). This replaces the old 70 ms target.
Build wall limit180s, execution30s, per-process address space24GiB, stack64MiB,
core dumps disabled, output1MiB, nice19. Checked whole-Core builds are excluded.

`--functional-only` runs one execution with no timing gate or shared lock; it is
safe for independent hosted functionality work with its own output directory.
A busy host can spoil a timing measurement: inspect contention before changing
an established budget. The lock excludes coordinated heavy work, not all host
activity. No native, kernel, image or QEMU build occurs.

Capture JSON contains `tables` (ordered entries with positive `id`, `path`, and
SHA256), `expected_semantics` (exact `result `, `synthetic host`, `failed-field`
lines), and `budget_seconds`. Paths resolve relative to the JSON file. Preserve
firmware table order and hashes; a changed failure boundary requires an explicit
fixture review. The capture belongs to its machine and is supplied separately;
firmware dumps are not included in this suite. The harness models selected
register reads and does not complete laptop `_INI`, power or input bring-up.
