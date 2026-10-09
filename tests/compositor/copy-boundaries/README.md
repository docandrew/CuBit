# Actual pixel-copy boundary regression

Run in the repository Nix environment:

```sh
python3 tests/compositor/copy-boundaries/run.py --toolchain-root "$PWD"
```

The runner copies current production units and records their hashes. A linker
wrapper counts calls and forwards to the real libc memcpy. Assertions cover
tight multirow batches, nonzero starting rows, padded destinations, batch-budget
rejection, end-of-image, overlap/null rejection and immutable source/canaries.
Tight batches require one memcpy; padded rows require separate calls.

This checks hosted foreign memory writes; it proves neither physical alias
exclusion nor GPU fence/mapping authority. SPARK row-layout contracts are
unchanged. The optimization reduces calls, not bytes copied. It leaves
production SPARK specifications On and the raw-memory body Off.
