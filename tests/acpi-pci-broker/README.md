# Hosted PCI broker and ECAM exclusions

Export TMPDIR before entering the Nix development environment:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop
bash tests/acpi-pci-broker/run.sh
```

The suite requires Node, GNAT/gprbuild and GNU timeout. It uses one compiler job, private output directories, a 64 MiB stack, no core dumps, 180 seconds per build, 30 seconds per executable and 1 MiB of captured output per command. It needs no Git history, private scratch files, native build, image or QEMU.

`sources.json` pins every production and fixture input. Source mismatches fail before compilation; inputs and copies are checked again around commands. Output, logs and RESULT.json are written only in a fresh TMPDIR directory. Production and fixture hashes must be reviewed together when the implementation changes. The runner never updates hashes or edits the checkout.

Checked/assertion/overflow and release profiles exercise the actual broker, MCFG decoder and traversal, window policy, opaque process IDs, read-only H.Copy derivations and growable claim ledger. Kernel snapshot reads, root descriptions, heap refusal and IPC outcomes are modeled in fixtures. The typed fixture constructors create model handles only. This is Linux-hosted execution, not live CuBit capability, mapping or hardware validation, and not a SPARK proof.

Coverage includes every bus, complete copying across chunks, named read-only grants, duplicate IDs and delivery uncertainty; 300 allocation exclusions beyond the first 4 KiB; partial second-chunk, short-copy, source, duplicate-table and heap failures; header-policy rejection without callbacks; late invalid allocations and retention refusal. A callback prefix is unusable unless the final collection status succeeds. Main's integration and launcher changes require separate semantic and native gates; this executable does not run their event loops.
