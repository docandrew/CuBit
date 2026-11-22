# Instrument views: hosted tests

Linux-hosted tests of the toolkit's data views, each over a small synthetic
model (not CuBit runs): `CuBit.UI.Graphs` (a stream-wiring graph),
`CuBit.UI.Hex_Views`, `CuBit.UI.Timelines`, `CuBit.UI.Code_Views` and
`CuBit.UI.Tree_Tables`. From the repository root, inside `nix develop`, with
the hosted font library built (`make -C userspace/rust fonts-host`):

```sh
cd kernel && alr exec -- gprbuild -p -P ../userspace/lib/ui/tests/instrument/instrument_tests.gpr
for t in graphs hex_views timelines code_views tree_tables; do
  ../userspace/lib/ui/tests/instrument/build/${t}_tests
done
```

The Debugger (`userspace/apps/debugger`) is their first user; its own hosted
tests are in `tests/debugger-gui`.
