# Workbench output

The shared native CuBit / Linux-preview Workbench has a read-only Output pane.
It uses the toolkit `Output_Box` widget: monospace text, vertical/horizontal
scrollbars and Clear. It does not yet support selecting/copying output text.

From either source or REPL:

```lisp
(ui.output-append "Hello, Cubie!")
(ui.output-append (to-string (clock.monotonic-ms)))
(ui.output-clear)
```

`output-append` appends a string plus LF (even an empty string produces a line).
Each call accepts at most 1,024 bytes, as declared in the UI interface schema.
The host-owned buffer holds 16 KiB; overflow returns `false`, changes nothing,
and does not discard old text. Clear releases the buffered contents. Output
persists across runs, REPL submissions and Stop until explicitly cleared or
the Workbench exits. Appending follows the newest line; wheel/scrollbars allow
reviewing older lines. A callback example is `samples/button-output.ccl`.

These are explicitly granted embedded host operations, not compiler builtins,
filesystem access, system logging, or a global stdout/file descriptor. No
observer authority is needed to see this Workbench's own text. Other hosts can
reuse `CCL.UI_Outputs` independently of the toolkit and decide which script
instances share a sink. Separate processes/remote sinks still require native
IPC authority and are not implemented by this widget.

Verification (inside Nix):

```sh
nix develop -c bash tests/ccl-sessions/run-preview.sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/ccl-sessions/sessions_tests.gpr --subdirs=output-proof -u ccl-ui_outputs.adb --level=2 --report=all --checks-as-errors=on -j2'
```

The model proof checks absence of runtime errors. Interpreter tests cover
missing authority, wrong types, appending/persistence, exact capacity, atomic
rejection, non-one-based input and clear. SDL tests exercise the real shared
Workbench rendering. They are hosted tests, not evidence of IPC transport.
