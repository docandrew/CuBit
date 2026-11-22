# ACPI root export admission

Inside the Nix development environment, with `TMPDIR` exported before entry:

```sh
bash tests/acpi-root-export/run.sh
bash tests/acpi-root-export/run.sh --prove
```

The runner copies hash-pinned sources into a unique temporary directory. It
needs Node, GPRbuild and GNU timeout; `--prove` also needs GNATprove, cvc5 and
Z3 on PATH. No Git checkout, shared build outputs, kernel build or private
workspace is needed. Build/test/proof timeouts and captured output are bounded.

Both checked/overflow and release builds exercise every device/firmware split
at the launch block's derived root ceiling and one above it, plus zero and
extreme demands. Admission must preserve both counts exactly; refusal must
return neither kind of root. The SPARK postcondition proves the same property
for the complete 64-bit input domain. The ceiling is derived from
`Launch_Configuration.Maximum_Roots`; this suite does not choose a larger limit.

These are policy tests and a policy proof. They do not execute kernel startup,
test capability placement, validate the boot-panel rendering, or establish
native/hardware behavior. The boot-timeline suite separately tests the named
failure stage. Native startup validation remains a separate gate.

The result receipt is printed on completion and retained alongside logs and
copied inputs under TMPDIR, including on failure. Refresh the source manifest
when intentionally changing an input; mismatches fail before compilation.
