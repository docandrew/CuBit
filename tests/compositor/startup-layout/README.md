# Desktop startup layout admission

Run in the pinned Nix environment from the repository root, using the kernel's
Alire toolchain:

```sh
cd kernel
alr exec -- gprbuild -p -P ../tests/compositor/startup-layout/test.gpr
../tests/compositor/startup-layout/build/layout_tests
alr exec -- gnatprove -P ../tests/compositor/startup-layout/test.gpr -u desktop_startup_layout.adb --mode=all --level=2 --report=all
```

The hosted tests exercise zero dimensions, supported 1080p, exact byte limits,
unsupported image edges and unsigned inputs whose old multiplication wrapped to
small byte counts. The proof establishes the numeric rejection/byte-count
contract. Neither establishes GPU allocation, physical mapping authority,
hardware presentation or latency. Epoch admission and startup stage ordering
are separate Desktop_Renderer_Startup contracts.
