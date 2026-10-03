# CopyObject oracle

Run `acpica_copyobject.py` in the pinned Nix environment with `--tools` pointing
to ACPICA's bin directory and `--output` pointing to an isolated output folder.
An explicit mode is required: `--reference-only` or `--runner /path/to/table_runner`.
The latter is a strict differential check: unsupported operations fail.

The 42 reference observations cover both integer widths, destination type
replacement, independent nested data, argument/local replacement, self-copy,
empty buffers, partially initialized packages, and expression-result independence.
Each method executes in a fresh ACPICA process. Logs and input/table hashes are
retained. Only the exact known ACPICA shutdown allocation diagnostic is allowed,
and only after the control evaluation reproduces it.

All 42 reference observations passed with ACPICA 20260408. The isolated CuBit
Debug/clock runner fails the first TEXT case with UNSUPPORTED, as expected while
CopyObject is unimplemented. This suite is therefore not registered in the main
passing regression runner yet. Reference-only results are not CuBit conformance.

ALIA verifies that mutating the captured CopyObject expression result leaves its
named destination unchanged. ACPICA's AcpiExStoreDirectToNode copies on attachment;
sharing one mutable arena object for both results would fail this case.
References, cyclic graphs, resource exhaustion and object lifetimes require
additional coverage and formal contracts before full integration.

Reference-target cases confirm that CopyObject through Arg0 holding RefOf
changes the referenced named object, while a Local containing RefOf is replaced.
An Arg containing an Index reference is replaced without writing the package
element. These six observations (both widths) are part of the 42-case oracle;
they remain requirements for the future mutable-reference implementation.

NEST and PKGM mutate through nested Index operations. Assigning the nested
buffer/package to a Local first can copy it and hide aliasing.
BASE and SRCO are positive alias controls: modifying the first source package
entry changes the second entry and named source buffer (result 9).
REPT and ORIG modify the copied package directly and require the other copied
entry and source buffer to remain unchanged (result 1). Both widths pass with
ACPICA 20260408. CuBit CopyObject integration is still pending.
