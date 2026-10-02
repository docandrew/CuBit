# Mesa in CuBit

This bundle accompanies the CuBit native Mesa software-rendering demo.
`MESA-VERSION` identifies the upstream version; `SOURCE.nix` records the exact
source URL and Nix content hash. `CUBIT-PLATFORM.patch` records the CuBit platform
adaptation. The CuBit frontend and build instructions are in this repository's
`tests/mesa-software` and `userspace/mesa` directories.

`MESA-SOURCE.tar.gz` preserves the complete adapted Mesa tree used by this
build, including per-file copyright notices, headers and generator inputs.
Archive timestamps and owner IDs are normalized; file contents are retained.
This covers Mesa sources, not separate CuBit, libc or compiler-runtime sources.

`UPSTREAM-LICENSE.rst` and the complete `licenses` directory are copied without
editing from that upstream source. Their presence does not imply that every
component covered by those licenses is linked into this demo. Individual source
files also contain copyright and license notices; this bundle does not replace
those notices or a linked-component distribution audit.

Normal native builds additionally include `LINK-MAP.txt` (archive members,
discarded sections and symbol cross-references) and `ELF-SHA256.txt` binding
that inventory to the demo binary. The map contains local build paths and is
audit evidence, not a license classifier. An extracted archive member may
have sections discarded by the linker; do not equate inclusion with every
function in that member being retained.

`LINKED-SOURCES.json` maps extracted Mesa archive objects to compilation-record
source paths and hashes. Generated sources are identified separately. Runtime
archive members without Mesa compilation records remain explicitly unresolved.
Directly linked objects, included headers and generator inputs are outside this
report's scope; it must not be treated as a complete source or notice inventory.

The demo uses softpipe on the CPU. It is not Intel hardware acceleration and is
not a claim of Khronos API conformance.
