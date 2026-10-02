# Owned-region preparation tests

These are preparation for native owned mappings and Mesa JIT support, not a
public mapping API or a working native Mesa port. Run hosted projects from the
Nix environment, with assertions enabled by their GPR files.

For example, from the checkout root:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/region-transition/install.gpr && ../tests/region-transition/build-install/install_tests'
```

`install.gpr` exercises contiguous backing installation and rollback. Backing
must not be released until every installed leaf is removed and translation
invalidation succeeds. Failures leave the attempt quarantined; the caller
must retain its registry reservation. This is callback-model testing, not
live page-table or allocator validation.

`release.gpr` tests normal live-region retirement, paired with the installation
controller. Run it like `install.gpr`, using `build-release/release_tests`.
It covers352 success/failure/exception cases for1–16 pages, including every
unmap, translation synchronization and backing-release callback. It composes
with the actual region registry: denied attempts remain live; successful
release permits retirement and a new generation; failed attempts retain both
virtual and physical reservations. Callback order and retry rejection are
asserted. This is hosted regression evidence, not a proof of real TLB, grant
pinning or frame reclamation. No public syscall uses this controller yet.

## Owned-mapping aperture

`layout.gpr` exercises the owned-mapping aperture admission predicate against
221213 wide-integer reference cases. It checks disjointness from received
grants, the initrd seed range and framebuffer. `gnatprove -P layout.gpr -u
owned_memory_layout.ads --mode=prove --level=2 --report=all` proves the predicate's
postcondition: malformed intervals conflict; valid intervals conflict exactly
when their half-open ranges overlap. This small arithmetic proof is not a
proof of the syscall call sites or memory ownership/reclamation.

The native aperture is `0x580000000000..0x590000000000`. The original trial at
`0x500000000000` collided with the existing initrd seed and was rejected by the
boot regression; do not reuse that bootstrap address for owned mappings.

## Native compile probes must be isolated

Do not compile an otherwise unreachable unit into `kernel/build` with `-u`.
The kernel currently links `build/*.o`, so such a probe contaminates later
normal links even though the unit is outside the main program's dependency
closure. A standalone compile of `virtmem-regions.adb` previously did exactly
this, leaving an unresolved `region_pte__plan` reference in the normal link.

Use a separate output subdirectory, while holding the shared build lock:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -P cubit.gpr --subdirs=region-adapter-compile -u virtmem-regions.adb'
```

That is only a compilation check; `-u` does not establish a complete linked
dependency closure. Native integration still requires allocation ownership,
mapping/grant alias exclusions, synchronization, process teardown, and actual
syscall and hardware tests. The hosted `adapter.gpr` uses synthetic page tables.
