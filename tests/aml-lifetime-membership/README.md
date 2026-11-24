# Method lifetime membership and code-bound checks

The namespace keeps keyed memberships for live temporary owners and temporary
lexical children, plus a retained-slot method-code prefix maximum. Final method
exit removes owned roots and temporary descendants, trims dead suffixes and
scrubs discarded code bytes. Recursive nonfinal returns keep the shared owner.
Root-owned permanent children stop lexical deletion propagation. Dead interior
method slots still contribute to retained code bounds, including nonzero offsets
of zero-length bodies. Module frame/table cleanup is not migrated here.

In the pinned Nix shell, set TMPDIR=/home/doc/cubit-build-tmp first. Build
`gprbuild -p -j2 -P lifetime_checked.gpr`, using a disjoint ACPI_LIFETIME_BUILD.
Run the four binaries: membership_tests768, bounds_tests768, cleanup_tests2306,
and container_lifetime_tests60 explicit checks,3902 total per profile. Release
uses O3/gnatp and keeps explicit Check calls active. Cap builds480s and tests60s,
with24GiB address space and1GiB stack. No native/hardware validation is implied.

The cleanup child fixture exercises the actual private cleanup entrypoint and
compares with the named-base scan behavior: foreign descendants, permanent
barriers, recursion, old dead tails and retained interior code. The normal ASL
Device-in-method fixture at `fixtures/container.asl` is compiled normally by
pinned iasl20260408. The exact
payload returns11 twice and1 on ACPICA; its cache-tracking diagnostics are
retained in external receipts. Both baseline and corrected CuBit replay pass;
the initial omitted-container-index update is a failing negative control.

The helper proof drivers cover512/16384 nodes and65536 code bytes. Membership
and bounds helper proofs each close50 total checks. Those unchanged results are
reused. For reproduction, select the supplied proof lines and drivers under
`systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0`, gnatprove-j2,
with an explicit90s cap. Do not re-prove unchanged scopes routinely.

The complete changed End_Method caller is NOT proved: its one90s attempt timed
out during Global generation. Helper proofs do not establish namespace lifetime
authorization or full caller ARO. New canonical coherence is a Ghost/type
invariant; the existing operational Valid_Context guard is unchanged. Service
and the opt-in collector remain OFF. See the guarded packet for exact source,
commands, proof qualifications and matched CPU results.
