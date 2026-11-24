# Bounded runtime Name reservations

The child-index package owns a small receipt for one provisional Name append.
The receipt authenticates node, incarnation, lifetime owner/incarnation and
definition identity. It preserves the pre-append count/stamp for exact abort.
Release consumes it before a later node can reuse the same slot. The caller
still owns index publication, value atomicity and namespace cleanup.

In the pinned Nix development shell, build with `gprbuild -P proof.gpr -p -j2`
and run `build/bin/reservation_tests`. The checked runtime has 3456 assertions.
The proof drivers cover capacities512 and16384 with the production incarnation
and definition types. Select both driver bodies with GNATprove, under a user
systemd scope with MemoryMax=24G, MemorySwapMax=0, -j2 and an explicit90s timeout:
`gnatprove -P proof.gpr -u reservation_default_proof.adb reservation_laptop_proof.adb --level=2 --timeout=15 --report=all -j2`.

These are local receipt/index proofs, not a proof of the whole namespace or
interpreter. In particular, general Name caller closure and complete value
undo/reclamation remain separate obligations. No tracked collector is enabled.
The runtime Name implementation retains its existing independent value
transaction while removing its full namespace snapshot.
