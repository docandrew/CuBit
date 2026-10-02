# Hardware capability admission tests

These hosted tests compile the actual kernel capability specification. The
standard AML runner includes this target; `--prove` also checks its SPARK
contracts. Run this target independently with:

```sh
nix develop -c bash -c 'set -e; cd kernel; alr exec -- gprbuild -p -P ../tests/aml-core/hardware-capabilities/capabilities.gpr; ../tests/aml-core/build/hardware-capabilities/capability_tests; alr exec -- gnatprove -P ../tests/aml-core/hardware-capabilities/capabilities.gpr -u capabilities.ads --mode=all --level=2 --prover=cvc5,z3 --checks-as-errors=on -j2'
```

The kernel appends CAP_HARDWARE_GROUP and CAP_HARDWARE_REGISTER after existing
kinds, preserving their ordinals. Generic POLICY_MINT_CAPABILITY accepts raw
object words, so its handler uses isPolicyMintable to deny these two kinds.
They must originate through trusted catalog admission or checked membership
delegation. Ordinary derivation can reduce rights but preserves the existing
object, generation and authority tag; it cannot select another group member.

2026-10-01: after promotion under the shared build lock, 273 hosted checks and
14 SPARK analysis checks passed, with no unproved or justified checks. The
checks cover every type's mint classification and all 32 rights combinations
for both hardware kinds. The test's historical output label still says
HARDWARE-CAPABILITY-CANDIDATE, but its source path now selects kernel/src.
Config is a hosted stub supplying only the unchanged 64-slot table size.
Kernel handler execution, capability object lookup, parent revocation, startup
admission and hardware I/O are not covered or implemented here. The obsolete
private-copy generator and admission patch have been removed.

Private native validation compiled the candidate syscall-admin body, capability
types and Hardware_Catalog against the actual kernel runtime and compiler
settings. A subsequent full gprbuild compile produced 138 objects successfully.
Snapshot: /tmp/cubit-hardware-capabilities-kernel-n1ldf8bk. The three promoted
kernel files matched its candidate hashes exactly. Logs:
/tmp/cubit-hardware-capabilities-kernel.log and
/tmp/cubit-hardware-capabilities-kernel-all.log. No link, boot or handler execution
is claimed. Of 368 recorded baseline inputs, only generated build.ads date/hash
metadata changed during validation. The kernel GPR now includes shared/hardware.

Under existing kernel compiler settings (-gnatp), Resolve/Begin_Access/
Finish_Access frames were 144/24/8 bytes. These differ from checked-library
contract snapshot sizes and do not prove a whole-call-chain stack bound.
