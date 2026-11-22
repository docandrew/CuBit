# Original ASLTS constant collection

**The original gate currently fails.** CuBit completes MN00 with result1 and
ERRS1. The stored error code is0x1100203C: constants.asl line60, which checks
Revision against ACPICA's date-style version range. CuBit deliberately returns
its own interpreter policy version1. ACPI6.6 section19.6.121 defines Revision as
the interpreter's revision and does not prescribe ACPICA's date encoding:
https://uefi.org/sites/default/files/resources/ACPI_Spec_6.6.pdf
The original expected zero and all AML assertions are unchanged. This is not a
passing conformance result, xfail or reason to impersonate ACPICA's version.

Explicit hosted release gate, run in the pinned Nix environment with TMPDIR set
to /home/doc/cubit-build-tmp before and inside Nix:

    python3 tests/aml-aslts-constant/run.py --profile release --selection constant-n64

The unmodified original functional/constant collection and its nine-file include
closure come from ACPICA commit232ff3f8ae1a4da11c709f61d9154482cfe8e6df, archive
SHA25691addf34cf6f00c310dcff1d456c2e04629c2b45efe409385068e9fc5e9fe43b.
Normal upstream common n64 flags were -oa -r2 -of -cr -vs, with listing outputs;
no force, suppression, wrapper AML or removed original tests. The retained AML is
18522 bytes. iASL compile/disassembly succeeded with3warnings/36remarks.

Normal acpiexec returned MN00/ERRS0/0 twice and RMRC1then2, with original CST1
PASS debug records. Full RP0P debugger queries hit its fixed result-buffer limit;
those errors and4outstanding cache allocations are preserved in reference/results.
Process0 is not a claim that every debugger query or teardown succeeded.

CuBit must return MN00/ERRS0 twice, observe RMRC1then2, read every original RP0P
record as exactly :STST:functional:constant:CST1:PASS:, and release all observation
handles. A skipped-test zero cannot pass. The runner uses actual Core/AML and
retains the original test's full assertions. Limits:180sbuild,30sruntime,1m method
fuel,100kmodulefuel,128explicit continuations,64MiBstack,1MiBlog,nice19/j1. Source,
compiler/ALI and artifact hashes are audited. Existing results are never replaced.
Only this release n64 configuration is selected; no checked, other configurations,
whole ASLTS, new proof or native execution claim.

The diagnostic successor reads existing stored data after the nonzero result:
ERRS1, RMRC1, CST1 FAIL, ERRP[0]=0x1100203C. It invokes no diagnostic AML and
restores the retained-handle count before preserving the original failure.
Original and diagnostic failed runs remain recorded separately. Metadata-only
validation is `python3 tests/aml-aslts-constant/run.py --profile release --validate-only`;
it checks hashes/inventories and cannot establish runtime success.
