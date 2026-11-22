# EC event receiver — hosted integration

Run run.sh in Nix with TMPDIR=/home/doc/cubit-build-tmp. --source-root DIR
selects pinned sources; --validate-only checks inputs without compilation.
The suite uses disjoint temporary outputs,j1/nice19,180-second build and
30-second runtime bounds,64MiB stack and1MiB captured logs.

Actual EC query→retained publication→receiver→AML _Q01→EC Field readback through
the saved-reply owner→lid Notify is exercised. Hardware,clock,ownership,IPC
and enqueue callbacks are modeled. Real Process_ID/handle/ABI types are used.

Receiver enrollment is trusted local work after independent EC identification
and provider readiness. It checks current Device descriptors,full opaque sender
identity,controller/epoch binding,strictly increasing tokens,exact duplicates
and conflicting payloads. Tokens are claimed before lookup/AML. Missing handlers,
zero fuel and handler failures remain consumed. Revoked route slots and epoch
high-water marks are never recycled. Retained cleanup blocks re-enrollment;
foreign Core refuses cleanup; Field uncertainty holds independently of roots
and remains visible alongside charge-report errors.

Install does not evaluate _HID,establish readiness,grant source/hardware rights,
or verify physical alias ownership. Native transport must authenticate the
sender,decode the notice and preserve per-controller order; one serialized
receiver owns this domain. Kernel source review finds per-publisher FIFO,but
this suite is not a native queue test or kernel proof. Older distinct tokens
are rejected; no reorder buffer is provided.

Run oracle.sh OUTPUT/ec-fields.aml for fresh repair-disabled ACPICA execution
with simulated zero-filled regions. Expected count/field/lid:1,0,0; LID0 Notify
0x80 and alias-backed EC01._Q03 buffer01,02 are also checked. This is independent
AML reference evidence,not physical EC/IRQ/IPC validation. Normal iASL is used.

Final functional tests pass1692joined+1063geometry checks/profile,plus original
datum162373+plan686. No new formal proof is claimed. The selected receiver proof
attempt stopped during existing AML dependency global-contract generation with
the previously documented Capture_Operand/Start_Package diagnostic and internal
errout failure; no solver acceptance was reached,no assertions weakened and no
compiler-crash research/variants pursued. The old geometry proof remains scoped
to its own unchanged units.

Native admission/leases,launch/transport/acknowledgment,_REG/provider/startup,
clock/scheduling,GPE handling and full verified AML/service goal remain pending.
Negative cases intentionally retain receiver uncertainty,publisher unknown
delivery and lower hardware recovery; no global-quiescence claim is made.


## Successor policy proof (actual receiver implementation)

The receiver now stores private ACPI_EC_Event_Policy states and uses its binding,
revocation,decision/claim,guard and settlement operations. Sender/epoch/token
logic is no longer duplicated in the receiver. The complete unchanged1692-check
integration suite passes both profiles with the extracted implementation.

Tests/acpi-ec-policy retains and reproduces level1/2 proofs of this actual pure
policy:13all-SPARK entities,11flow results,10proved checks and5functional
postconditions per level;96CVC5+11Trivial Valid attempts. Failure preservation,
epoch monotonicity,claim authorization/token advance and sticky uncertainty are
proved under API preconditions. Scope truth and callback observations are caller
obligations. Full receiver/Core/AML proof remains open; the older dependency/tool
diagnostic is not bypassed or relabeled as resolved. No native execution claimed.


## Exact receiver claim reconciliation

Receiver.Receipt returns the exact originally claimed notice,or absent for an
unused/rejected delivery. It does not attest handler success,cleanup or hardware
quiescence. Redispatch of a consumed Delivery cannot produce a receipt for a
different payload. A fresh duplicate Delivery has no receipt of its own.

Publisher.Confirm_Claim accepts only Delivery_Unknown,an exact retained notice
(controller,epoch,token,event number),and the full authenticated sender matching
the independently provisioned Expected_Consumer. Default No_Process disables
receipt acceptance. Sender must come from kernel receive metadata; these are
internal APIs,not a wire format or a source of hardware authority. Receipt loss
still retains the notice; reliable receipt transport/retrieval remains native
integration work. All entry points must remain serialized and nonreentrant.

Acceptance releases publication metadata only. It performs no EC transaction,
enqueue or AML invocation,and never clears receiver Field uncertainty,result
roots or catalog hardware recovery. Ready means publication can progress; the
catalog still rejects requests while retained for recovery.

Hosted checked/release:1705joined+1063geometry checks/profile.
Cases include upper sender bits,altered controller/epoch/token/number,repeated
receipt,an unclaimed duplicate,and confirmation beside sticky Field uncertainty
and hardware recovery. Fixture bytes unchanged from the prior ACPICA reference.
No new proof,native execution,physical hardware or native event endpoint claim.
