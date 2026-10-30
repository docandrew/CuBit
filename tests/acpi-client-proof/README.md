# Selected ACPI client and provider proof milestone

In the pinned Nix shell, with TMPDIR set before entering and inside the shell,
run phases separately and inspect each result before starting the next:

```sh
bash tests/acpi-client-proof/run.sh --validate-only
nice -n19 bash tests/acpi-client-proof/run.sh client_proof_model check
nice -n19 bash tests/acpi-client-proof/run.sh client_proof_model flow
nice -n19 bash tests/acpi-client-proof/run.sh client_proof_model prove
bash tests/acpi-client-proof/check-result.sh <printed-result-directory>
```

Repeat the check/flow/prove selections for `acpi_region_protocol`,
`aml_identity-issuer`, `acpi_fadt-transactions` and `aml_field_datums`.
The latter two deliberately select only Octets/Aligned (spec lines 7–14) and
Width_Mask (body lines 4–9). The result checker requires the exact selected
nonempty proof trees, Valid results, no skipped entities, and unchanged source
and prover hashes. A successful process alone does not count as a proof.
The generic templates alone are not proof selections; the concrete model
instantiates the production client and provider.

The audited milestone contains 94 checks and 209 valid proof obligations:

| Selection | Checks | Valid obligations |
| --- | ---: | ---: |
| Codec | 10 | 24 |
| Instantiated client/provider | 75 | 164 |
| Identity issuer | 4 | 14 |
| Octets/Aligned | 3 | 3 |
| Width_Mask | 2 | 4 |

Codec contracts cover canonical requests and reply value halves. Client
contracts cover coherence, exact Initialize/Dispatch/Confirm/Retire transitions,
unchanged endpoint/identity, request counter exhaustion and unknown completion
receipts. Provider checks establish selected runtime safety, including explicit
rejection of a missing register slot. They do not establish its complete
functional admission, epoch or confirmation-history behavior.

The model uses the full UInt64 endpoint domain and production default capacities
of 32, arbitrary reply/delivery and clock inputs, and proved normal-return
Exchange/Read_Window contracts. This is not a proof for every generic capacity
or for native callbacks/exceptions, hardware, capability provenance, backing
lifetime or terminal completion evidence. Standard Ada/Interfaces shift models,
the pinned Nix toolchain and GNATprove are trusted. Review effects and callee
dependencies before composing this evidence into a larger claim.

Flow mode reports three reviewed INEFFECTIVE assignments to In_Flight or
Dispatching (two client instances and one provider instance). The normal-return,
alias-free SPARK model cannot observe these transient guards; they remain
necessary for exceptions and native callbacks outside the model. Other flow
warnings need investigation. Prove-mode trees have no outstanding flow warnings.

Limits: level 1, cvc5/z3, five seconds per prover obligation, one worker,
180 seconds per phase, 64 MiB stack, 1 MiB log output. The maintained saved
collector terminates the process group on timeout/log overflow. Internal proof
data is not a log and is not truncated to 1 MiB. There are no automatic retries.
Use at most two heavy workers. Proof runs do not build the kernel or a boot image.

At this proof milestone, hosted functional regression evidence was separate:
6,885 checks per release and checked profile in tests/acpi-field-remote, including all 90 ACPICA Qword
store images. Native API gates are compile-only. Whole-interpreter correctness
and live CuBit startup/hardware acceptance remain unfinished.

The later exact-Qword Bind/Add_Binding extension has separate expanded runtime
evidence and changes layout admission and Namespace binding. No new proof of
that functionality is claimed. In particular, the earlier Admitted proof that
excluded Qword is historical. Run this recipe on the current pinned inputs to
obtain fresh selected proof results; registration alone is not a proof rerun.
