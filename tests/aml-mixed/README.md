# Focused mixed AML comparison checks

From the repository root, using the existing pinned hosted entrypoint:

```sh
tests/acpi-hosted/run.sh --group mixed-comparison --mode release
tests/acpi-hosted/run.sh --group mixed-comparison --mode checked
```

Each mode selects338 explicit checks (pure66, owner46 and Buffer226), plus60 cached ACPICA classifications:52 original matrix cases and8 operand-effect cases. Of these,58 return matching scalar values and2 produce the expected Empty_Buffer status. Errors are not reported as successful scalar evaluations. All selected mismatches/timeouts make the entrypoint nonzero; per-case output and classifications are preserved. Existing groups and the `all` selection remain unchanged; this focused group is explicitly selected.

The distinct Mixed_Oracle_Runner is adapted from the retained BCD runner and selects TEST via its normal method argument. Neither that original runner nor the ASLTS MN00 runner is replaced. Every reference payload is verified before and after replay; the executed binary hash is recorded and rechecked. No private workspace path is required by a command or project. Historical provenance records retain original absolute artifact paths solely as evidence. Source archive commit and packaged Nix executable provenance remain distinct.

Reference ASL, AML, compile/runtime output and observations are cached under reference/. Replay needs no network or installed ACPICA executable. It does not regenerate the oracle or claim full ASLTS coverage. Each runtime has30seconds,1GiB address space and64MiB stack. The canonical harness runs gprbuild -j1 under nice19; projects use strict O1 release or assertion/overflow-enabled checked flags.

Production sources are unchanged from the frozen annotated owner checkpoint. No new proof is run here. The existing49-obligation helper proof covers bounds/current failure contracts, not full semantic ordering; differential and hosted tests supply separate behavior evidence.

The canonical Buffer comparison fixture is also selected by this group (226 checks per profile); its security/current-byte cases remain intact.

Validation was scoped: the original112 checks and60 classifications passed both profiles before relocation; the newly registered Buffer226 main then passed both profiles separately. The entire expanded group was not redundantly rerun.
