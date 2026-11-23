# Copier proof status

Checkpoint against the strengthened integer invariant, 2026-11-23. Counts below are emitted SPARK checks (flow and proof), not Why3 leaf goals. No whole copier or service AoRTE claim follows from helper closure.

| Subprogram | Closed / total | Status |
|---|---:|---|
| AML_Objects.Copies | 1 / 1 | Closed |
| Clone | 32 / 36 | Open |
| Clone.Allocate | 3 / 16 | Open |
| Clone.Copy_Byte_Object | 12 / 13 | Open |
| Clone.Copy_Integer_Object | 12 / 12 | Closed |
| Clone.Copy_Package_Header | 10 / 10 | Closed |
| Clone.Copy_Typed_Leaf | 13 / 13 | Closed |
| Clone.Enqueue | 9 / 14 | Open |
| Clone.Link_Child | 5 / 11 | Open |
| Clone.Matching | 4 / 6 | Open |
| Clone.Source_Shape | 6 / 6 | Closed |
| Clone.Traverse_Copy | 54 / 75 | Open |
| Copy_Frame | 12 / 14 | Open |
| Copy_Witness | 1 / 1 | Closed |
| Empty_Witness | 2 / 2 | Closed |
| Header_Matches | 25 / 25 | Closed |
| Is_Independent_Copy | 58 / 63 | Open |
| Valid_Witness | 35 / 46 | Open |

The byte helper has one functional length-equality item. Allocate, Link_Child, Enqueue, Traverse_Copy, Clone and witness observers also have open safety, precondition, flow or functional obligations. These are not a minor functional-only backlog.

The checkpoint requested AML_Objects, Byte_References, Package_References and Reclamation as well, but emitted no .spark report for those units before the 1,200 s cap (exit 124). Their requested coverage is incomplete, not proved. The parent layout observer has 11/11 persisted Why3 leaf goals; that is not a completed mutator-unit SPARK report. The namespace Global frontend failure remains separately tracked; no new namespace proof is claimed.

Runtime checks remain enabled for the entire production ACPI component, explicitly in native/component.ccl. No unproved reached unit earns -gnatp. Closed helper evidence is retained until a relevant source or dependency edit. The unchanged reference-count primitive retains its earlier 43/43 checks; the copier packet does not integrate reference counting.

Future changed-unit proof command (run only after relevant edits; do not repeat the capped broad checkpoint unchanged):

```sh
export TMPDIR=/home/doc/cubit-build-tmp
export ACPI_COPY_PROOF_OBJECTS=/absolute/private/proof-objects
timeout --kill-after=5s 1200s systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
  gnatprove -P tests/acpi-package-copies/proof.gpr -u aml_objects-copies.adb \
  --level=2 --timeout=5 -j2 --report=statistics
```

Run within the pinned Nix development shell. Narrow selection to the edited helpers and their proof dependencies. The packet receipts retain the exact five-unit checkpoint command, complete .spark output, raw Why3 sessions, source guards and per-subprogram audit.
