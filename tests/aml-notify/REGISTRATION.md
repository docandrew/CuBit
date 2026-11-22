# Hosted Notify and Alias checks

Inside the repository Nix development shell, run:

```sh
python3 tests/acpi-hosted/run.py --group notify-alias --mode release
python3 tests/acpi-hosted/run.py --group notify-alias --mode checked
```

This separately selected group preserves existing `all` membership and BufferField registration. It runs all six Alias/Notify mains, strict replay compilation, comparator protocol tests, and 66 cached comparisons. No main skips are accepted. Results and pinned inputs are recorded in `tests/aml-notify/results/`.

The public dispatcher passed in both strict profiles in the private validation tree `notify-alias-portable-validated-a82p1pl0`: six mains with 290 checks, 8,568 comparator checks, and 66 cached comparisons per profile. All 13 stages passed in each profile. This is hosted acceptance, not native deployment or proof. Root metadata, PackageRoot/Deref/Copy, and runtime Alias gaps remain outside this milestone.

Reference provenance verifies path-only relocation of all 88 original records and 422 evidence files. The 20 normal compiler rejections and two marker-inconclusive cases remain explicit. Of the comparable cases, 48 compare positively observed event multisets; 18 compare status/marker plus a separately justified local empty-queue policy. Callback order, upstream absence, universal cancellation safety, native integration and formal proof are not claimed.

Builds use one job and a 600-second limit. Replay children have 30 seconds, 1 GiB address space, 64 MiB stack and bounded logs. The dispatcher replaces itself with this suite; the replay has no competing outer timeout. Graceful TERM cleanup was tested in the predecessor harness. SIGKILL and interruption before an outer capture acquires its child are outside that guarantee.
