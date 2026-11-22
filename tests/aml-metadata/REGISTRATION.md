# Hosted metadata validation

Run from the repository Nix shell (set `TMPDIR=/home/doc/cubit-build-tmp` before
`nix develop`):

```sh
python3 tests/acpi-hosted/run.py --group metadata --mode release --test-timeout 30
python3 tests/acpi-hosted/run.py --group metadata --mode checked --test-timeout 30
```

This explicit group leaves the default and `all` groups unchanged. It requires
all ten focused mains and the retained-result replay; skipping mains is rejected.
The private final-production runs passed 2,914 focused checks per profile and
82 cached ACPICA comparisons per profile. The comparator passed 10,490 synthetic
checks. The public dispatcher passed all 21 stages in both strict profiles.

The 84-attempt reference corpus preserves 70 integer flows, 12 comparisons using
explicit coarse error mappings, and two normal compiler rejections. The three
upstream error classes retained in the evidence are not claimed equivalent to
CuBit's single `Unsupported_Value` status. TEST, MARK and POST share one loaded
state. Retained compound results retain their actual status and are released
before the next phase; they cannot masquerade as rejected scalar results.

Source and full reference-evidence hashes are checked before and after each
stage. Individual replay children retain the existing 64 MiB stack, 1 GiB address
space, 30-second and 1 MiB log limits. Focused children inherit an explicit 64 MiB
stack; compilation uses one worker. No outer replay deadline competes with the
per-child cleanup. Logs and source manifests are saved under this directory's
`results/` tree. Cached replay needs no fresh ACPICA execution or private paths.

Live namespace metadata and captured descriptors are distinct. Captures currently
support Device and Thermal descriptor types; tests do not establish all future
metadata operations or full descriptor-state copying. Authentication, source
replacement, root binding rollback, retention and garbage collection are tested.
Root metadata remains separate from ordinary data-cell authority. Current root
kind invariants and metadata copy contracts have checked execution coverage, not
a complete arena or interpreter proof.

Dynamic Device declarations and method-local Alias/lifetime behavior remain
unfinished. Separate normal ACPICA lifetime probes support captured values
surviving helper scope exit and recreation, but are not runnable interpreter
conformance cases yet. Live metadata escaping that lifetime remains unresolved.
The legacy constant-buffer decoder's 1,024-byte bound remains a known limitation;
this suite uses supported-size pressure while asserting actual collection.
CCL transport, subscriptions, hardware authority and native deployment are outside
this hosted milestone.
