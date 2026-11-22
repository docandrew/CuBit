# Frozen pipeline first-failure diagnostic

The patch applies to `/tmp/cubit-desktop-indicator-quiet-1`, not blindly to shared
Main. It adds no pipeline workaround and does not change the Mesa/runtime bundle.
Only eight files in `changes.json` differ. Existing images remain immutable.

C records the first nonzero Vulkan result into four startup-thread-only scalar
fields. Later cleanup or repeated wrapper failures cannot overwrite it. The
adapter does not allocate, log, call IPC or reenter Mesa. Ada reads the record
and logs `DESKTOP-VULKAN: pipeline failure stage= ... index= ... vk= ...` after
`Prepare_Pipeline` returns. A failure before the C boundary produces an explicit
unavailable diagnostic. These diagnostics are outside the SPARK proof boundary
and never influence admission, ownership or retirement decisions.

| Stage | Operation |
| --- | --- |
| 1, 2, 3 | Repeated setup, invalid context, missing device-proc entry |
| 100 | Affine prerequisite |
| 101–105 | Affine descriptor layout, pipeline layout, sampler, vertex module, fragment module |
| 106 | Affine graphics pipeline; index 0 opaque, 1 premultiplied, 2 straight alpha |
| 200 | Checker prerequisite |
| 201–204 | Checker layout, vertex module, fragment module, graphics pipeline |
| 300 | Source-descriptor prerequisite |
| 301, 302 | Descriptor pool creation, allocation of 140 descriptor sets |

Missing-function stages 110 (affine), 210 (checker), and 310 (sources) use
one-based function indices listed in `lookups.json`; these identifiers are
explicit in the source. Every required lookup is covered. Other indices are zero. `vk` is the signed raw Vulkan return value, including
positive non-success statuses. Synthetic prerequisite failures use
`VK_ERROR_INITIALIZATION_FAILED` (-3). They must not be described as a Mesa call
return. The weak scalar hook lets existing standalone engine fixtures link
without diagnostic storage; the frozen Desktop defines that storage and getter.

## Validation

Hosted mocked Vulkan calls exercise success and every one of 14 creation-call
failures, all 33 individually missing required function pointers, the affine precondition
guard, a positive non-success status, no record
on success, null getter pointers and first-failure preservation. This compiles
the actual changed engines. It does not validate Mesa, native GPU execution,
logsvc delivery, or the singleton wrapper's context admission guards.

Run `run.py` in the pinned compositor Nix shell with `--source-root` pointing to
the patched snapshot, `--generated` to the compatible build's generated shaders,
and `--vulkan-include` to the exact Mesa include directory. Outputs are private.

Native software-fallback regression evidence:
`/tmp/cubit-desktop-pipeline-diag-native-2/result.json` (three menu restoration
cycles, 32 cursor moves, eight round trips; deliberate HUD strip excluded).
Pipeline failure is not exercised by this software-only native boot.

Artifact: `/tmp/cubit-desktop-pipeline-diag-build-2/desktop-vulkan-compositor.svc`
SHA-256: `f6fc080d3ac0c61ede35dd04294ccf053a95e5a3231acf25db9de272504d6f85`.
Before packaging, use its existing compositor verifier/manifests. Package with
the same frozen compatible kernel/services/Mesa inputs as system-budget
`f0d2a706`, changing only Desktop, into a separately named candidate. Record the
image hash and run exact-image checks; neither linking nor these hosted tests
establishes NUC success.
