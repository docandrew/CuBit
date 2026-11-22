# ACPI startup admission regressions

Run inside the repository Nix environment, exporting TMPDIR before Nix:

    export TMPDIR=/home/doc/cubit-build-tmp
    node tests/acpi-startup-admission/run.mjs

Use `--group controller`, `--group sender` or `--group owner-cleanup` for one
group. Each group builds and runs with assertions/overflow checks enabled and
with checks suppressed, compares verdicts, and checks the compiler profiles.
All sources are explicit and hash checked. Output, copied sources, temporary
hosted GPR projects and phase logs remain in TMPDIR. No previous temporary
build directory, generated startup package, full build or hardware is required.

Production units are compiled directly from the tree. Each group's other Ada
files are labeled hosted fixtures and selected only into its temporary source
directory. They model clocks, heap, grants, messages, kernel behavior, physical
I/O, Core state and GPIO observation as appropriate. They are never native
provider implementations. The small modeled parent packages in owner-cleanup
exercise ownership policy, not the actual parent implementation or a proof.

- **Controller:** 23 cases, 222 checks per profile. Runs the actual one-shot
  pre-device owner, sender/codec and timing policy. Checks no AML observation
  replay, dynamically sized copy, root release before admission, paused I/O at
  each peer call, same-ID confirmation, no deadline renewal, typed degradation,
  retained uncertainty, and cleanup without sending a new peer call.
- **Sender:** 63 scenarios, 603 checks per profile. Runs the actual sender and
  codec against modeled runtime boundaries. Checks exact wire shape, full-width
  identity, explicit finite deadlines, grants/page lifetime, all dispositions,
  one confirmation, unknown completion, and abandonment without another call.
- **Owner cleanup:** 1,177 checks per profile. Runs the actual owner-cleanup
  bridges and timing policy against modeled parents. Checks attributable owner
  cleanup, preservation of other roots/notifications/revision, and paused clock
  inspection without admitting I/O or renewing the startup deadline.

This is hosted regression evidence. Real AML discovery is covered separately by
`tests/acpi-gpio-bootstrap-observation`; real Core/REG cleanup composition by
`tests/acpi-region-cleanup`; timing transition proofs by
`tests/acpi-delay-domain-contracts`. These tests do not establish native IPC,
physical device behavior or whole-service proof. Native component sources and
runtime SDK are validated separately with semantic-only checks.
