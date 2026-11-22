# CCL catalog completion

Run from the repository root, always in Nix:

```sh
nix develop -c make -C kernel ccl-completion-test ccl-completion-prove
nix develop -c bash tests/ccl-completion/run-preview.sh
nix develop -c bash tests/ccl-sessions/run-preview.sh
```

The pure SPARK query operates only on its supplied catalog snapshot. Tests cover
empty/isolated catalogs, case-sensitive and shifted-bound prefixes, oversized
prefix rejection, maximum-length qualified names, all 256 operation slots, and
explicit truncation of the 16-entry result list. Every returned contract is
compared against ordinary catalog resolution; runtime bindings remain absent.
Session regressions also check that querying one session reveals nothing from
another and does not append history.

The SDL regression drives the real shared Workbench: F6, `(clock.mon`, checks
that Ctrl+Space leaves the automatic inline suggestion unaccepted, then presses
Tab and compares the input pixels with manually entered `(clock.monotonic-ms`.
Additional cases check that strings, escaped quotes, comments, unknown names,
non-call heads, selections and mid-token carets are left unchanged by Tab.
It saves screenshots for visual inspection. This is a
Linux-hosted keyboard/rendering test, not a test of native IPC authority.

Call-context tests cover nesting, completed/unmatched parentheses, comments,
strings, source/name limits, invalid caret offsets, and string bounds ending at
`Positive'Last`. Session description tests require exact names and retain catalog
isolation. Context inspection is inert; it is not the language parser.

The completion/context units discharge 65 GNATprove checks (7 flow, 58 prover),
with no assumptions or justified checks. This proves the checked core's runtime
safety/loop obligations, not UI correctness, provider provenance, or whole-system
capability soundness. Completion executes no service operation. Native and
Linux adapters map Ctrl+Space to the same shared REPL event.
