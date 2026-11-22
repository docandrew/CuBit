# CCL source views and formatter

Run from the repository root:

```sh
nix develop -c bash tests/ccl-views/run-preview.sh
```

This runs bounded conversion/formatting tests and isolated Linux SDL sessions.
No user's existing window is closed. Screenshots are written to temporary
directories printed by the script. The source converter and Workbench body
are shared with native CuBit, but the SDL tests do not constitute a VM boot test.

Coverage includes both-direction round trips, canonical executable equality,
all current expression forms, visible Clock calls, strings/escapes, unusual
binding names, comments, nested scopes, conditional formatting, long-call
wrapping, formatting idempotence, per-node source-range reconstruction,
incomplete/type-invalid/unknown-call rejection, and quoted-name injection.
Infix tests cover precedence, left/right grouping, negative literals, nested
conditional expressions, keyword boundaries, overflow-sensitive grouping,
untaken division-by-zero branches, denied host admission before effects, and
exactly-once left-to-right host invocation. Prior call spellings remain valid.
Typed `define`/`FUNCTION` declarations and calls have canonical/source-span
round trips, including zero parameters, string results, earlier-function calls,
and escaped BASIC keyword names. There are currently 36 round-trip fixtures.
The UI exercises F8, both interpreter surfaces, and a paused bytecode VM
whose inspection must remain unchanged across F8 and Shift+F8.

The hosted interpreter checks also exercise scalar-copy host results: ownership
tags, noncopyable values, variants and failed callbacks must not become plain
scalars. A wrong primitive produces the existing result-type mismatch, while
valid true/false values round-trip through typed functions and host calls.
The ownership-tag case failed before the scalar-host adapter fix; these checks
are semantic regressions, not new F8/UI behavior or a kernel-security proof.

Focused proof command:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/ccl-views/views_tests.gpr -u ccl-language-views.adb --mode=all --level=1 --timeout=5 -j2'
```

No assumptions or SPARK-Off escape hatches are used in the converter. The
initial GNATprove run hit an internal compiler assertion in `sem_util.adb`,
so passing tests must not be described as a completed SPARK proof.
