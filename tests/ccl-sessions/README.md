# CCL sessions, typed host calls and functions

Run `nix develop -c bash tests/ccl-sessions/run-preview.sh` from the repo root.
Assertions are enabled in these Linux-only tests, not in the native runtime.

`function_tests.adb` covers zero/eight parameters, sixteen definitions, isolated
call environments, earlier-function calls, all four value types, owned string
returns, maximum-length text, depth/fuel exhaustion, rejected recursion/capture,
duplicate names, signature mismatches, full admission before effects, exactly-once
arguments, BASIC round trips, scalar bytecode rejection, and hostile edits.
`text_tests.adb` also exercises a string-returning function through Watch.
The SDL test executes a typed `greet` function through the real shared REPL and
checks the label region; it does not use a user's running window.

After rebuilding the USB Live CD, run the native integration test:

```sh
nix develop -c python3 tests/usb-optical/run-live.py --cpus 4 --ccl-ui-hooks
```

It types the function through the native keyboard path and captures
`ccl-text-label.ppm` for visual inspection.
The typed-function slice passes this native test on the rebuilt USB Live CD;
the screenshot shows `hello cubit` produced by `greet` and a Boolean success
result. Hosted session/text/function, 36 source-view round-trip, SDL interaction,
completion and remote authority/periodic regressions also pass.

SPARK flow check:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/ccl-sessions/sessions_tests.gpr --subdirs=functions-flow -u ccl-language.adb --mode=flow -j2'
```

The current run passes 105 initialization/termination flow checks with no flow
errors; unused-value warnings remain. This is **not** a runtime-error or
functional-correctness proof of the interpreter or its host instantiations.
No assumptions, SPARK-Off sections or proof-only contract padding were added.
