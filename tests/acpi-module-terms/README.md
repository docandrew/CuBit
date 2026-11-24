# Module TermList regressions

Run in the pinned Nix environment with a new private output directory:

```sh
python3 tests/acpi-module-terms/run.py --profile shipping --output /tmp/module-terms-shipping
python3 tests/acpi-module-terms/run.py --profile checked --output /tmp/module-terms-checked
python3 tests/acpi-module-terms/run.py --reference --output /tmp/module-terms-reference
```

The default builds the existing ASLTS corpus runner with the selected profile and records its source inputs. `--runner` accepts an already qualified binary. Builds use j2, warnings as errors, and a 240-second cap. Each runtime case retains the established 12-second wall, 24-GiB address-space and 128-MiB stack limits. Reference runs use ACPICA's default synthetic region simulation; no hardware is accessed.

All fixtures are our own. The generator compiles an ordinary control containing `Return (expression)`, validates the exact method envelope, and moves that encoded operand to the module TermList. Explicitly assembled constant, Noop and malformed controls use public AML encodings. It recalculates the table length and checksum. No captured firmware, OEM names or values are read or copied; no original ACPICA test, compiler diagnostic or expectation is altered.

The original six patterns are preserved: ordinary control, Noop, Zero, One, Ones and ByteConst. Additional cases cover all integer encodings, string/buffer/package values, named objects, references, arithmetic/conversion expressions, Revision, Timer, a void call, one-time and nested side effects, and 128 discarded buffer results. A following declaration and the MN00/ERRS/status-record oracle verify continued execution and side effects. Malformed operands and a buffer exceeding the runner's byte quota must still produce typed refusals. Repeated discard tests check observable continuation and named-object preservation; they are not a proof of all internal collector/root invariants.

Timer uses the existing owner clock callback and its availability result; module loading does not enable hardware field access. `--no-clock --only timer hardware_denied` builds a runner with the default unavailable clock and checks that both operations are refused.

Reference mode covers positive semantic cases. Malformed/quota controls are separate fail-closed CuBit expectations. `--start` and `--limit` support bounded batches. `--fixtures` reuses an already generated, hash-verified fixture set. `--baseline-observation` is diagnostic only and never marks the suite passed; it records the pre-fix failures without changing their expectations. Normal runs fail if any expected outcome is missing or differs.

JSON receipts preserve every command, input/binary/log hash and result. Counts describe these synthetic regressions, never the original full ASLTS pass percentage or SPARK proof coverage.

The initial candidate run records 47/47 expected shipping outcomes and 46/47 checked outcomes. The checked 128-buffer discard stress case reaches the original 12-second cap; this failure is retained, with no reduced iteration count or increased limit. ACPICA completes all 39 positive controls. These results qualify only the recorded candidate and inputs.
