# Deferred output retirement

The Desktop integration uses bounded, deferred output cleanup and retains the
functional synchronous software backends.

Desktop retains each output's presentation identity while renderer, Display,
and grant readers retire. Renderer `Targets_Busy` preserves ownership and returns
to the event loop. Renderer retirement alone cannot release an active Display
lease: the matching presentation completion must make that output writable.
Only an exact successful lease reply authorizes grant revocation. Each grant is
revoked once, then polled for confirmed retirement. Storage release follows all
of those confirmations. Unknown or rejected retirement quarantines the output;
it does not authorize reclamation.

Outputs may finish at different times. Their old completion identities remain
available until the global drain commits. The commit clears old aliases and
advances the request watermark. New windows retain their metadata; shutdown
suppresses reopening. A compatible fresh physical-output discovery restores
accepted DPI and placement. Logical display names are not GPU import authority.

## Evidence and boundaries

- Retirement policy: concrete three-target SPARK instantiation, 21 checks,
  no unproved or justified checks. Renderer Busy and pending grant confirmation
  preserve state; storage admission follows the ordered retirement chain.
- Layout restoration: 15 SPARK checks, no unproved or justified checks; ten
  layout cases plus changed mode, count, identity and invalid-layout cases.
- Typed backend interfaces: Mesa 86 checks and legacy 25 checks, no unproved
  or justified checks. Eleven hosted text/fill success and unsafe cases pass.
- `test-output-retirement.py` extracts the actual Desktop cleanup routines and
  runs 20 asynchronous cleanup modes plus the exact lease-completion routing
  branch using controlled foreign-call results. It covers held renderer
  and presentation, staggered output completion, retained IDs, new windows,
  all 16 partial lease/grant configurations, shutdown suppressing reopen,
  malformed lease replies and rejected revocation. Input hashes are recorded.
- Both full native Desktop variants link; the native output rendering test
  compiles against the typed interface.

The existing Desktop main routine is not wholly SPARK-proved. Its integration
is regression-tested. Kernel mapping validity, service reply truth, Mesa/C
behavior and actual device completion remain trusted boundaries. The native
fixtures delay retirement evidence from the synchronous software renderer;
they do not establish asynchronous GPU fence behavior, physical display timing,
240 Hz operation, or keypress-to-photon latency. Native shutdown passes with a real queued frame: the fixture observes delayed
presentation/grant retirement, zero tracked pixel storage, loop termination and
the kernel-reported Desktop process stop. Partial-setup recovery also passes: after a real allocation but before its grant
creation, the fixture aborts setup, observes delayed cleanup to zero tracked
storage, calls the existing internal-session activation routine, then passes
normal dual-output desktop interaction checks. This demonstrates explicit
recovery, not an automatic production startup-retry policy. The portable native fixtures and integration tests preserve these proof
boundaries.

## Reproduction

Run the hosted checks inside the repository Nix environment:

```sh
python3 tests/compositor/test-output-retirement.py
gprbuild -p -P tests/compositor/output_retirement.gpr
tests/compositor/build/output-retirement/output_retirement_tests
gprbuild -p -P tests/compositor/layout_restore.gpr
tests/compositor/build/layout-restore/layout_restore_tests
```

For native tests, hold `coordination/build.lock` through build and boot. Pass the
existing native Mesa build directory as the builder's positional argument:

```sh
python3 tests/compositor/build-desktop-completion-fixture.py "$MESA_BUILD" --output-retirement scaled
python3 tests/compositor/run-output-retirement-fixture.py "$FIXTURE_DIRECTORY"
```

Use `shutdown` and `partial` for the other two modes. The builder prints a unique
fixture directory. The runner refuses existing boot evidence, preserves staged
Desktop and Display binaries, records their hashes, and uses the Desktop image
override. Scaled testing injects one modifier press/release through a separate
QMP channel while the normal observer exercises DPI and monitor interactions.
Shutdown waits for the kernel-reported Desktop process stop. Partial setup
checks explicit recovery, then runs the normal desktop interaction observer.

The native evidence oracle rejects missing/reordered markers, missing input or
late completion where required, duplicate cleanup commits, unsafe retirement,
and reopening during shutdown. Its self-test has three positive cases and 29
negative controls. Kernel fault scanning remains in the headless runner.

Retained pre-integration evidence: scaled r6 binary
`da1b1918f9fcd0d7257c4500a2f4026fd621e24b71e5131669dfd90869f242cb`,
shutdown r1 `8a5a25a5997bafc3283512edd639c885836f3a1b6721f8fb580930dc1c71037f`,
and partial r2 `cd88d39e805fee6318e35c60440501e4ad7a0b0f5d1da13f50507ae188325204`.
The packaged instrumenter reproduces each tested main source byte-for-byte.
The packaged runner also passes a fresh shutdown boot of the retained binary.

## Asynchronous Display lease release

The output release control request now uses `capSubmit`. A proved per-output
request policy permits one accepted request at a time. Rejected submissions
retain the lease and allow a later bounded attempt; pending submissions cannot
be submitted again. Only a matching token, valid successful kernel completion,
and exact successful Display reply permit the retirement policy to revoke
grants. Reply label, length, flags, reserved field and all four words are checked.
Malformed and duplicate replies quarantine the output. Disabled outputs during
partial setup use the same completion route; old tokens are retained until the
global retirement watermark commits.

The retry rule relies on the current kernel contract: `capSubmit=False` means
no request was published. `submitResolvedEndpoint` reserves completion capacity
and publishes under mailbox locks; every failure path returns before successful
enqueue. This transport contract is an audited boundary, not a property proved
by the userspace policy. Reopening's information and acquire RPCs remain
synchronous; this change does not make all Desktop control traffic asynchronous.

The new policy has nine SPARK checks (four flow, five prover), with none unproved
or justified. Tests include 10,000 rejected admissions, 10,000 pending admission
attempts, 20 actual cleanup scenarios, and an extracted completion-routing
branch covering both outputs, every reply field, unrelated tokens and duplicates.
Native scaled-drain testing holds both real Display replies until fresh input
is dispatched; all four interaction groups then pass with DPI/placement restored.
Shutdown and partial setup deliberately do not require new UI dispatch: shutdown
stops accepting new UI work, and partial startup has not registered input yet.
Both cases wait for real lease replies and delayed grant retirement before
freeing tracked storage; shutdown exits and partial setup supports explicit
session recovery followed by normal desktop interaction.

Reproduce with the same build lock and Nix environment as above:

```sh
python3 tests/compositor/build-desktop-completion-fixture.py "$MESA_BUILD" --async-lease scaled
python3 tests/compositor/run-output-retirement-fixture.py "$FIXTURE_DIRECTORY"
```

The other modes are `shutdown` and `partial`. The asynchronous fixture checker
adds 31 negative controls to the existing 29. Its generated Ada matches the
three tested native fixtures byte-for-byte. No hardware GPU, refresh-rate, or
physical keypress-to-photon claim follows from these software-path tests.
