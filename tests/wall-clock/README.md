# Clock arithmetic regression (Linux hosted)

```sh
nix develop -c make -C kernel clock
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/wall-clock/clock.gpr && ../tests/wall-clock/build/main'
```

This enables assertions and overflow checks only in the host test. It checks
every date from 1970 through 2399, timezone enum/name round trips, UTC, Denver DST
boundaries, Kathmandu's quarter-hour offset and Lord Howe's half-hour DST.
It is not a claim of formal proof or complete Ada.Calendar semantics.
