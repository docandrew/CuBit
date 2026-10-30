# BankField reuse at namespace capacity

Run `python3 tests/aml-bank-capacity/run.py --mode release` or `--mode checked`
in the pinned Nix environment with the required TMPDIR. Seventeen checks pass
per profile using a static 25-node service: the exact fixture creates 23 initial
nodes and its two temporary BankField objects fill the remaining slots.

The same two-field declaration executes twice before a field is accessed. It
must reuse existing identities and capture selector one on the second execution.
Both method invocations return AB through the expected bank-write/data-read pair,
and cleanup restores both slots. The embedded fixture is shared with the access
suite and its cached reference checker. This does not cover all mixed new/reused
or differently shaped collision cases, nor prove the namespace constructor.

The standalone structure avoids the earlier combined test harness's GNAT15.3
compiler failure. It is an ordinary test organization change, not a compiler fix.
