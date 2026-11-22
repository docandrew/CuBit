# Host bridge discovery and button watches

Run in the Nix development environment, with `TMPDIR=/home/doc/cubit-build-tmp`
exported before entering Nix:

```
bash tests/acpi-host-bridge-watch/run.sh
bash tests/acpi-host-bridge-watch/run.sh --mode checked
bash tests/acpi-host-bridge-watch/run.sh --validate-only
```

`--source-root TREE` checks and builds against another source checkout. Every
selected production source, model and fixture must match `sources.json`.
Outputs and a hashed receipt go into a unique directory beneath TMPDIR.

The suite runs actual host-bridge discovery, channel ownership and scheduler
logic, the actual native endpoint, and the actual native button/watch owner
and pure policy. It checks ordinary nonroots, compatible-ID fallback,
acknowledged channel lifetime, requests during a retained AML query, exact
64-bit request identity, cached press/stop results, refusal preservation,
lost caller consumption without replay, serving modes and closed drain gates.
The legacy unavailable-watch path must fail its explicit negative control.

Kernel message/reply operations, grants, channels, clock, event-domain and
downstream hardware handlers are models. These are hosted functional tests,
not native execution, real hardware acceptance or a new SPARK proof. Release
uses `-gnatp`; checked mode enables assertions/overflow checks for the three
button/watch policy and owner bodies while leaving Core suppressed. Both
modes retain explicit functional checks and verify compiled ALI profiles.

Four independently compiled ACPICA fixtures compare scalar IDs and resource
buffer values at AML32/64. They do not prove CuBit discovery or reply policy.
Each reference currently exits successfully but emits exactly four outstanding
cache allocations; the runner records that qualification and rejects changed
diagnostics. Known hosted redundant-use/model warnings are retained; this is
not a warning-as-error gate for every selected unit.

Fresh startup coverage adds 310 checks across AML32/AML64: publication from an unconsumed manager (70 per width), readiness becoming available on a later turn (74), and Stop before start (11). The normal callback deliberately refuses; the publication-only path performs no normal/power callbacks. Existing query/result, accepted-session, grant retirement and original saved-watch delivery assertions run through the fresh-start cases. A wrong scheduler entry point fails explicitly at both widths. Existing 2,616 checks and two earlier controls remain; the registered suite totals 2,926 positive checks and four controls, plus four qualified ACPICA scalar-ID/resource-buffer comparisons. Cached-power/battery dispatch uses the already registered admission boundary models and is not acquisition coverage. No Native_Instance execution, hardware validation or new proof is established.
