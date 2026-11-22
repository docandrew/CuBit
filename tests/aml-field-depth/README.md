# Graph depth and default work policy (hosted)

Normal flat ASL defines 128/256 banked field dependencies over two independently
named direct registers. No malformed namespace mutation or recursive AML syntax.
The hosted test requires 128 levels to return3 with129 requests, and 256 levels to
return Value_Limit with zero effects under the default work policy, twice each.
These expected resource outcomes must be verified by the actual runs.

ACPICA compiled both tables without errors/warnings/remarks. Only the 128-level
runtime has three verified Integer observations. The 256-level runtime filled
the 1MiB log with reference-count warnings before any requested typed output;
its process status0 is NOT accepted as successful reference execution. Original
bounded diagnostics are retained without suppression/retry/limit increase.
The deep CuBit case tests explicit resource admission, not ACPICA compatibility.

Use pinned Nix, TMPDIR=/home/doc/cubit-build-tmp, run.py --mode release or checked.
No new formal proof, native hardware or whole-interpreter correctness claim.

The independent cases are shallow/exhausted/admitted. Each is a separate bounded
process under the unchanged30-second runtime limit. Rg6K6YLE release runs show
128levels/default ->Returned3/129requests;256levels/default ->Value_Limit/0requests;
256levels/explicit100,000work ->Returned3/257requests. Everycase invokes twice.
This demonstrates caller admission instead of a hard language-depth cutoff.
Checked results and exact artifacts are recorded in the checkpoint handoff.
The prior aZEncrej combined default-only run passed48checks/profile; it is a
separate historical test shape, not an extra rerun of the partitioned suite.
