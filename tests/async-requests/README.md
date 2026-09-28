# Common asynchronous request lifecycle

Run in the Nix environment:

```sh
nix develop -c bash tests/async-requests/run.sh --prove
```

The Linux-hosted test exercises the production `CuBit.Async_Requests` runtime
package without CCL or service dependencies. It covers stopping before/during/
after submission, rejected submissions consuming their tokens, late completions,
duplicate/invalid/wrong tokens, completion ownership, and counter exhaustion.
The SPARK target proves the state-transition contracts, including no mutation
on refused reserve/capture and retention of pending state across Stop.

Callers still authenticate completion receipts, serialize tracker access,
allocate process-wide unique tokens, and establish external grant retirement.
See [client lifetime architecture](../../docs/ipc-client-lifetimes.md).
