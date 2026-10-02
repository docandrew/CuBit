# Process-manager metrics authority

Run the hosted regression and pure-policy proof in the Nix shell:

```sh
nix develop -c python3 tests/procmgr-metrics/check.py --prove
```

The harness compiles the current `REQ_SERVICE` and metrics registration blocks
extracted from procmgr. It uses the real authority decision and metric/log tag
packages, with fake service lookup, capability minting, auditing and sleep.
Build products are private temporary files.

Checks include ordinary manifest-requested publication; startup-only observation;
the log-viewer exception not authorizing metric observation; observer endpoint
routing to the publisher service; distinct issuer identities; final identity
issuance followed by permanent exhaustion; missing-service denial; registration
requiring both trusted startup and the exact metrics package identity. Three
negative controls must fail: ordinary observer approval, incorrect observer
routing, and identity reuse after exhaustion.

`Issuance_Proof.Check` proves the pure policy and operation/tag separation for
all admitted issuance IDs (11 assertions, GNATprove level 2, zero unproved).
The surrounding procmgr implementation is not SPARK-proved. The extraction
harness does not prove manifest parsing, kernel capability enforcement, startup
plan integrity, or IPC authentication.

Native integration uses the existing metrics-check app, which publishes 1,005
typed records, queries three aggregated series, checks latency/counter/span
values, rejects a publisher attempting observation (including a forged message
tag), rejects observer publication, and rejects malformed batch length:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c '
  make -C kernel procmgr metricsvc metrics-check &&
  bash tests/headless/run.sh --test metrics --accel tcg,thread=multi \
    --cpus 4 --timeout 120 --serial /tmp/cubit-procmgr-metrics.serial --keep-logs
'
```

The original issuance patch omitted the service registration capability. Its
first native run failed with `metricsvc: registration failed` and missing
publisher endpoints. Procmgr therefore additionally grants role-24 registration
to `com.cubit.metrics` only when launched by the trusted startup plan. An
ordinary spawned executable cannot gain registration merely by declaring that
package identity. This follows the existing TLS registration policy.

This change grants endpoints to manifests that request `metrics` or
`metrics-observer`; it does not add producer declarations to other applications,
install metrics into every boot profile, or provide raw flame-graph streams.

## Verified native result (2026-10-01)

Four-CPU TCG, 120 seconds: **PASS metrics**, including the runner's final fault
scan. Serial: `/tmp/cubit-procmgr-metrics-v2.serial`; build/test output:
`/tmp/cubit-procmgr-metrics-v2-native.log`. The test accepted all 1,005 records
and observed the expected three series; all negative IPC checks passed.
These are synthetic functional samples, not compositor latency measurements.

The wrapper's pre-initrd procmgr hash check failed because `make initrd`
rebuilt procmgr after the initial capture. Verification extracted procmgr from
the actual boot ISO's initrd and matched it byte-for-byte to the build and
staging outputs; patched sources and metrics binaries retained their original
hashes. Evidence: `/tmp/cubit-procmgr-metrics-boot-evidence/verified.json`.
The native gate passed; the original wrapper exit status was 1 solely due to
that obsolete pre-initrd binary hash. No repeated native run was needed.

The default desktop boot profiles remain unchanged. A profile must start
`metrics.svc` before starting publishers or observers that request its service.
