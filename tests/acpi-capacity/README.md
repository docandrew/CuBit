# Hosted capacity checks

Run from the repository root:

```sh
tests/acpi-hosted/run.sh --group capacity --mode release
tests/acpi-hosted/run.sh --group capacity --mode checked
```

Four mains test independent node/aggregate-method/retained-root generic budgets, individual method limits, exact rejection frames, active-owner cleanup and Requests metrics. Expected43 checks per profile. These are explicit per-instance budgets, not firmware-derived runtime sizing or native allocation tests. Object/frame/default service limits are unchanged. Projects use only hosted production sources and isolated build directories. Runner records disjoint logs/results, uses-j1 and64MiB hosted stack; checked mode enables assertions and overflow checks. Frozen fixture origin: acpi-provisioned-capacity-djlhqqt5. No proof claim.
