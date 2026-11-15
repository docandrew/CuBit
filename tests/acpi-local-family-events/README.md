# Concrete local lid and AC-source event adapters

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c bash tests/acpi-local-family-events/run.sh

Pinned GNAT/GPRbuild, Node and ACPICA must be on PATH. Sources are hash-checked and copied; unlisted Ada/ASL inputs refuse. Builds use -O0 -gnatp with180-second/24GiB limits, executions30seconds; both binaries and non-binder ALI profiles/hashes are recorded. No full assertion-enabled Core generation.

1536 checks across68 processes:17 scenarios for each family and AML width. The concrete ACPI_Requests.Lids.With_Local_Events and Power_Sources.With_Local_Events children invoke their own actual parent caches and the same private request/Core/runtime. Both typed Open and Online sample values are checked. Collection diagnostics record one shared search/status/member charge. Failure injection requests an actual shared-runtime stop from the trusted evaluator after a member call, or marks the modeled source unhealthy during collection.

The suites exercise deferred publication, self-generated Notify without repeated AML, one later-external-event resample, partial all-consumer fanout, held receipts, early/foreign/stop states, clock loss/regression/overflow, delayed freshness, window failure, no family and source loss. The early case tests lifecycle guards without loading AML. Provider/clock/health/fact callbacks are modeled; native hardware and Main/source enrollment are not exercised. Callbacks must be serialized, bounded, normally returning, non-reentrant and non-mutating through aliases. Every consumer must route before ACK; no producer may interleave with Collect.

ACPICA with repairs disabled matches32 integer values across eight executions and the self-versus-baseline Notify-handler ratio for each family/width. This checks fixture semantics, not event policy. Native SDK binding is checked separately and deliberately refuses clock/source health. No new proof is claimed. Report_Publication is a copied-fact sink; Main uses the existing logger after Advance returns, outside active/pending/uncertain Field work. Consumer stop never authorizes backing retirement.
