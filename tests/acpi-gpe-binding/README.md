# GPE method binding and actual lifecycle integration

In the pinned Nix environment, exporting TMPDIR before nix develop:

    bash tests/acpi-gpe-binding/run.sh --output-root <private-directory>

Select ACPICA_BIN or PATH. This snapshots/pins its explicit source manifest,
tools, logs and outputs. It compiles fresh AML revisions1/2 and builds/runs
checked (-gnata -gnato) and release (-gnatp) profiles. An early compile check of
the new unit catches declaration warnings before the full hosted build.
935newchecks/process,1870/profile;506unchanged lifecycle checks/process,
1012/profile, total2882/profile. Eight fresh processes and20terminal0commands.
One compiler job,180s build,30s runtime,5s ACPICA,64MiB stack,no core dumps,
1MiB captured logs. Invoke nice -n 19. No native/kernel/image/QEMU build runs.

ACPI_GPE_Methods resolves canonical direct _Exx/_Lxx members from the root _GPE
scope or an owner-selected GPE-block Device. All256event numbers use uppercase
two-digit hex names. There is no ancestor fallback or discovery-time AML.
Aliases resolve to authenticated live method descriptors. Nonmethod definitions
are Unsupported_Handler; both trigger names are Ambiguous. This strict ambiguity
policy is explicit, not a claim that ACPICA's discovery chooses the same policy.
The ambiguous fixture uses an alias because iASL rejects direct duplicate GPE
methods. Missing/unsupported/ambiguous results have no callable binding.

Binding revalidation resolves both names again and compares identity, scope,
event and trigger. The metadata API does not expose method arity: resolving a
Method is not full signature validation. Invoke supplies zero arguments; a
malformed argument-taking alias is tested as an execution failure. No arbitrary
method name or argument array is accepted by this event API.

A fresh limited Invocation is consumed before evaluation, including invalid
binding or zero-budget attempts. Duplicate calls cannot replay. Normal scalar,
compound/reference/metadata return values are discarded; retained results get
one release attempt. Failed cleanup preserves the handle privately for Close.
The saved Core root authenticates cleanup even when method/scope identity has
changed. Foreign Core cleanup is refused. Close never evaluates AML. Keep the
Core generation/backing alive and serialize access until pending cleanup ends.

Coverage: all512names compared with standard-library hexadecimal formatting,
all256discovery numbers, canonical aliases, event0/255, root and device scopes,
foreign/non-scope rejection, no ancestor lookup, wrong/ambiguous/missing members,
zero/finite budgets, repeated attempts, package cleanup, wrong argument count,
overcharge and missing result handles, modeled Field_Indeterminate, retained
cleanup retry and wrong-owner rejection. Raw AML Fatal type/code/argument are
preserved as diagnostic data; the hosted interpreter never invokes an OS fatal
hook. Actual _E01/_L02 binding drives the published lifecycle, queues current lid
Notify and respects edge/level callback order; real AML error stays masked.

ACPICA verifies normal handler effects, alias/package returns, event0/255 and
device-scope calls, HITS/lid observations in both revisions. It does not validate
our ambiguity policy, modeled provider faults, source provenance or hardware
callback completion. Hardware callbacks use opaque source IDs in hosted memory.
The lifecycle fixtures/test are imported unchanged. The separate lifecycle
proof runner now establishes eight controller functional contracts plus eight
flow checks under an abstract callback model; it does not prove this binding
code. See tests/acpi-gpe-lifecycle/README.md for assumptions and reproduction.

Trusted requirements: the service owner independently associates source IDs,
epochs/capabilities, block-device identity, event number and provider readiness.
Bindings are metadata, not authority. A Device descriptor alone does not prove
it is a real GPE block. The evaluator/releaser satisfy Core owner/catalog/root/
budget contracts, do not raise/reset/reenter and preserve hardware control
ownership. The outer lifecycle requires source masking before invocation and
blocks automatic re-enable on failure. Do not discard an Invocation with pending
cleanup or retry an uncertain handler by allocating a new delivery session.

No wire role/endpoint/capability, physical address, register mapping, native
source provisioning or native logging is added. Actual SCI routing, active-bit
selection, admitted source association, provider/backend and native owner/loop/
logging integration, storm/wake/fixed/EC policy remain incomplete. Real bring-up
must follow coordination/release-stack.md logstore+serial stage/summary rules.
These new units are regression-tested, not newly SPARK-proved, syscall-executed,
native-booted or hardware-validated. Full AML/service correctness remains open.
