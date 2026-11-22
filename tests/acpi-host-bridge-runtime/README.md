# Hosted ACPI host-bridge runtime regressions

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c bash tests/acpi-host-bridge-runtime/run.sh --mode checked
nix develop -c bash tests/acpi-host-bridge-runtime/run.sh --mode release
```

`--validate-only` verifies the pinned source/helper hashes without compiling.
The runner freezes only listed inputs into a private TMPDIR directory and
prints its result path. No shared build outputs, kernel/image/QEMU builds,
hardware access, or Git changes are involved. The GPR is generated privately.

Actual code covers the serialized host-bridge manager and scheduler, query
and enumeration adapters, resource collection/projection, snapshot codec,
channel adapter, original-grant retirement and known terminal completions.
The actual Requests/Core runs the synthetic AML root methods in both AML widths.
The base hosted ACPI_Service instantiation uses its unavailable delay provider.
ACPICA independently checks the minimal two-root HID/CRS fixture in AML32/64.

Helpers explicitly model channel setup, grants, IPC queues, endpoint/event
boundaries, module logging and heap allocation. Channel_Bells is the actual
option-B implementation. This suite does not execute native Main, physical
SCI/EC/GPIO, native request admission, launcher authority or kernel validation.
Separate native-entry/admission/SCI and hardware validation remain required.

Checked mode enables assertions/overflow checks on the selected host-bridge
family units and test mains. Inherited Requests/Core/Namespace dependencies
use suppressed checks because of the known GNAT namespace compiler issue.
Release mode suppresses checks throughout. Each ALI's selected profile is
verified. The byte-identical previously proved resource collector retains a
scoped exception for its redundant/unused-use warnings. No new proof is claimed.

Expected checks per profile: owner/scheduler 1606, snapshot 2362654, channel
537161, boundary 523982, terminal completion 200, process wake 15.
