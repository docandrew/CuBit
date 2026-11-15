# Native power-button source hosted regression

Run `bash tests/acpi-native-power-source/run.sh` inside pinned Nix after exporting
`TMPDIR=/home/doc/cubit-build-tmp`. `--validate-only` checks the source inventory,
fixtures and tools without compiling. `--source-root DIR` permits an independently
copied source tree. No private workspace or earlier result directory is required.

The actual native discovery, dynamic inventory initialization, source selection,
private Requests/Bootstrap/Core/AML, retained receipt routing and typed watch owner
execute. Fixtures model the One/root boundary, clock, allocation refusal, copied
diagnostic output, kernel IPC and a smaller hosted Core configuration. Successful
inventory allocation uses real host-backed aligned storage; unknown close must not
free it. These models do not establish native allocator/capability/hardware behavior.

Twenty processes cover ten scenarios in both AML integer widths: normal, fixed,
no matching source, method Notify, owner not ready, unavailable/overflowing clock,
allocation refusal, stop before startup and unconfirmed close. Expected total is
374 checks. Tests cover fixed/control-method selection, ignored wake/other-target
notifications, repeated native fanout before ACK, saved-watch stop drain, no source
restart, allocation release ownership and no diagnostic channel opening while
notifications/results remain pending. Whole-Core functionality uses O0/gnatp with
strict warnings; this is not assertion-enabled Core evidence or a formal proof.

Six fresh comparisons run the same AML through ACPICA with initialization and
return repair disabled, checking HID/STA and ordered Notify class/target/value.
Cache-tracking diagnostics are recorded separately. ACPICA does not validate
the CuBit allocator, broker, root lifetime or IPC transport.

The suite pins every compiled source and the current production composition as
metadata. It is not the full Native_Instance startup suite and does not replace
its legacy 88-case port. CCL native component/manifest/SDK checks and actual
service link, IPC, QEMU and Lenovo acceptance are separate gates. No native or
hardware result is claimed here.
