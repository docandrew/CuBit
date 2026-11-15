# Bounded GPIO bootstrap observation

Export TMPDIR=/home/doc/cubit-build-tmp before entering Nix, then run bash tests/acpi-gpio-bootstrap-observation/run.sh. --validate-only checks exact inventories/hashes; --source-root selects a guarded candidate.

The actual Core evaluates the captured GPI0 method fragment with modeled PNVA inputs. Eight cases cover AML32/64, INTC1057/INTC1055, disabled PNP0C02 and zero SBRG. The one-shot observer shares fuel across STA/HID/CRS, rejects disabled status before resource evaluation, preserves a retained CRS on small-copy refusal, reuses it without AML replay, refuses foreign-owner cleanup, and recovers an attributable failed release only on explicit Close. Earlier same-Core repeated Buffer/CreateDWordField cleanup tests remain. ACPICA with initialization/repair disabled independently checks identity, status and every resource byte.

Native allocator/Field/PNVA/Global Lock/pool/interrupt behavior is not modeled as proven. This suite performs no device INI, mapping admission, I2C or HID transport. The native adapter is separately semantically checked; startup integration awaits the distinct coordinator-owned bootstrap admission contract. A copied resource description grants no rights. Outer startup must establish its actual quiet IPC boundary before handing off facts.

Whole Core functional compilation is -O0 -gnatp with explicit Check failures. One compile job/nice19,180s compile,30s runtime,24GiB address space,64MiB stack/core0 via approved wrapper; ACPICA5s/128MiB/2MiB stack. These are regression tests, not full SPARK proof or native/hardware validation.

The resource copy also runs with a caller array whose final index is Positive'Last; the observer computes its inclusive final index without overflowing the intermediate addition. The concrete SDK baseline includes the coordinator's exact image12 live-replace source prerequisites; those files are not overwritten by this library packet.
