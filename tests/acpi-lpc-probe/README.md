# Read-only LPC LGMR probe regression

Run `tests/acpi-lpc-probe/run.sh` in the Nix development shell with TMPDIR exported to `/home/doc/cubit-build-tmp` beforehand. The runner copies and hashes current production inputs into a unique output directory.

Exercises the actual native probe body using modeled PCI-window, clock, boundary and logger dependencies. Tests checked and release profiles, exact read offsets/BDF, full-width identifiers, original finite deadline, no window setup, read failures, disabled LGMR, guard refusal and shed logging. Capture/report remain one-shot. No hardware IO, native executable or proof is claimed.
