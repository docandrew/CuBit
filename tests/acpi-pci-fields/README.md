# Hosted PCI fields

Run `bash tests/acpi-pci-fields/run.sh` inside the Nix development environment, with TMPDIR=/home/doc/cubit-build-tmp. Uses private outputs, strict source hashes, one compiler job, 180-second compilation and 30-second process limits, checked and release profiles.

Covers complete bus0 geometry, read-only SystemMemory aliases, finite deadlines, named metadata/identity checks, stale authority, refusal, uncertain completion, target growth and actual AML startup. Hardware facts, transport, clock and allocation are modeled. Synthetic AML field extraction is compared with ACPICA; its known shutdown allocation diagnostics are retained. No native execution, hardware validation or full-service proof is implied. Captured laptop tables and NVS contents are not included.
