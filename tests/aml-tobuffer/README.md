# Hosted ToBuffer checks

Run from repository root inside its Nix shell:

    python3 tests/acpi-hosted/run.py --group to-buffer --mode release
    python3 tests/acpi-hosted/run.py --group to-buffer --mode checked

Builds owner/collecting fixtures and runner, then replays88 exact cached ACPICA classifications plus2 separately recorded normal compiler rejections. Full Buffer bytes and exact statuses are checked. See REFERENCE.md for frozen reference provenance. Existing tests remain unchanged; canonical release and checked registration passed (worker82505). No native or full-ASLTS claim.
