# Retained-region Stop policy regression

Run `bash tests/acpi-region-stop/run.sh` in the Nix development environment. Uses fresh temporary hosted outputs, two small checked/release executions (22 checks each), actual authenticated system-event classifier and opaque 64-bit process/control codecs. Message declarations are extracted from the runtime; no in-tree fingerprint catalog is required.

Every required local evidence flag is independently refused when unknown; valid Stop cannot override uncertain SCI/hardware/backing, and forged/malformed/non-Stop frames do not authorize exit. This does not execute native Park, exception-to-exit handling, kernel resource reclamation or physical hardware. Native compilation and image/laptop gates are separate evidence.
