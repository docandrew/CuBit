Hosted Field failure diagnostics

Export TMPDIR=/home/doc/cubit-build-tmp before entering the pinned Nix environment. Set ulimit -s 65536 and ulimit -c 0, then run node tests/acpi-field-diagnostics/run.mjs /absolute/fresh/output-directory. The runner copies its explicit, hash-checked production/test closure to that output and builds with one worker, nice19, a180second compile timeout and30second process timeout. Existing outputs are refused; failures retain their logs and partial receipt.

Expected975checks per checked/release profile:304NVS client failures,315captured PU2C geometry,251whole-Field lease checks and105diagnostic formatting/elapsed-time checks. These use modeled clocks, transport and hardware values; the PU2C fixture does not read real firmware memory. Coverage includes read-completed/Field-end-expired, retained original failure after cleanup, transport uncertainty and clock/deadline failures.

These checks do not identify the laptop's actual PU2C failure, execute native CuBit, prove the interpreter/service or establish hardware authority. The separate AML-owner/native semantic/startup-window checks are additional evidence outside this modular suite. Release uses -gnatp for functional testing; no new proof is claimed.
