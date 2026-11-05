Selected production firmware-data request-codec proof

Export TMPDIR=/home/doc/cubit-build-tmp before entering the pinned Nix environment, then run node tests/acpi-firmware-data-codec-proof/run.mjs. The runner pins source/tool bytes, uses one worker, a180second process limit and5second solver limits, and inspects the machine-readable proof artifact as well as the summary. GNATprove FS15_2025-06-10, CVC5 1.3.4 and Z3 4.16.0 are the pinned host toolchain.

Expected:53obligations (44run-time,1initialization,1data-dependency,7termination), no skipped/unproved/justified checks and no pragma Assume. Imported contracts remain trusted in this selected proof. No functional round-trip contract, lease/controller/authority, complete interpreter/service or native/hardware proof is claimed. Historical receipts are not repinned by this runner.
