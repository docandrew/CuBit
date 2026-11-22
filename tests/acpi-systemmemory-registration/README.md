# SystemMemory registration replay

Run `tests/acpi-systemmemory-registration/run.sh` inside the Nix development environment; export TMPDIR=/home/doc/cubit-build-tmp before Nix. Inputs and outputs are hashed in a unique RESULT.json directory.

Exercises the actual AML Core, platform field provider, region cache, startup binding cache and fallback resolver. Captured laptop REG body uses true ERAX SystemMemory geometry (0xFE0B0400 +0xFF), OSTY offset0xA3 bits3 and LSTE offset0xB8 bit1, with ByteAcc/Lock/Preserve. An independently modeled kernel pool includes an unrelated member and either includes or refuses the ERAX page. Refusal must identify OSTY and preserve kernel OUTSIDE_POOL evidence without data IO. Successful execution preserves neighboring bits and reads LSTE through the same provider. Covers AML32/64; ACPICA supplies an independent method-result comparison with its zero-initialized simulated memory.

OSYS/GFX/LIDS and hardware/map/guard dependencies are models. Full Core code generation uses -O0 -gnatp per the known compiler limitation. No full-table, native/hardware validation or new SPARK proof is claimed.
