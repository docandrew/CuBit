# Native process-construction rejection and rollback

```sh
nix develop -c make -C kernel capability-security-image
nix develop -c tests/headless/run.sh --test capability-security --disk kernel/capability_security_disk.img --accel kvm --cpus 4 --timeout 90
```

`make-fixtures.py` derives four intentionally invalid executables from the
authorityless capability-test binary. They exist only in ignored build output;
no new authority or production fault-injection syscall is introduced.

The startup profile attempts an invalid program-header size, an overflowing
file extent, and an invalid declared stack size. It then tries eight copies of
an ELF whose final load segment collides with an earlier mapping. That last
case fails **after** the process's stack and executable pages have been mapped,
exercising `discardUnpublished` on real kernel resources.

The log checker requires eight completed rollbacks, the same reclaimed PID on
each attempt, and a valid follow-up launch using that PID. The valid app must
then pass all stack-growth, heap-admission and authority checks. This catches
PID leaks, not merely whether the machine still has enough spare slots to boot.

The allocation host suite additionally tests and proves the integer-only
ELF64 program-header admission helpers. It covers file/memory extents, wrapping
values, header-table bounds, alignment and image/heap guard space.

Boundaries: the metadata snapshot prevents geometry from changing between
validation and construction. It does not authenticate or freeze executable
payload bytes. SPAWN now checks source geometry and copies required bytes and
the bounded name through the [checked user-memory adapter](../user-memory/README.md).
This fixture does not inject invalid pointers into an authorized SPAWN. Global physical
exhaustion, kernel guard-page SMP updates and every cleanup race are not tested
by this fixture. Kernel stack-use limits are per function, not a call-chain proof.
