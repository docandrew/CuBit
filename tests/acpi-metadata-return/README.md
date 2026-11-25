# Nested metadata-return regression

This hosted test imports one immutable synthetic DSDT into the real service
core. It needs no hardware, field provider, clock, delays or native service.
The embedded Ada bytes are compiled from `metadata_return.asl`; running the
test does not require ACPICA.

From the repository root, using the pinned development environment:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
timeout --kill-after=2s 240s nix develop -c gprbuild -q -p -j2 -P tests/acpi-metadata-return/metadata_return.gpr -XPROFILE=checked
timeout --kill-after=2s 60s nix develop -c prlimit --as=25769803776 --stack=134217728 tests/acpi-metadata-return/bin/checked/metadata_return_tests
```

Use `-XPROFILE=shipping` and `bin/shipping/metadata_return_tests` for the
shipping compiler profile. Checked builds enable assertions and overflow
checks; both profiles use O1 and treat warnings as errors.

The test checks scalar and Buffer-return controls first, then retained root
Device and ThermalZone results and their release. Eight nested cases cover
discarded returns, an additional method layer and metadata consumed by
`CopyObject` and `ObjectType`. Every scalar call leaves zero retained result
handles. An ordinary argument-count mismatch must remain rejected.

Without the fix, the first nested case, `TD00`, fails after the earlier
controls complete. In shipping mode its status is `UNSUPPORTED_VALUE`;
checked mode may stop at the existing operand-status postcondition. With the
fix, the test prints `METADATA RETURN: PASS 55` in both profiles.

The fix normalizes a successful nested metadata result to the operand
evaluator's `Returned` status, like its object and reference branches. Root
retained metadata results keep their public `Metadata_Returned` status.
This regression is functional evidence; it does not establish whole-service
proof or native integration.
