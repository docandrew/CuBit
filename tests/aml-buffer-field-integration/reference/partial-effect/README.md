# Field package partial effects

The legacy `field_package_rollback_tests.adb` name now tests the user decision
recorded in coordination mail 20261123-032000: failures leave a valid state;
already performed effects remain. It does not require a whole-store rollback.

These four pinned ACPICA probes cover named/anonymous packages and fixed/generic
buffer fields. TEST writes 0x42 through the field before constructing a package
with a missing member. ACPICA logs AE_NOT_FOUND but TEST returns zero normally;
READ returns bytes 0x42,0x42. Sources, recorded logs, tool pins and reproduction
commands are in provenance.json. Compile each ASL to a private output prefix;
no AML binary is committed.

The Ada test separately leaves one or two free object slots to inject typed
quota failure. It checks arena validity, an authenticated unchanged global
backing object, persistent prior field data, monotonic incarnation and retirement
of method-local names. This is synthetic fault injection, not an ACPICA malloc
failure simulation. Namespace proof remains separately tracked.
