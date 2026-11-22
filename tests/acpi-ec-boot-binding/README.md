# ECDT namespace/UID association

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-boot-binding/run.sh

Use the pinned project Nix environment. The runner supports --source-root DIR
and --validate-only. It checks272actual source pins,two test inputs,normal iASL
AML compilation and ACPICA ECDT template/compilation; bounded checked/release
executables each pass167checks. No replacement Core is used.

ACPI_EC_Boot_Binding revalidates copied table bytes,byte-register compatibility
and rooted path,then resolves that exact path to a current Device. It evaluates
only the direct canonical _UID once with finite caller fuel. A returned integer
must equal the full ECDT32-bit UID; values above32bits cannot alias by truncation.
Missing local _UID never inherits an ancestor value. Aliases are followed and
the exact path/UID descriptors are re-resolved around evaluation.

UID_Matched is descriptive post-namespace association. It does not establish
PNP0C09,_STA/_INI/_REG,resource equivalence,GPE identity,GlobalLock,controller
ownership or any hardware/event rights. Native admission must separately bind
the copied table's resources and the namespace discovery claims. It is not the
early EC provider itself: ECDT may be needed before namespace evaluation.

Sessions consume before evaluation. Repeating Match never reevaluates. A saved
compound result remains held through cleanup failure and can be closed only in
its owning Core; successful late cleanup never publishes a UID match. Overcharge,
missing handles,wrong type,evaluator failure and Field uncertainty are reported
separately. Field uncertainty is retained even when an overcharge is also found;
absence of result roots does not establish lower-provider quiescence.

Device returns a historical descriptor only for UID_Matched. Is_Current checks
namespace identity and alias paths,not an unchanged dynamic _UID value. It never
reevaluates AML. Keep these observations separate from hardware capabilities.

Tests cover direct/alias/nested paths,integer/method/alias UID,max32-bit values,
above32-bit mismatch,string and alias-backed compound UID,missing selectors,
bad tables/registers/paths,zero fuel,repeated calls,foreign-Core cleanup,release
failures and inconsistent trusted evaluator observations. Failure metadata is
injected; normal UID values and effects come from actual Core/AML execution.

Independent reference after the runner emits OUTPUT:

    bash tests/acpi-ec-boot-binding/oracle.sh OUTPUT/ec-fields.aml

Repair-disabled ACPICA observes integers0,0,1,2,7,4294967295,4294967296,2,
string"2",and alias-backed buffer01,02. The last integer counts two method
effects. This checks fixture semantics,not the binding algorithm or hardware.
The raw log retains existing-style ACPICA cache allocation diagnostics at
shutdown; functional acceptance is not a clean-shutdown claim.

Normative reference: ACPI6.6 section5.2.15,ECDT requires UID equal to the value
returned by the namespace device's _UID:
https://uefi.org/specs/ACPI/6.6/05_ACPI_Software_Programming_Model.html#embedded-controller-boot-resources-table-ecdt

SPARK_Mode source is not a proof result. No new proof,native execution or physical
hardware acceptance is claimed. The full verified interpreter/service remains
unfinished. This helper is not attached to native startup or a hardware provider.
