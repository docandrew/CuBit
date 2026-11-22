# Retained definition requests

ACPI_Module_Paths shares bounded path parsing/resolution between Values.Arena and the owning Core.State. It selects existing current canonical table-root/parameter descriptors and does not mutate them.

ACPI_Retained_Requests.Append_With_Store composes table lookup, path selection, retained append and explicitly ordinary named Store for an exclusive idle owner. An empty parameter skips Store; duplicates replay neither stage. Used retained sources are validated before effects. Post-module Store failure closes the owner through the Core/namespace recovery barrier, preserving effects and pins while preventing further AML/retry. No raw arena or hardware authority is exported.

Strings/data are already evaluated. Budget accounts module plus Store, not full AML operand/path work. Existing parameter targets are required. This is not LoadTable opcode admission, active-caller/GC/provider integration, full parameter compatibility, independent table ownership or cleanup/readiness. No new SPARK proof or native/hardware validation is claimed. Tests/results are in tests/aml-module-execution-context.
