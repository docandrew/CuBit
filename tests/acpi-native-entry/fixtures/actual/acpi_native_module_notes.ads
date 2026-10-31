with ACPI_Module_Diagnostics;
-- Process-lifetime cursor; uses the logger as-is and never evaluates AML.
package ACPI_Native_Module_Notes is
   procedure Observe (Item : ACPI_Module_Diagnostics.Observation);
end ACPI_Native_Module_Notes;
