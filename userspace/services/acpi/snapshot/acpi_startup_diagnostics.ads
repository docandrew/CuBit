with AML_Execute;
with ACPI_Capacity_Diagnostics;
with ACPI_Bootstrap;
-- Copied startup facts only. Formatting performs no AML, IO or owner access.
-- Failure enums are meaningful only in the phase that produced them.
package ACPI_Startup_Diagnostics is
   -- Explicit strings remain available with the native Discard_Names runtime.
   function Execution_Name (Item : AML_Execute.Execution_Status) return String;
   Maximum_Module_Text_Length : constant Positive := 384;
   function Format (Item : ACPI_Bootstrap.Finish_Report) return String
     with Post => Format'Result'Length <= Maximum_Module_Text_Length;
   function Format_Field_Failure
     (Item : ACPI_Bootstrap.Finish_Report;
      Capacity : ACPI_Capacity_Diagnostics.Report := ACPI_Capacity_Diagnostics.No_Report) return String;
   function Device_Initialization_Completed
     (Item : ACPI_Bootstrap.Device_Report) return Boolean;
   function Format (Item : ACPI_Bootstrap.Device_Report) return String;
end ACPI_Startup_Diagnostics;
