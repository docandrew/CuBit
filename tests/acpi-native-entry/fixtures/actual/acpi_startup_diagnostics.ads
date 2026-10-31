with ACPI_Bootstrap;
-- Copied startup facts only. Formatting performs no AML, IO or owner access.
-- Failure enums are meaningful only in the phase that produced them.
package ACPI_Startup_Diagnostics is
   function Format (Item : ACPI_Bootstrap.Finish_Report) return String;
   function Device_Initialization_Completed
     (Item : ACPI_Bootstrap.Device_Report) return Boolean;
   function Format (Item : ACPI_Bootstrap.Device_Report) return String;
end ACPI_Startup_Diagnostics;
