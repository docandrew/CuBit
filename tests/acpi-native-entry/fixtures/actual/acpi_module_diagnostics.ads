with ACPI_Service;
package ACPI_Module_Diagnostics with SPARK_Mode is
   subtype Observation is ACPI_Service.Module_Error_Observation;
   subtype Count is ACPI_Service.Namespace.Owned.Module_Error_Count;
   -- Resume offset and charge are module-local; no address or authority exports.
   Maximum_Text_Length : constant Positive := 200;
   function Format (Item : Observation) return String
     with Global => null, Post => Format'Result'Length <= Maximum_Text_Length;
end ACPI_Module_Diagnostics;
