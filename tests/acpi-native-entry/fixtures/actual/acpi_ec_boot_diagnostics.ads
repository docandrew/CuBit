with ACPI_EC_Boot_Observations;
package ACPI_EC_Boot_Diagnostics with SPARK_Mode, Pure is
   Maximum_Text_Length : constant Positive := 384;
   function Described (Item : ACPI_EC_Boot_Observations.Report) return Boolean;
   function Format (Item : ACPI_EC_Boot_Observations.Report) return String
     with Post => Format'Result'Length <= Maximum_Text_Length;
end ACPI_EC_Boot_Diagnostics;
