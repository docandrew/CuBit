pragma Ada_2022;
with AML_Copied_Tables;
package body ACPI_Service_Core.Copy_Probe with SPARK_Mode is
   function Tables (Service : State) return Natural is
     (AML_Copied_Tables.Count (Service.Copies));
   function Bytes (Service : State) return Natural is
     (AML_Copied_Tables.Used_Bytes (Service.Copies));
end ACPI_Service_Core.Copy_Probe;
