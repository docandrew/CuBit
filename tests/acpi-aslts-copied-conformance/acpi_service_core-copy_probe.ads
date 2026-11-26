pragma Ada_2022;
generic
package ACPI_Service_Core.Copy_Probe with SPARK_Mode is
   function Tables (Service : State) return Natural with Pre => Valid (Service);
   function Bytes (Service : State) return Natural with Pre => Valid (Service);
end ACPI_Service_Core.Copy_Probe;
