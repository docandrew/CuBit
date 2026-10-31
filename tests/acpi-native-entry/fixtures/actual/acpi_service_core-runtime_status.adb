pragma Ada_2022;
package body ACPI_Service_Core.Runtime_Status with SPARK_Mode is
   function Namespace_Ready (Service : State) return Boolean is
     (Values.Current (Service.Tree) = Values.Ready);
end ACPI_Service_Core.Runtime_Status;
