pragma Ada_2022;
-- Runtime observation of this exact Core instance; no mutable access escapes.
generic
package ACPI_Service_Core.Runtime_Status with SPARK_Mode is
   function Namespace_Ready (Service : State) return Boolean
     with Global => null,
       Post => Namespace_Ready'Result = (Namespace_Load_Phase (Service) = Values.Ready);
end ACPI_Service_Core.Runtime_Status;
