pragma Ada_2022;
with ACPI_Battery;
with ACPI_Battery_Observation;
with ACPI_Bootstrap.Battery_Observations;
generic
   with package Identity is new ACPI_Battery (Core => ACPI_Service, others => <>);
   with package Collector is new ACPI_Battery_Observation
     (Core => ACPI_Service, Dynamic => Identity, others => <>);
package ACPI_Requests.Battery_Observations with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Controller is new ACPI_Bootstrap.Battery_Observations (Identity, Collector);
   procedure Find (Server : in out State; After : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Device : out ACPI_Service.Namespace.Node_ID;
      Report : out Controller.Discovery_Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old and then Report.Charged <= Budget;
   procedure Observe (Server : in out State; Device : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Admitted : out Boolean; Report : out Controller.Observation_Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old and then Report.Charged <= Budget;
end ACPI_Requests.Battery_Observations;
