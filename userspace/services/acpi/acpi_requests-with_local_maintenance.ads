pragma Ada_2022;
with ACPI_Bootstrap.With_Local_Maintenance;
generic
   with procedure Step (Service : aliased in out ACPI_Service.State; Completed : out Boolean);
package ACPI_Requests.With_Local_Maintenance with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old(Allow);
   package Controller is new ACPI_Bootstrap.With_Local_Maintenance(Step);
   procedure Advance (Server : in out State; Allowed : out Controller.Admission; Completed : out Boolean)
     with Pre=>Valid(Server),Post=>Valid(Server) and then Revision(Server)=Revision(Server)'Old;
end ACPI_Requests.With_Local_Maintenance;
