pragma Ada_2022;
with Interfaces;
with ACPI_Sleep_Preparation; with ACPI_Sleep_Objects; with ACPI_Sleep_Controller;
with ACPI_Local_Runtime_Status; with ACPI_Bootstrap.Power_Preparation;
generic
   with package Methods is new ACPI_Sleep_Preparation (Core => ACPI_Service, others => <>);
   with package Objects is new ACPI_Sleep_Objects (Core => ACPI_Service, others => <>);
   with package Driver is new ACPI_Sleep_Controller
     (Core => ACPI_Service, Methods => Methods, Objects => Objects, others => <>);
   with procedure Begin_Event (Service : ACPI_Service.State; Deadline_Microseconds : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   with procedure End_Event (Service : ACPI_Service.State; Completed : Boolean;
      Result : out ACPI_Local_Runtime_Status.Outcome);
package ACPI_Requests.Power_Preparation with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Bridge is new ACPI_Bootstrap.Power_Preparation (Methods, Objects, Driver, Begin_Event, End_Event);
   subtype Run is Bridge.Run;
   subtype Report is Bridge.Report;
   procedure Invalidate (Item : in out Run) renames Bridge.Invalidate;
   procedure Retire (Item : in out Run) renames Bridge.Retire;
   function Current_Types (Server : State; Item : Run) return Driver.Copied_Types
     with Pre => Valid (Server);
   procedure Prepare (Server : in out State; Item : in out Run;
      Registers : Objects.Register_Model; Budget : Natural;
      Deadline_Microseconds : Interfaces.Unsigned_64; Result : out Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old and then Result.Preparation.Charged <= Budget;
end ACPI_Requests.Power_Preparation;
