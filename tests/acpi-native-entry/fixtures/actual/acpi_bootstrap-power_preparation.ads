pragma Ada_2022;
with Interfaces;
with ACPI_Sleep_Preparation; with ACPI_Sleep_Objects; with ACPI_Sleep_Controller;
with ACPI_Local_Runtime_Status;
generic
   with package Methods is new ACPI_Sleep_Preparation (Core => ACPI_Service, others => <>);
   with package Objects is new ACPI_Sleep_Objects (Core => ACPI_Service, others => <>);
   with package Driver is new ACPI_Sleep_Controller
     (Core => ACPI_Service, Methods => Methods, Objects => Objects, others => <>);
   with procedure Begin_Event (Service : ACPI_Service.State; Deadline_Microseconds : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   with procedure End_Event (Service : ACPI_Service.State; Completed : Boolean;
      Result : out ACPI_Local_Runtime_Status.Outcome);
-- Same private Core, one finite window, no wire request or register access.
package ACPI_Bootstrap.Power_Preparation with SPARK_Mode is
   type Outcome is (Not_Ready, Reentered, Retired, Window_Refused,
      Preparation_Failed, Close_Failed, Prepared, Prepared_With_Method_Failure);
   type Report is record
      Status : Outcome := Not_Ready;
      Opened, Closed : Boolean := False;
      Open_Result, Close_Result : ACPI_Local_Runtime_Status.Outcome := ACPI_Local_Runtime_Status.Not_Attempted;
      Preparation : Driver.Report;
   end record;
   type Run is limited private;
   procedure Invalidate (Item : in out Run);
   procedure Retire (Item : in out Run);
   function Current_Types (Session : State; Item : Run) return Driver.Copied_Types
     with Pre => Valid (Session);
   procedure Prepare (Session : in out State; Item : in out Run;
      Registers : Objects.Register_Model; Budget : Natural;
      Deadline_Microseconds : Interfaces.Unsigned_64; Result : out Report)
     with Pre => Valid (Session), Post => Valid (Session) and then Result.Preparation.Charged <= Budget;
private
   type Run is limited record
      Controller : Driver.State;
      Ready, Working, Retiring : Boolean := False;
      Invalidated : Boolean := True;
   end record;
end ACPI_Bootstrap.Power_Preparation;
