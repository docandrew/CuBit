pragma Ada_2022;
with Interfaces;with ACPI_Control_Method_Button;with ACPI_Bootstrap.Power_Buttons;
with ACPI_Local_Runtime_Status;
generic
   with package Identity is new ACPI_Control_Method_Button (Core => ACPI_Service, others => <>);
   with procedure Begin_Event (Service : ACPI_Service.State; Deadline : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   with procedure End_Event (Service : ACPI_Service.State; Completed : Boolean;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   with procedure Record_Press;
package ACPI_Requests.Power_Buttons with SPARK_Mode is
   package Bridge is new ACPI_Bootstrap.Power_Buttons (Identity, Begin_Event, End_Event, Record_Press);
   subtype Run is Bridge.Run;
   subtype Report is Bridge.Report;
   procedure Discover (Server : in out State; Item : in out Run; Budget : Natural;
      Deadline : Interfaces.Unsigned_64; Result : out Report)
     with Pre => Valid (Server), Post => Valid (Server) and then Result.Charged <= Budget;
   function Available (Server : State; Item : Run) return Boolean with Pre => Valid (Server);
   procedure Route (Server : State; Item : in out Run; Accepted : out Boolean) with Pre => Valid (Server);
   procedure Retire (Item : in out Run) renames Bridge.Retire;
   function Quiet (Item : Run) return Boolean renames Bridge.Quiet;
end ACPI_Requests.Power_Buttons;
