pragma Ada_2022;
with Interfaces;with ACPI_Control_Method_Button;with ACPI_Control_Method_Button.Receipts;
with ACPI_Local_Runtime_Status;
generic
   with package Identity is new ACPI_Control_Method_Button (Core => ACPI_Service, others => <>);
   with procedure Begin_Event (Service : ACPI_Service.State; Deadline : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   with procedure End_Event (Service : ACPI_Service.State; Completed : Boolean;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   -- Bounded fact sink only, no AML, IPC, logger, reentry or owner reset.
   with procedure Record_Press;
package ACPI_Bootstrap.Power_Buttons with SPARK_Mode is
   package Bindings is new Identity.Receipts;
   type Run (Capacity : Positive) is limited private
     with Default_Initial_Condition => Valid (Run);
   function Valid (Item : Run) return Boolean;
   type Outcome is (Not_Ready, Already_Attempted, Reentered, Retired, Invalid_Limits,
      Window_Refused, Storage_Insufficient, Topology_Changed, Observation_Failed,
      Close_Failed, Unavailable, Ready);
   type Report is record
      Status : Outcome := Not_Ready;
      Scanned, Candidates, Bound, Charged : Natural := 0;
      Opened, Closed : Boolean := False;
      Open_Result, Close_Result : ACPI_Local_Runtime_Status.Outcome := ACPI_Local_Runtime_Status.Not_Attempted;
      Last_Binding : Bindings.Binding_Report;
   end record;
   -- Capacity is selected by the owner from the observed namespace extent;
   -- it is not a fixed machine limit. Insufficient storage refuses publication.
   procedure Discover (Session : in out State; Item : in out Run; Budget : Natural;
      Deadline : Interfaces.Unsigned_64; Result : out Report)
     with Pre => Valid (Session) and then Valid (Item),
       Post => Valid (Session) and then Valid (Item) and then Result.Charged <= Budget;
   function Available (Session : State; Item : Run) return Boolean
     with Pre => Valid (Session) and then Valid (Item);
   procedure Retire (Item : in out Run) with Pre => Valid (Item), Post => Valid (Item);
   function Quiet (Item : Run) return Boolean with Pre => Valid (Item);
   -- Called only with the SAME private broker's receipt/event before ACK.
   procedure Observe_Retained (Session : State; Item : in out Run;
      ID : Bindings.Receipt_Number; Event : ACPI_Service.Notification_Observation;
      Accepted : out Boolean) with Pre => Valid (Session) and then Valid (Item);
private
   type Binding_Array is array (Positive range <>) of Bindings.State;
   type Run (Capacity : Positive) is limited record
      Values : Binding_Array (1 .. Capacity);
      Used : Natural := 0;
      Attempted, Working, Retiring, Published, Opened, Closed : Boolean := False;
   end record;
end ACPI_Bootstrap.Power_Buttons;
