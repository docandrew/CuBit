pragma Ada_2022;
with Interfaces;
generic
package ACPI_Control_Method_Button.Receipts with SPARK_Mode is
   subtype Receipt_Number is Interfaces.Unsigned_64;
   type State is limited private;
   type Binding_Outcome is (Not_Attempted, Bound, Already_Attempted, Identity_Refused,
      Status_Refused, Device_Unavailable, Provider_Error);
   type Binding_Report is record
      Outcome : Binding_Outcome := Not_Attempted;
      Identity : Identification_Report;
      Status : Device_Status.Status_Report;
      Charged : Natural := 0;
   end record;
   -- One lifetime, one same-Core descriptive binding. Caller establishes
   -- completed initialization and runtime admission; this grants no authority.
   -- Fixed HID and fresh STA share the accepted total budget. No reset/rebind.
   procedure Bind (Item : in out State; Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Binding_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Report.Charged <= Budget;
   function Is_Bound (Item : State; Service : Core.State) return Boolean
     with Pre => Core.Valid (Service);
   type Observation is (Rejected, Ignored, Pressed, Binding_Lost);
   -- SAME owner's retained receipt only, before fanout ACK. No AML, IPC, logger
   -- or queue operations. Repeated/older receipts never emit another press.
   procedure Observe (Item : in out State; Service : Core.State;
      ID : Receipt_Number; Event : Core.Notification_Observation; Result : out Observation)
     with Pre => Core.Valid (Service);
private
   type State is limited record
      Attempted, Has_Binding : Boolean := False;
      Device : Core.Node_Descriptor := Core.Values.No_Node;
      Last_Receipt : Receipt_Number := 0;
   end record;
end ACPI_Control_Method_Button.Receipts;
