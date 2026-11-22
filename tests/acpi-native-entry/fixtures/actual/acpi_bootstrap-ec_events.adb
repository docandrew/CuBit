pragma Ada_2022;
package body ACPI_Bootstrap.EC_Events with SPARK_Mode is
   function Ready (Session : State) return Boolean is
     (Session.Stage = Complete and then Session.Device_Data.Stage = Devices_Completed);
   procedure Install
     (Session : State; Controller : ACPI_EC_Event_Types.Controller_ID;
      Epoch : ACPI_EC_Event_Types.Epoch_ID; Sender : CuBit.Process_IDs.Process_ID;
      Scope : ACPI_Service.Namespace.Node_ID; Allowed : out Admission;
      Result : out Receiver.Enrollment)
   is
      Description : ACPI_Service.Node_Descriptor;
      Status : ACPI_Service.Values.Access_Status;
   begin
      Allowed := Not_Ready; Result := Receiver.Invalid_Scope;
      if not Ready (Session) then return; end if;
      Allowed := Admitted;
      ACPI_Service.Describe_Node (Session.Core, Scope, Description, Status);
      if Status /= ACPI_Service.Values.Available then return; end if;
      Receiver.Install (Session.Core, Controller, Epoch, Sender, Description, Result);
   end Install;
   procedure Dispatch
     (Session : in out State; Run : in out Receiver.Delivery;
      Sender : CuBit.Process_IDs.Process_ID; Item : ACPI_EC_Event_Types.Notice;
      Budget : Natural; Allowed : out Admission; Result : out Receiver.Outcome) is
   begin
      Allowed := Not_Ready; Result := Receiver.Not_Started;
      if not Ready (Session) then return; end if;
      Allowed := Admitted;
      Receiver.Dispatch (Run, Session.Core, Sender, Item, Budget, Result);
   end Dispatch;
   procedure Close
     (Session : in out State; Run : in out Receiver.Delivery;
      Result : out Receiver.Close_Result) is
   begin
      Receiver.Close (Run, Session.Core, Result);
   end Close;
end ACPI_Bootstrap.EC_Events;
