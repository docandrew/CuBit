pragma Ada_2022;
package body ACPI_Requests.EC_Events with SPARK_Mode is
   procedure Install
     (Server : State; Source : ACPI_EC_Event_Types.Controller_ID;
      Epoch : ACPI_EC_Event_Types.Epoch_ID; Sender : CuBit.Process_IDs.Process_ID;
      Scope : ACPI_Service.Namespace.Node_ID; Allowed : out Controller.Admission;
      Result : out Receiver.Enrollment) is
   begin
      Controller.Install (Server.Boot, Source, Epoch, Sender, Scope, Allowed, Result);
   end Install;
   procedure Dispatch
     (Server : in out State; Run : in out Receiver.Delivery;
      Sender : CuBit.Process_IDs.Process_ID; Item : ACPI_EC_Event_Types.Notice;
      Budget : Natural; Allowed : out Controller.Admission;
      Result : out Receiver.Outcome) is
   begin
      Controller.Dispatch (Server.Boot, Run, Sender, Item, Budget, Allowed, Result);
   end Dispatch;
   procedure Close
     (Server : in out State; Run : in out Receiver.Delivery;
      Result : out Receiver.Close_Result) is
   begin
      Controller.Close (Server.Boot, Run, Result);
   end Close;
end ACPI_Requests.EC_Events;
