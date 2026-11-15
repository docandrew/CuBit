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
      if Notifications_Pending(Server) then Allowed:=Controller.Notifications_Pending;Result:=Receiver.Not_Started;return;end if;
      Controller.Dispatch (Server.Boot, Run, Sender, Item, Budget, Allowed, Result);
   end Dispatch;
   procedure Close
     (Server : in out State; Run : in out Receiver.Delivery;
      Result : out Receiver.Close_Result) is
   begin
      Controller.Close (Server.Boot, Run, Result);
   end Close;
   procedure Retire (Server : in out State; Run : in out Receiver.Delivery;
      Allowed : out Controller.Admission; Accepted : out Boolean) is
   begin
      Accepted:=False;
      if Notifications_Pending(Server) then Allowed:=Controller.Notifications_Pending;return;end if;
      Controller.Retire(Server.Boot,Run,Allowed,Accepted);
   end Retire;
end ACPI_Requests.EC_Events;
