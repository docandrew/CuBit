pragma Ada_2022;
package body ACPI_Requests.GPE_Events with SPARK_Mode is
   procedure Resolve(Server:State;Event:Handlers.Event_Number;Item:out Handlers.Binding;
      Allowed:out Controller.Admission;Result:out Handlers.Resolution_Report) is
   begin Controller.Resolve(Server.Boot,Event,Item,Allowed,Result);end Resolve;
   procedure Invoke(Server:in out State;Run:in out Handlers.Invocation;Item:Handlers.Binding;
      Budget:Natural;Allowed:out Controller.Admission;Action:out Handlers.Action_Result) is
   begin
      if Notifications_Pending(Server) then Allowed:=Controller.Notifications_Pending;Action:=Handlers.Already_Consumed;return;end if;
      Controller.Invoke(Server.Boot,Run,Item,Budget,Allowed,Action);
   end Invoke;
   procedure Close(Server:in out State;Run:in out Handlers.Invocation;Action:out Handlers.Action_Result) is
   begin Controller.Close(Server.Boot,Run,Action);end Close;
end ACPI_Requests.GPE_Events;
