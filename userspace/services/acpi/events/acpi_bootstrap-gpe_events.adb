pragma Ada_2022;
package body ACPI_Bootstrap.GPE_Events with SPARK_Mode is
   function Ready(Session:State) return Boolean is
     (Device_Walk_Finished (Session));
   procedure Resolve(Session:State;Event:Handlers.Event_Number;Item:out Handlers.Binding;
      Allowed:out Admission;Result:out Handlers.Resolution_Report) is
      Node:ACPI_Service.Namespace.Node_ID;
      Scope:ACPI_Service.Node_Descriptor;
      Status:ACPI_Service.Values.Access_Status;
   begin
      Allowed:=Not_Ready;Item:=Handlers.No_Binding;Result:=(others=><>);
      if not Ready(Session) then return;end if;Allowed:=Admitted;
      ACPI_Service.Find_Child(Session.Core,ACPI_Service.Namespace.Root,"_GPE",Node,Status);
      if Status/=ACPI_Service.Values.Available then Result.Access_Result:=Status;return;end if;
      ACPI_Service.Describe_Node(Session.Core,Node,Scope,Status);
      if Status/=ACPI_Service.Values.Available then Result.Status:=Handlers.Lookup_Failed;Result.Access_Result:=Status;return;end if;
      Handlers.Resolve(Session.Core,Scope,Event,Item,Result);
   end Resolve;
   procedure Invoke(Session:in out State;Run:in out Handlers.Invocation;Item:Handlers.Binding;
      Budget:Natural;Allowed:out Admission;Action:out Handlers.Action_Result) is
   begin
      Allowed:=Not_Ready;Action:=Handlers.Already_Consumed;
      if not Ready(Session) then return;end if;
      if Session.Notify_Enabled and then Natural (ACPI_Service.Pending_Notifications(Session.Core)) > 0 then Allowed:=Notifications_Pending;return;end if;
      if Retained_Results_Pending(Session) then Allowed:=Results_Pending;return;end if;
      Allowed:=Admitted;Handlers.Invoke(Run,Session.Core,Item,Budget,Action);
   end Invoke;
   procedure Close(Session:in out State;Run:in out Handlers.Invocation;Action:out Handlers.Action_Result) is
   begin Handlers.Close(Run,Session.Core,Action);end Close;
   procedure Deliver_Local (Session : in out State; Item : Handlers.Binding;
      Budget : Natural; Allowed : out Admission; Completed : out Boolean) is
   begin
      Completed := False; Allowed := Not_Ready;
      if not Ready (Session) then return; end if;
      if Session.Notify_Enabled and then Natural (ACPI_Service.Pending_Notifications (Session.Core)) > 0 then
         Allowed := Notifications_Pending; return;
      end if;
      if Retained_Results_Pending (Session) then Allowed := Results_Pending; return; end if;
      Allowed := Admitted; Dispatch (Session.Core, Item, Budget, Completed);
   end Deliver_Local;
   procedure Close_Local (Session : in out State; Action : out Handlers.Action_Result) is
   begin Close_Pending (Session.Core, Action); end Close_Local;
end ACPI_Bootstrap.GPE_Events;
