pragma Ada_2022;
package body ACPI_Requests.Batteries with SPARK_Mode is
   function Unseen (Data : Cache; Server : State) return Boolean is
     (Receipts.Current (Server.Receipt) /= 0
      and then Receipts.Current (Server.Receipt) /= Data.Last_Receipt);
   procedure Find
     (Server : in out State; After : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Device : out ACPI_Service.Namespace.Node_ID;
      Report : out Controller.Discovery_Report) is
   begin Controller.Find (Server.Boot, After, Budget, Device, Report); end Find;
   procedure Bind
     (Data : in out Cache; Server : State;
      Device : ACPI_Service.Namespace.Node_ID; Allowed : out Controller.Admission;
      Result : out Store.Binding_Result) is
   begin
      Data.Last_Receipt := 0;
      Controller.Bind (Data.Value, Server.Boot, Device, Allowed, Result);
   end Bind;
   procedure Forget (Data : in out Cache) is
   begin Store.Forget (Data.Value); Data.Last_Receipt := 0; end Forget;
   procedure Invalidate (Data : in out Cache) is
   begin Store.Invalidate (Data.Value); end Invalidate;
   procedure Prepare
     (Data : in out Cache; Server : in out State; Budget : Natural;
      Allowed : out Controller.Admission; Report : out Store.Refresh_Report) is
   begin Controller.Prepare (Data.Value, Server.Boot, Budget, Allowed, Report); end Prepare;
   procedure Observe_Claimed
     (Data : in out Cache; Server : State; Observed : out Boolean)
   is
      Ignored : Controller.Notifications.Routing_Result;
   begin
      Observed := Unseen (Data, Server);
      if not Observed then return; end if;
      Controller.Observe_Notification
        (Data.Value, Server.Boot, Server.Staged_Notification, Ignored);
      Data.Last_Receipt := Receipts.Current (Server.Receipt);
   end Observe_Claimed;
   procedure Publish
     (Data : in out Cache; Server : State; At_Time : Samples.Clock_Reading;
      Events_Healthy : Boolean; Report : out Store.Refresh_Report) is
   begin
      Controller.Publish (Data.Value, Server.Boot, At_Time,
        Events_Healthy and then not Unseen (Data, Server), Report);
   end Publish;
   function Read
     (Data : Cache; Server : State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Samples.View
   is
      Result : Samples.View := Controller.Read (Data.Value, Server.Boot, Now, Maximum_Age);
   begin
      if Unseen (Data, Server) then Result := (Revision => Result.Revision, others => <>); end if;
      return Result;
   end Read;
end ACPI_Requests.Batteries;
