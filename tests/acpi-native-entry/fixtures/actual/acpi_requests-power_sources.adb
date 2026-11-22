pragma Ada_2022;
package body ACPI_Requests.Power_Sources with SPARK_Mode is
   use type ACPI_Bootstrap.Device_Phase;
   function Unseen (Data : Cache; Server : State) return Boolean is
     (Receipts.Current (Server.Receipt) /= 0 and then
      Receipts.Current (Server.Receipt) /= Data.Last_Receipt);
   procedure Forget (Data : in out Cache) is
   begin Cached.Forget (Data.Value); Data.Last_Receipt := 0; end Forget;
   procedure Invalidate (Data : in out Cache) is
   begin Cached.Invalidate (Data.Value); end Invalidate;
   procedure Bind (Data : in out Cache; Server : in out State; Budget : Natural;
      Report : out Controller.Discovery_Report) is
   begin
      Data.Last_Receipt := 0;
      Cached.Bind (Data.Value, Server.Boot, Budget, Report);
   end Bind;
   procedure Prepare (Data : in out Cache; Server : in out State; Budget : Natural;
      Report : out Cached.Refresh_Report) is
   begin Cached.Prepare (Data.Value, Server.Boot, Budget, Report); end Prepare;
   procedure Observe_Claimed (Data : in out Cache; Server : State; Observed : out Boolean) is
   begin
      Observed := Unseen (Data, Server);
      if not Observed then return; end if;
      Cached.Observe_Notification (Data.Value, Server.Boot, Server.Staged_Notification);
      Data.Last_Receipt := Receipts.Current (Server.Receipt);
   end Observe_Claimed;
   procedure Publish (Data : in out Cache; Server : State;
      At_Time : Samples.Clock_Reading; Events_Healthy : Boolean;
      Report : out Cached.Refresh_Report) is
   begin
      Cached.Publish (Data.Value, Server.Boot, At_Time,
        Events_Healthy and then not Unseen (Data, Server), Report);
   end Publish;
   function Read (Data : Cache; Server : State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Samples.View is
      Result : Samples.View := Cached.Read (Data.Value, Now, Maximum_Age);
   begin
      if Current (Server) /= Complete
        or else ACPI_Bootstrap.Device_Completion (Server.Boot).Stage /= ACPI_Bootstrap.Devices_Completed
        or else not ACPI_Bootstrap.Notifications_Enabled (Server.Boot)
        or else ACPI_Bootstrap.Pending_Notifications (Server.Boot) /= 0
        or else Unseen (Data, Server) then
         Result.Status := Samples.Not_Ready;
         Result.Value := Cached.Empty_Sample;
      end if;
      return Result;
   end Read;
   procedure Sample_Ready (Server : in out State; Budget : Natural;
      Report : out Controller.Sampling_Report) is
   begin Controller.Sample_Ready (Server.Boot, Budget, Report); end Sample_Ready;
end ACPI_Requests.Power_Sources;
