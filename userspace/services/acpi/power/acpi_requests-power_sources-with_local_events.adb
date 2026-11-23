pragma Ada_2022;
with AML_Execute;
package body ACPI_Requests.Power_Sources.With_Local_Events is
   use type Controller.Discovery_Outcome,Cached.Refresh_Outcome;
   use type Driver.Query_Outcome,Driver.Device_Status.Status_Outcome,AML_Execute.Execution_Status;
   function Collection_Report return Collection_Diagnostic is (Snapshot);
   procedure Collect (Data : in out Cache; Server : in out State; Budget : Natural;
      Healthy,Collected : out Boolean) is
      Remaining : Natural;
   begin
      Snapshot:=(others=><>);Healthy:=False;Collected:=False;May_Isolate:=False;
      Bind(Data,Server,Budget,Snapshot.Search);
      if Snapshot.Search.Charged>Budget then Snapshot.Charged:=Budget;return;end if;
      Snapshot.Charged:=Snapshot.Search.Charged;
      if Snapshot.Search.Outcome=Controller.No_Match then Healthy:=True;return;end if;
      if Snapshot.Search.Outcome/=Controller.Found then
         May_Isolate:=Snapshot.Search.Outcome in Controller.Incomplete|Controller.Failed
           and then Snapshot.Search.Last_Identity.Outcome not in Driver.Cleanup_Failed|Driver.Provider_Contract_Error|Driver.Identity_Changed
           and then Snapshot.Search.Last_Identity.Execution not in AML_Execute.Fatal_Requested|AML_Execute.Field_Indeterminate;
         return;
      end if;
      Remaining:=Budget-Snapshot.Charged;
      Prepare(Data,Server,Remaining,Snapshot.Refresh);
      if Snapshot.Refresh.Charged>Remaining then Snapshot.Charged:=Budget;return;end if;
      Snapshot.Charged:=Snapshot.Charged+Snapshot.Refresh.Charged;
      Collected:=Snapshot.Refresh.Status=Cached.Collected;
      May_Isolate:=Snapshot.Refresh.Device_Status.Outcome not in Driver.Device_Status.Cleanup_Failed|Driver.Device_Status.Provider_Contract_Error
        and then Snapshot.Refresh.Device_Status.Execution not in AML_Execute.Fatal_Requested|AML_Execute.Field_Indeterminate
        and then Snapshot.Refresh.Query.Outcome not in Driver.Cleanup_Failed|Driver.Provider_Contract_Error
        and then Snapshot.Refresh.Query.Execution not in AML_Execute.Fatal_Requested|AML_Execute.Field_Indeterminate;
      Healthy:=Collected and then May_Isolate;
   end Collect;
   procedure Publish_Collected (Data : in out Cache; Server : State; At_Time : Samples.Clock_Reading;
      Published : out Boolean; Flags : out Interfaces.Unsigned_64) is
      Report : Cached.Refresh_Report;
   begin
      Publish(Data,Server,At_Time,True,Report);Published:=Report.Status=Cached.Updated;Flags:=Report.Device_Status.Flags;
   end Publish_Collected;
   function Read_Cache (Data : Cache; Server : State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Samples.View is
     (ACPI_Requests.Power_Sources.Read(Data,Server,Now,Maximum_Age));
   procedure Start (Server : State; Accepted : out Boolean) renames Owner.Start;
   function Started return Boolean renames Owner.Started;
   function Last_Failure return Status.Failure renames Owner.Last_Failure;
   procedure Route (Server : State; Routed : out Boolean) renames Owner.Route;
   procedure Advance (Server : in out State; Result : out Status.Outcome) renames Owner.Advance;
   function Read (Server : State; Now : Samples.Clock_Reading; Maximum_Age : Samples.Tick) return Samples.View renames Owner.Read;
   function Metadata_View(Server:State) return Samples.View renames Owner.Metadata_View;
   function Metadata_Revision(Server:State) return Samples.Revision_Number renames Owner.Metadata_Revision;
   procedure Stop renames Owner.Stop;
end ACPI_Requests.Power_Sources.With_Local_Events;
