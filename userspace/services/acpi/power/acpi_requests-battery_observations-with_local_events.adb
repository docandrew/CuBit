pragma Ada_2022;
with AML_Execute;
package body ACPI_Requests.Battery_Observations.With_Local_Events is
   package C renames ACPI_Service;
   package R renames ACPI_Local_Runtime_Status;
   use type C.Values.Access_Status,C.Namespace.Object_Kind;
   use type Controller.Discovery_Outcome,Collector.Outcome,Identity.Identification_Outcome;
   use type AML_Execute.Execution_Status,Samples.Publication_Result,Samples.Availability,Samples.Cache_Status,R.Phase;
   function Collection_Report return Collection_Diagnostic is (Snapshot);
   function Collection_Isolatable return Boolean is
     (Snapshot.Outcome in Search_Failed | Budget_Exhausted | Inventory_Limit);
   procedure Invalidate(Data:in out Storage) is Ignored:Samples.Invalidation_Result;
   begin Samples.Invalidate(Data.Values,Ignored);end Invalidate;
   procedure Observe_Claimed(Data:in out Storage;Server:State;Observed:out Boolean) is
      pragma Unreferenced(Data,Server);
   begin Observed:=True;end Observe_Claimed;
   function Uncertain(Item:AML_Execute.Execution_Status) return Boolean is
     (Item in AML_Execute.Fatal_Requested|AML_Execute.Field_Indeterminate);
   function Current(Value:Inventory.Observation;Server:State) return Boolean is
      Good:Boolean;
      procedure Check(Service:C.State;Completed:out Boolean) is
      begin
         Completed:=True;
         for I in 1..Value.Count loop
            if not C.Current_Binding(Service,Value.Items(I).Device) or else
              C.Values.Node_Kind(Value.Items(I).Device)/=C.Namespace.Device_Object then Completed:=False;return;end if;
         end loop;
      end Check;
      package Metadata is new ACPI_Requests.With_Metadata_Continuation(Check);
   begin Metadata.Advance(Server,Good);return Good;end Current;
   procedure Collect(Data:in out Storage;Server:in out State;Budget:Natural;Healthy,Collected:out Boolean) is
      Remaining:Natural:=Budget;
      Cursor,Device:C.Namespace.Node_ID:=C.Namespace.Root;
      Admitted,Described:Boolean;
      Descriptor:C.Node_Descriptor:=C.Values.No_Node;
      procedure Describe(Service:C.State;Completed:out Boolean) is
         Access_Result:C.Values.Access_Status;
      begin
         C.Describe_Node(Service,Device,Descriptor,Access_Result);
         Completed:=Access_Result=C.Values.Available and then C.Current_Binding(Service,Descriptor)
           and then C.Values.Node_Kind(Descriptor)=C.Namespace.Device_Object;
      end Describe;
      package Metadata is new ACPI_Requests.With_Metadata_Continuation(Describe);
   begin
      Snapshot:=(others=><>);Healthy:=False;Collected:=False;
      Invalidate(Data);Data.Expected:=Samples.Revision(Data.Values);Data.Pending_Value:=Inventory.Empty_Observation;Data.Armed:=False;
      if Samples.Status(Data.Values)=Samples.Exhausted then Snapshot.Outcome:=Counter_Exhausted;return;end if;
      loop
         if not Events_Healthy then Snapshot.Outcome:=Source_Unhealthy;return;end if;
         if Runtime_Current/=R.Active then Snapshot.Outcome:=Recovery_Required;return;end if;
         Find(Server,Cursor,Remaining,Device,Snapshot.Search);
         if Snapshot.Search.Charged>Remaining then Snapshot.Charged:=Budget;Snapshot.Outcome:=Provider_Contract_Error;return;end if;
         Remaining:=Remaining-Snapshot.Search.Charged;Snapshot.Charged:=Budget-Remaining;
         if Snapshot.Search.Last_Identity.Outcome in Identity.Cleanup_Failed|Identity.Provider_Contract_Error or else
           Uncertain(Snapshot.Search.Last_Identity.Execution) then Snapshot.Outcome:=Recovery_Required;return;end if;
         exit when Snapshot.Search.Outcome=Controller.No_Match;
         if Snapshot.Search.Outcome/=Controller.Found then
            Snapshot.Outcome:=(if Snapshot.Search.Outcome=Controller.Incomplete then Budget_Exhausted else Search_Failed);return;
         end if;
         if Device<=Cursor then Snapshot.Outcome:=Search_Failed;return;end if;
         if Snapshot.Count=Inventory.Maximum_Batteries then Snapshot.Outcome:=Inventory_Limit;return;end if;
         if not Events_Healthy then Snapshot.Outcome:=Source_Unhealthy;return;end if;
         if Runtime_Current/=R.Active then Snapshot.Outcome:=Recovery_Required;return;end if;
         Metadata.Advance(Server,Described);if not Described then Snapshot.Outcome:=Descriptor_Failed;return;end if;
         Remember_Device (Descriptor);
         Observe(Server,Device,Remaining,Admitted,Snapshot.Probe);
         if Snapshot.Probe.Charged>Remaining then Snapshot.Charged:=Budget;Snapshot.Outcome:=Provider_Contract_Error;return;end if;
         Remaining:=Remaining-Snapshot.Probe.Charged;Snapshot.Charged:=Budget-Remaining;
         if not Admitted then Snapshot.Outcome:=Observation_Refused;return;end if;
         if Snapshot.Probe.Status in Collector.Cleanup_Failed|Collector.Provider_Contract_Error or else
           Uncertain(Snapshot.Probe.Identity.Execution) or else Uncertain(Snapshot.Probe.Before_Presence.Execution) or else
           Uncertain(Snapshot.Probe.Static_Data.Extended_Data.Execution) or else Uncertain(Snapshot.Probe.Static_Data.Legacy_Data.Execution) or else
           Uncertain(Snapshot.Probe.Dynamic_Data.Execution) or else Uncertain(Snapshot.Probe.After_Presence.Execution) then
            Snapshot.Outcome:=Recovery_Required;return;
         end if;
         if Snapshot.Probe.Status=Collector.Budget_Exhausted then Snapshot.Outcome:=Budget_Exhausted;return;end if;
         if not Events_Healthy then Snapshot.Outcome:=Source_Unhealthy;return;end if;
         if Runtime_Current/=R.Active then Snapshot.Outcome:=Recovery_Required;return;end if;
         Snapshot.Count:=Snapshot.Count+1;
         declare I:constant Inventory.Slot:=Snapshot.Count;begin
            Data.Pending_Value.Items(I).Device:=Descriptor;Data.Pending_Value.Items(I).Reason:=Snapshot.Probe.Status;
            case Snapshot.Probe.Status is
               when Collector.Sampled=>Data.Pending_Value.Items(I).Status:=Inventory.Sampled;Data.Pending_Value.Items(I).Value:=Snapshot.Probe.Value;
               when Collector.Absent=>Data.Pending_Value.Items(I).Status:=Inventory.Known_Absent;
               when others=>null;
            end case;
         end;
         Data.Pending_Value.Count:=Snapshot.Count;Cursor:=Device;
      end loop;
      if not Current(Data.Pending_Value,Server) then Snapshot.Outcome:=Binding_Changed;return;end if;
      Data.Armed:=True;Healthy:=True;Collected:=True;Snapshot.Outcome:=Complete;
   end Collect;
   procedure Publish(Data:in out Storage;Server:State;At_Time:Samples.Clock_Reading;Published:out Boolean;Flags:out Interfaces.Unsigned_64) is
      Result:Samples.Publication_Result;
   begin
      Published:=False;Flags:=0;if not Data.Armed then return;end if;Data.Armed:=False;
      if ACPI_Requests.Notifications_Pending(Server) or else not Current(Data.Pending_Value,Server) then Invalidate(Data);return;end if;
      Samples.Publish(Data.Values,Data.Expected,Samples.Sampled,Data.Pending_Value,At_Time,Result);
      Published:=Result=Samples.Published;Data.Pending_Value:=Inventory.Empty_Observation;
   end Publish;
   function Cached_Read(Data:Storage;Server:State;Now:Samples.Clock_Reading;Maximum_Age:Samples.Tick) return Samples.View is
      V:Samples.View:=Samples.Read(Data.Values,Now,Maximum_Age);
   begin
      if ACPI_Requests.Notifications_Pending(Server) or else (V.Status=Samples.Available and then not Current(V.Value,Server)) then
         V.Status:=Samples.Not_Ready;V.Value:=Inventory.Empty_Observation;
      end if;
      return V;
   end Cached_Read;
   procedure Start(Server:State;Accepted:out Boolean) renames Owner.Start;
   function Started return Boolean renames Owner.Started;
   function Last_Failure return Status.Failure renames Owner.Last_Failure;
   procedure Route(Server:State;Routed:out Boolean) renames Owner.Route;
   procedure Advance(Server:in out State;Result:out Status.Outcome) renames Owner.Advance;
   function Read(Server:State;Now:Samples.Clock_Reading;Maximum_Age:Samples.Tick) return Samples.View renames Owner.Read;
   function Owns_Context(Server:State) return Boolean renames Owner.Owns_Context;
   procedure Request_Refresh(Server:State;At_Time:Samples.Clock_Reading;Accepted:out Boolean) renames Owner.Request_Refresh;
   function Metadata_View(Server:State) return Samples.View renames Owner.Metadata_View;
   function Metadata_Revision(Server:State) return Samples.Revision_Number renames Owner.Metadata_Revision;
   procedure Stop renames Owner.Stop;
end ACPI_Requests.Battery_Observations.With_Local_Events;
