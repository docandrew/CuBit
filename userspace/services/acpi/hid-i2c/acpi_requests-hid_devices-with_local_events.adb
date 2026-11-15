pragma Ada_2022;
with AML_Execute;
package body ACPI_Requests.HID_Devices.With_Local_Events is
   package R renames ACPI_Local_Runtime_Status;
   use type Controller.Admission,Controller.Search_Outcome,Discovery.Action_Result;
   use type Discovery.Outcome,Discovery.Queries.Identification_Outcome,Resolver.Outcome;
   use type AML_Execute.Execution_Status,Samples.Publication_Result,Samples.Availability,Samples.Cache_Status,R.Phase;
   function Collection_Report return Collection_Diagnostic is (Snapshot);
   procedure Invalidate(Data:in out Storage) is Ignored:Samples.Invalidation_Result;
   begin Samples.Invalidate(Data.Values,Ignored);end Invalidate;
   procedure Observe_Claimed(Data:in out Storage;Server:State;Observed:out Boolean) is
      pragma Unreferenced(Data,Server);
   begin Observed:=True;end Observe_Claimed;
   function Uncertain(Item:AML_Execute.Execution_Status) return Boolean is
     (Item in AML_Execute.Fatal_Requested|AML_Execute.Field_Indeterminate);
   function Recovery(Item:Controller.Report) return Boolean is
     (Item.Firmware.Result in Discovery.Cleanup_Failed|Discovery.Provider_Contract_Error
      or else Item.Firmware.Candidate.Outcome in Discovery.Queries.Cleanup_Failed|Discovery.Queries.Provider_Contract_Error
      or else Item.Firmware.Status.Outcome in Discovery.Queries.Cleanup_Failed|Discovery.Queries.Provider_Contract_Error
      or else Item.Firmware.Descriptor.Outcome in Discovery.Queries.Cleanup_Failed|Discovery.Queries.Provider_Contract_Error
      or else Uncertain(Item.Firmware.Execution) or else Uncertain(Item.Firmware.Candidate.Execution)
      or else Uncertain(Item.Firmware.Status.Execution) or else Uncertain(Item.Firmware.Descriptor.Execution));
   function Current(Value:Inventory.Observation;Server:State) return Boolean is
      Good:Boolean;
      procedure Check(Service:ACPI_Service.State;Completed:out Boolean) is
      begin
         Completed:=True;
         for I in 1..Value.Count loop
            if not Resolver.Current(Service,Value.Items(I)) then Completed:=False;return;end if;
         end loop;
      end Check;
      package Metadata is new ACPI_Requests.With_Metadata_Continuation(Check);
   begin Metadata.Advance(Server,Good);return Good;end Current;
   procedure Collect(Data:in out Storage;Server:in out State;Budget:Natural;Healthy,Collected:out Boolean) is
      Remaining:Natural:=Budget;
      Cursor,Device:ACPI_Service.Namespace.Node_ID:=ACPI_Service.Namespace.Root;
      Allowed:Controller.Admission;
      Action:Discovery.Action_Result;
      Bytes:Firmware_Tables.Bytes(1..Maximum_Resource_Bytes);
      function Charge(Amount:Natural) return Boolean is
      begin
         if Amount>Remaining then Snapshot.Charged:=Budget;return False;end if;
         Remaining:=Remaining-Amount;Snapshot.Charged:=Budget-Remaining;return True;
      end Charge;
      function Available return Boolean is
      begin
         if not Events_Healthy then Snapshot.Outcome:=Source_Unhealthy;return False;end if;
         if Runtime_Current/=R.Active then Snapshot.Outcome:=Recovery_Required;return False;end if;
         return True;
      end Available;
   begin
      Snapshot:=(others=><>);Healthy:=False;Collected:=False;
      Invalidate(Data);Data.Expected:=Samples.Revision(Data.Values);Data.Pending_Value:=Inventory.Empty_Observation;Data.Armed:=False;
      -- Never replace or replay a retained probe, even after consumer stop.
      if Controller.Pending(Data.Probe) then Snapshot.Outcome:=Recovery_Required;return;end if;
      if Samples.Status(Data.Values)=Samples.Exhausted then Snapshot.Outcome:=Counter_Exhausted;return;end if;
      loop
         if not Available then return;end if;
         Find(Server,Cursor,Remaining,Device,Snapshot.Search);
         if not Charge(Snapshot.Search.Charged) then Snapshot.Outcome:=Provider_Contract_Error;return;end if;
         if Snapshot.Search.Last_Identity.Outcome in Discovery.Queries.Cleanup_Failed|Discovery.Queries.Provider_Contract_Error
           or else Uncertain(Snapshot.Search.Last_Identity.Execution) then Snapshot.Outcome:=Recovery_Required;return;end if;
         exit when Snapshot.Search.Outcome=Controller.No_Match;
         if Snapshot.Search.Outcome/=Controller.Found then
            Snapshot.Outcome:=(if Snapshot.Search.Outcome=Controller.Incomplete then Budget_Exhausted else Search_Failed);return;
         end if;
         if Device<=Cursor then Snapshot.Outcome:=Search_Failed;return;end if;
         if Snapshot.Count=Inventory.Maximum_Devices then Snapshot.Outcome:=Inventory_Limit;return;end if;
         if not Available then return;end if;
         Begin_Probe(Data.Probe,Server,Device,Remaining,Allowed,Action);
         Snapshot.Probe:=Controller.Observe(Data.Probe);
         if Allowed/=Controller.Admitted or else Action/=Discovery.Accepted then Snapshot.Outcome:=Observation_Refused;return;end if;
         -- Inspect uncertainty before another operation or attempted cleanup.
         if not Recovery(Snapshot.Probe) and then Available and then Controller.Pending(Data.Probe) then
            Copy_And_Resolve(Data.Probe,Server,Bytes,Allowed,Action);
            Snapshot.Probe:=Controller.Observe(Data.Probe);
         end if;
         -- Firmware.Charged is cumulative over Begin and Copy, not a delta.
         if not Charge(Snapshot.Probe.Firmware.Charged) then Snapshot.Outcome:=Provider_Contract_Error;return;end if;
         if Recovery(Snapshot.Probe) then Snapshot.Outcome:=Recovery_Required;return;end if;
         if not Available then return;end if;
         if Allowed/=Controller.Admitted or else Action/=Discovery.Accepted then Snapshot.Outcome:=Observation_Refused;return;end if;
         if Controller.Pending(Data.Probe) then
            -- A small copied buffer is policy refusal. Release the retained
            -- value once; cleanup failure remains process-lived for the owner.
            Cancel(Data.Probe,Server,Action);Snapshot.Probe:=Controller.Observe(Data.Probe);
            if Controller.Pending(Data.Probe) or else Recovery(Snapshot.Probe) then Snapshot.Outcome:=Cleanup_Failed;
            else Snapshot.Outcome:=Capacity_Refused;end if;
            return;
         end if;
         if Snapshot.Probe.Firmware.Result=Discovery.Budget_Exhausted then Snapshot.Outcome:=Budget_Exhausted;return;end if;
         if Snapshot.Probe.Firmware.Result/=Discovery.Observed then Snapshot.Outcome:=Discovery_Failed;return;end if;
         if not Snapshot.Probe.Sources_Attempted or else Snapshot.Probe.Sources.Status/=Resolver.Resolved
           or else not ACPI_Requests.HID_Devices.Current(Data.Probe,Server) then Snapshot.Outcome:=Sources_Failed;return;end if;
         Snapshot.Count:=Snapshot.Count+1;Data.Pending_Value.Items(Snapshot.Count):=Snapshot.Probe.Sources.Value;
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
   procedure Stop renames Owner.Stop;
end ACPI_Requests.HID_Devices.With_Local_Events;
