pragma Ada_2022;
with AML_Execute;
package body ACPI_Requests.HID_Offers with SPARK_Mode is
   use type ACPI_Service.Values.Access_Status;
   function Error_Name (Kind : Error_Kind) return String is
     (case Kind is
        when Policy_Error => "POLICY_ERROR",
        when Firmware_Error => "FIRMWARE_ERROR",
        when Source_Error => "SOURCE_ERROR",
        when Topology_Error => "TOPOLOGY_ERROR",
        when Description_Error => "DESCRIPTION_ERROR");
   package C renames ACPI_Service;
   use type C.Node_Descriptor;
   use type HID.Controller.Admission, HID.Controller.Policy_Outcome, Discovery.Action_Result;
   use type Discovery.Outcome, Resolver.Outcome;
   use type Discovery.Queries.Status_Outcome;
   use type PCI.Controller.Admission, Reader.Topology_Outcome, Reader.Outcome;
   use type Provenance.Outcome;
   use type Builder.Outcome;
   function Observe (Item : Run) return Report is (Item.View);
   function Uncertain (Status : AML_Execute.Execution_Status) return Boolean is
     (Status in AML_Execute.Fatal_Requested | AML_Execute.Field_Indeterminate);
   function Recovery (Info : HID.Controller.Report) return Boolean is
     (Info.Firmware.Result in Discovery.Cleanup_Failed | Discovery.Provider_Contract_Error
      or else Info.Firmware.Candidate.Outcome in Discovery.Queries.Cleanup_Failed |
        Discovery.Queries.Provider_Contract_Error
      or else Info.Firmware.Status.Outcome in Discovery.Queries.Cleanup_Failed |
        Discovery.Queries.Provider_Contract_Error
      or else Info.Firmware.Descriptor.Outcome in Discovery.Queries.Cleanup_Failed |
        Discovery.Queries.Provider_Contract_Error
      or else Uncertain (Info.Firmware.Execution)
      or else Uncertain (Info.Firmware.Candidate.Execution)
      or else Uncertain (Info.Firmware.Status.Execution)
      or else Uncertain (Info.Firmware.Descriptor.Execution));
   type Candidate_Result is (Candidate_Ready, Candidate_Unavailable, Candidate_Error, Batch_Stopped);
   procedure Collect_With_Policy (Item : in out Run; Server : in out State;
      Devices : Device_Array; Count : Candidate_Count;
      Admission_Device : C.Node_Descriptor;
      Admission : GPIO_Bootstrap_Payload.Request; History : Provenance.Stamp;
      Budget : Natural; Resource_Bytes : Positive; Result : out Report) is
      procedure Check is new HID.Check_With_Policy (Check_Candidate);
      Allowed : HID.Controller.Admission;
      PCI_Allowed : PCI.Controller.Admission;
      Action : Discovery.Action_Result;
      Info : HID.Controller.Report;
      Data : Firmware_Tables.Bytes (1 .. Resource_Bytes);
      Completed, Charged : Boolean;
      Selected : Candidate_Index := Candidate_Index'First;
      Current_Error : Error_Kind := Policy_Error;
      Candidate_Started : Natural := 0;
      Built : Builder.Report;
      Assembly_Status : Provenance.Outcome;
      Assembly_Attempted : Boolean := False;
      Facts : Builder.Wire.PCI_Observation;
      procedure Check_Device (Service : C.State; Done : out Boolean) is
      begin
         Done := C.Retained_Results (Service) = 0
           and then C.Current_Node (Service, Admission_Device)
           and then C.Current_Node (Service, Devices (Selected));
      end Check_Device;
      package Current_Device is new With_Metadata_Continuation (Check_Device);
      procedure Assemble (Service : C.State; Done : out Boolean) is
      begin
         Provenance.Build (Service, Item.Observations (Selected),
           Admission_Device, Admission, Facts, History, Built, Assembly_Status);
         Assembly_Attempted := True;
         Done := Assembly_Status = Provenance.Prepared;
      end Assemble;
      package Assembly is new With_Metadata_Continuation (Assemble);
      procedure Check_All (Service : C.State; Done : out Boolean) is
      begin
         Done := C.Retained_Results (Service) = 0;
         for I in 1 .. Count loop
            if Item.Included (I) then
               Done := Done and then Resolver.Current (Service, Item.Observations (I))
                 and then C.Current_Node (Service, Admission_Device);
            end if;
         end loop;
      end Check_All;
      package Final_Check is new With_Metadata_Continuation (Check_All);
      procedure Finish (Status : Outcome) is
      begin
         Item.View.Status := Status;
         Item.View.Working := False;
         if Status /= Prepared then
            Item.View.Count := 0;
            Item.View.Values := [others => Builder.Wire.Empty_Request];
         end if;
         Result := Item.View;
      end Finish;
      procedure Charge (Amount : Natural; Accepted : out Boolean) is
      begin
         Accepted := Amount <= Budget - Item.View.Charged;
         if Accepted then Item.View.Charged := Item.View.Charged + Amount;
         else Item.View.Recovery_Needed := True;
         end if;
      end Charge;
      procedure Capture_Error is
         E : Error_Observation :=
           (Kind => Current_Error, Input_Index => Selected, Device => Devices (Selected),
            Charged => Item.View.Charged - Candidate_Started,
            Policy => Item.View.Last_Policy, Firmware => Info.Firmware,
            Source_Status => Info.Sources.Status, Topology => Item.Last_Topology,
            Description_Status => Built.Status,
            Provenance_Status => (if Assembly_Attempted then Assembly_Status else Provenance.Stale_Observation),
            others => <>);
         Accepted : Boolean;
         procedure Copy_Path (Service : C.State; Done : out Boolean) is
            Cursor : C.Namespace.Node_ID := C.Values.Position (E.Device);
            Node : C.Node_Descriptor;
            Access_Result : C.Values.Access_Status;
            At_Character : Natural := E.Path'Last;
         begin
            Done := True;
            if not C.Current_Node (Service, E.Device) then return; end if;
            for Depth in 1 .. Maximum_Path_Segments loop
               if Cursor = C.Namespace.Root then
                  E.Path (At_Character) := '\'; E.Path_Complete := True; return;
               end if;
               C.Describe_Node (Service, Cursor, Node, Access_Result);
               if Access_Result /= C.Values.Available then return; end if;
               E.Path (At_Character - Segment_Characters + 1 .. At_Character) := C.Values.Node_Name (Node);
               At_Character := At_Character - Segment_Characters;
               if C.Values.Parent_Position (Node) = C.Namespace.Root then
                  E.Path (At_Character) := '\'; E.Path_Complete := True; return;
               end if;
               E.Path (At_Character) := '.'; At_Character := At_Character - 1;
               Cursor := C.Values.Parent_Position (Node);
            end loop;
         end Copy_Path;
         package Metadata is new With_Metadata_Continuation (Copy_Path);
      begin
         if Item.View.Captured_Errors = Maximum_Captured_Errors then
            Item.View.Omitted_Errors := Item.View.Omitted_Errors + 1; return;
         end if;
         -- Observation only, after the candidate has released its retained
         -- results. No AML, IPC, allocator or logging is called here.
         Metadata.Advance (Server, Accepted);
         if not Accepted then E.Path_Complete := False; end if;
         Item.View.Captured_Errors := Item.View.Captured_Errors + 1;
         Item.View.Errors (Item.View.Captured_Errors) := E;
      end Capture_Error;
      procedure Collect_One (I : Candidate_Index; Disposition : out Candidate_Result) is
      begin
         Disposition := Batch_Stopped;
         Selected := I;
         Candidate_Started := Item.View.Charged;
         Current_Error := Policy_Error;
         Assembly_Attempted := False;
         Built := (others => <>);
         Info := (others => <>);
         Item.Last_Topology := (others => <>);
         Current_Device.Advance (Server, Completed);
         if not Completed then Finish (Stale_Device); return; end if;
         Item.View.Recovery_Needed := True;
         Check (Server, Devices (I), Budget - Item.View.Charged, Item.View.Last_Policy);
         Charge (Item.View.Last_Policy.Charged, Charged);
         if not Charged or else Item.View.Last_Policy.Outcome = HID.Controller.Recovery_Required then
            Finish (Recovery_Required); return;
         end if;
         if Retained_Results_Pending (Server) then Finish (Recovery_Required); return; end if;
         Item.View.Recovery_Needed := False;
         case Item.View.Last_Policy.Outcome is
            when HID.Controller.Eligible => null;
            when HID.Controller.Excluded => Disposition := Candidate_Unavailable; return;
            when HID.Controller.Node_Error => Disposition := Candidate_Error; return;
            when HID.Controller.Policy_Incomplete => Finish (Policy_Unresolved); return;
            when others => Finish (Probe_Refused); return;
         end case;
         Item.View.Recovery_Needed := True;
         Current_Error := Firmware_Error;
         HID.Begin_Probe (Item.Probe, Server, C.Values.Position (Devices (I)),
           Budget - Item.View.Charged, Allowed, Action);
         Info := HID.Controller.Observe (Item.Probe);
         if Recovery (Info) then
            Charge (Info.Firmware.Charged, Charged);
            Finish (Recovery_Required); return;
         end if;
         if Allowed /= HID.Controller.Admitted or else Action /= Discovery.Accepted then
            Item.View.Recovery_Needed := HID.Controller.Pending (Item.Probe);
            Finish (if Item.View.Recovery_Needed then Recovery_Required else Probe_Refused); return;
         end if;
         -- Status may change after the path gate. Stop as soon as the fresh
         -- probe itself reports an unavailable leaf; never copy its retained
         -- template or invoke DSM after that observation.
         if Info.Firmware.Status.Outcome = Discovery.Queries.Status_Available
           and then (not Info.Firmware.Status.Present or else not Info.Firmware.Status.Enabled) then
            Item.View.Status_Known := True;
            Item.View.Status_Flags := Info.Firmware.Status.Flags;
            if HID.Controller.Pending (Item.Probe) then HID.Cancel (Item.Probe, Server, Action); end if;
            Info := HID.Controller.Observe (Item.Probe);
            Charge (Info.Firmware.Charged, Charged);
            Item.View.Recovery_Needed := not Charged or else HID.Controller.Pending (Item.Probe)
              or else Recovery (Info);
            if Item.View.Recovery_Needed or else Retained_Results_Pending (Server) then
               Item.View.Recovery_Needed := True; Finish (Recovery_Required);
            else Disposition := Candidate_Unavailable;
            end if;
            return;
         end if;
         if HID.Controller.Pending (Item.Probe) then
            HID.Copy_And_Resolve (Item.Probe, Server, Data, Allowed, Action);
         end if;
         Info := HID.Controller.Observe (Item.Probe);
         Charge (Info.Firmware.Charged, Charged);
         if not Charged or else Recovery (Info) then Finish (Recovery_Required); return; end if;
         if HID.Controller.Pending (Item.Probe) then
            HID.Cancel (Item.Probe, Server, Action);
            Info := HID.Controller.Observe (Item.Probe);
            Item.View.Recovery_Needed := HID.Controller.Pending (Item.Probe) or else Recovery (Info);
            Finish (if Item.View.Recovery_Needed then Recovery_Required else Capacity_Refused); return;
         end if;
         if Retained_Results_Pending (Server) then Finish (Recovery_Required); return; end if;
         Item.View.Recovery_Needed := False;
         if Info.Firmware.Result = Discovery.Budget_Exhausted then Finish (Budget_Exhausted); return; end if;
         if Info.Firmware.Result /= Discovery.Observed then
            if Discovery.Ordinary_Error (Info.Firmware.Result) then Disposition := Candidate_Error;
            else Finish (Probe_Failed);
            end if;
            return;
         end if;
         Item.View.Status_Known := Info.Firmware.Status.Outcome = Discovery.Queries.Status_Available;
         Item.View.Status_Flags := Info.Firmware.Status.Flags;
         -- Discovery is descriptive, including absent/disabled devices. A
         -- resource-decoding child requires BOTH present and enabled (_STA
         -- bits 0 and 1, ACPI 6.6 6.3.7). UI/diagnostic bits remain facts;
         -- they do not replace presence or resource enablement. No power-on
         -- policy, hardware readiness or atomic freshness is inferred here.
         if not Item.View.Status_Known or else not Info.Firmware.Status.Present
           or else not Info.Firmware.Status.Enabled then
            Disposition := Candidate_Unavailable; return;
         end if;
         if not Info.Sources_Attempted then Finish (Sources_Failed); return; end if;
         Current_Error := Source_Error;
         if Info.Sources.Status /= Resolver.Resolved then
            if Resolver.Ordinary_Error (Info.Sources) then Disposition := Candidate_Error;
            else Finish (Sources_Failed);
            end if;
            return;
         end if;
         if not HID.Current (Item.Probe, Server) then Finish (Sources_Failed); return; end if;
         Item.Observations (I) := Info.Sources.Value;
         Current_Error := Topology_Error;
         Item.View.Recovery_Needed := True;
         PCI.Observe_Controller (Server, Resolver.I2C_Controller (Info.Sources.Value),
           Budget - Item.View.Charged, PCI_Allowed, Item.Last_Topology);
         Charge (Item.Last_Topology.Charged, Charged);
         if not Charged or else Item.Last_Topology.Recovery_Needed then Finish (Recovery_Required); return; end if;
         Item.View.Recovery_Needed := False;
         if PCI_Allowed /= PCI.Controller.Admitted then Finish (Topology_Refused); return; end if;
         if Item.Last_Topology.Status = Reader.Topology_Budget_Exhausted then Finish (Budget_Exhausted); return; end if;
         if Item.Last_Topology.Status /= Reader.Topology_Observed then
            if Item.Last_Topology.Status in Reader.Topology_Incomplete | Reader.Parent_Unresolved |
              Reader.Root_Unresolved | Reader.Topology_Observation_Failed then Disposition := Candidate_Error;
            else Finish (Topology_Unresolved);
            end if;
            return;
         end if;
         Facts := (Has_SEG => Item.Last_Topology.Members (Reader.Segment).Status = Reader.Observed
             or else Item.Last_Topology.Segment_Defaulted, Has_BBN => True, Has_ADR => True,
           SEG => (if Item.Last_Topology.Segment_Defaulted then Builder.Wire.Word (Reader.Default_PCI_Segment)
             else Builder.Wire.Word (Item.Last_Topology.Members (Reader.Segment).Raw)),
           BBN => Builder.Wire.Word (Item.Last_Topology.Members (Reader.Base_Bus).Raw),
           ADR => Builder.Wire.Word (Item.Last_Topology.Members (Reader.Address).Raw));
         Current_Error := Description_Error;
         Assembly.Advance (Server, Completed);
         if not Completed then
            -- A known unsupported resource description is a device result.
            -- Admission, stale binding and all other provenance failures
            -- remain batch refusals; a rejected callback has no result.
            if Assembly_Attempted and then Assembly_Status = Provenance.Builder_Refused
              and then Built.Status = Builder.Unsupported_Candidate then
               Disposition := Candidate_Error;
            else Finish (Description_Refused);
            end if;
            return;
         end if;
         if Item.View.Collected = Candidate_Count'Last then Finish (Capacity_Refused); return; end if;
         Item.View.Values (Item.View.Collected + 1) := Built.Value;
         Item.View.Collected := Item.View.Collected + 1;
         Item.Included (I) := True;
         Disposition := Candidate_Ready;
      end Collect_One;
   begin
      if Item.View.Attempted then Result := Item.View; return; end if;
      Item.View.Attempted := True;
      Item.View.Working := True;
      Item.View.Status := Collecting;
      if Retained_Results_Pending (Server) then Finish (Owner_Busy); return; end if;
      -- Reject duplicates before executing even the first candidate.
      for I in 1 .. Count loop
         for J in 1 .. I - 1 loop
            if Devices (I) = Devices (J) then Finish (Duplicate_Device); return; end if;
         end loop;
      end loop;
      for I in 1 .. Count loop
         declare Disposition : Candidate_Result;
         begin
            Collect_One (I, Disposition);
            case Disposition is
               when Batch_Stopped => return;
               when Candidate_Ready => null;
               when Candidate_Unavailable => Item.View.Unavailable := Item.View.Unavailable + 1;
               when Candidate_Error =>
                  Item.View.Node_Errors := Item.View.Node_Errors + 1;
                  Capture_Error;
            end case;
         end;
      end loop;
      Final_Check.Advance (Server, Completed);
      if not Completed then Finish (Stale_Device); return; end if;
      Item.View.Count := Item.View.Collected;
      if Count /= 0 and then Item.View.Count = 0 and then Item.View.Node_Errors = 0 then
         Finish (Device_Unavailable);
      else Finish (Prepared);
      end if;
   end Collect_With_Policy;
   procedure Descriptive_Candidate
     (Service : aliased in out C.State; Device : C.Node_Descriptor;
      Budget : Natural; Result : out HID.Controller.Policy_Report) is
      pragma Unreferenced (Service, Device, Budget);
   begin Result := (HID.Controller.Eligible, 0); end Descriptive_Candidate;
   procedure Descriptive_Collect is new Collect_With_Policy (Descriptive_Candidate);
   procedure Collect (Item : in out Run; Server : in out State;
      Devices : Device_Array; Count : Candidate_Count;
      Admission_Device : C.Node_Descriptor;
      Admission : GPIO_Bootstrap_Payload.Request; History : Provenance.Stamp;
      Budget : Natural; Resource_Bytes : Positive; Result : out Report) is
   begin Descriptive_Collect (Item, Server, Devices, Count, Admission_Device,
      Admission, History, Budget, Resource_Bytes, Result); end Collect;
end ACPI_Requests.HID_Offers;
