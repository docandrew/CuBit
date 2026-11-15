pragma Ada_2022;
with ACPI_Native_Log;
with Firmware_Tables;
with AML_Execute;
package body ACPI_Native_HID_Detection is
   function Name (Status : Outcome) return String is
     (case Status is
        when Not_Attempted => "NOT_ATTEMPTED", when Not_Ready => "NOT_READY",
        when No_Devices => "NO_DEVICES", when Finished => "FINISHED",
        when Budget_Exhausted => "BUDGET_EXHAUSTED", when Policy_Unresolved => "POLICY_UNRESOLVED",
        when Search_Failed => "SEARCH_FAILED", when Discovery_Failed => "DISCOVERY_FAILED",
        when Sources_Failed => "SOURCES_FAILED", when Capacity_Refused => "CAPACITY_REFUSED",
        when Candidate_Limit => "CANDIDATE_LIMIT", when Cleanup_Failed => "CLEANUP_FAILED",
        when Owner_Error => "OWNER_ERROR", when Recovery_Required => "RECOVERY_REQUIRED",
        when Channel_Refused => "CHANNEL_REFUSED");
   function Step_Name (Step : Progress_Step) return String is
     (case Step is
        when Not_Started => "NOT_STARTED", when Search_Candidates => "SEARCH_CANDIDATES",
        when Begin_Device_Probe => "BEGIN_DEVICE_PROBE", when Release_Inactive_Probe => "RELEASE_INACTIVE_PROBE",
        when Resolve_Resources => "RESOLVE_RESOURCES", when Cancel_Probe => "CANCEL_PROBE",
        when Validate_Candidate => "VALIDATE_CANDIDATE", when Prepare_Channels => "PREPARE_CHANNELS",
        when Record_Observation => "RECORD_OBSERVATION");
   function Error_Name (Kind : Error_Kind) return String is
     (case Kind is
        when Policy_Error => "POLICY_ERROR",
        when Identity_Error => "IDENTITY_ERROR",
        when Firmware_Error => "FIRMWARE_ERROR",
        when Source_Error => "SOURCE_ERROR",
        when Channel_Error => "CHANNEL_ERROR");
   pragma Unevaluated_Use_Of_Old (Allow);
   package Core renames ACPI_Service;
   use type Owner.Controller.Admission, Owner.Controller.Search_Outcome, Owner.Controller.Policy_Outcome;
   use type Discovery.Action_Result, Discovery.Outcome, Resolver.Outcome;
   use type Discovery.Queries.Status_Outcome;
   use type Channels.Outcome;
   use type Owner.Controller.Search_Error_Kind, Core.Values.Access_Status;
   function Uncertain (Execution : AML_Execute.Execution_Status) return Boolean is
     (Execution in AML_Execute.Fatal_Requested | AML_Execute.Field_Indeterminate);
   function Search_Recovery (Item : Owner.Controller.Search_Report) return Boolean is
     (Item.Last_Policy.Outcome = Owner.Controller.Recovery_Required or else Item.Last_Identity.Outcome in Discovery.Queries.Cleanup_Failed |
        Discovery.Queries.Provider_Contract_Error or else Uncertain (Item.Last_Identity.Execution));
   function Probe_Recovery (Item : Owner.Controller.Report) return Boolean is
     (Item.Firmware.Result in Discovery.Cleanup_Failed | Discovery.Provider_Contract_Error
      or else Item.Firmware.Candidate.Outcome in Discovery.Queries.Cleanup_Failed |
        Discovery.Queries.Provider_Contract_Error
      or else Item.Firmware.Status.Outcome in Discovery.Queries.Cleanup_Failed |
        Discovery.Queries.Provider_Contract_Error
      or else Item.Firmware.Descriptor.Outcome in Discovery.Queries.Cleanup_Failed |
        Discovery.Queries.Provider_Contract_Error
      or else Uncertain (Item.Firmware.Execution)
      or else Uncertain (Item.Firmware.Candidate.Execution)
      or else Uncertain (Item.Firmware.Status.Execution)
      or else Uncertain (Item.Firmware.Descriptor.Execution));
   Attempted : Boolean := False;
   Saved : Report;
   Probe : Owner.Controller.Probe;
   procedure Run_With_Policy (Server : in out ACPI_Requests.State; Result : out Report;
      Limits : Policy := (others => <>)) is
      procedure Find is new Owner.Find_With_Policy (Check_Candidate);
      Cursor, Device : Core.Namespace.Node_ID := Core.Namespace.Root;
      Search : Owner.Controller.Search_Report;
      Allowed : Owner.Controller.Admission;
      Action : Discovery.Action_Result;
      Info : Owner.Controller.Report;
      Data : Firmware_Tables.Bytes (1 .. Limits.Resource_Bytes);
      procedure Capture_Error (Item : Error_Observation; Position : Core.Namespace.Node_ID := Core.Namespace.Root) is
         Copied : Error_Observation := Item;
         Accepted : Boolean;
         procedure Copy_Metadata (Service : Core.State; Completed : out Boolean) is
            Access_Result : Core.Values.Access_Status;
         begin
            if Position /= Core.Namespace.Root then
               Core.Describe_Node (Service, Position, Copied.Device, Access_Result);
               if Access_Result /= Core.Values.Available then Completed := False; return; end if;
            end if;
            if Core.Current_Node (Service, Copied.Device) then
               ACPI_Native_Initializer_History.Copy_Path
                 (Service, Core.Values.Position (Copied.Device), Copied.Path, Copied.Path_Complete);
            end if;
            Completed := True;
         end Copy_Metadata;
         package Metadata is new ACPI_Requests.With_Metadata_Continuation (Copy_Metadata);
      begin
         if Saved.Captured_Errors = Maximum_Captured_Errors then return; end if;
         -- Metadata-only capture after the owner call has released its Core.
         -- Rejected/currentness failures leave an explicitly incomplete path.
         Metadata.Advance (Server, Accepted);
         if not Accepted then Copied.Path_Complete := False; end if;
         Saved.Captured_Errors := Saved.Captured_Errors + 1;
         Saved.Errors (Saved.Captured_Errors) := Copied;
      end Capture_Error;
      function Path_Image (Text : String) return String is
      begin
         for I in Text'Range loop
            if Text (I) /= ' ' then return Text (I .. Text'Last); end if;
         end loop;
         return "UNKNOWN";
      end Path_Image;
      procedure Finish (Status : Outcome) is
      begin
         Saved.Status := Status;
         Saved.Cleanup_Required := Owner.Controller.Pending (Probe)
           or else ACPI_Requests.Retained_Results_Pending (Server);
         Saved.Recovery_Needed := Saved.Recovery_Needed or Saved.Cleanup_Required;
         -- Always publish one compact outcome before optional detailed rows.
         -- The logstore refuses records exceeding its 512-byte text limit.
         if Status in No_Devices | Finished then
            ACPI_Native_Log.Info ("acpi: stage hid-discovery-result ok " & Name (Status) &
              " step=" & Step_Name (Saved.Last_Step) & " fuel=" & Saved.Charged'Image);
         else
            ACPI_Native_Log.Warning ("acpi: stage hid-discovery-result fail " & Name (Status) &
              " step=" & Step_Name (Saved.Last_Step) &
              " recovery=" & Boolean'Image (Saved.Recovery_Needed) & " fuel=" & Saved.Charged'Image);
         end if;
         -- Emit only detached observations at the existing finish/log boundary.
         for I in Error_Index loop
            exit when I > Saved.Captured_Errors;
            declare E : constant Error_Observation := Saved.Errors (I);
            begin
               ACPI_Native_Log.Warning ("acpi: stage hid-node fail " & Error_Name (E.Kind) &
                 " node=" & Core.Values.Position (E.Device)'Image & " path=" & Path_Image (E.Path) &
                 " path-complete=" & Boolean'Image (E.Path_Complete) &
                 " policy=" & E.Search.Policy.Outcome'Image &
                 " identity=" & E.Search.Identity.Outcome'Image &
                 " identity-exec=" & E.Search.Identity.Execution'Image &
                 " query=" & E.Firmware.Result'Image & " resource-exec=" & E.Firmware.Execution'Image &
                 " status-exec=" & E.Firmware.Status.Execution'Image &
                 " descriptor-exec=" & E.Firmware.Descriptor.Execution'Image &
                 " source=" & E.Source_Status'Image & " channel=" & E.Channel_Status'Image &
                 " fuel=" & Natural'Image (if E.Kind in Policy_Error | Identity_Error then
                   E.Search.Policy.Charged + E.Search.Identity.Charged else E.Firmware.Charged));
            end;
         end loop;
         if Saved.Node_Errors /= 0 then
            ACPI_Native_Log.Warning ("acpi: stage hid-node-summary errors=" & Saved.Node_Errors'Image &
              " captured=" & Saved.Captured_Errors'Image &
              " omitted=" & Natural'Image (Saved.Node_Errors - Natural (Saved.Captured_Errors)));
         end if;
         if Status in No_Devices | Finished then
            ACPI_Native_Log.Info ("acpi: stage hid-discovery ok " & Status'Image &
              " candidates=" & Saved.Candidates'Image & " observed=" & Saved.Observed'Image &
              " inactive=" & Saved.Unavailable'Image & " policy-excluded=" & Saved.Policy_Excluded'Image &
              " node-errors=" & Saved.Node_Errors'Image & " failed-candidates=" & Saved.Failed_Candidates'Image &
              " fuel=" & Saved.Charged'Image);
         else
            ACPI_Native_Log.Warning ("acpi: stage hid-discovery fail " & Status'Image &
              " search=" & Search.Outcome'Image & " policy=" & Search.Last_Policy.Outcome'Image & " query=" & Info.Firmware.Result'Image &
              " source=" & Info.Sources.Status'Image &
              " channel=" & (if Saved.Channel_Attempted then Saved.Last_Channel_Status'Image else "NOT_ATTEMPTED") &
              " identity-exec=" & Search.Last_Identity.Execution'Image &
              " resource-exec=" & Info.Firmware.Execution'Image &
              " status-exec=" & Info.Firmware.Status.Execution'Image &
              " descriptor-exec=" & Info.Firmware.Descriptor.Execution'Image &
              " recovery=" & Boolean'Image (Saved.Recovery_Needed) &
              " required=" & Info.Firmware.Copy.Required'Image &
              " candidates=" & Saved.Candidates'Image & " observed=" & Saved.Observed'Image &
              " fuel=" & Saved.Charged'Image);
         end if;
         Result := Saved;
      end Finish;
   begin
      if Attempted then Result := Saved; return; end if;
      Attempted := True;
      Saved.Status := Owner_Error;
      loop
         Saved.Recovery_Needed := True;
         if Owner.Controller.Pending (Probe) then Finish (Recovery_Required); return; end if;
         Saved.Last_Step := Search_Candidates;
         Find (Server, Cursor, Limits.Fuel - Saved.Charged, Device, Search);
         Allowed := (if Search.Outcome = Owner.Controller.Not_Ready then Owner.Controller.Not_Ready
           else Owner.Controller.Admitted);
         Action := Discovery.Not_Pending;
         Saved.Charged := Saved.Charged + Search.Charged;
         if Search.Excluded_Count > Unavailable_Count'Last - Saved.Policy_Excluded then
            Finish (Owner_Error); return;
         end if;
         Saved.Policy_Excluded := Saved.Policy_Excluded + Search.Excluded_Count;
         if Search.Node_Errors > Unavailable_Count'Last - Saved.Node_Errors then Finish (Owner_Error); return; end if;
         Saved.Node_Errors := Saved.Node_Errors + Search.Node_Errors;
         for I in Owner.Controller.Search_Error_Index loop
            exit when I > Search.Captured_Errors;
            Capture_Error ((Kind => (if Search.Errors (I).Kind = Owner.Controller.Policy_Error then
              Policy_Error else Identity_Error), Device => Search.Errors (I).Device,
              Search => Search.Errors (I), others => <>));
         end loop;
         Saved.Recovery_Needed := Search_Recovery (Search);
         if Saved.Recovery_Needed then Finish (Recovery_Required); return; end if;
         if Allowed = Owner.Controller.Not_Ready then Finish (Not_Ready); return; end if;
         if Search.Outcome = Owner.Controller.No_Match then
            Finish (if Saved.Candidates = 0 then No_Devices else Finished); return;
         end if;
         if Search.Outcome = Owner.Controller.Incomplete then
            Finish (if Search.Last_Policy.Outcome = Owner.Controller.Policy_Incomplete
              then Policy_Unresolved else Budget_Exhausted); return;
         end if;
         if Search.Outcome /= Owner.Controller.Found then Finish (Search_Failed); return; end if;
         if Search.Outcome = Owner.Controller.Found then
            Saved.Recovery_Needed := True;
            Saved.Last_Step := Begin_Device_Probe;
            Owner.Begin_Probe (Probe, Server, Device,
              Limits.Fuel - Saved.Charged, Allowed, Action);
         end if;
         if Action /= Discovery.Accepted then Finish (Owner_Error); return; end if;
         Saved.Candidates := Saved.Candidates + 1;
         Info := Owner.Controller.Observe (Probe);
         Saved.Recovery_Needed := Probe_Recovery (Info);
         if Saved.Recovery_Needed then
            Saved.Charged := Saved.Charged + Info.Firmware.Charged;
            Finish (Recovery_Required); return;
         end if;
         if Info.Firmware.Status.Outcome = Discovery.Queries.Status_Available
           and then (not Info.Firmware.Status.Present or else not Info.Firmware.Status.Enabled) then
            -- The library describes inactive devices too. They cannot become
            -- startup candidates; release any retained CRS without copying or
            -- querying DSM, then continue past the alternative definition.
            if Owner.Controller.Pending (Probe) then
               Saved.Recovery_Needed := True;
               Saved.Last_Step := Release_Inactive_Probe;
               Owner.Cancel (Probe, Server, Action);
            end if;
            Info := Owner.Controller.Observe (Probe);
            Saved.Charged := Saved.Charged + Info.Firmware.Charged;
            Saved.Recovery_Needed := Probe_Recovery (Info) or else Owner.Controller.Pending (Probe);
            if Saved.Recovery_Needed then Finish (Recovery_Required); return; end if;
            Saved.Candidates := Saved.Candidates - 1;
            Saved.Unavailable := Saved.Unavailable + 1;
            Cursor := Device;
         else
         if Owner.Controller.Pending (Probe) then
            Saved.Recovery_Needed := True;
            Saved.Last_Step := Resolve_Resources;
            Owner.Copy_And_Resolve (Probe, Server, Data, Allowed, Action);
         end if;
         Info := Owner.Controller.Observe (Probe);
         Saved.Charged := Saved.Charged + Info.Firmware.Charged;
         Saved.Recovery_Needed := Probe_Recovery (Info);
         if not Owner.Controller.Pending (Probe) and then ACPI_Requests.Retained_Results_Pending (Server) then
            Saved.Recovery_Needed := True;
         end if;
         if Saved.Recovery_Needed then Finish (Recovery_Required); return; end if;
         if Owner.Controller.Pending (Probe) then
            Saved.Recovery_Needed := True;
            Saved.Last_Step := Cancel_Probe;
            Owner.Cancel (Probe, Server, Action);
            Info := Owner.Controller.Observe (Probe);
            Saved.Recovery_Needed := Probe_Recovery (Info);
            if Owner.Controller.Pending (Probe) then Finish (Cleanup_Failed); return; end if;
            if Saved.Recovery_Needed then Finish (Recovery_Required); return; end if;
            -- No sizing retry executes AML. The configured byte budget is final.
            Finish (Capacity_Refused); return;
         end if;
         if Info.Firmware.Result = Discovery.Budget_Exhausted then Finish (Budget_Exhausted); return; end if;
         Saved.Last_Step := Validate_Candidate;
         declare Candidate_Ready : Boolean := False;
         begin
            if Info.Firmware.Result /= Discovery.Observed then
               if not Discovery.Ordinary_Error (Info.Firmware.Result) then Finish (Discovery_Failed); return; end if;
            elsif not Info.Sources_Attempted then Finish (Sources_Failed); return;
            elsif Info.Sources.Status /= Resolver.Resolved then
               if not Resolver.Ordinary_Error (Info.Sources) then Finish (Sources_Failed); return; end if;
            elsif not Owner.Current (Probe, Server) then Finish (Sources_Failed); return;
            else
               Saved.Last_Step := Prepare_Channels;
               declare
                  Facts : constant Candidates.Candidate := Resolver.Facts (Info.Sources.Value);
                  Prepared : constant Channels.Preparation := Channels.Prepare (Facts);
               begin
                  Saved.Channel_Attempted := True;
                  Saved.Last_Channel_Status := Prepared.Status;
                  Candidate_Ready := Prepared.Status = Channels.Prepared;
                  if Candidate_Ready then
                     Saved.Last_Step := Record_Observation;
                     ACPI_Native_Log.Info ("acpi: stage hid-resource ok OBSERVED" &
                       " device=" & Device'Image &
                       " i2c=" & Core.Values.Position (Resolver.I2C_Controller (Info.Sources.Value))'Image &
                       " gpio=" & Core.Values.Position (Resolver.GPIO_Controller (Info.Sources.Value))'Image &
                       " address=" & Facts.I2C.Address'Image &
                       " descriptor=" & Facts.Descriptor_Register'Image & " pin=" & Facts.Pin'Image);
                     Saved.Items (Saved.Observed + 1) :=
                       (Device => Resolver.Device (Info.Sources.Value),
                        I2C_Controller => Resolver.I2C_Controller (Info.Sources.Value),
                        GPIO_Controller => Resolver.GPIO_Controller (Info.Sources.Value),
                        Channel_Data => Prepared);
                     Saved.Observed := Saved.Observed + 1;
                  end if;
               end;
            end if;
            if not Candidate_Ready then
               if Saved.Failed_Candidates = Candidate_Count'Last or else Saved.Node_Errors = Unavailable_Count'Last then
                  Finish (Owner_Error); return;
               end if;
               Saved.Failed_Candidates := Saved.Failed_Candidates + 1;
               Saved.Node_Errors := Saved.Node_Errors + 1;
               Capture_Error ((Kind => (if Info.Firmware.Result /= Discovery.Observed then Firmware_Error
                 elsif Info.Sources.Status /= Resolver.Resolved then Source_Error else Channel_Error),
                 Firmware => Info.Firmware, Source_Status => Info.Sources.Status,
                 Channel_Status => Saved.Last_Channel_Status, others => <>), Device);
            end if;
         end;
         if Saved.Candidates = Limits.Candidates then Finish (Candidate_Limit); return; end if;
         Cursor := Device;
         end if;
      end loop;
   exception
      when others =>
         -- No cleanup AML or retry. Native startup has already opened its
         -- normal log publisher before any discovery owner call.
         Saved.Status := Owner_Error;
         Saved.Exceptional_Exit := True;
         Saved.Recovery_Needed := True;
         Saved.Cleanup_Required := Owner.Controller.Pending (Probe);
         if ACPI_Requests.Valid (Server) then
            Saved.Cleanup_Required := Saved.Cleanup_Required
              or else ACPI_Requests.Retained_Results_Pending (Server);
         else Saved.Cleanup_Required := True;
         end if;
         ACPI_Native_Log.Warning ("acpi: stage hid-discovery-result fail OWNER_EXCEPTION" &
           " step=" & Step_Name (Saved.Last_Step) & " cursor=" & Cursor'Image &
           " device=" & Device'Image & " candidates=" & Saved.Candidates'Image &
           " observed=" & Saved.Observed'Image & " fuel=" & Saved.Charged'Image);
         Result := Saved;
   end Run_With_Policy;
   procedure Descriptive_Candidate
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Owner.Controller.Policy_Report) is
      pragma Unreferenced (Service, Item, Budget);
   begin Result := (Owner.Controller.Eligible, 0); end Descriptive_Candidate;
   procedure Descriptive_Run is new Run_With_Policy (Descriptive_Candidate);
   procedure Run (Server : in out ACPI_Requests.State; Result : out Report;
      Limits : Policy := (others => <>)) is
   begin Descriptive_Run (Server, Result, Limits); end Run;
end ACPI_Native_HID_Detection;
