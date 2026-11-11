with ACPI_Catalog_Header_Notes;
with ACPI_Native_Startup_Timing;
with ACPI_Native_AML_Profile;
with ACPI_Native_Firmware_Profile;
with ACPI_Native_Lock_Dump;
with Interfaces; use Interfaces;
with AML_Decode;
with ACPI_Field_Diagnostics;
with ACPI_Clock_Source;
with ACPI_Bootstrap;
with ACPI_Native_Delays;
with ACPI_Fatal_Diagnostics;
with ACPI_Snapshot_Startup;
with ACPI_Native_Snapshot_Load;
with ACPI_Native_Server;
with ACPI_Native_HID_Detection;
with ACPI_Native_Power_Detection;
with ACPI_Native_Log;
with ACPI_Native_Module_Notes;
with ACPI_Requests;
with ACPI_Startup_Diagnostics;
with ACPI_Allocation_Diagnostics;
with ACPI_Native_EC_Boot_Notes;
with CuBit.Messages;
with CuBit.Firmware_Snapshot;
with Startup;
with ACPI_Service;
with ACPI_Device_Initialization;
with ACPI_Device_Policy;
with ACPI_Unprovisioned_Regions;
with AML_Execute;
with ACPI_Native_Evaluation;
with ACPI_Native_Fields;
with ACPI_Native_PCI_Aliases;
with ACPI_Native_Data;
with ACPI_Capacity_Diagnostics;
with AML_Field_Access;
with ACPI_Native_Snapshot_Load.With_Driver;
with CuBit.Handles.Startup;
with CuBit.Monotonic;
package body ACPI_Native_Instance is
   package L renames ACPI_Snapshot_Startup;
   use type L.Outcome;
   use type ACPI_Native_Server.Stop_Reason;
   use type L.State_Access;
   use type L.R.Tick;
   Maximum_Diagnostic_Records : constant Positive := 8;
   -- Failure-only passive snapshot. No cleanup, retry, clock or authority.
   procedure Report_Firmware_Session is
      Ready, Pending : Boolean := False;
      Observation : AML_Field_Access.Observation := AML_Field_Access.No_Observation;
      Capacity : ACPI_Capacity_Diagnostics.Report := ACPI_Capacity_Diagnostics.No_Report;
      function Bit (Value : Boolean) return String is (if Value then "1" else "0");
   begin
      ACPI_Native_Data.Session.Inspect (Ready, Pending, Observation, Capacity);
      ACPI_Native_Log.Warning
        ("acpi: stage firmware-session fail SNAPSHOT ready=" & Bit (Ready) &
         " pending=" & Bit (Pending) & " uncertain=" & Bit (Observation.Present) &
         " capacity=" & Bit (ACPI_Capacity_Diagnostics.Present (Capacity)));
      ACPI_Native_Log.Warning ("acpi: stage firmware-state fail " & ACPI_Native_Data.Session.Diagnostic_Summary);
      for I in 1 .. Maximum_Diagnostic_Records loop
         declare Line : constant String := ACPI_Native_Data.Session.Diagnostic_Line (I); begin
            if Line'Length /= 0 then ACPI_Native_Log.Warning ("acpi: stage firmware-client fail " & Line); end if;
         end;
         declare Line : constant String := ACPI_Native_Data.Native.Diagnostic_Line (I); begin
            if Line'Length /= 0 then ACPI_Native_Log.Warning ("acpi: stage firmware-rpc trace " & Line); end if;
         end;
      end loop;
      if ACPI_Capacity_Diagnostics.Present (Capacity) then
         ACPI_Native_Log.Warning
           ("acpi: stage firmware-capacity fail " & ACPI_Capacity_Diagnostics.Format (Capacity));
      end if;
   end Report_Firmware_Session;
   -- Native monotonic samples and snapshot deadlines are microseconds.
   Startup_Timeout_Microseconds : constant L.R.Tick := 60_000_000;
   -- Separate absolute deadlines: snapshot/delays use microseconds; register
   -- IPC uses GETTIME milliseconds. Their raw epochs are never interconverted.
   Startup_Timeout_Milliseconds : constant Unsigned_64 := 60_000;
   -- Discard_Names in the native runtime makes enum Image numeric. Keep
   -- explicit names for the failure stages visible in laptop boot photos.
   function Name (Item : L.Outcome) return String is
     (case Item is
        when L.Loaded => "LOADED",
        when L.Source_Refused => "SOURCE_REFUSED",
        when L.Inventory_Not_Ready => "INVENTORY_NOT_READY",
        when L.Invalid_Inventory => "INVALID_INVENTORY",
        when L.Representation_Limit => "REPRESENTATION_LIMIT",
        when L.Allocation_Refused => "ALLOCATION_REFUSED",
        when L.Invalid_Description => "INVALID_DESCRIPTION",
        when L.Inventory_Mismatch => "INVENTORY_MISMATCH",
        when L.Clock_Unavailable => "CLOCK_UNAVAILABLE",
        when L.Clock_Reversed => "CLOCK_REVERSED",
        when L.Deadline_Expired => "DEADLINE_EXPIRED",
        when L.Copy_Refused => "COPY_REFUSED",
        when L.Admission_Refused => "ADMISSION_REFUSED",
        when L.Startup_Refused => "STARTUP_REFUSED",
        when L.Startup_Already_Consumed => "STARTUP_ALREADY_CONSUMED");
   function Name (Item : ACPI_Requests.Outcome) return String is
     (case Item is
        when ACPI_Requests.OK => "OK",
        when ACPI_Requests.Denied => "DENIED",
        when ACPI_Requests.Malformed => "MALFORMED",
        when ACPI_Requests.Stale => "STALE",
        when ACPI_Requests.Wrong_Order => "WRONG_ORDER",
        when ACPI_Requests.Resource_Limit => "RESOURCE_LIMIT",
        when ACPI_Requests.Table_Rejected => "TABLE_REJECTED",
        when ACPI_Requests.Incomplete => "INCOMPLETE",
        when ACPI_Requests.Not_Found => "NOT_FOUND",
        when ACPI_Requests.Wrong_Table_Kind => "WRONG_TABLE_KIND",
        when ACPI_Requests.Index_Out_Of_Range => "INDEX_OUT_OF_RANGE",
        when ACPI_Requests.Unsupported_Record_Kind => "UNSUPPORTED_RECORD_KIND");
   function Name (Item : L.R.Outcome) return String is
     (case Item is
        when L.R.Copied_Table => "COPIED_TABLE",
        when L.R.Wrong_Capacity => "WRONG_CAPACITY",
        when L.R.Invalid_Descriptor => "INVALID_DESCRIPTOR",
        when L.R.Clock_Unavailable => "CLOCK_UNAVAILABLE",
        when L.R.Clock_Reversed => "CLOCK_REVERSED",
        when L.R.Deadline_Expired => "DEADLINE_EXPIRED",
        when L.R.Source_Refused => "SOURCE_REFUSED",
        when L.R.Short_Copy => "SHORT_COPY",
        when L.R.Excess_Copy => "EXCESS_COPY",
        when L.R.Table_Refused => "TABLE_REFUSED",
        when L.R.Metadata_Mismatch => "METADATA_MISMATCH");
   function Name (Item : CuBit.Firmware_Snapshot.Outcome) return String is
     (case Item is
        when CuBit.Firmware_Snapshot.Ok => "OK",
        when CuBit.Firmware_Snapshot.Not_Ready => "NOT_READY",
        when CuBit.Firmware_Snapshot.Unknown_Table => "UNKNOWN_TABLE",
        when CuBit.Firmware_Snapshot.Beyond_End => "BEYOND_END",
        when CuBit.Firmware_Snapshot.Empty_Before_End => "EMPTY_BEFORE_END",
        when CuBit.Firmware_Snapshot.Copy_Failed => "COPY_FAILED",
        when CuBit.Firmware_Snapshot.Not_Authorized => "NOT_AUTHORIZED",
        when CuBit.Firmware_Snapshot.Malformed => "MALFORMED");
   package Empty_Regions is new ACPI_Unprovisioned_Regions (ACPI_Service);
   procedure Initialize_Regions
     (Service : aliased in out ACPI_Service.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      Accepted : Boolean;
   begin
      Charged := 0; Outcome := ACPI_Device_Policy.Regions_Unavailable;
      ACPI_Native_Evaluation.Prepare_Fields (Service, Accepted);
      if not Accepted then return; end if;
      Empty_Regions.Initialize (Service, Budget, Charged, Outcome);
   end Initialize_Regions;
   package Driver is new ACPI_Device_Initialization
     (ACPI_Service, Initialize_Regions, ACPI_Native_Evaluation.Evaluate_Named);
   function Hex (Value : Unsigned_64) return String;
   Catalog_Reported : Boolean := False;
   procedure Report_Catalog (Service : ACPI_Service.State;
      IO_Pending, IO_Uncertain : Boolean) is
      package H renames ACPI_Catalog_Header_Notes;
      Text : H.Line_Text;
      Last : H.Line_Length;
      Status : H.Format_Status;
      Count : Natural;
      Diagnostic_Batch_Rows : constant Positive := 4;
      Drained : Boolean;
      use type H.Format_Status;
      procedure Drain_Catalog (After_Row : Natural; Accepted : out Boolean) is
      begin
         ACPI_Native_Log.Drain_Diagnostics (IO_Pending, IO_Uncertain, Accepted);
         if not Accepted then
            ACPI_Native_Log.Warning ("acpi: stage catalog-headers fail LOG_NOT_DRAINED after=" & After_Row'Image);
         end if;
      end Drain_Catalog;
   begin
      if Catalog_Reported or else IO_Pending or else IO_Uncertain then return; end if;
      Catalog_Reported := True;
      if not ACPI_Service.Valid (Service) then
         ACPI_Native_Log.Warning ("acpi: stage catalog-headers fail CATALOG_INVALID"); return;
      end if;
      Count := ACPI_Service.Table_Count (Service);
      Drain_Catalog (0, Drained);
      if not Drained then return; end if;
      for Index in 1 .. Count loop
         declare
            Metadata : constant ACPI_Service.Table_Metadata := ACPI_Service.Table_Info (Service, Index);
         begin
            -- Retained catalog entries passed SDT checksum admission. This
            -- reports that existing result; it never rereads table payloads.
            H.Format ((Index => Index, ID => Metadata.ID, Length => Metadata.Extent,
              Revision => Metadata.Revision, Identity => ACPI_Service.Table_Identity (Service, Index),
              Checksum_Admitted => True), Text, Last, Status);
            if Status = H.Formatted then ACPI_Native_Log.Info (Text (1 .. Last));
            else ACPI_Native_Log.Warning ("acpi: stage table-header fail FORMAT_LIMIT index=" & Index'Image);
            end if;
         end;
         if Index mod Diagnostic_Batch_Rows = 0 then
            Drain_Catalog (Index, Drained);
            if not Drained then return; end if;
         end if;
      end loop;
      if Count mod Diagnostic_Batch_Rows /= 0 then
         Drain_Catalog (Count, Drained);
         if not Drained then return; end if;
      end if;
      ACPI_Native_Log.Info ("acpi: stage catalog-headers ok rows=" & Count'Image & " source=ADMITTED_COPY");
      Drain_Catalog (Count, Drained);
   end Report_Catalog;
   procedure Report_Module_Field_Context
     (Result : ACPI_Service.Execution_Report; IO_Pending, IO_Uncertain : Boolean) is
   begin
      -- Copied diagnostics only, after execution stopped. No provider callback,
      -- metadata refresh, register access or logging during unresolved Field IO.
      if IO_Pending or else IO_Uncertain then return; end if;
      declare
         First : constant ACPI_Field_Diagnostics.Report := ACPI_Native_Fields.First_Failure;
      begin
         ACPI_Native_Log.Warning ("acpi: stage module-field-context ok aml-recorded=" &
           Result.Modules.Field_Failure.Present'Image & " provider-recorded=" &
           ACPI_Field_Diagnostics.Present (First)'Image &
           (if Result.Modules.Field_Failure.Present then
              " aml-name=" & Result.Modules.Field_Failure.Name &
              " aml-region=" & Result.Modules.Field_Failure.Region &
              " region-known=" & Result.Modules.Field_Failure.Region_Known'Image &
              " space=" & Result.Modules.Field_Failure.Space'Image &
              " write=" & Result.Modules.Field_Failure.For_Write'Image &
              " region-base=0x" & Hex (Unsigned_64 (Result.Modules.Field_Failure.Region_Address)) &
              " region-bytes=0x" & Hex (Unsigned_64 (Result.Modules.Field_Failure.Region_Length)) &
              " address-literal=" & Result.Modules.Field_Failure.Address_Is_Literal'Image &
              " length-literal=" & Result.Modules.Field_Failure.Length_Is_Literal'Image &
              " failure=" & ACPI_Startup_Diagnostics.Execution_Name (Result.Modules.Field_Failure.Failure) else "") &
           " " & ACPI_Field_Diagnostics.Format (First));
      end;
   end Report_Module_Field_Context;
   procedure Timed_Module_Startup
     (Service : in out ACPI_Service.State; Budget : Natural;
      Result : out ACPI_Service.Execution_Report; Status : out ACPI_Service.Values.Access_Status) is
      package T renames ACPI_Native_Startup_Timing;
      Timing : T.Token;
      Before : constant ACPI_Service.Metrics := ACPI_Service.Observe (Service);
      Module_Before : constant T.P.Counters := T.Snapshot (T.P.Module_Execution);
      Sleep_Before : constant T.P.Counters := T.Snapshot (T.P.AML_Sleep);
      Stall_Before : constant T.P.Counters := T.Snapshot (T.P.AML_Stall);
      GC_Before : constant T.P.Counters := T.Snapshot (T.P.Work_Allocation);
      function Delta_Value (After, Prior : Unsigned_64) return Unsigned_64 is
        (if After >= Prior then After-Prior else 0);
   begin
      ACPI_Native_Log.Info ("acpi: stage module-start ok budget=" & Budget'Image);
      T.Enter (T.P.Module_Execution,Timing);
      ACPI_Native_AML_Profile.Start;
      ACPI_Native_Evaluation.Execute_Module_Startup (Service,Budget,Result,Status);
      ACPI_Native_AML_Profile.Stop;
      T.Leave (Timing);
      ACPI_Native_AML_Profile.Report;
      case Result.Modules.Outcome is
         when ACPI_Service.Values.Modules_Completed => null;
         when others => Report_Module_Field_Context (Result,
           IO_Pending => ACPI_Native_Fields.Observe.IO_Pending,
           IO_Uncertain => ACPI_Native_Fields.Uncertain.Present);
            ACPI_Native_AML_Profile.Report_Tail
           (IO_Pending => ACPI_Native_Fields.Observe.IO_Pending,
            IO_Uncertain => ACPI_Native_Fields.Uncertain.Present);
            Report_Catalog (Service, IO_Pending => ACPI_Native_Fields.Observe.IO_Pending,
              IO_Uncertain => ACPI_Native_Fields.Uncertain.Present);
      end case;
      declare
         After : constant ACPI_Service.Metrics := ACPI_Service.Observe (Service);
         Module_After : constant T.P.Counters := T.Snapshot (T.P.Module_Execution);
         GC_After : constant T.P.Counters := T.Snapshot (T.P.Work_Allocation);
         Wall : constant Unsigned_64 := Delta_Value (Module_After.Total_US,Module_Before.Total_US);
         External_Time : constant Unsigned_64 := Delta_Value (Module_After.External_US,Module_Before.External_US);
      begin
         ACPI_Native_Log.Info ("acpi: stage module-timing ok basis=WALL_ONLY cpu_us=UNAVAILABLE scope=BATCH" &
           " failed-table=" & Result.Modules.Failed_Table'Image & " completed=" & Result.Modules.Completed'Image &
           " charged=" & Result.Modules.Charged'Image & " budget=" & Budget'Image &
           " wall_us=" & Wall'Image & " external_union_us=" & External_Time'Image &
           " sleep_us=" & Delta_Value (T.Snapshot (T.P.AML_Sleep).Total_US,Sleep_Before.Total_US)'Image &
           " stall_us=" & Delta_Value (T.Snapshot (T.P.AML_Stall).Total_US,Stall_Before.Total_US)'Image &
           " unaccounted_wall_us=" & Delta_Value (Wall,External_Time)'Image);
         ACPI_Native_Log.Info ("acpi: stage gc-timing ok attempted=" &
           Delta_Value (Unsigned_64 (After.Collections.Attempted),Unsigned_64 (Before.Collections.Attempted))'Image &
           " completed_mark_sweeps=" & Delta_Value (Unsigned_64 (After.Collections.Completed),Unsigned_64 (Before.Collections.Completed))'Image &
           " rejected=" & Delta_Value (Unsigned_64 (After.Collections.Rejected),Unsigned_64 (Before.Collections.Rejected))'Image &
           " us=" & Delta_Value (GC_After.Total_US,GC_Before.Total_US)'Image & " max_us_total=" & GC_After.Worst_US'Image &
           " freed_objects=" & Delta_Value (Unsigned_64 (After.Collections.Freed_Objects),Unsigned_64 (Before.Collections.Freed_Objects))'Image &
           " freed_bytes=" & Delta_Value (Unsigned_64 (After.Collections.Freed_Bytes),Unsigned_64 (Before.Collections.Freed_Bytes))'Image &
           " freed_elements=" & Delta_Value (Unsigned_64 (After.Collections.Freed_Elements),Unsigned_64 (Before.Collections.Freed_Elements))'Image);
         ACPI_Native_Log.Info ("acpi: stage value-arena ok policy=FIXED growths=0 sample=EXIT_NOT_PEAK objects=" &
           After.Value_Objects'Image & "/" & Service.Value_Object_Capacity'Image &
           " bytes=" & After.Value_Bytes'Image & "/" & Service.Value_Byte_Capacity'Image &
           " elements=" & After.Package_Elements'Image & "/" & Service.Value_Element_Capacity'Image);
      end;
   end Timed_Module_Startup;
   package Loader is new ACPI_Native_Snapshot_Load.With_Driver
     (Driver, Timed_Module_Startup);
   Device_Run : Driver.Session;
   Owner : L.State_Access;
   Started : Boolean := False;
   function Hex (Value : Unsigned_64) return String is
      Hex_Digits : constant String := "0123456789ABCDEF";
      Text : String (1 .. 16);
      Left : Unsigned_64 := Value;
   begin
      for I in reverse Text'Range loop
         Text (I) := Hex_Digits (Natural (Left and 16#F#) + 1);
         Left := Shift_Right (Left, 4);
      end loop;
      return Text;
   end Hex;
   procedure Start is
      Clock : AML_Decode.Integer_Value;
      Available : Boolean;
      Result : ACPI_Native_Snapshot_Load.Report;
      Registration_Result : Unsigned_64;
      Reason : ACPI_Native_Server.Stop_Reason;
      Detection : ACPI_Native_HID_Detection.Report;
      Power : ACPI_Native_Power_Detection.Report;
      Delay_Accepted, Provider_Accepted : Boolean;
      Registers : constant CuBit.Handles.Startup.Optional_Endpoint := Startup.Registers;
      Register_Now : Unsigned_64;
      Devices : ACPI_Bootstrap.Device_Report;
      Root_Timing, Provider_Timing, Load_Timing : ACPI_Native_Startup_Timing.Token;
      Timing_Finished : Boolean := False;
      procedure Finish_Timing is
      begin
         if Timing_Finished then return; end if;
         Timing_Finished := True;
         ACPI_Native_Startup_Timing.Leave (Root_Timing);
         ACPI_Native_Startup_Timing.Report;
         ACPI_Native_Lock_Dump.Request (Registers);
      end Finish_Timing;
      function Elapsed_Startup_Milliseconds return String is
         Current : AML_Decode.Integer_Value;
         Known : Boolean;
      begin
         ACPI_Clock_Source.Read (Current, Known);
         return ACPI_Field_Diagnostics.Elapsed_Milliseconds
           (ACPI_Field_Diagnostics.Microseconds (Clock),
            ACPI_Field_Diagnostics.Microseconds (Current), Known);
      end Elapsed_Startup_Milliseconds;
      procedure Attempt is
      begin
      ACPI_Native_Log.Info ("acpi: stage service-start ok");
      ACPI_Native_Firmware_Profile.Log_Selected;
      ACPI_Clock_Source.Read (Clock, Available);
      if not Available or else L.R.Tick (Clock) > L.R.Tick'Last - Startup_Timeout_Microseconds then
         ACPI_Native_Log.Warning ("acpi: stage snapshot fail CLOCK_UNAVAILABLE");
         ACPI_Native_Log.Warning ("acpi: bring-up reached service-start (1/4)");
         return;
      end if;
      -- CuBit's allocator logs and stops the process on native OOM; it does
      -- not raise Storage_Error. No partial owner can then enter this runner.
      ACPI_Native_Delays.Begin_Startup
        (Unsigned_64 (L.R.Tick (Clock) + Startup_Timeout_Microseconds), Delay_Accepted);
      if not Delay_Accepted then
         ACPI_Native_Log.Warning ("acpi: stage delay-budget fail ALREADY_CLOSED");
         return;
      end if;
      if Registers.Present then
         declare
            Alias_Report : ACPI_Native_PCI_Aliases.Report;
            use type ACPI_Native_PCI_Aliases.Phase;
         begin
            ACPI_Native_PCI_Aliases.Initialize
              (L.R.Tick (Clock) + Startup_Timeout_Microseconds, Alias_Report);
            if Alias_Report.Stage = ACPI_Native_PCI_Aliases.Ready then
               ACPI_Native_Log.Info ("acpi: stage pci-alias-catalog ok bytes=" & Alias_Report.Bytes'Image &
                 " allocations=" & Alias_Report.Allocations'Image);
            elsif Alias_Report.Stage = ACPI_Native_PCI_Aliases.Absent then
               ACPI_Native_Log.Info ("acpi: stage pci-alias-catalog unavailable NO_MCFG");
            else
               ACPI_Native_Log.Warning ("acpi: stage pci-alias-catalog fail " & ACPI_Native_PCI_Aliases.Name (Alias_Report.Why) &
                 " copy=" & Name (Alias_Report.Copy.Copy.Status));
            end if;
         end;
         Register_Now := CuBit.Monotonic.Milliseconds;
         if Register_Now >= Unsigned_64'Last - Startup_Timeout_Milliseconds then
            ACPI_Native_Log.Warning ("acpi: stage register-provider fail DEADLINE_OVERFLOW"); return;
         end if;
         ACPI_Native_Startup_Timing.Enter (ACPI_Native_Startup_Timing.P.Provider_Init,Provider_Timing);
         ACPI_Native_Fields.Begin_Startup (Registers.Value,
           ACPI_Native_Fields.Wire.Time_Instant (Register_Now + Startup_Timeout_Milliseconds), Provider_Accepted);
         ACPI_Native_Startup_Timing.Leave (Provider_Timing);
         if Provider_Accepted then ACPI_Native_Evaluation.Select_Registers (Provider_Accepted); end if;
         if not Provider_Accepted then
            ACPI_Native_Log.Warning ("acpi: stage register-provider fail " & ACPI_Native_Fields.Observe.Stage'Image);
            ACPI_Native_Log.Warning ("acpi: bring-up reached service-start (1/4)"); return;
         end if;
         ACPI_Native_Log.Info ("acpi: stage register-provider ok named=" & Natural'Image (ACPI_Native_Fields.Observe.Registers));
      else
         ACPI_Native_Evaluation.Select_Observer (Provider_Accepted);
         if not Provider_Accepted then
            ACPI_Native_Log.Warning ("acpi: stage register-provider fail ALREADY_SELECTED"); return;
         end if;
         ACPI_Native_Log.Info ("acpi: stage register-provider unavailable observer-only");
      end if;
      ACPI_Native_Startup_Timing.Enter (ACPI_Native_Startup_Timing.P.Snapshot_Load,Load_Timing);
      ACPI_Native_Log.Info ("acpi: stage snapshot-start ok");
      Loader.Load (Device_Run, L.R.Tick (Clock) + Startup_Timeout_Microseconds, Owner, Result);
      ACPI_Native_Startup_Timing.Leave (Load_Timing);
      if Result.Startup.Catalog.Ready then
         ACPI_Native_Log.Info ("acpi: stage aml-input ok OBSERVED tables=" & Result.Startup.Catalog.Tables'Image &
           " bytes=" & Result.Startup.Catalog.Total_Bytes'Image & " largest=" & Result.Startup.Catalog.Largest_Table'Image);
      end if;
      if ACPI_Allocation_Diagnostics.Present (Result.Startup.Allocation) then
         ACPI_Native_Log.Warning ("acpi: stage allocation fail " &
           ACPI_Allocation_Diagnostics.Format (Result.Startup.Allocation));
      end if;
      if Result.Startup.Fatal.Present then
         ACPI_Native_Log.Critical
           (ACPI_Fatal_Diagnostics.Format (Result.Startup.Fatal));
      end if;
      ACPI_Native_Module_Notes.Observe (Result.Startup.Module_Errors);
      if Result.Startup.Status /= L.Loaded or else Owner = null then
         ACPI_Native_Log.Warning
           ("acpi: stage snapshot fail " & Name (Result.Startup.Status) &
            " table=" & Natural'Image (Result.Startup.Table) &
            " admission=" & Name (Result.Startup.Admission) &
            " copy=" & Name (Result.Startup.Copy.Status));
         if Result.Startup.Status = L.Startup_Refused then
            if Result.Startup.Completion.Execution.Modules.Field_Failure.Present then
               Report_Firmware_Session;
               ACPI_Native_Log.Warning
                 (ACPI_Startup_Diagnostics.Format_Field_Failure
                    (Result.Startup.Completion, ACPI_Native_Fields.Observe.Capacity) & " " &
                  ACPI_Field_Diagnostics.Format (ACPI_Native_Fields.First_Failure) &
                  " elapsed_ms=" & Elapsed_Startup_Milliseconds);
            end if;
            ACPI_Native_Log.Warning
              (ACPI_Startup_Diagnostics.Format (Result.Startup.Completion));
            if Result.Startup.Values.Available then
               ACPI_Native_Log.Warning ("acpi: stage aml-storage ok " &
                 ACPI_Allocation_Diagnostics.Format (Result.Startup.Values));
            end if;
            ACPI_Native_Log.Warning
              ("acpi: stage aml-limits ok CONFIGURED nodes=" &
               Positive'Image (ACPI_Service.Max_Namespace_Nodes) &
               " method-bytes=" & ACPI_Service.Namespace.Aggregate_Method_Count'Last'Image &
               " single-method=" & Positive'Image (AML_Execute.Max_Method_Bytes));
         end if;
         if Result.Last_Copy.Available then
            ACPI_Native_Log.Warning
              ("acpi: stage snapshot-detail fail " & Name (Result.Startup.Status) &
               " last-copy-status=" & Name (Result.Last_Copy.Status) &
               " table=0x" & Hex (Unsigned_64 (Result.Last_Copy.Table)) &
               " offset=0x" & Hex (Unsigned_64 (Result.Last_Copy.Offset)) &
               " copied=0x" & Hex (Unsigned_64 (Result.Last_Copy.Copied)));
         end if;
         ACPI_Native_Log.Warning ("acpi: bring-up reached service-start (1/4)");
         return;
      end if;
      ACPI_Native_Log.Info
        ("acpi: stage snapshot ok tables=" & L.Count'Image (Result.Startup.Catalog.Tables) &
         " bytes=" & L.Count'Image (Result.Startup.Catalog.Total_Bytes));
      ACPI_Native_Log.Info (ACPI_Startup_Diagnostics.Format (Result.Startup.Completion));
      ACPI_Native_EC_Boot_Notes.Observe (Owner.all);
      Devices := ACPI_Requests.Device_Completion (Owner.all);
      if ACPI_Startup_Diagnostics.Device_Initialization_Completed (Devices) then
         ACPI_Native_Log.Info (ACPI_Startup_Diagnostics.Format (Devices));
      else
         ACPI_Native_Log.Warning (ACPI_Startup_Diagnostics.Format (Devices));
      end if;
      ACPI_Native_HID_Detection.Run (Owner.all, Detection);
      ACPI_Native_Module_Notes.Observe (ACPI_Requests.Module_Errors (Owner.all));
      if Detection.Cleanup_Required or else Detection.Recovery_Needed then
         ACPI_Native_Log.Warning ("acpi: bring-up reached hid-discovery (3/4)");
         -- Do not proceed to a serving owner after uncertain retained cleanup.
         return;
      end if;
      ACPI_Native_Power_Detection.Run (Owner.all, Power);
      ACPI_Native_Module_Notes.Observe (ACPI_Requests.Module_Errors (Owner.all));
      if Power.Recovery_Needed then
         ACPI_Native_Log.Warning ("acpi: bring-up reached power-discovery (3/4)");
         return;
      end if;
      ACPI_Native_Evaluation.Close_Startup;
      ACPI_Native_Delays.Close_Startup;
      Registration_Result := CuBit.Messages.registerDriver
        (CuBit.Messages.DRIVER_ACPI, Startup.Registration);
      if Registration_Result = Unsigned_64'Last then
         ACPI_Native_Log.Warning ("acpi: stage registration fail REFUSED");
         ACPI_Native_Log.Warning ("acpi: bring-up reached startup (3/4)");
         return;
      end if;
      ACPI_Native_Log.Info ("acpi: stage registration ok observer-only");
      ACPI_Native_Log.Info ("acpi: bring-up reached registration (4/4) hardware-pending");
      Finish_Timing;
      ACPI_Native_Server.Run (Owner.all, Reason);
      ACPI_Native_Module_Notes.Observe (ACPI_Requests.Module_Errors (Owner.all));
      if Reason = ACPI_Native_Server.Stop_Requested then
         ACPI_Native_Log.Info ("acpi: stage serve ok STOP_REQUESTED");
      else
         ACPI_Native_Log.Warning ("acpi: stage serve fail " & Reason'Image);
      end if;
      end Attempt;
   begin
      if Started then return; end if;
      Started := True;
      ACPI_Native_Startup_Timing.Start;
      ACPI_Native_Startup_Timing.Enter (ACPI_Native_Startup_Timing.P.Startup,Root_Timing);
      -- Expected failures return typed outcomes from Attempt. The native
      -- runtime terminates unexpected failures; it has no exception handlers.
      Attempt;
      ACPI_Native_Evaluation.Close_Startup;
      ACPI_Native_Delays.Close_Startup;
      Finish_Timing;
   end Start;
end ACPI_Native_Instance;
