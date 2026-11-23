with ACPI_Native_Server.With_Passive_Queries;
with ACPI_Native_Server.With_Host_Bridges;
with ACPI_Host_Bridge_Owners;
with ACPI_Native_Host_Bridge_Windows;
with CuBit.Host_Bridge_Windows;
with CuBit.Control_Events;
with CuBit.Device_Interrupts;
with ACPI_Native_Events;
with ACPI_Native_Field_Profile;
with ACPI_Native_Passive_Queries;
with ACPI_Startup_Phase_Trace;
with ACPI_Native_HID_Producer;
with ACPI_Native_HID_Collection;
with ACPI_Serving_State;
with ACPI_Startup_Completion;
with ACPI_Native_Consumer_Completion;
with ACPI_Native_Startup_Completion;
with CuBit.Consumer_Initialization_Exchange;
with ACPI_Native_Initializer_History;
with ACPI_Native_LPC_Probe;
with ACPI_Native_Pre_Device;
with ACPI_Requests.With_Owner_Cleanup;
with ACPI_Catalog_Header_Notes;
with ACPI_Native_Startup_Timing;
with ACPI_Native_AML_Profile;
with ACPI_Native_Firmware_Profile;
with Interfaces; use Interfaces;
with AML_Decode;
with ACPI_Field_Diagnostics;
with AML_Field_Access;
with ACPI_One_Service;
with ACPI_Native_Endpoint;
with ACPI_Native_Power_Entry;
with ACPI_Native_Power_Buttons;
with ACPI_Native_Power_Inventory;
with ACPI_Native_Battery_Reads;
with ACPI_Power_Watch_Protocol;
with ACPI_Native_Button_Discovery;
with ACPI_Native_Server.With_Power_Entry;
with ACPI_System_Events;
with ACPI_Backend_Kernel_Handles;
with ACPI_Backend_Binding_Types;
with CuBit.ACPI_Windows;
with CuBit.Handles.IPC;
with CuBit.Process_IDs;
with ACPI_Clock_Source;
with ACPI_Bootstrap;
with ACPI_Native_Delays;
with ACPI_Fatal_Diagnostics;
with ACPI_Snapshot_Startup;
with ACPI_Native_Snapshot_Load;
with ACPI_Native_Server;
with ACPI_SCI_Events;
with ACPI_SCI_Registers;
with ACPI_SCI_Quiesce;
with ACPI_Native_HID_Detection;
with ACPI_HID_Initialization;
with ACPI_Native_Power_Detection;
with ACPI_Native_Log;
with ACPI_Native_Diagnostic_Output;
with ACPI_Native_EC_Discovery;
with ACPI_Native_Module_Notes;
with CuBit.CPU_Times;with CuBit.CPU_Load;
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
with ACPI_Native_Regions;
with ACPI_Native_Event_Domain;
with ACPI_Requests.With_Region_Cleanup;
with AML_Execute;
with ACPI_Native_PCI_Aliases;
with ACPI_Native_Snapshot_Load.With_Driver;
with CuBit.Handles.Startup;
with CuBit.Monotonic;
package body ACPI_Native_Instance is
   package L renames ACPI_Snapshot_Startup;
   package One renames ACPI_One_Service;
   package Root renames ACPI_One_Service.Root;
   use type L.Outcome;
   use type ACPI_Native_Server.Stop_Reason;
   use type ACPI_Native_EC_Discovery.Outcome;
   use type ACPI_Native_Server.Owner_Outcome;
   use type One.Runtime.Status.Outcome;
   use type One.Runtime.Status.Phase;
   use type L.State_Access;
   use type L.R.Tick;
   package HB renames ACPI_Host_Bridge_Owners;
   package HB_Windows renames ACPI_Native_Host_Bridge_Windows;
   Owner : L.State_Access;
   Bridge_Target : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   Bridge_Peer : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
   Bridge_Query_Microseconds : constant Unsigned_64 := 5_000_000;
   Bridge_Query_Milliseconds : constant Unsigned_64 := 5_000;
   Bridge_Metadata_Batch : constant Positive := 16;
   Bridge_Metadata_Budget : constant Natural := ACPI_Service.Max_Namespace_Nodes + 1;
   Bridge_AML_Budget : constant Natural := 1_000_000;
   Bridge_Item_Limit : constant HB.G.H.R.Item_Count := 4096;
   Bridge_Stop_Milliseconds : constant Unsigned_64 := 1_000;
   -- Process-local reservation ACHB: low 32 bits are paired, never reused.
   Bridge_Token_First : constant Unsigned_64 := 16#4143_4842_0000_0000#;
   Bridge_Token_Entries : constant Unsigned_64 := 2 ** 32;
   subtype Bridge_Allocation_Count is Unsigned_64 range 0 .. Bridge_Token_Entries / 2;
   Bridge_Used : Bridge_Allocation_Count := 0;
   Bridge_Connector : constant Unsigned_16 := 0;
   Bridge_First_Sequence : constant Unsigned_64 := 1;
   Bridge_SCI_Ack_Noted : Boolean := False;
   Bridge_Report_Noted : Boolean := False;
   Bridge_Retention_Noted : Boolean := False;
   function Bridge_Metadata_Ready return Boolean;
   function Bridge_Query_Ready return Boolean;
   procedure Bridge_Correlation (Device : ACPI_Service.Node_Descriptor;
      Root_ID, Sequence, Open_Token, Data_Token : out Unsigned_64; Available : out Boolean) is
      pragma Unreferenced (Device);
   begin
      Root_ID := 0; Sequence := 0; Open_Token := 0; Data_Token := 0; Available := False;
      if Bridge_Used = Bridge_Allocation_Count'Last then return; end if;
      Bridge_Used := Bridge_Used + 1; Root_ID := Bridge_Used; Sequence := Bridge_First_Sequence;
      Open_Token := Bridge_Token_First + 2 * (Bridge_Used - 1); Data_Token := Open_Token + 1;
      Available := True;
   end Bridge_Correlation;
   package Bridges is new HB.Managers
     (L.State_Access, Bridge_Metadata_Ready, Bridge_Query_Ready, HB_Windows.Current,
      HB_Windows.Clock, HB_Windows.Begin_Window, HB_Windows.End_Window,
      One.Runtime.Evaluate_Named, Bridge_Correlation, HB.G.Canonical_Reasons);
   use type HB.Phase, HB.Failure;
   procedure Maybe_Start_Bridges (Server : in out ACPI_Requests.State) is
      Us, Ms : Unsigned_64; Good : Boolean;
   begin
      if Bridges.Observe.Attempted or Bridges.Observe.Stop_Requested then return; end if;
      if not ACPI_Requests.Valid (Server) then Bridges.Request_Stop; return; end if;
      if not CuBit.Handles.Valid (Bridge_Target) or else not CuBit.Process_IDs.Is_Process (Bridge_Peer) then
         Bridges.Request_Stop;
         ACPI_Native_Diagnostic_Output.Write ("acpi: stage host-bridge fail ENDPOINT_UNAVAILABLE", False);
         return;
      end if;
      if not Bridge_Metadata_Ready then return; end if;
      HB_Windows.Clock (Us, Ms, Good);
      if not Good or else Us >= CuBit.Messages.Wait_Forever - Bridge_Query_Microseconds
        or else Ms >= CuBit.Messages.Wait_Forever - Bridge_Query_Milliseconds then
         Bridges.Request_Stop;
         ACPI_Native_Diagnostic_Output.Write ("acpi: stage host-bridge fail CLOCK_UNAVAILABLE", False);
         return;
      end if;
      Bridges.Start (Owner, Bridge_Target, Bridge_Peer, CuBit.Host_Bridge_Windows.Element, Bridge_Connector,
         Us + Bridge_Query_Microseconds, Ms + Bridge_Query_Milliseconds,
         Bridge_Metadata_Budget, Bridge_Metadata_Batch, Bridge_AML_Budget, Bridge_Item_Limit);
   end Maybe_Start_Bridges;

   -- Native monotonic samples and snapshot deadlines are microseconds.
   Startup_Timeout_Microseconds : constant L.R.Tick := 60_000_000;
   -- Separate absolute deadlines: snapshot/delays use microseconds; setup
   -- IPC (pool, PCI broker) uses GETTIME milliseconds; Hardware's lock clock
   -- is CuBit.Monotonic microseconds. Raw epochs are never interconverted:
   -- each deadline is derived from its own clock's reading.
   Startup_Timeout_Milliseconds : constant Unsigned_64 := 60_000;
   -- Disarm is retried this often when a register write is refused; each
   -- attempt is a few port writes and one acknowledgement (no wait).
   SCI_Disarm_Attempts : constant := 3;
   -- Correlates the one platform-pool request (CuBit.Platform_Pool_Exchange).
   Pool_Request_Nonce : constant Unsigned_64 := 1;
   -- GPIO admission uses request 1 on the firmware-data endpoint. Reserve 2
   -- for the one process-lifetime completion declaration; HID offers must use
   -- later, nonreused IDs. A new process incarnation has a separate epoch.
   Completion_Request_Nonce : constant Unsigned_64 := 2;
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
   procedure Initialize_Regions
     (Service : aliased in out ACPI_Service.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome)
      renames ACPI_Native_Regions.Prepare;
   Region_Cleanup_Fuel : constant Natural := 100_000;
   Region_Cleanup_Window_Microseconds : constant Unsigned_64 := 500_000;
   procedure Disconnect_Regions
     (Service : aliased in out ACPI_Service.State; Completed : out Boolean) is
      Charged : Natural;
   begin
      ACPI_Native_Regions.Disconnect(Service,Region_Cleanup_Fuel,Charged,Completed);
   end Disconnect_Regions;
   package Region_Cleanup is new ACPI_Requests.With_Region_Cleanup(Disconnect_Regions);
   procedure Close_Pre_Device
     (Service : aliased in out ACPI_Service.State; Completed : out Boolean) is
   begin ACPI_Native_Pre_Device.Close(Service);Completed:=ACPI_Native_Pre_Device.Quiet;end;
   package Pre_Device_Cleanup is new ACPI_Requests.With_Owner_Cleanup(Close_Pre_Device);
   procedure Prepare_Consumers
     (Service : aliased in out ACPI_Service.State; Budget : Natural;
      Charged : in out Natural; Accepted : in out Boolean) is
   begin ACPI_Native_Pre_Device.Prepare(Service,Budget,Charged,Accepted);end;
   package Driver is new ACPI_Device_Initialization
     (ACPI_Service, Initialize_Regions, Root.Evaluation.Evaluate_Named,
      After_Initializer => ACPI_Native_Regions.After_Ancestor,
      Finish_Regions => ACPI_Native_Regions.Finish,
      Note_Initializer => ACPI_Native_Initializer_History.Note,
      Consumer_Preparation_Enabled => True,
      Classify_Prerequisite => ACPI_Native_Regions.Classify_Prerequisite,
      Prepare_Consumers => Prepare_Consumers);
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
         Uncertain : constant AML_Field_Access.Observation := Root.Fields.Uncertain;
      begin
         ACPI_Native_Log.Warning ("acpi: stage module-field-context ok aml-recorded=" &
           Result.Modules.Field_Failure.Present'Image & " provider-uncertain=" &
           Uncertain.Present'Image &
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
              " failure=" & ACPI_Startup_Diagnostics.Execution_Name (Result.Modules.Field_Failure.Failure) else ""));
      end;
   end Report_Module_Field_Context;
   procedure Timed_Module_Startup
     (Service : in out ACPI_Service.State; Budget : Natural;
      Result : out ACPI_Service.Execution_Report; Status : out ACPI_Service.Values.Access_Status) is
      package T renames ACPI_Native_Startup_Timing;
      Timing : T.Token;
      CPU_Before,CPU_After : CuBit.CPU_Load.Snapshot;
      CPU_Frequency_Before,CPU_Frequency_After : Unsigned_64 := 0;
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
      -- The local root's module callback: Hardware -> memory bridge ->
      -- Fields.Begin_Startup from the copied FADT, then module AML.
      CuBit.CPU_Times.Read(CPU_Before);
      CPU_Frequency_Before:=CuBit.CPU_Times.Tick_Frequency;
      Root.Evaluation.Execute_Module_Startup (Service,Budget,Result,Status);
      CuBit.CPU_Times.Read(CPU_After);
      CPU_Frequency_After:=CuBit.CPU_Times.Tick_Frequency;
      ACPI_Native_AML_Profile.Stop;
      declare
         Anchor : ACPI_Service.Node_Descriptor;
         Anchor_Status : ACPI_Service.Values.Access_Status;
      begin
         ACPI_Service.Describe_Node (Service, ACPI_Service.Namespace.Root, Anchor, Anchor_Status);
         ACPI_Native_Log.Info ("acpi: stage local-module-result ok status=" & Status'Image &
           " phase=" & ACPI_Service.Current_Import_Phase (Service)'Image &
           " report=" & Result.Phase'Image & " root-describe=" & Anchor_Status'Image);
      end;
      T.Leave (Timing);
      ACPI_Native_AML_Profile.Report;
      ACPI_Native_Field_Profile.Report;
      case Result.Modules.Outcome is
         when ACPI_Service.Values.Modules_Completed => null;
         when others => Report_Module_Field_Context (Result,
           IO_Pending => Root.Fields.Observe.IO_Pending,
           IO_Uncertain => Root.Fields.Uncertain.Present);
            ACPI_Native_AML_Profile.Report_Tail
           (IO_Pending => Root.Fields.Observe.IO_Pending,
            IO_Uncertain => Root.Fields.Uncertain.Present);
            Report_Catalog (Service, IO_Pending => Root.Fields.Observe.IO_Pending,
              IO_Uncertain => Root.Fields.Uncertain.Present);
      end case;
      declare
         After : constant ACPI_Service.Metrics := ACPI_Service.Observe (Service);
         Module_After : constant T.P.Counters := T.Snapshot (T.P.Module_Execution);
         GC_After : constant T.P.Counters := T.Snapshot (T.P.Work_Allocation);
         CPU_Interval : constant CuBit.CPU_Load.Interval := CuBit.CPU_Load.Sum(CPU_Before,CPU_After);
         Comparable_CPUs : Unsigned_64 := 0;
         CPU_Unavailable : constant Boolean := CPU_Before.Count=0 or else CPU_After.Count=0
           or else CPU_Frequency_Before=0 or else CPU_Frequency_After=0;
         CPU_Saturated : constant Boolean := CPU_Interval.Total=Unsigned_64'Last or else CPU_Interval.Busy=Unsigned_64'Last;
         Wall : constant Unsigned_64 := Delta_Value (Module_After.Total_US,Module_Before.Total_US);
         External_Time : constant Unsigned_64 := Delta_Value (Module_After.External_US,Module_Before.External_US);
      begin
         for CPU in CuBit.CPU_Times.CTP.Processor loop
            exit when Unsigned_64(CPU)>=CPU_Before.Count or else Unsigned_64(CPU)>=CPU_After.Count;
            if CuBit.CPU_Load.Comparable(CPU_Before.CPUs(CPU),CPU_After.CPUs(CPU)) and then
              CuBit.CPU_Load.Between(CPU_Before.CPUs(CPU).Totals,CPU_After.CPUs(CPU).Totals).Total>0 then
               Comparable_CPUs:=Comparable_CPUs+1;
            end if;
         end loop;
         -- System-wide accounting includes other processes and scheduling.
         -- Raw ticks avoid overflow/rounding in a diagnostic conversion to us.
         ACPI_Native_Log.Info("acpi: stage module-cpu ok scope=SYSTEM_WIDE busy_ticks="&CPU_Interval.Busy'Image&
           " total_ticks="&CPU_Interval.Total'Image&" hz="&CPU_Frequency_Before'Image);
         ACPI_Native_Log.Info("acpi: stage module-cpu-coverage ok before="&CPU_Before.Count'Image&
           " after="&CPU_After.Count'Image&" comparable="&Comparable_CPUs'Image&
           " quality="&(if CPU_Unavailable then "UNAVAILABLE"
             elsif CPU_Before.Count/=CPU_After.Count or else Comparable_CPUs/=CPU_Before.Count
               or else CPU_Frequency_Before/=CPU_Frequency_After or else CPU_Saturated then "PARTIAL" else "COMPLETE")&
           " saturated="&CPU_Saturated'Image&" end_hz="&CPU_Frequency_After'Image);
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
   procedure Logged_Member_Startup
     (Service : in out ACPI_Service.State; Budget : Natural;
      Result : out ACPI_Service.Namespace.Field_Initialization_Report;
      Status : out ACPI_Service.Values.Access_Status) is
      use type ACPI_Service.Values.Access_Status;
      use type ACPI_Service.Namespace.Field_Initialization_Outcome;
   begin
      Root.Evaluation.Initialize_Member_Startup(Service,Budget,Result,Status);
      if Status=ACPI_Service.Values.Available
        and then Result.Outcome=ACPI_Service.Namespace.Initialization_Completed
        and then Result.Members.Missing=0 and then Result.Members.Unsupported=0
        and then not Result.Fatal.Present and then not Result.Reconciliation_Required
      then
         ACPI_Native_Log.Info("acpi: stage members ok bound=" & Result.Members.Bound'Image &
           " charged=" & Result.Charged'Image & " effects=" & Result.Provider_May_Have_Effects'Image);
      else
         ACPI_Native_Log.Warning("acpi: stage members fail " & Result.Failure'Image &
           " access=" & Status'Image & " outcome=" & Result.Outcome'Image &
           " missing=" & Result.Members.Missing'Image & " unsupported=" & Result.Members.Unsupported'Image &
           " charged=" & Result.Charged'Image & " effects=" & Result.Provider_May_Have_Effects'Image &
           " pending=" & Result.Reconciliation_Required'Image & " fatal=" & Result.Fatal.Present'Image);
      end if;
   end Logged_Member_Startup;
   package Loader is new ACPI_Native_Snapshot_Load.With_Driver
     (Driver, Timed_Module_Startup, Logged_Member_Startup);
   -- SCI-DELIVERY binding point: the SCI's fixed events and GPEs go to these
   -- consumers. Fixed facts are copied, EC edges are masked/ACKed before
   -- deferred query work, and all Notify routes precede broker ACK.
   package SCI is new ACPI_SCI_Events.With_Consumer
     (Fixed_Event => ACPI_Native_Event_Domain.Fixed_Event,
      GPE_Event   => ACPI_Native_Event_Domain.GPE_Event);
   -- The sole serving loop: its idle wait is bounded by the root's hardware
   -- maintenance deadline (and every other root-owned deadline). The IRQ
   -- doorbell (the SCI) is handled in it, between requests.
   procedure Advance_Power (Server : ACPI_Requests.State; Did_Work : out Boolean) is
      Result : ACPI_Native_Power_Entry.Step_Outcome;
      use type ACPI_Native_Power_Entry.Step_Outcome;
      Button_Work,Inventory_Work,Read_Work : Boolean;
   begin
      ACPI_Native_Battery_Reads.Advance(Server,ACPI_Native_Event_Domain.Diagnostics_Quiet(Server),
        not ACPI_Native_Power_Entry.Blocks_Runtime,Read_Work);
      ACPI_Native_Power_Inventory.Advance(ACPI_Native_Event_Domain.Diagnostics_Quiet(Server),Inventory_Work);
      ACPI_Native_Power_Buttons.Advance (ACPI_Native_Event_Domain.Diagnostics_Quiet (Server), Button_Work);
      ACPI_Native_Power_Entry.Advance (Server, Result);
      Did_Work := Read_Work or else Button_Work or else Inventory_Work or else Result /= ACPI_Native_Power_Entry.Idle;
   end Advance_Power;
   function Bridge_Metadata_Ready return Boolean is
     (Owner /= null and then not SCI.Deferred and then HB_Windows.Metadata_Ready (Owner.all));
   function Bridge_Query_Ready return Boolean is
     (Owner /= null and then not SCI.Deferred and then HB_Windows.Query_Ready (Owner.all));
   procedure Advance_Normal (Server : in out ACPI_Requests.State; Result : out ACPI_Native_Server.Owner_Outcome) is
   begin
      if SCI.Deferred and then not Bridges.Blocks_AML and then ACPI_Native_Event_Domain.Diagnostics_Quiet (Server) then
         SCI.Service_Deferred;
      end if;
      if ACPI_Native_Power_Buttons.Retirement_Requested then
         Result := ACPI_Native_Server.Owner_Refused;
      else
         ACPI_Native_Event_Domain.Advance (Server, Result);
         if Result=ACPI_Native_Server.Owner_Idle and then (ACPI_Native_Power_Inventory.Pending or ACPI_Native_Battery_Reads.Pending)
           and then ACPI_Native_Event_Domain.Diagnostics_Quiet(Server) then
            Result:=ACPI_Native_Server.Owner_Pending;
         end if;
      end if;
   end Advance_Normal;
   package Power_Loop is new ACPI_Native_Server.With_Power_Entry
     (Advance_Power, ACPI_Native_Power_Entry.Runnable, ACPI_Native_Power_Entry.Blocks_Runtime,
      ACPI_Native_Power_Entry.Wake_Deadline_Milliseconds, Advance_Normal,
      One.Serving_Maintenance, ACPI_Native_Event_Domain.Diagnostics_Quiet);
   package Query_Loop is new ACPI_Native_Server.With_Passive_Queries
     (Power_Loop.Advance,Power_Loop.Maintain);
   package Observer_Query_Loop is new ACPI_Native_Server.With_Passive_Queries;
   procedure Maintain_Bridge_Owner (Deadline : out Unsigned_64; May_Continue : out Boolean) is
      Query_Deadline : Unsigned_64;
      Queries_Healthy : Boolean;
   begin
      --  The bridge manager exclusively owns this active or retained AML window.
      --  Keep actual hardware maintenance; do not apply the normal Idle-only test.
      Root.Maintain_Owner (Deadline, May_Continue);
      ACPI_Native_Passive_Queries.Maintain (Query_Deadline, Queries_Healthy);
      Deadline := Unsigned_64'Min (Deadline, Query_Deadline);
      Deadline := Unsigned_64'Min (Deadline, ACPI_Native_Power_Entry.Wake_Deadline_Milliseconds);
      May_Continue := May_Continue and then Queries_Healthy
        and then Bridges.Blocks_AML
        and then (One.Runtime.Current in One.Runtime.Status.Idle | One.Runtime.Status.Active
          or else Bridges.Observe.Stage = HB.Retained);
   end Maintain_Bridge_Owner;
   package Bridge_Loop is new ACPI_Native_Server.With_Host_Bridges
     (Bridges, Maybe_Start_Bridges, Query_Loop.Advance, Power_Loop.Advance,
      ACPI_Native_Power_Entry.Blocks_Runtime, Query_Loop.Maintain, Power_Loop.Diagnostics_Quiet, Maintain_Bridge_Owner);
   procedure Runtime_Dispatch (Server : in out ACPI_Requests.State; From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message) is
   begin
      ACPI_Native_Endpoint.Dispatch_With_Admission (Server, From, Request, Reply,
        Bridge_Loop.Admission_Open, Cached_Button_Watches_Open => True);
   end Runtime_Dispatch;
   procedure Runtime_Advance (Server : in out ACPI_Requests.State; Result : out ACPI_Native_Server.Owner_Outcome) is
      View : HB.Report;
   begin
      Bridge_Loop.Advance (Server, Result); View := Bridges.Observe;
      if View.Attempted and then View.Stage in HB.Holding | HB.Finished | HB.Retained
        and then (not Bridge_Report_Noted or else (View.Stage = HB.Retained and then not Bridge_Retention_Noted)) then
         Bridge_Report_Noted := True;
         if View.Stage = HB.Retained then Bridge_Retention_Noted := True; end if;
         ACPI_Native_Diagnostic_Output.Write
           ("acpi: stage host-bridge " & (if View.Had_Errors then "fail " else "ok ") &
            (if View.Stage = HB.Retained then "RETAINED" else "DISCOVERY_FINISHED") &
            " reason-id=" & HB.Failure'Pos (View.Why)'Image & " candidates=" & View.Candidates'Image &
            " visited=" & View.Visited'Image & " opened=" & View.Reports_Opened'Image &
            " acknowledged=" & View.Acknowledged'Image & " held=" & View.Held_Sessions'Image &
            " failed=" & View.Failed_Reports'Image & " incomplete=" & View.Incomplete_Reports'Image,
            Bridge_Loop.Diagnostics_Quiet (Server));
      end if;
   end Runtime_Advance;
   procedure Serving_Event (Publisher : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message;
      Stop : out Boolean; Handled : out Boolean) is
      Ack : ACPI_SCI_Quiesce.Acknowledgement;
      use type ACPI_System_Events.Action;
      use type ACPI_SCI_Quiesce.Acknowledgement;
      Ends : Boolean := False;
   begin
      if ACPI_System_Events.Classify (Publisher, Item) = ACPI_System_Events.Grant_Notice then
         Bridges.Note_Event (CuBit.Control_Events.Decode (Item.tag.label, Item.tag.length,
           Item.words (0), Item.words (1), Item.words (2)), Ends);
      end if;
      if Bridges.Blocks_AML and then CuBit.Device_Interrupts.Is_Doorbell (Item) then
         SCI.Defer_Event (Publisher, Item, Ack); Stop := False; Handled := True;
         if Ack = ACPI_SCI_Quiesce.Acknowledge_Refused and then not Bridge_SCI_Ack_Noted then
            Bridge_SCI_Ack_Noted := True;
            ACPI_Native_Diagnostic_Output.Write ("acpi: stage sci fail ACK_REFUSED deferred=1", False);
         end if;
      elsif ACPI_Native_Power_Entry.Blocks_Runtime then
         Stop := ACPI_System_Events.Classify (Publisher, Item) = ACPI_System_Events.Stop_Requested;
         ACPI_SCI_Events.Acknowledge_Only (Publisher, Item, Ack);
         Handled := True;
         -- Final entry admits no more GPE/EC/consumer work or channel logging.
      else SCI.Handle_Event (Publisher, Item, Stop, Handled); end if;
      Stop := Stop or Ends;
   end Serving_Event;
   package Serving is new ACPI_Native_Server.With_Dispatch
     (Runtime_Dispatch, Handle_Event => Serving_Event,
      Advance_Owner => Runtime_Advance, Maintain_Owner => Bridge_Loop.Maintain,
      Diagnostics_Quiet => Bridge_Loop.Diagnostics_Quiet, Reply_Now => ACPI_Native_Endpoint.Reply_Now,
      Handle_Completion => Bridge_Loop.Handle_Completion);
   package Observer_Serving is new ACPI_Native_Server.With_Dispatch
     (ACPI_Native_Endpoint.Dispatch,Advance_Owner=>Observer_Query_Loop.Advance,
      Maintain_Owner=>Observer_Query_Loop.Maintain,Reply_Now=>ACPI_Native_Endpoint.Reply_Now);
   Device_Run : Driver.Session;
   function Owner_Quiet return Boolean is
     (Owner /= null and then ACPI_Requests.Valid (Owner.all)
      and then not Bridges.Blocks_AML and then Bridges.Observe.Stage /= HB.Retained
      and then One.Runtime.Current = One.Runtime.Status.Idle
      and then ACPI_Native_Pre_Device.Quiet
      and then (ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime)
      and then not ACPI_Native_Regions.Observe.Working
      and then not ACPI_Native_Regions.Observe.Pending
      and then ACPI_Native_Event_Domain.Diagnostics_Quiet (Owner.all));
   package Startup_HID_Paths is new ACPI_HID_Initialization
     (ACPI_Service, Driver, ACPI_Native_HID_Detection.Discovery.Queries);
   package Runtime_HID_Paths is new ACPI_HID_Initialization
     (ACPI_Service, Driver, ACPI_Native_HID_Collection.Discovery.Queries);
   -- Copied observation only: an absent/failed member's Raw value is not a
   -- PCI fact. Preserve that distinction, including an observed zero.
   function HID_PCI_Member_Image
     (Item : ACPI_Native_HID_Collection.Reader.Report) return String is
   begin
      case Item.Status is
         when ACPI_Native_HID_Collection.Reader.Observed =>
            return Item.Status'Image & ":" & Item.Raw'Image;
         when others => return Item.Status'Image;
      end case;
   end HID_PCI_Member_Image;
   function HID_Error_Path_Image (Text : String) return String is
   begin
      for I in Text'Range loop
         if Text (I) /= ' ' then return Text (I .. Text'Last); end if;
      end loop;
      return "UNKNOWN";
   end HID_Error_Path_Image;
   Maximum_HID_Path_Diagnostics : constant Positive := 8;
   Maximum_HID_Path_Reason_Characters : constant Positive := 64;
   subtype HID_Path_Error_Count is Natural range 0 .. Maximum_HID_Path_Diagnostics;
   subtype HID_Path_Error_Index is Positive range 1 .. Maximum_HID_Path_Diagnostics;
   type HID_Path_Phase is (Startup_Path, Runtime_Path);
   type HID_Path_Error is record
      Phase : HID_Path_Phase := Startup_Path;
      Candidate, Failed_Node : ACPI_Service.Namespace.Node_ID := ACPI_Service.Namespace.Root;
      Path : ACPI_Native_Initializer_History.Path_Text := [others => ' '];
      Path_Complete : Boolean := False;
      Reason : String (1 .. Maximum_HID_Path_Reason_Characters) := [others => ' '];
      Reason_Length : Natural range 0 .. Maximum_HID_Path_Reason_Characters := 0;
      Status_Known : Boolean := False;
      Flags : Unsigned_64 := 0;
      Access_Result : ACPI_Service.Values.Access_Status := ACPI_Service.Values.Wrong_Phase;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Charged : Natural := 0;
   end record;
   HID_Path_Errors : array (HID_Path_Error_Index) of HID_Path_Error;
   HID_Path_Used, HID_Path_Emitted : HID_Path_Error_Count := 0;
   HID_Path_Total, HID_Path_Reported : Natural := 0;
   procedure Capture_HID_Path_Error
     (Service : ACPI_Service.State; Phase : HID_Path_Phase;
      Candidate, Failed_Node : ACPI_Service.Node_Descriptor; Reason : String;
      Status_Known : Boolean; Flags : Unsigned_64;
      Access_Result : ACPI_Service.Values.Access_Status;
      Execution : AML_Execute.Execution_Status; Charged : Natural) is
      E : HID_Path_Error;
   begin
      HID_Path_Total := HID_Path_Total + 1;
      if HID_Path_Used = Maximum_HID_Path_Diagnostics then return; end if;
      E.Phase := Phase;
      E.Candidate := ACPI_Service.Values.Position (Candidate);
      E.Failed_Node := ACPI_Service.Values.Position (Failed_Node);
      E.Reason_Length := Natural'Min (Reason'Length, E.Reason'Length);
      if E.Reason_Length > 0 then
         E.Reason (1 .. E.Reason_Length) := Reason (Reason'First .. Reason'First + E.Reason_Length - 1);
      end if;
      E.Status_Known := Status_Known; E.Flags := Flags;
      E.Access_Result := Access_Result; E.Execution := Execution; E.Charged := Charged;
      if ACPI_Service.Current_Node (Service, Failed_Node) then
         ACPI_Native_Initializer_History.Copy_Path (Service, E.Failed_Node, E.Path, E.Path_Complete);
      end if;
      HID_Path_Used := HID_Path_Used + 1;
      HID_Path_Errors (HID_Path_Used) := E;
   end Capture_HID_Path_Error;
   procedure Report_HID_Path_Errors is
   begin
      -- Detached facts only: neither AML nor a borrowed Core reaches logging.
      while HID_Path_Emitted < HID_Path_Used loop
         HID_Path_Emitted := HID_Path_Emitted + 1;
         declare E : constant HID_Path_Error := HID_Path_Errors (HID_Path_Emitted);
         begin
            ACPI_Native_Diagnostic_Output.Write
              ("acpi: stage hid-ancestry fail " & (if E.Phase = Startup_Path then "STARTUP_PATH" else "RUNTIME_PATH") &
               " reason=" & E.Reason (1 .. E.Reason_Length) &
               " candidate=" & E.Candidate'Image & " node=" & E.Failed_Node'Image &
               " path=" & HID_Error_Path_Image (E.Path) & " path-complete=" & Boolean'Image (E.Path_Complete) &
               " sta=" & (if E.Status_Known then "0x" & Hex (E.Flags) else "UNKNOWN") &
               " access=" & E.Access_Result'Image & " execution=" & ACPI_Startup_Diagnostics.Execution_Name (E.Execution) &
               " fuel=" & E.Charged'Image, Owner_Quiet);
         end;
      end loop;
      if HID_Path_Total /= HID_Path_Reported then
         HID_Path_Reported := HID_Path_Total;
         ACPI_Native_Diagnostic_Output.Write
           ("acpi: stage hid-ancestry-summary errors=" & HID_Path_Total'Image &
            " captured=" & HID_Path_Used'Image &
            " omitted=" & Natural'Image (HID_Path_Total - Natural (HID_Path_Used)), Owner_Quiet);
      end if;
   end Report_HID_Path_Errors;
   procedure Check_Startup_HID
     (Service : aliased in out ACPI_Service.State;
      Item : ACPI_Service.Node_Descriptor; Budget : Natural;
      Result : out ACPI_Native_HID_Detection.Owner.Controller.Policy_Report) is
      View : Startup_HID_Paths.Report;
      use type Startup_HID_Paths.Outcome;
   begin
      Startup_HID_Paths.Check_Path (Device_Run, Service, Item, Budget, View);
      Result := (Outcome => (case View.Result is
         when Startup_HID_Paths.Eligible => ACPI_Native_HID_Detection.Owner.Controller.Eligible,
         when Startup_HID_Paths.Excluded => ACPI_Native_HID_Detection.Owner.Controller.Excluded,
         when Startup_HID_Paths.Status_Failed => ACPI_Native_HID_Detection.Owner.Controller.Node_Error,
         when Startup_HID_Paths.Budget_Exhausted | Startup_HID_Paths.History_Unknown |
           Startup_HID_Paths.Initialization_Incomplete => ACPI_Native_HID_Detection.Owner.Controller.Policy_Incomplete,
         when Startup_HID_Paths.Recovery_Required => ACPI_Native_HID_Detection.Owner.Controller.Recovery_Required,
         when others => ACPI_Native_HID_Detection.Owner.Controller.Policy_Failed), Charged => View.Charged);
      if View.Result not in Startup_HID_Paths.Eligible | Startup_HID_Paths.Excluded then
         Capture_HID_Path_Error (Service, Startup_Path, Item, View.Last_Node,
           Startup_HID_Paths.Name (View.Result), Startup_HID_Paths.Status_Known (View),
           View.Last_Status.Flags, View.Last_Status.Access_Result, View.Last_Status.Execution, View.Charged);
      end if;
   end Check_Startup_HID;
   procedure Check_Runtime_HID
     (Service : aliased in out ACPI_Service.State;
      Item : ACPI_Service.Node_Descriptor; Budget : Natural;
      Result : out ACPI_Native_HID_Collection.Controller.HID.Controller.Policy_Report) is
      View : Runtime_HID_Paths.Report;
      use type Runtime_HID_Paths.Outcome;
   begin
      Runtime_HID_Paths.Check_Path (Device_Run, Service, Item, Budget, View);
      Result := (Outcome => (case View.Result is
         when Runtime_HID_Paths.Eligible => ACPI_Native_HID_Collection.Controller.HID.Controller.Eligible,
         when Runtime_HID_Paths.Excluded => ACPI_Native_HID_Collection.Controller.HID.Controller.Excluded,
         when Runtime_HID_Paths.Status_Failed => ACPI_Native_HID_Collection.Controller.HID.Controller.Node_Error,
         when Runtime_HID_Paths.Budget_Exhausted | Runtime_HID_Paths.History_Unknown |
           Runtime_HID_Paths.Initialization_Incomplete => ACPI_Native_HID_Collection.Controller.HID.Controller.Policy_Incomplete,
         when Runtime_HID_Paths.Recovery_Required => ACPI_Native_HID_Collection.Controller.HID.Controller.Recovery_Required,
         when others => ACPI_Native_HID_Collection.Controller.HID.Controller.Policy_Failed), Charged => View.Charged);
      if View.Result not in Runtime_HID_Paths.Eligible | Runtime_HID_Paths.Excluded then
         Capture_HID_Path_Error (Service, Runtime_Path, Item, View.Last_Node,
           Runtime_HID_Paths.Name (View.Result), Runtime_HID_Paths.Status_Known (View),
           View.Last_Status.Flags, View.Last_Status.Access_Result, View.Last_Status.Execution, View.Charged);
      end if;
   end Check_Runtime_HID;
   procedure Discover_HID is new ACPI_Native_HID_Detection.Run_With_Policy (Check_Startup_HID);
   procedure Collect_HID is new ACPI_Native_HID_Collection.Controller.Collect_With_Policy (Check_Runtime_HID);
   package HID_Producer is new ACPI_Native_HID_Producer (Owner_Quiet, Collect_HID);
   function Completion_Quiet return Boolean is (Owner_Quiet and then HID_Producer.Quiet);
   -- All state, including an unknown receipt, outlives Start and every Stop.
   package Consumer_Completion is new ACPI_Native_Consumer_Completion (Completion_Quiet);
   type Copied_Node_Error is record
      Facts : Driver.Error_Report;
      Path : ACPI_Native_Initializer_History.Path_Text := [others => ' '];
      Path_Complete : Boolean := False;
   end record;
   type Copied_Node_Errors is array (Driver.Error_Position) of Copied_Node_Error;
   type Startup_Diagnostic is record
      Captured, Core_Observed, Metadata_Accepted : Boolean := False;
      Import_State : ACPI_Service.Import_Phase := ACPI_Service.Import_Phase'First;
      Namespace_State : ACPI_Service.Values.Phase := ACPI_Service.Values.Phase'First;
      Nodes, Pending_Members, Work_Limit : Natural := 0;
      Devices : Driver.Report;
      Error_Count : Driver.Error_Count := 0;
      Error_Omitted : Natural := 0;
      Errors : Copied_Node_Errors;
      Initializers : ACPI_Native_Initializer_History.Report;
      Consumer_Preparation : ACPI_Native_Pre_Device.Report;
   end record;
   Saved_Startup : Startup_Diagnostic;
   Node_Errors_Emitted : Boolean := False;
   procedure Capture_Startup_Diagnostic is
      procedure Copy_State (Service : ACPI_Service.State; Completed : out Boolean) is
      begin
         Saved_Startup.Core_Observed := True;
         Saved_Startup.Import_State := ACPI_Service.Current_Import_Phase(Service);
         Saved_Startup.Namespace_State := ACPI_Service.Namespace_Phase(Service);
         Saved_Startup.Nodes := Natural(ACPI_Service.Namespace_Node_Count(Service));
         Saved_Startup.Pending_Members := ACPI_Service.Pending_Members(Service);
         for I in Driver.Error_Position loop
            exit when Natural (I) > Natural (Saved_Startup.Error_Count);
            Saved_Startup.Errors (I).Facts := Driver.Error_At (Device_Run, I);
            ACPI_Native_Initializer_History.Copy_Path
              (Service, Saved_Startup.Errors (I).Facts.Scope,
               Saved_Startup.Errors (I).Path, Saved_Startup.Errors (I).Path_Complete);
         end loop;
         Completed := True;
      end Copy_State;
      package Metadata is new ACPI_Requests.With_Metadata_Continuation(Copy_State);
   begin
      Saved_Startup.Captured := True;
      Saved_Startup.Devices := Driver.Observe(Device_Run);
      Saved_Startup.Error_Count := Driver.Captured_Errors (Device_Run);
      Saved_Startup.Error_Omitted := Driver.Omitted_Errors (Device_Run);
      Saved_Startup.Work_Limit := Driver.Work_Limit(Device_Run);
      Saved_Startup.Initializers := ACPI_Native_Initializer_History.Observe;
      Saved_Startup.Consumer_Preparation := ACPI_Native_Pre_Device.Observe;
      ACPI_Native_Regions.Capture_If_Unvisited;
      if Owner /= null and then ACPI_Requests.Valid(Owner.all) then
         Metadata.Advance(Owner.all,Saved_Startup.Metadata_Accepted);
      end if;
   end Capture_Startup_Diagnostic;
   package Phase_Trace renames ACPI_Startup_Phase_Trace;
   Post_Device_Trace : Phase_Trace.State;
   subtype Node_Log_Attempt_Count is Natural range 0 .. Natural (Driver.Error_Position'Last) + 1;
   Node_Log_Attempts : Node_Log_Attempt_Count := 0;
   Node_Log_Loss_Before, Node_Log_Loss_After : Unsigned_64 := 0;
   Node_Log_Drain_Attempted, Node_Log_Drained : Boolean := False;
   function Power_Phase_Result (Value : ACPI_Native_Power_Detection.Outcome) return String is
     (case Value is
        when ACPI_Native_Power_Detection.Not_Attempted => "NOT_ATTEMPTED",
        when ACPI_Native_Power_Detection.Not_Ready => "NOT_READY",
        when ACPI_Native_Power_Detection.No_Devices => "NO_DEVICES",
        when ACPI_Native_Power_Detection.Finished => "FINISHED",
        when ACPI_Native_Power_Detection.Budget_Exhausted => "BUDGET_EXHAUSTED",
        when ACPI_Native_Power_Detection.Lid_Failed => "LID_FAILED",
        when ACPI_Native_Power_Detection.Source_Failed => "SOURCE_FAILED",
        when ACPI_Native_Power_Detection.Battery_Failed => "BATTERY_FAILED",
        when ACPI_Native_Power_Detection.Battery_Search_Failed => "BATTERY_SEARCH_FAILED",
        when ACPI_Native_Power_Detection.Battery_Limit => "BATTERY_LIMIT",
        when ACPI_Native_Power_Detection.Recovery_Required => "RECOVERY_REQUIRED",
        when ACPI_Native_Power_Detection.Owner_Error => "OWNER_ERROR");
   function Runtime_Phase_Result (Value : One.Runtime.Status.Outcome) return String is
     (case Value is
        when One.Runtime.Status.Not_Attempted => "NOT_ATTEMPTED",
        when One.Runtime.Status.Rejected => "REJECTED",
        when One.Runtime.Status.Owner_Refused => "OWNER_REFUSED",
        when One.Runtime.Status.Runtime_Entered => "RUNTIME_ENTERED",
        when One.Runtime.Status.Event_Opened => "EVENT_OPENED",
        when One.Runtime.Status.Event_Completed => "EVENT_COMPLETED",
        when One.Runtime.Status.Delay_Refused => "DELAY_REFUSED",
        when One.Runtime.Status.Evaluation_Failed => "EVALUATION_FAILED",
        when One.Runtime.Status.Retained_Results_Pending => "RETAINED_RESULTS_PENDING",
        when One.Runtime.Status.IO_Pending => "IO_PENDING",
        when One.Runtime.Status.Stop_Pending => "STOP_PENDING",
        when One.Runtime.Status.Stopped_By_Owner => "STOPPED_BY_OWNER",
        when One.Runtime.Status.Already_Stopped => "ALREADY_STOPPED");
   function SCI_Phase_Result (Value : ACPI_SCI_Events.Arm_Outcome) return String is
     (case Value is
        when ACPI_SCI_Events.Armed => "ARMED",
        when ACPI_SCI_Events.No_SCI_Line => "NO_SCI_LINE",
        when ACPI_SCI_Events.FADT_Unreadable => "FADT_UNREADABLE",
        when ACPI_SCI_Events.FADT_Malformed => "FADT_MALFORMED",
        when ACPI_SCI_Events.Layout_Refused => "LAYOUT_REFUSED",
        when ACPI_SCI_Events.Register_Outside_Grant => "REGISTER_OUTSIDE_GRANT",
        when ACPI_SCI_Events.ACPI_Mode_Port_Outside_Grant => "ACPI_MODE_PORT_OUTSIDE_GRANT",
        when ACPI_SCI_Events.ACPI_Mode_Not_Entered => "ACPI_MODE_NOT_ENTERED",
        when ACPI_SCI_Events.Enable_Refused => "ENABLE_REFUSED");
   function Events_Phase_Result (Value : ACPI_Native_Event_Domain.Start_Outcome) return String is
     (case Value is
        when ACPI_Native_Event_Domain.Ready => "READY",
        when ACPI_Native_Event_Domain.EC_Unavailable => "EC_UNAVAILABLE",
        when ACPI_Native_Event_Domain.SCI_Unavailable => "SCI_UNAVAILABLE",
        when ACPI_Native_Event_Domain.Refused => "REFUSED");
   function Completion_Phase_Result (Value : Consumer_Completion.Outcome) return String is
     (case Value is
        when Consumer_Completion.Not_Attempted => "NOT_ATTEMPTED",
        when Consumer_Completion.Boundary_Refused => "BOUNDARY_REFUSED",
        when Consumer_Completion.Request_Refused => "REQUEST_REFUSED",
        when Consumer_Completion.Endpoint_Refused => "ENDPOINT_REFUSED",
        when Consumer_Completion.Clock_Refused => "CLOCK_REFUSED",
        when Consumer_Completion.Completion_Unknown => "COMPLETION_UNKNOWN",
        when Consumer_Completion.Invalid_Reply => "INVALID_REPLY",
        when Consumer_Completion.Receipt_Confirmed => "RECEIPT_CONFIRMED");
   function Serve_Phase_Result (Value : ACPI_Native_Server.Stop_Reason) return String is
     (case Value is
        when ACPI_Native_Server.Wait_Unavailable => "WAIT_UNAVAILABLE",
        when ACPI_Native_Server.Unexpected_Completion => "UNEXPECTED_COMPLETION",
        when ACPI_Native_Server.Stop_Requested => "STOP_REQUESTED",
        when ACPI_Native_Server.Owner_Work_Refused => "OWNER_WORK_REFUSED",
        when ACPI_Native_Server.Owner_Maintenance_Stopped => "OWNER_MAINTENANCE_STOPPED");
   procedure Enter_Phase (Phase : Phase_Trace.Measured_Phase) is
   begin
      Phase_Trace.Enter (Post_Device_Trace, Phase);
      if not ACPI_Native_Power_Entry.Blocks_Runtime then
         ACPI_Native_Log.Info ("acpi: phase " & Phase_Trace.Name (Phase) & " ENTRY");
      end if;
   end Enter_Phase;
   procedure Leave_Phase (Phase : Phase_Trace.Measured_Phase; Outcome : String;
      Succeeded : Boolean) is
      View : Phase_Trace.Checkpoint;
   begin
      Phase_Trace.Leave (Post_Device_Trace, Phase, Outcome, Succeeded);
      View := Phase_Trace.Observe (Post_Device_Trace);
      if not ACPI_Native_Power_Entry.Blocks_Runtime then
         ACPI_Native_Log.Info ("acpi: phase " & Phase_Trace.Name (Phase) & " EXIT result=" &
           Phase_Trace.Text (View.Result) & " truncated=" & Boolean'Image (View.Result.Truncated));
      end if;
   end Leave_Phase;
   procedure Report_Phase_Checkpoint is
      View : constant Phase_Trace.Checkpoint := Phase_Trace.Observe (Post_Device_Trace);
   begin
      ACPI_Native_Log.Warning
        (Phase_Trace.Image (View, ACPI_Native_Log.Publisher_Dropped));
      ACPI_Native_Log.Warning ("acpi: node-log-checkpoint expected=" &
        Natural'Image (Natural (Saved_Startup.Error_Count) + Boolean'Pos (Saved_Startup.Devices.Errors /= 0)) &
        " attempted=" & Node_Log_Attempts'Image &
        " loss-before=" & Node_Log_Loss_Before'Image & " loss-after=" & Node_Log_Loss_After'Image &
        " drain-attempted=" & Boolean'Image (Node_Log_Drain_Attempted) &
        " drained=" & Boolean'Image (Node_Log_Drained));
   end Report_Phase_Checkpoint;
   Event_Start_Captured : Boolean := False;
   Event_Start_View : ACPI_Native_Event_Domain.Report;
   EC_Start_View : ACPI_Native_EC_Discovery.Report;
   Event_Start_Result : ACPI_Native_Event_Domain.Start_Outcome := ACPI_Native_Event_Domain.Refused;
   procedure Report_Event_Start_Checkpoint is
      function Bit (Value : Boolean) return String is (if Value then "1" else "0");
      function EC_Path_Image(Text:String) return String is
      begin
         for I in Text'Range loop if Text(I)/=' ' then return Text(I..Text'Last);end if;end loop;
         return "UNKNOWN";
      end EC_Path_Image;
      Path:constant String:=EC_Path_Image(EC_Start_View.Device_Path);
      Candidate_Path:constant String:=EC_Path_Image(EC_Start_View.Candidate_Path);
      Maximum_Path_Preview:constant Positive:=100;
   begin
      if not Event_Start_Captured then return; end if;
      -- Scalars copied immediately after Start, before drain/retirement can
      -- change the live domain. No Core inspection or new channel at a busy
      -- owner boundary. Preserve the original startup failure in the tail.
      ACPI_Native_Diagnostic_Output.Write
        ("acpi: stage event-start-checkpoint result=" & Event_Start_Result'Image &
         " stage=" & Event_Start_View.Stage'Image & " reason=" & Event_Start_View.Why'Image &
         " ec=" & Event_Start_View.EC'Image &
         " consumers=" & Bit (Event_Start_View.Consumers_Started) &
         " armed=" & Bit (Event_Start_View.EC_Armed) &
         " general=" & Event_Start_View.General_GPEs'Image,
         (ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime));
      -- Short records fit image22's single-result log window as well as a photo.
      ACPI_Native_Diagnostic_Output.Write
        ("acpi: stage ec-selector status="&EC_Start_View.Status'Image&
         " check="&EC_Start_View.At_Checkpoint'Image&" node="&EC_Start_View.At_Node'Image&
         " hid="&EC_Start_View.Identity_Node'Image&" roots="&EC_Start_View.Retained'Image&
         " candidates="&EC_Start_View.Candidates'Image&" role=SCAN",False);
      ACPI_Native_Diagnostic_Output.Write
        ("acpi: stage ec-identity access="&EC_Start_View.Identity_Access'Image&
         " exec="&EC_Start_View.Identity_Execution'Image&" cleanup="&EC_Start_View.Identity_Cleanup'Image&
         " kind="&(if EC_Start_View.Identity_Kind_Known then EC_Start_View.Identity_Kind'Image else "UNKNOWN"),False);
      ACPI_Native_Diagnostic_Output.Write
        ("acpi: stage ec-driver status="&EC_Start_View.Failure.Status'Image&
         " member="&EC_Start_View.Failure.At_Member'Image&
         " uncertain="&EC_Start_View.Failure.Field_Uncertainty'Image,False);
      ACPI_Native_Diagnostic_Output.Write
        ("acpi: stage ec-path complete="&EC_Start_View.Path_Complete'Image&
         " clipped="&Boolean'Image(Path'Length>Maximum_Path_Preview)&" role=SCAN"&
         " path="&Path(Path'First..Path'First+Natural'Min(Path'Length,Maximum_Path_Preview)-1),False);
      ACPI_Native_Diagnostic_Output.Write
        ("acpi: stage ec-candidate described="&Bit(EC_Start_View.Candidate_Described)&
         " ready="&Bit(EC_Start_View.Status=ACPI_Native_EC_Discovery.Ready)&
         " armed="&Bit(Event_Start_View.EC_Armed)&" node="&EC_Start_View.Candidate_Node'Image,False);
      if EC_Start_View.Candidate_Described then
         ACPI_Native_Diagnostic_Output.Write
           ("acpi: stage ec-candidate-path complete="&EC_Start_View.Candidate_Path_Complete'Image&
            " clipped="&Boolean'Image(Candidate_Path'Length>Maximum_Path_Preview)&
            " path="&Candidate_Path(Candidate_Path'First..Candidate_Path'First+
              Natural'Min(Candidate_Path'Length,Maximum_Path_Preview)-1),False);
      end if;
   end Report_Event_Start_Checkpoint;
   procedure Park_With_Final_Checkpoint is
      View : constant Phase_Trace.Checkpoint := Phase_Trace.Observe (Post_Device_Trace);
   begin
      Report_Event_Start_Checkpoint;
      -- Last line before park: existing copied writer, no channel opening at
      -- an unsafe owner boundary. No logstore/transport changes or retry.
      ACPI_Native_Diagnostic_Output.Write
        (Phase_Trace.Image (View, ACPI_Native_Log.Publisher_Dropped),
         (ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime));
      One.Park;
   end Park_With_Final_Checkpoint;
   procedure Report_Early_Node_Errors is
   begin
      if Node_Errors_Emitted or else Driver.Observe (Device_Run).Errors = 0 then return; end if;
      Capture_Startup_Diagnostic;
      Node_Errors_Emitted := True;
      if (ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime) then
         Node_Log_Drain_Attempted := True;
         ACPI_Native_Log.Drain_Diagnostics
           (Root.Fields.Observe.IO_Pending, Root.Fields.Uncertain.Present, Node_Log_Drained);
      end if;
      Node_Log_Loss_Before := ACPI_Native_Log.Publisher_Dropped;
      for I in Driver.Error_Position loop
         exit when Natural (I) > Natural (Saved_Startup.Error_Count);
         declare E : constant Copied_Node_Error := Saved_Startup.Errors (I);
         begin
            Node_Log_Attempts := Node_Log_Attempts + 1;
            ACPI_Native_Log.Warning
              ("acpi: stage node-init fail " & ACPI_Startup_Diagnostics.Execution_Name (E.Facts.Failure) &
               " node=" & E.Facts.Scope'Image & " path=" & HID_Error_Path_Image (E.Path) &
               " path-complete=" & Boolean'Image (E.Path_Complete) & " method=" & E.Facts.Member &
               " method-node=" & E.Facts.Method'Image & " fuel=" & E.Facts.Reported_Charge'Image);
         end;
      end loop;
      Node_Log_Attempts := Node_Log_Attempts + 1;
      ACPI_Native_Log.Warning
        ("acpi: stage node-init-summary errors=" & Saved_Startup.Devices.Errors'Image &
         " captured=" & Saved_Startup.Error_Count'Image & " omitted=" & Saved_Startup.Error_Omitted'Image);
      Node_Log_Loss_After := ACPI_Native_Log.Publisher_Dropped;
   end Report_Early_Node_Errors;
   procedure Report_Startup_Diagnostic
      (Point : ACPI_Native_Regions.Emission_Point := ACPI_Native_Regions.After_Cleanup) is
      use type Driver.Phase;
      function Bit (Value : Boolean) return String is (if Value then "1" else "0");
      function Path_Image (Text : String) return String is
      begin
         for I in Text'Range loop
            if Text (I) /= ' ' then return Text (I .. Text'Last); end if;
         end loop;
         return "UNKNOWN";
      end Path_Image;
      Line : constant String := "acpi: stage startup-state " &
         (if Saved_Startup.Devices.Stage in Driver.Completed | Driver.Completed_With_Errors then "ok " else "fail ") &
         Saved_Startup.Devices.Stage'Image &
         " captured=" & Bit(Saved_Startup.Captured) & " meta=" & Bit(Saved_Startup.Metadata_Accepted) &
         " core=" & Bit(Saved_Startup.Core_Observed) & " import=" & Saved_Startup.Import_State'Image &
         " namespace=" & Saved_Startup.Namespace_State'Image & " nodes=" & Saved_Startup.Nodes'Image &
         " members=" & Saved_Startup.Pending_Members'Image & " fuel=" & Saved_Startup.Devices.Charged'Image &
         " budget=" & Saved_Startup.Work_Limit'Image &
         " sta=" & Saved_Startup.Devices.Status_Methods'Image & " ini=" & Saved_Startup.Devices.Initialization_Methods'Image &
         " errors=" & Saved_Startup.Devices.Errors'Image & " at=" & Point'Image;
   begin
      ACPI_Native_Regions.Report_Copied_Diagnostics(Point);
      ACPI_Native_Diagnostic_Output.Write(Line,(ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime),
         Saved_Startup.Devices.Stage=Driver.Completed);
      -- Emit detached facts only at the established quiet checkpoint.
      if Saved_Startup.Devices.Errors /= 0 and then not Node_Errors_Emitted then
         Node_Errors_Emitted := True;
         for I in Driver.Error_Position loop
            exit when Natural (I) > Natural (Saved_Startup.Error_Count);
            declare E : constant Copied_Node_Error := Saved_Startup.Errors (I);
            begin
               ACPI_Native_Diagnostic_Output.Write
                 ("acpi: stage node-init fail " & ACPI_Startup_Diagnostics.Execution_Name (E.Facts.Failure) &
                  " node=" & E.Facts.Scope'Image & " path=" & Path_Image (E.Path) &
                  " path-complete=" & Bit (E.Path_Complete) & " method=" & E.Facts.Member &
                  " method-node=" & E.Facts.Method'Image & " access=" & E.Facts.Access_Result'Image &
                  " execution=" & ACPI_Startup_Diagnostics.Execution_Name (E.Facts.Evaluation) &
                  " fuel=" & E.Facts.Reported_Charge'Image,
                  (ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime));
            end;
         end loop;
         ACPI_Native_Diagnostic_Output.Write
           ("acpi: stage node-init-summary errors=" & Saved_Startup.Devices.Errors'Image &
            " captured=" & Saved_Startup.Error_Count'Image & " omitted=" & Saved_Startup.Error_Omitted'Image,
            (ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime));
      end if;
      ACPI_Native_Diagnostic_Output.Write
        ("acpi: stage initializer-history invocations=" & Saved_Startup.Initializers.Invocations'Image &
         " ordered-digest=0x" & Hex(Saved_Startup.Initializers.Ordered_Digest) &
         " encoding=" & ACPI_Native_Initializer_History.Encoding.Encoding_Version'Image &
         " trace-complete=" & Bit(Saved_Startup.Initializers.Complete) & " at=" & Point'Image,
         (ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime));
      ACPI_Native_Diagnostic_Output.Write
        ("acpi: stage initializer-last seen=" & Bit(Saved_Startup.Initializers.Seen) &
         " scope=" & Saved_Startup.Initializers.Last_Scope'Image &
         " method=" & Saved_Startup.Initializers.Last_Method'Image &
         " path=" & Saved_Startup.Initializers.Last_Path &
         " path-complete=" & Bit(Saved_Startup.Initializers.Path_Complete) &
         " access=" & Saved_Startup.Initializers.Access_Result'Image &
         " execution=" & Saved_Startup.Initializers.Execution'Image &
         " invoke-charged=" & Saved_Startup.Initializers.Reported_Charge'Image,
         (ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime));
      ACPI_Native_Diagnostic_Output.Write
        ("acpi: stage gpio-admission phase=" & Saved_Startup.Consumer_Preparation.Stage'Image &
         " why=" & Saved_Startup.Consumer_Preparation.Why'Image &
         " attempted=" & Bit(Saved_Startup.Devices.Consumer_Preparation_Attempted) &
         " prepared=" & Bit(Saved_Startup.Devices.Consumers_Prepared) &
         " initializers=" & Saved_Startup.Consumer_Preparation.Initializer_Invocations'Image &
         " history=0x" & Hex(Saved_Startup.Consumer_Preparation.Initializer_Ordered_Digest) &
         " complete=" & Bit(Saved_Startup.Consumer_Preparation.Initializer_History_Complete) &
         " known=" & Bit(Saved_Startup.Consumer_Preparation.Admission_Known) &
         " answer=" & Saved_Startup.Consumer_Preparation.Answer'Image &
         " grant-pending=" & Bit(Saved_Startup.Consumer_Preparation.Grant_Pending),
         (ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime));
   end Report_Startup_Diagnostic;
   Started : Boolean := False;
   Stop_Served, Retired_Quiescent : Boolean := False;
   function Stopped_And_Retired return Boolean is (Stop_Served and then Retired_Quiescent);
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
      Runtime_Entry : One.Runtime.Status.Outcome;
      Observer_Only, Mode_Selected : Boolean := False;
      Runtime_Serve_Started : Boolean := False;
      Core_View : One.Core_Retirement_State := One.Unobserved;
      SCI_View : One.SCI_Retirement_State := One.Not_Armed;
      Events_Quiet:Boolean:=True;
      Completion_Target : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Completion_Deadline : Unsigned_64 := 0;
      Original_Startup_Deadline : Unsigned_64 := 0;
      use type One.SCI_Retirement_State;
      use type ACPI_Native_Event_Domain.Drain_Outcome;
      procedure Publish_Bridges_Before_Events (Completed : out Boolean) is
         Until_Ms : Unsigned_64 := 0;
         Routes_Ready, Publication_Complete : Boolean := False;
         Publication_Reason : ACPI_Native_Server.Stop_Reason;
         Stopped_At : Unsigned_64;
         procedure Advance_Publication (Server : in out ACPI_Requests.State;
            Outcome : out ACPI_Native_Server.Owner_Outcome) is
         begin
            -- Metadata enumeration and an open query window both belong to
            -- Bridges. Receipt fanout changes the broker revision: wait for
            -- that owner's boundary before draining captured notifications.
            if not Bridges.Blocks_AML then
               ACPI_Native_Event_Domain.Route_Prepared (Server, Outcome);
               -- Preserve runnable notification batches. The bridge cannot
               -- start until the captured queue is drained, so sleeping here
               -- could consume the whole original publication allowance.
               if Outcome /= ACPI_Native_Server.Owner_Idle then return; end if;
            end if;
            Bridge_Loop.Advance_Publication (Server, Outcome);
            if Bridges.Observe.Stage in HB.Holding | HB.Finished
              and then not Bridges.Blocks_AML then
               Publication_Complete := True;
               -- Local loop completion, not a refusal of the native runtime.
               Outcome := ACPI_Native_Server.Owner_Refused;
            end if;
         end Advance_Publication;
         procedure Maintain_Publication (Deadline : out Unsigned_64; May_Continue : out Boolean) is
            Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
         begin
            Deadline := Until_Ms;
            May_Continue := Now /= CuBit.Messages.Wait_Forever and then Now < Until_Ms;
            if May_Continue then
               Bridge_Loop.Maintain (Deadline, May_Continue);
               Deadline := Unsigned_64'Min (Deadline, Until_Ms);
            end if;
         end Maintain_Publication;
         procedure Publication_Dispatch (Server : in out ACPI_Requests.State;
            From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
            Reply : out CuBit.Messages.Message) is
         begin
            ACPI_Native_Endpoint.Dispatch_With_Admission
              (Server, From, Request, Reply, False, Cached_Button_Watches_Open => True);
         end Publication_Dispatch;
         function Never_Diagnose (Server : ACPI_Requests.State) return Boolean is
            pragma Unreferenced (Server);
         begin return False; end Never_Diagnose;
         package Publication_Loop is new ACPI_Native_Server.With_Dispatch
           (Publication_Dispatch, Serving_Event, Advance_Publication,
            Maintain_Publication, Never_Diagnose, ACPI_Native_Endpoint.Reply_Now,
            Bridge_Loop.Handle_Completion);
      begin
         Completed := False;
         ACPI_Native_Event_Domain.Prepare_Consumers (Owner.all, Routes_Ready);
         if not Routes_Ready then return; end if;
         Until_Ms := CuBit.Monotonic.Milliseconds;
         if Until_Ms = CuBit.Messages.Wait_Forever
           or else Until_Ms >= CuBit.Messages.Wait_Forever - Bridge_Query_Milliseconds then return; end if;
         Until_Ms := Until_Ms + Bridge_Query_Milliseconds;
         Publication_Loop.Serve (Owner.all, Publication_Reason);
         Completed := Publication_Complete
           and then Publication_Reason = ACPI_Native_Server.Owner_Work_Refused;
         if not Completed then
            if Publication_Reason = ACPI_Native_Server.Stop_Requested then
               Reason := Publication_Reason;
               Stop_Served := True;
               return;
            end if;
            Stopped_At := CuBit.Monotonic.Milliseconds;
            Bridges.Suspend_Discovery
              ((if Stopped_At = CuBit.Messages.Wait_Forever then HB.Clock_Failed
                elsif Stopped_At >= Until_Ms then HB.Deadline_Expired
                else HB.Window_Refused));
         end if;
         ACPI_Native_Diagnostic_Output.Write
           ("acpi: stage host-bridge-before-events " & (if Completed then "ok " else "degraded ") &
            (if Completed then "BOUNDARY_CLOSED" else "PUBLICATION_SUSPENDED") &
            " stage=" & Bridges.Observe.Stage'Image &
            " why=" & Bridges.Observe.Why'Image &
            " loop=" & Publication_Reason'Image &
            " aml-held=" & (if Bridges.Blocks_AML then "1" else "0") &
            " acknowledged=" & Bridges.Observe.Acknowledged'Image &
            " held=" & Bridges.Observe.Held_Sessions'Image, False);
      end Publish_Bridges_Before_Events;
      procedure Serve_Preserved_Bridges is
         procedure Preserved_Dispatch (Server : in out ACPI_Requests.State;
            From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
            Reply : out CuBit.Messages.Message) is
         begin
            ACPI_Native_Endpoint.Dispatch_With_Admission
              (Server, From, Request, Reply, False, Cached_Button_Watches_Open => True, Cached_Shutdown_Open => True);
         end Preserved_Dispatch;
         procedure Preserved_Event (Publisher : CuBit.Messages.Process_ID;
            Item : CuBit.Messages.Message; Stop, Handled : out Boolean) is
            Ends : Boolean := False;
            Ack : ACPI_SCI_Quiesce.Acknowledgement;
            use type ACPI_System_Events.Action;
         begin
            if ACPI_System_Events.Classify (Publisher, Item) = ACPI_System_Events.Grant_Notice then
               Bridges.Note_Event (CuBit.Control_Events.Decode (Item.tag.label, Item.tag.length,
                 Item.words (0), Item.words (1), Item.words (2)), Ends);
            end if;
            ACPI_Native_Events.Handle (Publisher, Item, Stop, Handled);
            ACPI_SCI_Events.Acknowledge_Only (Publisher, Item, Ack);
            Stop := Stop or Ends;
         end Preserved_Event;
         function Never_Diagnose (Server : ACPI_Requests.State) return Boolean is
            pragma Unreferenced (Server);
         begin return False; end Never_Diagnose;
         procedure Advance_Shutdown (Server : ACPI_Requests.State; Did_Work : out Boolean) is
            Inventory_Work,Read_Work : Boolean;
            Outcome : ACPI_Native_Power_Entry.Step_Outcome;
            use type ACPI_Native_Power_Entry.Step_Outcome;
         begin
            ACPI_Native_Power_Entry.Advance (Server, Outcome);
            ACPI_Native_Battery_Reads.Advance(Server,ACPI_Native_Event_Domain.Diagnostics_Quiet(Server),
        False,Read_Work);
      ACPI_Native_Power_Inventory.Advance(ACPI_Native_Event_Domain.Diagnostics_Quiet(Server),Inventory_Work);
            Did_Work := Read_Work or else Inventory_Work or else ACPI_Native_Battery_Reads.Pending or else ACPI_Native_Power_Inventory.Pending or else Outcome /= ACPI_Native_Power_Entry.Idle;
         end Advance_Shutdown;
         procedure Preserved_Idle (Server : in out ACPI_Requests.State; Outcome : out ACPI_Native_Server.Owner_Outcome) is
         begin
            Outcome:=(if (ACPI_Native_Power_Inventory.Pending or ACPI_Native_Battery_Reads.Pending) and then ACPI_Native_Event_Domain.Diagnostics_Quiet(Server)
              then ACPI_Native_Server.Owner_Pending else ACPI_Native_Server.Owner_Idle);
         end Preserved_Idle;
         package Preserved_Power is new ACPI_Native_Server.With_Power_Entry
           (Advance_Shutdown, ACPI_Native_Power_Entry.Runnable, ACPI_Native_Power_Entry.Blocks_Runtime,
            ACPI_Native_Power_Entry.Wake_Deadline_Milliseconds, Preserved_Idle,
            ACPI_Native_Server.No_Maintenance, Never_Diagnose);
         -- Only final sleep-entry advancement; no normal/event/passive AML.
         -- Already acknowledged original producer sessions stay owned until
         -- a real Stop/revocation. Unknown retained owners are not retried.
         package Preserved_Loop is new ACPI_Native_Server.With_Dispatch
           (Preserved_Dispatch, Preserved_Event, Preserved_Power.Advance,
            Preserved_Power.Maintain, Never_Diagnose,
            ACPI_Native_Endpoint.Reply_Now, Bridge_Loop.Handle_Completion);
      begin
         ACPI_Native_Diagnostic_Output.Write
           ("acpi: stage event-quarantine fail CACHED_ONLY acknowledged=" &
            Bridges.Observe.Acknowledged'Image & " held=" & Bridges.Observe.Held_Sessions'Image, False);
         Preserved_Loop.Serve (Owner.all, Reason);
      end Serve_Preserved_Bridges;
      procedure Drain_Bridges is
         Until_Ms : Unsigned_64 := 0;
         Drain_Reason : ACPI_Native_Server.Stop_Reason;
         procedure Advance_Drain (Server : in out ACPI_Requests.State; Outcome : out ACPI_Native_Server.Owner_Outcome) is
         begin
            Bridge_Loop.Drain (Server, Outcome);
            if Bridges.Retired then Outcome := ACPI_Native_Server.Owner_Refused; end if;
         end Advance_Drain;
         procedure Maintain_Drain (Deadline : out Unsigned_64; May_Continue : out Boolean) is
            Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
         begin
            Deadline := Until_Ms; May_Continue := Now /= CuBit.Messages.Wait_Forever and then Now < Until_Ms;
            if May_Continue then
               Bridge_Loop.Maintain (Deadline, May_Continue); Deadline := Unsigned_64'Min (Deadline, Until_Ms);
            end if;
         end Maintain_Drain;
         procedure Drain_Event (Publisher : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message; Stop, Handled : out Boolean) is
            Ends, Ignored : Boolean; Ack : ACPI_SCI_Quiesce.Acknowledgement;
            use type ACPI_System_Events.Action;
         begin
            if ACPI_System_Events.Classify (Publisher, Item) = ACPI_System_Events.Grant_Notice then
               Bridges.Note_Event (CuBit.Control_Events.Decode (Item.tag.label, Item.tag.length,
                 Item.words (0), Item.words (1), Item.words (2)), Ends);
            end if;
            ACPI_Native_Events.Handle (Publisher, Item, Ignored, Handled);
            ACPI_SCI_Events.Acknowledge_Only (Publisher, Item, Ack);
            Stop := False;
         end Drain_Event;
         procedure Drain_Dispatch (Server : in out ACPI_Requests.State; From : CuBit.Messages.Process_ID;
            Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message) is
         begin ACPI_Native_Endpoint.Dispatch_With_Admission (Server, From, Request, Reply, False); end Drain_Dispatch;
         function Never_Diagnose (Server : ACPI_Requests.State) return Boolean is
            pragma Unreferenced (Server);
         begin return False; end Never_Diagnose;
         package Drain_Loop is new ACPI_Native_Server.With_Dispatch
           (Drain_Dispatch, Drain_Event, Advance_Drain, Maintain_Drain, Never_Diagnose,
            ACPI_Native_Endpoint.Reply_Now, Bridge_Loop.Handle_Completion);
      begin
         Bridges.Request_Stop;
         if Bridges.Retired then return; end if;
         Until_Ms := CuBit.Monotonic.Milliseconds;
         if Until_Ms = CuBit.Messages.Wait_Forever or else Until_Ms >= CuBit.Messages.Wait_Forever - Bridge_Stop_Milliseconds then return; end if;
         Until_Ms := Until_Ms + Bridge_Stop_Milliseconds;
         Drain_Loop.Serve (Owner.all, Drain_Reason);
      end Drain_Bridges;
      procedure Close_SCI is
         use type ACPI_SCI_Quiesce.Outcome;
         Result:ACPI_SCI_Quiesce.Result;
      begin
         if SCI_View in One.Not_Armed | One.Clear_SCI | One.Retained_SCI then return;end if;
         for Attempt in 1..SCI_Disarm_Attempts loop
            ACPI_SCI_Events.Disarm_At_Boundary (Result, Owner_Quiet);
            exit when Result.State=ACPI_SCI_Quiesce.Quiesced;
         end loop;
         ACPI_Native_Event_Domain.Confirm_Disarm(Result);
         SCI_View:=(if Result.State=ACPI_SCI_Quiesce.Quiesced then One.Clear_SCI else One.Retained_SCI);
      end Close_SCI;
      procedure Drain_Events(Final:Boolean;Completed:out Boolean) is
         Result:ACPI_Native_Event_Domain.Drain_Outcome;
      begin
         Completed:=not ACPI_Native_Event_Domain.Attempted;
         if Owner=null or else not ACPI_Requests.Valid(Owner.all) then return;end if;
         for Turn in 1..ACPI_Native_Event_Domain.Shutdown_Turns loop
            if Final then ACPI_Native_Event_Domain.Close(Owner.all,Result);
            else ACPI_Native_Event_Domain.Drain(Owner.all,Result);end if;
            if Result=ACPI_Native_Event_Domain.Retained then Completed:=False;return;
            elsif Result=ACPI_Native_Event_Domain.Drained then
               Completed:=(if Final then ACPI_Native_Event_Domain.Quiet(Owner.all) else True);return;
            end if;
         end loop;
         Completed:=False;
      end Drain_Events;
      Detection : ACPI_Native_HID_Detection.Report;
      Power : ACPI_Native_Power_Detection.Report;
      Delay_Accepted, Configured, Finished, Retired : Boolean;
      Devices : ACPI_Bootstrap.Device_Report;
      Root_Timing, Provider_Timing, Load_Timing : ACPI_Native_Startup_Timing.Token;
      Timing_Finished : Boolean := False;
      procedure Close_Regions is
         Opened,Allowed_Window,Completed : Boolean := False;
         Until_Time,Now : Unsigned_64;
         Admission : Region_Cleanup.Controller.Admission;
         Window_Result : One.Runtime.Status.Outcome;
      begin
         if Owner=null or else not ACPI_Requests.Valid(Owner.all) then return;end if;
         Pre_Device_Cleanup.Advance(Owner.all,Completed);
         if ACPI_Native_Regions.Quiet then return;end if;
         if One.Runtime.Current=One.Runtime.Status.Idle then
            ACPI_Clock_Source.Read(Now,Allowed_Window);
            if not Allowed_Window or else Now>Unsigned_64'Last-Region_Cleanup_Window_Microseconds then return;end if;
            Until_Time:=Now+Region_Cleanup_Window_Microseconds;
            One.Runtime_Access.Begin_Event(Owner.all,Until_Time,Window_Result);
            Opened:=Window_Result=One.Runtime.Status.Event_Opened;Allowed_Window:=Opened;
         elsif One.Runtime.Current=One.Runtime.Status.Fresh then
            -- Startup failure gets only the remaining original startup window;
            -- cleanup cannot replenish an expired/failed provider deadline.
            ACPI_Native_Delays.Inspect_Window(Until_Time,Allowed_Window);
         end if;
         if not Allowed_Window then return;end if;
         Region_Cleanup.Advance(Owner.all,Admission,Completed);
         if Opened then One.Runtime_Access.End_Event(Owner.all,Completed,Window_Result);end if;
      end Close_Regions;
      procedure Finish_Timing is
      begin
         if Timing_Finished then return; end if;
         Timing_Finished := True;
         ACPI_Native_Startup_Timing.Leave (Root_Timing);
         ACPI_Native_Startup_Timing.Report;
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
      -- Hardware startup inputs, captured before the loader runs. Nothing is
      -- mapped here: the module callback initializes Hardware from the copied
      -- FADT once the tables are admitted (ACPI_One_Service, step 2).
      procedure Configure_Hardware (Accepted : out Boolean) is
         Grant : constant CuBit.ACPI_Windows.Grant := Startup.Hardware;
         Pool_Endpoint : constant CuBit.Handles.Startup.Optional_Endpoint := Startup.Hardware_Data;
         PCI_Endpoint : constant CuBit.Handles.Startup.Optional_Endpoint := Startup.Pci_Windows;
         Pool : Root.Backing.Pool_Configuration;
         PCI : Root.Backing.PCI_Configuration;
         Lock_Now : ACPI_Backend_Binding_Types.Tick;
         Lock_Clock : Boolean;
         Setup_Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
         use type ACPI_Backend_Binding_Types.Tick;
      begin
         Accepted := False;
         ACPI_Backend_Kernel_Handles.Read_Clock (Lock_Now, Lock_Clock);
         if not Lock_Clock
           or else Lock_Now >= ACPI_Backend_Binding_Types.No_Finite_Deadline
                                 - ACPI_Backend_Binding_Types.Tick (Startup_Timeout_Microseconds)
           or else Setup_Now >= CuBit.Messages.Wait_Forever - Startup_Timeout_Milliseconds
         then
            ACPI_Native_Log.Warning ("acpi: stage hardware-config fail CLOCK_UNAVAILABLE"); return;
         end if;
         if Grant.IO_Used = 0 and then Grant.Memory_Used = 0 then
            ACPI_Native_Log.Warning ("acpi: stage hardware-config fail EMPTY_GRANT"); return;
         end if;
         if Pool_Endpoint.Present then
            declare
               D : constant CuBit.Handles.IPC.Description :=
                 CuBit.Handles.IPC.Describe (Pool_Endpoint.Value);
            begin
               if D.Current then
                  -- devmgr answers the firmware-data endpoint; the pool owner
                  -- rechecks its tag and owner before the one request.
                  Pool := (Present => True, Endpoint => Pool_Endpoint.Value,
                           Expected_Owner => CuBit.Messages.From_Word (D.Object),
                           Request => Pool_Request_Nonce,
                           Deadline_Milliseconds => Setup_Now + Startup_Timeout_Milliseconds);
               end if;
            end;
         end if;
         if PCI_Endpoint.Present then
            declare
               D : constant CuBit.Handles.IPC.Description :=
                 CuBit.Handles.IPC.Describe (PCI_Endpoint.Value);
            begin
               if D.Current then
                  -- The epoch only names this process lifetime to the local
                  -- PCI window owner; the process incarnation is unique.
                  PCI := (Present => True, Endpoint => PCI_Endpoint.Value,
                          Expected_Owner => CuBit.Process_IDs.From_Word (D.Object),
                          Epoch => CuBit.Messages.To_Word (CuBit.Messages.Own_Process));
                  Bridge_Target := PCI.Endpoint; Bridge_Peer := PCI.Expected_Owner;
               end if;
            end;
         end if;
         One.Configure_Hardware
           (Grant, Unsigned_64 (Lock_Now) + Unsigned_64 (Startup_Timeout_Microseconds),
            Setup_Now + Startup_Timeout_Milliseconds, Pool, PCI, Accepted);
         if Accepted then
            ACPI_Native_Log.Info ("acpi: stage hardware-config ok memory=" & Natural'Image (Grant.Memory_Used) &
              " ports=" & Natural'Image (Grant.IO_Used) & " pool=" & (if Pool.Present then "1" else "0") &
              " pci=" & (if PCI.Present then "1" else "0"));
         else
            ACPI_Native_Log.Warning ("acpi: stage hardware-config fail ALREADY_CONFIGURED");
         end if;
      end Configure_Hardware;
      procedure Report_Local_Hardware is
         H : constant Root.Hardware.Report := Root.Hardware.Observe;
         B : constant Root.Backing.Report := Root.Backing.Observe;
         M : constant Root.Evaluation.Startup.Report := Root.Evaluation.Observe;
         F : constant Root.Fields.Field_Report := Root.Fields.Observe;
         use type Root.Hardware.Phase, Root.Backing.Reason, Root.Evaluation.Startup.Reason;
         function Bit (Value : Boolean) return String is (if Value then "1" else "0");
      begin
         ACPI_Native_Log.Info ("acpi: stage local-startup " &
           (if M.Status = Root.Evaluation.Startup.Modules_Completed then "ok " else "fail ") &
           M.Status'Image & " stage=" & M.Stage'Image & " fadt=" & M.FADT_Index'Image &
           " local=" & Bit (M.Local_Ready) & " module=" & Bit (M.Module_Attempted) &
           " raised=" & Bit (M.Initialization_Raised) & Bit (M.Execution_Raised) &
           " fields=" & F.Stage'Image & " fields-module=" & Bit (F.Module_Attempted) & Bit (F.Module_Completed) &
           " io-pending=" & Bit (F.IO_Pending) & " field-registers=" & F.Registers'Image);
         if H.Stage = Root.Hardware.Ready then
            ACPI_Native_Log.Info ("acpi: stage hardware ok registers=" & Natural'Image (H.Registers) &
              " nvs=" & Natural'Image (H.NVS_Records) & " lock=" & (if H.Lock_Ready then "1" else "0"));
         else
            ACPI_Native_Log.Warning ("acpi: stage hardware fail " & H.Status'Image);
         end if;
         -- The bridge closes its admission once module startup ends, so a
         -- completed bring-up reads Closed here; Status keeps the result.
         if B.Status = Root.Backing.Initialized then
            ACPI_Native_Log.Info ("acpi: stage memory-bridge ok pool=" & (if B.Pool_Acquired then "1" else "0") &
              " cache=" & (if B.Cache_Ready then "1" else "0") & " pci=" & (if B.PCI_Ready then "1" else "0"));
         else
            ACPI_Native_Log.Warning ("acpi: stage memory-bridge fail " & B.Status'Image);
         end if;
      end Report_Local_Hardware;
      function Enter_Observer_Only return Boolean is
         Regions : constant ACPI_Native_Regions.Report:=ACPI_Native_Regions.Observe;
         Fields : constant Root.Fields.Field_Report:=Root.Fields.Observe;
         Device_State : ACPI_Bootstrap.Device_Report;
         Facts : ACPI_Serving_State.Eligibility;
         Physical_Retired,Selected : Boolean;
         use type ACPI_Bootstrap.Device_Phase,ACPI_Device_Policy.Region_Outcome,ACPI_Requests.Phase;
      begin
         if Owner=null or else not ACPI_Requests.Valid(Owner.all) then return False;end if;
         Device_State:=ACPI_Requests.Device_Completion(Owner.all);
         Facts:=(Catalog_Complete=>ACPI_Requests.Current(Owner.all)=ACPI_Requests.Complete,
           Results_Clear=>not ACPI_Requests.Retained_Results_Pending(Owner.all),
           Region_Unavailable=>Device_State.Region_Status=ACPI_Device_Policy.Regions_Unavailable,
           Prefix_Terminal=>Device_State.Stage in ACPI_Bootstrap.Devices_Incomplete|ACPI_Bootstrap.Devices_Failed,
           No_Fatal=>not Device_State.Fatal.Present and then not Result.Startup.Fatal.Present,
           Regions_Unannounced=>not Regions.Announced,Regions_Clear=>not Regions.Pending,
           Regions_Idle=>not Regions.Working,Fields_Idle=>Root.Fields.Control_Idle,
           Fields_Clear=>not Fields.IO_Pending,Fields_Certain=>not Root.Fields.Uncertain.Present,
           EC_Quiet=>Root.Fields.EC_Commands.Quiescent,SCI_Not_Armed=>SCI_View=One.Not_Armed,
           Events_Not_Started=>not ACPI_Native_Event_Domain.Attempted,
           Consumer_Owner_Quiet=>ACPI_Native_Pre_Device.Quiet);
         if not ACPI_Serving_State.Eligible(Facts) then return False;end if;
         -- Retire THIS successor's physical backend, never the predecessor.
         -- Copied catalog storage belongs to Owner and is not freed here.
         One.Try_Retire(One.Clear_Core,One.Not_Armed,Physical_Retired);
         if not Physical_Retired or else not ACPI_Native_Pre_Device.Quiet
           or else not ACPI_Requests.Valid(Owner.all)
           or else ACPI_Requests.Current(Owner.all)/=ACPI_Requests.Complete
           or else ACPI_Requests.Retained_Results_Pending(Owner.all) then return False;end if;
         ACPI_Native_Endpoint.Select_Observer(Facts,Physical_Retired,Selected);
         if not Selected then return False;end if;
         Retired_Quiescent:=True;Observer_Only:=True;return True;
      end Enter_Observer_Only;
      Startup_Checkpoint_Emitted : Boolean := False;
      procedure Startup_Diagnostic_Checkpoint is
         Ignored_Drain_Status : Boolean;
         procedure Drain_If_Quiet is
         begin
            if (ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime) then
               -- Existing bounded diagnostic flush only. Registration already
               -- precedes this checkpoint on the serving path. No retry loop.
               ACPI_Native_Log.Drain_Diagnostics
                 (IO_Pending=>Root.Fields.Observe.IO_Pending,
                  IO_Uncertain=>Root.Fields.Uncertain.Present,
                  Drained=>Ignored_Drain_Status);
            end if;
         end Drain_If_Quiet;
      begin
         if Startup_Checkpoint_Emitted then return;end if;
         Startup_Checkpoint_Emitted:=True;
         -- Give the final copied phase journal room before the larger snapshot.
         Drain_If_Quiet;
         Report_Phase_Checkpoint;
         Report_Event_Start_Checkpoint;
         Drain_If_Quiet;
         Report_Startup_Diagnostic(ACPI_Native_Regions.Before_Cleanup);
         -- Give the two copied probe records room after the larger snapshot.
         -- Capture performed no IPC and retained its original startup deadline.
         Drain_If_Quiet;
         ACPI_Native_LPC_Probe.Report;
         Drain_If_Quiet;
      end Startup_Diagnostic_Checkpoint;
      procedure Attempt is
      begin
      ACPI_Native_Log.Info ("acpi: stage service-start ok one-service");
      Enter_Phase (Phase_Trace.Register_Service);
      Registration_Result := CuBit.Messages.registerDriver
        (CuBit.Messages.DRIVER_ACPI, Startup.Registration);
      Leave_Phase (Phase_Trace.Register_Service, "0x" & Hex (Registration_Result),
        Registration_Result /= Unsigned_64'Last);
      if Registration_Result = Unsigned_64'Last then
         ACPI_Native_Log.Warning ("acpi: stage registration fail REFUSED");
         ACPI_Native_Log.Warning ("acpi: bring-up reached startup (3/4)");
         return;
      end if;
      ACPI_Native_Log.Info ("acpi: stage registration ok STARTING requests=QUEUED");
      ACPI_Native_Firmware_Profile.Log_Selected;
      ACPI_Clock_Source.Read (Clock, Available);
      if not Available or else L.R.Tick (Clock) > L.R.Tick'Last - Startup_Timeout_Microseconds then
         ACPI_Native_Log.Warning ("acpi: stage snapshot fail CLOCK_UNAVAILABLE");
         ACPI_Native_Log.Warning ("acpi: bring-up reached service-start (1/4)");
         return;
      end if;
      -- CuBit's allocator logs and stops the process on native OOM; it does
      -- not raise Storage_Error. No partial owner can then enter this runner.
      Original_Startup_Deadline := Unsigned_64 (L.R.Tick (Clock) + Startup_Timeout_Microseconds);
      ACPI_Native_Delays.Begin_Startup (Original_Startup_Deadline, Delay_Accepted);
      if not Delay_Accepted then
         ACPI_Native_Log.Warning ("acpi: stage delay-budget fail ALREADY_CLOSED");
         return;
      end if;
      declare
         Data_Endpoint : constant CuBit.Handles.Startup.Optional_Endpoint := Startup.Hardware_Data;
         Target : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      begin
         if Data_Endpoint.Present then Target:=Data_Endpoint.Value;end if;
         Completion_Target := Target;
         if Data_Endpoint.Present then
            declare
               Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
            begin
               -- GETTIME's own epoch; never derive this from the snapshot's
               -- HPET microsecond deadline. Zero retains a refused budget.
               if Now < CuBit.Messages.Wait_Forever - Startup_Timeout_Milliseconds then
                  Completion_Deadline := Now + Startup_Timeout_Milliseconds;
               end if;
            end;
         end if;
         ACPI_Native_Pre_Device.Configure(Target,
           Unsigned_64(L.R.Tick(Clock)+Startup_Timeout_Microseconds),Configured);
         if not Configured then ACPI_Native_Log.Warning("acpi: stage pre-device fail ALREADY_CONFIGURED");return;end if;
      end;
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
      ACPI_Native_Startup_Timing.Enter (ACPI_Native_Startup_Timing.P.Provider_Init,Provider_Timing);
      Configure_Hardware (Configured);
      ACPI_Native_Startup_Timing.Leave (Provider_Timing);
      if not Configured then
         ACPI_Native_Log.Warning ("acpi: bring-up reached service-start (1/4)"); return;
      end if;
      -- Tables first: the loader admits and copies the catalog, then its
      -- module callback brings up Hardware, the memory bridge and Fields
      -- from the copied FADT before module AML (never the reverse).
      ACPI_Native_Startup_Timing.Enter (ACPI_Native_Startup_Timing.P.Snapshot_Load,Load_Timing);
      ACPI_Native_Log.Info ("acpi: stage snapshot-start ok");
      Loader.Load (Device_Run, L.R.Tick (Clock) + Startup_Timeout_Microseconds, Owner, Result);
      Capture_Startup_Diagnostic;
      -- Three bounded read-only local ECAM loads, without setup/peer IPC.
      -- Capture while the original startup window still exists; runtime
      -- transfer closes it. Publish only at the checkpoint after registration.
      ACPI_Native_LPC_Probe.Capture;
      ACPI_Native_Startup_Timing.Leave (Load_Timing);
      Report_Local_Hardware;
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
            " copy=" & Name (Result.Startup.Copy.Status) &
            " local=" & Root.Evaluation.Observe.Status'Image);
         if Result.Startup.Status = L.Startup_Refused then
            if Result.Startup.Completion.Execution.Modules.Field_Failure.Present then
               ACPI_Native_Log.Warning
                 (ACPI_Startup_Diagnostics.Format_Field_Failure (Result.Startup.Completion) &
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
      if ACPI_Startup_Diagnostics.Device_Initialization_Completed (Devices) and then Devices.Errors = 0 then
         ACPI_Native_Log.Info (ACPI_Startup_Diagnostics.Format (Devices));
      else
         ACPI_Native_Log.Warning (ACPI_Startup_Diagnostics.Format (Devices));
      end if;
      Enter_Phase (Phase_Trace.Early_Node_Errors);
      Report_Early_Node_Errors;
      Leave_Phase (Phase_Trace.Early_Node_Errors, "ATTEMPTED", True);
      Enter_Phase (Phase_Trace.HID_Discovery);
      Discover_HID (Owner.all, Detection);
      Leave_Phase (Phase_Trace.HID_Discovery, ACPI_Native_HID_Detection.Name (Detection.Status),
        Detection.Status in ACPI_Native_HID_Detection.No_Devices | ACPI_Native_HID_Detection.Finished
          and then not Detection.Recovery_Needed and then not Detection.Cleanup_Required);
      declare
         Ready : constant Boolean := Detection.Status in
           ACPI_Native_HID_Detection.No_Devices | ACPI_Native_HID_Detection.Finished
           and then not Detection.Recovery_Needed and then not Detection.Cleanup_Required;
         Line : constant String := "acpi: stage hid-return " & (if Ready then "ok " else "fail ") &
           ACPI_Native_HID_Detection.Name (Detection.Status) &
           " step=" & ACPI_Native_HID_Detection.Step_Name (Detection.Last_Step) &
           " exceptional=" & Boolean'Image (Detection.Exceptional_Exit) &
           " recovery=" & Boolean'Image (Detection.Recovery_Needed) &
           " pending=" & Boolean'Image (Detection.Cleanup_Required);
      begin
         if Ready then ACPI_Native_Log.Info (Line); else ACPI_Native_Log.Warning (Line); end if;
      end;
      Enter_Phase (Phase_Trace.HID_Path_Diagnostics);
      Report_HID_Path_Errors;
      Leave_Phase (Phase_Trace.HID_Path_Diagnostics, "RETURNED", True);
      Enter_Phase (Phase_Trace.HID_Module_Notes);
      ACPI_Native_Module_Notes.Observe (ACPI_Requests.Module_Errors (Owner.all));
      Leave_Phase (Phase_Trace.HID_Module_Notes, "RETURNED", True);
      if Detection.Cleanup_Required or else Detection.Recovery_Needed then
         ACPI_Native_Log.Warning ("acpi: stage startup-stop fail HID_DISCOVERY" &
           " reason=" & ACPI_Native_HID_Detection.Name (Detection.Status) &
           " step=" & ACPI_Native_HID_Detection.Step_Name (Detection.Last_Step));
         ACPI_Native_Log.Warning ("acpi: bring-up reached hid-discovery (3/4)");
         -- Do not proceed to a serving owner after uncertain retained cleanup.
         return;
      end if;
      Enter_Phase (Phase_Trace.Power_Discovery);
      ACPI_Native_Power_Detection.Run (Owner.all, Power);
      Leave_Phase (Phase_Trace.Power_Discovery, Power_Phase_Result (Power.Status),
        Power.Status in ACPI_Native_Power_Detection.No_Devices | ACPI_Native_Power_Detection.Finished
          and then not Power.Recovery_Needed);
      Enter_Phase (Phase_Trace.Power_Module_Notes);
      ACPI_Native_Module_Notes.Observe (ACPI_Requests.Module_Errors (Owner.all));
      Leave_Phase (Phase_Trace.Power_Module_Notes, "RETURNED", True);
      if Power.Recovery_Needed then
         ACPI_Native_Log.Warning ("acpi: stage startup-stop fail POWER_DISCOVERY recovery=1");
         ACPI_Native_Log.Warning ("acpi: bring-up reached power-discovery (3/4)");
         return;
      end if;
      -- Device _INI and discovery are done: close setup admission (new PCI
      -- buses, pool regions). Runtime evaluation stays on the same Fields.
      Enter_Phase (Phase_Trace.Close_Startup);
      One.Finish_Startup (Finished);
      Leave_Phase (Phase_Trace.Close_Startup, (if Finished then "CLOSED" else "REFUSED"), Finished);
      if Finished then
         ACPI_Native_Log.Info ("acpi: stage finish-startup ok setup=CLOSED");
      else
         ACPI_Native_Log.Warning ("acpi: stage finish-startup fail " & Root.Fields.Observe.Stage'Image);
         ACPI_Native_Log.Warning ("acpi: bring-up reached startup (3/4)");
         return;
      end if;
      Enter_Phase (Phase_Trace.Enter_Runtime);
      One.Runtime_Access.Enter_Runtime (Owner.all,Runtime_Entry);
      Leave_Phase (Phase_Trace.Enter_Runtime, Runtime_Phase_Result (Runtime_Entry),
        Runtime_Entry = One.Runtime.Status.Runtime_Entered);
      if Runtime_Entry /= One.Runtime.Status.Runtime_Entered then
         if Runtime_Entry/=One.Runtime.Status.Rejected or else not Enter_Observer_Only then
            ACPI_Native_Log.Warning("acpi: stage runtime fail " & Runtime_Entry'Image);
            ACPI_Native_Log.Warning("acpi: bring-up reached startup (3/4)");return;
         end if;
         ACPI_Native_Log.Warning("acpi: stage runtime unavailable OBSERVER_ONLY physical=RETIRED aml=DISABLED sci=DISABLED");
      else
         ACPI_Native_Endpoint.Select_Runtime(True,Mode_Selected);
         if not Mode_Selected then ACPI_Native_Log.Warning("acpi: stage registration fail MODE_ALREADY_SELECTED");return;end if;
         ACPI_Native_Log.Info("acpi: stage runtime ok LOCAL_IDLE");
      end if;
      ACPI_Native_Log.Info("acpi: stage runtime-selection ok one-service mode=" &
        (if Observer_Only then "OBSERVER_ONLY" else "LOCAL_RUNTIME"));
      ACPI_Native_Log.Info("acpi: bring-up reached registration (4/4) hardware=" &
        (if Observer_Only then "RETIRED mode=OBSERVER_ONLY" else "LOCAL"));
      Finish_Timing;
      if Observer_Only then
         Startup_Diagnostic_Checkpoint;
         Enter_Phase (Phase_Trace.Serve);
         Observer_Serving.Serve(Owner.all,Reason);
      else
      declare Published : Boolean;
      begin
         Publish_Bridges_Before_Events (Published);
         if not Published then
            if Stop_Served then return; end if;
            if Bridges.Observe.Stage /= HB.Suspended
              or else Bridges.Observe.Why /= HB.Deadline_Expired or else Bridges.Blocks_AML
              or else One.Runtime.Current /= One.Runtime.Status.Idle
              or else not ACPI_Native_Event_Domain.Diagnostics_Quiet (Owner.all)
            then
               -- An uncertain original owner is never reopened. Keep copied
               -- service and acknowledged bridge sessions available instead
               -- of entering global cleanup just because publication stopped.
               Startup_Diagnostic_Checkpoint;
               Close_SCI;
               Enter_Phase (Phase_Trace.Serve);
               Serve_Preserved_Bridges;
               Leave_Phase (Phase_Trace.Serve, Serve_Phase_Result (Reason),
                 Reason = ACPI_Native_Server.Stop_Requested);
               Stop_Served := Reason = ACPI_Native_Server.Stop_Requested;
               return;
            end if;
         end if;
      end;
      -- The SCI, once the namespace and device _INI are done: arm it, then
      -- serve. Not armed is a typed state; nothing polls instead.
      declare
         Armed : ACPI_SCI_Events.Arm_Outcome;
         Why : ACPI_SCI_Registers.Refusal;
         use type ACPI_SCI_Events.Arm_Outcome;
      begin
         SCI_View := One.SCI_Unobserved;
         Enter_Phase (Phase_Trace.Arm_SCI);
         ACPI_SCI_Events.Arm (Startup.Hardware, Startup.Tables, Armed, Why);
         Leave_Phase (Phase_Trace.Arm_SCI, SCI_Phase_Result (Armed), Armed = ACPI_SCI_Events.Armed);
         if Armed /= ACPI_SCI_Events.Armed then
            ACPI_Native_Log.Warning ("acpi: SCI unavailable: " & Armed'Image &
              (if Armed = ACPI_SCI_Events.Layout_Refused then " " & Why'Image else ""));
         end if;
      end;
      declare
         Outcome:ACPI_Native_Event_Domain.Start_Outcome;
         use type ACPI_Native_Event_Domain.Start_Outcome;
         use type ACPI_Native_Event_Domain.Phase;
      begin
         Enter_Phase (Phase_Trace.Start_Events);
         ACPI_Native_Event_Domain.Start(Owner.all,Outcome);
         Event_Start_View := ACPI_Native_Event_Domain.Observe;
         EC_Start_View := ACPI_Native_EC_Discovery.Observe;
         Event_Start_Result := Outcome; Event_Start_Captured := True;
         Leave_Phase (Phase_Trace.Start_Events, Events_Phase_Result (Outcome),
           Outcome = ACPI_Native_Event_Domain.Ready);
         if ACPI_Native_Event_Domain.Diagnostics_Quiet(Owner.all) then
            if Outcome=ACPI_Native_Event_Domain.Ready then ACPI_Native_Log.Info("acpi: stage events ok EC_NOTIFY general="&ACPI_Native_Event_Domain.Observe.General_GPEs'Image);
            elsif Outcome=ACPI_Native_Event_Domain.EC_Unavailable and then ACPI_Native_Event_Domain.Observe.Stage=ACPI_Native_Event_Domain.Live then
               ACPI_Native_Log.Info("acpi: stage events ok CONSUMERS ec="&ACPI_Native_Event_Domain.Observe.EC'Image&
                  " general="&ACPI_Native_Event_Domain.Observe.General_GPEs'Image);
            elsif Outcome=ACPI_Native_Event_Domain.EC_Unavailable and then ACPI_Native_Event_Domain.Observe.EC=ACPI_Native_EC_Discovery.No_EC then
               ACPI_Native_Log.Info("acpi: stage events ok FIXED_ONLY ec=ABSENT");
            else ACPI_Native_Log.Warning("acpi: stage events fail "&Outcome'Image&" ec="&ACPI_Native_Event_Domain.Observe.EC'Image);end if;
         end if;
         if Outcome=ACPI_Native_Event_Domain.Refused then
            -- Stop SCI dispatch before copied-only serving. Keep bridge grants
            -- out of the outer retirement path until a real stop/revocation.
            Startup_Diagnostic_Checkpoint;
            Close_SCI;
            Enter_Phase (Phase_Trace.Serve);
            Serve_Preserved_Bridges;
            Leave_Phase (Phase_Trace.Serve, Serve_Phase_Result (Reason),
              Reason = ACPI_Native_Server.Stop_Requested);
            return;
         end if;
         -- Configure may evaluate AML. Fresh HID collection is the LAST startup
         -- AML. Every candidate finishes and the runtime window closes before
         -- any offer IPC or the consumer initialization declaration.
         declare
            Handoff : HID_Producer.Report;
            use type HID_Producer.Outcome;
         begin
            Enter_Phase (Phase_Trace.Offer_HID);
            HID_Producer.Start (Owner.all, Detection, Completion_Target,
              Original_Startup_Deadline, Completion_Deadline, Handoff);
            HID_Producer.Advance_Cleanup;
            Handoff := HID_Producer.Observe;
            Leave_Phase (Phase_Trace.Offer_HID, HID_Producer.Name (Handoff.Status),
              Handoff.Status in HID_Producer.No_Devices | HID_Producer.Offers_Queued
                and then not Handoff.Recovery_Needed);
            Enter_Phase (Phase_Trace.Offer_Path_Diagnostics);
            Report_HID_Path_Errors;
            Leave_Phase (Phase_Trace.Offer_Path_Diagnostics, "RETURNED", True);
            declare
               package Collection renames ACPI_Native_HID_Collection.Controller;
               Fresh : constant Collection.Report := HID_Producer.Collection_Observation;
            begin
               -- The window is closed and owner calls have returned. These
               -- paths and outcomes are copied facts, never live Core views.
               for I in Collection.Error_Index loop
                  exit when I > Fresh.Captured_Errors;
                  declare E : constant Collection.Error_Observation := Fresh.Errors (I);
                  begin
                     ACPI_Native_Diagnostic_Output.Write
                       ("acpi: stage hid-collection-node fail " & Collection.Error_Name (E.Kind) &
                        " input=" & E.Input_Index'Image &
                        " node=" & ACPI_Service.Values.Position (E.Device)'Image &
                        " path=" & HID_Error_Path_Image (E.Path) & " path-complete=" & Boolean'Image (E.Path_Complete) &
                        " policy=" & E.Policy.Outcome'Image &
                        " query=" & E.Firmware.Result'Image &
                        " resource-exec=" & ACPI_Startup_Diagnostics.Execution_Name (E.Firmware.Execution) &
                        " status-exec=" & ACPI_Startup_Diagnostics.Execution_Name (E.Firmware.Status.Execution) &
                        " descriptor-exec=" & ACPI_Startup_Diagnostics.Execution_Name (E.Firmware.Descriptor.Execution) &
                        " source=" & E.Source_Status'Image & " topology=" & E.Topology.Status'Image &
                        " fuel=" & E.Charged'Image, Owner_Quiet);
                     case E.Kind is
                        when Collection.Topology_Error =>
                           ACPI_Native_Diagnostic_Output.Write
                             ("acpi: stage hid-pci fail input=" & E.Input_Index'Image &
                              " node=" & ACPI_Service.Values.Position (E.Device)'Image &
                              " controller=" & ACPI_Service.Values.Position (E.Topology.Controller)'Image &
                              " host=" & ACPI_Service.Values.Position (E.Topology.Host_Bridge)'Image &
                              " seg-defaulted=" & Boolean'Image (E.Topology.Segment_Defaulted) &
                              " _SEG=" & HID_PCI_Member_Image
                                (E.Topology.Members (ACPI_Native_HID_Collection.Reader.Segment)) &
                              " _BBN=" & HID_PCI_Member_Image
                                (E.Topology.Members (ACPI_Native_HID_Collection.Reader.Base_Bus)) &
                              " _ADR=" & HID_PCI_Member_Image
                                (E.Topology.Members (ACPI_Native_HID_Collection.Reader.Address)), Owner_Quiet);
                        when others => null;
                     end case;
                  end;
               end loop;
               if Fresh.Node_Errors /= 0 then
                  ACPI_Native_Diagnostic_Output.Write
                    ("acpi: stage hid-collection-summary errors=" & Fresh.Node_Errors'Image &
                     " captured=" & Fresh.Captured_Errors'Image & " omitted=" & Fresh.Omitted_Errors'Image,
                     Owner_Quiet);
               end if;
            end;
            ACPI_Native_Diagnostic_Output.Write
              ("acpi: stage hid-offer " &
               (if Handoff.Status in HID_Producer.No_Devices | HID_Producer.Offers_Queued then "ok " else "fail ") &
               HID_Producer.Name (Handoff.Status) & " candidates=" & Handoff.Candidates'Image &
               " dispatched=" & Handoff.Dispatched'Image & " known=" & Handoff.Known'Image &
               " queued=" & Handoff.Queued'Image & " node-errors=" & Handoff.Node_Errors'Image &
               " fuel=" & Handoff.Charged'Image &
               " transport=" & HID_Producer.Last_Transport & " receipt=" & HID_Producer.Last_Receipt &
               " request=" & Handoff.Last_Request'Image & " detail=" & Handoff.Last_Detail'Image &
               " sta=" & (if Handoff.Status_Known then "0x" & Hex (Handoff.Status_Flags) else "UNKNOWN"),
               Owner_Quiet);
            if not HID_Producer.Quiet then
               -- Unknown dispatch/cleanup/recovery forbids any further AML.
               -- The outer stop masks SCI then parks before _REG(0)/retirement.
               return;
            end if;
         end;
         declare
            package P renames CuBit.Consumer_Initialization_Exchange;
            use type ACPI_Service.Namespace.Owned.Module_Error_Count;
            function Event_Result return ACPI_Startup_Completion.Phase_Result is
            begin
               if Outcome = ACPI_Native_Event_Domain.Ready then
                  return ACPI_Startup_Completion.Finished;
               elsif Outcome = ACPI_Native_Event_Domain.EC_Unavailable then
                  return ACPI_Startup_Completion.Unavailable_Events
                    (Domain_Live => ACPI_Native_Event_Domain.Observe.Stage = ACPI_Native_Event_Domain.Live,
                     Controller_Absent => ACPI_Native_Event_Domain.Observe.EC = ACPI_Native_EC_Discovery.No_EC,
                     Recovery_Required => ACPI_Native_Event_Domain.Observe.EC = ACPI_Native_EC_Discovery.Recovery_Required);
               else
                  return ACPI_Startup_Completion.Failed;
               end if;
            end Event_Result;
            Status : P.Phase_Status := ACPI_Native_Startup_Completion.Classify
              (Devices, Detection, Power, Finished, Events => Event_Result,
               Module_Errors => ACPI_Requests.Module_Errors (Owner.all).Count /= 0);
            Receipt : Consumer_Completion.Report;
            function Bit (Value : Boolean) return String is (if Value then "1" else "0");
         begin
            declare
               Handoff : constant HID_Producer.Report := HID_Producer.Observe;
               use type HID_Producer.Outcome, P.Phase_Status;
            begin
               if Handoff.Recovery_Needed or else Handoff.Working or else Handoff.Window_Open then
                  Status := P.Failed;
               elsif Handoff.Status not in HID_Producer.No_Devices | HID_Producer.Offers_Queued
                 and then Status /= P.Failed then
                  Status := P.Incomplete;
               elsif Handoff.Node_Errors /= 0 and then Status = P.Completed then
                  Status := P.Completed_With_Errors;
               end if;
            end;
            Enter_Phase (Phase_Trace.Complete_Consumers);
            Consumer_Completion.Submit (Completion_Target, Completion_Request_Nonce,
              CuBit.Messages.To_Word (CuBit.Messages.Own_Process), Completion_Deadline,
              Status, Receipt);
            Leave_Phase (Phase_Trace.Complete_Consumers,
              Completion_Phase_Result (Receipt.State) & ":" & P.Name (Status),
              Receipt.Receipt_Known and then Receipt.Dispatched);
            ACPI_Native_Diagnostic_Output.Write
              ("acpi: stage consumer-completion status=" & P.Name (Status) &
               " transport=" & Receipt.State'Image & " dispatched=" & Bit (Receipt.Dispatched) &
               " known=" & Bit (Receipt.Receipt_Known) & " receipt=" & P.Name (Receipt.Answer) &
               " released=" & (if Receipt.Receipt_Known then Receipt.Released'Image else "UNKNOWN"),
               Completion_Quiet);
         end;
      end;
      Startup_Diagnostic_Checkpoint;
      Enter_Phase (Phase_Trace.Serve);
      Runtime_Serve_Started := Mode_Selected;
      Serving.Serve (Owner.all, Reason);
      end if;
      Leave_Phase (Phase_Trace.Serve, Serve_Phase_Result (Reason),
        Reason = ACPI_Native_Server.Stop_Requested);
      -- A served Stop counts for the exit status whether or not Fields are
      -- idle now; only the logging below waits for an idle Field boundary.
      Stop_Served := Reason = ACPI_Native_Server.Stop_Requested;
      if Bridge_Loop.Diagnostics_Quiet (Owner.all) then
      ACPI_Native_Module_Notes.Observe (ACPI_Requests.Module_Errors (Owner.all));
      if Reason = ACPI_Native_Server.Stop_Requested then
         ACPI_Native_Log.Info ("acpi: stage serve ok STOP_REQUESTED");
      else
         ACPI_Native_Log.Warning ("acpi: stage serve fail " & Reason'Image);
      end if;
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
      if Runtime_Serve_Started and then Reason=ACPI_Native_Server.Owner_Work_Refused
        and then One.Runtime.Current in One.Runtime.Status.Idle | One.Runtime.Status.Stopped
        and then Owner/=null and then ACPI_Requests.Valid(Owner.all)
        and then not ACPI_Requests.Storage_Released(Owner.all)
      then
         -- A failed AML owner cannot be reopened. Keep its original namespace,
         -- bridge sessions and uncertain resources alive, disarm sources once,
         -- and continue the already selected cached-only power endpoint before
         -- Begin_Stop permanently closes the sleep-entry owner. Its existing
         -- context/Fields/EC/hardware guards remain mandatory for cached S5.
         if One.Runtime.Current=One.Runtime.Status.Idle then One.Runtime.Request_Stop(Runtime_Entry);end if;
         ACPI_Native_Battery_Reads.Begin_Stop;ACPI_Native_Power_Inventory.Begin_Stop;
         ACPI_Native_Event_Domain.Begin_Stop;
         Close_SCI;
         ACPI_Native_Diagnostic_Output.Write
           ("acpi: stage runtime-quarantine fail CACHED_SHUTDOWN retained=1",False);
         Serve_Preserved_Bridges;
         if Reason/=ACPI_Native_Server.Stop_Requested then Park_With_Final_Checkpoint;end if;
         Stop_Served:=True;
      end if;
      ACPI_Native_Battery_Reads.Begin_Stop;ACPI_Native_Power_Inventory.Begin_Stop;
      ACPI_Native_Passive_Queries.Begin_Stop;
      ACPI_Native_Button_Discovery.Begin_Stop;
      ACPI_Native_Power_Buttons.Begin_Stop;
      ACPI_Native_Power_Entry.Begin_Stop;
      if Owner /= null and then ACPI_Requests.Valid (Owner.all) and then not ACPI_Requests.Storage_Released (Owner.all) then
         declare
            Stop_Drain_Turns : constant Positive := Positive'Max(ACPI_Power_Watch_Protocol.Maximum_Watches,ACPI_Native_Battery_Reads.Maximum_Pending);
            Step : ACPI_Native_Power_Entry.Step_Outcome;
            Button_Work : Boolean;
         begin
            for Turn in 1 .. Stop_Drain_Turns loop
               ACPI_Native_Battery_Reads.Advance(Owner.all,ACPI_Native_Event_Domain.Diagnostics_Quiet(Owner.all),False,Button_Work);
               ACPI_Native_Power_Inventory.Advance(ACPI_Native_Event_Domain.Diagnostics_Quiet(Owner.all),Button_Work);
               ACPI_Native_Power_Buttons.Advance (ACPI_Native_Event_Domain.Diagnostics_Quiet (Owner.all), Button_Work);
               ACPI_Native_Power_Entry.Advance (Owner.all, Step);
            end loop;
         end;
      end if;
      -- Early startup failures also publish before cleanup can retain/stall.
      -- A serving invocation already emitted the one-shot checkpoint.
      Startup_Diagnostic_Checkpoint;
      -- Every return after an attempted Arm reaches this typed disarm barrier.
      -- Route before _REG(0), keep routes alive for its own Notify, then close.
      ACPI_Native_Event_Domain.Begin_Stop;
      Close_SCI;
      Drain_Bridges;
      if not Bridges.Retired then
         ACPI_Native_Diagnostic_Output.Write ("acpi: stage host-bridge-retire fail RETAINED", False);
         Park_With_Final_Checkpoint;
      end if;
      if Owner/=null and then ACPI_Requests.Valid(Owner.all) and then not ACPI_Requests.Storage_Released(Owner.all) then
         declare Queries_Quiet:Boolean;begin
            ACPI_Native_Passive_Queries.Drain(Owner.all,Queries_Quiet);
            if not Queries_Quiet then
               ACPI_Native_Diagnostic_Output.Write("acpi: stage query-retire fail RETAINED loan-or-reply=PENDING",False);
               Park_With_Final_Checkpoint;
            end if;
         end;
      end if;
      declare
         Completion : constant Consumer_Completion.Report := Consumer_Completion.Observe;
      begin
         HID_Producer.Advance_Cleanup;
         if HID_Producer.Retention_Required
           or else ACPI_Startup_Completion.Requires_Retention
             (Completion.Dispatched, Completion.Receipt_Known, Completion.Answer, Completion.Released) then
            -- Even a lost/refused receipt cannot prove no child has started.
            -- No child-quiescence acknowledgement exists yet. Do not run
            -- _REG(0), restart AML, or retire this namespace/its mappings.
            -- Keep the process alive; a successor may serve OBSERVER_ONLY.
            ACPI_Native_Diagnostic_Output.Write
              ("acpi: stage retire fail RETAINED child-quiescence=UNKNOWN consumer-completion=" &
               (if Completion.Dispatched then "DISPATCHED" else "NOT_DISPATCHED") &
               " receipt-known=" & (if Completion.Receipt_Known then "1" else "0") &
               " released=" & (if Completion.Receipt_Known then Completion.Released'Image else "UNKNOWN"),
               (ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime));
            Park_With_Final_Checkpoint;
         end if;
      end;
      Drain_Events(False,Events_Quiet);
      if Events_Quiet then
         Close_Regions;
         Drain_Events(True,Events_Quiet);
      end if;
      -- Admission stays closed after Begin_Stop even when the entry owner is
      -- already quiet. Only actual outstanding work prevents final reporting.
      if ACPI_Native_Regions.May_Open_Diagnostic_Channel and then ACPI_Native_Power_Entry.Quiet then Finish_Timing;end if;
      -- Whatever Attempt reached, the root owns hardware state: drive stop,
      -- Field cleanup and hardware retirement before the process may exit.
      -- The loop has returned; no callback or queued request may mutate Core.
      -- This observation is not by itself a hardware retirement permission.
      if Owner = null then Core_View := One.No_Core;
      elsif ACPI_Requests.Valid (Owner.all) then
         Core_View := (if ACPI_Requests.Retained_Results_Pending (Owner.all)
           then One.Retained_Results else One.Clear_Core);
      end if;
      if not ACPI_Native_Regions.Quiet then Core_View:=One.Retained_Regions;end if;
      if not Events_Quiet then Core_View:=One.Retained_Events;end if;
      -- Receipt cleanup can establish the quiet boundary missing at early stop.
      if Owner /= null and then ACPI_Requests.Valid (Owner.all) and then not ACPI_Requests.Storage_Released (Owner.all) then
         declare Worked : Boolean;begin
            for Turn in 1..Positive'Max(ACPI_Power_Watch_Protocol.Maximum_Watches,ACPI_Native_Battery_Reads.Maximum_Pending) loop
               ACPI_Native_Battery_Reads.Advance(Owner.all,ACPI_Native_Event_Domain.Diagnostics_Quiet(Owner.all),False,Worked);
               ACPI_Native_Power_Inventory.Advance(ACPI_Native_Event_Domain.Diagnostics_Quiet(Owner.all),Worked);
            end loop;
            ACPI_Native_Power_Buttons.Advance (ACPI_Native_Event_Domain.Diagnostics_Quiet (Owner.all), Worked);
         end;
      end if;
      -- A saved kernel reply must outlive an unsafe boundary. Never exit and
      -- discard it when no safe stopping reply could be sent.
      if not ACPI_Native_Power_Inventory.Quiet or not ACPI_Native_Battery_Reads.Quiet then Core_View:=One.Retained_Events;end if;
      One.Retire (Core_View,SCI_View,Retired);
      if Retired then
         Retired_Quiescent := True;
         Report_Startup_Diagnostic;
         ACPI_Native_Diagnostic_Output.Write
           ("acpi: stage retire ok QUIESCENT",
            ACPI_Native_Regions.May_Open_Diagnostic_Channel and then ACPI_Native_Power_Entry.Quiet,
            Success=>True);
      else
         Report_Startup_Diagnostic;
            declare
               View : constant Root.Shutdown.Control.Report := Root.Shutdown.Observe;
               Regions : constant ACPI_Native_Regions.Report := ACPI_Native_Regions.Observe;
               function Bit (Value : Boolean) return String is (if Value then "1" else "0");
            begin
               ACPI_Native_Diagnostic_Output.Write ("acpi: stage retire fail RETAINED stage=" & View.Stage'Image &
                 " core=" & Core_View'Image & " sci=" & SCI_View'Image &
                 " events-quiet=" & Bit(Events_Quiet) & " regions-announced=" & Bit(Regions.Announced) &
                 " regions-pending=" & Bit(Regions.Pending) & " regions-working=" & Bit(Regions.Working) &
                 " consumers-closed=" & Bit(View.Consumers_Closed) & " local-closed=" & Bit(View.Local_Closed) &
                 " hardware-closing=" & Bit(View.Hardware_Closing) & " fields-cleaned=" & Bit(View.Fields_Cleaned) &
                 " hardware-idle=" & Bit(View.Hardware_Idle) & " others-idle=" & Bit(View.Others_Idle) &
                 " hardware-continues=" & Bit(View.Hardware_Continues) &
                 " wake-ms=" & View.Wake_Deadline_Milliseconds'Image,
                 (ACPI_Native_Regions.May_Open_Diagnostic_Channel and then not ACPI_Native_Power_Entry.Blocks_Runtime));
            end;
         -- Retained owners must outlive every effect: park, never exit.
         Park_With_Final_Checkpoint;
      end if;
   end Start;
end ACPI_Native_Instance;
