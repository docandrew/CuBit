with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core;
with ACPI_Battery;
with ACPI_Battery_Information;
with ACPI_Battery_Information_Reader;
with ACPI_Battery_Observation;
with ACPI_Battery_Cache;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Battery_Cache_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 8192, Retained_Result_Capacity => 8);
   package F is new ACPI_Battery_Information (8);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   Calls, Clock_Calls, Events, Checks : Natural := 0;
   Fault_At : Natural := 0;
   Event_At : Natural := 0;
   Clock_Available : Boolean := True;
   procedure Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status);
   package D is new ACPI_Battery (C, Evaluate);
   package I is new ACPI_Battery_Information_Reader (C, F, Evaluate);
   package O is new ACPI_Battery_Observation (C, F, D, I);
   package B is new ACPI_Battery_Cache (C, F, D, I, O);
   package S renames B.Samples;
   use type O.Outcome;
   use type O.Observation;
   use type B.Binding_Result;
   use type B.Refresh_Result;
   use type S.Availability;
   use type S.Publication_Result;
   use type S.Tick;
   Cache : B.State;
   Near_End : B.State (S.Revision_Number'Last - 2);
   procedure Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Calls := Calls + 1;
      C.Evaluate_Named (State, Item, Budget, Result, Status);
      if Calls = Fault_At then Result.Charged := Budget + 1; end if;
      if Calls = Event_At then B.Invalidate (Cache); Events := Events + 1; end if;
   end Evaluate;
   function Read_Clock return S.Clock_Reading is
   begin
      Clock_Calls := Clock_Calls + 1;
      return (Available => Clock_Available, Value => 100);
   end Read_Clock;
   procedure Collect_And_Publish (Target : in out B.State; State : aliased in out C.State;
      Budget : Natural; Report : out B.Refresh_Report) is
   begin
      B.Prepare_Refresh (Target, State, Budget, Report);
      if Report.Status = B.Collected then
         B.Publish_Refresh (Target, State, Read_Clock, True, Report);
      end if;
   end Collect_And_Publish;
   Service : aliased C.State (1, 4096, 4096, 1);
   Device, Setter : C.Node_Descriptor;
   Binding : B.Binding_Result;
   Report : B.Refresh_Report;
   Item : S.View;
   subtype Case_Number is Natural range 0 .. 9;
   Action : Case_Number;
   procedure Check (OK : Boolean; Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Text; end if;
   end Check;
   function Find (Name : String) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available, "find " & Name);
      C.Describe_Node (Service, Node, Item, Status);
      Check (Status = C.Values.Available, "describe " & Name);
      return Item;
   end Find;
   procedure Mode (Number : Natural) is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Invoke_Retained (Service, C.Values.Position (Setter),
        [0 => AML_Decode.Integer_Value (Number), others => 0], 1, 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.No_Return, "fixture mode");
      Calls := 0; Clock_Calls := 0;
   end Mode;
   procedure Refresh is
   begin
      Collect_And_Publish (Cache, Service, 1000, Report);
      Check (C.Retained_Results (Service) = 0, "no retained interpreter handles in cache");
   end Refresh;
   procedure Read_Check (Now : S.Tick; Age : S.Tick; Expected : S.Availability) is
      Before : constant Natural := Calls;
      Before_Clock : constant Natural := Clock_Calls;
   begin
      Item := B.Read (Cache, (True, Now), Age);
      Check (Item.Status = Expected and then Calls = Before and then Clock_Calls = Before_Clock,
             "cache reads never evaluate AML or clock callbacks");
      Check (Expected = S.Available or else Item.Value = O.Empty_Observation, "unavailable read has no payload");
   end Read_Check;
begin
   Check (Ada.Command_Line.Argument_Count = 2, "fixture and case");
   Action := Case_Number'Value (Ada.Command_Line.Argument (2));
   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
      Stage : C.Stage_Status;
      Freeze : C.Freeze_Status;
      Executed : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (File, Buffer, Last); Ada.Streams.Stream_IO.Close (File);
      declare Data : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for N in Data'Range loop Data (N) := Firmware_Tables.Byte (Buffer (Ada.Streams.Stream_Element_Offset (N))); end loop;
         C.Stage_Table (Service, 1, C.DSDT, Data, Stage); Check (Stage = C.Staged, "stage");
      end;
      C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (Service, 1000, Executed, Status);
      Check (Status = C.Values.Available and then Executed.Modules.Outcome = C.Values.Modules_Completed, "modules");
      C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "members");
   end;
   Device := Find ("BAT0"); Setter := Find ("SELM");
   if Action = 6 then
      B.Bind (Near_End, Service, Device, Binding);
      Check (Binding = B.Bound, "near-end binding");
      Collect_And_Publish (Near_End, Service, 1000, Report);
      Check (Report.Status = B.Counter_Exhausted and then Calls = 0 and then Clock_Calls = 0,
             "no AML without publication counter reserve");
      Item := B.Read (Near_End, (True, 100), 0);
      Check (Item.Status = S.Counter_Exhausted and then Item.Value = O.Empty_Observation,
             "terminal counter has no data");
      Collect_And_Publish (Near_End, Service, 1000, Report);
      Check (Report.Status = B.Counter_Exhausted and then Calls = 0, "no revival after exhaustion");
   else
      B.Bind (Cache, Service, Device, Binding); Check (Binding = B.Bound, "valid binding");
      Read_Check (100, 0, S.Not_Ready);
      case Action is
         when 0 =>
            Mode (0); Refresh;
            Check (Report.Status = B.Updated and then Report.Collection.Status = O.Sampled, "sample recorded");
            for N in 1 .. 100 loop Read_Check (100, 0, S.Available); end loop;
            Check (Calls = 5 and then Clock_Calls = 1 and then Item.Value.Information.Design.Value = 42000,
                   "owned sample copied without any later AML");
            Read_Check (101, 0, S.Too_Old); Read_Check (101, 1, S.Available);
            Read_Check (99, S.Tick'Last, S.Clock_Regressed);
            B.Invalidate (Cache); Read_Check (100, 0, S.Not_Ready);
         when 1 =>
            Mode (2); Refresh;
            Check (Report.Collection.Status = O.Absent and then Calls = 2, "absence skips information/status");
            Read_Check (100, 0, S.Missing); Read_Check (101, 0, S.Too_Old);
         when 2 =>
            Mode (0); Refresh; Read_Check (100, 0, S.Available);
            Mode (7); Refresh;
            Check (Report.Status = B.Updated and then Report.Collection.Status = O.Information_Failed and then Calls = 3,
                   "failed refresh records failure without replay");
            Read_Check (100, 0, S.Failure);
         when 3 =>
            Mode (0); Refresh; Read_Check (100, 0, S.Available);
            B.Bind (Cache, Service, C.Values.No_Node, Binding);
            Check (Binding = B.Invalid_Device, "invalid rebind clears old binding");
            Refresh; Check (Report.Status = B.Not_Bound and then Calls = 5, "unbound refresh performs no AML");
            Read_Check (100, 0, S.Not_Ready);
         when 4 =>
            Mode (5); Refresh;
            Check (Report.Collection.Status = O.Presence_Changed and then Calls = 5, "changed presence discards bundle");
            Read_Check (100, 0, S.Failure);
         when 5 =>
            Mode (0); Clock_Available := False; Refresh;
            Check (Report.Collection.Status = O.Sampled, "untimed collection is explicit");
            Read_Check (100, 10, S.Clock_Unknown);
            Check (not Item.Time.Available and then Item.Time.Value = 0, "untimed clock metadata normalized");
            Clock_Available := True; Refresh; Read_Check (100, 0, S.Available);
         when 7 =>
            Mode (0); Fault_At := 3; Refresh;
            Check (Report.Collection.Status = O.Provider_Contract_Error and then Calls = 3, "overcharge stops collection");
            Read_Check (100, 0, S.Failure);
         when 8 =>
            Mode (0); Event_At := 3; Refresh;
            Check (Report.Collection.Status = O.Sampled and then Report.Status = B.Publication_Refused
                   and then Report.Publication = S.Stale_Revision and then Calls = 5 and then Events = 1,
                   "invalidation during collection refuses late sample without retry");
            Read_Check (100, 0, S.Not_Ready);
         when 9 =>
            Mode (0); Refresh; Read_Check (100, 0, S.Available);
            B.Forget (Cache); Refresh;
            Check (Report.Status = B.Not_Bound and then Calls = 5, "forgotten binding performs no AML");
            Read_Check (100, 0, S.Not_Ready);
         when 6 => raise Program_Error;
      end case;
   end if;
   Put_Line ("Battery cache integration checks" & Checks'Image);
end Battery_Cache_Tests;
