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
with ACPI_Battery_Cache.Groups;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Group_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 8192, Retained_Result_Capacity => 8,
      Notification_Capacity => 4);
   package F is new ACPI_Battery_Information (8);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   Calls, Clock_Calls, Checks : Natural := 0;
   Clock_Available : Boolean := True;
   procedure Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Calls := Calls + 1;
      C.Evaluate_Named (State, Item, Budget, Result, Status);
   end Evaluate;
   package D is new ACPI_Battery (C, Evaluate);
   package I is new ACPI_Battery_Information_Reader (C, F, Evaluate);
   package O is new ACPI_Battery_Observation (C, F, D, I);
   package B is new ACPI_Battery_Cache (C, F, D, I, O);
   function Clock return B.Samples.Clock_Reading is
   begin
      Clock_Calls := Clock_Calls + 1;
      return (Clock_Available, 100);
   end Clock;
   package G is new B.Groups (2, Clock);
   use type B.Binding_Result;
   use type B.Refresh_Result;
   use type O.Outcome;
   Group : G.Owner;
   Service : aliased C.State (1, 4096, 4096);
   Device, Other_Device, Setter, Sender : C.Node_Descriptor;
   Binding : B.Binding_Result;
   Report : B.Refresh_Report;
   Batch : G.Batch;
   Status : C.Values.Access_Status;
   Before_One, Before_Two : G.Pages.Words;
   Evaluations, Clocks : Natural;
   subtype Case_Number is Natural range 0 .. 6;
   Action : Case_Number;
   procedure Check (OK : Boolean; Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Text; end if;
   end Check;
   function Find (Name : String) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Result : C.Values.Access_Status;
   begin
      C.Find_Child (Service, C.Namespace.Root, Name, Node, Result);
      Check (Result = C.Values.Available, "find " & Name);
      C.Describe_Node (Service, Node, Item, Result);
      Check (Result = C.Values.Available, "describe " & Name);
      return Item;
   end Find;
   procedure Invoke (Method : C.Node_Descriptor; Argument : Natural) is
      Value : C.Values.Result;
      Result : C.Values.Access_Status;
   begin
      C.Invoke_Retained (Service, C.Values.Position (Method),
        [0 => AML_Decode.Integer_Value (Argument), others => 0], 1, 1000, Value, Result);
      Check (Result = C.Values.Available and then Value.Status = AML_Execute.No_Return, "invoke owner fixture");
   end Invoke;
   function Header (Index : G.Slot) return G.Pages.Words is
     (G.Header (Group, Index, (True, 100), 0));
   function Page (Index : G.Slot; Revision : Unsigned_64) return G.Pages.Words is
     (G.Read_Page (Group, Index, (True, 100), 0, Revision, 0, 0));
begin
   Check (Ada.Command_Line.Argument_Count = 2, "fixture/case");
   Action := Case_Number'Value (Ada.Command_Line.Argument (2));
   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
      Stage : C.Stage_Status;
      Freeze : C.Freeze_Status;
      Executed : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
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
   Device := Find ("BAT0"); Other_Device := Find ("OTH0"); Setter := Find ("SELM"); Sender := Find ("SEND");
   C.Configure_Notifications (Service, True, Status);
   Check (Status = C.Values.Available, "enable actual notification queue");
   G.Bind (Group, Service, 1, Device, Binding); Check (Binding = B.Bound, "bind first cache");
   G.Bind (Group, Service, 2, Device, Binding); Check (Binding = B.Bound, "bind second observer cache to same target");
   if Action = 1 then Invoke (Setter, 19);
   elsif Action = 2 then Invoke (Setter, 18);
   elsif Action = 5 then Invoke (Setter, 22);
   else Invoke (Setter, 0);
   end if;
   if Action = 3 then Clock_Available := False; end if;
   G.Refresh (Group, Service, 1, 1000, Report, Batch);
   Check (Batch.Healthy and then Clock_Calls = 1, "bounded routing completes before one clock sample");
   Check (C.Pending_Notifications (Service) = 0 and then C.Retained_Results (Service) = 0,
          "refresh leaves no retained result or hidden queued event");
   case Action is
      when 0 =>
         Check (Report.Status = B.Updated and then Batch.Count = 0, "empty batch publishes once");
         G.Refresh (Group, Service, 2, 1000, Report, Batch);
         Before_One := Header (1); Before_Two := Header (2);
         Check (Before_One (1) = 0 and then Before_Two (1) = 0, "both slots independently cached");
         Evaluations := Calls; Clocks := Clock_Calls;
         Invoke (Sender, 0);
         G.Drain (Group, Service, Batch);
         Check (Batch.Healthy and then Batch.Count = 1 and then Batch.Events (1).Number = 16#81#,
                "drain returns original copied event");
         Check (Header (1) (1) = 1 and then Header (2) (1) = 1, "one event invalidates both matching slots");
         Check (Page (1, Before_One (0)) (1) = 8 and then Page (2, Before_Two (0)) (1) = 8,
                "both old revisions rejected");
         Check (Calls = Evaluations and then Clock_Calls = Clocks, "routing/readout invokes no firmware or clock");
      when 1 =>
         Check (Report.Status = B.Publication_Refused and then Report.Collection.Status = O.Sampled
                and then Calls = 5, "notification during collection refuses stale result without replay");
         Check (Batch.Count = 3 and then Batch.Events (1).Node = C.Values.Position (Device)
                and then Batch.Events (2).Node = C.Values.Position (Other_Device)
                and then Batch.Events (3).Node = C.Values.Position (Device)
                and then Batch.Events (1).Number = 16#80# and then Batch.Events (3).Number = 16#81#,
                "mixed target FIFO fully returned to outer owner");
         Check (Header (1) (1) = 1 and then Header (2) (1) = 1, "both bindings invalidated during refresh");
      when 2 =>
         Check (Report.Status = B.Updated and then Batch.Count = 1
                and then Batch.Events (1).Node = C.Values.Position (Other_Device), "unmatched event preserved while chosen cache publishes");
         Check (Header (1) (1) = 0, "unrelated target does not invalidate selected cache");
      when 3 =>
         Check (Report.Status = B.Updated and then Header (1) (1) = 5,
                "unknown clock never certifies freshness");
         Check (Page (1, Header (1) (0)) (2) = 0 and then Page (1, Header (1) (0)) (3) = 0, "clock failure hides payload");
      when 4 =>
         Before_Two := Header (2);
         G.Forget (Group, 2); G.Bind (Group, Service, 2, Other_Device, Binding);
         Check (Binding = B.Bound and then Header (2) (0) > Before_Two (0), "slot rebind preserves advancing cache lifetime");
         Check (Page (2, Before_Two (0)) (1) = 8, "old slot token cannot read new binding");
      when 5 =>
         Check (Report.Collection.Status = O.Information_Failed and then Report.Status = B.Publication_Refused,
                "queue overflow fails AML collection and matching events refuse publication");
         Check (Batch.Count = G.Event_Count'Last and then Calls = 3, "full queue drained including final empty probe without retry");
         for N in 1 .. Batch.Count loop
            Check (Batch.Events (N).Node = C.Values.Position (Device), "every accepted event remains observable");
         end loop;
         Check (Header (1) (1) = 1, "overflow never yields partial battery payload");
      when 6 =>
         Before_One := Header (1); Before_Two := Header (2);
         G.Invalidate_All (Group);
         Check (Header (1) (1) = 1 and then Header (2) (1) = 1, "epoch uncertainty invalidates every slot");
         Check (Header (1) (0) > Before_One (0) and then Header (2) (0) > Before_Two (0), "group invalidation advances all revisions");
   end case;
   Check (C.Valid (Service) and then C.Retained_Results (Service) = 0, "final core remains valid without retained authority");
   Put_Line ("Battery group checks" & Checks'Image);
end Group_Tests;
