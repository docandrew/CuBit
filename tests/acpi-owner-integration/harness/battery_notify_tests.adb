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
with ACPI_Battery_Cache.Notifications;
with ACPI_Battery_Cache.Observers;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Battery_Notify_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 8192, Retained_Result_Capacity => 8);
   package F is new ACPI_Battery_Information (8);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   Calls, Checks, Clock_Calls, Drained, Matched, Unmatched : Natural := 0;
   Fail_Drain, Forget_In_Hook : Boolean := False;
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
   package E is new B.Notifications;
   package W is new B.Observers;
   package S renames B.Samples;
   use type O.Outcome;
   use type O.Observation;
   use type B.Binding_Result;
   use type B.Refresh_Result;
   use type S.Availability;
   use type S.Publication_Result;
   use type S.View;
   use type E.Routing_Result;
   use type W.Words;
   Cache, Other_Cache : B.State;
   Service : aliased C.State (1, 4096, 4096, 1);
   Last_Event : C.Notification_Observation;
   procedure Check (OK : Boolean; Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Text; end if;
   end Check;
   function Read_Clock return S.Clock_Reading is
   begin Clock_Calls := Clock_Calls + 1; return (True, 100); end Read_Clock;
   procedure Process_Events (Target : in out B.State; State : in out C.State; Healthy : out Boolean) is
      Event : C.Notification_Observation;
      Present : Boolean;
      Status : C.Values.Access_Status;
      Routed : E.Routing_Result;
   begin
      Healthy := False;
      if Fail_Drain then return; end if;
      if Forget_In_Hook then B.Forget (Target); end if;
      for Attempt in 0 .. C.Notification_Count'Last loop
         E.Take (Target, State, Event, Present, Status, Routed);
         if Status /= C.Values.Available then return; end if;
         if not Present then Healthy := True; return; end if;
         Drained := Drained + 1;
         Last_Event := Event;
         if Routed = E.Cache_Invalidated then Matched := Matched + 1;
         else Unmatched := Unmatched + 1;
         end if;
      end loop;
   end Process_Events;
   procedure Collect_Route_Publish (Target : in out B.State; State : aliased in out C.State;
      Budget : Natural; Report : out B.Refresh_Report) is
      Healthy : Boolean;
      Clock : S.Clock_Reading;
   begin
      B.Prepare_Refresh (Target, State, Budget, Report);
      if Report.Status /= B.Collected then return; end if;
      Process_Events (Target, State, Healthy);
      if Healthy then Clock := Read_Clock; else Clock := (others => <>); end if;
      B.Publish_Refresh (Target, State, Clock, Healthy, Report);
   end Collect_Route_Publish;
   Device, Other_Device, Setter, Sender : C.Node_Descriptor;
   Binding : B.Binding_Result;
   Report : B.Refresh_Report;
   View : S.View;
   Event : C.Notification_Observation;
   Present : Boolean;
   Status : C.Values.Access_Status;
   Routed : E.Routing_Result;
   Before : S.Revision_Number;
   subtype Case_Number is Natural range 0 .. 17;
   Action : Case_Number;
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
      Check (Result = C.Values.Available and then Value.Status = AML_Execute.No_Return, "owner method invocation");
   end Invoke;
   procedure Read_Check (Expected : S.Availability) is
      Evaluations : constant Natural := Calls;
      Header : W.Words;
      Words : W.Words;
      type Pairs is array (Natural range 0 .. 20) of W.Words;
      Expected_Pages : Pairs :=
        [0 => [0, 0, 2, 2], 1 => [0, 0, 1, 0], 2 => [0, 0, 1, 0],
         3 => [0, 0, 1, 100], 4 => [0, 0, 1, 20000], 5 => [0, 0, 1, 11000],
         6 => [0, 0, 1, 42000], 7 => [0, 0, 1, 40000], 8 => [0, 0, 1, 11000],
         9 => [0, 0, 1, 12], 10 => [0, 0, 0, 0], 11 => [0, 0, 1, 10],
         12 => [0, 0, 2000, 1000], 13 => [0, 0, 100000, 0],
         14 => [0, 0, 500, 5], 15 => [0, 0, 1, 10], 16 => [0, 0, 1, 0],
         17 => [0, 0, 5, 16#5449_4255_43#], 18 => [0, 0, 4, 16#3152_4553#],
         19 => [0, 0, 4, 16#4E4F_494C#], 20 => [0, 0, 0, 0]];
      procedure Refused (Got : W.Words; Code : W.Result_Code; Text : String) is
      begin
         Check (Got = W.Words'[View.Revision, W.Word (W.Result_Code'Enum_Rep (Code)), 0, 0], Text);
      end Refused;
   begin
      View := B.Read (Cache, (True, 100), 0);
      Check (View.Status = Expected and then Calls = Evaluations, "read performs no AML");
      Check (Expected = S.Available or else View.Value = O.Empty_Observation, "no unavailable payload");
      Header := W.Header (Cache, (True, 100), 0);
      Check (Header (0) = View.Revision and then Header (2) = 1 and then Header (3) = 20,
             "header revision and static schema");
      Words := W.Read_Page (Cache, (True, 100), 0, View.Revision, 0, 0);
      if Expected = S.Available then
         Check (Header (1) = 0, "available header status");
         if Action = 12 then
            Expected_Pages (2) := [0, 0, 13, 16#FFFF_FFF0#];
            Expected_Pages (3) := [0, 0, 0, 0]; Expected_Pages (4) := [0, 0, 1, 0];
            Expected_Pages (5) := [0, 0, 1, 16#7FFF_FFFF#];
            Expected_Pages (9) := [0, 0, 0, 0]; Expected_Pages (16) := [0, 0, 1, 16#FFFF_FFFF#];
            Expected_Pages (17) := [0, 0, 8, 16#7F7F_7F7F_7F7F_7F#];
         elsif Action = 13 then
            Expected_Pages (2) := [0, 0, 0, 0]; Expected_Pages (3) := [0, 0, 0, 0];
            Expected_Pages (4) := [0, 0, 1, 0];
         elsif Action = 16 then
            Expected_Pages (1) := [0, 0, 0, 0]; Expected_Pages (16) := [0, 0, 0, 0];
         end if;
         for Page in Expected_Pages'Range loop
            Expected_Pages (Page) (0) := View.Revision;
            Check (W.Read_Page (Cache, (True, 100), 0, View.Revision, Unsigned_64 (Page), 0)
                   = Expected_Pages (Page), "independent expected page" & Page'Image);
         end loop;
         for Page in 17 .. 20 loop
            declare
               Length : constant Natural := Natural (Expected_Pages (Page) (2));
            begin
               for Offset in 0 .. Length loop
                  Words := W.Read_Page (Cache, (True, 100), 0, View.Revision, Unsigned_64 (Page), Unsigned_64 (Offset));
                  Check (Words (1) = 0 and then Words (2) = Unsigned_64 (Length), "text offsets preserve length");
                  if Offset = Length then Check (Words (3) = 0, "text end is empty"); end if;
                  if Action = 12 and then Page = 17 and then Offset = 7 then
                     Check (Words (3) = 127, "eighth byte survives second chunk");
                  end if;
               end loop;
               Refused (W.Read_Page (Cache, (True, 100), 0, View.Revision, Unsigned_64 (Page), Unsigned_64 (Length + 1)),
                        W.Bad_Offset, "past text end refused");
            end;
         end loop;
         Refused (W.Read_Page (Cache, (True, 100), 0, View.Revision, 0, 1), W.Bad_Offset, "numeric offset refused");
         Refused (W.Read_Page (Cache, (True, 100), 0, View.Revision, 21, 0), W.Bad_Page, "past last page refused");
         Refused (W.Read_Page (Cache, (True, 100), 0, View.Revision, Unsigned_64'Last, 0), W.Bad_Page, "full width page refused before narrowing");
         Refused (W.Read_Page (Cache, (True, 100), 0, View.Revision, 17, Unsigned_64'Last), W.Bad_Offset, "full width text offset refused before narrowing");
         Refused (W.Read_Page (Cache, (False, 100), 0, View.Revision, 0, 0), W.Clock_Unknown, "missing clock hides values");
         Refused (W.Read_Page (Cache, (True, 99), 0, View.Revision, 0, 0), W.Clock_Regressed, "regressing clock hides values");
         Refused (W.Read_Page (Cache, (True, 101), 0, View.Revision, 0, 0), W.Too_Old, "expired page hides values");
      elsif Expected = S.Missing then
         Check (Header (1) = 2 and then Words = W.Words'[View.Revision, 2, 0, 0], "absent is explicit and empty");
         Refused (W.Read_Page (Cache, (True, 101), 0, View.Revision, 0, 0), W.Too_Old, "cached absence expires");
      elsif Expected = S.Failure then
         Check (Header (1) = 3 and then Words = W.Words'[View.Revision, 3, 0, 0], "acquisition failure is not absence");
      else
         Check (Header (1) = 1 and then Words = W.Words'[View.Revision, 1, 0, 0], "pending has no wire payload");
      end if;
      Refused (W.Read_Page (Cache, (True, 100), 0, Unsigned_64'Last, 0, 0), W.Stale, "full width revision never narrows");
      Check (Calls = Evaluations and then B.Read (Cache, (True, 100), 0) = View,
             "all observer queries preserve cache and perform no AML");
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
   Check (Status = C.Values.Available, "enable actual notification admission");
   if Action /= 6 then
      B.Bind (Cache, Service, Device, Binding); Check (Binding = B.Bound, "bind current battery");
   end if;
   if Action = 12 then Invoke (Setter, 21);
   elsif Action = 13 then Invoke (Setter, 15);
   elsif Action = 14 then Invoke (Setter, 2);
   elsif Action = 15 then Invoke (Setter, 7);
   elsif Action = 16 then Invoke (Setter, 13);
   elsif Action in 0 | 2 | 7 | 9 then Invoke (Setter, (if Action = 2 then 18 else 0));
   elsif Action in 1 | 4 then Invoke (Setter, 17);
   elsif Action = 3 then Invoke (Setter, 19);
   elsif Action = 8 then Invoke (Setter, 20);
   else Invoke (Setter, 0);
   end if;
   if Action = 4 then Fail_Drain := True; end if;
   if Action = 5 then Forget_In_Hook := True; end if;
   if Action /= 6 and then Action not in 10 .. 11 then
      Collect_Route_Publish (Cache, Service, 1000, Report);
      Check (C.Retained_Results (Service) = 0, "collector releases before queue processing");
   end if;
   case Action is
      when 0 =>
         Check (Report.Status = B.Updated and then Report.Events_Drained, "empty queue permits publication");
         Read_Check (S.Available); Before := View.Revision;
         Invoke (Sender, 0);
         E.Take (Cache, Service, Event, Present, Status, Routed);
         Check (Status = C.Values.Available and then Present and then Event.Current_Target
                and then Event.Node = C.Values.Position (Device) and then Event.Number = 16#81#
                and then Routed = E.Cache_Invalidated, "actual queued target invalidates cache");
         Read_Check (S.Not_Ready); Check (View.Revision > Before, "event advances revision");
         Check (W.Read_Page (Cache, (True, 100), 0, Before, 0, 0) =
                W.Words'[View.Revision, 8, 0, 0], "inter-page Notify refuses old header revision");
      when 1 | 3 =>
         Check (Report.Collection.Status = O.Sampled and then Report.Status = B.Publication_Refused
                and then Report.Publication = S.Stale_Revision and then Report.Events_Drained
                and then Calls = 5 and then Clock_Calls = 1, "Notify during BIX rejects late result without replay");
         Check (Drained = (if Action = 1 then 1 else 3) and then Matched = (if Action = 1 then 1 else 2)
                and then Unmatched = (if Action = 1 then 0 else 1), "mixed events preserved for owner routing");
         Read_Check (S.Not_Ready);
      when 2 =>
         Check (Report.Status = B.Updated and then Drained = 1 and then Matched = 0 and then Unmatched = 1
                and then Last_Event.Node = C.Values.Position (Other_Device), "unrelated copied event is returned");
         Read_Check (S.Available);
      when 4 =>
         Check (Report.Status = B.Events_Failed and then not Report.Events_Drained
                and then Calls = 5 and then Clock_Calls = 0 and then C.Pending_Notifications (Service) = 1,
                "uncertain event processing blocks publication and retains pending event");
         Read_Check (S.Not_Ready);
      when 5 =>
         Check (Report.Status = B.Invalid_Binding and then Report.Events_Drained and then Clock_Calls = 1,
                "binding is revalidated after owner hook");
         Read_Check (S.Not_Ready);
      when 6 =>
         Invoke (Sender, 0);
         E.Take (Cache, Service, Event, Present, Status, Routed);
         Check (Status = C.Values.Available and then Present and then Routed = E.Ignored,
                "unbound cache returns event to owner");
         Read_Check (S.Not_Ready);
      when 7 =>
         Invoke (Sender, 0);
         C.Take_Notification (Service, Event, Present, Status);
         Check (Status = C.Values.Available and then Present, "dequeue for immediate copied routing");
         Read_Check (S.Available); Before := View.Revision;
         Event.Current_Target := False;
         E.Apply (Cache, Service, Event, Routed);
         Check (Routed = E.Ignored, "stale target metadata ignored");
         Read_Check (S.Available); Check (View.Revision = Before, "ignored event leaves revision alone");
      when 8 =>
         Check (Report.Collection.Status = O.Information_Failed and then Report.Status = B.Publication_Refused
                and then Calls = 3 and then Drained = 1 and then Matched = 1,
                "notification is routed even after collection failure");
         Read_Check (S.Not_Ready);
      when 9 =>
         B.Bind (Other_Cache, Service, Other_Device, Binding); Check (Binding = B.Bound, "second device binding");
         Invoke (Sender, 1);
         E.Take (Cache, Service, Event, Present, Status, Routed);
         Check (Status = C.Values.Available and then Present and then Routed = E.Ignored, "first binding does not consume routing meaning");
         E.Apply (Other_Cache, Service, Event, Routed);
         Check (Routed = E.Cache_Invalidated, "same copied event routes to second binding without AML");
         Read_Check (S.Available);
      when 10 =>
         B.Prepare_Refresh (Cache, Service, 1000, Report);
         Check (Report.Status = B.Collected and then Calls = 5, "one pending collection");
         Read_Check (S.Not_Ready);
         B.Prepare_Refresh (Cache, Service, 1000, Report);
         Check (Report.Status = B.Already_Pending and then Calls = 5, "second prepare performs no AML");
         B.Publish_Refresh (Cache, Service, (True, 100), True, Report);
         Check (Report.Status = B.Updated and then Report.Collection.Status = O.Sampled, "private pending collection survives busy report");
         Read_Check (S.Available); Before := View.Revision;
         B.Publish_Refresh (Cache, Service, (True, 100), True, Report);
         Check (Report.Status = B.Publication_Refused and then Calls = 5, "pending result consumed once");
         Read_Check (S.Available); Check (View.Revision = Before, "repeat publish does not mutate cache");
      when 11 =>
         B.Prepare_Refresh (Cache, Service, 1000, Report);
         Check (Report.Status = B.Collected, "prepare first cache");
         B.Bind (Other_Cache, Service, Other_Device, Binding); Check (Binding = B.Bound, "bind independent cache");
         B.Publish_Refresh (Other_Cache, Service, (True, 100), True, Report);
         Check (Report.Status = B.Publication_Refused, "pending update cannot be published through another cache");
         View := B.Read (Other_Cache, (True, 100), 0);
         Check (View.Status = S.Not_Ready and then View.Value = O.Empty_Observation, "other cache has no payload");
         B.Publish_Refresh (Cache, Service, (True, 100), True, Report);
         Check (Report.Status = B.Updated and then Calls = 5, "correct cache publishes without replay");
         Read_Check (S.Available);
      when 12 | 13 =>
         Check (Report.Status = B.Updated and then Report.Collection.Status = O.Sampled, "wire boundary collection");
         Read_Check (S.Available);
      when 14 =>
         Check (Report.Status = B.Updated and then Report.Collection.Status = O.Absent, "known absence cached");
         Read_Check (S.Missing);
      when 15 =>
         Check (Report.Status = B.Updated and then Report.Collection.Status = O.Information_Failed, "failed acquisition cached");
         Read_Check (S.Failure);
      when 16 =>
         Check (Report.Status = B.Updated and then Report.Collection.Status = O.Sampled, "BIX revision zero cached");
         Read_Check (S.Available);
      when 17 =>
         declare
            Exhausted : B.State (S.Revision_Number'Last);
            Header : W.Words;
         begin
            B.Bind (Exhausted, Service, Device, Binding);
            Check (Binding = B.Counter_Exhausted, "terminal revision cannot wrap");
            Header := W.Header (Exhausted, (True, 100), 0);
            Check (Header = W.Words'[S.Revision_Number'Last, 7, 1, 20], "exhausted header remains signed representable");
            Check (W.Read_Page (Exhausted, (True, 100), 0, S.Revision_Number'Last, 0, 0)
                   = W.Words'[S.Revision_Number'Last, 7, 0, 0], "exhaustion carries no sample");
         end;
         Read_Check (S.Available);
   end case;
   Check (C.Retained_Results (Service) = 0, "no retained notification authority");
   Put_Line ("Battery notification checks" & Checks'Image);
end Battery_Notify_Tests;
