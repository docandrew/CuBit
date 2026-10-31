pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service_Core;
with ACPI_Region_Initialization;
with AML_Delays;
with AML_Execute;
with Firmware_Tables;
procedure Region_Cleanup_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 32, Aggregate_Method_Capacity => 1024,
      Retained_Result_Capacity => 2);
   package E renames AML_Execute;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status,
      C.Values.Module_Outcome, C.Values.Value_Handle;
   use type E.Execution_Status;
   type Scenario is (Ordinary, Overcharge, Overcharge_Refused, Refused_Release,
      Uncleared_Release, Cleared_Failure, Readiness_Lost, Access_Failure,
      Missing_Handle, Disconnect_Refused);
   Selected : Scenario := Ordinary;
   type Cleanup_Mode is (Normal, Refuse, Unclear, Clear_Error);
   Next_Release : Cleanup_Mode := Normal;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Selected'Image & ": " & Why; end if;
   end Check;
   type Owner_Context is limited record
      Calls, Releases : Natural := 0;
      Ready : Boolean := True;
   end record;
   Owner : Owner_Context;
   function Ready (Context : Owner_Context; Region : C.Node_Descriptor;
      Data : C.Namespace.Operation_Region_Attributes) return Boolean is
      pragma Unreferenced (Region, Data);
   begin return Context.Ready; end Ready;
   function PCI_Root (Context : Owner_Context; Region : C.Node_Descriptor) return Boolean is
      pragma Unreferenced (Context, Region);
   begin return False; end PCI_Root;
   procedure Invoke (Service : aliased in out C.State; Context : in out Owner_Context;
      Node : C.Namespace.Node_ID; Args : E.Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Check (Argument_Count = 2, "two notice arguments");
      Context.Calls := Context.Calls + 1;
      if Selected = Missing_Handle then
         Result := (Status => E.Object_Returned, Charged => 1, Handle => C.Values.No_Value);
         Status := C.Values.Available;
      else
         C.Invoke_Retained (Service, Node, Args, Argument_Count, Budget, Result, Status);
         Check (Status = C.Values.Available and then Result.Status = E.Object_Returned
            and then Result.Handle /= C.Values.No_Value, "actual AML retained package result");
      end if;
      if Selected in Overcharge | Overcharge_Refused then Result.Charged := Budget + 1; end if;
      if Selected = Readiness_Lost then Context.Ready := False; end if;
      if Selected = Access_Failure then Status := C.Values.Invalid_Value; end if;
   end Invoke;
   procedure Release (Service : in out C.State; Handle : in out C.Values.Value_Handle;
      Status : out C.Values.Access_Status) is
      Mode : constant Cleanup_Mode := Next_Release;
   begin
      Owner.Releases := Owner.Releases + 1; Next_Release := Normal;
      case Mode is
         when Refuse => Status := C.Values.Invalid_Value;
         when Unclear => Status := C.Values.Available;
         when Normal | Clear_Error =>
            C.Release_Result (Service, Handle, Status);
            Check (Status = C.Values.Available and then Handle = C.Values.No_Value,
               "actual release completes");
            if Mode = Clear_Error then Status := C.Values.Invalid_Value; end if;
      end case;
   end Release;
   package R is new ACPI_Region_Initialization
      (C, Owner_Context, Ready, PCI_Root, Invoke, Release_Result => Release);
   use type R.Phase, R.Cleanup_Action;
   Service, Foreign : aliased C.State (1, 4096, 4096, Load_Capacity => 1);
   Run : R.Session;
   Accepted : Boolean;
   Action : R.Cleanup_Action;
   procedure Prepare (State : aliased in out C.State; Image : Firmware_Tables.Bytes) is
      Stage : C.Stage_Status;
      Freeze : C.Freeze_Status;
      Report : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      C.Stage_Table (State, 1, C.DSDT, Image, Stage); Check (Stage = C.Staged, "stage");
      C.Freeze_Catalog (State, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (State, 1000, Report, Status);
      Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed, "modules");
      C.Initialize_Members (State, Members, Status); Check (Status = C.Values.Available, "members");
   end Prepare;
begin
   Check (Ada.Command_Line.Argument_Count = 2, "fixture/scenario");
   Selected := Scenario'Value (Ada.Command_Line.Argument (2));
   declare
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type;
      Data : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      IO.Read (File, Data, Last); IO.Close (File);
      declare Image : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Firmware_Tables.Byte (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Image); Prepare (Foreign, Image);
      end;
   end;
   R.Close (Run, Service, Action);
   Check (Action = R.No_Pending_Result and then Owner.Releases = 0, "fresh cleanup is inert");
   Next_Release := (case Selected is
      when Overcharge_Refused | Refused_Release => Refuse,
      when Uncleared_Release => Unclear,
      when Cleared_Failure => Clear_Error,
      when others => Normal);
   R.Connect (Run, Service, Owner, 1000, Accepted);
   Check (Accepted and then R.Observe (Run).Charged <= 1000, "consumed/bounded report");
   if Selected in Ordinary | Disconnect_Refused then
      Check (R.Observe (Run).Stage = R.Connected and then Owner.Calls = 2
         and then Owner.Releases = 2 and then not R.Pending (Run), "two ordinary notices cleaned");
      if Selected = Disconnect_Refused then Next_Release := Refuse; end if;
      R.Disconnect (Run, Service, Owner, 1000, Accepted);
      Check (Accepted, "disconnect consumed");
      if Selected = Ordinary then
         Check (R.Observe (Run).Stage = R.Disconnected and then Owner.Calls = 4
            and then Owner.Releases = 4, "normal disconnect cleans both results");
      else
         Check (R.Observe (Run).Stage = R.Failed and then Owner.Calls = 3
            and then Owner.Releases = 3, "failed disconnect stops before second notice");
      end if;
   else
      Check (R.Observe (Run).Stage = R.Failed and then Owner.Calls = 1
         and then R.Observe (Run).Methods = 1, "failure stops before second notice");
      if Selected in Overcharge | Overcharge_Refused then
         Check (R.Observe (Run).Charged = 0 and then R.Observe (Run).Reported_Charge = 1001,
            "overcharge preserved without clamping");
      end if;
      Check (Owner.Releases = (if Selected = Missing_Handle then 0 else 1), "one release attempt per returned handle");
   end if;
   if Selected in Overcharge_Refused | Refused_Release | Uncleared_Release | Disconnect_Refused then
      Check (R.Pending (Run) and then C.Retained_Results (Service) = 1, "session keeps failed release handle");
      declare
         Before : constant R.Report := R.Observe (Run);
         Releases : constant Natural := Owner.Releases;
         use type R.Report;
      begin
         R.Close (Run, Foreign, Action);
         Check (Action = R.Wrong_Context and then R.Pending (Run)
            and then Owner.Releases = Releases and then R.Observe (Run) = Before, "foreign cleanup preserves all state");
         R.Connect (Run, Service, Owner, 1000, Accepted);
         Check (not Accepted and then R.Pending (Run), "pending connect cannot replay");
         R.Disconnect (Run, Service, Owner, 1000, Accepted);
         Check (not Accepted and then R.Pending (Run), "pending disconnect cannot replay");
         R.Close (Run, Service, Action);
         Check (Action = R.Attempted and then not R.Pending (Run)
            and then Owner.Releases = Releases + 1 and then R.Observe (Run).Stage = R.Failed,
            "cleanup releases once without reopening failure");
      end;
   end if;
   declare Before : constant Natural := Owner.Calls; Releases : constant Natural := Owner.Releases; begin
      R.Connect (Run, Service, Owner, 1000, Accepted);
      Check (not Accepted and then Owner.Calls = Before, "connect cannot replay");
      R.Disconnect (Run, Service, Owner, 1000, Accepted);
      Check (not Accepted and then Owner.Calls = Before, "disconnect cannot replay");
      R.Close (Run, Service, Action);
      Check (Action = R.No_Pending_Result and then Owner.Releases = Releases, "completed cleanup is inert");
   end;
   Check (C.Valid (Service) and then C.Valid (Foreign) and then not R.Pending (Run)
      and then C.Retained_Results (Service) = 0 and then C.Retained_Results (Foreign) = 0,
      "valid owners and no retained results lost");
   Ada.Text_IO.Put_Line ("Region cleanup checks" & Checks'Image);
end Region_Cleanup_Tests;
