pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with Firmware_Tables;
procedure Idle_Session_Tests is
   Provider_Calls : Natural := 0;
   procedure Count_Delay (Item : AML_Delays.Request; Outcome : out AML_Delays.Outcome) is
      pragma Unreferenced (Item);
   begin Provider_Calls := Provider_Calls + 1; Outcome := AML_Delays.Completed; end Count_Delay;
   package C is new ACPI_Service_Core (Perform_Delay => Count_Delay);
   package V renames C.Values;
   package N renames C.Namespace;
   use type Ada.Streams.Stream_Element_Offset;
   use type AML_Decode.Integer_Value;
   use type AML_Execute.Execution_Status;
   use type V.Access_Status, V.Module_Outcome, V.Value_Kind, V.Value_Handle, V.Audit_Value;
   use type C.Stage_Status, C.Freeze_Status, C.Import_Phase, C.Retained_Table_Phase;
   use type C.Retained_Load_Outcome, C.Module_Error_Observation;
   use type N.Definition_Identity, N.Owned.Module_Error_Count;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Read_Table (Argument : Positive) return Firmware_Tables.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (Argument));
      declare
         Data : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Result : Firmware_Tables.Bytes (1 .. Data'Length);
      begin
         IO.Read (File, Data, Last); IO.Close (File); Check (Last = Data'Last, "whole fixture");
         for I in Result'Range loop Result (I) := Firmware_Tables.Byte (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Result;
      end;
   end Read_Table;
   Boot : constant Firmware_Tables.Bytes := Read_Table (1);
   Parent : constant Firmware_Tables.Bytes := Read_Table (2);
   Child : constant Firmware_Tables.Bytes := Read_Table (3);
   Later : constant Firmware_Tables.Bytes := Read_Table (4);
   Mode : constant String := Ada.Command_Line.Argument (5);
   Budget : constant Natural := (if Mode'Length > 7 and then Mode (1 .. 7) = "budget-"
     then Natural'Value (Mode (8 .. Mode'Last)) else 100000);
   Capacity : constant Positive := Boot'Length + Parent'Length + Child'Length + Later'Length;
   Limit : constant Positive := Positive'Max (Positive'Max (Boot'Length, Parent'Length), Positive'Max (Child'Length, Later'Length));
   Service : aliased C.State (4, Capacity, Limit, 4);
   Status : V.Access_Status;
   Execution : C.Execution_Report;
   Initial : N.Initialization_Report;
   Report : C.Retained_Load_Report;
   Parent_ID, Child_ID, Observed : N.Definition_Identity;
   Destination, Node : N.Node_ID;
   Descriptor, Root_Descriptor : C.Node_Descriptor;
   Result : V.Result;
   Pin : V.Value_Handle;
   Description : V.Value_Description;
   Errors : C.Module_Error_Observation;
   procedure Find (Scope : N.Node_ID; Part : String; Item : out N.Node_ID) is
   begin C.Find_Child (Service, Scope, Part, Item, Status); Check (Status = V.Available and then Item /= N.Root, "find " & Part); end Find;
   procedure Value_Is (Scope : N.Node_ID; Part : String; Expected : AML_Decode.Integer_Value) is
      Item : N.Node_ID;
   begin
      Find (Scope, Part, Item); C.Observe_Named_Value (Service, Item, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Returned and then Result.Number = Expected, "value " & Part);
   end Value_Is;
   procedure Definition_Is (Part : String; Expected : N.Definition_Identity) is
      Item : N.Node_ID;
   begin
      Find (Destination, Part, Item); C.Describe_Node (Service, Item, Descriptor, Status); Check (Status = V.Available, "describe " & Part);
      C.Describe_Definition (Service, Descriptor, Observed, Status); Check (Status = V.Available and then Observed = Expected, "definition " & Part);
   end Definition_Is;
   Stage : C.Stage_Status;
   Frozen : C.Freeze_Status;
begin
   Pin := V.No_Value;
   C.Release_Result (Service, Pin, Status);
   Check (Status = V.Wrong_Phase and then Pin = V.No_Value and then C.Retained_Results (Service) = 0,
     "uninitialized owner still refuses release");
   C.Stage_Table (Service, 11, C.DSDT, Boot, Stage); Check (Stage = C.Staged, "stage boot");
   C.Stage_Table (Service, 22, C.Description, Parent, Stage); Check (Stage = C.Staged, "stage parent");
   C.Stage_Table (Service, 33, C.Description, Child, Stage); Check (Stage = C.Staged, "stage child");
   C.Stage_Table (Service, 44, C.SSDT, Later, Stage); Check (Stage = C.Staged, "stage later SSDT");
   C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
   C.Release_Result (Service, Pin, Status);
   Check (Status = V.Wrong_Phase and then Pin = V.No_Value and then C.Retained_Results (Service) = 0,
     "ordinary loading phase still refuses release");
   C.Execute_Frozen (Service, 100000, Execution, Status);
   Check (Status = V.Available and then Execution.Modules.Outcome = V.Modules_Completed, "boot before append");
   C.Initialize_Members (Service, Initial, Status); Check (Status = V.Available and then Initial.Missing = 0 and then Initial.Unsupported = 0, "ready before append");
   C.Release_Result (Service, Pin, Status);
   Check (Status = V.Invalid_Value and then C.Retained_Results (Service) = 0,
     "ready owner rejects empty handle without changing retention");
   Check (C.Retained_Load_State (Service, 2) = C.Retained and then C.Retained_Load_State (Service, 3) = C.Retained, "both sources initially unconsumed");
   Find (N.Root, "DST0", Destination); Find (N.Root, "PKG0", Node);
   C.Describe_Node (Service, Node, Descriptor, Status); Check (Status = V.Available, "package descriptor");
   C.Evaluate_Named (Service, Descriptor, 1000, Result, Status); Check (Status = V.Available and then Result.Status = AML_Execute.Object_Returned, "retain package before append"); Pin := Result.Handle;
   C.Describe_Node (Service, Destination, Root_Descriptor, Status); Check (Status = V.Available, "selected append root");
   C.Append_Retained_Module (Service, 2, Budget, Report, Status, (V.Selected_Root, Root_Descriptor));
   Check (Report.Append.Charged <= Budget, "bounded append charge");
   Parent_ID := C.Retained_Definition (Service, 2); Child_ID := C.Retained_Definition (Service, 3); Errors := C.Module_Errors (Service);
   Check (C.Retained_Load_State (Service, 2) /= C.Loading_Definition and then C.Retained_Load_State (Service, 3) /= C.Loading_Definition, "no outstanding reservation");
   Check (C.Retained_Results (Service) = 1 and then Provider_Calls = 0, "retain pin and inherited provider restrictions");
   C.Describe_Result (Service, Pin, Description, Status); Check ((if C.Current_Import_Phase (Service) = C.Catalog_Failed then Status = V.Wrong_Phase
     else Status = V.Available and then Description.Kind = V.Package_Description), "retained package accessible only in ready phase");
   if Budget = 0 then
      Check (Report.Outcome = C.Retained_Load_Not_Started and then not N.Has_Definition (Parent_ID), "zero budget unconsumed");
      Check (C.Retained_Load_State (Service, 2) = C.Retained and then C.Retained_Load_State (Service, 3) = C.Retained, "zero budget releases reservation");
      C.Append_Retained_Module (Service, 2, 100000, Report, Status, (V.Selected_Root, Root_Descriptor));
      Check (Status = V.Available and then Report.Outcome = C.Retained_Load_Completed, "zero budget permits later real append");
      Parent_ID := C.Retained_Definition (Service, 2); Child_ID := C.Retained_Definition (Service, 3);
   elsif Mode in "parent-fail" | "child-fail" | "self" | "escape" or else Mode in "budget-1" | "budget-64" then
      Check (Report.Outcome = C.Retained_Load_Failed and then C.Current_Import_Phase (Service) = C.Catalog_Failed, "failed append closes service");
      Check (N.Has_Definition (Parent_ID) and then C.Retained_Load_State (Service, 2) = C.Failed_Definition, "outer definition consumed once");
      if Budget < 100000 then Check (Report.Append.Failure = AML_Execute.Budget_Exceeded, "fuel failure is budget exhaustion"); end if;
      if Mode in "parent-fail" | "escape" then
         Check (C.Retained_Load_State (Service, 3) = C.Loaded_Definition, "completed child remains loaded when outer effects retained");
         Value_Is (N.Root, "ORD0", 123); Value_Is (Destination, "PRM0", 77);
      elsif Mode = "child-fail" then
         Check (C.Retained_Load_State (Service, 3) = C.Failed_Definition, "failed child stays consumed");
         Value_Is (N.Root, "ORD0", 12); Value_Is (Destination, "PRM0", 0);
      elsif Mode = "self" then Check (C.Retained_Load_State (Service, 3) = C.Retained, "self-load does not consume unrelated child");
      end if;
      Check (Errors.Count = 0, "unresumable errors not reported as recovered");
      C.Append_Retained_Module (Service, 2, 100000, Report, Status, (V.Selected_Root, Root_Descriptor)); Check (Status = V.Wrong_Phase, "failed append cannot retry");
      Check (C.Retained_Definition (Service, 2) = Parent_ID and then C.Retained_Definition (Service, 3) = Child_ID, "failed identities stable");
      declare
         Foreign : aliased C.State (4, Capacity, Limit, 4);
         Foreign_Result : V.Result;
         Foreign_Pin, Saved_Foreign, Duplicate_Pin : V.Value_Handle;
         Foreign_Node, Method_Node : N.Node_ID;
         Foreign_Descriptor : C.Node_Descriptor;
         Foreign_Execution : C.Execution_Report;
         Foreign_Initial : N.Initialization_Report;
         Before : constant V.Audit_Value := C.Audit (Service);
      begin
         C.Stage_Table (Foreign, 11, C.DSDT, Boot, Stage); Check (Stage = C.Staged, "foreign boot");
         C.Stage_Table (Foreign, 22, C.Description, Parent, Stage); Check (Stage = C.Staged, "foreign parent");
         C.Stage_Table (Foreign, 33, C.Description, Child, Stage); Check (Stage = C.Staged, "foreign child");
         C.Stage_Table (Foreign, 44, C.SSDT, Later, Stage); Check (Stage = C.Staged, "foreign later");
         C.Freeze_Catalog (Foreign, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "foreign freeze");
         C.Execute_Frozen (Foreign, 100000, Foreign_Execution, Status);
         Check (Status = V.Available and then Foreign_Execution.Modules.Outcome = V.Modules_Completed, "foreign boot completes");
         C.Initialize_Members (Foreign, Foreign_Initial, Status); Check (Status = V.Available, "foreign ready");
         C.Find_Child (Foreign, N.Root, "PKG0", Foreign_Node, Status); Check (Status = V.Available and Foreign_Node /= N.Root, "foreign package");
         C.Describe_Node (Foreign, Foreign_Node, Foreign_Descriptor, Status); Check (Status = V.Available, "foreign descriptor");
         C.Evaluate_Named (Foreign, Foreign_Descriptor, 1000, Foreign_Result, Status);
         Check (Status = V.Available and then Foreign_Result.Status = AML_Execute.Object_Returned, "foreign retained result");
         Foreign_Pin := Foreign_Result.Handle; Saved_Foreign := Foreign_Pin;
         C.Release_Result (Service, Foreign_Pin, Status);
         Check (Status = V.Invalid_Value and then Foreign_Pin = Saved_Foreign and then
           C.Retained_Results (Service) = 1 and then C.Retained_Results (Foreign) = 1,
           "failed owner refuses another owner's live handle without consuming either pin");
         C.Describe_Result (Foreign, Foreign_Pin, Description, Status);
         Check (Status = V.Available and then Description.Kind = V.Package_Description, "foreign result remains readable");
         C.Release_Result (Foreign, Foreign_Pin, Status);
         Check (Status = V.Available and then Foreign_Pin = V.No_Value and then C.Retained_Results (Foreign) = 0, "foreign owner releases its own pin");
         Duplicate_Pin := Pin;
         C.Release_Result (Service, Pin, Status);
         Check (Status = V.Available and then Pin = V.No_Value and then C.Retained_Results (Service) = 0,
           "failed owner drops its authenticated caller pin");
         Check (C.Current_Import_Phase (Service) = C.Catalog_Failed and then C.Audit (Service) = Before,
           "pin cleanup preserves failed phase and namespace effects");
         C.Describe_Result (Service, Duplicate_Pin, Description, Status);
         Check (Status = V.Wrong_Phase, "failed retained reads remain closed");
         C.Release_Result (Service, Duplicate_Pin, Status);
         Check (Status = V.Invalid_Value and then Duplicate_Pin /= V.No_Value and then C.Retained_Results (Service) = 0,
           "duplicate release cannot remove another pin or decrement twice");
         C.Release_Result (Service, Pin, Status);
         Check (Status = V.Invalid_Value and then Pin = V.No_Value, "released empty handle remains invalid");
         C.Evaluate_Named (Service, Descriptor, 1000, Result, Status);
         Check (Status = V.Wrong_Phase and then C.Retained_Results (Service) = 0, "cleanup does not permit new retained evaluation");
         C.Find_Child (Service, N.Root, "GO00", Method_Node, Status); Check (Status = V.Available and then Method_Node /= N.Root, "failed namespace method still identifiable");
         C.Invoke_Retained (Service, Method_Node, [others => 0], 0, 1000, Result, Status);
         Check (Status = V.Wrong_Phase and then C.Current_Import_Phase (Service) = C.Catalog_Failed, "cleanup never reopens AML execution");
         Check (C.Retained_Definition (Service, 2) = Parent_ID and then C.Retained_Definition (Service, 3) = Child_ID
           and then C.Module_Errors (Service) = Errors and then Provider_Calls = 0, "cleanup preserves definition, recovery and provider state");
         Check (C.Valid (Service) and then C.Valid (Foreign), "both owners valid after cleanup");
      end;
      Ada.Text_IO.Put_Line ("Idle session checks" & Natural'Image (Checks)); return;
   end if;
   Check (C.Retained_Load_State (Service, 2) = C.Loaded_Definition and then C.Retained_Load_State (Service, 3) = C.Loaded_Definition, "common registry loaded both definitions");
   Check (N.Has_Definition (Parent_ID) and then N.Has_Definition (Child_ID) and then Parent_ID /= Child_ID, "distinct declaration identities");
   Value_Is (N.Root, "ORD0", 123); Value_Is (Destination, "PRM0", 77); Value_Is (Destination, "BEF0", 17); Value_Is (Destination, "AFT0", 19); Value_Is (Destination, "CHLD", 22);
   Definition_Is ("BEF0", Parent_ID); Definition_Is ("AFT0", Parent_ID); Definition_Is ("CHLD", Child_ID);
   if Mode in "parent-recover" | "child-recover" then
      Check (Errors.Count = 1 and then Errors.Definition = (if Mode = "parent-recover" then Parent_ID else Child_ID), "diagnostic belongs to resumed definition");
      Value_Is (Destination, "RSM0", 91);
   else Check (Errors.Count = 0, "clean append has no recovered errors"); end if;
   C.Append_Retained_Module (Service, 2, 100000, Report, Status, (V.Selected_Root, Root_Descriptor));
   Check (Status = V.Available and then Report.Outcome = C.Retained_Load_Already_Present and then Report.Existing_Index = 2, "repeat outer append idempotent");
   C.Append_Retained_Module (Service, 3, 100000, Report, Status, (V.Selected_Root, Root_Descriptor));
   Check (Status = V.Available and then Report.Outcome = C.Retained_Load_Already_Present and then Report.Existing_Index = 3, "nested child also idempotent to idle API");
   Value_Is (N.Root, "ORD0", 123); Check (C.Module_Errors (Service) = Errors, "idempotent calls preserve diagnostics");
   C.Release_Result (Service, Pin, Status); Check (Status = V.Available and then C.Retained_Results (Service) = 0, "release package pin");
   Ada.Text_IO.Put_Line ("Idle session checks" & Natural'Image (Checks));
end Idle_Session_Tests;
