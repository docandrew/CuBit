pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Bad_Table_Length_Tests is
   Boot_Quota : constant Positive := 2;
   Copy_Quota : constant Natural := 2;
   Registry_Quota : constant Positive := Boot_Quota + Copy_Quota;
   Fuel : constant Natural := 10000;
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Copied_Table_Capacity => Copy_Quota,
      Copied_Byte_Capacity => 4096);
   package V renames C.Values;
   package N renames C.Namespace;
   package F renames Firmware_Tables;
   use type Ada.Streams.Stream_Element_Offset;
   use type C.Stage_Status, C.Freeze_Status, C.Import_Phase, C.Execution_Phase_Result;
   use type V.Access_Status, V.Module_Outcome, AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value, F.Byte, N.Definition_Identity;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Read (Argument : Positive) return F.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (Argument));
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : F.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File); Check (Last = Raw'Last, "whole fixture");
         for I in Data'Range loop Data (I) := F.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read;
   Boot : constant F.Bytes := Read (1);
   Wrapper : F.Bytes := Read (2);
   Short : constant Boolean := Ada.Command_Line.Argument (3) = "bad-header-length";
   Service : aliased C.State (Boot_Quota, Boot'Length + Wrapper'Length, Positive'Max (Boot'Length, Wrapper'Length), Registry_Quota);
   Stage : C.Stage_Status;
   Freeze : C.Freeze_Status;
   Status : V.Access_Status;
   Execution : C.Execution_Report;
   Initial : N.Initialization_Report;
   Item : C.Node_Descriptor;
   Node : N.Node_ID;
   Value : V.Result;
   Root_Definition, Added_Definition : N.Definition_Identity;
   Success : constant AML_Decode.Integer_Value :=
     (if Boot (9) < 2 then 16#FFFF_FFFF# else AML_Decode.Integer_Value'Last);
   procedure Find (Name : String) is
   begin
      C.Find_Child (Service, N.Root, Name, Node, Status);
      Check (Status = V.Available and then Node /= N.Root, "find " & Name);
      C.Describe_Node (Service, Node, Item, Status); Check (Status = V.Available, "describe " & Name);
   end Find;
   procedure Value_Is (Name : String; Expected : AML_Decode.Integer_Value) is
   begin
      Find (Name); C.Evaluate_Named (Service, Item, Fuel, Value, Status);
      Check (Status = V.Available and then Value.Status = AML_Execute.Returned
        and then Value.Number = Expected, "value " & Name);
   end Value_Is;
begin
   C.Stage_Table (Service, 11, C.DSDT, Boot, Stage); Check (Stage = C.Staged, "boot");
   C.Stage_Table (Service, 12, C.Description, Wrapper, Stage); Check (Stage = C.Staged, "wrapper");
   Wrapper := [others => 0];
   C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
   C.Execute_Frozen (Service, Fuel, Execution, Status);
   Check (Status = V.Available and then Execution.Phase = C.Module_Execution_Reported
     and then Execution.Modules.Outcome = V.Modules_Completed, "startup");
   C.Initialize_Members (Service, Initial, Status); Check (Status = V.Available, "ready");
   Value_Is ("BCNT", 0); Value_Is ("LTGT", 0);
   Find ("LDTB"); C.Describe_Definition (Service, Item, Root_Definition, Status);
   Check (Status = V.Available and then Root_Definition = C.Retained_Definition (Service, 1), "boot registry identity");
   C.Invoke_With_Arguments (Service, Node, [others => (V.Immediate_Argument, 0)], 0, 1, Value, Status);
   Check (Status = V.Available and then Value.Status = AML_Execute.Budget_Exceeded, "pre-effect fuel refusal " & Status'Image & " " & Value.Status'Image);
   Value_Is ("BCNT", 0); Value_Is ("LTGT", 0);
   if Short then
      Find ("LDTB");
      C.Invoke_With_Arguments (Service, Node, [others => (V.Immediate_Argument, 0)], 0, Fuel, Value, Status);
      Check (Status = V.Available and then Value.Status = AML_Execute.Invalid_Table_Length,
        "short table field refusal " & Status'Image & " " & Value.Status'Image);
      Value_Is ("BCNT", 0); Value_Is ("LTGT", 0); Value_Is ("TTYP", 1);
      C.Find_Child (Service, N.Root, "DNUM", Node, Status);
      Check (Status = V.Available and then Node = N.Root, "no child declaration after short refusal");
      Check (C.Valid (Service) and then C.Current_Import_Phase (Service) = C.Catalog_Executed,
        "short refusal preserves ready owner");
      Ada.Text_IO.Put_Line ("Table field checks" & Checks'Image); return;
   end if;
   Value_Is ("LDTB", Success); Value_Is ("LTGT", Success); Value_Is ("TTYP", 1);
   Value_Is ("BCNT", 1); Value_Is ("DNUM", 42);
   Find ("DNUM"); C.Describe_Definition (Service, Item, Added_Definition, Status);
   Check (Status = V.Available and then N.Has_Definition (Added_Definition)
     and then Added_Definition /= Root_Definition, "copied definition owns method");
   Find ("LDTB");
   C.Invoke_With_Arguments (Service, Node, [others => (V.Immediate_Argument, 0)], 0, Fuel, Value, Status);
   Check (Status = V.Available and then Value.Status = AML_Execute.Duplicate_Name, "duplicate consumed bytes do not replay");
   Value_Is ("BCNT", 1); Value_Is ("DNUM", 42);
   Check (C.Valid (Service) and then C.Current_Import_Phase (Service) = C.Catalog_Executed
     and then C.Table_Count (Service) = 2, "ready owner and immutable boot catalog");
   Ada.Text_IO.Put_Line ("Table field checks" & Checks'Image);
end Bad_Table_Length_Tests;
