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
procedure Field_Short_Tests is
   Boot_Quota : constant Positive := 1;
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
   use type AML_Decode.Integer_Value;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Read return F.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
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
   Boot : constant F.Bytes := Read;
   Service : aliased C.State (Boot_Quota, Boot'Length, Boot'Length, Registry_Quota);
   Stage : C.Stage_Status;
   Freeze : C.Freeze_Status;
   Status : V.Access_Status;
   Execution : C.Execution_Report;
   Initial : N.Initialization_Report;
   Item : C.Node_Descriptor;
   Node : N.Node_ID;
   Value : V.Result;
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
   C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
   C.Execute_Frozen (Service, Fuel, Execution, Status);
   Check (Status = V.Available and then Execution.Phase = C.Module_Execution_Reported
     and then Execution.Modules.Outcome = V.Modules_Completed, "startup");
   C.Initialize_Members (Service, Initial, Status); Check (Status = V.Available, "ready");
   Value_Is ("BCNT", 0); Value_Is ("LTGT", 0);
   Find ("LDTB"); C.Evaluate_Named (Service, Item, Fuel, Value, Status);
   Check (Status = V.Available and then Value.Status = AML_Execute.Buffer_Field_Limit, "table extent exceeds declared Field bytes");
   Value_Is ("BCNT", 0); Value_Is ("LTGT", 0);
   C.Find_Child (Service, N.Root, "DNUM", Node, Status);
   Check (Status = V.Available and then Node = N.Root, "short Field creates no method");
   Check (C.Valid (Service) and then C.Current_Import_Phase (Service) = C.Catalog_Executed, "short Field leaves owner usable");
   Ada.Text_IO.Put_Line ("Field short checks" & Checks'Image);
end Field_Short_Tests;
