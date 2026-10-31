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
procedure Busy_Release_Tests is
   procedure Delay_Callback (Item : AML_Delays.Request; Outcome : out AML_Delays.Outcome);
   package C is new ACPI_Service_Core (Perform_Delay => Delay_Callback);
   package V renames C.Values;
   package N renames C.Namespace;
   use type Ada.Streams.Stream_Element_Offset;
   use type C.Stage_Status, C.Freeze_Status;
   use type V.Access_Status, V.Module_Outcome, V.Value_Handle;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Read_Table return Firmware_Tables.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      F : IO.File_Type;
   begin
      IO.Open (F, IO.In_File, Ada.Command_Line.Argument (1));
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (F)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (F, Raw, Last); IO.Close (F); Check (Last = Raw'Last, "whole busy fixture");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Boot : constant Firmware_Tables.Bytes := Read_Table;
   Service : aliased C.State (1, Boot'Length, Boot'Length, 1);
   Pin, Saved : V.Value_Handle;
   Calls : Natural := 0;
   Busy_Preserved : Boolean := False;
   Callback_Status : V.Access_Status := V.Invalid_Value;
   Callback_Count : Natural := 0;
   procedure Delay_Callback (Item : AML_Delays.Request; Outcome : out AML_Delays.Outcome) is
      pragma Unreferenced (Item);
      Status : V.Access_Status;
      Before : constant N.Owned.Retained_Root_Count := C.Retained_Results (Service);
   begin
      Calls := Calls + 1;
      C.Release_Result (Service, Pin, Status);
      Callback_Status := Status; Callback_Count := C.Retained_Results (Service);
      Busy_Preserved := Status = V.Busy and then Pin = Saved and then Before > 0
        and then C.Retained_Results (Service) = Before;
      Outcome := AML_Delays.Completed;
   end Delay_Callback;
   Stage : C.Stage_Status;
   Frozen : C.Freeze_Status;
   Execution : C.Execution_Report;
   Initial : N.Initialization_Report;
   Status : V.Access_Status;
   Node : N.Node_ID;
   Descriptor : C.Node_Descriptor;
   Result : V.Result;
begin
   C.Stage_Table (Service, 1, C.DSDT, Boot, Stage); Check (Stage = C.Staged, "stage busy fixture");
   C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze busy fixture");
   C.Execute_Frozen (Service, 10000, Execution, Status);
   Check (Status = V.Available and then Execution.Modules.Outcome = V.Modules_Completed, "boot busy fixture");
   C.Initialize_Members (Service, Initial, Status); Check (Status = V.Available, "ready busy owner");
   C.Find_Child (Service, N.Root, "PKG0", Node, Status); Check (Status = V.Available and then Node /= N.Root, "busy package");
   C.Describe_Node (Service, Node, Descriptor, Status); Check (Status = V.Available, "busy package descriptor");
   C.Evaluate_Named (Service, Descriptor, 1000, Result, Status);
   Check (Status = V.Available and then Result.Status = AML_Execute.Object_Returned, "retain before callback");
   Pin := Result.Handle; Saved := Pin;
   C.Find_Child (Service, N.Root, "BSY0", Node, Status); Check (Status = V.Available and then Node /= N.Root, "busy method");
   C.Invoke_Retained (Service, Node, [others => 0], 0, 1000, Result, Status);
   Check (Status = V.Available and then Result.Status = AML_Execute.Returned and then Result.Number = 1, "method completes after callback");
   Check (Calls = 1 and then Busy_Preserved and then Pin = Saved and then C.Retained_Results (Service) = 1,
     "in-progress release remains Busy and cannot drop the caller pin: calls=" & Calls'Image &
       " status=" & Callback_Status'Image & " count=" & Callback_Count'Image);
   C.Release_Result (Service, Pin, Status);
   Check (Status = V.Available and then Pin = V.No_Value and then C.Retained_Results (Service) = 0, "idle caller can release after invocation");
   Check (C.Valid (Service), "busy owner invariant");
   Ada.Text_IO.Put_Line ("Busy release checks" & Checks'Image);
end Busy_Release_Tests;
