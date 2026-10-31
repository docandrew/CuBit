pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Coercion_Tests is
   package C renames ACPI_Service;
   package V renames C.Values;
   package N renames C.Namespace;
   use type Ada.Streams.Stream_Element_Offset;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   use type V.Access_Status, V.Module_Outcome;
   use type C.Stage_Status, C.Freeze_Status, C.Retained_Table_Phase;
   use type Firmware_Tables.Byte;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Read_Table (Argument : Positive) return Firmware_Tables.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      F : IO.File_Type;
   begin
      IO.Open (F, IO.In_File, Ada.Command_Line.Argument (Argument));
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (F)));
         Last : Ada.Streams.Stream_Element_Offset;
         Bytes : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (F, Raw, Last); IO.Close (F); Check (Last = Raw'Last, "complete fixture");
         for I in Bytes'Range loop Bytes (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Bytes;
      end;
   end Read_Table;
   Boot : constant Firmware_Tables.Bytes := Read_Table (1);
   Added : constant Firmware_Tables.Bytes := Read_Table (2);
   Service : aliased C.State (2, Boot'Length + Added'Length, Positive'Max (Boot'Length, Added'Length), 2);
   Mode : constant String := Ada.Command_Line.Argument (3);
   Baseline : constant Boolean := Ada.Command_Line.Argument (4) = "baseline";
   Want_Load : constant Boolean := Mode = "strings" or Mode = "string-oem" or
     (not Baseline and then
       (Mode in "empty-buffer-oem" | "empty-buffer-table" | "buffer-oem" | "buffer-root" | "buffer-parameter"
         or else (Mode = "integer-table" and Boot (9) = 1)));
   Stage : C.Stage_Status;
   Frozen : C.Freeze_Status;
   Execution : C.Execution_Report;
   Initial : N.Initialization_Report;
   Status : V.Access_Status;
   Result : V.Result;
   Node, Mark : N.Node_ID;
   Definition : N.Definition_Identity;
begin
   C.Stage_Table (Service, 1, C.DSDT, Boot, Stage); Check (Stage = C.Staged, "stage boot");
   C.Stage_Table (Service, 2, C.Description, Added, Stage); Check (Stage = C.Staged, "stage additional");
   C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
   C.Execute_Frozen (Service, 10000, Execution, Status);
   Check (Status = V.Available and then Execution.Modules.Outcome = V.Modules_Completed, "boot module");
   C.Initialize_Members (Service, Initial, Status); Check (Status = V.Available, "initialized owner");
   C.Find_Child (Service, N.Root, "GO00", Node, Status); Check (Status = V.Available, "method");
   C.Invoke_Retained (Service, Node, [others => 0], 0, 0, Result, Status);
   Check (Result.Status = AML_Execute.Budget_Exceeded and Result.Charged = 0, "zero budget executes nothing");
   Check (C.Retained_Load_State (Service, 2) = C.Retained, "zero budget leaves catalog entry reusable");
   C.Invoke_Retained (Service, Node, [others => 0], 0, 10000, Result, Status);
   Check (Status = V.Available, "owner invocation boundary");
   if Want_Load then
      Check (Result.Status = AML_Execute.Returned and then Result.Number =
        (if Boot (9) = 1 then 16#FFFF_FFFF# else 16#FFFF_FFFF_FFFF_FFFF#), "successful converted selection");
      Check (C.Retained_Load_State (Service, 2) = C.Loaded_Definition, "definition consumed exactly once");
      Definition := C.Retained_Definition (Service, 2);
      Check (N.Has_Definition (Definition), "loaded definition identity");
      C.Find_Child (Service, N.Root, "MARK", Mark, Status); Check (Status = V.Available, "module side effect present");
      C.Observe_Named_Value (Service, Mark, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Returned and then Result.Number = 91, "module value");
   else
      Check (Result.Status = AML_Execute.Unsupported_Value, "conversion/extent refusal before module");
      Check (C.Retained_Load_State (Service, 2) = C.Retained, "refusal does not consume table");
      Check (not N.Has_Definition (C.Retained_Definition (Service, 2)), "refusal issues no consumed definition");
      C.Find_Child (Service, N.Root, "MARK", Mark, Status); Check (Status = V.Available and Mark = N.Root, "refusal creates no namespace effect");
   end if;
   Check (C.Valid (Service), "owner invariant");
   Ada.Text_IO.Put_Line ("LoadTable coercion checks" & Checks'Image);
end Coercion_Tests;
