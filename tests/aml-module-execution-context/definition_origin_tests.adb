pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service;
with AML_Decode;
with AML_Names;
with AML_Execute;
with AML_Table_Backing;
with Firmware_Tables;
procedure Definition_Origin_Tests is
   package N renames ACPI_Service.Namespace;
   package V renames ACPI_Service.Values;
   use type Ada.Streams.Stream_Element_Offset;
   use type N.Definition_Identity, N.Object_Kind;
   use type V.Access_Status, V.Preparation_Outcome, V.Module_Outcome, V.Append_Outcome;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value, AML_Decode.Byte;
   type Names is array (Positive range <>) of AML_Names.Segment;
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
         IO.Read (File, Data, Last); IO.Close (File); Check (Last = Data'Last, "whole table");
         for I in Result'Range loop Result (I) := Firmware_Tables.Byte (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Result;
      end;
   end Read_Table;
   Boot : constant Firmware_Tables.Bytes := Read_Table (1);
   Extra : constant Firmware_Tables.Bytes := Read_Table (2);
   Added : constant Firmware_Tables.Bytes := Read_Table (3);
   Input : aliased AML_Table_Backing.State (3, Boot'Length + Extra'Length + Added'Length);
   A, Other : V.Arena;
   Status : V.Access_Status;
   Root, Device, Destination, Item, Old_Parameter, Foreign : V.Node_Descriptor;
   Boot_Definition, SSDT_Definition, Added_Definition, Foreign_Definition, Observed : N.Definition_Identity;
   Attempt, Zero_Attempt : V.Append_Attempt;
   Appended : V.Append_Report;
   Outcome : V.Result;
   Width : constant AML_Decode.Integer_Width := (if Boot (9) < 2 then AML_Decode.Bits_32 else AML_Decode.Bits_64);
   procedure Initialize (Owner : in out V.Arena) is
      Prepared : V.Preparation_Outcome;
      Modules : V.Module_Report;
      Members : N.Initialization_Report;
   begin
      V.Reset (Owner, Status); Check (Status = V.Available, "reset");
      V.Prepare_Firmware_Namespace (Owner, Prepared, Status); Check (Status = V.Available and then Prepared = V.Firmware_Prepared, "firmware seeds");
      V.Execute_Modules (Owner, Input, 10000, Modules, Status);
      Check (Status = V.Available and then Modules.Outcome = V.Modules_Completed and then Modules.Completed = 2, "two boot definitions");
      V.Seal_With_Tables (Owner, Input, Members, Status); Check (Status = V.Available, "ready");
   end Initialize;
   procedure Child (Owner : V.Arena; Parent : N.Node_ID; Name : AML_Names.Segment; Descriptor : out V.Node_Descriptor) is
      Node : N.Node_ID;
   begin
      V.Find_Child (Owner, Parent, Name, Node, Status); Check (Status = V.Available and then Node /= N.Root, "find " & Name);
      V.Describe_Node (Owner, Node, Descriptor, Status); Check (Status = V.Available, "describe " & Name);
   end Child;
   procedure Source_Is (Descriptor : V.Node_Descriptor; Expected : N.Definition_Identity) is
      Before : constant V.Audit_Value := V.Audit (A);
      use type V.Audit_Value;
   begin
      V.Describe_Definition (A, Descriptor, Observed, Status);
      Check (Status = V.Available and then Observed = Expected and then V.Audit (A) = Before, "read-only definition provenance");
   end Source_Is;
begin
   Input.Count := 3;
   Input.Tables := [(0, Boot'Length), (Boot'Length, Extra'Length), (Boot'Length + Extra'Length, Added'Length)];
   Input.Data.Bytes (1 .. Boot'Length) := Boot;
   Input.Data.Bytes (Boot'Length + 1 .. Boot'Length + Extra'Length) := Extra;
   Input.Data.Bytes (Boot'Length + Extra'Length + 1 .. Input.Byte_Capacity) := Added;
   Initialize (A); Initialize (Other);
   V.Describe_Node (A, N.Root, Root, Status); Check (Status = V.Available, "root"); Source_Is (Root, N.No_Definition);
   Child (A, N.Root, "_SB_", Item); Source_Is (Item, N.No_Definition);
   Child (A, N.Root, "DEV0", Device); V.Describe_Definition (A, Device, Boot_Definition, Status);
   Check (Status = V.Available and then N.Has_Definition (Boot_Definition), "DSDT identity");
   Child (A, N.Root, "DST0", Destination); Source_Is (Destination, Boot_Definition);
   Child (A, N.Root, "SSV0", Item); V.Describe_Definition (A, Item, SSDT_Definition, Status);
   Check (Status = V.Available and then N.Has_Definition (SSDT_Definition) and then SSDT_Definition /= Boot_Definition, "SSDT distinct identity");
   Child (Other, N.Root, "DEV0", Foreign); V.Describe_Definition (Other, Foreign, Foreign_Definition, Status);
   Check (Status = V.Available and then Foreign_Definition /= Boot_Definition and then Foreign_Definition /= SSDT_Definition, "independent arena source identity");
   V.Describe_Definition (A, Foreign, Observed, Status); Check (Status = V.Invalid_Value and then Observed = N.No_Definition, "foreign descriptor refused");
   V.Append_Ready_Module (A, Input, 3, Width, 0, Zero_Attempt, Appended, Status, (V.Selected_Root, Destination));
   Check (Status = V.Available and then Appended.Outcome = V.Append_Budget_Refused and then Appended.Definition = N.No_Definition, "no definition on pre-effect refusal");
   V.Append_Ready_Module (A, Input, 3, Width, 10000, Attempt, Appended, Status, (V.Selected_Root, Destination));
   Check (Status = V.Available and then Appended.Outcome = V.Append_Completed, "append definition"); Added_Definition := Appended.Definition;
   Check (V.Consumed (Attempt) and then V.Attempt_Definition (Attempt) = Added_Definition
     and then not V.Consumed (Zero_Attempt) and then V.Attempt_Definition (Zero_Attempt) = N.No_Definition,
     "attempt retains source identity exactly when consumed");
   Check (N.Has_Definition (Added_Definition) and then Added_Definition /= Boot_Definition and then Added_Definition /= SSDT_Definition, "append has fresh source identity");
   for Name of Names'["NEWV", "NVAL", "RGN1", "FLD1", "TRG1", "HDR1", "BUF1", "BFL1", "EVT1", "MTX1", "DVC1", "LNK1"] loop
      Child (A, V.Position (Destination), Name, Item); Source_Is (Item, Added_Definition);
   end loop;
   Child (A, V.Position (Destination), "DVC1", Item); Child (A, V.Position (Item), "_HID", Item); Source_Is (Item, Added_Definition);
   Source_Is (Device, Boot_Definition); Source_Is (Destination, Boot_Definition);
   Child (A, N.Root, "PRM0", Old_Parameter); Source_Is (Old_Parameter, Boot_Definition);
   Child (A, N.Root, "RETP", Item); V.Evaluate_Named (A, Input, Item, Width, 1000, Outcome, Status);
   Check (Status = V.Available and then Outcome.Status = AML_Execute.Returned, "retag existing binding");
   Check (V.Current_Binding (A, Old_Parameter) and then not V.Current_Node (A, Old_Parameter), "kind change preserves binding identity");
   Source_Is (Old_Parameter, Boot_Definition);
   Child (A, V.Position (Destination), "NVAL", Item); V.Evaluate_Named (A, Input, Item, Width, 1000, Outcome, Status);
   Check (Status = V.Available and then Outcome.Status = AML_Execute.Returned and then Outcome.Number = 16#2A#, "table-owned method remains callable"); Source_Is (Item, Added_Definition);
   V.Append_Ready_Module (A, Input, 3, Width, 10000, Attempt, Appended, Status, (V.Selected_Root, Destination));
   Check (Status = V.Wrong_Phase and then Appended.Outcome = V.Append_Already_Consumed
     and then Appended.Definition = Added_Definition and then V.Attempt_Definition (Attempt) = Added_Definition,
     "consumed attempt refuses replay while preserving identity");
   Initialize (A); V.Describe_Definition (A, Device, Observed, Status); Check (Status = V.Invalid_Value and then Observed = N.No_Definition, "reset invalidates old descriptor");
   Child (A, N.Root, "DEV0", Item); V.Describe_Definition (A, Item, Observed, Status);
   Check (Status = V.Available and then Observed /= Boot_Definition and then Observed /= Added_Definition and then Observed /= SSDT_Definition, "definition identities never reused after reset");
   Ada.Text_IO.Put_Line ("Definition origin checks" & Checks'Image);
end Definition_Origin_Tests;
