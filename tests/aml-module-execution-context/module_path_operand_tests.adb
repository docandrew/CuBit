pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service;
with ACPI_Module_Paths;
with AML_Names;
with AML_Decode;
with AML_Execute;
with AML_Table_Backing;
with Firmware_Tables;
procedure Module_Path_Operand_Tests is
   package N renames ACPI_Service.Namespace;
   package V renames ACPI_Service.Values;
   package Paths is new ACPI_Module_Paths (ACPI_Service);
   use type Ada.Streams.Stream_Element_Offset;
   use type Firmware_Tables.Bytes, Firmware_Tables.Byte;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   use type V.Append_Outcome;
   use type V.Access_Status, V.Preparation_Outcome, V.Module_Outcome;
   use type V.Node_Descriptor, V.Audit_Value;
   use type Paths.Selection_Status, AML_Names.Parse_Status, N.Lookup_Status;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why; end if;
   end Check;
   function Read_Table (Argument : Positive) return Firmware_Tables.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (Argument));
      declare
         Raw : Ada.Streams.Stream_Element_Array
           (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File);
         Check (Last = Raw'Last, "whole copied fixture");
         for I in Data'Range loop
            Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I)));
         end loop;
         return Data;
      end;
   end Read_Table;
   Boot : constant Firmware_Tables.Bytes := Read_Table (1);
   Added : constant Firmware_Tables.Bytes := Read_Table (2);
   Input : aliased AML_Table_Backing.State (2, Boot'Length + Added'Length);
   Arena, Other : V.Arena;
   Root, Device, Destination, Caller, Stale, Foreign, System_Bus : V.Node_Descriptor;
   Root_Parameter, Device_Parameter, Destination_Parameter : V.Node_Descriptor;
   Status : V.Access_Status;
   procedure Initialize (Owner : in out V.Arena) is
      Prepared : V.Preparation_Outcome;
      Modules : V.Module_Report;
      Initial : N.Initialization_Report;
   begin
      V.Reset (Owner, Status); Check (Status = V.Available, "reset");
      V.Prepare_Firmware_Namespace (Owner, Prepared, Status);
      Check (Status = V.Available and then Prepared = V.Firmware_Prepared, "firmware names");
      V.Execute_Modules (Owner, Input, 10000, Modules, Status);
      Check (Status = V.Available and then Modules.Outcome = V.Modules_Completed, "boot module");
      V.Seal_With_Tables (Owner, Input, Initial, Status);
      Check (Status = V.Available, "ready namespace");
   end Initialize;
   procedure Describe_Child (Parent : V.Node_Descriptor; Name : AML_Names.Segment;
      Item : out V.Node_Descriptor) is
      Node : N.Node_ID;
   begin
      V.Find_Child (Arena, V.Position (Parent), Name, Node, Status);
      Check (Status = V.Available and then Node /= N.Root, "fixture child " & Name);
      V.Describe_Node (Arena, Node, Item, Status);
      Check (Status = V.Available, "current child descriptor");
   end Describe_Child;
   procedure Probe (From : V.Node_Descriptor; Root_Text, Parameter_Text : String;
      Expected : Paths.Selection_Status;
      Expected_Root : V.Node_Descriptor := V.No_Node;
      Expected_Parameter : V.Node_Descriptor := V.No_Node;
      Expected_Access : V.Access_Status := V.Available;
      Syntax : AML_Names.Parse_Status := AML_Names.Malformed;
      Lookup : N.Lookup_Status := N.Not_Found) is
      Result : Paths.Selection;
      Before : constant V.Audit_Value := V.Audit (Arena);
      Pins : constant Natural := V.Retained_Count (Arena);
   begin
      Paths.Select_Paths (Arena, From, Root_Text, Parameter_Text, Result, Status);
      Check (Status = Expected_Access and then Result.Status = Expected, "typed operand verdict");
      case Result.Status is
         when Paths.Selected =>
            Check (Result.Root = Expected_Root and then V.Current_Node (Arena, Result.Root),
              "exact canonical root descriptor");
            Check (Result.Has_Parameter = (Parameter_Text'Length /= 0)
              and then Result.Parameter = Expected_Parameter
              and then (not Result.Has_Parameter or else V.Current_Node (Arena, Result.Parameter)),
              "exact optional parameter descriptor");
         when Paths.Root_Syntax_Error | Paths.Parameter_Syntax_Error =>
            Check (Result.Syntax = Syntax, "syntax reason retained");
         when Paths.Root_Lookup_Error | Paths.Parameter_Lookup_Error =>
            Check (Result.Lookup = Lookup, "lookup reason retained");
         when Paths.Caller_Rejected | Paths.Root_Type_Error => null;
      end case;
      Check (V.Audit (Arena) = Before and then V.Retained_Count (Arena) = Pins,
        "selection never executes AML or stores parameter data");
   end Probe;
begin
   Input.Count := 2;
   Input.Tables (1) := (0, Boot'Length); Input.Tables (2) := (Boot'Length, Added'Length);
   Input.Data.Bytes := Boot & Added;
   Probe (V.No_Node, "", "", Paths.Caller_Rejected, Expected_Access => V.Wrong_Phase);
   Initialize (Arena);
   V.Describe_Node (Arena, N.Root, Root, Status);
   Check (Status = V.Available, "root descriptor");
   Describe_Child (Root, "DEV0", Device);
   Describe_Child (Root, "DST0", Destination);
   Describe_Child (Device, "CALL", Caller);
   Describe_Child (Root, "PRM0", Root_Parameter);
   Describe_Child (Device, "PRM0", Device_Parameter);
   Describe_Child (Destination, "PRM0", Destination_Parameter);
   Describe_Child (Root, "_SB_", System_Bus);
   Probe (Caller, "", "", Paths.Selected, Root);
   Probe (Caller, "", "PRM0", Paths.Selected, Root, Root_Parameter);
   Probe (Caller, "DST0", "PRM0", Paths.Selected, Destination, Destination_Parameter);
   Probe (Caller, "DST0", "^PRM0", Paths.Selected, Destination, Device_Parameter);
   Probe (Caller, "DST0", "\PRM0", Paths.Selected, Destination, Root_Parameter);
   Probe (Caller, "DST0", "^^PRM0", Paths.Selected, Destination, Root_Parameter);
   Probe (Caller, "DST0", "^", Paths.Selected, Destination, Device);
   Probe (Caller, "^", "PRM0", Paths.Selected, Device, Device_Parameter);
   Probe (Caller, "\DEV0", "PRM0", Paths.Selected, Device, Device_Parameter);
   Probe (Caller, "ALS0", "PRM0", Paths.Selected, Device, Device_Parameter);
   Probe (Caller, "\\dst0", "prm0", Paths.Selected, Destination, Destination_Parameter);
   Probe (Caller, "_sb", "", Paths.Selected, System_Bus);
   declare High : constant String (31 .. 34) := "dst0"; begin
      Probe (Caller, High, "PRM0", Paths.Selected, Destination, Destination_Parameter);
   end;
   Probe (Caller, "", "MISS", Paths.Parameter_Lookup_Error);
   Probe (Caller, "MISS", "", Paths.Root_Lookup_Error);
   Probe (Caller, "^^^", "", Paths.Root_Lookup_Error, Lookup => N.Above_Root);
   Probe (Caller, "DST0", "^^^", Paths.Parameter_Lookup_Error, Lookup => N.Above_Root);
   Probe (Caller, "PRM0", "", Paths.Root_Type_Error, Expected_Access => V.Wrong_Kind);
   Probe (Caller, "CALL", "", Paths.Root_Type_Error, Expected_Access => V.Wrong_Kind);
   Probe (Caller, "A..B", "", Paths.Root_Syntax_Error);
   Probe (Caller, "ABCDE", "", Paths.Root_Syntax_Error);
   Probe (Caller, "\^DEV0", "", Paths.Root_Syntax_Error);
   Probe (Caller, "DST0", "PRM0.", Paths.Parameter_Syntax_Error);
   Probe (Caller, "DEV0" & Character'Val (0), "", Paths.Root_Syntax_Error);
   Probe (Caller, "DST0", "PRM0" & Character'Val (0), Paths.Parameter_Syntax_Error);
   Probe (Caller, Character'Val (128) & "DEV", "", Paths.Root_Syntax_Error);
   declare
      Too_Many_Prefixes : constant String (1 .. Paths.Prefix_Limit + 1) := [others => '^'];
      Too_Long : constant String (1 .. Paths.Max_Path_Bytes + 1) := [others => 'A'];
      At_Prefix_Limit : constant String (1 .. Paths.Prefix_Limit) := [others => '^'];
   begin
      Probe (Caller, Too_Many_Prefixes, "", Paths.Root_Syntax_Error, Syntax => AML_Names.Limit_Exceeded);
      Probe (Caller, "", Too_Many_Prefixes, Paths.Parameter_Syntax_Error, Syntax => AML_Names.Limit_Exceeded);
      Probe (Caller, Too_Long, "", Paths.Root_Syntax_Error, Syntax => AML_Names.Limit_Exceeded);
      Probe (Caller, "", Too_Long, Paths.Parameter_Syntax_Error, Syntax => AML_Names.Limit_Exceeded);
      Probe (Caller, At_Prefix_Limit, "", Paths.Root_Lookup_Error, Lookup => N.Above_Root);
   end;
   Probe (V.No_Node, "\DST0", "", Paths.Caller_Rejected, Expected_Access => V.Invalid_Value);
   Probe (Root_Parameter, "\DST0", "", Paths.Caller_Rejected, Expected_Access => V.Wrong_Kind);
   Initialize (Other);
   V.Describe_Node (Other, N.Root, Foreign, Status);
   Check (Status = V.Available, "foreign root descriptor");
   Probe (Foreign, "", "", Paths.Caller_Rejected, Expected_Access => V.Invalid_Value);
   Stale := Caller;
   Initialize (Arena);
   Probe (Stale, "", "", Paths.Caller_Rejected, Expected_Access => V.Invalid_Value);
   V.Describe_Node (Arena, N.Root, Root, Status);
   Check (Status = V.Available, "fresh caller after reset");
   declare
      Selected : Paths.Selection;
      Attempt : V.Append_Attempt;
      Report : V.Append_Report;
      Node : N.Node_ID;
      Method_At : V.Node_Descriptor;
      Result : V.Result;
      Width : constant AML_Decode.Integer_Width :=
        (if Boot (9) < 2 then AML_Decode.Bits_32 else AML_Decode.Bits_64);
   begin
      Paths.Select_Paths (Arena, Root, "\DST0", "PRM0", Selected, Status);
      Check (Status = V.Available and then Selected.Status = Paths.Selected,
        "resolved operands available to append caller");
      V.Append_Ready_Module (Arena, Input, 2, Width, 10000, Attempt, Report, Status,
        (V.Selected_Root, Selected.Root));
      Check (Status = V.Available and then Report.Outcome = V.Append_Completed,
        "operand-selected destination accepts real module append");
      V.Evaluate_Named (Arena, Input, Selected.Parameter, Width, 10000, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Returned
        and then Result.Number = 0, "path selection did not perform parameter store");
      V.Find_Child (Arena, V.Position (Selected.Root), "NVAL", Node, Status);
      Check (Status = V.Available and then Node /= N.Root, "new method beneath selected destination");
      V.Describe_Node (Arena, Node, Method_At, Status);
      Check (Status = V.Available, "new method identity");
      V.Evaluate_Named (Arena, Input, Method_At, Width, 10000, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Returned
        and then Result.Number = 42, "new selected-root method executes");
   end;
   Ada.Text_IO.Put_Line ("Module path operand checks" & Checks'Image);
end Module_Path_Operand_Tests;
