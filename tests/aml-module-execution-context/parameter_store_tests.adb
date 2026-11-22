pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service;
with ACPI_Module_Paths;
with AML_Decode;
with AML_Execute;
with AML_Names;
with AML_Table_Backing;
with Firmware_Tables;
procedure Parameter_Store_Tests is
   package N renames ACPI_Service.Namespace;
   package V renames ACPI_Service.Values;
   package Paths is new ACPI_Module_Paths (ACPI_Service);
   use type Ada.Streams.Stream_Element_Offset;
   use type Firmware_Tables.Bytes, Firmware_Tables.Byte;
   use type AML_Decode.Integer_Value, AML_Decode.Integer_Width;
   use type AML_Execute.Execution_Status;
   use type V.Access_Status, V.Module_Outcome, V.Preparation_Outcome;
   use type V.Audit_Value, V.Append_Outcome, V.Value_Kind;
   use type Paths.Selection_Status;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Read_Table (Argument : Positive) return Firmware_Tables.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      F : IO.File_Type;
   begin
      IO.Open (F, IO.In_File, Ada.Command_Line.Argument (Argument));
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (F)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (F, Raw, Last); IO.Close (F); Check (Last = Raw'Last, "whole fixture");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Boot : constant Firmware_Tables.Bytes := Read_Table (1);
   Added : constant Firmware_Tables.Bytes := Read_Table (2);
   Input : aliased AML_Table_Backing.State (2, Boot'Length + Added'Length);
   Width : constant AML_Decode.Integer_Width :=
     (if Boot (9) < 2 then AML_Decode.Bits_32 else AML_Decode.Bits_64);
   Arena, Other : V.Arena;
   Status : V.Access_Status;
   Root, Target, Old_Target, Buffer_Target, Package_Target, Field_Target : V.Node_Descriptor;
   Held, Other_Held, Old_Held, Source_Package, Destination_Package : V.Value_Handle;
   Result : V.Result;
   procedure Initialize (A : in out V.Arena) is
      Prepared : V.Preparation_Outcome;
      Modules : V.Module_Report;
      Initial : N.Initialization_Report;
   begin
      V.Reset (A, Status); Check (Status = V.Available, "reset");
      V.Prepare_Firmware_Namespace (A, Prepared, Status);
      Check (Status = V.Available and then Prepared = V.Firmware_Prepared, "firmware names");
      V.Execute_Modules (A, Input, 10000, Modules, Status);
      Check (Status = V.Available and then Modules.Outcome = V.Modules_Completed, "boot module");
      V.Seal_With_Tables (A, Input, Initial, Status); Check (Status = V.Available, "ready");
   end Initialize;
   function Named (Name : AML_Names.Segment) return V.Node_Descriptor is
      Node : N.Node_ID;
      Item : V.Node_Descriptor;
   begin
      V.Find_Child (Arena, N.Root, Name, Node, Status);
      Check (Status = V.Available and then Node /= N.Root, "find " & Name);
      V.Describe_Node (Arena, Node, Item, Status); Check (Status = V.Available, "describe " & Name);
      return Item;
   end Named;
   procedure Evaluate (Item : V.Node_Descriptor) is
   begin
      V.Evaluate_Named (Arena, Input, Item, Width, 10000, Result, Status);
      Check (Status = V.Available, "evaluate current object");
   end Evaluate;
   procedure Expect_Integer (Item : V.Node_Descriptor; Number : AML_Decode.Integer_Value) is
   begin
      Evaluate (Item);
      Check (Result.Status = AML_Execute.Returned and then Result.Number = Number, "expected integer value node" & V.Position (Item)'Image & " expected" & Number'Image &
        (if Result.Status = AML_Execute.Returned then " actual" & Result.Number'Image else " status " & Result.Status'Image));
   end Expect_Integer;
   procedure Store (Item : V.Node_Descriptor; Source : V.Argument;
      Expected : AML_Execute.Execution_Status := AML_Execute.Returned;
      Access_Result : V.Access_Status := V.Available;
      Budget : Natural := V.Parameter_Store_Charge; Charge : Natural := V.Parameter_Store_Charge) is
      Before : constant V.Audit_Value := V.Audit (Arena);
      Pins : constant Natural := V.Retained_Count (Arena);
      Charged : Natural;
      Outcome : AML_Execute.Execution_Status;
   begin
      V.Store_Parameter_Data (Arena, Item, Source, Width, Budget, Charged, Outcome, Status);
      Check (Status = Access_Result and then Outcome = Expected and then Charged = Charge,
        "typed bounded Store result");
      Check (V.Valid (Arena) and then V.Retained_Count (Arena) = Pins, "valid Store preserves pins");
      Check ((Status = V.Available and then Outcome = AML_Execute.Returned) or else V.Audit (Arena) = Before,
        "failed Store leaves namespace unchanged");
   end Store;
   procedure First_Element (Handle : V.Value_Handle; Number : AML_Decode.Integer_Value) is
      Element : V.Value_Handle;
      Description : V.Value_Description;
   begin
      V.Read_Element (Arena, Handle, 0, Element, Status); Check (Status = V.Available, "read package child");
      V.Describe (Arena, Element, Description, Status);
      Check (Status = V.Available and then Description.Kind = V.Integer_Description
        and then Description.Number = Number, "package child value");
      V.Release (Arena, Element, Status); Check (Status = V.Available, "release child");
   end First_Element;
begin
   Input.Count := 2; Input.Tables (1) := (0, Boot'Length); Input.Tables (2) := (Boot'Length, Added'Length);
   Input.Data.Bytes := Boot & Added;
   Store (V.No_Node, (V.Immediate_Argument, 1), Access_Result => V.Wrong_Phase,
     Expected => AML_Execute.Unsupported_Value, Charge => 0);
   Initialize (Arena);
   V.Describe_Node (Arena, N.Root, Root, Status); Check (Status = V.Available, "root");
   Target := Named ("PRM0"); Buffer_Target := Named ("SBUF"); Package_Target := Named ("PDST");
   Store (Target, (V.Immediate_Argument, 7)); Expect_Integer (Target, 7);
   Store (Target, (V.Immediate_Argument, 9), Expected => AML_Execute.Budget_Exceeded, Budget => 0, Charge => 0);
   Expect_Integer (Target, 7);
   Store (Root, (V.Immediate_Argument, 9), Expected => AML_Execute.Unsupported_Value,
     Access_Result => V.Wrong_Kind, Charge => 0);
   Store (Named ("RGN0"), (V.Immediate_Argument, 9), Expected => AML_Execute.Unsupported_Value,
     Access_Result => V.Wrong_Kind, Charge => 0);
   Store (Named ("HW00"), (V.Immediate_Argument, 9), Expected => AML_Execute.Unsupported_Value,
     Access_Result => V.Wrong_Kind, Charge => 0);
   V.Create_Buffer (Arena, [16#78#,16#56#,16#34#,16#12#,16#EF#,16#CD#,16#AB#,16#90#], Result, Status);
   Check (Status = V.Available and then Result.Status = AML_Execute.Object_Returned, "buffer source"); Held := Result.Handle;
   Store (Target, (V.Retained_Argument, Held));
   Expect_Integer (Target, (if Width = AML_Decode.Bits_32 then 16#12345678# else 16#90ABCDEF12345678#));
   Old_Held := Held; V.Release (Arena, Held, Status); Check (Status = V.Available, "release source");
   Store (Target, (V.Retained_Argument, Old_Held), Expected => AML_Execute.Unsupported_Value,
     Access_Result => V.Invalid_Value, Charge => 0);
   V.Create_Buffer (Arena, [], Result, Status);
   Check (Status = V.Available and then Result.Status = AML_Execute.Object_Returned, "empty source"); Held := Result.Handle;
   Store (Target, (V.Retained_Argument, Held), Expected => AML_Execute.Empty_Buffer);
   V.Release (Arena, Held, Status); Check (Status = V.Available, "release empty source");
   Evaluate (Named ("STR0")); Check (Result.Status = AML_Execute.Object_Returned, "string source"); Held := Result.Handle;
   Store (Target, (V.Retained_Argument, Held)); Expect_Integer (Target, 42);
   V.Release (Arena, Held, Status); Check (Status = V.Available, "release string source");
   Evaluate (Named ("GET0")); Check (Result.Status = AML_Execute.Object_Returned, "held buffer alias"); Held := Result.Handle;
   Store (Buffer_Target, (V.Immediate_Argument, 16#123456#));
   Field_Target := Named ("BFLD"); Store (Field_Target, (V.Immediate_Argument, 16#EF#));
   declare Data : AML_Decode.Bytes (1 .. 3); Copied : Natural; begin
      V.Read_Bytes (Arena, Held, 0, Data, Copied, Status);
      Check (Status = V.Available and then Copied = 3 and then Data = [16#EF#,16#34#,16#12#], "buffer identity and local field Store");
   end;
   V.Release (Arena, Held, Status); Check (Status = V.Available, "release buffer alias");
   Evaluate (Named ("PKG0")); Check (Result.Status = AML_Execute.Object_Returned, "package source"); Source_Package := Result.Handle;
   Store (Package_Target, (V.Retained_Argument, Source_Package));
   Evaluate (Package_Target); Check (Result.Status = AML_Execute.Object_Returned, "stored package"); Destination_Package := Result.Handle;
   Evaluate (Named ("MUTP")); First_Element (Source_Package, 9); First_Element (Destination_Package, 1);
   V.Release (Arena, Source_Package, Status); Check (Status = V.Available, "release source package");
   V.Release (Arena, Destination_Package, Status); Check (Status = V.Available, "release copied package");
   Evaluate (Named ("RETP"));
   Check (V.Current_Binding (Arena, Target) and then not V.Current_Node (Arena, Target), "live binding survived type change");
   Store (Target, (V.Immediate_Argument, 16#99#)); Expect_Integer (Named ("RDI0"), 99);
   Initialize (Other); V.Create_Buffer (Other, [1], Result, Status);
   Check (Status = V.Available and then Result.Status = AML_Execute.Object_Returned, "foreign source"); Other_Held := Result.Handle;
   Store (Target, (V.Retained_Argument, Other_Held), Expected => AML_Execute.Unsupported_Value,
     Access_Result => V.Invalid_Value, Charge => 0);
   V.Release (Other, Other_Held, Status); Check (Status = V.Available, "release foreign source");
   declare
      Selected : Paths.Selection;
      Attempt : V.Append_Attempt;
      Report : V.Append_Report;
   begin
      Paths.Select_Paths (Arena, Root, "DST0", "PRM0", Selected, Status);
      Check (Status = V.Available and then Selected.Status = Paths.Selected, "preselect parameter before append");
      Store (Selected.Parameter, (V.Immediate_Argument, 16#30#));
      V.Append_Ready_Module (Arena, Input, 2, Width, 10000, Attempt, Report, Status, (V.Selected_Root, Selected.Root));
      Check (Status = V.Available and then Report.Outcome = V.Append_Completed, "module effects complete before Store");
      Expect_Integer (Named ("OBS0"), 16#30#);
      Store (Selected.Parameter, (V.Immediate_Argument, 16#99#));
      Expect_Integer (Selected.Parameter, 16#99#); Expect_Integer (Named ("OBS0"), 16#30#);
   end;
   Old_Target := Target; Initialize (Arena);
   Store (Old_Target, (V.Immediate_Argument, 9), Expected => AML_Execute.Unsupported_Value,
     Access_Result => V.Invalid_Value, Charge => 0);
   Check (V.Retained_Count (Arena) = 0, "no leaked caller pins");
   Ada.Text_IO.Put_Line ("Parameter Store checks" & Checks'Image);
end Parameter_Store_Tests;
