pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service;
with AML_Decode;
with AML_Execute;
with AML_Names;
with AML_Table_Backing;
with Firmware_Tables;
procedure Module_Path_Tests is
   package N renames ACPI_Service.Namespace;
   package V renames ACPI_Service.Values;
   use type Ada.Streams.Stream_Element_Offset;
   use type Firmware_Tables.Bytes, Firmware_Tables.Byte;
   use type V.Access_Status, V.Module_Outcome, V.Preparation_Outcome;
   use type V.Node_Descriptor, V.Audit_Value, V.Append_Outcome;
   use type N.Lookup_Status;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
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
   Status : V.Access_Status;
   Root_At, Device_At, Counter_At, Method_At, Foreign_At, Stale_At, Item : V.Node_Descriptor;
   Device_Node, Counter_Node, Inner_Node : N.Node_ID;
   Outcome : N.Lookup_Status;
   Width : constant AML_Decode.Integer_Width :=
     (if Boot (9) < 2 then AML_Decode.Bits_32 else AML_Decode.Bits_64);
   function Path (First : AML_Names.Segment := "____";
      Second : AML_Names.Segment := "____"; Count : Natural := 1;
      Rooted : Boolean := False; Parents : Natural := 0)
      return AML_Names.Name_Result is
   begin
      return (AML_Names.Accepted, Rooted, Parents, Count,
        [1 => First, 2 => Second, others => "____"], 1);
   end Path;
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
   procedure Probe (Scope : V.Node_Descriptor; Name : AML_Names.Name_Result;
      Expected : N.Lookup_Status; Node : N.Node_ID := N.Root;
      Access_Result : V.Access_Status := V.Available) is
      Before : constant V.Audit_Value := V.Audit (Arena);
      Pins : constant Natural := V.Retained_Count (Arena);
   begin
      V.Resolve_Node (Arena, Scope, Name, Item, Outcome, Status);
      Check (Status = Access_Result and then Outcome = Expected, "typed path verdict");
      Check ((if Status = V.Available and then Outcome = N.Found then
         V.Current_Node (Arena, Item) and then V.Position (Item) = Node
         else Item = V.No_Node), "exact descriptor or absent result");
      Check (V.Audit (Arena) = Before and then V.Retained_Count (Arena) = Pins,
        "lookup does not execute AML or change namespace/pins");
   end Probe;
begin
   Input.Count := 2;
   Input.Tables (1) := (0, Boot'Length);
   Input.Tables (2) := (Boot'Length, Added'Length);
   Input.Data.Bytes := Boot & Added;
   Probe (V.No_Node, Path ("DEV0"), N.Not_Found, Access_Result => V.Wrong_Phase);
   Initialize (Arena);
   V.Describe_Node (Arena, N.Root, Root_At, Status);
   Check (Status = V.Available, "root descriptor");
   V.Find_Child (Arena, N.Root, "DEV0", Device_Node, Status);
   Check (Status = V.Available and then Device_Node /= N.Root, "device");
   V.Describe_Node (Arena, Device_Node, Device_At, Status);
   Check (Status = V.Available, "device descriptor");
   V.Find_Child (Arena, N.Root, "CNT0", Counter_Node, Status);
   Check (Status = V.Available and then Counter_Node /= N.Root, "counter");
   V.Describe_Node (Arena, Counter_Node, Counter_At, Status);
   Check (Status = V.Available, "counter descriptor");
   V.Find_Child (Arena, Device_Node, "INNR", Inner_Node, Status);
   Check (Status = V.Available and then Inner_Node /= N.Root, "inner data");
   V.Find_Child (Arena, N.Root, "GET0", Counter_Node, Status);
   Check (Status = V.Available and then Counter_Node /= N.Root, "method scope");
   V.Describe_Node (Arena, Counter_Node, Method_At, Status);
   Check (Status = V.Available, "method scope descriptor");
   V.Find_Child (Arena, N.Root, "CNT0", Counter_Node, Status);
   Check (Status = V.Available, "counter selector restored");
   Probe (Method_At, Path ("CNT0"), N.Found, Counter_Node);
   Probe (Root_At, Path (Count => 0), N.Found);
   Probe (Device_At, Path (Count => 0), N.Found, Device_Node);
   Probe (Device_At, Path (Count => 0, Rooted => True), N.Found);
   Probe (Root_At, Path ("DEV0"), N.Found, Device_Node);
   Probe (Root_At, Path ("ALS0"), N.Found, Device_Node);
   Probe (Root_At, Path ("ALS0", "INNR", 2), N.Found, Inner_Node);
   Probe (Root_At, Path ("DEV0", "INNR", 2), N.Found, Inner_Node);
   Probe (Device_At, Path ("INNR"), N.Found, Inner_Node);
   Probe (Device_At, Path ("CNT0"), N.Found, Counter_Node);
   Probe (Device_At, Path ("CNT0", Parents => 1), N.Found, Counter_Node);
   Probe (Device_At, Path ("DEV0", "INNR", 2, Rooted => True), N.Found, Inner_Node);
   Probe (Root_At, Path ("MISS"), N.Not_Found);
   Probe (Root_At, Path ("DEV0", "MISS", 2), N.Not_Found);
   Probe (Device_At, Path ("CNT0", Parents => 2), N.Above_Root);
   Probe (Root_At, Path ("DEV0", Rooted => True, Parents => 1), N.Invalid_Path);
   Probe (Root_At, Path ("bad!"), N.Invalid_Path);
   Probe (Root_At, (Kind => AML_Names.Truncated), N.Invalid_Path);
   Probe (Root_At, (Kind => AML_Names.Malformed), N.Invalid_Path);
   Probe (Counter_At, Path (Count => 0), N.Not_Found, Access_Result => V.Wrong_Kind);
   Probe (V.No_Node, Path ("DEV0", Rooted => True), N.Not_Found,
     Access_Result => V.Invalid_Value);
   Initialize (Other);
   V.Describe_Node (Other, N.Root, Foreign_At, Status);
   Check (Status = V.Available, "foreign root descriptor");
   Probe (Foreign_At, Path ("DEV0", Rooted => True), N.Not_Found,
     Access_Result => V.Invalid_Value);
   Stale_At := Device_At;
   Initialize (Arena);
   Probe (Stale_At, Path ("INNR"), N.Not_Found, Access_Result => V.Invalid_Value);
   V.Describe_Node (Arena, N.Root, Root_At, Status);
   Check (Status = V.Available, "new generation root");
   V.Resolve_Node (Arena, Root_At, Path ("ALS0"), Item, Outcome, Status);
   Check (Status = V.Available and then Outcome = N.Found, "resolve selected table root");
   declare
      Attempt : V.Append_Attempt;
      Report : V.Append_Report;
      Loaded_Method : V.Node_Descriptor;
      Result : V.Result;
   begin
      V.Append_Ready_Module (Arena, Input, 2, Width, 10000, Attempt, Report, Status,
        (V.Selected_Root, Item));
      Check (Status = V.Available and then Report.Outcome = V.Append_Completed,
        "append uses resolved canonical scope descriptor");
      V.Resolve_Node (Arena, Root_At, Path ("DEV0", "NVAL", 2), Loaded_Method, Outcome, Status);
      Check (Status = V.Available and then Outcome = N.Found, "loaded method path");
      V.Evaluate_Named (Arena, Input, Loaded_Method, Width, 10000, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Returned
        and then Result.Number = 42, "resolved method executes");
      Probe (Root_At, Path ("NVAL"), N.Not_Found);
   end;
   V.Reset (Arena, Status); Check (Status = V.Available, "final reset");
   Probe (Root_At, Path ("DEV0"), N.Not_Found, Access_Result => V.Wrong_Phase);
   Ada.Text_IO.Put_Line ("Module path checks" & Checks'Image);
end Module_Path_Tests;
