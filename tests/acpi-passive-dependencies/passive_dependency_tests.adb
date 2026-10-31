pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service_Core;
with ACPI_Passive_Dependencies;
with AML_Delays;
with AML_Names;
with AML_Execute;
with AML_Decode;
with Firmware_Tables;
procedure Passive_Dependency_Tests is
   package C is new ACPI_Service_Core
     (AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 2);
   package D is new ACPI_Passive_Dependencies (C);
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status;
   use type C.Values.Module_Outcome, C.Node_Descriptor, C.Values.Value_Handle;
   use type AML_Execute.Execution_Status, D.Outcome, D.Element_Count;
   use type AML_Decode.Integer_Value;
   Service, Other : aliased C.State (1, 4096, 4096, 1);
   Items : D.Device_Array (Natural'Last - 7 .. Natural'Last);
   Result : D.Report;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with Message; end if; end Check;
   function Find (State : C.State; Name : AML_Names.Segment) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Value : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (State, C.Namespace.Root, Name, Node, Status); Check (Status = C.Values.Available, "find fixture node");
      C.Describe_Node (State, Node, Value, Status); Check (Status = C.Values.Available, "describe fixture node");
      return Value;
   end Find;
   procedure Prepare (State : aliased in out C.State; Bytes : Firmware_Tables.Bytes) is
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      C.Stage_Table (State, 1, C.DSDT, Bytes, Staged); Check (Staged = C.Staged, "stage fixture");
      C.Freeze_Catalog (State, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze fixture");
      C.Execute_Frozen (State, 1000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "module completion");
      C.Initialize_Members (State, Members, Status); Check (Status = C.Values.Available, "initialize static members");
   end Prepare;
   procedure Query (Name : AML_Names.Segment; Expected : D.Outcome; Maximum : D.Element_Count := 8) is
   begin
      D.Read (Service, Find (Service, Name), Maximum, Items, Result);
      Check (Result.Status = Expected, "dependency outcome " & Name & " expected " & Expected'Image & " got " & Result.Status'Image);
      Check (C.Valid (Service) and then C.Retained_Results (Service) = 0, "query returns all retained roots");
      if Expected /= D.Static_List then
         Check (Result.Copied = 0 and then (for all Item of Items => Item = C.Values.No_Node), "failure has no partial edges");
      end if;
   end Query;
   procedure Counter_Zero is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Evaluate_Named (Service, Find (Service, "HITS"), 100, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned and then Value.Number = 0,
        "passive dependency observation never executes method-valued _DEP");
   end Counter_Zero;
begin
   Check (Ada.Command_Line.Argument_Count = 1, "fixture path");
   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (File, Stream, Last); Ada.Streams.Stream_IO.Close (File);
      declare Image : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Image); Prepare (Other, Image);
      end;
   end;
   Query ("TPD0", D.Static_List);
   Check (Result.Size_Known and then Result.Required = 3 and then Result.Copied = 3, "complete static dependency count");
   Check (Items (Items'First) = Find (Service, "GPO0") and then Items (Items'First + 1) = Find (Service, "I2C0")
     and then Items (Items'First + 2) = Items (Items'First), "dependency order and duplicate references preserved");
   Check ((for all I in Items'First + 3 .. Items'Last => Items (I) = C.Values.No_Node), "success clears unused suffix");
   Query ("ALS0", D.Static_List);
   Check (Result.Copied = 1 and then Items (Items'First) = Find (Service, "GPO0"), "device alias resolves to live canonical device");
   Query ("ALDP", D.Static_List);
   Check (Result.Copied = 1 and then Items (Items'First) = Find (Service, "GPO0"), "direct _DEP alias resolves to static package");
   Query ("EMP0", D.Static_List, 0);
   Check (Result.Size_Known and then Result.Required = 0 and then Result.Copied = 0, "empty list is an observed list");
   Query ("NONE", D.Missing);
   Query ("METH", D.Method_Valued); Counter_Zero;
   Query ("WRNG", D.Unsupported);
   Query ("BAD0", D.Unsupported);
   Check (Result.Required = 2 and then Result.Size_Known, "late malformed dependency clears already read prefix");
   Query ("TPD0", D.Work_Limit, 2);
   Check (Result.Size_Known and then Result.Required = 3, "work bound reports size without traversing/truncating");
   declare Small : D.Device_Array (0 .. 1); begin
      D.Read (Service, Find (Service, "TPD0"), 8, Small, Result);
      Check (Result.Status = D.Capacity_Required and then Result.Required = 3 and then Result.Copied = 0
        and then (for all Item of Small => Item = C.Values.No_Node), "small output reports exact required capacity");
      Check (C.Retained_Results (Service) = 0, "capacity refusal releases root");
   end;
   Query ("TPD0", D.Static_List);
   Query ("SELF", D.Static_List);
   Check (Result.Copied = 1 and then Items (Items'First) = Find (Service, "SELF"), "self edge is descriptive and never recursively followed");
   Query ("CYCA", D.Static_List);
   Check (Result.Copied = 1 and then Items (Items'First) = Find (Service, "CYCB"), "forward device edge is live");
   Query ("CYCB", D.Static_List);
   Check (Result.Copied = 1 and then Items (Items'First) = Find (Service, "CYCA"), "cycle edge preserved without recursion");
   declare
      Node : C.Namespace.Node_ID;
      Child : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (Service, C.Values.Position (Find (Service, "PARN")), "CHLD", Node, Status);
      Check (Status = C.Values.Available, "nested child lookup"); C.Describe_Node (Service, Node, Child, Status);
      Check (Status = C.Values.Available, "nested child descriptor"); D.Read (Service, Child, 8, Items, Result);
      Check (Result.Status = D.Missing, "missing dependency does not inherit parent _DEP");
   end;
   D.Read (Service, Find (Other, "TPD0"), 8, Items, Result);
   Check (Result.Status = D.Invalid_Device and then C.Retained_Results (Service) = 0, "foreign namespace descriptor refused");
   D.Read (Service, Find (Service, "HITS"), 8, Items, Result);
   Check (Result.Status = D.Invalid_Device, "non-device descriptor refused");
   declare
      Held : C.Values.Result;
      Handle : C.Values.Value_Handle;
      Status : C.Values.Access_Status;
      Node : C.Namespace.Node_ID;
      Member : C.Node_Descriptor;
   begin
      C.Find_Child (Service, C.Values.Position (Find (Service, "GPO0")), "_HID", Node, Status);
      Check (Status = C.Values.Available, "root pressure HID lookup"); C.Describe_Node (Service, Node, Member, Status);
      Check (Status = C.Values.Available, "root pressure HID descriptor");
      C.Evaluate_Named (Service, Member, 100, Held, Status);
      Check (Status = C.Values.Available and then Held.Status = AML_Execute.Object_Returned, "retain unrelated caller result");
      Handle := Held.Handle;
      D.Read (Service, Find (Service, "TPD0"), 8, Items, Result);
      Check (Result.Status = D.Read_Failed and then C.Retained_Results (Service) = 1
        and then Result.Copied = 0 and then (for all Item of Items => Item = C.Values.No_Node),
        "root pressure is typed failure, preserves caller root and returns no edges");
      C.Release_Result (Service, Handle, Status);
      Check (Status = C.Values.Available and then Handle = C.Values.No_Value, "caller root remains releasable");
   end;
   Counter_Zero;
   Check (C.Retained_Results (Service) = 0 and then C.Valid (Service), "final owned state");
   Ada.Text_IO.Put_Line ("Dependency checks" & Checks'Image);
end Passive_Dependency_Tests;
