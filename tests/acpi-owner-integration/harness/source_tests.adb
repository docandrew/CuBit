with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service_Core;
with ACPI_Controller_Source;
with ACPI_Resource_Names;
with AML_Delays;
with AML_Names;
with Firmware_Tables;
procedure Source_Tests is
   package C is new ACPI_Service_Core (Perform_Delay => AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Aggregate_Method_Capacity => 2048);
   package R is new ACPI_Controller_Source (C);
   use type C.Values.Access_Status;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type R.Outcome;
   use type AML_Names.Parse_Status;
   Service : aliased C.State (1, 4096, 4096);
   Checks : Natural := 0;
   procedure Check (Value : Boolean; Text : String) is
   begin Checks := Checks + 1; if not Value then raise Program_Error with Text; end if; end Check;
   function Bytes (Text : String) return Firmware_Tables.Bytes is
      Data : Firmware_Tables.Bytes (1 .. Text'Length + 1) := [others => 0];
   begin
      for I in Text'Range loop Data (I - Text'First + 1) := Character'Pos (Text (I)); end loop;
      return Data;
   end Bytes;
   function Find (Parent : C.Namespace.Node_ID; Name : String) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (Service, Parent, Name, Node, Status); Check (Status = C.Values.Available, "find child");
      C.Describe_Node (Service, Node, Item, Status); Check (Status = C.Values.Available, "describe child");
      return Item;
   end Find;
   Parent, Clean, Shadow, Local_Controller, Root_Controller : C.Node_Descriptor;
   procedure Resolve (Context : C.Node_Descriptor; Text : String; Expected : R.Outcome;
      Node : C.Namespace.Node_ID := C.Namespace.Root) is
      Result : R.Report;
   begin
      R.Resolve (Service, Context, Bytes (Text), Result);
      Check (Result.Status = Expected, "resolve " & Text & ": " & Result.Status'Image);
      Check (C.Values.Position (Result.Target) = Node, "target or empty failure");
   end Resolve;
begin
   for Value in 0 .. 255 loop
      declare
         Data : constant Firmware_Tables.Bytes := [65, Firmware_Tables.Byte (Value), 0];
         Parsed : constant AML_Names.Name_Result := ACPI_Resource_Names.Parse (Data);
         Valid : constant Boolean := Value = 95 or else Value in 65 .. 90 or else Value in 97 .. 122 or else Value in 48 .. 57;
      begin Check ((Parsed.Kind = AML_Names.Accepted) = Valid, "all tail byte classes"); end;
   end loop;
   declare
      Parsed : constant AML_Names.Name_Result := ACPI_Resource_Names.Parse (Bytes ("\_sb.i2c"));
      High : constant Firmware_Tables.Bytes (Natural'Last - 5 .. Natural'Last) := [92,73,50,67,48,0];
      Other : constant AML_Names.Name_Result := ACPI_Resource_Names.Parse (High);
   begin
      Check (Parsed.Kind = AML_Names.Accepted and then Parsed.Rooted and then Parsed.Count = 2
        and then Parsed.Parts (1) = "_SB_" and then Parsed.Parts (2) = "I2C_", "case folding/padding");
      Check (Other.Kind = AML_Names.Accepted and then Other.Parts (1) = "I2C0", "high-bound bytes");
   end;
   for Count in 255 .. 256 loop
      declare
         Parents : constant String (1 .. Count) := [others => '^'];
         Parsed : constant AML_Names.Name_Result := ACPI_Resource_Names.Parse (Bytes (Parents & "I2C0"));
         Parts : String (1 .. Count * 2 - 1) := [others => '.'];
      begin
         Check (Parsed.Kind = (if Count = 255 then AML_Names.Accepted else AML_Names.Limit_Exceeded), "parent prefix quota");
         for I in 1 .. Count loop Parts (I * 2 - 1) := 'A'; end loop;
         Check (ACPI_Resource_Names.Parse (Bytes (Parts)).Kind =
           (if Count = 255 then AML_Names.Accepted else AML_Names.Limit_Exceeded), "segment quota");
      end;
   end loop;
   Check (ACPI_Resource_Names.Parse (Firmware_Tables.Bytes'[65]).Kind = AML_Names.Truncated, "missing terminator");
   Check (ACPI_Resource_Names.Parse (Firmware_Tables.Bytes'[65,0,66,0]).Kind = AML_Names.Malformed, "embedded NUL");
   Check (ACPI_Resource_Names.Parse (Bytes ("ABCDE")).Kind = AML_Names.Malformed, "long segment");
   Check (ACPI_Resource_Names.Parse (Bytes ("A..B")).Kind = AML_Names.Malformed, "empty interior segment");
   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
      Stage : C.Stage_Status;
      Freeze : C.Freeze_Status;
      Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (File, Buffer, Last); Ada.Streams.Stream_IO.Close (File);
      declare Data : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Buffer (Ada.Streams.Stream_Element_Offset (I))); end loop;
         C.Stage_Table (Service, 1, C.DSDT, Data, Stage); Check (Stage = C.Staged, "stage");
      end;
      C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (Service, 1000, Execution, Status); Check (Status = C.Values.Available, "execute");
      C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "members");
   end;
   Root_Controller := Find (0, "I2C0"); Parent := Find (0, "SCP0");
   Clean := Find (C.Values.Position (Parent), "CLN0"); Shadow := Find (C.Values.Position (Parent), "DEV0");
   Local_Controller := Find (C.Values.Position (Parent), "I2C0");
   Resolve (Clean, "I2C0", R.Resolved, C.Values.Position (Local_Controller));
   Resolve (Clean, "^i2c0", R.Resolved, C.Values.Position (Local_Controller));
   Resolve (Clean, "\I2C0", R.Resolved, C.Values.Position (Root_Controller));
   Resolve (Clean, "ALS0", R.Resolved, C.Values.Position (Local_Controller));
   Resolve (Shadow, "I2C0", R.Not_Device);
   Resolve (Clean, "I2C0.MARK", R.Not_Found);
   Resolve (Clean, "\SCP0.ALS0", R.Resolved, C.Values.Position (Local_Controller));
   Resolve (Clean, "^^^I2C0", R.Above_Root);
   Resolve (Clean, "ABCD.", R.Bad_Source);
   Resolve (C.Values.No_Node, "\I2C0", R.Invalid_Context);
   Resolve (Clean, "\", R.Not_Device);
   Check (C.Valid (Service) and then C.Retained_Results (Service) = 0, "lookup retains no AML result");
   Put_Line ("Controller source checks" & Checks'Image);
end Source_Tests;
