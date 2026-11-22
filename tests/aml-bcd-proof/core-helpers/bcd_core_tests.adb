pragma Ada_2022;
with ACPI_Service; with ACPI_Hosted_Heap;
with AML_Objects; with AML_Execute; with Firmware_Tables;
with Ada.Command_Line; with Ada.Streams; with Ada.Streams.Stream_IO; with Ada.Text_IO;
pragma Elaborate_All (ACPI_Hosted_Heap);
procedure BCD_Core_Tests is
   package C renames ACPI_Service;
   package IO renames Ada.Streams.Stream_IO;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Namespace.Lookup_Status;
   use type AML_Execute.Execution_Status;
   File : IO.File_Type;
   function Read_Table return Firmware_Tables.Bytes is
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      declare Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset; Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File);
         if Natural (Last) /= Raw'Length then raise Program_Error with "short table"; end if;
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   Service : aliased C.State (1, Table'Length, Table'Length, 1,
      Value_Object_Capacity => AML_Objects.Max_Objects, Value_Byte_Capacity => AML_Objects.Max_Bytes,
      Value_Element_Capacity => AML_Objects.Max_Elements);
   Staged : C.Stage_Status; Frozen : C.Freeze_Status;
   Execution : C.Execution_Report; Members : C.Namespace.Initialization_Report;
   Status : C.Values.Access_Status;
begin
   C.Stage_Table (Service, 1, C.DSDT, Table, Staged);
   if Staged /= C.Staged then raise Program_Error with "stage"; end if;
   C.Freeze_Catalog (Service, Frozen);
   if Frozen /= C.Catalog_Frozen_OK then raise Program_Error with "freeze"; end if;
   C.Execute_Frozen (Service, 10_000, Execution, Status);
   if Status /= C.Values.Available then raise Program_Error with "modules"; end if;
   C.Initialize_Members (Service, Members, Status);
   if Status /= C.Values.Available then raise Program_Error with "members"; end if;
   for I in 0 .. 9 loop
      declare Name : constant String := "B00" & Character'Val (Character'Pos ('0') + I);
         Found : C.Namespace.Lookup_Result; Item : C.Node_Descriptor;
         Value : C.Values.Result;
      begin
         C.Find_Object (Service, C.Namespace.Root, Name, Found, Status);
         if Status /= C.Values.Available or else Found.Status /= C.Namespace.Found then raise Program_Error with "lookup"; end if;
         C.Describe_Node (Service, Found.Node, Item, Status);
         if Status /= C.Values.Available then raise Program_Error with "describe"; end if;
         C.Evaluate_Named (Service, Item, 1000, Value, Status);
         if Status /= C.Values.Available then raise Program_Error with "evaluate access"; end if;
         if I < 8 then
            if Value.Status /= AML_Execute.Returned then raise Program_Error with "normal conversion status"; end if;
            Ada.Text_IO.Put_Line (Name & " " & Value.Status'Image & Value.Number'Image);
         else
            if Value.Status /= AML_Execute.Numeric_Overflow then raise Program_Error with "overflow status"; end if;
            Ada.Text_IO.Put_Line (Name & " " & Value.Status'Image);
         end if;
         if C.Retained_Results (Service) /= 0 then raise Program_Error with "retained conversion debt"; end if;
      end;
   end loop;
end BCD_Core_Tests;
