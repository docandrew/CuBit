with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service;
with Firmware_Tables;
with Firmware_Tables.Identifiers;
procedure Table_Find_Runner is
   package IO renames Ada.Streams.Stream_IO;
   use type IO.Count;
   use type Ada.Streams.Stream_Element_Offset;
   use type ACPI_Service.Install_Status;
   Service : ACPI_Service.State (ACPI_Service.Max_Tables, ACPI_Service.Max_Total_Bytes, ACPI_Service.Max_Table_Bytes);
   Status : ACPI_Service.Install_Status;
   File : IO.File_Type;
   Query : Firmware_Tables.Identifiers.Selection := (Name => "____", others => <>);
begin
   if Ada.Command_Line.Argument_Count /= 4 then
      raise Program_Error with "usage: table_find_runner table.aml SIGNATURE OEM-ID OEM-TABLE-ID";
   end if;
   declare
      Name : constant String := Ada.Command_Line.Argument (2);
      OEM : constant String := Ada.Command_Line.Argument (3);
      OEM_Table : constant String := Ada.Command_Line.Argument (4);
   begin
      if Name'Length /= 4 or else OEM'Length not in 0 | 6
        or else OEM_Table'Length not in 0 | 8
      then raise Program_Error with "fixed-width selectors required"; end if;
      Query.Name := Name;
      Query.Match_OEM := OEM'Length /= 0;
      if Query.Match_OEM then Query.OEM := OEM; end if;
      Query.Match_OEM_Table := OEM_Table'Length /= 0;
      if Query.Match_OEM_Table then Query.OEM_Table := OEM_Table; end if;
   end;
   IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
   if IO.Size (File) < 36 or else IO.Size (File) > ACPI_Service.Max_Table_Bytes then
      raise Program_Error with "table size";
   end if;
   declare
      Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
      Last : Ada.Streams.Stream_Element_Offset;
      Data : Firmware_Tables.Bytes (1 .. Raw'Length);
   begin
      IO.Read (File, Raw, Last); IO.Close (File);
      if Last /= Raw'Last then raise Program_Error with "short read"; end if;
      for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
      ACPI_Service.Install (Service, 1, ACPI_Service.DSDT, Data, Status);
      if Status /= ACPI_Service.Installed then raise Program_Error with Status'Image; end if;
   end;
   Ada.Text_IO.Put_Line ("MATCH" & ACPI_Service.Find_Table (Service, Query)'Image);
end Table_Find_Runner;
