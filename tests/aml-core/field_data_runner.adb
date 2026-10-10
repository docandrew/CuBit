with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service;
with Firmware_Tables;
with AML_Decode;
with AML_Field_Data;
procedure Field_Data_Runner is
   package IO renames Ada.Streams.Stream_IO;
   use type IO.Count;
   use type Ada.Streams.Stream_Element_Offset;
   use type ACPI_Service.Install_Status;
   use type ACPI_Service.Namespace.Bind_Status;
   use type AML_Decode.Status;
   use type AML_Decode.Integer_Value;
   Service : ACPI_Service.State (ACPI_Service.Max_Tables, ACPI_Service.Max_Total_Bytes, ACPI_Service.Max_Table_Bytes);
   Status : ACPI_Service.Install_Status;
   File : IO.File_Type;
   Result : AML_Field_Data.Read_Result;
   Region, Field : ACPI_Service.Namespace.Node_ID;
   Bound : ACPI_Service.Namespace.Bind_Status;
   Value : AML_Decode.Integer_Value := 0;
begin
   if Ada.Command_Line.Argument_Count /= 3 then
      raise Program_Error with "usage: field_data_runner table.aml bit-offset bit-count";
   end if;
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
   ACPI_Service.Declare_Table_Region (Service, 0, "RGN0", (Name => "DSDT", others => <>), Region, Bound);
   if Bound /= ACPI_Service.Namespace.Bound then raise Program_Error with Bound'Image; end if;
   ACPI_Service.Declare_Table_Field (Service, 0, "FLD0", Region,
     Natural'Value (Ada.Command_Line.Argument (2)), Natural'Value (Ada.Command_Line.Argument (3)), Field, Bound);
   if Bound /= ACPI_Service.Namespace.Bound then raise Program_Error with Bound'Image; end if;
   Result := ACPI_Service.Read_Namespace_Field (Service, Field);
   if Result.Status /= AML_Decode.Accepted or else Result.Length > 8 then
      raise Program_Error with "invalid integer field";
   end if;
   for I in reverse 1 .. Result.Length loop
      Value := Value * 256 + AML_Decode.Integer_Value (Result.Content (I));
   end loop;
   Ada.Text_IO.Put_Line ("VALUE" & Value'Image);
end Field_Data_Runner;
