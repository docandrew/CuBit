with ACPI_Test_Results;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service; use ACPI_Service;
with AML_Execute;
with Firmware_Tables;
procedure Mid_Runner is
   use type Ada.Streams.Stream_Element_Offset;
   use type Ada.Streams.Stream_IO.Count;
   use type AML_Execute.Execution_Status;
   package IO renames Ada.Streams.Stream_IO;
   File : IO.File_Type;
   Service : aliased State (Max_Tables, Max_Total_Bytes, Max_Table_Bytes);
   Held : ACPI_Test_Results.Holder;
   Status : Install_Status;
   Result : Values.Result;
   Node : Namespace.Node_ID;
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      raise Program_Error with "usage: mid_runner table.aml TEST";
   end if;
   IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
   if IO.Size (File) > IO.Count (Max_Table_Bytes) then
      IO.Close (File); raise Program_Error with "oversized table";
   end if;
   declare
      Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
      Last : Ada.Streams.Stream_Element_Offset;
      Data : Firmware_Tables.Bytes (1 .. Raw'Length);
   begin
      IO.Read (File, Raw, Last); IO.Close (File);
      if Last /= Raw'Last then raise Program_Error with "short read"; end if;
      for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
      Install (Service, 1, DSDT, Data, Status);
   end;
   if Status /= Installed then raise Program_Error with "install " & Status'Image; end if;
   ACPI_Test_Results.Seal (Service);
   Node := ACPI_Test_Results.Child (Service, Namespace.Root, Ada.Command_Line.Argument (2));
   if Node = Namespace.Root then raise Program_Error with "missing TEST"; end if;
   ACPI_Test_Results.Invoke (Service, Held, Node, [others => 0], 0, 100_000, Result);
   Ada.Text_IO.Put_Line ("STATUS " & Result.Status'Image);
   case Result.Status is
      when AML_Execute.Returned => Ada.Text_IO.Put_Line ("INTEGER" & Result.Number'Image);
      when AML_Execute.Object_Returned | AML_Execute.Reference_Returned => ACPI_Test_Results.Emit (Service, Result.Handle);
      when others => null;
   end case;
   ACPI_Test_Results.Drop (Service, Held);
   Node := ACPI_Test_Results.Child (Service, Namespace.Root, "SEEN");
   if Node = Namespace.Root then raise Program_Error with "missing SEEN"; end if;
   ACPI_Test_Results.Invoke (Service, Held, Node, [others => 0], 0, 100_000, Result);
   if Result.Status /= AML_Execute.Returned then raise Program_Error with "SEEN failed"; end if;
   Ada.Text_IO.Put_Line ("SEEN" & Result.Number'Image);
   ACPI_Test_Results.Drop (Service, Held);
exception
   when others =>
      ACPI_Test_Results.Drop (Service, Held);
      raise;
end Mid_Runner;
