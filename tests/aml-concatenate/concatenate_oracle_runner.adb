with ACPI_Test_Results;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service; use ACPI_Service;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Concatenate_Oracle_Runner is
   use type Ada.Streams.Stream_Element_Offset;
   use type AML_Execute.Execution_Status;
   package IO renames Ada.Streams.Stream_IO;
   File : IO.File_Type;
   Service : aliased State (Max_Tables, Max_Total_Bytes, Max_Table_Bytes);
   Held : ACPI_Test_Results.Holder;
   Status : Install_Status;
   Metrics_Mode : constant Boolean := Ada.Command_Line.Argument_Count = 2
     and then Ada.Command_Line.Argument (2) = "--metrics";
   Args : AML_Execute.Arguments := [others => 0];
   Object_Mode : constant Boolean := Ada.Command_Line.Argument_Count = 3
     and then Ada.Command_Line.Argument (3) = "--object";
   Result_Object_Mode : constant Boolean := Ada.Command_Line.Argument_Count >= 3
     and then Ada.Command_Line.Argument (3) = "--result-object";

begin
   if Ada.Command_Line.Argument_Count < 2
     or else Ada.Command_Line.Argument_Count > 9
   then
      raise Program_Error with "usage: table_runner table.aml NAME [integer args]";
   end if;
   IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
   declare
      Raw : Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
      Last : Ada.Streams.Stream_Element_Offset;
      Data : Firmware_Tables.Bytes (1 .. Raw'Length);
   begin
      IO.Read (File, Raw, Last);
      IO.Close (File);
      if Last /= Raw'Last then raise Program_Error with "short read"; end if;
      for I in Data'Range loop
         Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I)));
      end loop;
      Install (Service, 1, DSDT, Data, Status);
   end;
   if Status /= Installed then
      if Observe (Service).Last_Load_Code /= 0 then
         Ada.Text_IO.Put_Line ("load: " & Namespace.Load_Status'Image
           (Namespace.Load_Status'Val (Observe (Service).Last_Load_Code - 1)));
      end if;
      raise Program_Error with "install: " & Status'Image;
   end if;
   -- This runner accepts exactly one initial definition block. Bind deferred
   -- package members only after its complete installation, before reads/calls.
   ACPI_Test_Results.Seal (Service);
   if Metrics_Mode then
      declare
         M : constant Metrics := Observe (Service);
      begin
         Ada.Text_IO.Put_Line ("NODES" & M.Objects'Image);
         Ada.Text_IO.Put_Line ("VALUE_OBJECTS" & M.Value_Objects'Image);
         Ada.Text_IO.Put_Line ("VALUE_BYTES" & M.Value_Bytes'Image);
         Ada.Text_IO.Put_Line ("PACKAGE_ELEMENTS" & M.Package_Elements'Image);
         Ada.Text_IO.Put_Line ("METHOD_BYTES" & M.Method_Bytes'Image);
      end;
      return;
   end if;
   if not Object_Mode then
      for I in (if Result_Object_Mode then 4 else 3) .. Ada.Command_Line.Argument_Count loop
         Args (I - (if Result_Object_Mode then 4 else 3)) := AML_Decode.Integer_Value'Value (Ada.Command_Line.Argument (I));
      end loop;
   end if;
   declare
      Node : Namespace.Node_ID := Namespace.Root;
      Path : constant String := Ada.Command_Line.Argument (2);
      Start : Positive := Path'First;
      Result : Values.Result;
   begin
      for I in Path'First .. Path'Last + 1 loop
         if I > Path'Last or else Path (I) = '.' then
            if I - Start /= 4 then raise Program_Error with "expected four-character path segments"; end if;
            Node := ACPI_Test_Results.Child (Service, Node, Path (Start .. I - 1));
            if Node = Namespace.Root then raise Program_Error with "missing path segment"; end if;
            Start := I + 1;
         end if;
      end loop;
      if Node = Namespace.Root then raise Program_Error with "missing method"; end if;
      if Object_Mode then
         ACPI_Test_Results.Emit_Named (Service, Node);
         return;
      end if;
      ACPI_Test_Results.Invoke (Service, Held, Node, Args, (if Result_Object_Mode then Ada.Command_Line.Argument_Count - 3 else Ada.Command_Line.Argument_Count - 2), 100_000, Result);
      declare
         Mark : constant Namespace.Node_ID := ACPI_Test_Results.Child (Service, Namespace.Root, "MARK");
      begin
         if Mark /= Namespace.Root then
            Ada.Text_IO.Put_Line ("MARK" & ACPI_Test_Results.Integer_Data (Service, Mark)'Image);
         end if;
      end;
      if Result_Object_Mode then
         if Result.Status = AML_Execute.Object_Returned then
            ACPI_Test_Results.Emit (Service, Result.Handle);
         elsif Result.Status = AML_Execute.Returned then
            Ada.Text_IO.Put_Line ("INTEGER" & Result.Number'Image);
         else
            raise Program_Error with "execute: " & Result.Status'Image;
         end if;
         ACPI_Test_Results.Drop (Service, Held);
         return;
      end if;
      Ada.Text_IO.Put_Line ("STATUS " & Result.Status'Image);
      case Result.Status is
         when AML_Execute.Returned => Ada.Text_IO.Put_Line ("INTEGER" & Result.Number'Image);
         when AML_Execute.Object_Returned =>
            ACPI_Test_Results.Emit (Service, Result.Handle);
         when AML_Execute.Reference_Returned =>
            ACPI_Test_Results.Emit (Service, Result.Handle);
         when others => null;
      end case;
      ACPI_Test_Results.Drop (Service, Held);
   exception
      when others =>
         ACPI_Test_Results.Drop (Service, Held);
         raise;
   end;
end Concatenate_Oracle_Runner;
