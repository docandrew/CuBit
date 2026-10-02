with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service; use ACPI_Service;
with AML_Decode;
with AML_Execute;
with AML_Objects;
with Firmware_Tables;
procedure Table_Runner is
   use type Ada.Streams.Stream_Element_Offset;
   use type AML_Execute.Execution_Status;
   package IO renames Ada.Streams.Stream_IO;
   File : IO.File_Type;
   Service : aliased State := Fresh;
   Status : Install_Status;
   Metrics_Mode : constant Boolean := Ada.Command_Line.Argument_Count = 2
     and then Ada.Command_Line.Argument (2) = "--metrics";
   Args : AML_Execute.Arguments := [others => 0];
   Object_Mode : constant Boolean := Ada.Command_Line.Argument_Count = 3
     and then Ada.Command_Line.Argument (3) = "--object";
   Result_Object_Mode : constant Boolean := Ada.Command_Line.Argument_Count = 3
     and then Ada.Command_Line.Argument (3) = "--result-object";
   procedure Emit (Store : AML_Objects.State; ID : AML_Objects.Object_ID; Depth : Natural) is
      use AML_Objects;
   begin
      if Depth > 64 then raise Program_Error with "object depth"; end if;
      if ID = 0 then Ada.Text_IO.Put_Line ("NULL"); return; end if;
      case Kind (Store, ID) is
         when Integer_Object =>
            Ada.Text_IO.Put_Line ("INTEGER" & Integer_Data (Store, ID)'Image);
         when String_Object =>
            Ada.Text_IO.Put ("STRING ");
            for B of Byte_Data (Store, ID) loop Ada.Text_IO.Put (Character'Val (B)); end loop;
            Ada.Text_IO.New_Line;
         when Package_Object =>
            Ada.Text_IO.Put_Line ("PACKAGE" & Length (Store, ID)'Image);
            for I in 1 .. Length (Store, ID) loop Emit (Store, Element (Store, ID, I - 1), Depth + 1); end loop;
         when Buffer_Object =>
            Ada.Text_IO.Put ("BUFFER");
            for B of Byte_Data (Store, ID) loop Ada.Text_IO.Put (B'Image); end loop;
            Ada.Text_IO.New_Line;
      end case;
   end Emit;
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
   if not Object_Mode and not Result_Object_Mode then
      for I in 3 .. Ada.Command_Line.Argument_Count loop
         Args (I - 3) := AML_Decode.Integer_Value'Value (Ada.Command_Line.Argument (I));
      end loop;
   end if;
   declare
      Tree : Namespace.State := Snapshot (Service);
      Node : Namespace.Node_ID := Namespace.Root;
      Path : constant String := Ada.Command_Line.Argument (2);
      Start : Positive := Path'First;
      Result : AML_Execute.Execution_Result;
   begin
      for I in Path'First .. Path'Last + 1 loop
         if I > Path'Last or else Path (I) = '.' then
            if I - Start /= 4 then raise Program_Error with "expected four-character path segments"; end if;
            Node := Namespace.Child (Tree, Node, Path (Start .. I - 1));
            if Node = Namespace.Root then raise Program_Error with "missing path segment"; end if;
            Start := I + 1;
         end if;
      end loop;
      if Node = Namespace.Root then raise Program_Error with "missing method"; end if;
      if Object_Mode then
         Emit (Namespace.Value_Store (Tree), Namespace.Data_Object (Tree, Node), 0);
         return;
      end if;
      Invoke
        (Service, Node, Args, (if Result_Object_Mode then 0 else Ada.Command_Line.Argument_Count - 2), 100_000, Result);
      Tree := Snapshot (Service);
      if Result_Object_Mode then
         if Result.Status = AML_Execute.Object_Returned then
            Emit (Namespace.Value_Store (Tree), Result.Object.ID, 0);
         elsif Result.Status = AML_Execute.Returned then
            Ada.Text_IO.Put_Line ("INTEGER" & Result.Value'Image);
         else
            raise Program_Error with "execute: " & Result.Status'Image;
         end if;
         return;
      end if;
      if Result.Status /= AML_Execute.Returned then
         raise Program_Error with "execute: " & Result.Status'Image;
      end if;
      Ada.Text_IO.Put_Line ("RESULT" & Result.Value'Image);
   end;
end Table_Runner;
