with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with AML_Decode;
with AML_Fields;
procedure Field_Runner is
   package IO renames Ada.Streams.Stream_IO;
   use type IO.Count;
   use type Ada.Streams.Stream_Element_Offset;
   use type AML_Decode.Status;
   File : IO.File_Type;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      raise Program_Error with "usage: field_runner field-list.bin";
   end if;
   IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
   if IO.Size (File) > 65536 then raise Program_Error with "input size"; end if;
   declare
      Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
      Last : Ada.Streams.Stream_Element_Offset;
      Data : AML_Decode.Bytes (1 .. Raw'Length);
      Position : Positive := 1;
   begin
      IO.Read (File, Raw, Last); IO.Close (File);
      if Last /= Raw'Last then raise Program_Error with "short read"; end if;
      for I in Data'Range loop
         Data (I) := AML_Decode.Byte (Raw (Ada.Streams.Stream_Element_Offset (I)));
      end loop;
      while Position <= Data'Last loop
         declare
            R : constant AML_Fields.Entry_Result := AML_Fields.Read_Entry (Data (Position .. Data'Last));
         begin
            if R.Status /= AML_Decode.Accepted then raise Program_Error with R.Status'Image; end if;
            Ada.Text_IO.Put_Line (R.Kind'Image & " " & R.Name & R.Bits'Image
              & R.Access_Type'Image & R.Attribute'Image & R.Access_Length'Image);
            Position := Position + R.Consumed;
         end;
      end loop;
   end;
end Field_Runner;
