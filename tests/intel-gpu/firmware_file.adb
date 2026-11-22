with Ada.Command_Line;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Firmware; use Intel_GPU_Firmware;
with Intel_GPU_Firmware_Reader; use Intel_GPU_Firmware_Reader;
procedure Firmware_File is
   package IO renames Ada.Streams.Stream_IO;
   File : IO.File_Type;
   Buffer : Byte_Array (0 .. Maximum_Blob_Bytes - 1);
   Status : Read_Status;
   Plan : Layout;
   Size : Unsigned_64;
   procedure Read_At
     (Offset : Unsigned_64; Destination : out Byte_Array;
      Count : out Unsigned_64; Success : out Boolean)
   is
      Bytes : Stream_Element_Array (1 .. Stream_Element_Offset (Destination'Length));
      Last : Stream_Element_Offset;
   begin
      IO.Set_Index (File, IO.Positive_Count (Offset + 1));
      IO.Read (File, Bytes, Last);
      Count := Unsigned_64 (Last);
      for Index in 1 .. Last loop
         Destination (Destination'First + Natural (Index - 1)) := Unsigned_8 (Bytes (Index));
      end loop;
      Success := True;
   end Read_At;
   procedure Load is new Intel_GPU_Firmware_Reader.Load (Read_At);
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      raise Program_Error with "usage: firmware_file BLOB";
   end if;
   IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
   Size := Unsigned_64 (IO.Size (File));
   Load (Size, Buffer, Status, Plan);
   IO.Close (File);
   if Status /= Loaded then
      raise Program_Error with "firmware read rejected: " & Read_Status'Image (Status);
   end if;
   declare
      Header : CSS_Header;
   begin
      for Index in Header'Range loop
         Header (Index) := Buffer (Index);
      end loop;
      if not Matches_Selected_ADLN_GuC (Header, Size) then
         raise Program_Error with "not selected ADLN GuC metadata";
      end if;
   end;
   Ada.Text_IO.Put_Line ("CSS layout valid (NOT authenticated): blob" &
     Unsigned_64'Image (Size) & " code" & Unsigned_64'Image (Plan.Code_Bytes) &
     " signature-offset" & Unsigned_64'Image (Plan.Signature_Offset) &
     " signature-bytes" & Unsigned_64'Image (Plan.Signature_Bytes));
end Firmware_File;
