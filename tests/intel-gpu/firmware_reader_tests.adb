with Interfaces; use Interfaces;
with Intel_GPU_Firmware;
with Intel_GPU_Firmware_Reader; use Intel_GPU_Firmware_Reader;
procedure Firmware_Reader_Tests is
   Source : Byte_Array (0 .. 8191) := [others => 0];
   Buffer : Byte_Array (17 .. 8208);
   Calls : Natural := 0;
   Expected_Offset : Unsigned_64 := 0;
   Maximum_Read : Unsigned_64 := Chunk_Bytes;
   Mode : Natural := 0;
   Status : Read_Status;
   Plan : Intel_GPU_Firmware.Layout;
   procedure Read_At
     (Offset : Unsigned_64; Destination : out Byte_Array;
      Count : out Unsigned_64; Success : out Boolean)
   is
   begin
      pragma Assert (Offset = Expected_Offset);
      pragma Assert (Destination'Length in 1 .. Chunk_Bytes);
      Calls := Calls + 1;
      Count := Unsigned_64'Min (Maximum_Read, Unsigned_64 (Destination'Length));
      Success := Mode /= 1 and then not (Mode = 4 and Calls = 2);
      if Mode = 2 then Count := 0;
      elsif Mode = 3 then Count := Unsigned_64 (Destination'Length) + 1;
      else
         for Index in 0 .. Natural (Count) - 1 loop
            Destination (Destination'First + Index) := Source (Natural (Offset) + Index);
         end loop;
      end if;
      Expected_Offset := Offset + Count;
   end Read_At;
   procedure Load is new Intel_GPU_Firmware_Reader.Load (Read_At);
   procedure Run (Size : Unsigned_64; Expected : Read_Status) is
   begin
      Calls := 0; Expected_Offset := 0;
      Load (Size, Buffer, Status, Plan);
      pragma Assert (Status = Expected);
      pragma Assert (Plan.Valid = (Status = Loaded));
   end Run;
   procedure Put (Offset : Natural; Value : Unsigned_32) is
   begin
      for Index in 0 .. 3 loop
         Source (Offset + Index) := Unsigned_8 (Shift_Right (Value, Index * 8) and 255);
      end loop;
   end Put;
begin
   Run (0, Invalid_Size); pragma Assert (Calls = 0);
   Run (127, Invalid_Size); pragma Assert (Calls = 0);
   Run (8193, Invalid_Size); pragma Assert (Calls = 0);
   Run (Unsigned_64'Last, Invalid_Size); pragma Assert (Calls = 0);
   Run (8192, Invalid_Layout);
   --  CSS + 7808 bytes of code + 256 bytes of signature.
   Put (4, 96); Put (24, 2048); Put (28, 64);
   Run (8192, Loaded); pragma Assert (Calls = 2);
   pragma Assert (Plan.Code_Bytes = 7808);
   Maximum_Read := 137;
   Run (8192, Loaded); pragma Assert (Calls = 60);
   pragma Assert (Buffer = Source);
   Mode := 1; Run (8192, Read_Failed); pragma Assert (Calls = 1);
   Mode := 2; Run (8192, Truncated); pragma Assert (Calls = 1);
   Mode := 3; Run (8192, Invalid_Reply); pragma Assert (Calls = 1);
   Mode := 4; Run (8192, Read_Failed); pragma Assert (Calls = 2);
   Mode := 0; Maximum_Read := Chunk_Bytes;
   declare
      High_Buffer : Byte_Array (Natural'Last - 8191 .. Natural'Last);
   begin
      Calls := 0; Expected_Offset := 0;
      Load (8192, High_Buffer, Status, Plan);
      pragma Assert (Status = Loaded and High_Buffer = Source);
   end;
end Firmware_Reader_Tests;
