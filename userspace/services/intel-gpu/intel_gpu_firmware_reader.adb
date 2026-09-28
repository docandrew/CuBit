package body Intel_GPU_Firmware_Reader is
   procedure Load
     (File_Bytes : Unsigned_64; Buffer : out Byte_Array;
      Status : out Read_Status; Plan : out Intel_GPU_Firmware.Layout)
   is
      Position : Natural := 0;
      Wanted : Natural;
      Count : Unsigned_64;
      Success : Boolean;
      Header : Intel_GPU_Firmware.CSS_Header;
   begin
      Status := Invalid_Size;
      Plan := (others => <>);
      if File_Bytes < 128 or else File_Bytes > Maximum_Blob_Bytes or else
        File_Bytes > Unsigned_64 (Buffer'Length)
      then return; end if;
      while Position < Natural (File_Bytes) loop
         Wanted := Natural'Min (Chunk_Bytes, Natural (File_Bytes) - Position);
         Read_At (Unsigned_64 (Position),
           Buffer (Buffer'First + Position .. Buffer'First + Position + (Wanted - 1)),
           Count, Success);
         if not Success then Status := Read_Failed; return; end if;
         if Count > Unsigned_64 (Wanted) then
            Status := Invalid_Reply; return;
         end if;
         if Count = 0 then Status := Truncated; return; end if;
         Position := Position + Natural (Count);
      end loop;
      for Index in Header'Range loop
         Header (Index) := Buffer (Buffer'First + Index);
      end loop;
      Plan := Intel_GPU_Firmware.Decode (Header, File_Bytes);
      Status := (if Plan.Valid then Loaded else Invalid_Layout);
   end Load;
end Intel_GPU_Firmware_Reader;
