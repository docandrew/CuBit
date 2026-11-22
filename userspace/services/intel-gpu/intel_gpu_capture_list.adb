package body Intel_GPU_Capture_List with SPARK_Mode is
   function Encode (Items : Descriptors) return Image is
      Result : Image;
      Cursor : Natural := 4;
      procedure Put (At_Byte : Natural; Value : Unsigned_32)
        with Pre => At_Byte <= 4092
      is
      begin
         for J in Natural range 0 .. 3 loop
            Result.Bytes (At_Byte + J) :=
              Unsigned_8 (Shift_Right (Value, 8 * J) and 255);
         end loop;
      end Put;
   begin
      if Items'Length > Max_Descriptors then return Result; end if;
      for Item of Items loop
         if Item.Offset mod 4 /= 0 or else Item.Offset >= 16#0100_0000# then
            return Result;
         end if;
      end loop;
      Put (0, Unsigned_32 (Items'Length));
      for I in Items'Range loop
         pragma Loop_Invariant
           (Cursor = 4 + 16 * (I - Items'First));
         Put (Cursor, Items (I).Offset);
         Put (Cursor + 4, 16#DEAD_F00D#);
         Put (Cursor + 8,
              Shift_Left (Unsigned_32 (Items (I).Group_ID), 12) or
              Shift_Left (Unsigned_32 (Items (I).Instance), 20));
         -- mask and page padding remain zero.
         Cursor := Cursor + 16;
      end loop;
      Result.Valid := True;
      return Result;
   end Encode;
end Intel_GPU_Capture_List;
