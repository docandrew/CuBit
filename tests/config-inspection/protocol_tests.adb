with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Config_Protocol; use CuBit.Config_Protocol;
procedure Protocol_Tests is
   Bounds : Request_Bounds;
   Valid : Boolean;
begin
   for Op in Operation loop
      Decode (Op, 4, Unsigned_64'Last, 0, Bounds, Valid);
      pragma Assert (not Valid);
      Decode (Op, 4, 1, Unsigned_64'Last, Bounds, Valid);
      pragma Assert (not Valid);
      Decode (Op, 3, 1, 0, Bounds, Valid);
      pragma Assert (not Valid);
      Decode (Op, 4, 0, 0, Bounds, Valid);
      pragma Assert (Valid = (Op = List_Keys));
      for Key in 1 .. 128 loop
         Decode (Op, 4, Unsigned_64 (Key), 0, Bounds, Valid);
         pragma Assert (Valid and Bounds.Key = Key and Bounds.Input_Bytes <= Bounds.Mapping_Bytes);
      end loop;
      Decode (Op, 4, 128, 4096, Bounds, Valid);
      pragma Assert (Valid = (Op = Set_Value));
      if Valid then pragma Assert (Bounds.Input_Bytes = 4224 and Bounds.Mapping_Bytes = 4224); end if;
   end loop;
   Ada.Text_IO.Put_Line ("PASS Config wire bounds: lengths, empty keys, full payload and legacy framing rejection");
end Protocol_Tests;
