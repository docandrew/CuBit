with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADS_Policies; use Intel_GPU_ADS_Policies;
procedure ADS_Policies_Tests is
   function Word (Bytes : Policy_Bytes; Offset : Natural) return Unsigned_32 is
     (Unsigned_32 (Bytes (Offset)) +
      256 * Unsigned_32 (Bytes (Offset + 1)) +
      65536 * Unsigned_32 (Bytes (Offset + 2)) +
      16777216 * Unsigned_32 (Bytes (Offset + 3)));
   Bytes : Policy_Bytes;
begin
   for Enabled in Boolean loop
      Bytes := Encode (Enabled);
      for I in 0 .. 15 loop
         pragma Assert (Word (Bytes, I * 4) = 0);
      end loop;
      pragma Assert (Word (Bytes, 64) = 500_000);
      pragma Assert (Word (Bytes, 68) = 1);
      pragma Assert (Word (Bytes, 72) = 15);
      pragma Assert (Word (Bytes, 76) = (if Enabled then 0 else 1));
      for I in 20 .. 23 loop
         pragma Assert (Word (Bytes, I * 4) = 0);
      end loop;
      for I in Bytes'Range loop
         if I /= 76 then
            pragma Assert (Bytes (I) = Encode (not Enabled) (I));
         end if;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("ADS policies: both reset modes, all 24 DWORDs PASS");
end ADS_Policies_Tests;
