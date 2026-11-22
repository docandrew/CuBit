with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Ada.Text_IO;
procedure ADLN_PPGTT_Tests is
   Expected_Flags : constant array (Cache_Policy) of Unsigned_64 :=
     [3, 11, 19, 27];
begin
   for Policy in Cache_Policy loop
      for Page in Unsigned_64 range 1 .. 1048575 loop
         pragma Assert (Encode_Leaf (Page * 4096, Policy, Read_Write) =
           (Page * 4096 or Expected_Flags (Policy)));
         pragma Assert (Encode_Leaf (Page * 4096, Policy, Read_Only) = 0);
      end loop;
   end loop;
   for Offset in Unsigned_64 range 1 .. 4095 loop
      pragma Assert (Encode_Leaf (4096 + Offset, Write_Back, Read_Write) = 0);
      pragma Assert (Encode_Directory (4096 + Offset) = 0);
   end loop;
   pragma Assert (Encode_Leaf (0, Write_Back, Read_Write) = 0);
   pragma Assert (Encode_Leaf (2 ** 32, Write_Back, Read_Write) = 0);
   pragma Assert (Encode_Leaf (Unsigned_64'Last, Write_Back, Read_Write) = 0);
   pragma Assert (Encode_Directory (16#FFFFF000#) = 16#FFFFF003#);
   pragma Assert (Encode_Directory (2 ** 32) = 0);
   pragma Assert (Encode_Directory (0) = 0);
   for Level in 0 .. 3 loop
      for Index in Table_Index loop
         declare
            Address : constant Unsigned_64 :=
              Unsigned_64 (Index) * 2 ** (12 + Level * 9) + 4095;
            Path : constant Walk := Locate (Address);
         begin
            pragma Assert (Path.Valid and Path.Offset = 4095);
            pragma Assert ((case Level is when 0 => Path.PT, when 1 => Path.PD,
                             when 2 => Path.PDP, when others => Path.PML4) = Index);
         end;
      end loop;
   end loop;
   pragma Assert (Locate (2 ** 48 - 1) = Walk'(True, 511, 511, 511, 511, 4095));
   pragma Assert (not Locate (2 ** 48).Valid);
   pragma Assert (not Locate (Unsigned_64'Last).Valid);
   Ada.Text_IO.Put_Line ("ADLN PPGTT PASS: all admitted DMA pages/cache policies, RO rejection, geometry (no hardware mappings)");
end ADLN_PPGTT_Tests;
