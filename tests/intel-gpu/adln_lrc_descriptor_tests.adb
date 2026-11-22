with Interfaces; use Interfaces;
with Intel_GPU_ADLN_LRC_Descriptor; use Intel_GPU_ADLN_LRC_Descriptor;
with Ada.Text_IO;
procedure ADLN_LRC_Descriptor_Tests is
   Expected : constant array (EU_Priority) of Unsigned_32 :=
     [16#11D#, 16#31D#, 16#51D#];
begin
   for Page in Unsigned_64 range 1 .. 16#FEDFF# loop
      for Priority in EU_Priority loop
         pragma Assert (Encode (Page * 4096, 4096, Priority) =
           Unsigned_32 (Page * 4096) + Expected (Priority));
      end loop;
   end loop;
   for Offset in Unsigned_64 range 1 .. 4095 loop
      pragma Assert (Encode (4096 + Offset, 4096, Normal) = 0);
      pragma Assert (Encode (4096, 4096 + Offset, Normal) = 0);
   end loop;
   pragma Assert (Encode (0, 4096, Normal) = 0);
   pragma Assert (Encode (4096, 0, Normal) = 0);
   pragma Assert (Encode (16#FEE0_0000#, 4096, Normal) = 0);
   pragma Assert (Encode (16#FEDF_F000#, 8192, Normal) = 0);
   pragma Assert (Encode (Unsigned_64'Last, 4096, Normal) = 0);
   pragma Assert (Encode (4096, Unsigned_64'Last, Normal) = 0);
   pragma Assert (Encode (16#100000#, 14 * 4096, Normal) = 16#10031D#);
   Ada.Text_IO.Put_Line ("ADL-N LRC descriptor PASS (numeric encoding only)");
end ADLN_LRC_Descriptor_Tests;
