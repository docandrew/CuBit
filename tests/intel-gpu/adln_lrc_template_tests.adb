with Interfaces; use Interfaces;
with Intel_GPU_ADLN_LRC_Template; use Intel_GPU_ADLN_LRC_Template;
with Ada.Text_IO;
with Ada.Command_Line;
procedure ADLN_LRC_Template_Tests is
   Page : constant Register_Page := Build;
begin
   pragma Assert (Page (1) = 16#11081019#);
   pragma Assert (Page (33) = 16#11081011#);
   pragma Assert (Page (52) = 16#11081005#);
   pragma Assert (Page (65) = 16#11080001#);
   pragma Assert (Page (81) = 16#11081065#);
   pragma Assert (Page (2) = 16#2244# and Page (3) = 0);
   pragma Assert (Page (8) = 16#2038# and Page (9) = 0);
   pragma Assert (Page (48) = 16#2274# and Page (49) = 0);
   pragma Assert (Page (50) = 16#2270# and Page (51) = 0);
   pragma Assert (Page (96) = 16#209C# and Page (97) = 0);
   pragma Assert (Page (185) = 16#05000000#);
   pragma Assert (for all I in 186 .. 1023 => Page (I) = 0);
   if Ada.Command_Line.Argument_Count = 1 then
      for Word of Page loop Ada.Text_IO.Put_Line (Word'Image); end loop;
   else
      Ada.Text_IO.Put_Line ("ADL-N LRC template PASS (uninitialized values)");
   end if;
end ADLN_LRC_Template_Tests;
