with Interfaces; use Interfaces;
with Intel_GPU_Reset_Pages; use Intel_GPU_Reset_Pages;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
with Ada.Text_IO;
procedure Reset_Pages_Tests is
   function Covered (Register_Offset : Unsigned_64) return Boolean is
   begin
      for P in Page_Index loop
         if Register_Offset >= Offset (P) and then Register_Offset - Offset (P) < 4096 then
            return True;
         end if;
      end loop;
      return False;
   end Covered;
begin
   pragma Assert (Covered (16#941C#));
   pragma Assert (Covered (16#CEE8#));
   pragma Assert (Covered (16#13816C#));
   pragma Assert (Offset (7) = 16#138000#);
   pragma Assert (Offset (8) = 16#190000#);
   pragma Assert (Offset (9) = 16#4000#);
   pragma Assert (Offset (10) = 16#B000#);
   pragma Assert (Offset (11) = 0);
   pragma Assert (Offset (12) = 16#E000#);
   pragma Assert (Offset (13) = 16#1C3000#);
   pragma Assert (Offset (14) = 16#1D3000#);
   pragma Assert (Offset (15) = 16#A000#);
   pragma Assert (Covered (16#A008#) and Covered (16#A168#));
   pragma Assert (Covered (16#FDC#) and Covered (16#E18C#) and Covered (16#E4F4#));
   pragma Assert (Covered (16#B020#) and Covered (16#B09C#));
   pragma Assert (Covered (16#4800#) and Covered (16#481C#));
   pragma Assert (Covered (16#1901F0#) and Covered (16#19024C#));
   for E in Engine loop
      pragma Assert (Covered (Unsigned_64 (Engine_Base (E)) + 16#9C#));
      pragma Assert (Covered (Unsigned_64 (Engine_Base (E)) + 16#D0#));
      pragma Assert (Covered (Unsigned_64 (Engine_Base (E)) + 16#29C#));
   end loop;
   for A in Page_Index loop
      pragma Assert (Offset (A) mod 4096 = 0 and Offset (A) < 16#200000#);
      for B in Page_Index loop
         pragma Assert (A = B or else Offset (A) /= Offset (B));
      end loop;
   end loop;
   pragma Assert (not Covered (16#800000#) and not Covered (16#70000#));
   Ada.Text_IO.Put_Line ("PASS: sixteen distinct control pages cover engines, reset, GuC, PAT, MOCS, steering and GT frequency");
end Reset_Pages_Tests;
