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
   pragma Assert (Offset (7) = 16#138000# and Slot (7) = 32);
   pragma Assert (Offset (8) = 16#190000# and Slot (8) = 33);
   pragma Assert (Offset (9) = 16#4000# and Slot (9) = 34);
   pragma Assert (Offset (10) = 16#B000# and Slot (10) = 35);
   pragma Assert (Offset (11) = 0 and Slot (11) = 36);
   pragma Assert (Offset (12) = 16#E000# and Slot (12) = 37);
   pragma Assert (Offset (13) = 16#1C3000# and Slot (13) = 38);
   pragma Assert (Offset (14) = 16#1D3000# and Slot (14) = 39);
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
      pragma Assert (Slot (A) in 16 .. 22 | 32 .. 39);
      pragma Assert (Slot (A) not in 23 .. 31 and Slot (A) < 63);
      for B in Page_Index loop
         pragma Assert (A = B or else (Offset (A) /= Offset (B) and Slot (A) /= Slot (B)));
      end loop;
   end loop;
   pragma Assert (not Covered (16#800000#) and not Covered (16#70000#));
   Ada.Text_IO.Put_Line ("PASS: fifteen distinct control pages cover engines, reset, GuC, PAT, MOCS and steering");
end Reset_Pages_Tests;
