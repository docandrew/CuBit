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
   for E in Engine loop
      pragma Assert (Covered (Unsigned_64 (Engine_Base (E)) + 16#9C#));
      pragma Assert (Covered (Unsigned_64 (Engine_Base (E)) + 16#D0#));
      pragma Assert (Covered (Unsigned_64 (Engine_Base (E)) + 16#29C#));
   end loop;
   for A in Page_Index loop
      pragma Assert (Offset (A) mod 4096 = 0 and Offset (A) < 16#200000#);
      pragma Assert (Slot (A) in 16 .. 21);
      for B in Page_Index loop
         pragma Assert (A = B or else (Offset (A) /= Offset (B) and Slot (A) /= Slot (B)));
      end loop;
   end loop;
   pragma Assert (not Covered (16#800000#) and not Covered (16#70000#));
   Ada.Text_IO.Put_Line ("PASS: six distinct reset pages cover engine writes and full reset");
end Reset_Pages_Tests;
