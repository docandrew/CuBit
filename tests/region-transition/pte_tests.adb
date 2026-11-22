with Region_PTE; use Region_PTE;
with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure PTE_Tests is
   Page : constant Unsigned_64 := 16#12345000#;
   NX : constant Unsigned_64 := 2 ** 63;
   D : Decision;
   Expected : Boolean;
begin
   for Bits in Unsigned_64 range 0 .. 4095 loop
      for Execute_Disabled in Boolean loop
         for Mode in Access_Mode loop
            declare
               Old : constant Unsigned_64 := Page or Bits or
                 (if Execute_Disabled then NX else 0);
            begin
               Expected := (Bits and 4) /= 0 and (Bits and not 16#67#) = 0
                 and not ((Bits and 2) /= 0 and not Execute_Disabled)
                 and (Mode = Inaccessible or (Bits and 1) = 0);
               D := Plan (Old, Page, Mode);
               pragma Assert (D.Allowed = Expected);
               if Expected then
                  pragma Assert ((D.Value and 1) = (if Mode = Inaccessible then 0 else 1));
                  pragma Assert (((D.Value and 2) /= 0) = (Mode = Read_Write));
                  pragma Assert (((D.Value and NX) /= 0) = (Mode /= Read_Execute));
                  pragma Assert ((D.Value and not Change_Mask) = (Old and not Change_Mask));
               else pragma Assert (D.Value = Old); end if;
            end;
         end loop;
      end loop;
   end loop;
   for Bit in 48 .. 62 loop
      pragma Assert (not Plan (Page or 4 or Shift_Left (1, Bit), Page, Read_Write).Allowed);
   end loop;
   pragma Assert (not Plan (Page or 4, Page + 4096, Read_Write).Allowed);
   pragma Assert (not Plan (Page or 4, Page + 1, Read_Write).Allowed);
   pragma Assert (not Plan (4, 0, Read_Write).Allowed);
   Ada.Text_IO.Put_Line ("PASS: 32768 PTE flag/mode cases plus reserved/address rejection");
end PTE_Tests;
