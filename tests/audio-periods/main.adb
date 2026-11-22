with Ada.Text_IO; use Ada.Text_IO;
with CuBit.Audio_Periods; use CuBit.Audio_Periods;
procedure Main is
   Cases : Natural := 0;
begin
   for Count in Period_Count loop
      for Previous in 0 .. Count - 1 loop
         declare Latest : Natural := Previous; begin
            for Steps in 0 .. Count - 1 loop
               pragma Assert (Advance (Previous, Latest, Count) = Steps);
               if Steps > 0 then
                  declare Slot : Natural := (Previous + 1) mod Count; begin
                     for Offset in 0 .. Steps - 1 loop
                        pragma Assert (Refill (Latest, Count, Steps, Offset) = Slot);
                        pragma Assert (Slot /= (Latest + 1) mod Count);
                        Slot := (Slot + 1) mod Count;
                        Cases := Cases + 1;
                     end loop;
                  end;
               end if;
               Latest := (Latest + 1) mod Count;
            end loop;
         end;
      end loop;
   end loop;
   Put_Line ("PASS coalesced audio completion order and active-slot exclusion" & Cases'Image);
end Main;
