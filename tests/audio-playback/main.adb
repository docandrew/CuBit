with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Audio_Playback; use CuBit.Audio_Playback;
procedure Main is
   Cases : Natural := 0;
begin
   for Count in 2 .. 32 loop
      declare Cap : constant Unsigned_64 := Unsigned_64 (Count * 256); begin
         for Pending in Unsigned_64 range 0 .. Cap loop
            pragma Assert (Valid (0, Pending, Cap, 2048, 0));
            pragma Assert (Valid (2048, Pending, Cap, 2048, 0));
            Cases := Cases + 2;
         end loop;
         pragma Assert (not Valid (2049, 0, Cap, 2048, 0));
         pragma Assert (not Valid (0, Cap + 1, Cap, 2048, 0));
         pragma Assert (not Valid (0, Unsigned_64'Last, Cap, 2048, 0));
         for Bit in 0 .. 63 loop
            pragma Assert (not Valid (0, 0, Cap, 2048, Shift_Left (1, Bit)));
         end loop;
      end;
   end loop;
   for Cap in Unsigned_64 range 0 .. 8448 loop
      if Cap < 512 or else Cap > 8192 or else Cap mod 256 /= 0 then
         pragma Assert (not Valid (0, 0, Cap, 2048, 0));
      end if;
   end loop;
   pragma Assert (not Valid (0, 0, Unsigned_64'Last, 2048, 0));
   Put_Line ("PASS playback capacity and backlog boundary cases" & Cases'Image);
end Main;
