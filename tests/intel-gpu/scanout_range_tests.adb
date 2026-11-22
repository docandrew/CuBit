with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Scanout_Range; use Intel_GPU_Scanout_Range;
procedure Scanout_Range_Tests is
   E : Extent;
   Cases : Natural := 0;
begin
   -- Independent per-pixel addresses, including leading offsets, pitch
   -- padding and non-page-sized rows. Every touched page must be retained.
   for BPP in Unsigned_64 range 1 .. 8 loop
      if BPP in 1 | 2 | 4 | 8 then
         for W in Unsigned_64 range 1 .. 17 loop
            for H in Unsigned_64 range 1 .. 19 loop
               for Offset in Unsigned_64 range 0 .. 3 loop
                  declare
                     Pitch : constant Unsigned_64 := (W + Offset) * BPP + 7;
                     Last_Page : Unsigned_64 := 0;
                  begin
                     E := Linear (4096, 4096, Pitch, W, H, Offset, Offset, BPP);
                     pragma Assert (E.Valid and E.First = 4096);
                     for Row in Unsigned_64 range 0 .. H - 1 loop
                        for Col in Unsigned_64 range 0 .. W - 1 loop
                           Last_Page := ((Row + Offset) * Pitch +
                             (Col + Offset) * BPP + BPP - 1) / 4096;
                           pragma Assert ((Last_Page + 1) * 4096 <= E.Bytes);
                        end loop;
                     end loop;
                     pragma Assert (E.Bytes >= (Offset + H) * Pitch);
                     pragma Assert (E.Bytes - (Offset + H) * Pitch < 4096);
                     Cases := Cases + 1;
                  end;
               end loop;
            end loop;
         end loop;
      end if;
   end loop;
   E := Linear (8 * 1024 * 1024, 2 ** 32 - 4096, 4096, 1024, 1, 0, 0, 4);
   pragma Assert (E.Valid and E.Bytes = 4096);
   E := Linear (8 * 1024 * 1024, 2 ** 32 - 4096, 4096, 1024, 2, 0, 0, 4);
   pragma Assert (not E.Valid);
   for Bad in Unsigned_64 range 0 .. 7 loop
      E := Linear (4096, 0,
        (if Bad = 0 then 0 elsif Bad = 1 then Unsigned_64'Last else 4096),
        (if Bad = 2 then 0 elsif Bad = 3 then Unsigned_64'Last else 1024),
        (if Bad = 4 then 0 else 1),
        (if Bad = 5 then 1 else 0),
        (if Bad = 6 then Unsigned_64'Last else 0),
        (if Bad = 7 then 3 else 4));
      pragma Assert (not E.Valid and E.First = 0 and E.Bytes = 0);
   end loop;
   E := Linear (4096, 1, 4096, 1, 1, 0, 0, 4);
   pragma Assert (not E.Valid);
   E := Linear (Unsigned_64'Last, 0, 4096, 1, 1, 0, 0, 4);
   pragma Assert (not E.Valid);
   Ada.Text_IO.Put_Line ("Scanout range PASS:" & Cases'Image & " pixel-range cases and malformed boundaries");
end Scanout_Range_Tests;
