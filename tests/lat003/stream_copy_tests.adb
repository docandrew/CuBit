--  LAT-003: Compositor_Stream_Copy.Copy_Box writes exactly the box (every
--  alignment of the row start, every width), nothing outside it.
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Stream_Copy;
procedure Stream_Copy_Tests is
   W : constant := 157;   --  odd pitch in pixels: rows start at every alignment
   H : constant := 40;
   type Image is array (0 .. W * H - 1) of Unsigned_32 with Alignment => 64;
   Source, Target : Image;
   Checks : Natural := 0;
begin
   for I in Source'Range loop Source (I) := Unsigned_32 (I) * 2654435761; end loop;
   for X in 0 .. 20 loop
      for Width in 0 .. 70 loop
         Target := [others => 16#DEAD_BEEF#];
         Compositor_Stream_Copy.Copy_Box (Target'Address, W * 4, Source'Address, W * 4, X, 3, Width, 5);
         Compositor_Stream_Copy.Fence;
         for Y in 0 .. H - 1 loop
            for C in 0 .. W - 1 loop
               declare
                  Inside : constant Boolean := C in X .. X + Width - 1 and then Y in 3 .. 7;
                  Expected : constant Unsigned_32 := (if Inside then Source (Y * W + C) else 16#DEAD_BEEF#);
               begin
                  if Target (Y * W + C) /= Expected then
                     Ada.Text_IO.Put_Line ("STREAM-COPY: FAIL x" & X'Image & " width" & Width'Image);
                     raise Program_Error;
                  end if;
               end;
            end loop;
         end loop;
         Checks := Checks + 1;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("STREAM-COPY: PASS" & Checks'Image & " boxes, every row alignment, exact");
end Stream_Copy_Tests;
