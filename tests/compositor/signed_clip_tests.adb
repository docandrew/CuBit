with Ada.Text_IO;
with Interfaces.C;
with Client_Signed_Clip;
procedure Signed_Clip_Tests is
   subtype I is Interfaces.C.int;
   use type I;
   Values : constant array (Positive range <>) of I :=
     (I'First, I'First + 1, -65_535, -1, 0, 1, 65_535, I'Last - 1, I'Last);
   Count : Natural := 0;
   procedure Check (Value, Offset, Limit : I) is
      Actual : constant I := Client_Signed_Clip.Edge (Value, Offset, Limit);
      -- Independent piecewise interval oracle, not the implementation's
      -- subtract-and-clamp branch ordering.
      Expected : I := 0;
   begin
      if Limit > 0 then
         if Long_Long_Integer (Value) >=
           Long_Long_Integer (Offset) + Long_Long_Integer (Limit)
         then
            Expected := Limit;
         elsif Value > Offset then
            Expected := I (Long_Long_Integer (Value) - Long_Long_Integer (Offset));
         end if;
      end if;
      if Actual /= Expected then
         raise Program_Error with "signed clipping mismatch";
      end if;
      Count := Count + 1;
   end Check;
begin
   for Value of Values loop
      for Offset of Values loop
         for Limit of Values loop
            Check (Value, Offset, Limit);
         end loop;
      end loop;
   end loop;
   for Value in I range -64 .. 64 loop
      for Offset in I range -64 .. 64 loop
         for Limit in I range -2 .. 32 loop
            Check (Value, Offset, Limit);
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS signed clipping" & Count'Image & " cases");
end Signed_Clip_Tests;
