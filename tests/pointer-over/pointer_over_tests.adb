with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Pointer_Over;
procedure Pointer_Over_Tests is
   package PO renames Compositor_Pointer_Over;
   --  Desktop's blend before IPC-009 moved it (main.adb premultipliedPixelOver).
   function Reference (srcARGB, bg : Unsigned_32) return Unsigned_32 is
      alpha : constant Natural := Natural (Shift_Right (srcARGB, 24) and 16#FF#);
      inv   : constant Natural := 255 - alpha;
      sr    : constant Natural := Natural (Shift_Right (srcARGB, 16) and 16#FF#);
      sg    : constant Natural := Natural (Shift_Right (srcARGB, 8) and 16#FF#);
      sb    : constant Natural := Natural (srcARGB and 16#FF#);
      dr    : constant Natural := Natural (Shift_Right (bg, 16) and 16#FF#);
      dg    : constant Natural := Natural (Shift_Right (bg, 8) and 16#FF#);
      db    : constant Natural := Natural (bg and 16#FF#);
      rr    : constant Natural := sr + (dr * inv + 127) / 255;
      rg    : constant Natural := sg + (dg * inv + 127) / 255;
      rb    : constant Natural := sb + (db * inv + 127) / 255;
   begin
      if alpha = 0 then
         return bg;
      elsif alpha = 255 then
         return srcARGB and 16#00FF_FFFF#;
      else
         return Shift_Left (Unsigned_32 (Natural'Min (rr, 255)), 16) or
                Shift_Left (Unsigned_32 (Natural'Min (rg, 255)), 8) or
                Unsigned_32 (Natural'Min (rb, 255));
      end if;
   end Reference;
   Checks : Natural := 0;
   Seed : Unsigned_32 := 16#1234_5678#;
   function Next return Unsigned_32 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 17);
      Seed := Seed xor Shift_Left (Seed, 5);
      return Seed;
   end Next;
   S, D : Unsigned_32;
begin
   --  Every alpha, every source and frame channel value on one channel at a
   --  time, then random words (including non-premultiplied sources, whose
   --  sums clamp).
   for A in Unsigned_32 range 0 .. 255 loop
      for V in Unsigned_32 range 0 .. 255 loop
         for W in Unsigned_32 range 0 .. 255 loop
            S := Shift_Left (A, 24) or Shift_Left (V, 16) or Shift_Left (V, 8) or V;
            D := 16#FF00_0000# or Shift_Left (W, 16) or Shift_Left (W, 8) or W;
            if PO.Over (S, D) /= Reference (S, D) then
               raise Program_Error with "FAIL over" & S'Image & D'Image;
            end if;
            Checks := Checks + 1;
         end loop;
      end loop;
   end loop;
   for I in 1 .. 4_000_000 loop
      S := Next; D := Next;
      if PO.Over (S, D) /= Reference (S, D) then
         raise Program_Error with "FAIL random" & S'Image & D'Image;
      end if;
      Checks := Checks + 1;
   end loop;
   --  A row: only mapped columns change, each from its source column.
   declare
      Frame : PO.Words (0 .. 9) := [others => 16#0011_2233#];
      Before : constant PO.Words := Frame;
      Source : constant PO.Words (0 .. 2) := [16#8040_2010#, 16#FF00_FF00#, 0];
      Map : constant PO.Column_Map (0 .. 9) := [0, 1, 2, PO.No_Column, 2, 1, 0, PO.No_Column, 1, 0];
   begin
      PO.Over_Row (Frame, Source, Map);
      for I in Frame'Range loop
         if Frame (I) /= (if Map (I) = PO.No_Column then Before (I) else Reference (Source (Map (I)), Before (I))) then
            raise Program_Error with "FAIL row" & I'Image;
         end if;
         Checks := Checks + 1;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("POINTER-OVER: PASS" & Checks'Image & " checks");
end Pointer_Over_Tests;
