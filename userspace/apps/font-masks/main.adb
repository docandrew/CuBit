------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Rasterizes every printable glyph of Sans and Sans_Semibold at densities
--  1, 3/2 and 2 through the native font engine (all four subpixel phases)
--  and prints an FNV-1a hash of each mask with its advance, and one over
--  them all: the engine's exact output as CuBit runs it, so two builds of
--  it (soft- and hard-float) can be compared byte for byte
--  (tests/headless --test font-masks).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Messages;
with CuBit.Display_Geometry;
with Compositor_Glyph_FFI;
with Compositor_Glyph_Layout;

procedure Main is
   package L renames Compositor_Glyph_Layout;
   package G renames CuBit.Display_Geometry;
   type Byte_Array is array (Natural range <>) of Unsigned_8;
   type Density is record
      Numerator, Denominator : G.Scale_Component;
   end record;
   Densities : constant array (1 .. 3) of Density := [(1, 1), (3, 2), (2, 1)];
   Faces : constant array (1 .. 2) of Unsigned_32 := [Unsigned_32 (L.Sans_Face), Unsigned_32 (L.Semibold_Face)];
   Offset_Basis : constant Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
   Prime : constant Unsigned_64 := 16#0000_0100_0000_01B3#;
   All_Hash : Unsigned_64 := Offset_Basis;
   Glyphs, Failed : Natural := 0;

   function Hex (V : Unsigned_64) return String is
      Digits_Of : constant String := "0123456789abcdef";
      R : String (1 .. 16);
      X : Unsigned_64 := V;
   begin
      for I in reverse R'Range loop
         R (I) := Digits_Of (Natural (X and 15) + 1);
         X := Shift_Right (X, 4);
      end loop;
      return R;
   end Hex;
   function Image (N : Natural) return String is
      S : constant String := N'Image;
   begin
      return S (S'First + 1 .. S'Last);
   end Image;
begin
   for F of Faces loop
      for D of Densities loop
         declare
            Layout : constant L.Layout := L.Plan ((D.Numerator, D.Denominator));
            Mask : Byte_Array (0 .. Layout.Bytes - 1) := [others => 0];
            Advance : Natural;
            Completed : Boolean;
            H : Unsigned_64;
         begin
            for Code in L.First_Code .. 126 loop
               Mask := [others => 0];
               Compositor_Glyph_FFI.Rasterize
                 (F, Unsigned_32 (Code), Layout, Mask'Address, Unsigned_64 (Layout.Bytes),
                  Advance, Completed);
               H := Offset_Basis;
               if Completed then
                  for B of Mask loop
                     H := (H xor Unsigned_64 (B)) * Prime;
                  end loop;
                  H := (H xor Unsigned_64 (Advance)) * Prime;
               else
                  Failed := Failed + 1;
               end if;
               All_Hash := (All_Hash xor H) * Prime;
               Glyphs := Glyphs + 1;
               CuBit.Messages.debugPrint
                 ("font-masks: face=" & Image (Natural (F)) & " density=" & Image (Natural (D.Numerator)) & "/" &
                  Image (Natural (D.Denominator)) & " code=" & Image (Code) &
                  " advance=" & Image (Advance) & " hash=" & Hex (H) &
                  (if Completed then "" else " FAILED") & ASCII.LF);
            end loop;
         end;
      end loop;
   end loop;
   CuBit.Messages.debugPrint
     ("font-masks: glyphs=" & Image (Glyphs) & " failed=" & Image (Failed) &
      " all=" & Hex (All_Hash) & " done" & ASCII.LF);
end Main;
