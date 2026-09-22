package body CuBit.Fonts is
   function Native_Glyph (Font, Size, Code : Interfaces.Unsigned_32)
     return Glyph_Access
     with Import, Convention => C, External_Name => "cubit_font_glyph";

   --  Defensive fallback if the bundled asset cannot be decoded; do not
   --  dereference a failed FFI result. This is not a second font backend.
   Missing : aliased constant Glyph :=
     (Advance => Mono_Width, Height => Line_Height, Alpha => [others => [others => 0]]);

   function Get (Font : Face; Ch : Character;
                 Size : Raster_Size := Normal) return not null Glyph_Access
   is
      Result : constant Glyph_Access := Native_Glyph
        (Face'Pos (Font), Raster_Size'Pos (Size), Character'Pos (Ch));
   begin
      return (if Result = null then Missing'Access else Result);
   end Get;

   function Width (Font : Face; Ch : Character) return Natural is
     (Natural (Get (Font, Ch).Advance));
end CuBit.Fonts;
