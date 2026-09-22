with Interfaces;

--  Bundled TrueType fonts rasterized in Rust userspace, never in the kernel.
--  Published glyphs have process lifetime and are immutable/thread-safe.
package CuBit.Fonts is
   type Face is (Sans, Monospace);
   type Raster_Size is (Normal, Double_Size);
   Line_Height : constant := 17;
   Mono_Width : constant := 8;
   Max_Width : constant := 32;
   type Coverage is array (0 .. 35, 0 .. 31) of Interfaces.Unsigned_8
     with Convention => C;
   type Glyph is record
      Advance : Interfaces.Unsigned_32;
      Height : Interfaces.Unsigned_32;
      Alpha : Coverage;
   end record with Convention => C;
   type Glyph_Access is access constant Glyph with Convention => C;
   function Get (Font : Face; Ch : Character;
                 Size : Raster_Size := Normal) return not null Glyph_Access;
   function Width (Font : Face; Ch : Character) return Natural;
end CuBit.Fonts;
