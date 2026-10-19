with Interfaces;
with Glyph_Pen;

--  Bundled TrueType fonts rasterized in Rust userspace, never in the kernel.
--  Published glyphs and layout tables have process lifetime and are
--  immutable/thread-safe. Width and Lay_Out are the one text measure and
--  placement path (toolkit and Desktop): unrounded advances and GPOS pair
--  kerning for the Sans faces; the fixed Mono_Width grid, unkerned, for
--  Monospace. Sans_Semibold (IBM Plex Sans SemiBold) is the emphasis face:
--  window titles, the focused Deskbar button, notification and panel
--  headers. Face'Pos is the Rust engine's face number (cubit_font_*).
package CuBit.Fonts is
   type Face is (Sans, Monospace, Sans_Semibold);
   type Raster_Size is (Normal, Double_Size);
   Line_Height : constant := 17;
   Mono_Width : constant := 8;
   Max_Width : constant := 32;
   --  Every glyph raster (cells here, density masks too) puts the pen
   --  origin this many logical pixels from its left edge, keeping ink left
   --  of the origin; ink is never clipped at the advance.
   Origin : constant := 2;
   subtype Phase is Glyph_Pen.Phase;
   type Coverage is array (0 .. 35, 0 .. 31) of Interfaces.Unsigned_8
     with Convention => C;
   --  Advance in 1/64 pixel. The pen origin is column Origin on the
   --  baseline; only the ink box (Ink_Width = 0: none) holds coverage.
   type Glyph is record
      Advance : Interfaces.Unsigned_32;
      Height : Interfaces.Unsigned_32;
      Origin : Interfaces.Unsigned_32;
      Ink_Left, Ink_Top, Ink_Width, Ink_Height : Interfaces.Unsigned_32;
      Reserved : Interfaces.Unsigned_32;
      Alpha : Coverage;
   end record with Convention => C;
   type Glyph_Access is access constant Glyph with Convention => C;
   --  The raster of Ch with its pen origin At_Phase quarter pixels right of
   --  column Origin.
   function Get (Font : Face; Ch : Character;
                 Size : Raster_Size := Normal; At_Phase : Phase := 0) return not null Glyph_Access;

   --  Logical width of Text set from pen 0: the rounded final pen position.
   function Width (Font : Face; Text : String) return Natural;

   --  Caret geometry from the same layout: the logical x, from the start of
   --  Text, of the boundary before its Count-th character (Count >= length:
   --  the end, Width); and the boundary nearest X (hit testing), as the
   --  number of characters left of it.
   function Boundary (Font : Face; Text : String; Count : Natural) return Natural;
   function Boundary_At (Font : Face; Text : String; X : Natural) return Natural
     with Post => Boundary_At'Result <= Text'Length;

   --  Visit each character of Text with its pen position, the text starting
   --  at logical X. Callers place a glyph at Glyph_Pen.Pixel_Of (Pen) with
   --  phase Glyph_Pen.Phase_Of (Pen), or at Glyph_Pen.Nearest (Pen) where
   --  their raster cache has no phases. Width (Font, Text) =
   --  Glyph_Pen.Nearest (final pen) - X, so layout and drawing never differ.
   generic
      with procedure Visit (Index : Positive; Ch : Character; Pen : Glyph_Pen.Position);
   procedure Lay_Out (Font : Face; Text : String; X : Glyph_Pen.Pixel);
end CuBit.Fonts;
