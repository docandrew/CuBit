with Interfaces;
with System;
with Glyph_Pen;
with CuBit.UI_Metrics;

--  Bundled TrueType fonts rasterized in Rust userspace, never in the kernel.
--  Published glyphs and layout tables have process lifetime and are
--  immutable/thread-safe. Width and Lay_Out are the one text measure and
--  placement path (toolkit and Desktop): unrounded advances and GPOS pair
--  kerning for the Sans faces; the fixed Mono_Width grid, unkerned, for
--  Monospace. Sans_Semibold (IBM Plex Sans SemiBold) is the emphasis face:
--  window titles, the focused Deskbar button, notification and panel
--  headers. Face'Pos is the Rust engine's face number (cubit_font_*).
--
--  Reading_1 .. Reading_3 are reading-face slots (APP-005): no bytes are
--  linked into programs that do not use them. A program fills a slot with a
--  face it embeds (ccl-build's pinned `embedded` blob; the font store,
--  UI-034, later) through CuBit.Reading_Faces, once per slot; until then
--  its glyphs are Missing and its widths zero. Each slot is its own face
--  number, so switching faces never invalidates a cache. Besides printable
--  ASCII a reading face draws 16#80# .. 16#FF# as Windows-1252 (curly
--  quotes, dashes, the ellipsis, the pilcrow, the Latin-1 letters); the
--  bundled faces draw those as '?'.
package CuBit.Fonts is
   type Face is (Sans, Monospace, Sans_Semibold, Reading_1, Reading_2, Reading_3);
   subtype Bundled_Face is Face range Sans .. Sans_Semibold;
   subtype Reading_Face is Face range Reading_1 .. Reading_3;
   --  The base em, twice it (density 2 and large headings), and three
   --  quarters of it (superscripts: verse numbers).
   type Raster_Size is (Normal, Double_Size, Small);
   --  The line box and the monospace grid at Base_Em (CuBit.UI_Metrics).
   Line_Height : constant := CuBit.UI_Metrics.Line_Height;
   Mono_Width : constant := CuBit.UI_Metrics.Mono_Advance;
   Max_Width : constant := CuBit.UI_Metrics.Glyph_Width_Bound;
   --  Every glyph raster (cells here, density masks too) puts the pen
   --  origin this many logical pixels from its left edge, keeping ink left
   --  of the origin; ink is never clipped at the advance.
   Origin : constant := CuBit.UI_Metrics.Glyph_Origin;
   subtype Phase is Glyph_Pen.Phase;
   type Coverage is array (0 .. CuBit.UI_Metrics.Glyph_Cell_Rows - 1,
                           0 .. CuBit.UI_Metrics.Glyph_Cell_Columns - 1) of Interfaces.Unsigned_8
     with Convention => C;
   --  Advance in 1/64 pixel. The pen origin is column Origin on row
   --  Baseline; only the ink box (Ink_Width = 0: none) holds coverage.
   type Glyph is record
      Advance : Interfaces.Unsigned_32;
      Height : Interfaces.Unsigned_32;
      Origin : Interfaces.Unsigned_32;
      Ink_Left, Ink_Top, Ink_Width, Ink_Height : Interfaces.Unsigned_32;
      Baseline : Interfaces.Unsigned_32;
      Alpha : Coverage;
   end record with Convention => C;
   type Glyph_Access is access constant Glyph with Convention => C;
   --  The raster of Ch with its pen origin At_Phase quarter pixels right of
   --  column Origin.
   function Get (Font : Face; Ch : Character;
                 Size : Raster_Size := Normal; At_Phase : Phase := 0) return not null Glyph_Access;

   --  A reading slot's face: Length bytes at Bytes, unchanged and alive
   --  for the rest of the process (an embedded blob). Registered: also
   --  when these bytes already are; Refused: other bytes are, or a null,
   --  empty or oversized font; Not_A_Font: it does not parse, lacks a
   --  printable ASCII glyph, or does not fit the line box.
   type Register_Status is (Registered, Refused, Not_A_Font);
   procedure Register
     (Slot : Reading_Face; Bytes : System.Address; Length : Natural; Status : out Register_Status);
   --  The face can be drawn (bundled faces always; a reading slot once filled).
   function Available (Font : Face) return Boolean;

   --  Logical width of Text set from pen 0: the rounded final pen position.
   function Width (Font : Face; Text : String; Size : Raster_Size := Normal) return Natural;

   --  The face's baseline row and line box height at Size, logical pixels
   --  from the top of the line box (Size Normal: within Line_Height).
   function Baseline (Font : Face; Size : Raster_Size := Normal) return Natural;
   function Line (Font : Face; Size : Raster_Size := Normal) return Natural;

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
   procedure Lay_Out (Font : Face; Text : String; X : Glyph_Pen.Pixel; Size : Raster_Size := Normal);
end CuBit.Fonts;
