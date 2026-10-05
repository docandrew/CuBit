pragma Assertion_Policy (Post => Ignore, Loop_Invariant => Ignore, Assert => Ignore);
--  The postconditions and loop invariants describe whole surfaces (Target'Old)
--  and are discharged by the proof; checking them at run time would copy the
--  surface on every call. Preconditions stay checked where assertions are on.
with Interfaces;
with Client_Glyph_Blend;

--  The toolkit's pixel loops, over plain arrays: rectangle fills and
--  bitmap-glyph blits. Pure and proved (tests/ui-raster): every access stays
--  inside the target and the mask, and pixels outside the rectangle are
--  never written. Proof is what makes compiling this unit with -gnatp (no
--  run-time checks) safe.
--
--  A surface is a pixel array, row after row, Pitch pixels per row. The
--  caller (CuBit.UI's bindings) validates the real surface and passes only
--  the rows a call touches, so Area.Y is usually 0.
package Client_Raster with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_32;
   subtype Byte is Interfaces.Unsigned_8;
   type Pixels is array (Natural range <>) of Word;
   use type Word, Byte;

   --  A rectangle; empty when Width or Height is zero.
   type Area is record
      X, Y : Natural := 0;
      Width, Height : Natural := 0;
   end record;

   --  Area lies within a surface of Length pixels and Pitch pixels per row.
   function Fits (Length : Natural; Pitch : Positive; R : Area) return Boolean is
     (R.Width <= Pitch and then R.X <= Pitch - R.Width and then
      (R.Height = 0 or else R.Width = 0 or else
       (R.X + R.Width <= Length and then
        R.Y <= Natural'Last - (R.Height - 1) and then
        R.Y + (R.Height - 1) <= (Length - R.X - R.Width) / Pitch)));

   function Inside (Index : Natural; Pitch : Positive; R : Area) return Boolean is
     (Index / Pitch >= R.Y and then Index / Pitch - R.Y < R.Height and then
      Index mod Pitch >= R.X and then Index mod Pitch - R.X < R.Width);

   procedure Fill (Target : in out Pixels; Pitch : Positive; R : Area; Value : Word)
     with Pre => Target'First = 0 and then Target'Last < Natural'Last and then
                 Fits (Target'Length, Pitch, R),
          Post => (for all I in Target'Range =>
                     (if Inside (I, Pitch, R) then Target (I) = Value else Target (I) = Target'Old (I)));

   --  A bitmap glyph's coverage (CuBit.Fonts.Coverage's shape).
   MASK_ROWS : constant := 36;
   MASK_COLUMNS : constant := 32;
   subtype Mask_Row is Natural range 0 .. MASK_ROWS - 1;
   subtype Mask_Column is Natural range 0 .. MASK_COLUMNS - 1;
   type Mask is array (Mask_Row, Mask_Column) of Byte;

   --  Ink the glyph's coverage into R, starting at mask cell (Row, Column).
   --  Opaque: covered pixels mix Ink over Background, uncovered ones become
   --  Background (the glyph cell is fully painted). Otherwise covered pixels
   --  mix Ink over what is already there and uncovered ones are untouched.
   procedure Blit_Mask
     (Target : in out Pixels; Pitch : Positive; R : Area; Source : Mask;
      Row : Mask_Row; Column : Mask_Column; Ink : Word;
      Opaque : Boolean; Background : Word)
     with Pre => Target'First = 0 and then Target'Last < Natural'Last and then
                 Fits (Target'Length, Pitch, R) and then
                 R.Height <= MASK_ROWS - Row and then R.Width <= MASK_COLUMNS - Column,
          Post => (for all I in Target'Range =>
                     (if not Inside (I, Pitch, R) then Target (I) = Target'Old (I)));

   --  Copy the R.Width x R.Height block at (Source_X, Source_Y) of Source
   --  (Source_Pitch pixels per row) into R of Target: one row copy each.
   procedure Copy_Block
     (Target : in out Pixels; Pitch : Positive; R : Area;
      Source : Pixels; Source_Pitch : Positive; Source_X, Source_Y : Natural)
     with Pre => Target'First = 0 and then Target'Last < Natural'Last and then
                 Source'First = 0 and then Source'Last < Natural'Last and then
                 Fits (Target'Length, Pitch, R) and then
                 Fits (Source'Length, Source_Pitch, (Source_X, Source_Y, R.Width, R.Height)),
          Post => (for all I in Target'Range =>
                     (if not Inside (I, Pitch, R) then Target (I) = Target'Old (I)));

   function Mix (Foreground, Background : Word; Alpha : Byte) return Word
     renames Client_Glyph_Blend.Mix;
end Client_Raster;
