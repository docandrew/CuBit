with Interfaces;
with Client_Glyph_Blend;
-- Software fallback only. No allocation, IPC or foreign pointers. Source is
-- tightly packed bottom-up RGBA; destination is top-down opaque BGRA/XRGB.
package Servo_Frame_Copy with SPARK_Mode, Pure is
   subtype Bytes is Client_Glyph_Blend.Bytes;
   subtype Pixels is Client_Glyph_Blend.Pixels;
   subtype Rectangle is Client_Glyph_Blend.Rectangle;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   Maximum_Bytes : constant := 16 * 1_024 * 1_024;
   -- Foreign scalar admission, before constructing any imported array.
   -- Pointer validity/non-aliasing remain the serialized FFI caller's duty.
   function Accepts
     (Length : Interfaces.Unsigned_64;
      Width, Height, Expected_Width, Expected_Height : Interfaces.Unsigned_32;
      Pitch, Surface_Height, Page_Top : Natural; Page_Left : Natural := 0) return Boolean is
     (Width in 1 .. 65_535 and then Height in 1 .. 65_535 and then
      Width = Expected_Width and then Height = Expected_Height and then
      Length <= Maximum_Bytes and then
      Length = Interfaces.Unsigned_64 (Width) * Interfaces.Unsigned_64 (Height) * 4 and then
      Pitch in 4 .. Maximum_Bytes and then Pitch mod 4 = 0 and then
      Page_Left <= Pitch / 4 and then Natural (Width) <= Pitch / 4 - Page_Left and then
      Surface_Height <= Maximum_Bytes / Pitch and then
      Page_Top < Surface_Height and then Natural (Height) <= Surface_Height - Page_Top);
   procedure Paint
     (Source : Bytes; Target : in out Pixels; Pitch : Positive; Area : Rectangle)
     with Pre => Source'First = 0 and then Target'First = 0 and then
       Source'Last < Natural'Last and then Target'Last < Natural'Last and then
       Area.Width <= Natural'Last / 4 and then
       Area.Height <= Source'Length / (Area.Width * 4) and then
       Client_Glyph_Blend.Fits (Target'Length, Pitch, Area),
       Post => (for all I in Target'Range =>
         (if not Client_Glyph_Blend.Inside (I, Pitch, Area)
          then Target (I) = Target'Old (I)));
   -- SWGL's native bottom-up BGRA buffer may have padded source rows.
   -- Length covers all rows including padding; padding is never copied.
   function Accepts_BGRA
     (Length : Interfaces.Unsigned_64;
      Width, Height, Expected_Width, Expected_Height, Source_Pitch :
        Interfaces.Unsigned_32;
      Pitch, Surface_Height, Page_Top : Natural; Page_Left : Natural := 0)
      return Boolean is
     (Source_Pitch in 4 .. Maximum_Bytes and then
      Source_Pitch mod 4 = 0 and then
      Interfaces.Unsigned_64 (Width) * 4 <=
        Interfaces.Unsigned_64 (Source_Pitch) and then
      Length <= Maximum_Bytes and then
      Length = Interfaces.Unsigned_64 (Source_Pitch) *
        Interfaces.Unsigned_64 (Height) and then
      Accepts (Interfaces.Unsigned_64 (Width) *
        Interfaces.Unsigned_64 (Height) * 4,
        Width, Height, Expected_Width, Expected_Height,
        Pitch, Surface_Height, Page_Top, Page_Left));
   procedure Paint_BGRA
     (Source : Bytes; Source_Pitch : Positive;
      Target : in out Pixels; Pitch : Positive; Area : Rectangle)
     with Pre => Source'First = 0 and then Target'First = 0 and then
       Source'Last < Natural'Last and then Target'Last < Natural'Last and then
       Area.Width <= Natural'Last / 4 and then
       Area.Width * 4 <= Source_Pitch and then
       Area.Height <= Source'Length / Source_Pitch and then
       Client_Glyph_Blend.Fits (Target'Length, Pitch, Area),
       Post => (for all I in Target'Range =>
         (if not Client_Glyph_Blend.Inside (I, Pitch, Area)
          then Target (I) = Target'Old (I)));
end Servo_Frame_Copy;
