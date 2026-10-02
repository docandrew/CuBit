with Interfaces;
-- Pure pixel operation; the caller validates backing and non-aliasing before
-- binding byte/pixel arrays. Rectangles may end in a partially stored last row.
package Client_Glyph_Blend with SPARK_Mode, Pure is
   subtype Byte is Interfaces.Unsigned_8;
   subtype Word is Interfaces.Unsigned_32;
   use type Word, Byte;
   type Bytes is array (Natural range <>) of Byte;
   type Pixels is array (Natural range <>) of Word;
   subtype Channel_Value is Natural range 0 .. 255;
   function Channel (Foreground, Background, Alpha : Channel_Value) return Channel_Value
     with Post => Channel'Result =
       (Foreground * Alpha + Background * (255 - Alpha) + 127) / 255;
   function RGB (Foreground, Background : Word; Alpha : Byte) return Word
     with Post => RGB'Result <= 16#FFFFFF#;
   function Mix (Foreground, Background : Word; Alpha : Byte) return Word is
     (if Alpha = 0 then Background elsif Alpha = 255 then Foreground
      else RGB (Foreground, Background, Alpha));
   type Rectangle is record
      X, Y : Natural := 0;
      Width, Height : Positive := 1;
   end record;
   function Fits (Length : Natural; Pitch : Positive; Area : Rectangle) return Boolean is
     (Area.X < Pitch and then Area.Width <= Pitch - Area.X and then
      Area.X + Area.Width <= Length and then
      Area.Y <= Natural'Last - (Area.Height - 1) and then
      Area.Y + (Area.Height - 1) <= (Length - Area.X - Area.Width) / Pitch);
   function Inside (Index : Natural; Pitch : Positive; Area : Rectangle) return Boolean is
     (Index / Pitch >= Area.Y and then Index / Pitch - Area.Y < Area.Height and then
      Index mod Pitch >= Area.X and then Index mod Pitch - Area.X < Area.Width);
   procedure Paint
     (Mask : Bytes; Target : in out Pixels; Source_Pitch, Target_Pitch : Positive;
      Source, Destination : Rectangle; Tint : Word)
     with Pre => Mask'First = 0 and then Target'First = 0 and then
       Mask'Last < Natural'Last and then Target'Last < Natural'Last and then
       Source.Width = Destination.Width and then Source.Height = Destination.Height and then
       Fits (Mask'Length, Source_Pitch, Source) and then
       Fits (Target'Length, Target_Pitch, Destination),
       Post => (for all I in Target'Range =>
         (if not Inside (I, Target_Pitch, Destination) then Target (I) = Target'Old (I)));
end Client_Glyph_Blend;
