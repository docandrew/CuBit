with Interfaces;
package Compositor_Gradient with SPARK_Mode, Pure is
   subtype Byte is Natural range 0 .. 255;
   subtype Word is Interfaces.Unsigned_32;
   use type Word;
   -- Row remains relative to the original rectangle, never its damage clip.
   function Weight (Row : Natural; Height : Positive) return Byte
     with Pre => Row < Height,
       Post => (if Height = 1 then Weight'Result = 0 else
         Long_Long_Integer (Weight'Result) =
           Long_Long_Integer (Row) * 255 / Long_Long_Integer (Height - 1));
   function Channel (Top, Bottom, Alpha : Byte) return Byte
     with Post => Channel'Result =
       (Bottom * Alpha + Top * (255 - Alpha) + 127) / 255 and then
       Channel'Result >= Natural'Min (Top, Bottom) and then
       Channel'Result <= Natural'Max (Top, Bottom);
   -- Match toolkit RGB gradient semantics, including the single-row case.
   function At_Row (Top, Bottom : Word; Row : Natural; Height : Positive)
     return Word with Pre => Row < Height;
end Compositor_Gradient;
