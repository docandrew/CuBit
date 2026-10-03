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
   -- Last row sharing this blend weight; logarithmic search, no pixel storage.
   function Run_Last (Row : Natural; Height : Positive) return Natural
     with Pre => Row < Height,
       Post => Run_Last'Result >= Row and then Run_Last'Result < Height and then
         (for all Y in Row .. Run_Last'Result => Weight (Y, Height) = Weight (Row, Height)) and then
         (if Run_Last'Result < Height - 1 then
            Weight (Run_Last'Result + 1, Height) /= Weight (Row, Height));
   function Channel (Top, Bottom, Alpha : Byte) return Byte
     with Post => Channel'Result =
       (Bottom * Alpha + Top * (255 - Alpha) + 127) / 255 and then
       Channel'Result >= Natural'Min (Top, Bottom) and then
       Channel'Result <= Natural'Max (Top, Bottom);
   -- Match toolkit RGB gradient semantics, including the single-row case.
   function At_Weight (Top, Bottom : Word; Alpha : Byte) return Word;
   function At_Row (Top, Bottom : Word; Row : Natural; Height : Positive)
     return Word is
     (if Height = 1 then Top else At_Weight (Top, Bottom, Weight (Row, Height)))
     with Pre => Row < Height;
   -- Coalesce adjacent blend-weight bands whose final RGB bytes are equal.
   -- At most 256 weight groups are inspected, independent of pixel height.
   function Color_Run_Last
     (Top, Bottom : Word; Row : Natural; Height : Positive) return Natural
     with Pre => Row < Height,
       Post => Color_Run_Last'Result >= Row and then
         Color_Run_Last'Result < Height and then
         (for all Y in Row .. Color_Run_Last'Result =>
            At_Row (Top, Bottom, Y, Height) = At_Row (Top, Bottom, Row, Height));
end Compositor_Gradient;
