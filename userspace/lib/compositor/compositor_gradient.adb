package body Compositor_Gradient with SPARK_Mode is
   function Weight (Row : Natural; Height : Positive) return Byte is
   begin
      if Height = 1 then return 0; end if;
      return Byte (Long_Long_Integer (Row) * 255 / Long_Long_Integer (Height - 1));
   end Weight;

   function Channel (Top, Bottom, Alpha : Byte) return Byte is
   begin
      return (Bottom * Alpha + Top * (255 - Alpha) + 127) / 255;
   end Channel;

   function At_Row (Top, Bottom : Word; Row : Natural; Height : Positive)
     return Word
   is
      Alpha : constant Byte := Weight (Row, Height);
      Result : Word := 0;
   begin
      if Height = 1 then return Top; end if;
      for Component in 0 .. 2 loop
         Result := Result or Interfaces.Shift_Left
           (Word (Channel
             (Byte (Interfaces.Shift_Right (Top, Component * 8) and 255),
              Byte (Interfaces.Shift_Right (Bottom, Component * 8) and 255), Alpha)),
            Component * 8);
      end loop;
      return Result;
   end At_Row;
end Compositor_Gradient;
