package body Compositor_Gradient with SPARK_Mode is
   function Weight (Row : Natural; Height : Positive) return Byte is
   begin
      if Height = 1 then return 0; end if;
      return Byte (Long_Long_Integer (Row) * 255 / Long_Long_Integer (Height - 1));
   end Weight;

   function Run_Last (Row : Natural; Height : Positive) return Natural is
      First : Natural := Row;
      Limit : Natural := Height;
      Blend : constant Byte := Weight (Row, Height);
      Middle : Natural;
   begin
      while Limit - First > 1 loop
         pragma Loop_Invariant (First >= Row and First < Limit and Limit <= Height);
         pragma Loop_Invariant (for all Y in Row .. First => Weight (Y, Height) = Blend);
         pragma Loop_Invariant
           (if Limit < Height then Weight (Limit, Height) /= Blend);
         pragma Loop_Variant (Decreases => Limit - First);
         Middle := First + (Limit - First) / 2;
         if Weight (Middle, Height) = Blend then
            First := Middle;
         else
            Limit := Middle;
         end if;
      end loop;
      return First;
   end Run_Last;

   function Channel (Top, Bottom, Alpha : Byte) return Byte is
   begin
      return (Bottom * Alpha + Top * (255 - Alpha) + 127) / 255;
   end Channel;

   function At_Weight (Top, Bottom : Word; Alpha : Byte) return Word
   is
      Result : Word := 0;
   begin
      for Component in 0 .. 2 loop
         Result := Result or Interfaces.Shift_Left
           (Word (Channel
             (Byte (Interfaces.Shift_Right (Top, Component * 8) and 255),
              Byte (Interfaces.Shift_Right (Bottom, Component * 8) and 255), Alpha)),
            Component * 8);
      end loop;
      return Result;
   end At_Weight;

   function Color_Run_Last
     (Top, Bottom : Word; Row : Natural; Height : Positive) return Natural
   is
      Last : Natural := Run_Last (Row, Height);
      Color : constant Word := At_Row (Top, Bottom, Row, Height);
      Next : Natural;
   begin
      for Band in 1 .. 255 loop
         pragma Loop_Invariant (Last >= Row and Last < Height);
         pragma Loop_Invariant
           (for all Y in Row .. Last => At_Row (Top, Bottom, Y, Height) = Color);
         if Last = Height - 1 then return Last; end if;
         Next := Last + 1;
         if At_Row (Top, Bottom, Next, Height) /= Color then return Last; end if;
         Last := Run_Last (Next, Height);
      end loop;
      return Last;
   end Color_Run_Last;
end Compositor_Gradient;
