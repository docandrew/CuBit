package body CuBit.Audio_Periods with SPARK_Mode is
   function Advance (Previous, Latest : Natural; Count : Period_Count) return Natural is
   begin
      return (Latest + Count - Previous) mod Count;
   end Advance;
   function Refill (Latest, Count, Number, Offset : Natural) return Natural is
   begin
      return (Latest + Count + 1 - Number + Offset) mod Count;
   end Refill;
end CuBit.Audio_Periods;
