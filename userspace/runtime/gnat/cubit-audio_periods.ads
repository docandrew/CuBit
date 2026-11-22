package CuBit.Audio_Periods with SPARK_Mode, Pure is
   subtype Period_Count is Positive range 2 .. 32;
   --  Only observable modulo-ring progress is reported. Equal positions may
   --  mean a duplicate notification or a missed complete lap; this is not a
   --  scheduling guarantee or a counter of unobservable full wraps.
   function Advance (Previous, Latest : Natural; Count : Period_Count) return Natural
     with Pre => Previous < Count and Latest < Count,
          Post => Advance'Result < Count;
   function Refill (Latest, Count, Number, Offset : Natural) return Natural
     with Pre => Count in Period_Count and Latest < Count and Number in 1 .. Count - 1
       and Offset < Number,
          Post => Refill'Result < Count and Refill'Result /= (Latest + 1) mod Count;
end CuBit.Audio_Periods;
