pragma Ada_2022;
package body CuBit.Display_Frame_Damage with SPARK_Mode is
   function Encode (Frame : Unsigned_64; Item : Damage) return Word_Array is
     ([for I in Word_Array'Range =>
         (if I = 0 then Magic
          elsif I = 1 then Frame
          elsif I = 2 then Unsigned_64 (Item.Count)
          elsif I - Header_Words + 1 <= Item.Count then Pack (Item.Boxes (I - Header_Words + 1))
          else 0)]);

   function Decode (Words : Word_Array; Frame : Unsigned_64; Area : DP.Rectangle) return Decoding is
      Result : Damage;
      Count : Box_Count;
   begin
      if Words (0) /= Magic or else Words (1) /= Frame or else
        Words (2) not in 1 .. Maximum_Boxes
      then
         return (Valid => False);
      end if;
      Count := Box_Count (Words (2));
      Result.Count := Count;
      for I in 1 .. Count loop
         Result.Boxes (I) := Unpack (Words (Header_Words + I - 1));
         if not Nonempty (Result.Boxes (I)) or else not Inside (Result.Boxes (I), Area) then
            return (Valid => False);
         end if;
         pragma Loop_Invariant
           (Result.Count = Count and Unsigned_64 (Count) = Words (2) and
            (for all J in 1 .. I => Nonempty (Result.Boxes (J)) and Inside (Result.Boxes (J), Area)));
      end loop;
      return (True, Result);
   end Decode;
end CuBit.Display_Frame_Damage;
