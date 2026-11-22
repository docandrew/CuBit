------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Internet_Checksum with SPARK_Mode is

   Word_Limit : constant := 16#1_0000#;
   --  Every partial sum: at most Maximum_Length / 2 + 1 words.
   type Accumulator is range 0 .. (Maximum_Length / 2 + 1) * (Word_Limit - 1);

   function Of_Bytes (B : Bytes) return Unsigned_16 is
      Words : constant Natural := B'Length / 2;
      Sum   : Accumulator := 0;
   begin
      for K in 0 .. Words - 1 loop
         pragma Loop_Invariant (Sum <= Accumulator (K) * (Word_Limit - 1));
         declare
            Word : constant Accumulator :=
              Accumulator (B (B'First + 2 * K)) * 256 +
              Accumulator (B (B'First + 2 * K + 1));
         begin
            pragma Assert (Word <= Word_Limit - 1);
            Sum := Sum + Word;
         end;
      end loop;
      if B'Length mod 2 = 1 then
         Sum := Sum + Accumulator (B (B'Last)) * 256;
      end if;
      pragma Assert (Sum <= (Maximum_Length / 2 + 1) * (Word_Limit - 1));
      --  Three end-around folds bring any such sum under 2 ** 16.
      Sum := Sum mod Word_Limit + Sum / Word_Limit;
      pragma Assert (Sum <= 2 * (Word_Limit - 1));
      Sum := Sum mod Word_Limit + Sum / Word_Limit;
      pragma Assert (Sum <= Word_Limit);
      Sum := Sum mod Word_Limit + Sum / Word_Limit;
      pragma Assert (Sum < Word_Limit);
      return not Unsigned_16 (Sum);
   end Of_Bytes;

   function Add_Bytes (Sum : Partial_Sum; B : Bytes) return Partial_Sum is
      --  Four 16-bit words per step (eight bytes), then the rest one word
      --  at a time: the same sum, with fewer loop steps.
      Quads : constant Natural := B'Length / 8;
      Total : Partial_Sum := Sum;
      Pos   : Natural;
      function W (I : Natural) return Partial_Sum is
        (Partial_Sum (B (I)) * 256 + Partial_Sum (B (I + 1)))
      with Pre => I >= B'First and then I < B'Last,
           Post => W'Result <= Word_Limit - 1;
   begin
      for K in 0 .. Quads - 1 loop
         pragma Loop_Invariant (Total <= Sum + Partial_Sum (K) * 4 * (Word_Limit - 1));
         Total := Total + W (B'First + 8 * K) + W (B'First + 8 * K + 2) +
                  W (B'First + 8 * K + 4) + W (B'First + 8 * K + 6);
      end loop;
      if B'Length = 0 then
         return Total;   --  (an empty array's bounds need not be Naturals)
      end if;
      Pos := B'First + 8 * Quads;
      for K in 0 .. (B'Length - 8 * Quads) / 2 - 1 loop
         pragma Loop_Invariant
           (Total <= Sum + Partial_Sum (Quads) * 4 * (Word_Limit - 1) +
                     Partial_Sum (K) * (Word_Limit - 1));
         Total := Total + W (Pos + 2 * K);
      end loop;
      if B'Length mod 2 = 1 then
         Total := Total + Partial_Sum (B (B'Last)) * 256;
      end if;
      return Total;
   end Add_Bytes;

   function Fold (Sum : Partial_Sum) return Unsigned_16 is
      S : Partial_Sum := Sum;
   begin
      --  Four end-around folds bring any partial sum under 2 ** 16.
      S := S mod Word_Limit + S / Word_Limit;
      pragma Assert (S <= Word_Limit - 1 + Maximum_Partial / Word_Limit);
      S := S mod Word_Limit + S / Word_Limit;
      pragma Assert (S <= 2 * (Word_Limit - 1));
      S := S mod Word_Limit + S / Word_Limit;
      pragma Assert (S <= Word_Limit);
      S := S mod Word_Limit + S / Word_Limit;
      pragma Assert (S < Word_Limit);
      return not Unsigned_16 (S);
   end Fold;

end Internet_Checksum;
