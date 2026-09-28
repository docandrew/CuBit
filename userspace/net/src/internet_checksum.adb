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

end Internet_Checksum;
