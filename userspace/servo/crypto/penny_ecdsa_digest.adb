package body Penny_ECDSA_Digest with SPARK_Mode is

   function Bits2Int (H : Byte_Seq; Q : Order_Bytes) return Byte_Seq is
      Result : Byte_Seq (0 .. Q - 1) := (others => 0);
      Pad    : constant N32 := (if H'Length >= Q then 0 else Q - H'Length);
   begin
      for I in 0 .. Q - 1 loop
         if I >= Pad then
            Result (I) := H (I - Pad);
         end if;
         pragma Loop_Invariant
           (for all J in 0 .. I =>
              Result (J) = (if J < Pad then 0 else H (J - Pad)));
      end loop;
      return Result;
   end Bits2Int;

end Penny_ECDSA_Digest;
