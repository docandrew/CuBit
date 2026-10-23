package body Storehouse.Hex with SPARK_Mode is

   function Parse (S : Text) return Digest is
      Result : Digest := [others => 0];
   begin
      for I in Digest_Index loop
         Result (I) := Value (S (2 * I - 1)) * Nibble_Base + Value (S (2 * I));
         pragma Assert (High (Result (I)) = Value (S (2 * I - 1)));
         pragma Assert (Low (Result (I)) = Value (S (2 * I)));
         pragma Loop_Invariant
           (for all K in 1 .. I =>
              Digit (High (Result (K))) = S (2 * K - 1)
              and then Digit (Low (Result (K))) = S (2 * K));
      end loop;
      return Result;
   end Parse;

end Storehouse.Hex;
