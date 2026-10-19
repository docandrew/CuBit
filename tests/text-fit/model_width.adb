package body Model_Width with SPARK_Mode is
   function Width (Text : String) return Natural is
      Total : Natural := 0;
   begin
      for I in Text'Range loop
         Total := (if Total > Natural'Last - Widest then Natural'Last
                   else Total + Advance_Of (Text (I)));
         if I > Text'First and then Text (I - 1) = 'A' and then Text (I) = 'V'
           and then Total >= Pair_Kerning
         then
            Total := Total - Pair_Kerning;
         end if;
      end loop;
      return Total;
   end Width;

   function Hint (Text : String; X : Natural) return Natural is
      Used : Natural := 0;
      Count : Natural := 0;
   begin
      for I in Text'Range loop
         exit when Used > X or else X - Used < Advance_Of (Text (I));
         Used := Used + Advance_Of (Text (I));
         Count := Count + 1;
         pragma Loop_Invariant (Count = I - Text'First + 1);
      end loop;
      return Count;
   end Hint;
end Model_Width;
