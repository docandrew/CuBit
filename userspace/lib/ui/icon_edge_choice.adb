------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which exported icon edge to draw at an output density (see the spec).
------------------------------------------------------------------------------
package body Icon_Edge_Choice with SPARK_Mode is
   function Nearest
     (Available : Edge_List; Design : Edge; Numerator, Denominator : Scale_Part) return Positive
   is
      Best : Positive := Available'First;
   begin
      for I in Available'Range loop
         if Distance (Available (I), Design, Numerator, Denominator) <
            Distance (Available (Best), Design, Numerator, Denominator)
         then
            Best := I;
         end if;
         pragma Loop_Invariant (Best in Available'First .. I);
         pragma Loop_Invariant
           (for all J in Available'First .. I =>
              Distance (Available (Best), Design, Numerator, Denominator) <=
              Distance (Available (J), Design, Numerator, Denominator));
         pragma Loop_Invariant
           (for all J in Available'First .. Best - 1 =>
              Distance (Available (J), Design, Numerator, Denominator) >
              Distance (Available (Best), Design, Numerator, Denominator));
      end loop;
      return Best;
   end Nearest;
end Icon_Edge_Choice;
