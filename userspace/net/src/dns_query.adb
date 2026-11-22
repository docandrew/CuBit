------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body DNS_Query with SPARK_Mode is

   use DNS_Response;

   procedure Build
     (Id : Unsigned_16; Name : DNS_Name.Wire; Name_Length : DNS_Name.Wire_Length;
      M : out Message; Length : out Natural)
   is
      Tail : constant Natural := Header_Size + Name_Length;
   begin
      M := [others => 0];
      M (0) := Unsigned_8 (Shift_Right (Id, 8));
      M (1) := Unsigned_8 (Id and 16#FF#);
      M (2) := Unsigned_8 (Recursion_Desired / 256);
      M (5) := 1;
      for K in 0 .. Name_Length - 1 loop
         M (Header_Size + K) := Name (K);
         pragma Loop_Invariant (for all J in 0 .. K => M (Header_Size + J) = Name (J));
         pragma Loop_Invariant
           (U16 (M, 0) = Id and then U16 (M, 2) = Recursion_Desired and then
            U16 (M, 4) = 1 and then U16 (M, 6) = 0 and then
            (for all J in Header_Size + K + 1 .. M'Last => M (J) = 0));
      end loop;
      M (Tail + 1) := Type_A;
      M (Tail + 3) := Class_IN;
      Length := Tail + Question_Tail;
   end Build;

end DNS_Query;
