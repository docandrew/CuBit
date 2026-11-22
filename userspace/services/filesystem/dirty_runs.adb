------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Dirty_Runs with SPARK_Mode is

   procedure Sort (Items : in out Item_Array; Count : Item_Count) is
      Moving : Item;
      J : Natural;
   begin
      --  Insertion sort: the client fills pages mostly in order, so this is
      --  near linear in practice.
      for K in 1 .. Count - 1 loop
         Moving := Items (K);
         J := K;
         while J > 0 and then Items (J - 1).Page > Moving.Page loop
            Items (J) := Items (J - 1);
            J := J - 1;
            pragma Loop_Invariant (J < K);
            pragma Loop_Invariant
              (for all M in 1 .. J - 1 => Items (M - 1).Page <= Items (M).Page);
            pragma Loop_Invariant
              (for all M in J + 1 .. K => Items (M).Page > Moving.Page);
            pragma Loop_Invariant
              (for all M in J + 2 .. K => Items (M - 1).Page <= Items (M).Page);
            pragma Loop_Invariant
              (if J > 0 then Items (J - 1).Page <= Items (J + 1).Page);
         end loop;
         Items (J) := Moving;
         pragma Loop_Invariant
           (for all M in 1 .. K => Items (M - 1).Page <= Items (M).Page);
      end loop;
   end Sort;

   procedure Next_Run
     (Items : Item_Array; Count : Item_Count; First : Item_Index;
      Last : out Item_Index; Bytes : out Natural) is
   begin
      Last := First;
      Bytes := Length (Items (First));
      while Last + 1 < Count loop
         pragma Loop_Invariant (Last in First .. Count - 1);
         pragma Loop_Invariant (Bytes in 1 .. Buffer_Bytes);
         pragma Loop_Invariant (for all K in First .. Last => Valid (Items (K)));
         pragma Loop_Invariant
           (for all K in First + 1 .. Last => Adjacent (Items (K - 1), Items (K)));
         exit when not Continues (Items, Last + 1);
         exit when Bytes + Length (Items (Last + 1)) > Buffer_Bytes;
         pragma Assert (Valid (Items (Last + 1)));
         Bytes := Bytes + Length (Items (Last + 1));
         Last := Last + 1;
      end loop;
      pragma Assert (for all K in First .. Last => Valid (Items (K)));
      pragma Assert (for all K in First + 1 .. Last => Continues (Items, K));
   end Next_Run;

end Dirty_Runs;
