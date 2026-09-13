pragma Ada_2022;
package body Buddy_Bitmap with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   procedure Prove_Word_Shift_Monotonic
     (Lower, Upper : Interfaces.Unsigned_64; O : Order)
     with Ghost, Pre => Lower <= Upper,
       Post => Interfaces.Shift_Right (Lower, O + 1) <=
               Interfaces.Shift_Right (Upper, O + 1)
   is
   begin
      null;
   end Prove_Word_Shift_Monotonic;

   procedure Prove_Pair_Ordering (Lower, Upper : Pair_Number)
     with Ghost,
       Pre => Lower <= Upper,
       Post => Count (Lower) <= Count (Upper)
   is
   begin
      null;
   end Prove_Pair_Ordering;

   -- Isolate the bitvector theorem from layout quantifiers and integer bounds.
   -- This is proof code only; the production lookup remains one right shift.
   procedure Prove_Shift_Monotonic (Lower, Upper : Frame_Number; O : Order)
     with Ghost, Pre => Lower <= Upper,
       Post => Count (Pair_Index (Lower, O)) <= Count (Pair_Index (Upper, O))
   is
      L : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Lower);
      U : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Upper);
   begin
      Prove_Word_Shift_Monotonic (L, U, O);
      Prove_Pair_Ordering (Pair_Index (Lower, O), Pair_Index (Upper, O));
   end Prove_Shift_Monotonic;

   function Pair_Span (O : Order) return Positive_Frame_Count is
   begin
      return 2 ** (O + 1);
   end Pair_Span;

   function Make (Last_Frame : Frame_Number; Last_Order : Order) return Layout is
      L : Layout (Last_Order) :=
        (Last_Order => Last_Order, Last_Frame => Last_Frame,
         Boundaries => [others => 0]);
   begin
      for O in 0 .. Last_Order loop
         L.Boundaries (O) := L.Boundaries (O - 1) + Pair_Count (Last_Frame, O);
         pragma Loop_Invariant (L.Boundaries (-1) = 0);
         pragma Loop_Invariant (L.Last_Frame = Last_Frame);
         pragma Loop_Invariant (L.Boundaries (O) <= Count (O + 1) * 2 ** 40);
         pragma Loop_Invariant
           (for all J in 0 .. O =>
             Limit_Bit (L, J) = First_Bit (L, J) + Pair_Count (Last_Frame, J) and then
             Limit_Bit (L, J) <= L.Boundaries (O));
      end loop;
      return L;
   end Make;

   function Locate (L : Layout; O : Order; Frame : Frame_Number) return Count is
      Pair : constant Frame_Number := Count (Pair_Index (Frame, O));
      Bit : constant Count := First_Bit (L, O) + Pair;
   begin
      Prove_Shift_Monotonic (Frame, Highest_Frame (L), O);
      pragma Assert (Limit_Bit (L, O) = First_Bit (L, O) + Pair_Count (Highest_Frame (L), O));
      -- Establish the local bound once, then carry it through the layout's
      -- enclosing bound. This assertion is proved, not an assumption.
      pragma Assert (Bit < Limit_Bit (L, O));
      return Bit;
   end Locate;

   procedure Prove_Separate_Orders
     (L : Layout; Lower, Upper : Order; Left, Right : Frame_Number)
   is
   begin
      for O in Lower .. Upper - 1 loop
         pragma Loop_Invariant (Limit_Bit (L, Lower) <= Limit_Bit (L, O));
      end loop;
   end Prove_Separate_Orders;

   procedure Prove_Word_Coverage (L : Layout; Bit : Count) is
   begin
      null;
   end Prove_Word_Coverage;
end Buddy_Bitmap;
