-- Layout of the physical allocator's XOR buddy-pair metadata. No addresses,
-- physical overlays, locks or allocator payload bytes belong in this core.
pragma Ada_2022;
with Interfaces;
package Buddy_Bitmap with SPARK_Mode, Pure is
   -- x86-64 page-table entries encode at most 52 physical address bits:
   -- 40 frame-number bits after removing the 12-bit page offset.
   type Count is range 0 .. 2 ** 46;
   subtype Frame_Number is Count range 0 .. 2 ** 40 - 1;
   subtype Order is Natural range 0 .. 39;
   subtype Bit_In_Word is Natural range 0 .. 63;
   subtype Positive_Frame_Count is Count range 1 .. 2 ** 40;
   subtype Pair_Number is Interfaces.Unsigned_64 range
     0 .. Interfaces.Unsigned_64 (Frame_Number'Last);

   function Pair_Span (O : Order) return Positive_Frame_Count with Ghost;
   function Pair_Index (Frame : Frame_Number; O : Order) return Pair_Number is
     (Interfaces.Shift_Right (Interfaces.Unsigned_64 (Frame), O + 1));
   function Pair_Count (Last_Frame : Frame_Number; O : Order) return Positive_Frame_Count is
     (Count (Pair_Index (Last_Frame, O)) + 1);

   type Layout (Last_Order : Order) is private;
   function Highest_Frame (L : Layout) return Frame_Number;
   function First_Bit (L : Layout; O : Order) return Count
     with Pre => O <= L.Last_Order;
   function Limit_Bit (L : Layout; O : Order) return Count
     with Ghost, Pre => O <= L.Last_Order;
   function Total_Bits (L : Layout) return Count;
   function Consistent (L : Layout) return Boolean with Ghost;

   function Make (Last_Frame : Frame_Number; Last_Order : Order) return Layout
     with Post => Make'Result.Last_Order = Last_Order and then
       Highest_Frame (Make'Result) = Last_Frame and then Consistent (Make'Result);

   function Locate (L : Layout; O : Order; Frame : Frame_Number) return Count
     with Pre => Consistent (L) and then O <= L.Last_Order and then
       Frame <= Highest_Frame (L),
       Post => Locate'Result >= First_Bit (L, O) and then
         Locate'Result < Limit_Bit (L, O) and then Locate'Result < Total_Bits (L);

   function Word_Count (L : Layout) return Count is
     (Total_Bits (L) / 64 + (if Total_Bits (L) mod 64 = 0 then 0 else 1))
     with Post => Word_Count'Result <= Count'Last / 64 + 1;
   function Word_Index (Bit : Count) return Count is (Bit / 64);
   function Within_Word (Bit : Count) return Bit_In_Word is (Natural (Bit mod 64));

   procedure Prove_Separate_Orders
     (L : Layout; Lower, Upper : Order; Left, Right : Frame_Number)
     with Ghost,
       Pre => Consistent (L) and then Lower < Upper and then Upper <= L.Last_Order
         and then Left <= Highest_Frame (L) and then Right <= Highest_Frame (L),
       Post => Locate (L, Lower, Left) < Locate (L, Upper, Right);

   procedure Prove_Word_Coverage (L : Layout; Bit : Count)
     with Ghost, Pre => Bit < Total_Bits (L),
       Post => Word_Index (Bit) < Word_Count (L) and then
         Word_Index (Bit) * 64 + Count (Within_Word (Bit)) = Bit;

private
   subtype Slice_Start is Count range 0 .. 40 * 2 ** 40;
   subtype Boundary_Index is Integer range -1 .. Order'Last;
   type Boundary_Array is array (Boundary_Index range <>) of Slice_Start;
   type Layout (Last_Order : Order) is record
      Last_Frame : Frame_Number := 0;
      Boundaries : Boundary_Array (-1 .. Last_Order) := [others => 0];
   end record;
   function Highest_Frame (L : Layout) return Frame_Number is (L.Last_Frame);
   function First_Bit (L : Layout; O : Order) return Count is (L.Boundaries (O - 1));
   function Limit_Bit (L : Layout; O : Order) return Count is
     (L.Boundaries (O));
   function Total_Bits (L : Layout) return Count is (L.Boundaries (L.Last_Order));
   function Consistent (L : Layout) return Boolean is
     (L.Boundaries (-1) = 0 and then
      (for all O in 0 .. L.Last_Order =>
        Limit_Bit (L, O) = First_Bit (L, O) + Pair_Count (L.Last_Frame, O) and then
        Limit_Bit (L, O) <= Total_Bits (L)));
end Buddy_Bitmap;
