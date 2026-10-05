pragma Ada_2022;

package body CuBit.SameBoy_Frames with SPARK_Mode is

   procedure Enlarge (Screen : Screen_Pixels; View : out View_Pixels) is
   begin
      for I in View'Range loop
         View (I) := Screen (Source_Of (I));
         pragma Loop_Invariant
           (for all J in View'First .. I =>
              View (J)'Initialized and then View (J) = Screen (Source_Of (J)));
      end loop;
   end Enlarge;

   function Nanoseconds (Cycles : Unsigned_64; Clock_Rate : Unsigned_32)
     return Unsigned_64
   is
      Per_Second : constant Unsigned_64 := 1_000_000_000;
      Units      : constant Unsigned_64 := 2 * Unsigned_64 (Clock_Rate);
      Counted    : constant Unsigned_64 :=
        Unsigned_64'Min (Cycles, Maximum_Cycles);
   begin
      --  Whole seconds and the remainder separately: neither product
      --  exceeds 2**33 * 10**9 < 2**64.
      return (Counted / Units) * Per_Second
        + (Counted mod Units) * Per_Second / Units;
   end Nanoseconds;

end CuBit.SameBoy_Frames;
