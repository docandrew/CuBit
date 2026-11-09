package body Owned_Reservation_Policy with SPARK_Mode => On is
   function Align_Up (Candidate, Alignment : Unsigned_64) return Unsigned_64 is
      Mask : constant Unsigned_64 := Alignment - 1;
      Sum : Unsigned_64;
   begin
      if Candidate > Unsigned_64'Last - Mask then
         return 0;
      end if;
      Sum := Candidate + Mask;
      pragma Assert ((Sum and Mask) <= Mask);
      pragma Assert ((Sum and not Mask) = Sum - (Sum and Mask));
      return Sum and not Mask;
   end Align_Up;
end Owned_Reservation_Policy;
