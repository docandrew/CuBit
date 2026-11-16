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

   procedure Decode (Raw : Unsigned_64; Operation : out Range_Operation;
                     Valid : out Boolean) is
   begin
      Valid := Raw <= Range_Operation'Pos (Range_Operation'Last);
      Operation := (if Valid then Range_Operation'Val (Raw) else Range_Operation'First);
   end Decode;

   function Split (Run_Base : Unsigned_64; Run_Pages : Page_Count;
                   Address, Bytes : Unsigned_64) return Run_Split is
      -- In page numbers, where every bound is exact and the arithmetic
      -- linear: the run is [Base_Page, Run_Limit), the range
      -- [Address_Page, Address_Page + Range_Pages).
      Base_Page : constant Unsigned_64 := Run_Base / Page_Bytes;
      Address_Page : constant Unsigned_64 := Address / Page_Bytes;
      Range_Pages : constant Unsigned_64 := Bytes / Page_Bytes;
      Run_Limit : constant Unsigned_64 := Base_Page + Run_Pages;
      First : Unsigned_64;
      Last : Unsigned_64;
   begin
      pragma Assert (Run_Base = Base_Page * Page_Bytes);
      pragma Assert (Address = Address_Page * Page_Bytes);
      pragma Assert (Bytes = Range_Pages * Page_Bytes);
      pragma Assert (Run_Base + Run_Pages * Page_Bytes = Run_Limit * Page_Bytes);
      pragma Assert (Address + Bytes = (Address_Page + Range_Pages) * Page_Bytes);
      First := Unsigned_64'Max (Base_Page, Address_Page);
      Last := Unsigned_64'Min (Run_Limit, Address_Page + Range_Pages);
      pragma Assert (Address_Page < Run_Limit and then Base_Page < Address_Page + Range_Pages);
      pragma Assert (Base_Page <= First and then First < Last and then Last <= Run_Limit);
      pragma Assert (First * Page_Bytes = Unsigned_64'Max (Run_Base, Address));
      pragma Assert (Last * Page_Bytes =
                     Unsigned_64'Min (Run_Base + Run_Pages * Page_Bytes, Address + Bytes));
      pragma Assert (Run_Base + (First - Base_Page) * Page_Bytes = First * Page_Bytes);
      pragma Assert (Run_Base + (Last - Base_Page) * Page_Bytes = Last * Page_Bytes);
      return (Lower => First - Base_Page, Dropped => Last - First, Upper => Run_Limit - Last);
   end Split;
end Owned_Reservation_Policy;
