------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  PCI_BAR_Extents - implementation
------------------------------------------------------------------------------
pragma Ada_2022;

package body PCI_BAR_Extents with SPARK_Mode is

   function Size_Of (Mask : Unsigned_64) return Unsigned_64 is
     --  Two's complement: the lowest set bit alone.
     (Mask and ((not Mask) + 1));

   function Extent_Of (Base, Size : Unsigned_64) return Extent is
   begin
      if Size = 0 then
         return (Status => Unimplemented, First_Page => 0, End_Page => 0);
      elsif Base = 0 then
         return (Status => Unassigned, First_Page => 0, End_Page => 0);
      elsif Base / Page_Bytes >= Page_Limit then
         return (Status => Beyond_Limit, First_Page => 0, End_Page => 0);
      elsif Size - 1 > Unsigned_64'Last - Base then
         return (Status => Overflows, First_Page => Base / Page_Bytes, End_Page => Page_Limit);
      end if;
      return (Status     => Sized,
              First_Page => Base / Page_Bytes,
              End_Page   => Unsigned_64'Min ((Base + (Size - 1)) / Page_Bytes + 1, Page_Limit));
   end Extent_Of;

   function Window_Extent (Base, Limit : Unsigned_64) return Extent is
   begin
      if Base > Limit then
         return (Status => Unimplemented, First_Page => 0, End_Page => 0);
      elsif Base = 0 then
         return (Status => Unassigned, First_Page => 0, End_Page => 0);
      end if;
      return Extent_Of (Base, Limit - Base + 1);
   end Window_Extent;

   function Scanout_Holder (Bases : Base_Array; FB_First, FB_Bytes : Unsigned_64) return Natural is
      Best : Natural := 0;
   begin
      if FB_Bytes = 0 then
         return 0;
      end if;
      for I in Bases'Range loop
         if Bases (I) /= 0 and then Bases (I) <= FB_First
           and then (Best = 0 or else Bases (I) > Bases (Best))
         then
            Best := I;
         end if;
         pragma Loop_Invariant
           (if Best = 0 then
              (for all J in Bases'First .. I => Bases (J) = 0 or else Bases (J) > FB_First)
            else Best in Bases'First .. I
                 and then Bases (Best) /= 0 and then Bases (Best) <= FB_First
                 and then (for all J in Bases'First .. I =>
                             (if Bases (J) /= 0 and then Bases (J) <= FB_First
                              then Bases (J) <= Bases (Best))));
      end loop;
      return Best;
   end Scanout_Holder;

   function Bounded_Extent (Base : Unsigned_64; Wide : Boolean; Next_Claim : Unsigned_64)
     return Extent
   is
      First : Unsigned_64;
      Last  : Unsigned_64;   --  past the ceiling
   begin
      if Base = 0 then
         return (Status => Unassigned, First_Page => 0, End_Page => 0);
      elsif Base / Page_Bytes >= Page_Limit then
         return (Status => Beyond_Limit, First_Page => 0, End_Page => 0);
      end if;
      First := Base / Page_Bytes;
      Last := (if Wide then Page_Limit
               else Unsigned_64'Min (First + Ceiling_32 / Page_Bytes, Page_Limit));
      if Next_Claim > First + 1 then
         Last := Unsigned_64'Min (Last, Next_Claim);
      else
         Last := First + 1;
      end if;
      return (Status => Bounded, First_Page => First, End_Page => Unsigned_64'Max (Last, First + 1));
   end Bounded_Extent;

end PCI_BAR_Extents;
