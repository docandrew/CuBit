pragma Ada_2022;
package body Firmware_Page_Cache with SPARK_Mode is
   function Decode (Raw, Virtual, Maximum : Unsigned_64; Kind : Leaf_Kind)
     return Description
   is
      Size : constant Unsigned_64 := Leaf_Bytes (Kind);
      Frame : constant Unsigned_64 := Address_Of (Raw, Virtual, Kind);
   begin
      if (Raw and 1) = 0 or else
        (Kind /= Page_4K and then
         ((Raw and 128) = 0 or else (Raw and ((Size - 1) and not Unsigned_64'(8191))) /= 0))
      then
         return (Valid => False);
      end if;
      if Frame > Maximum then return (Valid => False); end if;
      if Maximum - Frame < 4095 then return (Valid => False); end if;
      pragma Assert (Maximum - Frame >= 4095);
      return (True, Frame, Selector (Raw, Kind));
   end Decode;
end Firmware_Page_Cache;
