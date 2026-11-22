------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Wifi_Control with SPARK_Mode is

   Byte_Mask : constant Unsigned_64 := 16#FF#;

   function Word_Of (I : Positive) return Natural is ((I - 1) / Octets_Per_Word)
   with Pre => I <= Maximum_SSID, Post => Word_Of'Result < SSID_Words;

   function Shift_Of (I : Positive) return Natural is (8 * ((I - 1) mod Octets_Per_Word))
   with Pre => I <= Maximum_SSID, Post => Shift_Of'Result <= 56;

   function Encode (S : SSID) return Words4 is
      W : Words4 := [others => 0];
   begin
      for I in 1 .. Maximum_SSID loop
         W (Word_Of (I)) := W (Word_Of (I)) or Shift_Left (Unsigned_64 (S.Octets (I)), Shift_Of (I));
      end loop;
      return W;
   end Encode;

   function Decode (W : Words4) return Decoded_SSID is
      S : SSID;
   begin
      for I in 1 .. Maximum_SSID loop
         S.Octets (I) := Unsigned_8 (Shift_Right (W (Word_Of (I)), Shift_Of (I)) and Byte_Mask);
         if S.Octets (I) /= 0 then
            S.Length := I;
         end if;
      end loop;
      if not Normal (S) or else Encode (S) /= W then
         return (Valid => False);
      end if;
      return (Valid => True, Item => S);
   end Decode;

end CuBit.Wifi_Control;
